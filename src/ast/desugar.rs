use std::sync::Arc;

use crate::arena::{Arena, Handle};

use super::*;

/// A pointer to a [Def] stored in a [DefArena]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DefPtr(usize);

/// The result of a desugaring operation, a global pool of all expressions
#[derive(Debug)]
pub struct ASTArena<I: IdentTy> {
    /// A global arena of all member definitions
    pub members: Arena<Node<Members<I>>>,

    /// A global arena of all block definitions
    pub blocks: Arena<Node<Block<I>>>,

    /// A global arena of all expressions
    pub exprs: Arena<Node<Expr<I>>>,
}

impl<I: IdentTy> Default for ASTArena<I> {
    fn default() -> Self {
        Self {
            members: Arena::default(),
            blocks: Arena::default(),
            exprs: Arena::default(),
        }
    }
}

/// An error that can occur during desugaring
#[derive(Clone, Debug)]
pub enum DesugarError {}

/// A generic expression
#[derive(Clone, Debug)]
pub enum Expr<I: IdentTy> {
    /// An identifier expression
    Ident(I),

    /// An index expression
    Index(Handle<Node<Expr<I>>>, Handle<Node<Expr<I>>>),

    /// A field expression
    Field(Handle<Node<Expr<I>>>, Node<Field>),

    /// A handle to a struct expression
    Struct(Handle<Node<Members<I>>>),

    /// A handle to an enum expression
    Enum(Handle<Node<Members<I>>>),

    /// A call expression
    Call(Handle<Node<Expr<I>>>, Handle<Node<Members<I>>>),

    /// A function expression
    Func(Handle<Node<Members<I>>>, Handle<Node<Expr<I>>>),

    /// A handle to a block expression
    Block(Handle<Node<Block<I>>>),

    /// An array expression
    Array(Node<Array<I>>),

    /// An array type expression
    ArrayType(Option<Handle<Node<Expr<I>>>>, Handle<Node<Expr<I>>>),

    /// A primitive expression
    Primitive(Primitive),
}

/// An identifier type, which parametrizes the desugared AST
pub trait IdentTy: Clone + Debug {}

/// A yet unresolved identifier
pub type UnresolvedIdent = Node<Ident>;

impl IdentTy for Node<Ident> {}

/// A handle to the "parent" of an expression (with regards to scoping)
#[derive(Debug)]
pub enum Parent<I: IdentTy> {
    /// A handle to a parent of type [Members]
    Members(Handle<Node<Members<I>>>),

    /// A handle to a parent of type [Block]
    Block(Handle<Node<Block<I>>>),
}

impl<I: IdentTy> Clone for Parent<I> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<I: IdentTy> Copy for Parent<I> {}

/// A block of code
#[derive(Clone, Debug)]
pub struct Block<I: IdentTy> {
    /// The list of block statements
    pub stmts: Vec<Node<Stmt<I>>>,

    /// The block's parent
    pub parent: Option<Parent<I>>,
}

/// An array expression
#[derive(Clone, Debug)]
pub struct Array<I: IdentTy> {
    /// The elements of the array
    pub elements: Vec<Handle<Node<Expr<I>>>>,
}

/// A list of members (named or positional)
#[derive(Clone, Debug)]
pub struct Members<I: IdentTy> {
    /// The list of members
    pub members: Vec<Node<Member<I>>>,

    /// The parent of this node
    pub parent: Option<Parent<I>>,
}

/// A named member (of a struct or enum)
#[derive(Clone, Debug)]
pub struct Member<I: IdentTy> {
    /// The list of fields
    pub field: Node<Field>,

    /// The parents of this node
    pub expr: Handle<Node<Expr<I>>>,
}

/// An identifier
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ident {
    /// A value identifier
    VIdent(Arc<str>),

    /// A type identifier
    TIdent(Arc<str>),

    /// A hoisted identifier
    Hoist(Box<Ident>),
}

/// A uniquely-named identifier
#[derive(Clone, Debug)]
pub struct UniqueIdent {
    /// The name of the identifier (e.g. `hello`)
    pub name: Arc<str>,

    /// The scope in which the identifier lives (e.g. `Std`)
    pub scope: Option<Arc<UniqueIdent>>,
}

impl PartialEq for UniqueIdent {
    fn eq(&self, other: &Self) -> bool {
        if self.name == other.name {
            true
        } else {
            match (&self.scope, &other.scope) {
                (Some(lhs), Some(rhs)) => lhs.eq(rhs),
                _ => false,
            }
        }
    }
}

impl UniqueIdent {
    /// Constructs a new unique identifier within the given scope
    pub fn new(name: impl Into<Arc<str>>, parent_scope: Option<Arc<Self>>) -> Self {
        Self {
            name: name.into(),
            scope: parent_scope.clone(),
        }
    }
}

/// A statement, for use within blocks
#[derive(Clone, Debug)]
pub struct Stmt<I: IdentTy> {
    /// The identifier the statement assigns to
    pub ident: Option<Node<Ident>>,

    /// The type of the statement
    pub ty: Handle<Node<Expr<I>>>,

    /// The value of the statement
    pub value: Handle<Node<Expr<I>>>,
}

/// A field (a struct member or an enum variant)
#[derive(Clone, Debug)]
pub enum Field {
    /// An identified field
    Ident(Node<Ident>),

    /// A numbered (positional) field
    Number(u64),
}

/// Desugaring from the parse tree to the desugared abstract syntax tree
pub trait Desugar: Sized {
    /// The desugared version of this type
    type Desugared;

    /// Desugars the given type
    fn desugar(
        self,
        arena: &mut ASTArena<UnresolvedIdent>,
        parent: Option<Parent<UnresolvedIdent>>,
    ) -> Result<Self::Desugared, DesugarError>;
}

impl Desugar for Node<parse_tree::Expr> {
    type Desugared = Handle<Node<Expr<UnresolvedIdent>>>;

    fn desugar(
        self,
        arena: &mut ASTArena<UnresolvedIdent>,
        parent: Option<Parent<UnresolvedIdent>>,
    ) -> Result<Self::Desugared, DesugarError> {
        let loc = self.loc;
        match self.val {
            parse_tree::Expr::Ident(ident) => {
                let ident = ident.desugar(arena, parent)?;
                Ok(arena.exprs.add(Expr::Ident(ident).node(loc)))
            }
            parse_tree::Expr::Binop(binop) => {
                let lhs_loc = binop.val.lhs.loc;
                let rhs_loc = binop.val.rhs.loc;
                let binop_expr = arena.exprs.add(
                    Expr::Primitive(Primitive::Binop(binop.val.op.val)).node(binop.val.op.loc),
                );
                let members = vec![
                    parse_tree::Member::Expr(*binop.val.lhs).node(lhs_loc),
                    parse_tree::Member::Expr(*binop.val.rhs).node(rhs_loc),
                ]
                .node(loc)
                .desugar(arena, parent)?;
                Ok(arena.exprs.add(Expr::Call(binop_expr, members).node(loc)))
            }
            parse_tree::Expr::Unop(unop) => {
                let expr_loc = unop.val.expr.loc;
                let unop_expr = arena
                    .exprs
                    .add(Expr::Primitive(Primitive::Unop(unop.val.op.val)).node(unop.val.op.loc));
                let members = vec![parse_tree::Member::Expr(*unop.val.expr).node(expr_loc)]
                    .node(loc)
                    .desugar(arena, parent)?;
                Ok(arena.exprs.add(Expr::Call(unop_expr, members).node(loc)))
            }
            parse_tree::Expr::Index(array, index) => {
                let array = array.desugar(arena, parent)?;
                let index = index.desugar(arena, parent)?;
                Ok(arena.exprs.add(Expr::Index(array, index).node(loc)))
            }
            parse_tree::Expr::Field(expr, field) => {
                let expr = expr.desugar(arena, parent)?;
                let field = field.desugar(arena, parent)?;
                Ok(arena.exprs.add(Expr::Field(expr, field).node(loc)))
            }
            parse_tree::Expr::Struct(fields) => {
                let fields = fields.desugar(arena, parent)?;
                Ok(arena.exprs.add(Expr::Struct(fields).node(loc)))
            }
            parse_tree::Expr::Enum(variants) => {
                let variants = variants.desugar(arena, parent)?;
                Ok(arena.exprs.add(Expr::Enum(variants).node(loc)))
            }
            parse_tree::Expr::Call(func, args) => {
                let func = func.desugar(arena, parent)?;
                let args = args.desugar(arena, parent)?;
                Ok(arena.exprs.add(Expr::Call(func, args).node(loc)))
            }
            parse_tree::Expr::Func(args, body) => {
                let args = args.desugar(arena, parent)?;
                let body = body.desugar(arena, parent)?;
                Ok(arena.exprs.add(Expr::Func(args, body).node(loc)))
            }
            parse_tree::Expr::Block(stmts) => {
                let stmts = stmts.desugar(arena, parent)?;
                Ok(arena.exprs.add(Expr::Block(stmts).node(loc)))
            }
            parse_tree::Expr::Array(elements) => {
                let elements = elements.desugar(arena, parent)?;
                Ok(elements)
            }
            parse_tree::Expr::ArrayType(len, ty) => {
                let ty = ty.desugar(arena, parent)?;
                match len {
                    Some(len) => {
                        let len = len.desugar(arena, parent)?;
                        Ok(arena.exprs.add(Expr::ArrayType(Some(len), ty).node(loc)))
                    }
                    None => Ok(arena.exprs.add(Expr::ArrayType(None, ty).node(loc))),
                }
            }
            parse_tree::Expr::Primitive(primitive) => {
                Ok(arena.exprs.add(Expr::Primitive(primitive).node(loc)))
            }
        }
    }
}

impl Desugar for Node<parse_tree::Ident> {
    type Desugared = Node<Ident>;

    fn desugar(
        self,
        _arena: &mut ASTArena<UnresolvedIdent>,
        _parent: Option<Parent<UnresolvedIdent>>,
    ) -> Result<Self::Desugared, DesugarError> {
        let loc = self.loc;
        match self.val {
            parse_tree::Ident::VIdent(vident) => Ok(Ident::VIdent(vident).node(loc)),
            parse_tree::Ident::TIdent(tident) => Ok(Ident::TIdent(tident).node(loc)),
            parse_tree::Ident::Hoist(ident) => Ok(Ident::Hoist(Box::new(
                (*ident).node(loc).desugar(_arena, _parent)?.val,
            ))
            .node(loc)),
        }
    }
}

impl Desugar for Node<parse_tree::Field> {
    type Desugared = Node<Field>;

    fn desugar(
        self,
        arena: &mut ASTArena<UnresolvedIdent>,
        parent: Option<Parent<UnresolvedIdent>>,
    ) -> Result<Self::Desugared, DesugarError> {
        let loc = self.loc;
        match self.val {
            parse_tree::Field::Number(number) => Ok(Field::Number(number).node(loc)),
            parse_tree::Field::Ident(ident) => {
                Ok(Field::Ident(ident.unloc().desugar(arena, parent)?).node(loc))
            }
        }
    }
}

impl Desugar for Node<Vec<Node<parse_tree::Member>>> {
    type Desugared = Handle<Node<Members<UnresolvedIdent>>>;

    fn desugar(
        self,
        arena: &mut ASTArena<UnresolvedIdent>,
        parent: Option<Parent<UnresolvedIdent>>,
    ) -> Result<Self::Desugared, DesugarError> {
        let loc = self.loc;

        let mut members = Vec::with_capacity(self.val.len());
        for (idx, member) in self.val.into_iter().enumerate() {
            let member_loc = member.loc;
            let member = match member.val {
                parse_tree::Member::Expr(expr) => {
                    let expr = expr.desugar(arena, parent)?;
                    Member {
                        field: Field::Number(idx as u64).unloc(),
                        expr,
                    }
                }
                parse_tree::Member::Named(ident, expr) => {
                    let ident = ident.desugar(arena, parent)?;
                    let expr = expr.desugar(arena, parent)?;
                    Member {
                        field: Field::Ident(ident).unloc(),
                        expr,
                    }
                }
                parse_tree::Member::NamedFunc(ident, args, expr) => {
                    let ident = ident.desugar(arena, parent)?;
                    let expr = parse_tree::Expr::Func(args, Box::new(expr))
                        .node(loc)
                        .desugar(arena, parent)?;
                    Member {
                        field: Field::Ident(ident).unloc(),
                        expr,
                    }
                }
            };
            members.push(member.node(member_loc));
        }
        Ok(arena.members.add(Members { members, parent }.node(loc)))
    }
}

impl Desugar for Node<Vec<Node<parse_tree::Stmt>>> {
    type Desugared = Handle<Node<Block<UnresolvedIdent>>>;

    fn desugar(
        self,
        arena: &mut ASTArena<UnresolvedIdent>,
        parent: Option<Parent<UnresolvedIdent>>,
    ) -> Result<Self::Desugared, DesugarError> {
        let loc = self.loc;

        let mut stmts = Vec::with_capacity(self.val.len());
        for stmt in self.val {
            let stmt_loc = stmt.loc;
            let stmt = match stmt.val {
                parse_tree::Stmt::Expr(expr) => {
                    let expr = expr.desugar(arena, parent)?;
                    Stmt {
                        ident: None,
                        ty: expr,
                        value: expr,
                    }
                }
                parse_tree::Stmt::Binding(ident, ty, value) => {
                    let ident = Some(ident.desugar(arena, parent)?);
                    let value = value.desugar(arena, parent)?;
                    match ty {
                        Some(ty) => {
                            let ty = ty.desugar(arena, parent)?;
                            Stmt { ident, ty, value }
                        }
                        None => Stmt {
                            ident,
                            ty: value,
                            value,
                        },
                    }
                }
            };
            stmts.push(stmt.node(stmt_loc));
        }

        Ok(arena.blocks.add(Block { stmts, parent }.node(loc)))
    }
}

impl Desugar for Node<Vec<Node<parse_tree::Expr>>> {
    type Desugared = Handle<Node<Expr<UnresolvedIdent>>>;

    fn desugar(
        self,
        arena: &mut ASTArena<UnresolvedIdent>,
        parent: Option<Parent<UnresolvedIdent>>,
    ) -> Result<Self::Desugared, DesugarError> {
        let loc = self.loc;

        let mut exprs = Vec::with_capacity(self.val.len());
        for expr in self.val {
            exprs.push(expr.desugar(arena, parent)?);
        }

        Ok(arena
            .exprs
            .add(Expr::Array(Array { elements: exprs }.node(loc)).node(loc)))
    }
}
