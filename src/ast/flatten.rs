use std::sync::Arc;

use crate::arena::{Arena, Handle};

use super::*;

/// A global pool of all AST expressions, members, and blocks
#[derive(Debug, Default)]
pub struct ASTArena {
    /// A global arena of all member definitions
    pub members: Arena<Node<Members>>,

    /// A global arena of all block definitions
    pub blocks: Arena<Node<Block>>,

    /// A global arena of all expressions
    pub exprs: Arena<Node<Expr>>,
}

/// An error that can occur during parse tree flattening
#[derive(Clone, Debug)]
pub enum FlattenError {}

/// A generic expression
#[derive(Clone, Debug)]
pub enum Expr {
    /// An identifier expression
    Ident(Node<Ident>),

    /// An index expression
    Index(Handle<Node<Expr>>, Handle<Node<Expr>>),

    /// A field expression
    Field(Handle<Node<Expr>>, Node<Field>),

    /// A handle to a struct expression
    Struct(Handle<Node<Members>>),

    /// A handle to an enum expression
    Enum(Handle<Node<Members>>),

    /// A call expression
    Call(Handle<Node<Expr>>, Handle<Node<Members>>),

    /// A function expression
    Func(Handle<Node<Members>>, Handle<Node<Expr>>),

    /// A handle to a block expression
    Block(Handle<Node<Block>>),

    /// An array expression
    Array(Node<Array>),

    /// An array type expression
    ArrayType(Option<Handle<Node<Expr>>>, Handle<Node<Expr>>),

    /// A primitive expression
    Primitive(Primitive),
}

/// A handle to the "parent" of an expression (with regards to scoping)
#[derive(Clone, Copy, Debug)]
pub enum Parent {
    /// A handle to a parent of type [Members]
    Members(Handle<Node<Members>>),

    /// A handle to a parent of type [Block]
    Block(Handle<Node<Block>>),
}

/// A block of code
#[derive(Clone, Debug)]
pub struct Block {
    /// The list of block statements
    pub stmts: Vec<Node<Stmt>>,

    /// The block's parent
    pub parent: Option<Parent>,
}

/// An array expression
#[derive(Clone, Debug)]
pub struct Array {
    elements: Vec<Handle<Node<Expr>>>,
}

/// A list of members (named or positional)
#[derive(Clone, Debug)]
pub struct Members {
    /// The list of members
    pub members: Vec<Node<Member>>,

    /// The parent of this node
    pub parent: Option<Parent>,
}

/// A named member (of a struct or enum)
#[derive(Clone, Debug)]
pub struct Member {
    /// The list of fields
    pub field: Node<Field>,

    /// The parents of this node
    pub expr: Handle<Node<Expr>>,
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

    /// A resolved identifier
    Resolved(Handle<Node<Expr>>),
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
pub struct Stmt {
    ident: Option<Node<Ident>>,
    ty: Handle<Node<Expr>>,
    value: Handle<Node<Expr>>,
}

/// A field (a struct member or an enum variant)
#[derive(Clone, Debug)]
pub enum Field {
    /// An identified field
    Ident(Node<Ident>),

    /// A numbered (positional) field
    Number(u64),
}

/// Converts the parse tree into the flattened abstract syntax tree
pub trait Flatten: Sized {
    /// The flattened version of this type
    type Flattened;

    /// Flattens the given type
    fn flatten(
        self,
        arena: &mut ASTArena,
        parent: Option<Parent>,
    ) -> Result<Self::Flattened, FlattenError>;
}

impl Flatten for Node<parse_tree::Expr> {
    type Flattened = Handle<Node<Expr>>;

    fn flatten(
        self,
        arena: &mut ASTArena,
        parent: Option<Parent>,
    ) -> Result<Self::Flattened, FlattenError> {
        let loc = self.loc;
        match self.val {
            parse_tree::Expr::Ident(ident) => {
                let ident = ident.flatten(arena, parent)?;
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
                .flatten(arena, parent)?;
                Ok(arena.exprs.add(Expr::Call(binop_expr, members).node(loc)))
            }
            parse_tree::Expr::Unop(unop) => {
                let expr_loc = unop.val.expr.loc;
                let unop_expr = arena
                    .exprs
                    .add(Expr::Primitive(Primitive::Unop(unop.val.op.val)).node(unop.val.op.loc));
                let members = vec![parse_tree::Member::Expr(*unop.val.expr).node(expr_loc)]
                    .node(loc)
                    .flatten(arena, parent)?;
                Ok(arena.exprs.add(Expr::Call(unop_expr, members).node(loc)))
            }
            parse_tree::Expr::Index(array, index) => {
                let array = array.flatten(arena, parent)?;
                let index = index.flatten(arena, parent)?;
                Ok(arena.exprs.add(Expr::Index(array, index).node(loc)))
            }
            parse_tree::Expr::Field(expr, field) => {
                let expr = expr.flatten(arena, parent)?;
                let field = field.flatten(arena, parent)?;
                Ok(arena.exprs.add(Expr::Field(expr, field).node(loc)))
            }
            parse_tree::Expr::Struct(fields) => {
                let fields = fields.flatten(arena, parent)?;
                Ok(arena.exprs.add(Expr::Struct(fields).node(loc)))
            }
            parse_tree::Expr::Enum(variants) => {
                let variants = variants.flatten(arena, parent)?;
                Ok(arena.exprs.add(Expr::Enum(variants).node(loc)))
            }
            parse_tree::Expr::Call(func, args) => {
                let func = func.flatten(arena, parent)?;
                let args = args.flatten(arena, parent)?;
                Ok(arena.exprs.add(Expr::Call(func, args).node(loc)))
            }
            parse_tree::Expr::Func(args, body) => {
                let args = args.flatten(arena, parent)?;
                let body = body.flatten(arena, parent)?;
                Ok(arena.exprs.add(Expr::Func(args, body).node(loc)))
            }
            parse_tree::Expr::Block(stmts) => {
                let stmts = stmts.flatten(arena, parent)?;
                Ok(arena.exprs.add(Expr::Block(stmts).node(loc)))
            }
            parse_tree::Expr::Array(elements) => {
                let elements = elements.flatten(arena, parent)?;
                Ok(elements)
            }
            parse_tree::Expr::ArrayType(len, ty) => {
                let ty = ty.flatten(arena, parent)?;
                match len {
                    Some(len) => {
                        let len = len.flatten(arena, parent)?;
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

impl Flatten for Node<parse_tree::Ident> {
    type Flattened = Node<Ident>;

    fn flatten(
        self,
        arena: &mut ASTArena,
        parent: Option<Parent>,
    ) -> Result<Self::Flattened, FlattenError> {
        let loc = self.loc;
        match self.val {
            parse_tree::Ident::VIdent(vident) => Ok(Ident::VIdent(vident).node(loc)),
            parse_tree::Ident::TIdent(tident) => Ok(Ident::TIdent(tident).node(loc)),
            parse_tree::Ident::Hoist(ident) => Ok(Ident::Hoist(Box::new(
                (*ident).node(loc).flatten(arena, parent)?.val,
            ))
            .node(loc)),
        }
    }
}

impl Flatten for Node<parse_tree::Field> {
    type Flattened = Node<Field>;

    fn flatten(
        self,
        arena: &mut ASTArena,
        parent: Option<Parent>,
    ) -> Result<Self::Flattened, FlattenError> {
        let loc = self.loc;
        match self.val {
            parse_tree::Field::Number(number) => Ok(Field::Number(number).node(loc)),
            parse_tree::Field::Ident(ident) => {
                Ok(Field::Ident(ident.unloc().flatten(arena, parent)?).node(loc))
            }
        }
    }
}

impl Flatten for Node<Vec<Node<parse_tree::Member>>> {
    type Flattened = Handle<Node<Members>>;

    fn flatten(
        self,
        arena: &mut ASTArena,
        parent: Option<Parent>,
    ) -> Result<Self::Flattened, FlattenError> {
        let loc = self.loc;

        let mut members = Vec::with_capacity(self.val.len());
        for (idx, member) in self.val.into_iter().enumerate() {
            let member_loc = member.loc;
            let member = match member.val {
                parse_tree::Member::Expr(expr) => {
                    let expr = expr.flatten(arena, parent)?;
                    Member {
                        field: Field::Number(idx as u64).unloc(),
                        expr,
                    }
                }
                parse_tree::Member::Named(ident, expr) => {
                    let ident = ident.flatten(arena, parent)?;
                    let expr = expr.flatten(arena, parent)?;
                    Member {
                        field: Field::Ident(ident).unloc(),
                        expr,
                    }
                }
                parse_tree::Member::NamedFunc(ident, args, expr) => {
                    let ident = ident.flatten(arena, parent)?;
                    let expr = parse_tree::Expr::Func(args, Box::new(expr))
                        .node(loc)
                        .flatten(arena, parent)?;
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

impl Flatten for Node<Vec<Node<parse_tree::Stmt>>> {
    type Flattened = Handle<Node<Block>>;

    fn flatten(
        self,
        arena: &mut ASTArena,
        parent: Option<Parent>,
    ) -> Result<Self::Flattened, FlattenError> {
        let loc = self.loc;

        let mut stmts = Vec::with_capacity(self.val.len());
        for stmt in self.val {
            let stmt_loc = stmt.loc;
            let stmt = match stmt.val {
                parse_tree::Stmt::Expr(expr) => {
                    let expr = expr.flatten(arena, parent)?;
                    Stmt {
                        ident: None,
                        ty: expr,
                        value: expr,
                    }
                }
                parse_tree::Stmt::Binding(ident, ty, value) => {
                    let ident = Some(ident.flatten(arena, parent)?);
                    let value = value.flatten(arena, parent)?;
                    match ty {
                        Some(ty) => {
                            let ty = ty.flatten(arena, parent)?;
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

impl Flatten for Node<Vec<Node<parse_tree::Expr>>> {
    type Flattened = Handle<Node<Expr>>;

    fn flatten(
        self,
        arena: &mut ASTArena,
        parent: Option<Parent>,
    ) -> Result<Self::Flattened, FlattenError> {
        let loc = self.loc;

        let mut exprs = Vec::with_capacity(self.val.len());
        for expr in self.val {
            exprs.push(expr.flatten(arena, parent)?);
        }

        Ok(arena
            .exprs
            .add(Expr::Array(Array { elements: exprs }.node(loc)).node(loc)))
    }
}

/// Indicates that a type is capable of lookup of identifiers
pub trait Lookup {
    /// Recursively looks up the given identifier
    fn lookup(&self, arena: &ASTArena, ident: &Ident) -> Result<Handle<Node<Expr>>, LookupError>;
}

/// An error that can occur during a lookup
pub struct LookupError;

impl Lookup for Parent {
    fn lookup(&self, arena: &ASTArena, ident: &Ident) -> Result<Handle<Node<Expr>>, LookupError> {
        match self {
            Parent::Members(members) => members.lookup(arena, ident),
            Parent::Block(block) => block.lookup(arena, ident),
        }
    }
}

impl Lookup for Handle<Node<Block>> {
    fn lookup(&self, arena: &ASTArena, ident: &Ident) -> Result<Handle<Node<Expr>>, LookupError> {
        // TODO: shadowing
        let block = arena.blocks.get(*self);
        for stmt in &block.val.stmts {
            // TODO: type lookup?
            if let Some(stmt_ident) = &stmt.val.ident {
                if stmt_ident.val == *ident {
                    return Ok(stmt.val.value);
                }
            }
        }

        match block.val.parent {
            Some(parent) => parent.lookup(arena, ident),
            None => Err(LookupError),
        }
    }
}

impl Lookup for Handle<Node<Members>> {
    fn lookup(&self, arena: &ASTArena, ident: &Ident) -> Result<Handle<Node<Expr>>, LookupError> {
        let members = arena.members.get(*self);
        for member in &members.val.members {
            if let Field::Ident(field_ident) = &member.val.field.val {
                if field_ident.val == *ident {
                    return Ok(member.val.expr);
                }
            }
        }

        match members.val.parent {
            Some(parent) => parent.lookup(arena, ident),
            None => Err(LookupError),
        }
    }
}

/// "Resolves" identifiers within the flattened AST
pub trait Resolve {
    /// "Resolves" identifiers within [self] using the given [ASTArena] and [Parent]
    fn resolve(self, arena: &mut ASTArena, parent: Parent) -> Result<(), LookupError>;
}

impl Resolve for Handle<Node<Expr>> {
    fn resolve(self, arena: &mut ASTArena, parent: Parent) -> Result<(), LookupError> {
        let expr = arena.exprs.get(self);
        let loc = expr.loc;
        match &expr.val {
            Expr::Ident(ident) => match parent.lookup(arena, &ident.val) {
                Ok(expr) => {
                    arena.exprs.get_mut(self).val = Expr::Ident(Ident::Resolved(expr).node(loc));
                    Ok(())
                }
                Err(e) => Err(e),
            },
            Expr::Struct(fields) => {
                let () = ();
                fields.resolve(arena, parent)
            }
            Expr::Enum(variants) => todo!(),
            Expr::Block(block) => todo!(),
            _ => Ok(()),
        }
    }
}

impl Resolve for Handle<Node<Members>> {
    fn resolve(self, arena: &mut ASTArena, parent: Parent) -> Result<(), LookupError> {
        todo!()
    }
}
