use std::sync::Arc;

use crate::arena::{Arena, Handle};

use super::{
    desugar::{self, DesugarArena},
    Node, Primitive,
};

/// A global pool of all flattened expressions and statements
#[derive(Debug, Default)]
pub struct FlattenArena {
    /// A global arena of all expressions
    pub exprs: Arena<Node<Expr>>,

    /// A global arena of all statements
    pub stmts: Arena<Node<Stmt>>,
}

/// An argument that can be passed to a function or constructor
#[derive(Clone, Debug)]
pub enum CallArg {
    /// An identifier, which references its declaration
    Ident(Decl),

    /// An index argument, such as `arr[5]`
    Index(Node<Box<CallArg>>, Node<Box<CallArg>>),

    /// A field argument, such as `vec.x`
    Field(Node<Box<CallArg>>, Node<Box<CallArg>>),
}

/// An identifier declaration
#[derive(Clone, Debug)]
pub enum Decl {
    /// A local declaration, referencing the statement in which it is defined
    Local(Handle<Node<Stmt>>),

    /// A global/static declaration
    Global(Handle<Node<Expr>>),
}

/// A function call
#[derive(Clone, Debug)]
pub struct Call {
    func: Node<Stmt>,
    args: Vec<Node<CallArg>>,
}

/// A statement
#[derive(Clone, Debug)]
pub struct Stmt {
    ident: Option<Node<Ident>>,
    ty: Handle<Node<Expr>>,
    value: Node<Box<Call>>,
}

/// An identifier
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ident {
    /// A value identifier
    VIdent(Arc<str>),

    /// A type identifier
    TIdent(Arc<str>),
}

/// An expression, which is possibly a type
#[derive(Clone, Debug)]
pub enum Expr {
    /// A primitive type or value
    Primitive(Primitive),

    /// A function declaration
    Func(Vec<Node<Ident>>, Node<Block>),
}

/// A function argument declaration
#[derive(Clone, Debug)]
pub struct FuncArg {
    ident: Node<Ident>,
    ty: Handle<Node<Expr>>,
}

/// A block/function body
#[derive(Clone, Debug)]
pub struct Block {
    stmts: Vec<Stmt>,
    ret_ty: Handle<Node<Expr>>,
    ret_val: Handle<Node<Expr>>,
}

/// An error that can occur during flattening
#[derive(Clone, Debug)]
pub enum FlattenError {}

/// Converts the desugared AST into the flattened AST
pub trait Flatten: Sized {
    /// The flattened AST type
    type Flattened;

    /// Flattens the given type
    fn flatten(
        self,
        desugar_arena: &mut DesugarArena,
        flatten_arena: &mut FlattenArena,
    ) -> Result<Self::Flattened, FlattenError>;
}

impl Flatten for Handle<Node<desugar::Stmt>> {
    type Flattened = Handle<Node<Block>>;

    #[allow(unused, unreachable_code)]
    fn flatten(
        self,
        desugar_arena: &mut DesugarArena,
        flatten_arena: &mut FlattenArena,
    ) -> Result<Self::Flattened, FlattenError> {
        let stmt = desugar_arena.stmts.get(self);
        let value = desugar_arena.exprs.get(stmt.val.value);
        match &value.val {
            desugar::Expr::Ident(node) => todo!("flatten: reassignment"),
            desugar::Expr::Index(handle, handle1) => todo!("flatten: indexing"),
            desugar::Expr::Field(handle, node) => todo!("flatten: field"),
            desugar::Expr::Struct(handle) => todo!("flatten: struct"),
            desugar::Expr::Enum(handle) => todo!("flatten: enum"),
            desugar::Expr::Call(func, args) => todo!("flatten: call"),
            desugar::Expr::Func(handle, handle1) => todo!("flatten: func"),
            desugar::Expr::Block(handle) => todo!("flatten: block"),
            desugar::Expr::Array(node) => todo!("flatten: array"),
            desugar::Expr::ArrayType(handle, handle1) => todo!("flatten: array type"),
            desugar::Expr::Primitive(primitive) => todo!("flatten: primitive"),
        }

        todo!()
    }
}
