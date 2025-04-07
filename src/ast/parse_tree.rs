use std::{fmt::Debug, sync::Arc};

use super::*;
use crate::lexer::Operator;

use lazy_static::lazy_static;

/// A member AST node, used within structs and enums
#[derive(Clone, Debug)]
pub enum Member {
    /// An unnamed member, represented just as an expression
    Expr(Node<Expr>),

    /// A named member, represented as an identifier and an expression
    Named(Node<Ident>, Node<Expr>),

    /// A named function member, syntax sugar for a lambda function
    NamedFunc(Node<Ident>, Node<Vec<Node<Member>>>, Node<Expr>),
}

/// A generic expression
#[derive(Clone, Debug)]
pub enum Expr {
    /// An identifier (e.g. `Vector3` or `x`)
    Ident(Node<Ident>),

    /// A unary operation (e.g. `-x`)
    Unop(Node<Unop>),

    /// A binary operation (e.g. `x + y`)
    Binop(Node<Binop>),

    /// An array index (e.g. `array[4]`)
    Index(Box<Node<Expr>>, Box<Node<Expr>>),

    /// A struct field or enum variant (e.g. `person.height`)
    Field(Box<Node<Expr>>, Node<Field>),

    /// A struct (e.g. `('x', 'y', 'z')` or `(x: 1.0, y: 2.0)`)
    Struct(Node<Vec<Node<Member>>>),

    /// An enum (e.g. `<VariantA, VariantB>` or `<some: I32, None: ()>`)
    Enum(Node<Vec<Node<Member>>>),

    /// A function call or a constructor (e.g. `func(x, y)` or `Vector3(1.0, 2.0, 3.0)`)
    Call(Box<Node<Expr>>, Node<Vec<Node<Member>>>),

    /// A function declaration (e.g. `(x: I32) => x * 2`)
    Func(Node<Vec<Node<Member>>>, Box<Node<Expr>>),

    /// A code block (e.g. `{ x = 10, y = 20, x + y }`)
    Block(Node<Vec<Node<Stmt>>>),

    /// An array (e.g. `[1, 2, 3, 4, 5]`)
    Array(Node<Vec<Node<Expr>>>),

    /// A vector type (e.g. `[4]I32` or `[]String`)
    Vector(Option<Box<Node<Expr>>>, Box<Node<Expr>>),

    /// A builtin primitive (e.g. numeric types)
    Primitive(Primitive),
}

impl Expr {
    /// Generates a unit expression `()`
    pub fn unit() -> Self {
        Expr::Struct(Vec::new().unloc())
    }

    /// Generates a never expression `<>`
    pub fn never() -> Self {
        Expr::Enum(Vec::new().unloc())
    }
}

/// An identifier
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ident {
    /// A value identifier written in snake case (e.g. `foo_bar`)
    VIdent(Arc<str>),

    /// A type identifier written in pascal case (e.g. `FooBar`)
    TIdent(Arc<str>),

    /// A hoisted identifier to access previously-shadowed identifiers (e.g. `^old_x`)
    Hoist(Box<Self>),
}

lazy_static! {
    static ref IDENT_VOID: Arc<str> = "_".into();
}

impl Ident {
    /// The void identifier `_`, which discards its value
    pub fn void() -> Self {
        Self::VIdent(IDENT_VOID.clone())
    }

    /// Returns [true] if this identifier is the void identifier `_`
    pub fn is_void(&self) -> bool {
        match self {
            Ident::VIdent(vident) => vident.as_ref() == "_",
            Ident::TIdent(tident) => tident.as_ref() == "_",
            Ident::Hoist(ident) => ident.is_void(),
        }
    }
}

/// A unary operation (e.g. `-x`)
#[derive(Clone, Debug)]
pub struct Unop {
    /// The (right-hand-side) of the operation
    pub expr: Box<Node<Expr>>,

    /// The operator to use
    pub op: Node<Operator>,
}

impl Unop {
    /// Constructs a new [Unop] with the given [Operator] and expression
    pub fn new(op: Node<Operator>, expr: Node<Expr>) -> Self {
        Self {
            op,
            expr: Box::new(expr),
        }
    }
}

/// A binary operation (e.g. `x + y`)
#[derive(Clone, Debug)]
pub struct Binop {
    /// The left-hand-side of the operation
    pub lhs: Box<Node<Expr>>,

    /// The right-hand-side of the operation
    pub rhs: Box<Node<Expr>>,

    /// The operator to use
    pub op: Node<Operator>,
}

impl Binop {
    /// Constructs a new [Binop] with the given [Operator] and operands
    pub fn new(lhs: Node<Expr>, op: Node<Operator>, rhs: Node<Expr>) -> Self {
        Self {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }
}

/// A statement, used within code blocks
#[derive(Clone, Debug)]
pub enum Stmt {
    /// An expression statement
    Expr(Node<Expr>),

    /// A local binding (e.g. `x = 5`)
    Binding(Node<Ident>, Option<Node<Expr>>, Node<Expr>),
}

/// A field, used to get members of structs and enums
#[derive(Clone, Debug)]
pub enum Field {
    /// An identified field, used to access named members
    Ident(Ident),

    /// A numbered field, used to access unnamed members
    Number(u64),
}
