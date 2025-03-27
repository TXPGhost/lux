use std::{fmt::Debug, sync::Arc};

use crate::lexer::{LocatedToken, Operator};

use lazy_static::lazy_static;

pub mod desugar;
pub use desugar::*;

#[derive(Clone, Debug)]
pub struct Node<T: Clone + Debug> {
    pub value: T,
    pub loc: Option<Loc>,
}

#[derive(Copy, Clone, Debug)]
pub struct Loc {
    pub line_min: usize,
    pub line_max: usize,
    pub col_min: usize,
    pub col_max: usize,
}

impl Loc {
    pub fn combine(lhs: Option<Self>, rhs: Option<Self>) -> Option<Self> {
        match (lhs, rhs) {
            (Some(lhs), Some(rhs)) => Some(Self {
                line_min: lhs.line_min.min(rhs.line_min),
                line_max: lhs.line_max.max(rhs.line_max),
                col_min: lhs.col_min.min(rhs.col_min),
                col_max: lhs.col_max.max(rhs.col_max),
            }),
            (Some(lhs), None) => Some(lhs),
            (None, Some(rhs)) => Some(rhs),
            (None, None) => None,
        }
    }

    pub fn from_token(token: &LocatedToken) -> Self {
        Self {
            line_min: token.line,
            line_max: token.line,
            col_min: token.col_start,
            col_max: token.col_end,
        }
    }
}

#[derive(Clone, Debug)]
pub struct List<T> {
    pub elements: Vec<T>,
}

#[derive(Clone, Debug)]
pub enum Member {
    Expr(Node<Expr>),
    Named(Node<Ident>, Node<Expr>),
    NamedFunc(Node<Ident>, Node<List<Node<Member>>>, Node<Expr>),
}

#[derive(Clone, Debug)]
pub enum Expr {
    /// An identifier (e.g. `Vector3` or `x`)
    Ident(Node<Ident>),

    /// A number (e.g. `42`)
    Number(u64),

    /// A binary operation (e.g. `x + y`)
    Binop(Node<Binop>),

    /// An array index (e.g. `array[4]`)
    Index(Box<Node<Expr>>, Box<Node<Expr>>),

    /// A struct field or enum variant (e.g. `person.height`)
    Field(Box<Node<Expr>>, Node<Field>),

    /// A struct (e.g. `('x', 'y', 'z')` or `(x: 1.0, y: 2.0)`)
    Struct(Node<List<Node<Member>>>),

    /// An enum (e.g. `<VariantA, VariantB>` or `<some: I32, None: ()>`)
    Enum(Node<List<Node<Member>>>),

    /// A function call or a constructor (e.g. `func(x, y)` or `Vector3(1.0, 2.0, 3.0)`)
    Call(Box<Node<Expr>>, Node<List<Node<Member>>>),

    /// A function declaration (e.g. `(x: I32) => x * 2`)
    Func(Node<List<Node<Member>>>, Box<Node<Expr>>),

    /// A code block (e.g. `{ x = 10, y = 20, x + y }`)
    Block(Node<List<Node<Stmt>>>),

    /// An array (e.g. `[1, 2, 3, 4, 5]`)
    Array(Node<List<Node<Expr>>>),

    /// An array type (e.g. `[4]I32` or `[]String`)
    ArrayType(Option<Box<Node<Expr>>>, Box<Node<Expr>>),

    /// A builtin primitive (e.g. numeric types)
    Primitive(Primitive),
}

impl Expr {
    /// Generates a unit expression `()`
    pub fn unit() -> Self {
        Expr::Struct(Node {
            value: List {
                elements: Vec::new(),
            },
            loc: None,
        })
    }

    /// Generates a never expression `<>`
    pub fn never() -> Self {
        Expr::Enum(Node {
            value: List {
                elements: Vec::new(),
            },
            loc: None,
        })
    }
}

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
    pub fn void() -> Self {
        Self::VIdent(IDENT_VOID.clone())
    }

    pub fn is_void(&self) -> bool {
        match self {
            Ident::VIdent(vident) => vident.as_ref() == "_",
            Ident::TIdent(tident) => tident.as_ref() == "_",
            Ident::Hoist(ident) => ident.is_void(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Binop {
    pub lhs: Box<Node<Expr>>,
    pub rhs: Box<Node<Expr>>,
    pub op: Operator,
}

#[derive(Clone, Debug)]
pub struct Unop {
    pub expr: Box<Node<Expr>>,
    pub op: Operator,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Expr(Node<Expr>),
    Binding(Node<Ident>, Option<Node<Expr>>, Node<Expr>),
}

#[derive(Clone, Debug)]
pub enum Field {
    Ident(Ident),
    Number(u64),
}

#[derive(Clone, Debug)]
pub enum Primitive {
    U64,
}
