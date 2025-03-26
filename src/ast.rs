use std::{fmt::Debug, rc::Rc};

use crate::lexer::{LocatedToken, Operator};

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
    Ident(Node<Ident>),
    Number(u64),
    Binop(Node<Binop>),
    Index(Box<Node<Expr>>, Box<Node<Expr>>),
    Field(Box<Node<Expr>>, Node<Field>),
    Struct(Node<List<Node<Member>>>),
    Enum(Node<List<Node<Member>>>),
    Call(Box<Node<Expr>>, Node<List<Node<Member>>>),
    Func(Node<List<Node<Member>>>, Box<Node<Expr>>),
    Block(Node<List<Node<Stmt>>>),
    List(Node<List<Node<Expr>>>),
    ListType(Option<Box<Node<Expr>>>, Box<Node<Expr>>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ident {
    TIdent(Rc<str>),
    VIdent(Rc<str>),
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
    Block(List<Node<Stmt>>),
}

#[derive(Clone, Debug)]
pub enum Field {
    Ident(Ident),
    Number(u64),
}
