use std::fmt::Debug;
use std::ops::{Add, Range};

use logos::{Lexer, Logos};

use crate::lexer::Token;

pub mod parse_expr;

pub struct Parser<'source> {
    lexer: Lexer<'source, Token>,
    indent_level: usize,
    next_tok: Option<Result<Node<Token>, Node<ParseError>>>,
    line: usize,
    col: usize,
    prec: usize,
}

impl<'source> Parser<'source> {
    pub fn new(source: &'source str) -> Self {
        let mut lexer = Token::lexer(source);
        let next_tok = lexer.next();
        let span = lexer.span();
        let loc = Loc {
            line: 0..0,
            col: span.start..span.end,
        };
        let mut line = 0;
        let mut col = loc.col.end;
        let next_tok = match next_tok {
            Some(Ok(next_tok)) => {
                if next_tok == Token::Newline {
                    line += 1;
                    col = 0;
                }
                Some(Ok(next_tok.loc(loc)))
            }
            Some(Err(())) => Some(Err(
                ParseError::UnrecognizedToken(lexer.slice().to_owned()).loc(loc)
            )),
            _ => None,
        };
        Self {
            lexer,
            indent_level: 0,
            next_tok,
            line,
            col,
            prec: 0,
        }
    }

    pub fn peek(&self) -> Option<Result<Node<Token>, Node<ParseError>>> {
        self.next_tok.clone()
    }

    pub fn eat(&mut self) -> Result<(), Node<ParseError>> {
        let next_tok = self.lexer.next();
        let span = self.lexer.span();
        let loc = Loc {
            line: 0..0,
            col: span.start..span.end,
        };
        let mut line = 0;
        let mut col = loc.col.end;
        let next_tok = match next_tok {
            Some(Ok(next_tok)) => {
                if next_tok == Token::Newline {
                    self.col = 0;
                    self.line += 1;
                } else {
                    self.col += span.end - span.start;
                }
                Some(Ok(next_tok.loc(loc)))
            }
            Some(Err(())) => Some(Err(ParseError::UnrecognizedToken(
                self.lexer.slice().to_owned(),
            )
            .loc(loc))),
            _ => None,
        };
        self.next_tok = next_tok;
        Ok(())
    }

    pub fn next(&mut self) -> Result<Node<Token>, Node<ParseError>> {
        let cur_tok = match self.next_tok.clone() {
            Some(cur_tok) => cur_tok,
            None => Err(ParseError::OutOfTokens.unloc()),
        };
        self.eat();
        cur_tok
    }
}

impl Parser<'_> {}

#[derive(Clone, Debug)]
pub enum ParseError {
    UnrecognizedToken(String),
    OutOfTokens,
}

pub trait Parse: Sized + Clone + Debug {
    fn parse(parser: &mut Parser<'_>) -> Result<Node<Self>, Node<ParseError>>;
}

#[derive(Clone, Debug)]
pub struct Loc {
    line: Range<usize>,
    col: Range<usize>,
}

impl Add for Loc {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            line: self.line.start.min(rhs.line.start)..self.line.end.max(rhs.line.end),
            col: self.col.start.min(rhs.col.start)..self.col.end.max(rhs.col.end),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Node<T: Clone + Debug> {
    elt: T,
    loc: Option<Loc>,
}

pub trait NodeExt: Sized + Clone + Debug {
    fn node(self, loc: Option<Loc>) -> Node<Self> {
        Node { elt: self, loc }
    }

    fn loc(self, loc: Loc) -> Node<Self> {
        self.node(Some(loc))
    }

    fn unloc(self) -> Node<Self> {
        self.node(None)
    }
}

impl<T: Sized + Clone + Debug> NodeExt for T {}

#[derive(Clone, Debug)]
pub enum Expr {
    Ident(Node<Ident>),
    Func(Node<Func>),
    Call(Node<Call>),
    Struct(Node<Struct>),
    Enum(Node<Enum>),
    Constructor(Node<Constructor>),
    FieldAccess(Node<FieldAccess>),
    MethodCall(Node<MethodCall>),
    Unop(Node<Unop>),
    Binop(Node<Binop>),
    Block(Node<Block>),
}

#[derive(Clone, Debug)]
pub struct Func {
    args: Node<FuncArgs>,
    body: Node<Box<Expr>>,
}

#[derive(Clone, Debug)]
pub struct Call {
    func: Node<Box<Expr>>,
    args: Node<CallArgs>,
}

#[derive(Clone, Debug)]
pub struct FuncArgs {
    args: Node<MemberList>,
}

#[derive(Clone, Debug)]
pub struct CallArgs {
    args: Vec<Node<Arg>>,
}

#[derive(Clone, Debug)]
pub struct Arg {
    mutable: bool,
    expr: Node<Expr>,
}

#[derive(Clone, Debug)]
pub struct Struct {
    fields: Node<MemberList>,
}

#[derive(Clone, Debug)]
pub struct Enum {
    variants: Node<MemberList>,
}

#[derive(Clone, Debug)]
pub struct MemberList {
    members: Vec<Node<Member>>,
}

#[derive(Clone, Debug)]
pub struct Member {
    mutable: bool,
    name: Node<Ident>,
    def: Node<Expr>,
}

#[derive(Clone, Debug)]
pub enum Ident {
    Variable(Node<Variable>),
    Class(Node<Class>),
}

#[derive(Clone, Debug)]
pub struct Variable {
    name: String,
}

#[derive(Clone, Debug)]
pub struct Class {
    name: String,
}

#[derive(Clone, Debug)]
pub struct Constructor {
    r#type: Node<Box<Expr>>,
    r#struct: Node<Struct>,
}

#[derive(Clone, Debug)]
pub struct FieldAccess {
    expr: Node<Box<Expr>>,
    field: Node<Ident>,
}

#[derive(Clone, Debug)]
pub struct MethodCall {
    object: Node<Box<Expr>>,
    method: Node<Ident>,
    args: Node<CallArgs>,
}

#[derive(Clone, Debug)]
pub struct Unop {
    expr: Node<Box<Expr>>,
    op: UnaryOperation,
}

#[derive(Clone, Debug)]
pub struct Binop {
    lhs: Node<Box<Expr>>,
    op: BinaryOperation,
    rhs: Node<Box<Expr>>,
}

#[derive(Clone, Debug)]
pub struct Block {
    stmt: Node<Stmt>,
    expr: Node<Box<Expr>>,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Let(Node<Let>),
    Assn(Node<Assn>),
}

#[derive(Clone, Debug)]
pub struct Let {
    mutable: bool,
    ident: Node<Ident>,
    ty: Option<Node<Box<Expr>>>,
    val: Node<Box<Expr>>,
}

#[derive(Clone, Debug)]
pub struct Assn {
    ident: Node<Ident>,
    op: Option<BinaryOperation>,
    val: Node<Box<Expr>>,
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOperation {
    Copy,
    Minus,
    Inversion,
    LogicNot,
    Length,
    Assert,
    Address,
    Deref,
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Concat,
    Repeat,
    Intersection,
    Union,
    Cast,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
    Cmp,
}
