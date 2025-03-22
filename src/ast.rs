use std::rc::Rc;

use crate::lexer::Operator;

#[derive(Clone, Debug)]
pub struct ASTNode {
    line_min: usize,
    line_max: usize,
    col_min: usize,
    col_max: usize,
}

#[derive(Clone, Debug)]
pub struct ASTList<T>(pub Vec<T>);

#[derive(Clone, Debug)]
pub enum ASTMember {
    Expr(ASTExpr),
    Named(ASTIdent, ASTExpr),
    NamedFunc(ASTIdent, ASTList<ASTMember>, ASTExpr),
}

#[derive(Clone, Debug)]
pub enum ASTExpr {
    Ident(ASTIdent),
    Binop(ASTBinop),
    Lambda(Box<ASTExpr>, Box<ASTExpr>),
    Index(Box<ASTExpr>, Box<ASTExpr>),
    Struct(ASTList<ASTMember>),
    Func(Box<ASTExpr>, ASTList<ASTMember>),
    Block(ASTList<ASTStmt>),
}

#[derive(Clone, Debug)]
pub enum ASTIdent {
    TIdent(Rc<str>),
    VIdent(Rc<str>),
}

#[derive(Clone, Debug)]
pub struct ASTBinop {
    pub lhs: Box<ASTExpr>,
    pub rhs: Box<ASTExpr>,
    pub op: Operator,
}

#[derive(Clone, Debug)]
pub struct ASTUnop {
    pub expr: Box<ASTExpr>,
    pub op: Operator,
}

#[derive(Clone, Debug)]
pub enum ASTStmt {
    Expr(ASTExpr),
    Binding(ASTIdent, Option<ASTExpr>, ASTExpr),
    Block(ASTList<ASTStmt>),
}
