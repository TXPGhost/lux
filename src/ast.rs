use std::rc::Rc;

use crate::lexer::Operator;

#[derive(Clone, Debug)]
pub struct ASTMetadata {
    pub line_min: usize,
    pub line_max: usize,
    pub col_min: usize,
    pub col_max: usize,
}

impl ASTMetadata {
    pub fn combine(self, rhs: Self) -> Self {
        Self {
            line_min: self.line_min.min(rhs.line_min),
            line_max: self.line_max.max(rhs.line_max),
            col_min: self.col_min.min(rhs.col_min),
            col_max: self.col_max.max(rhs.col_max),
        }
    }
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
    Number(u64),
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
