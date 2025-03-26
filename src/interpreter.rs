use std::{collections::HashMap, fmt::Debug};

use crate::ast::*;

pub mod interpret_expr;
pub mod interpret_list;
pub mod interpret_member;

pub struct Context<'a> {
    definitions: Vec<Node<Expr>>,
    associations: HashMap<Ident, usize>,
    prev_frame: Option<&'a Context<'a>>,
    strategy: InterpretStrategy,
}

impl<'a> Context<'a> {
    pub fn local_lookup(&self, ident: &Ident) -> Option<&Node<Expr>> {
        let index = self.associations.get(ident)?;
        let expr = self.definitions.get(*index)?;
        Some(expr)
    }

    pub fn lookup(&self, ident: &Ident) -> Option<&Node<Expr>> {
        match self.local_lookup(ident) {
            Some(expr) => Some(expr),
            None => match &self.prev_frame {
                Some(frame) => frame.lookup(ident),
                None => None,
            },
        }
    }

    pub fn add(&mut self, ident: Ident, expr: Node<Expr>) {
        println!("adding to context {:?} -> {:?}", ident, expr.value);
        self.associations.insert(ident, self.definitions.len());
        self.definitions.push(expr);
    }

    pub fn frame(&'a self, strategy: InterpretStrategy) -> Self {
        Self {
            definitions: Vec::new(),
            associations: HashMap::new(),
            prev_frame: Some(self),
            strategy,
        }
    }

    pub fn strategy(&self) -> InterpretStrategy {
        self.strategy
    }
}

impl Default for Context<'_> {
    fn default() -> Self {
        let mut context = Self {
            definitions: Vec::new(),
            associations: HashMap::new(),
            prev_frame: None,
            strategy: InterpretStrategy::Eval,
        };
        context.add(
            Ident::TIdent("U64".into()),
            Node {
                value: Expr::Primitive(Primitive::U64),
                loc: None,
            },
        );
        context
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InterpretStrategy {
    /// Value-level evaluation
    Eval,

    /// Type-level simplification
    Simplify,
}

#[derive(Debug)]
pub enum InterpretError {
    UndefinedSymbol(Node<Ident>),
    IllegalBinop(&'static str, Node<Binop>),
    ArgumentMismatch(&'static str, usize, usize),
    IllegalFieldOperation(&'static str),
    IllegalCallOperation(&'static str, Node<Expr>),
}

pub trait Interpret {
    type Output;
    fn eval(self, context: &mut Context) -> Result<Self::Output, InterpretError>;
}
