use std::{collections::HashMap, fmt::Debug};

use crate::ast::*;

/// Interpreter implementation for expressions
pub mod interpret_expr;

/// Interpreter implementation for lists
pub mod interpret_list;

/// Interpreter implementation for members
pub mod interpret_member;

/// Interpreter implementation for statements
pub mod interpret_stmt;

/// The context for an interpreter
pub struct Context<'a> {
    /// A list of context definitions
    definitions: Vec<Node<Expr>>,

    /// A map from identifiers to context definitions (by index)
    associations: HashMap<Ident, usize>,

    /// An optional pointer to the previous context frame (i.e. scope)
    prev_frame: Option<&'a Context<'a>>,

    /// The strategy used by the interpreter when interpreting the AST
    strategy: InterpretStrategy,
}

impl<'a> Context<'a> {
    /// Attempts to look up an identifier within the local (highest level) frame
    pub fn local_lookup(&self, ident: &Node<Ident>) -> Result<&Node<Expr>, InterpretError> {
        if ident.value.is_void() {
            return Err(InterpretError::VoidIsUndefined);
        }
        let index = self
            .associations
            .get(&ident.value)
            .ok_or_else(|| InterpretError::UndefinedSymbol(ident.clone()))?;
        let expr = self
            .definitions
            .get(*index)
            .ok_or_else(|| InterpretError::UndefinedSymbol(ident.clone()))?;
        Ok(expr)
    }

    /// Attempts to look up an identifier within all frames of this context
    pub fn lookup(&self, ident: &Node<Ident>) -> Result<&Node<Expr>, InterpretError> {
        match self.local_lookup(ident) {
            Ok(expr) => Ok(expr),
            Err(InterpretError::UndefinedSymbol(symbol)) => match &self.prev_frame {
                Some(frame) => frame.lookup(ident),
                None => Err(InterpretError::UndefinedSymbol(symbol)),
            },
            Err(e) => Err(e),
        }
    }

    /// Registers a new identifier with the context, returning an error if it already exists
    pub fn unique_add(&mut self, ident: Ident, expr: Node<Expr>) -> Result<(), InterpretError> {
        if ident.is_void() {
            return Ok(());
        }
        println!("adding to context {:?} -> {:?}", ident, expr.value);
        let None = self
            .associations
            .insert(ident.clone(), self.definitions.len())
        else {
            return Err(InterpretError::SymbolDefinedTwice(ident));
        };
        self.definitions.push(expr);
        Ok(())
    }

    /// Recursively hoists the given identifier by its index, inserting a leading `^`
    fn hoist(&mut self, ident: Ident, index: usize) {
        let ident = Ident::Hoist(Box::new(ident));
        if let Some(index) = self.associations.insert(ident.clone(), index) {
            self.hoist(ident, index)
        }
    }

    /// Registers a new identifier with the context, shadowing an existing identifier if it already exists
    pub fn shadow_add(&mut self, ident: Ident, expr: Node<Expr>) -> Result<(), InterpretError> {
        if ident.is_void() {
            return Ok(());
        }
        println!("adding to context {:?} -> {:?}", ident, expr.value);
        if let Some(index) = self
            .associations
            .insert(ident.clone(), self.definitions.len())
        {
            self.hoist(ident, index)
        };
        self.definitions.push(expr);
        Ok(())
    }

    /// Creates a new context frame, whose definitions will be discarded when it gets dropped
    pub fn frame(&'a self, strategy: InterpretStrategy) -> Self {
        Self {
            definitions: Vec::new(),
            associations: HashMap::new(),
            prev_frame: Some(self),
            strategy,
        }
    }

    /// Returns the interpreter strategy currently in use
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
        context
            .unique_add(
                Ident::TIdent("U64".into()),
                Node {
                    value: Expr::Primitive(Primitive::U64),
                    loc: None,
                },
            )
            .expect("name conflict should not happen");
        context
    }
}

/// The interpreter's strategy
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum InterpretStrategy {
    /// Evaluation that tries to produce a single value (e.g. a mathematical operation)
    Eval,

    /// Simplification without changing the behavior of or actually "running" the code (e.g. a function body)
    Simplify,
}

/// An error that is encountered while interpreting
#[derive(Debug)]
pub enum InterpretError {
    /// An undefined symbol was encountered
    UndefinedSymbol(Node<Ident>),

    /// An undefined field was encountered
    UndefinedField(Node<Field>),

    /// A symbol was attempted to be defined twice
    SymbolDefinedTwice(Ident),

    /// An illegal binary operation was used
    IllegalBinop(&'static str, Node<Binop>),

    /// A function argument did not match what was expected
    ArgumentMismatch(&'static str, usize, usize),

    /// Tried to access a field of a non-struct value
    IllegalFieldOperation(&'static str, Node<Expr>),

    /// Tried to call a value that is not a function or constructor
    IllegalCallOperation(&'static str, Node<Expr>),

    /// Tried to use the void identifier `_` as an expression
    VoidIsUndefined,
}

/// Trait for interpreting expressions
pub trait Interpret {
    /// The output of the interpreter for this type
    type Output;

    /// Interprets the code with the given context
    fn interp(self, context: &mut Context) -> Result<Self::Output, InterpretError>;
}
