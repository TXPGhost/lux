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

/// A context definition
#[derive(Clone, Debug)]
pub enum ContextDefinition {
    /// A static definition, such as a struct member
    Static(Node<Expr>),

    /// A local definition with a type and a value, arising from a block statement
    Local(Node<Expr>, Node<Expr>),

    /// A function parameter definition with a specificed type
    Argument(Node<Expr>),
}

/// The context for an interpreter
pub struct Context<'a> {
    /// A list of context definitions
    definitions: Vec<ContextDefinition>,

    /// A map from identifiers to context definitions (by index)
    associations: HashMap<Ident, usize>,

    /// An optional pointer to the previous context frame (i.e. scope)
    prev_frame: Option<&'a Context<'a>>,

    /// The strategy used by the interpreter when interpreting the AST
    strategy: InterpretStrategy,
}

impl<'a> Context<'a> {
    /// Attempts to look up an identifier within the top (highest level) frame
    pub fn toplevel_lookup(
        &self,
        ident: &Node<Ident>,
    ) -> Result<&ContextDefinition, InterpretError> {
        if ident.val.is_void() {
            return Err(InterpretError::VoidIsUndefined);
        }
        let index = self
            .associations
            .get(&ident.val)
            .ok_or_else(|| InterpretError::UndefinedSymbol(ident.clone()))?;
        let expr = self
            .definitions
            .get(*index)
            .ok_or_else(|| InterpretError::UndefinedSymbol(ident.clone()))?;
        Ok(expr)
    }

    /// Attempts to look up an identifier within all frames of this context
    pub fn lookup(&self, ident: &Node<Ident>) -> Result<&ContextDefinition, InterpretError> {
        match self.toplevel_lookup(ident) {
            Ok(def) => Ok(def),
            Err(InterpretError::UndefinedSymbol(symbol)) => match &self.prev_frame {
                Some(frame) => frame.lookup(ident),
                None => Err(InterpretError::UndefinedSymbol(symbol)),
            },
            Err(e) => Err(e),
        }
    }

    /// Registers a static identifier with the context, returning an error if it already exists
    pub fn add_static(&mut self, ident: Ident, expr: Node<Expr>) -> Result<(), InterpretError> {
        if ident.is_void() {
            return Ok(());
        }
        let None = self
            .associations
            .insert(ident.clone(), self.definitions.len())
        else {
            return Err(InterpretError::SymbolDefinedTwice(ident));
        };
        self.definitions.push(ContextDefinition::Static(expr));
        Ok(())
    }

    /// Recursively hoists the given identifier by its index, inserting a leading `^`
    fn hoist(&mut self, ident: Ident, index: usize) {
        let ident = Ident::Hoist(Box::new(ident));
        if let Some(index) = self.associations.insert(ident.clone(), index) {
            self.hoist(ident, index)
        }
    }

    /// Registers a local identifier with the context, shadowing an existing identifier if it already exists
    pub fn add_local(
        &mut self,
        ident: Ident,
        ty: Node<Expr>,
        value: Node<Expr>,
    ) -> Result<(), InterpretError> {
        if ident.is_void() {
            return Ok(());
        }
        if let Some(index) = self
            .associations
            .insert(ident.clone(), self.definitions.len())
        {
            self.hoist(ident, index)
        };
        self.definitions.push(ContextDefinition::Local(ty, value));
        Ok(())
    }

    /// Registers a function argument with the context, shadowing an existing identifier if it already exists
    pub fn add_argument(&mut self, ident: Ident, ty: Node<Expr>) -> Result<(), InterpretError> {
        if ident.is_void() {
            return Ok(());
        }
        if let Some(index) = self
            .associations
            .insert(ident.clone(), self.definitions.len())
        {
            self.hoist(ident, index)
        };
        self.definitions.push(ContextDefinition::Argument(ty));
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

        let mut add_prim = |ident, primitive| {
            context
                .add_static(ident, Expr::Primitive(primitive).unloc())
                .expect("name conflict should not happen");
        };

        add_prim(Ident::TIdent("U64".into()), Primitive::U64Ty);
        add_prim(Ident::TIdent("Char".into()), Primitive::CharTy);
        add_prim(Ident::TIdent("Bool".into()), Primitive::Bool);

        add_prim(Ident::VIdent("true".into()), Primitive::True);
        add_prim(Ident::VIdent("false".into()), Primitive::False);

        add_prim(Ident::VIdent("debug_print".into()), Primitive::DebugPrint);

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
#[derive(Clone, Debug)]
pub enum InterpretError {
    /// An undefined symbol was encountered
    UndefinedSymbol(Node<Ident>),

    /// An undefined field was encountered
    UndefinedField(Node<Field>),

    /// A symbol was attempted to be defined twice
    SymbolDefinedTwice(Ident),

    /// An illegal binary operation was used
    IllegalBinop(&'static str, Node<Binop>),

    /// A function received the wrong number of arguments (expected, actual)
    IncorrectNumberOfArguments(&'static str, usize, usize),

    /// A function received an argument of the wrong name (expected, actual)
    ArgumentNameMismatch(&'static str, Ident, Node<Ident>),

    /// Tried to access a field of a non-struct value
    IllegalFieldOperation(&'static str, Node<Expr>),

    /// Tried to call a value that is not a function or constructor
    IllegalCallOperation(&'static str, Node<Expr>),

    /// Tried to use the void identifier `_` as an expression
    VoidIsUndefined,

    /// An interpreter feature is not yet implemented
    Unimplemented(&'static str),

    /// The provided type is not a subtype of what was expected
    NotASubtype(&'static str, Node<Expr>, Node<Expr>),

    /// An unnamed member was expected, but a name was provided
    UnexpectedMemberName(&'static str, Node<Ident>),

    /// Tried to evaluate a type in the "eval" stage of the interpreter
    CannotEvaluateType(&'static str, Node<Expr>),
}

/// Trait for interpreting expressions
pub trait Interpret {
    /// The output of the interpreter for this type
    type Output;

    /// Interprets the code with the given context
    fn interp(self, context: &mut Context) -> Result<Self::Output, InterpretError>;
}
