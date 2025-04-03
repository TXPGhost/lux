use std::fmt::{Display, Formatter, Result};

/// Pretty printer for the parse tree
pub mod parse_tree;

/// Pretty printer for the desugared AST
pub mod desugar;

/// Indicates that a type is pretty-printable
pub trait PrettyPrint: Sized {
    /// External state to read from (e.g. an arena)
    type State;

    /// The context used for pretty printing (e.g. indentation)
    type Context: PrettyPrintContext;

    /// Recursively pretty prints the type using the given [Formatter] and pretty-print context
    fn pretty_print(
        &self,
        f: &mut Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> Result;

    /// Returns a wrapper of this type that implements the [Display] trait
    fn printable<'a>(&'a self, state: &'a Self::State) -> PrettyPrintable<'a, Self> {
        PrettyPrintable(self, state, Self::Context::default())
    }

    /// Returns a wrapper of this type that implements the [Display] trait with the given context
    fn printable_ctx<'a>(
        &'a self,
        state: &'a Self::State,
        context: Self::Context,
    ) -> PrettyPrintable<'a, Self> {
        PrettyPrintable(self, state, context)
    }

    /// Prints the current indentation level
    fn indent(&self, f: &mut Formatter<'_>, context: &Self::Context) -> Result {
        write!(f, "{}", "    ".repeat(context.indent_level()))
    }
}

/// A wrapper struct that allows a pretty-printable type to implement [Display]
pub struct PrettyPrintable<'a, T: PrettyPrint>(&'a T, &'a T::State, T::Context);

impl<T: PrettyPrint> Display for PrettyPrintable<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0.pretty_print(f, self.1, &mut self.2.clone())
    }
}

/// A type that supports being used as a pretty print context
pub trait PrettyPrintContext: Clone + Default {
    /// Returns the current indentation level
    fn indent_level(&self) -> usize;

    /// Increases the current indendentation level by one
    fn indented(self) -> Self;
}
