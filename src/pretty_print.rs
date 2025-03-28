use std::fmt::{Display, Formatter, Result};

/// Indicates that a type is pretty-printable
pub trait PrettyPrint: Sized {
    /// The context used for pretty printing (e.g. indentation)
    type Context: Default;

    /// Recursively pretty prints the type using the given [Formatter] and pretty-print context
    fn pretty_print(&self, f: &mut Formatter<'_>, context: &mut Self::Context) -> Result;

    /// Returns a wrapper of this type that implements the [Display] trait
    fn printable(&self) -> PrettyPrintable<Self> {
        PrettyPrintable(self)
    }
}

/// A wrapper struct that allows a pretty-printable type to implement [Display]
pub struct PrettyPrintable<'a, T: PrettyPrint>(&'a T);

impl<T: PrettyPrint> Display for PrettyPrintable<'_, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0.pretty_print(f, &mut T::Context::default())
    }
}
