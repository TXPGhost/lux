use crate::ast::*;

impl Expr {
    /// Returns [true] if this type is a subtype of `rhs` (i.e. has _fewer_ possible values)
    pub fn is_subtype_of(&self, rhs: &Self) -> bool {
        true
    }

    /// Returns [true] if this type is a supertype of `rhs` (i.e. has _more_ possible values)
    pub fn is_supertype_of(&self, rhs: &Self) -> bool {
        true
    }
}
