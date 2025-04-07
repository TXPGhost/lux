use crate::{
    arena::Handle,
    ast::{desugar::*, Node},
};

use super::TypeArena;

/// The result of a type comparison operation between `A` and `B`
pub enum TypeComparison {
    /// The two types `A` and `B` are identical
    Equal,

    /// Type `A` is a subtype of `B`
    Subtype,

    /// Type `A` is a supertype of `B`
    Supertype,

    /// Types `A` and `B` have no relation
    NoRelation,
}

impl TypeComparison {
    /// Returns [true] if the two types `A` and `B` are identical
    pub fn is_equal(&self) -> bool {
        matches!(self, TypeComparison::Equal)
    }

    /// Returns [true] if type `A` is a subtype of `B`
    pub fn is_subtype(&self) -> bool {
        matches!(self, TypeComparison::Equal)
    }

    /// Returns [true] if type `A` is a supertype of `B`
    pub fn is_supertype(&self) -> bool {
        matches!(self, TypeComparison::Equal)
    }
}

pub trait TypeCompare {
    fn type_compare(self, other: Self, arena: &DesugarArena, types: &TypeArena) -> TypeComparison;
}

impl TypeCompare for Handle<Node<Expr>> {
    fn type_compare(self, other: Self, arena: &DesugarArena, types: &TypeArena) -> TypeComparison {
        let lhs = &types.exprs.get(self).unwrap();
        let rhs = &types.exprs.get(other).unwrap();
        match (lhs, rhs) {
            (Expr::Ident(ident), _) => match ident.val {
                Ident::Resolved(expr) => expr.type_compare(other, arena, types),
                _ => unreachable!("all identifiers should be resolved by now"),
            },
            (_, Expr::Ident(ident)) => match ident.val {
                Ident::Resolved(expr) => self.type_compare(expr, arena, types),
                _ => unreachable!("all identifiers should be resolved by now"),
            },
            _ => todo!("type comparison"),
        }
    }
}

impl TypeCompare for Handle<Node<MemberList>> {
    fn type_compare(self, other: Self, arena: &DesugarArena, types: &TypeArena) -> TypeComparison {
        todo!()
    }
}
