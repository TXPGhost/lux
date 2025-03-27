#![allow(clippy::match_like_matches_macro)]

use crate::ast::*;

/// A trait used to compare two types
pub trait TypeCompare {
    /// Returns [true] if this is a type (i.e. does not have _exactly_ one possible value)
    fn is_type(&self) -> bool;

    /// Returns [true] if this type is equivalent to `rhs`
    fn is_equivalent_to(&self, rhs: &Self) -> bool;

    /// Returns [true] if this type is a subtype of `rhs` (i.e. has _fewer_ possible values)
    fn is_subtype_of(&self, rhs: &Self) -> bool;

    /// Returns [true] if this type is a supertype of `rhs` (i.e. has _more_ possible values)
    fn is_supertype_of(&self, rhs: &Self) -> bool {
        if self.is_equivalent_to(rhs) {
            return true;
        }
        !self.is_subtype_of(rhs)
    }
}

impl TypeCompare for Expr {
    fn is_type(&self) -> bool {
        match self {
            Expr::Ident(_) => todo!(),
            Expr::Primitive(prim) => prim.is_type(),
            _ => todo!(),
        }
    }

    fn is_equivalent_to(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Expr::Ident(lhs), Expr::Ident(rhs)) => lhs.val == rhs.val,
            (Expr::Binop(lhs), Expr::Binop(rhs)) => {
                lhs.val.op == rhs.val.op
                    && lhs.val.lhs.val.is_equivalent_to(&rhs.val.lhs.val)
                    && rhs.val.rhs.val.is_equivalent_to(&rhs.val.rhs.val)
            }
            (Expr::Index(larr, lidx), Expr::Index(rarr, ridx)) => {
                lidx.val.is_equivalent_to(&ridx.val) && larr.val.is_equivalent_to(&rarr.val)
            }
            (Expr::Field(lstruct, lfield), Expr::Field(rstruct, rfield)) => {
                lfield.val.is_equivalent_to(&rfield.val)
                    && lstruct.val.is_equivalent_to(&rstruct.val)
            }
            (Expr::Struct(lhs) | Expr::Enum(lhs), Expr::Struct(rhs) | Expr::Enum(rhs)) => {
                if lhs.val.elements.len() != rhs.val.elements.len() {
                    return false;
                }
                for i in 0..lhs.val.elements.len() {
                    if !&lhs.val.elements[i]
                        .val
                        .is_equivalent_to(&rhs.val.elements[i].val)
                    {
                        return false;
                    }
                }
                true
            }
            (Expr::Primitive(lhs), Expr::Primitive(rhs)) => lhs.is_equivalent_to(rhs),
            _ => false,
        }
    }

    fn is_subtype_of(&self, rhs: &Self) -> bool {
        if self.is_equivalent_to(rhs) {
            return true;
        }
        match (self, rhs) {
            (Expr::Primitive(Primitive::U64Val(_)), Expr::Primitive(Primitive::U64Ty)) => true,
            _ => false,
        }
    }
}

impl TypeCompare for Field {
    fn is_type(&self) -> bool {
        match self {
            Field::Ident(Ident::VIdent(_)) => false,
            Field::Ident(Ident::TIdent(_)) => true,
            Field::Ident(Ident::Hoist(ident)) => Field::Ident(*ident.clone()).is_type(),
            Field::Number(_) => false,
        }
    }

    fn is_equivalent_to(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Field::Ident(lhs), Field::Ident(rhs)) => lhs == rhs,
            (Field::Number(lhs), Field::Number(rhs)) => lhs == rhs,
            _ => false,
        }
    }

    fn is_subtype_of(&self, rhs: &Self) -> bool {
        self.is_equivalent_to(rhs)
    }
}

impl TypeCompare for Member {
    fn is_type(&self) -> bool {
        todo!()
    }

    fn is_equivalent_to(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Member::NamedFunc(_, _, _), _) => unreachable!(),
            (_, Member::NamedFunc(_, _, _)) => unreachable!(),
            (Member::Expr(lhs), Member::Expr(rhs)) => lhs.val.is_equivalent_to(&rhs.val),
            (Member::Named(lident, lexpr), Member::Named(rident, rexpr)) => {
                lident.val == rident.val && lexpr.val.is_equivalent_to(&rexpr.val)
            }
            _ => false,
        }
    }

    fn is_subtype_of(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Member::NamedFunc(_, _, _), _) => unreachable!(),
            (_, Member::NamedFunc(_, _, _)) => unreachable!(),
            (Member::Expr(lhs), Member::Expr(rhs)) => lhs.val.is_subtype_of(&rhs.val),
            (Member::Named(lident, lexpr), Member::Named(rident, rexpr)) => {
                lident.val == rident.val && lexpr.val.is_subtype_of(&rexpr.val)
            }
            _ => false,
        }
    }
}

impl TypeCompare for Primitive {
    fn is_type(&self) -> bool {
        match self {
            Primitive::U64Ty => true,
            Primitive::U64Val(_) => false,
            Primitive::CharTy => true,
            Primitive::CharVal(_) => false,
            Primitive::Bool => true,
            Primitive::True => false,
            Primitive::False => false,
            Primitive::DebugPrint => false,
        }
    }

    fn is_equivalent_to(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Primitive::U64Ty, Primitive::U64Ty) => true,
            (Primitive::U64Val(lhs), Primitive::U64Val(rhs)) => lhs == rhs,
            (Primitive::Bool, Primitive::Bool) => true,
            (Primitive::True, Primitive::True) => true,
            (Primitive::False, Primitive::False) => true,
            (Primitive::DebugPrint, Primitive::DebugPrint) => true,
            _ => false,
        }
    }

    fn is_subtype_of(&self, rhs: &Self) -> bool {
        if self.is_equivalent_to(rhs) {
            return true;
        }
        match (self, rhs) {
            (Primitive::U64Val(_), Primitive::U64Ty) => true,
            (Primitive::True, Primitive::Bool) => true,
            (Primitive::False, Primitive::Bool) => true,
            _ => false,
        }
    }
}
