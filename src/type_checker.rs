#![allow(clippy::match_like_matches_macro)]

use crate::{
    ast::{parse_tree::*, *},
    interpreter::{Context, ContextDefinition, InterpretError},
};

/// A trait used to compare two types
pub trait TypeCompare {
    /// Returns [Ok(true)] if this is a type (i.e. does not have _exactly_ one possible value)
    fn is_type(&self, context: &Context) -> Result<bool, InterpretError>;

    /// Returns [Ok(true)] if this type is equivalent to `rhs`
    fn is_equivalent_to(&self, rhs: &Self, context: &Context) -> Result<bool, InterpretError>;

    /// Returns [Ok(true)] if this type is a subtype of `rhs` (i.e. has _fewer_ possible values)
    fn is_subtype_of(&self, rhs: &Self, context: &Context) -> Result<bool, InterpretError>;

    /// Returns [Ok(true)] if this type is a supertype of `rhs` (i.e. has _more_ possible values)
    fn is_supertype_of(&self, rhs: &Self, context: &Context) -> Result<bool, InterpretError> {
        if self.is_equivalent_to(rhs, context)? {
            return Ok(true);
        }
        Ok(!self.is_subtype_of(rhs, context)?)
    }
}

impl TypeCompare for Expr {
    fn is_type(&self, context: &Context) -> Result<bool, InterpretError> {
        match self {
            Expr::Ident(ident) => context.lookup(ident)?.ty().val.is_type(context),
            Expr::Primitive(prim) => prim.is_type(context),
            _ => todo!(),
        }
    }

    fn is_equivalent_to(&self, rhs: &Self, context: &Context) -> Result<bool, InterpretError> {
        match (self, rhs) {
            (Expr::Ident(lhs), rhs) => {
                let lhs = &context.lookup(lhs)?.ty().val;
                lhs.is_equivalent_to(rhs, context)
            }
            (lhs, Expr::Ident(rhs)) => {
                let rhs = &context.lookup(rhs)?.ty().val;
                lhs.is_equivalent_to(rhs, context)
            }
            (Expr::Binop(lhs), Expr::Binop(rhs)) => Ok(lhs.val.op.val == rhs.val.op.val
                && lhs
                    .val
                    .lhs
                    .val
                    .is_equivalent_to(&rhs.val.lhs.val, context)?
                && rhs
                    .val
                    .rhs
                    .val
                    .is_equivalent_to(&rhs.val.rhs.val, context)?),
            (Expr::Index(larr, lidx), Expr::Index(rarr, ridx)) => {
                Ok(lidx.val.is_equivalent_to(&ridx.val, context)?
                    && larr.val.is_equivalent_to(&rarr.val, context)?)
            }
            (Expr::Field(lstruct, lfield), Expr::Field(rstruct, rfield)) => {
                Ok(lfield.val.is_equivalent_to(&rfield.val, context)?
                    && lstruct.val.is_equivalent_to(&rstruct.val, context)?)
            }
            (Expr::Struct(lhs) | Expr::Enum(lhs), Expr::Struct(rhs) | Expr::Enum(rhs)) => {
                if lhs.val.len() != rhs.val.len() {
                    return Ok(false);
                }
                for i in 0..lhs.val.len() {
                    if !&lhs.val[i].val.is_equivalent_to(&rhs.val[i].val, context)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (Expr::Primitive(lhs), Expr::Primitive(rhs)) => lhs.is_equivalent_to(rhs, context),
            _ => Ok(false),
        }
    }

    fn is_subtype_of(&self, rhs: &Self, context: &Context) -> Result<bool, InterpretError> {
        if self.is_equivalent_to(rhs, context)? {
            return Ok(true);
        }
        match (self, rhs) {
            (Expr::Primitive(Primitive::U64Val(_)), Expr::Primitive(Primitive::U64Ty)) => Ok(true),
            _ => Ok(false),
        }
    }
}

impl TypeCompare for Ident {
    fn is_type(&self, context: &Context) -> Result<bool, InterpretError> {
        context
            .lookup(&self.clone().unloc())?
            .ty()
            .val
            .is_type(context)
    }

    fn is_equivalent_to(&self, rhs: &Self, context: &Context) -> Result<bool, InterpretError> {
        let lhs = match context.lookup(&self.clone().unloc())? {
            ContextDefinition::Static(ty) => ty,
            ContextDefinition::Local(ty, _) => ty,
            ContextDefinition::Argument(ty) => ty,
        };
        let rhs = match context.lookup(&rhs.clone().unloc())? {
            ContextDefinition::Static(ty) => ty,
            ContextDefinition::Local(ty, _) => ty,
            ContextDefinition::Argument(ty) => ty,
        };
        lhs.val.is_equivalent_to(&rhs.val, context)
    }

    fn is_subtype_of(&self, rhs: &Self, context: &Context) -> Result<bool, InterpretError> {
        let lhs = match context.lookup(&self.clone().unloc())? {
            ContextDefinition::Static(ty) => ty,
            ContextDefinition::Local(ty, _) => ty,
            ContextDefinition::Argument(ty) => ty,
        };
        let rhs = match context.lookup(&rhs.clone().unloc())? {
            ContextDefinition::Static(ty) => ty,
            ContextDefinition::Local(ty, _) => ty,
            ContextDefinition::Argument(ty) => ty,
        };
        lhs.val.is_subtype_of(&rhs.val, context)
    }
}

impl TypeCompare for Field {
    fn is_type(&self, context: &Context) -> Result<bool, InterpretError> {
        match self {
            Field::Ident(ident) => ident.is_type(context),
            Field::Number(_) => Ok(false),
        }
    }

    fn is_equivalent_to(&self, rhs: &Self, context: &Context) -> Result<bool, InterpretError> {
        match (self, rhs) {
            (Field::Ident(lhs), Field::Ident(rhs)) => lhs.is_equivalent_to(rhs, context),
            (Field::Number(lhs), Field::Number(rhs)) => Ok(lhs == rhs),
            _ => Ok(false),
        }
    }

    fn is_subtype_of(&self, rhs: &Self, context: &Context) -> Result<bool, InterpretError> {
        match (self, rhs) {
            (Field::Ident(lhs), Field::Ident(rhs)) => lhs.is_subtype_of(rhs, context),
            (Field::Number(lhs), Field::Number(rhs)) => Ok(lhs == rhs),
            _ => Ok(false),
        }
    }
}

impl TypeCompare for Member {
    fn is_type(&self, _: &Context) -> Result<bool, InterpretError> {
        todo!()
    }

    fn is_equivalent_to(&self, rhs: &Self, context: &Context) -> Result<bool, InterpretError> {
        match (self, rhs) {
            (Member::NamedFunc(_, _, _), _) => unreachable!(),
            (_, Member::NamedFunc(_, _, _)) => unreachable!(),
            (Member::Expr(lhs), Member::Expr(rhs)) => lhs.val.is_equivalent_to(&rhs.val, context),
            (Member::Named(lident, lexpr), Member::Named(rident, rexpr)) => {
                Ok(lident.val == rident.val && lexpr.val.is_equivalent_to(&rexpr.val, context)?)
            }
            _ => Ok(false),
        }
    }

    fn is_subtype_of(&self, rhs: &Self, context: &Context) -> Result<bool, InterpretError> {
        match (self, rhs) {
            (Member::NamedFunc(_, _, _), _) => unreachable!(),
            (_, Member::NamedFunc(_, _, _)) => unreachable!(),
            (Member::Expr(lhs), Member::Expr(rhs)) => lhs.val.is_subtype_of(&rhs.val, context),
            (Member::Named(lident, lexpr), Member::Named(rident, rexpr)) => {
                Ok(lident.val == rident.val && lexpr.val.is_subtype_of(&rexpr.val, context)?)
            }
            _ => Ok(false),
        }
    }
}

impl TypeCompare for Primitive {
    fn is_type(&self, _: &Context) -> Result<bool, InterpretError> {
        match self {
            Primitive::U64Ty => Ok(true),
            Primitive::U64Val(_) => Ok(false),
            Primitive::CharTy => Ok(true),
            Primitive::CharVal(_) => Ok(false),
            Primitive::Bool => Ok(true),
            Primitive::True => Ok(false),
            Primitive::False => Ok(false),
            Primitive::DebugPrint => Ok(false),
            Primitive::Assert(_) => Ok(false),
            Primitive::Binop(_) => Ok(false),
            Primitive::Unop(_) => Ok(false),
        }
    }

    fn is_equivalent_to(&self, rhs: &Self, _: &Context) -> Result<bool, InterpretError> {
        Ok(self == rhs)
    }

    fn is_subtype_of(&self, rhs: &Self, context: &Context) -> Result<bool, InterpretError> {
        if self.is_equivalent_to(rhs, context)? {
            return Ok(true);
        }
        match (self, rhs) {
            (Primitive::U64Val(_), Primitive::U64Ty) => Ok(true),
            (Primitive::True, Primitive::Bool) => Ok(true),
            (Primitive::False, Primitive::Bool) => Ok(true),
            _ => Ok(false),
        }
    }
}
