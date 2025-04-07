use type_compare::TypeCompare;

use crate::{
    arena::{ArenaMap, Handle},
    ast::{desugar::*, Node},
};

/// Compares two types
pub mod type_compare;

/// The result of constant evaluation, a mapping from desugared handles to (type) expressions
pub struct TypeArena {
    member_lists: ArenaMap<Node<MemberList>, Expr>,
    members: ArenaMap<Node<Member>, Expr>,
    blocks: ArenaMap<Node<Block>, Expr>,
    exprs: ArenaMap<Node<Expr>, Expr>,
    stmts: ArenaMap<Node<Stmt>, Expr>,
}

/// An error that can occur during constant evaluation
pub enum TypeError {
    ExpectedFunction,
    FunctionArgumentMismatch,
}

/// Indicates that a type can be constant-evaluated
pub trait AssignTypes: Sized {
    /// Assigns types to the given type
    fn assign_types(self, arena: &DesugarArena, types: &mut TypeArena) -> Result<(), TypeError>;
}

impl AssignTypes for Handle<Node<Expr>> {
    fn assign_types(self, arena: &DesugarArena, types: &mut TypeArena) -> Result<(), TypeError> {
        let expr = &arena.exprs.get(self).val;
        match expr {
            Expr::Ident(ident) => match ident.val {
                Ident::Resolved(expr) => {
                    expr.assign_types(arena, types);
                    Ok(())
                }
                _ => unreachable!("all identifiers should be resolved by now"),
            },
            Expr::Index(handle, handle1) => todo!("consteval index"),
            Expr::Field(handle, node) => todo!("consteval field"),
            Expr::Struct(handle) => todo!("consteval struct"),
            Expr::Enum(handle) => todo!("consteval enum"),
            Expr::Call(func, args) => {
                func.assign_types(arena, types)?;
                args.assign_types(arena, types)?;

                let Expr::Func(fargs, fbody) = arena.exprs.get(*func).val else {
                    return Err(TypeError::ExpectedFunction);
                };

                fargs.assign_types(arena, types)?;
                fbody.assign_types(arena, types)?;

                let comparison = args.type_compare(fargs, arena, types);
                if !comparison.is_subtype() {
                    return Err(TypeError::FunctionArgumentMismatch);
                }

                Ok(())
            }
            Expr::Func(handle, handle1) => todo!("consteval func"),
            Expr::Block(handle) => todo!("consteval block"),
            Expr::Array(elements) => todo!("consteval array"),
            Expr::Vector(handle, handle1) => todo!("consteval array type"),
            Expr::Primitive(primitive) => {
                types.exprs.insert(self, Expr::Primitive(primitive.clone()));
                Ok(())
            }
        }
    }
}

impl AssignTypes for Handle<Node<MemberList>> {
    fn assign_types(self, arena: &DesugarArena, types: &mut TypeArena) -> Result<(), TypeError> {
        todo!()
    }
}
