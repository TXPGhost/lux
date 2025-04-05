use crate::{
    arena::{Arena, Handle},
    ast::Node,
};

use super::*;

/// Assigns types to every expression
pub struct Types {
    /// A global arena of all expression types, using the same indices as the [DesugarArena]
    pub types: Arena<Type>,
}

/// The type of an expression
#[derive(Clone, Debug)]
pub enum Type {
    /// The type is currently unknown
    Unknown,

    /// The type is currently in the process of being discovered
    InProgress,

    /// The type is known
    Known(Expr),
}

impl Types {
    /// Assigns [Types] from the given [DesugarArena]
    pub fn new_from_arena(arena: &DesugarArena) -> Self {
        let mut types = Self {
            types: Arena::new_filled(Type::Unknown, arena.exprs.num_entries()),
        };
        types
    }
}

impl Handle<Node<Expr>> {
    pub fn get_type(self, types: &Types) -> &Type {
        types.types.get(self.transmute())
    }
}

trait AssignTypes {
    fn assign_types(self, arena: &mut DesugarArena, types: &mut Types);
}

impl AssignTypes for Handle<Node<Expr>> {
    fn assign_types(self, arena: &mut DesugarArena, types: &mut Types) {
        let expr = arena.exprs.get(self);
        match &expr.val {
            Expr::Ident(ident) => match ident.val {
                Ident::Resolved(expr) => expr.assign_types(arena, types),
                _ => panic!("unresolved identifier {:?}", ident),
            },
            Expr::Index(array, index) => {
                let array = *array;
                let index = *index;
                array.assign_types(arena, types);
                index.assign_types(arena, types);
            }
            Expr::Field(handle, node) => todo!(),
            Expr::Struct(handle) => todo!(),
            Expr::Enum(handle) => todo!(),
            Expr::Call(handle, handle1) => todo!(),
            Expr::Func(handle, handle1) => todo!(),
            Expr::Block(handle) => todo!(),
            Expr::Array(node) => todo!(),
            Expr::ArrayType(handle, handle1) => todo!(),
            Expr::Primitive(primitive) => todo!(),
        }
    }
}
