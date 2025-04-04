use crate::{
    arena::{Arena, Handle},
    ast::Node,
};

use super::*;

/// Assigns types to every expression
pub struct Types {
    /// A global arena of all expression types, using the same indices as the [DesugarArena]
    pub types: Arena<Option<Expr>>,
}

impl Types {
    /// Constructs new [Types] from the given [DesugarArena], with each type initially [None]
    pub fn new_from_arena(arena: &DesugarArena) -> Self {
        Self {
            types: Arena::new_filled(None, arena.exprs.num_entries()),
        }
    }
}

/// A helper trait to assign types to expressions
pub trait AssignTypes {
    /// Assigns types for the given handle
    fn assign_types(self, arena: &mut DesugarArena, types: &mut Types);
}

impl AssignTypes for Handle<Node<Expr>> {
    fn assign_types(self, arena: &mut DesugarArena, types: &mut Types) {
        todo!()
    }
}
