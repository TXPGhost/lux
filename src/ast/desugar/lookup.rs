use crate::arena::Handle;

use super::*;

/// Indicates that a type is capable of lookup of identifiers
pub trait Lookup {
    /// Recursively looks up the given identifier
    fn lookup(
        &self,
        arena: &DesugarArena,
        ident: &Node<Ident>,
    ) -> Result<Handle<Node<Expr>>, LookupError>;
}

/// An error that can occur during a lookup
#[derive(Clone, Debug)]
pub struct LookupError(pub &'static str, pub Node<Ident>);

impl Lookup for Parent {
    fn lookup(
        &self,
        arena: &DesugarArena,
        ident: &Node<Ident>,
    ) -> Result<Handle<Node<Expr>>, LookupError> {
        match self {
            Parent::MemberList(members) => members.lookup(arena, ident),
            Parent::Block(block) => block.lookup(arena, ident),
        }
    }
}

impl Lookup for Handle<Node<Block>> {
    fn lookup(
        &self,
        arena: &DesugarArena,
        ident: &Node<Ident>,
    ) -> Result<Handle<Node<Expr>>, LookupError> {
        // TODO: shadowing
        let block = arena.blocks.get(*self);
        for stmt in &block.val.stmts {
            // TODO: type lookup?
            let stmt = arena.stmts.get(*stmt);
            if let Some(stmt_ident) = &stmt.val.ident {
                if stmt_ident.val == ident.val {
                    return Ok(stmt.val.value);
                }
            }
        }

        match block.val.parent {
            Some(parent) => parent.lookup(arena, ident),
            None => Err(LookupError("unable to resolve identifier", ident.clone())),
        }
    }
}

impl Lookup for Handle<Node<MemberList>> {
    fn lookup(
        &self,
        arena: &DesugarArena,
        ident: &Node<Ident>,
    ) -> Result<Handle<Node<Expr>>, LookupError> {
        let members = arena.member_lists.get(*self);
        for member in &members.val.members {
            let member = arena.members.get(*member);
            if let Field::Ident(field_ident) = &member.val.field.val {
                if field_ident.val == ident.val {
                    return Ok(member.val.expr);
                }
            }
        }

        match members.val.parent {
            Some(parent) => parent.lookup(arena, ident),
            None => Err(LookupError("unable to resolve identifier", ident.clone())),
        }
    }
}
