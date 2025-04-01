use crate::arena::Handle;

use super::{desugar::*, Node};

/// A resolved identifier, which points to an expression in the [ASTArena]
#[derive(Clone, Debug)]
pub struct ResolvedIdent {
    /// The expression the identifier resolves to
    pub expr: Handle<Node<Expr<ResolvedIdent>>>,
}

impl IdentTy for ResolvedIdent {}

/// Trait to "resolve" an AST from having [RawIdent] identifiers to having [ResolvedIdent]
pub trait Resolve {}

/// Indicates that a type is capable of lookup of identifiers
pub trait Lookup<I: IdentTy> {
    /// Recursively looks up the given identifier
    fn lookup(
        &self,
        arena: &ASTArena<I>,
        ident: Ident,
    ) -> Result<Handle<Node<Expr<I>>>, LookupError>;
}

/// An error that can occur during a lookup
pub struct LookupError;

impl<I: IdentTy> Lookup<I> for Parent<I> {
    fn lookup(
        &self,
        arena: &ASTArena<I>,
        ident: Ident,
    ) -> Result<Handle<Node<Expr<I>>>, LookupError> {
        match self {
            Parent::Members(members) => members.lookup(arena, ident),
            Parent::Block(block) => block.lookup(arena, ident),
        }
    }
}

impl<I: IdentTy> Lookup<I> for Handle<Node<Block<I>>> {
    fn lookup(
        &self,
        arena: &ASTArena<I>,
        ident: Ident,
    ) -> Result<Handle<Node<Expr<I>>>, LookupError> {
        // TODO: shadowing
        let block = arena.blocks.get(*self);
        for stmt in &block.val.stmts {
            // TODO: type lookup?
            if let Some(stmt_ident) = &stmt.val.ident {
                if stmt_ident.val == ident {
                    return Ok(stmt.val.value);
                }
            }
        }

        match block.val.parent {
            Some(parent) => parent.lookup(arena, ident),
            None => Err(LookupError),
        }
    }
}

impl<I: IdentTy> Lookup<I> for Handle<Node<Members<I>>> {
    fn lookup(
        &self,
        arena: &ASTArena<I>,
        ident: Ident,
    ) -> Result<Handle<Node<Expr<I>>>, LookupError> {
        let members = arena.members.get(*self);
        for member in &members.val.members {
            if let Field::Ident(field_ident) = &member.val.field.val {
                if field_ident.val == ident {
                    return Ok(member.val.expr);
                }
            }
        }

        match &members.val.parent {
            Some(parent) => parent.lookup(arena, ident),
            None => Err(LookupError),
        }
    }
}
