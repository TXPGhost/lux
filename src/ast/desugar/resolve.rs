use crate::arena::Handle;

use super::{
    lookup::{Lookup, LookupError},
    *,
};

/// "Resolves" identifiers within the desugared AST
pub trait Resolve {
    /// "Resolves" identifiers within [self] using the given [DesugarArena] and [Parent]
    fn resolve(self, arena: &mut DesugarArena, parent: Parent) -> Result<(), LookupError>;
}

impl Resolve for Handle<Node<Expr>> {
    fn resolve(self, arena: &mut DesugarArena, parent: Parent) -> Result<(), LookupError> {
        let expr = arena.exprs.get(self);
        let loc = expr.loc;
        match &expr.val {
            Expr::Ident(ident) => match parent.lookup(arena, ident) {
                Ok(expr) => {
                    arena.exprs.get_mut(self).val = Expr::Ident(Ident::Resolved(expr).node(loc));
                    Ok(())
                }
                Err(e) => Err(e),
            },
            Expr::Struct(fields) => fields.resolve(arena, parent),
            Expr::Enum(variants) => variants.resolve(arena, parent),
            Expr::Block(block) => block.resolve(arena, parent),
            Expr::Call(func, args) => {
                let func = *func;
                let args = *args;
                func.resolve(arena, parent)?;
                args.resolve(arena, parent)?;
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

impl Resolve for Handle<Node<MemberList>> {
    fn resolve(self, arena: &mut DesugarArena, _: Parent) -> Result<(), LookupError> {
        let members = arena.member_lists.get(self);
        let members = members.val.members.clone();
        let parent = Parent::MemberList(self);
        for member in members {
            member.resolve(arena, parent)?;
        }
        Ok(())
    }
}

impl Resolve for Handle<Node<Block>> {
    fn resolve(self, arena: &mut DesugarArena, _: Parent) -> Result<(), LookupError> {
        let block = arena.blocks.get(self);
        let stmts = block.val.stmts.clone();
        let parent = Parent::Block(self);
        for stmt in stmts {
            stmt.resolve(arena, parent)?;
        }
        Ok(())
    }
}

impl Resolve for Handle<Node<Member>> {
    fn resolve(self, arena: &mut DesugarArena, parent: Parent) -> Result<(), LookupError> {
        let member = arena.members.get(self);
        member.val.expr.resolve(arena, parent)
    }
}

impl Resolve for Handle<Node<Stmt>> {
    fn resolve(self, arena: &mut DesugarArena, parent: Parent) -> Result<(), LookupError> {
        //let stmt = arena.stmts.get(self);
        //if let Some(ident) = stmt.val.ident {
        //    ident.resolve(arena, parent)?;
        //}
        //stmt.val.ty.resolve(arena, parent)?;
        //stmt.val.value.resolve(arena, parent)?;
        Ok(())
    }
}
