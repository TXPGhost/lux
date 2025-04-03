use crate::{
    arena::Handle,
    ast::{desugar::*, Node},
};

use super::{PrettyPrint, PrettyPrintContext};

/// Context for pretty printing the parse tree
#[derive(Clone, Copy)]
pub struct Context {
    indent_level: usize,
    multiline: bool,
    forbid_multiline: bool,
    max_depth: usize,
    ident_and_prim_hints: bool,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            indent_level: 0,
            multiline: true,
            forbid_multiline: false,
            max_depth: usize::MAX,
            ident_and_prim_hints: true,
        }
    }
}

impl PrettyPrintContext for Context {
    fn indent_level(&self) -> usize {
        self.indent_level
    }

    fn indented(self) -> Self {
        Self {
            indent_level: self.indent_level + 1,
            ..self
        }
    }
}

impl Context {
    /// Returns [true] if the context is requesting multiline output
    pub fn is_multiline(&self) -> bool {
        self.multiline && !self.forbid_multiline
    }

    /// Returns the context with the given [multiline] option
    pub fn multiline(self, multiline: bool) -> Self {
        Self { multiline, ..self }
    }

    /// Returns the context with the given maximum traversal depth
    pub fn max_depth(self, max_depth: usize) -> Self {
        Self { max_depth, ..self }
    }

    /// Returns the context with multiline forbidden
    pub fn forbid_multiline(self) -> Self {
        Self {
            forbid_multiline: true,
            ..self
        }
    }

    /// Returns the context without identifier and primitive hints
    pub fn no_ident_and_prim_hints(self) -> Self {
        Self {
            ident_and_prim_hints: false,
            ..self
        }
    }

    fn depth_check(&mut self) -> bool {
        if self.max_depth == 0 {
            return true;
        }
        self.max_depth -= 1;
        false
    }
}

impl PrettyPrint for Handle<Node<Member>> {
    type State = DesugarArena;
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        if context.depth_check() {
            return write!(f, "member${}", self.get_idx());
        }
        let member = &state.members.get(*self).val;
        member.field.val.pretty_print(f, state, context)?;
        write!(f, ": ")?;
        member.expr.pretty_print(f, state, context)?;
        Ok(())
    }
}

impl PrettyPrint for Handle<Node<Expr>> {
    type State = DesugarArena;
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        let expr = &state.exprs.get(*self).val;
        if context.depth_check() {
            if context.ident_and_prim_hints {
                if let Expr::Ident(ident) = expr {
                    return write!(
                        f,
                        "expr${}<{}>",
                        self.get_idx(),
                        ident.val.printable_ctx(state, *context)
                    );
                }
                if let Expr::Primitive(primitive) = expr {
                    return write!(f, "expr${}<{}>", self.get_idx(), primitive.printable(&()));
                }
            }
            return write!(f, "expr${}", self.get_idx());
        }
        match expr {
            Expr::Ident(ident) => ident.val.pretty_print(f, state, context)?,
            Expr::Index(expr, index) => {
                expr.pretty_print(f, state, context)?;
                write!(f, "[")?;
                expr.pretty_print(f, state, context)?;
                write!(f, "]")?;
            }
            Expr::Field(expr, field) => {
                expr.pretty_print(f, state, context)?;
                write!(f, ".")?;
                field.val.pretty_print(f, state, context)?;
            }
            Expr::Struct(member_list) => {
                write!(f, "(")?;
                member_list.pretty_print(f, state, context)?;
                write!(f, ")")?;
            }
            Expr::Enum(member_list) => {
                write!(f, "<")?;
                member_list.pretty_print(f, state, context)?;
                write!(f, ">")?;
            }
            Expr::Call(func, args) => {
                func.pretty_print(f, state, context)?;
                write!(f, "(")?;
                args.pretty_print(f, state, &mut context.multiline(false))?;
                write!(f, ")")?;
            }
            Expr::Func(args, body) => {
                write!(f, "(")?;
                args.pretty_print(f, state, context)?;
                write!(f, ") => ")?;
                body.pretty_print(f, state, context)?;
            }
            Expr::Block(block) => {
                block.pretty_print(f, state, context)?;
            }
            Expr::Array(array) => todo!(),
            Expr::ArrayType(len, ty) => {
                write!(f, "[")?;
                if let Some(len) = len {
                    len.pretty_print(f, state, context)?;
                }
                write!(f, "]")?;
                ty.pretty_print(f, state, context)?;
            }
            Expr::Primitive(primitive) => write!(f, "{}", primitive.printable(&()))?,
        }
        Ok(())
    }
}

impl PrettyPrint for Ident {
    type State = DesugarArena;
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        match self {
            Ident::VIdent(vident) => write!(f, "{}", vident),
            Ident::TIdent(tident) => write!(f, "{}", tident),
            Ident::Hoist(ident) => {
                write!(f, "^")?;
                ident.pretty_print(f, state, context)
            }
            Ident::Resolved(handle) => write!(f, "expr${}", handle.get_idx()),
        }
    }
}

impl PrettyPrint for Field {
    type State = DesugarArena;
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        match self {
            Field::Ident(ident) => ident.val.pretty_print(f, state, context),
            Field::Number(number) => write!(f, "{}", number),
        }
    }
}

impl PrettyPrint for Handle<Node<Stmt>> {
    type State = DesugarArena;
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        if context.depth_check() {
            return write!(f, "stmt${}", self.get_idx());
        }
        let stmt = &state.stmts.get(*self).val;
        if let Some(ident) = &stmt.ident {
            ident.val.pretty_print(f, state, context)?;
            write!(f, ": ")?;
            stmt.ty.pretty_print(f, state, context)?;
            write!(f, " = ")?;
        }
        stmt.value.pretty_print(f, state, context)?;
        Ok(())
    }
}

impl PrettyPrint for Handle<Node<MemberList>> {
    type State = DesugarArena;
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        if context.depth_check() {
            return write!(f, "member_list${}", self.get_idx());
        }
        let member_list = &state.member_lists.get(*self).val;
        for (i, member) in member_list.members.iter().enumerate() {
            if context.is_multiline() {
                self.indent(f, context)?;
            }
            member.pretty_print(f, state, context)?;
            if context.is_multiline() {
                writeln!(f)?;
            } else if i != member_list.members.len() - 1 {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}

impl PrettyPrint for Handle<Node<Block>> {
    type State = DesugarArena;
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        if context.depth_check() {
            return write!(f, "block${}", self.get_idx());
        }
        let block = &state.blocks.get(*self).val;
        {
            let context = &mut context.indented();
            if context.is_multiline() {
                writeln!(f, "{{")?;
            } else {
                write!(f, "{{ ")?;
            }
            for (i, stmt) in block.stmts.iter().enumerate() {
                if context.is_multiline() {
                    self.indent(f, context)?;
                }
                stmt.pretty_print(f, state, context)?;
                if context.is_multiline() {
                    writeln!(f)?;
                } else if i != block.stmts.len() - 1 {
                    write!(f, ", ")?;
                }
            }
        }
        if context.is_multiline() {
            self.indent(f, context)?;
            write!(f, "}}")?;
        } else {
            write!(f, " }}")?;
        }
        Ok(())
    }
}

impl PrettyPrint for DesugarArena {
    type State = ();
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        let ctx = Context::default().forbid_multiline();
        for item in self.exprs.iter_handles() {
            writeln!(
                f,
                "{} ::: {}",
                item.printable_ctx(self, ctx.max_depth(0).no_ident_and_prim_hints()),
                item.printable_ctx(self, ctx.max_depth(1))
            )?;
        }
        for item in self.members.iter_handles() {
            writeln!(
                f,
                "{} ::: {}",
                item.printable_ctx(self, ctx.max_depth(0).no_ident_and_prim_hints()),
                item.printable_ctx(self, ctx.max_depth(1))
            )?;
        }
        for item in self.blocks.iter_handles() {
            writeln!(
                f,
                "{} ::: {}",
                item.printable_ctx(self, ctx.max_depth(0).no_ident_and_prim_hints()),
                item.printable_ctx(self, ctx.max_depth(1))
            )?;
        }
        for item in self.member_lists.iter_handles() {
            writeln!(
                f,
                "{} ::: {}",
                item.printable_ctx(self, ctx.max_depth(0).no_ident_and_prim_hints()),
                item.printable_ctx(self, ctx.max_depth(1))
            )?;
        }
        Ok(())
    }
}
