use std::usize;

use crate::ast::{desugar::*, Node, Primitive};

use super::{PrettyPrint, PrettyPrintContext};

/// Context for pretty printing the parse tree
#[derive(Clone, Copy)]
pub struct Context {
    indent_level: usize,
    multiline: bool,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            indent_level: 0,
            multiline: true,
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
    fn multiline(self, multiline: bool) -> Self {
        Self { multiline, ..self }
    }
}

impl PrettyPrint for Member {
    type State = DesugarArena;
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        self.field.val.pretty_print(f, state, context)?;
        write!(f, ": ")?;
        state
            .exprs
            .get(self.expr)
            .val
            .pretty_print(f, state, context)?;
        Ok(())
    }
}

impl PrettyPrint for Expr {
    type State = DesugarArena;
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        match self {
            Expr::Ident(ident) => ident.val.pretty_print(f, state, context)?,
            Expr::Index(expr, index) => {
                state.exprs.get(*expr).val.pretty_print(f, state, context)?;
                write!(f, "[")?;
                state
                    .exprs
                    .get(*index)
                    .val
                    .pretty_print(f, state, context)?;
                write!(f, "]")?;
            }
            Expr::Field(expr, field) => {
                state.exprs.get(*expr).val.pretty_print(f, state, context)?;
                write!(f, ".")?;
                field.val.pretty_print(f, state, context)?;
            }
            Expr::Struct(member_list) => {
                write!(f, "(")?;
                state
                    .member_lists
                    .get(*member_list)
                    .val
                    .pretty_print(f, state, context)?;
                write!(f, ")")?;
            }
            Expr::Enum(member_list) => {
                write!(f, "<")?;
                state
                    .member_lists
                    .get(*member_list)
                    .val
                    .pretty_print(f, state, context)?;
                write!(f, ">")?;
            }
            Expr::Call(func, args) => {
                state.exprs.get(*func).val.pretty_print(f, state, context)?;
                write!(f, "(")?;
                state.member_lists.get(*args).val.pretty_print(
                    f,
                    state,
                    &mut context.multiline(false),
                )?;
                write!(f, ")")?;
            }
            Expr::Func(args, body) => {
                write!(f, "(")?;
                state
                    .member_lists
                    .get(*args)
                    .val
                    .pretty_print(f, state, context)?;
                write!(f, ") => ")?;
                state.exprs.get(*body).val.pretty_print(f, state, context)?;
            }
            Expr::Block(block) => {
                state
                    .blocks
                    .get(*block)
                    .val
                    .pretty_print(f, state, context)?;
            }
            Expr::Array(array) => todo!(),
            Expr::ArrayType(len, ty) => {
                write!(f, "[")?;
                if let Some(len) = len {
                    state.exprs.get(*len).val.pretty_print(f, state, context)?;
                }
                write!(f, "]")?;
                state.exprs.get(*ty).val.pretty_print(f, state, context)?;
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

impl PrettyPrint for Vec<Node<Member>> {
    type State = DesugarArena;
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        for (i, member) in self.iter().enumerate() {
            if context.multiline {
                self.indent(f, context)?;
            }
            member.val.pretty_print(f, state, context)?;
            if context.multiline {
                writeln!(f)?;
            } else if i != self.len() - 1 {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}

impl PrettyPrint for Vec<Node<Stmt>> {
    type State = DesugarArena;
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        for (i, stmt) in self.iter().enumerate() {
            if context.multiline {
                self.indent(f, context)?;
            }
            stmt.val.pretty_print(f, state, context)?;
            if context.multiline {
                writeln!(f)?;
            } else if i != self.len() - 1 {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}

impl PrettyPrint for Stmt {
    type State = DesugarArena;
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        if let Some(ident) = &self.ident {
            ident.val.pretty_print(f, state, context)?;
            write!(f, ": ")?;
            state
                .exprs
                .get(self.ty)
                .val
                .pretty_print(f, state, context)?;
            write!(f, " = ")?;
        }
        state
            .exprs
            .get(self.value)
            .val
            .pretty_print(f, state, context)?;
        Ok(())
    }
}

impl PrettyPrint for MemberList {
    type State = DesugarArena;
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        for (i, member) in self.members.iter().enumerate() {
            if context.multiline {
                self.indent(f, context)?;
            }
            state
                .members
                .get(*member)
                .val
                .pretty_print(f, state, context)?;
            if context.multiline {
                writeln!(f)?;
            } else if i != self.members.len() - 1 {
                write!(f, ", ")?;
            }
        }
        Ok(())
    }
}

impl PrettyPrint for Block {
    type State = DesugarArena;
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        {
            let context = &mut context.indented();
            if context.multiline {
                writeln!(f, "{{")?;
            } else {
                write!(f, "{{ ")?;
            }
            for (i, stmt) in self.stmts.iter().enumerate() {
                if context.multiline {
                    self.indent(f, context)?;
                }
                state.stmts.get(*stmt).val.pretty_print(f, state, context)?;
                if context.multiline {
                    writeln!(f)?;
                } else if i != self.stmts.len() - 1 {
                    write!(f, ", ")?;
                }
            }
        }
        if context.multiline {
            self.indent(f, context)?;
            write!(f, "}}")?;
        } else {
            write!(f, " }}")?;
        }
        Ok(())
    }
}
