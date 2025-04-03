use std::usize;

use crate::{
    ast::{parse_tree::*, Node, Primitive},
    lexer::{Operator, OperatorChars},
};

use super::{PrettyPrint, PrettyPrintContext};

/// Context for pretty printing the parse tree
#[derive(Clone, Copy)]
pub struct Context {
    indent_level: usize,
    prec: usize,
    multiline: bool,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            indent_level: 0,
            prec: usize::MIN,
            multiline: true,
        }
    }
}

/// Indicates that a type has a precedence (w.r.t. pretty printing)
pub trait HasPrec {
    /// The precedence level of this type
    fn prec(&self) -> usize;
}

impl HasPrec for Binop {
    fn prec(&self) -> usize {
        match self.op.val {
            Operator::FatArrow => 0,
            Operator::DoubleEquals
            | Operator::NotEquals
            | Operator::Greater
            | Operator::Less
            | Operator::GreaterEquals
            | Operator::LessEquals => 1,
            Operator::Backslash => 2,
            Operator::Or => 3,
            Operator::And => 4,
            Operator::Plus | Operator::Minus | Operator::Concat => 5,
            Operator::Times | Operator::Divide | Operator::Modulo | Operator::Repeat => 6,
            Operator::ThinArrow => 7,
            _ => todo!("prec of binop {:?}", self.op.val),
        }
    }
}

impl HasPrec for Expr {
    fn prec(&self) -> usize {
        match self {
            Expr::Binop(binop) => binop.val.prec(),
            Expr::Unop(_) => 8,
            Expr::Index(_, _) | Expr::Field(_, _) | Expr::Call(_, _) => 9,
            Expr::Struct(_)
            | Expr::Enum(_)
            | Expr::Array(_)
            | Expr::Block(_)
            | Expr::ArrayType(_, _) => 10,
            Expr::Ident(_) | Expr::Primitive(_) => 11,
            _ => todo!("prec of expr {:?}", self),
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
    fn prec(self, prec: usize) -> Self {
        Self { prec, ..self }
    }

    fn multiline(self, multiline: bool) -> Self {
        Self { multiline, ..self }
    }
}

impl PrettyPrint for Member {
    type State = ();
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        let context = &mut context.prec(0);
        match self {
            Member::Expr(expr) => expr.val.pretty_print(f, state, context),
            Member::Named(ident, expr) => {
                ident.val.pretty_print(f, state, context)?;
                write!(f, ": ")?;
                expr.val.pretty_print(f, state, context)?;
                Ok(())
            }
            Member::NamedFunc(ident, args, expr) => {
                ident.val.pretty_print(f, state, context)?;
                write!(f, "(")?;
                for (i, arg) in args.val.iter().enumerate() {
                    arg.val.pretty_print(f, state, context)?;
                    if i != args.val.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "): ")?;
                expr.val.pretty_print(f, state, context)?;
                Ok(())
            }
        }
    }
}

impl PrettyPrint for Expr {
    type State = ();
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        let prec = self.prec();
        let wrap_parens = prec < context.prec;
        if wrap_parens {
            write!(f, "(")?;
        }
        let context = &mut context.prec(prec);
        match self {
            Expr::Ident(ident) => ident.val.pretty_print(f, state, context)?,
            Expr::Unop(unop) => unop.val.pretty_print(f, state, context)?,
            Expr::Binop(binop) => binop.val.pretty_print(f, state, context)?,
            Expr::Index(expr, index) => {
                expr.val.pretty_print(f, state, context)?;
                write!(f, "[")?;
                index.val.pretty_print(f, state, context)?;
                write!(f, "]")?;
            }
            Expr::Field(expr, field) => {
                expr.val.pretty_print(f, state, context)?;
                write!(f, ".")?;
                field.val.pretty_print(f, state, context)?;
            }
            Expr::Struct(fields) => {
                writeln!(f, "(")?;
                fields
                    .val
                    .pretty_print(f, state, &mut context.indented().multiline(true))?;
                self.indent(f, context)?;
                write!(f, ")")?;
            }
            Expr::Enum(variants) => {
                writeln!(f, "<")?;
                variants
                    .val
                    .pretty_print(f, state, &mut context.indented().multiline(true))?;
                self.indent(f, context)?;
                write!(f, ">")?;
            }
            Expr::Call(func, args) => {
                func.val.pretty_print(f, state, context)?;
                write!(f, "(")?;
                args.val
                    .pretty_print(f, state, &mut context.multiline(false))?;
                write!(f, ")")?;
            }
            Expr::Func(args, body) => todo!(),
            Expr::Block(stmts) => {
                writeln!(f, "{{")?;
                stmts
                    .val
                    .pretty_print(f, state, &mut context.indented().multiline(true))?;
                self.indent(f, context)?;
                write!(f, "}}")?;
            }
            Expr::Array(elements) => todo!(),
            Expr::ArrayType(len, ty) => {
                write!(f, "[")?;
                if let Some(len) = len {
                    len.val.pretty_print(f, state, context)?;
                }
                write!(f, "]")?;
                ty.val.pretty_print(f, state, context)?;
            }
            Expr::Primitive(primitive) => primitive.pretty_print(f, state, context)?,
        }
        if wrap_parens {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl PrettyPrint for Ident {
    type State = ();
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
        }
    }
}

impl PrettyPrint for Unop {
    type State = ();
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        todo!()
    }
}

impl PrettyPrint for Binop {
    type State = ();
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        if self.lhs.val.prec() < self.prec() {
            write!(f, "(")?;
        }
        self.lhs.val.pretty_print(f, state, context)?;
        if self.lhs.val.prec() < self.prec() {
            write!(f, ")")?;
        }
        match self.op.val.chars() {
            OperatorChars::Single(c1) => write!(f, " {} ", c1)?,
            OperatorChars::Double(c1, c2) => write!(f, " {}{} ", c1, c2)?,
        }
        if self.rhs.val.prec() <= self.prec() {
            write!(f, "(")?;
        }
        self.rhs.val.pretty_print(f, state, context)?;
        if self.rhs.val.prec() <= self.prec() {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl PrettyPrint for Field {
    type State = ();
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        match self {
            Field::Ident(ident) => ident.pretty_print(f, state, context),
            Field::Number(number) => write!(f, "{}", number),
        }
    }
}

impl PrettyPrint for Vec<Node<Member>> {
    type State = ();
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        let context = &mut context.prec(0);
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
    type State = ();
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        let context = &mut context.prec(0);
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
    type State = ();
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        let context = &mut context.prec(0);
        match self {
            Stmt::Expr(expr) => expr.val.pretty_print(f, state, context),
            Stmt::Binding(ident, None, expr) => {
                ident.val.pretty_print(f, state, context)?;
                write!(f, " = ")?;
                expr.val.pretty_print(f, state, &mut context.indented())?;
                Ok(())
            }
            Stmt::Binding(ident, Some(ty), expr) => {
                ident.val.pretty_print(f, state, context)?;
                write!(f, ": ")?;
                ty.val.pretty_print(f, state, &mut context.indented())?;
                write!(f, " = ")?;
                expr.val.pretty_print(f, state, &mut context.indented())?;
                Ok(())
            }
        }
    }
}

impl PrettyPrint for Primitive {
    type State = ();
    type Context = Context;

    fn pretty_print(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: &Self::State,
        context: &mut Self::Context,
    ) -> std::fmt::Result {
        match self {
            Primitive::U64Ty => write!(f, "%U64"),
            Primitive::U64Val(val) => write!(f, "{}", val),
            Primitive::CharTy => todo!(),
            Primitive::CharVal(_) => todo!(),
            Primitive::Bool => todo!(),
            Primitive::True => todo!(),
            Primitive::False => todo!(),
            Primitive::DebugPrint => write!(f, "debug_print"),
            Primitive::Assert(assertion) => todo!(),
            Primitive::Unop(operator) | Primitive::Binop(operator) => match operator {
                Operator::Dot => unreachable!(),
                Operator::Plus => write!(f, "%add"),
                Operator::Minus => write!(f, "%sub"),
                Operator::Times => write!(f, "%mul"),
                Operator::Divide => write!(f, "%div"),
                Operator::Modulo => write!(f, "%mod"),
                Operator::Concat => write!(f, "%concat"),
                Operator::Repeat => write!(f, "%repeat"),
                Operator::And => unreachable!(),
                Operator::Or => unreachable!(),
                Operator::Not => write!(f, "%not"),
                Operator::Backslash => unreachable!(),
                Operator::ThinArrow => unreachable!(),
                Operator::FatArrow => unreachable!(),
                Operator::Question => todo!("question mark operator"),
                Operator::Length => write!(f, "%len"),
                Operator::Range => todo!("range operator"),
                Operator::DoubleEquals => write!(f, "%eq"),
                Operator::NotEquals => write!(f, "%ne"),
                Operator::Greater => write!(f, "%gt"),
                Operator::Less => write!(f, "%lt"),
                Operator::GreaterEquals => write!(f, "%ge"),
                Operator::LessEquals => write!(f, "%le"),
                Operator::Caret => unreachable!(),
            },
        }
    }
}
