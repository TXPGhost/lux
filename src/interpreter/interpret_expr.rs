use crate::{interpreter::*, lexer::Operator};

impl Interpret for Node<Expr> {
    type Output = Node<Expr>;
    fn interp(self, context: &mut Context) -> Result<Self::Output, InterpretError> {
        let loc = self.loc;
        match self.val {
            Expr::Ident(ident) => match context.lookup(&ident) {
                Ok(ContextDefinition::Static(expr)) => Ok(expr.clone()),
                Ok(ContextDefinition::Local(ty, value)) => match context.strategy() {
                    InterpretStrategy::Eval => Ok(value.clone()),
                    InterpretStrategy::Simplify => Ok(ty.clone()),
                },
                Ok(ContextDefinition::Argument(ty)) => Ok(ty.clone()),
                // allow recursive definitions in some cases
                Err(InterpretError::UndefinedSymbol(_)) => match context.strategy() {
                    InterpretStrategy::Eval => Err(InterpretError::UndefinedSymbol(ident)),
                    InterpretStrategy::Simplify => Ok(Expr::Ident(ident).node(loc)),
                },
                Err(e) => Err(e),
            },
            Expr::Number(_) => Ok(self),
            Expr::Binop(binop) => {
                let lhs = binop.val.lhs.interp(context)?;
                let rhs = binop.val.rhs.interp(context)?;

                if let (Expr::Number(x), Expr::Number(y)) = (&lhs.val, &rhs.val) {
                    return match binop.val.op {
                        Operator::Plus => Ok(Expr::Number(x + y).node(loc)),
                        Operator::Minus => Ok(Expr::Number(x - y).node(loc)),
                        Operator::Times => Ok(Expr::Number(x * y).node(loc)),
                        Operator::Divide => Ok(Expr::Number(x / y).node(loc)),
                        Operator::Modulo => Ok(Expr::Number(x % y).node(loc)),
                        Operator::DoubleEquals => todo!("comparison"),
                        Operator::NotEquals => todo!("comparison"),
                        Operator::Greater => todo!("comparison"),
                        Operator::Less => todo!("comparison"),
                        Operator::GreaterEquals => todo!("comparison"),
                        Operator::LessEquals => todo!("comparison"),
                        _ => Err(InterpretError::IllegalBinop(
                            "illegal binary operation",
                            Binop::new(lhs, binop.val.op, rhs).node(loc),
                        )),
                    };
                }

                match binop.val.op {
                    Operator::ThinArrow => todo!("pipe"),
                    Operator::FatArrow => todo!("lambda"),
                    _ => todo!(),
                }
            }
            Expr::Func(_, _) => todo!("func"),
            Expr::Index(_, _) => todo!("index"),
            Expr::Field(expr, field) => {
                let expr = expr.interp(context)?;
                let Expr::Struct(members) = expr.val else {
                    return Err(InterpretError::IllegalFieldOperation(
                        "cannot access field of a non-struct value",
                        expr,
                    ));
                };
                match &field.val {
                    Field::Ident(ident) => {
                        for member in members.val.elements {
                            match member.val {
                                Member::Expr(_) => (),
                                Member::Named(member_ident, expr) => {
                                    if *ident == member_ident.val {
                                        return Ok(expr);
                                    }
                                }
                                Member::NamedFunc(_, _, _) => unreachable!(),
                            }
                        }

                        Err(InterpretError::UndefinedField(field))
                    }
                    Field::Number(index) => {
                        let index = *index as usize;
                        if members.val.elements.len() >= index {
                            Err(InterpretError::UndefinedField(field))
                        } else {
                            match &members.val.elements[index].val {
                                Member::Expr(expr) => Ok(expr.clone()),
                                Member::Named(_, expr) => Ok(expr.clone()),
                                Member::NamedFunc(_, _, _) => unreachable!(),
                            }
                        }
                    }
                }
            }
            Expr::Struct(members) => {
                let loc = members.loc;
                Ok(Expr::Struct(members.interp(context)?).node(loc))
            }
            Expr::Enum(variants) => {
                let loc = variants.loc;
                Ok(Expr::Enum(variants.interp(context)?).node(loc))
            }
            Expr::Call(func, args) => {
                let func = func.interp(context)?;
                let args = args.interp(context)?;
                match &func.val {
                    Expr::Func(fargs, _) => {
                        if fargs.val.elements.len() != args.val.elements.len() {
                            return Err(InterpretError::ArgumentMismatch(
                                "wrong number of arguments passed to function (expected, actual)",
                                fargs.val.elements.len(),
                                args.val.elements.len(),
                            ));
                        }
                        Err(InterpretError::Unimplemented("todo call"))
                    }
                    Expr::Struct(_) => todo!("constructor"),
                    Expr::Ident(_) if context.strategy() == InterpretStrategy::Simplify => {
                        Ok(Expr::Call(Box::new(func), args).node(loc))
                    }
                    _ => Err(InterpretError::IllegalCallOperation(
                        "call must be a function or constructor",
                        func,
                    )),
                }
            }
            Expr::Block(stmts) => {
                let mut context = context.frame(context.strategy());
                let mut value = Expr::unit().node(loc);
                let len = stmts.val.elements.len();
                let new_stmts = Vec::with_capacity(len);
                for (i, stmt) in stmts.val.elements.into_iter().enumerate() {
                    let stmt = stmt.interp(&mut context)?;
                    match stmt.val {
                        Stmt::Expr(expr) if i == len - 1 => value = expr,
                        Stmt::Expr(_) => (),
                        Stmt::Binding(_, _, _) => (),
                    }
                }

                match context.strategy() {
                    InterpretStrategy::Eval => Ok(value),
                    InterpretStrategy::Simplify => {
                        Ok(Expr::Block(List::new(new_stmts).node(loc)).node(loc))
                    }
                }
            }
            Expr::Array(_) => todo!("list"),
            Expr::ArrayType(_, _) => todo!("list type"),
            Expr::Primitive(primitive) => Ok(Expr::Primitive(primitive).node(loc)),
        }
    }
}
