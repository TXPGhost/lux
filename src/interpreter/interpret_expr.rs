use crate::{interpreter::*, lexer::Operator};

impl Interpret for Node<Expr> {
    type Output = Node<Expr>;
    fn interp(self, context: &mut Context) -> Result<Self::Output, InterpretError> {
        let loc = self.loc;
        match self.value {
            Expr::Ident(ident) => match context.lookup(&ident) {
                Ok(ContextDefinition::Static(expr)) => Ok(expr.clone()),
                Ok(ContextDefinition::Local(ty, value)) => match context.strategy() {
                    InterpretStrategy::Eval => Ok(value.clone()),
                    InterpretStrategy::Simplify => Ok(ty.clone()),
                },
                Err(InterpretError::UndefinedSymbol(_)) => match context.strategy() {
                    InterpretStrategy::Eval => Err(InterpretError::UndefinedSymbol(ident)),
                    InterpretStrategy::Simplify => Ok(Node {
                        value: Expr::Ident(ident),
                        loc,
                    }),
                },
                Err(e) => Err(e),
            },
            Expr::Number(_) => Ok(self),
            Expr::Binop(binop) => {
                let lhs = binop.value.lhs.interp(context)?;
                let rhs = binop.value.rhs.interp(context)?;

                if let (Expr::Number(x), Expr::Number(y)) = (&lhs.value, &rhs.value) {
                    return match binop.value.op {
                        Operator::Plus => Ok(Node {
                            value: Expr::Number(x + y),
                            loc,
                        }),
                        Operator::Minus => Ok(Node {
                            value: Expr::Number(x - y),
                            loc,
                        }),
                        Operator::Times => Ok(Node {
                            value: Expr::Number(x * y),
                            loc,
                        }),
                        Operator::Divide => Ok(Node {
                            value: Expr::Number(x / y),
                            loc,
                        }),
                        Operator::Modulo => Ok(Node {
                            value: Expr::Number(x % y),
                            loc,
                        }),
                        Operator::DoubleEquals => todo!("comparison"),
                        Operator::NotEquals => todo!("comparison"),
                        Operator::Greater => todo!("comparison"),
                        Operator::Less => todo!("comparison"),
                        Operator::GreaterEquals => todo!("comparison"),
                        Operator::LessEquals => todo!("comparison"),
                        _ => Err(InterpretError::IllegalBinop(
                            "illegal binary operation",
                            Node {
                                value: Binop {
                                    lhs: Box::new(lhs),
                                    op: binop.value.op,
                                    rhs: Box::new(rhs),
                                },
                                loc,
                            },
                        )),
                    };
                }

                match binop.value.op {
                    Operator::ThinArrow => todo!("pipe"),
                    Operator::FatArrow => todo!("lambda"),
                    _ => todo!(),
                }
            }
            Expr::Func(_, _) => todo!("func"),
            Expr::Index(_, _) => todo!("index"),
            Expr::Field(expr, field) => {
                let expr = expr.interp(context)?;
                let Expr::Struct(members) = expr.value else {
                    return Err(InterpretError::IllegalFieldOperation(
                        "cannot access field of a non-struct value",
                        expr,
                    ));
                };
                match &field.value {
                    Field::Ident(ident) => {
                        for member in members.value.elements {
                            match member.value {
                                Member::Expr(_) => (),
                                Member::Named(member_ident, expr) => {
                                    if *ident == member_ident.value {
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
                        if members.value.elements.len() >= index {
                            Err(InterpretError::UndefinedField(field))
                        } else {
                            match &members.value.elements[index].value {
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
                Ok(Node {
                    value: Expr::Struct(members.interp(context)?),
                    loc,
                })
            }
            Expr::Enum(variants) => {
                let loc = variants.loc;
                Ok(Node {
                    value: Expr::Enum(variants.interp(context)?),
                    loc,
                })
            }
            Expr::Call(func, args) => {
                let func = func.interp(context)?;
                let args = args.interp(context)?;
                match &func.value {
                    Expr::Func(fargs, _) => {
                        if fargs.value.elements.len() != args.value.elements.len() {
                            return Err(InterpretError::ArgumentMismatch(
                                "wrong number of arguments passed to function (expected, actual)",
                                fargs.value.elements.len(),
                                args.value.elements.len(),
                            ));
                        }
                        todo!("call")
                    }
                    Expr::Struct(_) => todo!("constructor"),
                    Expr::Ident(_) if context.strategy() == InterpretStrategy::Simplify => {
                        Ok(Node {
                            value: Expr::Call(Box::new(func), args),
                            loc,
                        })
                    }
                    _ => Err(InterpretError::IllegalCallOperation(
                        "call must be a function or constructor",
                        func,
                    )),
                }
            }
            Expr::Block(stmts) => {
                let mut context = context.frame(context.strategy());
                let mut value = Node {
                    value: Expr::unit(),
                    loc,
                };
                let len = stmts.value.elements.len();
                let new_stmts = Vec::with_capacity(len);
                for (i, stmt) in stmts.value.elements.into_iter().enumerate() {
                    let stmt = stmt.interp(&mut context)?;
                    match stmt.value {
                        Stmt::Expr(expr) if i == len - 1 => value = expr,
                        Stmt::Expr(_) => (),
                        Stmt::Binding(_, _, _) => (),
                    }
                }

                match context.strategy() {
                    InterpretStrategy::Eval => Ok(value),
                    InterpretStrategy::Simplify => Ok(Node {
                        value: Expr::Block(Node {
                            value: List {
                                elements: new_stmts,
                            },
                            loc,
                        }),
                        loc,
                    }),
                }
            }
            Expr::Array(_) => todo!("list"),
            Expr::ArrayType(_, _) => todo!("list type"),
            Expr::Primitive(primitive) => Ok(Node {
                value: Expr::Primitive(primitive),
                loc,
            }),
        }
    }
}
