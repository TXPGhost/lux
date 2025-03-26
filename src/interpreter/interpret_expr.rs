use crate::{interpreter::*, lexer::Operator};

impl Interpret for Node<Expr> {
    type Output = Node<Expr>;
    fn eval(self, context: &mut Context) -> Result<Self::Output, InterpretError> {
        let loc = self.loc;
        match self.value {
            Expr::Ident(node) => match context.lookup(&node.value) {
                Some(expr) => Ok(expr.clone()),
                None => match context.strategy() {
                    InterpretStrategy::Eval => Err(InterpretError::UndefinedSymbol(node)),
                    InterpretStrategy::Simplify => Ok(Node {
                        value: Expr::Ident(node),
                        loc,
                    }),
                },
            },
            Expr::Number(_) => Ok(self),
            Expr::Binop(binop) => {
                let lhs = binop.value.lhs.eval(context)?;
                let rhs = binop.value.rhs.eval(context)?;

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
            Expr::Field(_, _) => todo!("field"),
            Expr::Struct(members) => {
                let loc = members.loc;
                Ok(Node {
                    value: Expr::Struct(members.eval(context)?),
                    loc,
                })
            }
            Expr::Enum(variants) => {
                let loc = variants.loc;
                Ok(Node {
                    value: Expr::Enum(variants.eval(context)?),
                    loc,
                })
            }
            Expr::Call(func, args) => {
                let func = func.eval(context)?;
                let args = args.eval(context)?;
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
            Expr::Block(_) => todo!("block"),
            Expr::List(_) => todo!("list"),
            Expr::ListType(_, _) => todo!("list type"),
            Expr::Primitive(primitive) => Ok(Node {
                value: Expr::Primitive(primitive),
                loc,
            }),
        }
    }
}
