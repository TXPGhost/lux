use crate::{interpreter::*, lexer::Operator, type_checker::TypeCompare};

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
                Ok(ContextDefinition::Argument(_)) => Ok(Expr::Ident(ident).node(loc)),
                // allow recursive definitions in some cases
                Err(InterpretError::UndefinedSymbol(_)) => match context.strategy() {
                    InterpretStrategy::Eval => Err(InterpretError::UndefinedSymbol(ident)),
                    InterpretStrategy::Simplify => Ok(Expr::Ident(ident).node(loc)),
                },
                Err(e) => Err(e),
            },
            Expr::Primitive(_) => Ok(self),
            Expr::Binop(binop) => {
                let lhs = binop.val.lhs.interp(context)?;
                let rhs = binop.val.rhs.interp(context)?;

                if let (
                    Expr::Primitive(Primitive::U64Val(x)),
                    Expr::Primitive(Primitive::U64Val(y)),
                ) = (&lhs.val, &rhs.val)
                {
                    return match binop.val.op {
                        Operator::Plus => Ok(Expr::Primitive(Primitive::U64Val(x + y)).node(loc)),
                        Operator::Minus => Ok(Expr::Primitive(Primitive::U64Val(x - y)).node(loc)),
                        Operator::Times => Ok(Expr::Primitive(Primitive::U64Val(x * y)).node(loc)),
                        Operator::Divide => Ok(Expr::Primitive(Primitive::U64Val(x / y)).node(loc)),
                        Operator::Modulo => Ok(Expr::Primitive(Primitive::U64Val(x % y)).node(loc)),
                        Operator::DoubleEquals => todo!("comparison"),
                        Operator::NotEquals => todo!("comparison"),
                        Operator::Greater => todo!("comparison"),
                        Operator::Less => todo!("comparison"),
                        Operator::GreaterEquals => todo!("comparison"),
                        Operator::LessEquals => todo!("comparison"),
                        _ => Err(InterpretError::IllegalBinop(
                            "illegal binary operation between numbers",
                            Binop::new(lhs, binop.val.op, rhs).node(loc),
                        )),
                    };
                }

                match binop.val.op {
                    Operator::ThinArrow => todo!("pipe"),
                    Operator::FatArrow => todo!("lambda"),
                    _ => {
                        let binop = Binop::new(lhs, binop.val.op, rhs).node(loc);
                        match context.strategy() {
                            InterpretStrategy::Eval => Err(InterpretError::IllegalBinop(
                                "illegal binary operation",
                                binop,
                            )),
                            InterpretStrategy::Simplify => Ok(Expr::Binop(binop).node(loc)),
                        }
                    }
                }
            }
            Expr::Func(args, body) => {
                let mut context = context.frame(InterpretStrategy::Simplify);

                let args_loc = args.loc;
                let mut new_args = Vec::new();
                for arg in args.val.elements {
                    let arg = arg.interp(&mut context)?;
                    match &arg.val {
                        Member::Expr(_) => (),
                        Member::Named(ident, expr) => {
                            context.add_argument(ident.val.clone(), expr.clone())?;
                        }
                        Member::NamedFunc(_, _, _) => unreachable!(),
                    }
                    new_args.push(arg);
                }
                let new_args = List::new(new_args).node(args_loc);

                Ok(Expr::Func(new_args, Box::new(body.interp(&mut context)?)).node(loc))
            }
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
                    Expr::Primitive(Primitive::DebugPrint) => {
                        for arg in args.val.elements {
                            match arg.loc {
                                Some(loc) => println!("\t[{}] {:?}", loc.line_min, arg.val),
                                None => println!("\t[?] {:?}", arg.val),
                            }
                        }
                        Ok(Expr::unit().unloc())
                    }
                    Expr::Func(fargs, body) => {
                        // make sure the number of arguments matches
                        if fargs.val.elements.len() != args.val.elements.len() {
                            return Err(InterpretError::IncorrectNumberOfArguments(
                                "wrong number of arguments passed to function (expected, actual)",
                                fargs.val.elements.len(),
                                args.val.elements.len(),
                            ));
                        }

                        // check that each argument name and type matches
                        // TODO: this should be done by the `is_subtype_of` function in the future
                        for i in 0..args.val.elements.len() {
                            let arg = &args.val.elements[i];
                            let farg = &fargs.val.elements[i];
                            match (&arg.val, &farg.val) {
                                (_, Member::NamedFunc(_, _, _)) => unreachable!(),
                                (Member::NamedFunc(_, _, _), _) => unreachable!(),
                                (Member::Named(ident, expr), Member::Named(fident, fexpr)) => {
                                    if ident.val == fident.val {
                                        if !expr.val.is_subtype_of(&fexpr.val) {
                                            return Err(InterpretError::NotASubtype(
                                                "function argument must be a subtype",
                                                expr.clone(),
                                                fexpr.clone(),
                                            ));
                                        }
                                    } else {
                                        return Err(InterpretError::ArgumentNameMismatch(
                                            "argument name does not match",
                                            fident.val.clone(),
                                            ident.clone(),
                                        ));
                                    }
                                }
                                (
                                    Member::Expr(expr),
                                    Member::Expr(fexpr) | Member::Named(_, fexpr),
                                ) => {
                                    if !expr.val.is_subtype_of(&fexpr.val) {
                                        return Err(InterpretError::NotASubtype(
                                            "function argument must be a subtype",
                                            expr.clone(),
                                            fexpr.clone(),
                                        ));
                                    }
                                }
                                (Member::Named(ident, _), Member::Expr(_)) => {
                                    return Err(InterpretError::UnexpectedMemberName(
                                        "an unnamed member was expected, but a name was provided",
                                        ident.clone(),
                                    ))
                                }
                            }
                        }

                        // assign each argument a value
                        let mut context = context.frame(InterpretStrategy::Eval);
                        for i in 0..args.val.elements.len() {
                            let arg = &args.val.elements[i];
                            let farg = &fargs.val.elements[i];

                            if let Member::Named(ident, ty) = &farg.val {
                                let value = match &arg.val {
                                    Member::Expr(expr) => expr,
                                    Member::Named(_, expr) => expr,
                                    Member::NamedFunc(_, _, _) => unreachable!(),
                                };
                                context.add_local(ident.val.clone(), ty.clone(), value.clone())?;
                            }
                        }

                        let result = body.clone().interp(&mut context)?;
                        Ok(result)
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
        }
    }
}
