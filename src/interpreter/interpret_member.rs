use crate::interpreter::*;

impl Interpret for Node<Member> {
    type Output = Node<Member>;

    fn eval(self, context: &mut Context) -> Result<Self::Output, InterpretError> {
        let loc = self.loc;
        match self.value {
            Member::Expr(expr) => Ok(Node {
                value: Member::Expr(expr.eval(context)?),
                loc,
            }),
            Member::Named(ident, expr) => {
                let expr = expr.eval(&mut context.frame(InterpretStrategy::Eval))?;
                context.add(ident.value.clone(), expr.clone());
                Ok(Node {
                    value: Member::Named(ident, expr),
                    loc,
                })
            }
            Member::NamedFunc(ident, args, expr) => {
                let expr = Node {
                    value: Expr::Func(
                        args,
                        Box::new(expr.eval(&mut context.frame(InterpretStrategy::Simplify))?),
                    ),
                    loc,
                };
                context.add(ident.value.clone(), expr.clone());
                Ok(Node {
                    value: Member::Named(ident, expr),
                    loc,
                })
            }
        }
    }
}
