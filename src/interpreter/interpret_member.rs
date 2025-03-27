use crate::interpreter::*;

impl Interpret for Node<Member> {
    type Output = Node<Member>;

    fn interp(self, context: &mut Context) -> Result<Self::Output, InterpretError> {
        let loc = self.loc;
        match self.val {
            Member::Expr(expr) => Ok(Member::Expr(expr.interp(context)?).node(loc)),
            Member::Named(ident, expr) => {
                let expr = expr.interp(&mut context.frame(InterpretStrategy::Eval))?;
                context.add_static(ident.val.clone(), expr.clone())?;
                Ok(Member::Named(ident, expr).node(loc))
            }
            Member::NamedFunc(ident, args, expr) => {
                let expr = Expr::Func(
                    args,
                    Box::new(expr.interp(&mut context.frame(InterpretStrategy::Simplify))?),
                )
                .node(loc);
                context.add_static(ident.val.clone(), expr.clone())?;
                Ok(Member::Named(ident, expr).node(loc))
            }
        }
    }
}
