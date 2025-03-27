use crate::interpreter::*;

impl Interpret for Node<Stmt> {
    type Output = Node<Stmt>;

    fn interp(self, context: &mut Context) -> Result<Self::Output, InterpretError> {
        let loc = self.loc;
        match self.value {
            Stmt::Expr(expr) => Ok(Node {
                value: Stmt::Expr(expr.interp(context)?),
                loc,
            }),
            Stmt::Binding(ident, ty, value) => {
                let ty = match ty {
                    Some(ty) => Some(ty.interp(context)?),
                    None => None,
                };
                let value = value.interp(context)?;
                match context.strategy() {
                    InterpretStrategy::Eval => {
                        context.shadow_add(ident.value.clone(), value.clone())?
                    }
                    InterpretStrategy::Simplify => {
                        if let Some(ty) = &ty {
                            context.shadow_add(ident.value.clone(), ty.clone())?;
                        }
                    }
                }
                Ok(Node {
                    value: Stmt::Binding(ident, ty, value),
                    loc: None,
                })
            }
        }
    }
}
