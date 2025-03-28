use crate::interpreter::*;

impl Interpret for Node<Stmt> {
    type Output = Node<Stmt>;

    fn interp(self, context: &mut Context) -> Result<Self::Output, InterpretError> {
        let loc = self.loc;
        match self.val {
            Stmt::Expr(expr) => Ok(Stmt::Expr(expr.interp(context)?).node(loc)),
            Stmt::Binding(ident, ty, value) => {
                let value = value.interp(context)?;
                let ty = match ty {
                    Some(ty) => Some(ty.interp(context)?),
                    None => None,
                };
                context.add_local(
                    ident.val.clone(),
                    ty.clone().unwrap_or_else(|| value.clone()),
                    value.clone(),
                )?;
                Ok(Stmt::Binding(ident, ty, value).node(loc))
            }
        }
    }
}
