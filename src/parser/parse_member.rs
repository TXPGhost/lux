use crate::{
    ast::{parse_tree::*, *},
    lexer::Token,
    parser::ParseError,
};

use super::Parse;

impl Parse for Node<Member> {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        let token_loc = parser.cur_loc().cloned();
        let expr = Node::<Expr>::parse(parser)?;
        let loc = expr.loc;
        match (expr.val, parser.cur()) {
            (Expr::Ident(ident), Some(Token::Colon)) => {
                let loc = Loc::combine(loc, Some(Loc::from_token(parser.cur_loc().unwrap())));
                parser.eat();
                let expr = Node::<Expr>::parse(parser)?;
                let loc = Loc::combine(loc, expr.loc);
                Ok(Member::Named(ident, expr).node(loc))
            }
            (Expr::Call(func, args), Some(Token::Colon)) => match func.val {
                Expr::Ident(ident) => {
                    let loc = Loc::combine(loc, Some(Loc::from_token(parser.cur_loc().unwrap())));
                    parser.eat();
                    let expr = Node::<Expr>::parse(parser)?;
                    let loc = Loc::combine(loc, expr.loc);
                    Ok(Member::NamedFunc(ident, args, expr).node(loc))
                }
                _ => Err(ParseError::ExpectedIdent(
                    "expected identifier or function identifier",
                    token_loc,
                )),
            },
            (expr, _) => Ok(Member::Expr(expr.node(loc)).node(loc)),
        }
    }
}
