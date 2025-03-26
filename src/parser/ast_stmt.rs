use crate::{ast::*, lexer::Token, parser::ParseError};

use super::Parse;

impl Parse for Node<Stmt> {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        let expr = Node::<Expr>::parse(parser)?;
        let mut ty = None;
        if matches!(parser.cur(), Some(Token::Colon)) {
            parser.eat();
            ty = Some(Node::<Expr>::parse(parser)?)
        }
        if matches!(parser.cur(), Some(Token::Equals)) {
            parser.eat();
            let val = Node::<Expr>::parse(parser)?;
            let Expr::Ident(ident) = expr.value else {
                return Err(ParseError::ExpectedIdent(
                    "while parsing binding",
                    parser.cur_loc().cloned(),
                ));
            };
            let meta = val.loc;
            return Ok(Node {
                value: Stmt::Binding(ident, ty, val),
                loc: Loc::combine(expr.loc, meta),
            });
        }
        let None = ty else {
            return Err(ParseError::ExpectedToken(
                "while parsing binding",
                Token::Equals,
                parser.cur_loc().cloned(),
            ));
        };

        let meta = expr.loc;
        Ok(Node {
            value: Stmt::Expr(expr),
            loc: meta,
        })
    }
}
