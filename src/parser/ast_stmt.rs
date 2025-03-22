use crate::{ast::*, lexer::Token, parser::ParseError};

use super::Parse;

impl Parse for ASTStmt {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        let expr = ASTExpr::parse(parser)?;
        let mut ty = None;
        if matches!(parser.cur(), Some(Token::Colon)) {
            parser.eat();
            ty = Some(ASTExpr::parse(parser)?);
        }
        if matches!(parser.cur(), Some(Token::Equals)) {
            parser.eat();
            let val = ASTExpr::parse(parser)?;
            let ASTExpr::Ident(ident) = expr else {
                return Err(ParseError::ExpectedIdent(
                    "while parsing binding",
                    parser.cur_loc().cloned(),
                ));
            };
            return Ok(ASTStmt::Binding(ident, ty, val));
        }
        let None = ty else {
            return Err(ParseError::ExpectedToken(
                "while parsing binding",
                Token::Equals,
                parser.cur_loc().cloned(),
            ));
        };

        Ok(ASTStmt::Expr(expr))
    }
}
