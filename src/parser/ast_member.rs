use crate::{
    ast::*,
    lexer::{Grouping, Operator, Token},
    parser::ParseError,
};

use super::Parse;

impl Parse for ASTMember {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        if matches!(parser.cur(), Some(Token::TIdent(_) | Token::VIdent(_)))
            && !matches!(parser.peek(), Some(Token::Operator(Operator::Dot)))
        {
            let ident = ASTIdent::parse(parser)?;
            let mut args = None;
            if matches!(parser.cur(), Some(Token::Open(Grouping::Paren))) {
                parser.eat();
                args = Some(ASTList::parse(parser)?);
                let Some(Token::Close(Grouping::Paren)) = parser.cur() else {
                    return Err(ParseError::ExpectedToken(
                        "while parsing function member",
                        Token::Close(Grouping::Paren),
                        parser.cur_loc().cloned(),
                    ));
                };
                parser.eat();
            }
            if matches!(parser.cur(), Some(Token::Colon)) {
                parser.eat();
                let expr = ASTExpr::parse(parser)?;
                match args {
                    Some(args) => Ok(ASTMember::NamedFunc(ident, args, expr)),
                    None => Ok(ASTMember::Named(ident, expr)),
                }
            } else if matches!(parser.cur(), Some(Token::Separator(_) | Token::Close(_))) {
                Ok(ASTMember::Expr(ASTExpr::Ident(ident)))
            } else {
                Err(ParseError::UnexpectedToken(
                    "while parsing member",
                    parser.cur_loc().cloned(),
                ))
            }
        } else {
            Ok(ASTMember::Expr(ASTExpr::parse(parser)?))
        }
    }
}
