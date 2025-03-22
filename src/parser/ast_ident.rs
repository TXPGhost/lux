use crate::{
    ast::*,
    lexer::{Grouping, Token},
    parser::ParseError,
};

use super::Parse;

impl Parse for ASTIdent {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        let tok = parser.next()?;
        let ident = match &tok.token {
            Token::VIdent(ident) => ASTIdent::VIdent(ident.as_str().into()),
            Token::TIdent(ident) => ASTIdent::TIdent(ident.as_str().into()),
            _ => {
                return Err(ParseError::ExpectedIdent(
                    "while parsing ident",
                    Some(tok.clone()),
                ))
            }
        };
        if let Some(Token::Open(Grouping::Paren)) = parser.cur() {
            parser.next()?;
            let args = ASTList::parse(parser)?;
            let Some(Token::Close(Grouping::Paren)) = parser.cur() else {
                return Err(ParseError::ExpectedToken(
                    "while parsing function ident",
                    Token::Close(Grouping::Paren),
                    parser.cur_loc().cloned(),
                ));
            };
            parser.next()?;
            return Ok(ASTIdent::Func(Box::new(ident), args));
        }
        Ok(ident)
    }
}
