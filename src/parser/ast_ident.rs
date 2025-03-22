use crate::{
    ast::*,
    lexer::{Grouping, Token},
    parser::ParseError,
};

use super::Parse;

impl Parse for ASTIdent {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        let tok = parser.next()?;
        let ident = match &tok.map(|loc| &loc.token) {
            Some(Token::VIdent(ident)) => ASTIdent::VIdent(ident.as_str().into()),
            Some(Token::TIdent(ident)) => ASTIdent::TIdent(ident.as_str().into()),
            _ => {
                return Err(ParseError::ExpectedIdent(
                    "while parsing ident",
                    tok.cloned(),
                ))
            }
        };
        Ok(ident)
    }
}
