use crate::{ast::*, lexer::Token, parser::ParseError};

use super::Parse;

impl Parse for Node<Ident> {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        let tok = parser.next()?;
        let ident = match &tok.map(|loc| &loc.token) {
            Some(Token::VIdent(ident)) => Ident::VIdent(ident.as_str().into()),
            Some(Token::TIdent(ident)) => Ident::TIdent(ident.as_str().into()),
            _ => {
                return Err(ParseError::ExpectedIdent(
                    "while parsing ident",
                    tok.cloned(),
                ))
            }
        };
        Ok(ident.loc(Loc::from_token(tok.unwrap())))
    }
}
