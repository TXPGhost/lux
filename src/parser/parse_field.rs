use crate::{
    ast::{parse_tree::*, *},
    lexer::Token,
    parser::ParseError,
};

use super::Parse;

impl Parse for Node<Field> {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        let tok = parser.next()?;
        let ident = match &tok.map(|loc| &loc.token) {
            Some(Token::VIdent(ident)) => Field::Ident(Ident::VIdent(ident.as_str().into())),
            Some(Token::TIdent(ident)) => Field::Ident(Ident::TIdent(ident.as_str().into())),
            Some(Token::Number(number)) => {
                Field::Number(number.parse().map_err(ParseError::NumberParseError)?)
            }
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
