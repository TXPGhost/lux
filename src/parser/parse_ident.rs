use crate::{
    ast::*,
    lexer::{Operator, Token},
    parser::ParseError,
};

use super::Parse;

impl Parse for Node<Ident> {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        let tok = parser.next()?;
        match &tok.map(|loc| &loc.token) {
            Some(Token::VIdent(ident)) => {
                Ok(Ident::VIdent(ident.as_str().into()).loc(Loc::from_token(tok.unwrap())))
            }
            Some(Token::TIdent(ident)) => {
                Ok(Ident::TIdent(ident.as_str().into()).loc(Loc::from_token(tok.unwrap())))
            }
            Some(Token::Operator(Operator::Caret)) => {
                let hoist_loc = Loc::from_token(tok.unwrap());
                let ident = Node::<Ident>::parse(parser)?;
                let loc = Loc::combine(ident.loc, Some(hoist_loc));
                Ok(Ident::Hoist(Box::new(ident.val)).node(loc))
            }
            _ => Err(ParseError::ExpectedIdent(
                "while parsing ident",
                tok.cloned(),
            )),
        }
    }
}
