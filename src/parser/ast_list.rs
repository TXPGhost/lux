use crate::{ast::*, lexer::Token};

use super::Parse;

impl<T: Parse + std::fmt::Debug> Parse for ASTList<T> {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        if matches!(parser.cur(), Some(Token::Separator(_))) {
            parser.eat();
        }

        let mut list = Vec::new();
        while !matches!(parser.cur(), Some(Token::Close(_)) | None) {
            list.push(T::parse(parser)?);
            if matches!(parser.cur(), Some(Token::Separator(_))) {
                parser.eat();
            }
        }

        Ok(ASTList(list))
    }
}
