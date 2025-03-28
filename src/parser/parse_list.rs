use std::fmt::Debug;

use crate::{ast::*, lexer::Token};

use super::Parse;

impl<T: Clone + Debug> Parse for Node<Vec<Node<T>>>
where
    Node<T>: Parse,
{
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        if matches!(parser.cur(), Some(Token::Separator(_))) {
            parser.eat();
        }

        let mut list = Vec::new();
        while !matches!(parser.cur(), Some(Token::Close(_)) | None) {
            list.push(Node::<T>::parse(parser)?);
            if matches!(parser.cur(), Some(Token::Separator(_))) {
                parser.eat();
            }
        }

        let loc = list
            .iter()
            .fold(None, |acc, element| Loc::combine(acc, element.loc));

        Ok(list.node(loc))
    }
}
