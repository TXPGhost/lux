use std::num::ParseIntError;

use crate::{
    ast::*,
    lexer::{LocatedToken, Token},
};

pub mod parse_expr;
pub mod parse_field;
pub mod parse_ident;
pub mod parse_list;
pub mod parse_member;
pub mod parse_stmt;

pub struct Parser<'a> {
    tokens: &'a [LocatedToken],
    idx: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [LocatedToken]) -> Self {
        Self { tokens, idx: 0 }
    }

    pub fn parse(&mut self) -> Result<Node<List<Node<Member>>>, ParseError> {
        Node::<List<Node<Member>>>::parse(self)
    }

    fn eat(&mut self) {
        self.idx += 1;
    }

    fn cur(&self) -> Option<&'a Token> {
        self.tokens
            .get(self.idx)
            .map(|located_token| &located_token.token)
    }

    fn cur_loc(&self) -> Option<&'a LocatedToken> {
        self.tokens.get(self.idx)
    }

    fn peek(&self) -> Option<&'a Token> {
        self.tokens
            .get(self.idx + 1)
            .map(|located_token| &located_token.token)
    }

    fn next(&mut self) -> Result<Option<&'a LocatedToken>, ParseError> {
        let res = self.tokens.get(self.idx);
        self.eat();
        Ok(res)
    }
}

#[derive(Clone, Debug)]
pub enum ParseError {
    UnexpectedToken(&'static str, Option<LocatedToken>),
    ExpectedToken(&'static str, Token, Option<LocatedToken>),
    ExpectedTokens(&'static str, Vec<Token>, Option<LocatedToken>),
    ExpectedIdent(&'static str, Option<LocatedToken>),
    NumberParseError(ParseIntError),
    IllegalListType(&'static str, usize),
}

pub trait Parse: Sized {
    fn parse(parser: &mut Parser<'_>) -> Result<Self, ParseError>;
}
