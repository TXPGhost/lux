use std::num::ParseIntError;

use crate::{
    ast::*,
    lexer::{LocatedToken, Token},
};

/// Parser implementation for expressions
pub mod parse_expr;

/// Parser implementation for fields
pub mod parse_field;

/// Parser implementation for identifiers
pub mod parse_ident;

/// Parser implementation for lists
pub mod parse_list;

/// Parser implementation for members
pub mod parse_member;

/// Parser implementation for statements
pub mod parse_stmt;

/// A parser, which operates over a slice of tokens with lifetime ['a]
pub struct Parser<'a> {
    tokens: &'a [LocatedToken],
    idx: usize,
}

impl<'a> Parser<'a> {
    /// Constructs a new parser from the given tokens
    pub fn new(tokens: &'a [LocatedToken]) -> Self {
        Self { tokens, idx: 0 }
    }

    /// Parses the tokens into a member list, used to define a file
    pub fn parse(&mut self) -> Result<Node<Vec<Node<Member>>>, ParseError> {
        Node::<Vec<Node<Member>>>::parse(self)
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

    fn next(&mut self) -> Result<Option<&'a LocatedToken>, ParseError> {
        let res = self.tokens.get(self.idx);
        self.eat();
        Ok(res)
    }
}

/// An error that is encountered while parsing
#[derive(Clone, Debug)]
pub enum ParseError {
    /// An unexpected token was encountered
    UnexpectedToken(&'static str, Option<LocatedToken>),

    /// A token was expected but a different one was encountered
    ExpectedToken(&'static str, Token, Option<LocatedToken>),

    /// One of several tokens was expected, but a different one was encountered
    ExpectedTokens(&'static str, Vec<Token>, Option<LocatedToken>),

    /// An identifier was expected, but something else was encountered
    ExpectedIdent(&'static str, Option<LocatedToken>),

    /// A number failed to parse
    NumberParseError(ParseIntError),

    /// A list type was defined with more than one size
    IllegalListType(&'static str, usize),
}

/// A trait for parsing
pub trait Parse: Sized {
    /// Parses into the [Self] type
    fn parse(parser: &mut Parser<'_>) -> Result<Self, ParseError>;
}
