use crate::{
    ast::*,
    lexer::{LocatedToken, Token},
};

pub mod ast_expr;
pub mod ast_ident;
pub mod ast_list;
pub mod ast_member;

pub struct Parser<'a> {
    tokens: &'a [LocatedToken],
    idx: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [LocatedToken]) -> Self {
        Self { tokens, idx: 0 }
    }

    pub fn parse(&mut self) -> Result<ASTList<ASTMember>, ParseError> {
        ASTList::parse(self)
    }

    fn eat(&mut self) -> Result<(), ParseError> {
        println!("eat {} -> {}", self.idx, self.idx + 1);
        if self.idx < self.tokens.len() {
            self.idx += 1;
            Ok(())
        } else {
            Err(ParseError::OutOfTokens)
        }
    }

    fn cur(&self) -> Option<&'a Token> {
        println!("get: {}", self.idx);
        self.tokens
            .get(self.idx)
            .map(|located_token| &located_token.token)
    }

    fn cur_loc(&self) -> Option<&'a LocatedToken> {
        println!("get: {}", self.idx);
        self.tokens.get(self.idx)
    }

    fn peek(&self) -> Option<&'a Token> {
        println!("peek: {}", self.idx);
        self.tokens
            .get(self.idx + 1)
            .map(|located_token| &located_token.token)
    }

    fn next(&mut self) -> Result<&'a LocatedToken, ParseError> {
        println!("next: {}", self.idx);
        let Some(res) = self.tokens.get(self.idx) else {
            return Err(ParseError::OutOfTokens);
        };
        self.eat()?;
        Ok(res)
    }
}

#[derive(Clone, Debug)]
pub enum ParseError {
    OutOfTokens,
    UnexpectedToken(&'static str, Option<LocatedToken>),
    ExpectedToken(&'static str, Token, Option<LocatedToken>),
    ExpectedTokens(&'static str, Vec<Token>, Option<LocatedToken>),
    ExpectedIdent(&'static str, Option<LocatedToken>),
    IllegalPrecedenceLevel(usize, Option<LocatedToken>),
}

pub trait Parse: Sized {
    fn parse(parser: &mut Parser<'_>) -> Result<Self, ParseError>;
}
