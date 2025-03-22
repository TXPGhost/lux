use crate::{ast::*, lexer::Token};

use super::Parse;

impl<T: Parse + std::fmt::Debug> Parse for ASTList<T> {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        if matches!(parser.cur(), Some(Token::Separator(_))) {
            parser.eat();
        }

        let mut list = Vec::new();
        println!("PARSING LIST...");
        while !matches!(parser.cur(), Some(Token::Close(_)) | None) {
            println!("PARSING LIST ITEM ({})...", list.len());
            list.push(T::parse(parser)?);
            if matches!(parser.cur(), Some(Token::Separator(_))) {
                println!("\t\tconsumming newline!");
                parser.eat();
            }
        }

        println!("DONE PARSING LIST");
        dbg!(&list);
        Ok(ASTList(list))
    }
}
