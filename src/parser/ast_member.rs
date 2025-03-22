use crate::{ast::*, lexer::Token, parser::ParseError};

use super::Parse;

impl Parse for ASTMember {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        println!("parsing ASTMember");
        if matches!(parser.cur(), Some(Token::TIdent(_) | Token::VIdent(_))) {
            let ident = ASTIdent::parse(parser)?;
            if matches!(parser.cur(), Some(Token::Colon)) {
                println!("parsing identified member");
                parser.eat()?;
                println!("...begin expr (cur = {:?})", parser.cur());
                let expr = ASTExpr::parse(parser)?;
                println!("...end expr");
                Ok(ASTMember::Named(ident, expr))
            } else if matches!(parser.cur(), Some(Token::Separator(_) | Token::Close(_))) {
                Ok(ASTMember::Expr(ASTExpr::Ident(ident)))
            } else {
                Err(ParseError::UnexpectedToken(
                    "while parsing member",
                    parser.cur_loc().cloned(),
                ))
            }
        } else {
            Ok(ASTMember::Expr(ASTExpr::parse(parser)?))
        }
    }
}
