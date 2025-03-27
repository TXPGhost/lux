use crate::{
    ast::*,
    lexer::{Grouping, Operator, Token},
    parser::ParseError,
};

use super::Parse;

impl Parse for Node<Member> {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        if matches!(parser.cur(), Some(Token::TIdent(_) | Token::VIdent(_)))
            && !matches!(parser.peek(), Some(Token::Operator(Operator::Dot)))
        {
            let ident = Node::<Ident>::parse(parser)?;
            let mut args = None;
            let mut opening_paren = None;
            let mut closing_paren = None;
            if matches!(parser.cur(), Some(Token::Open(Grouping::Paren))) {
                opening_paren = Some(Loc::from_token(parser.cur_loc().unwrap()));
                parser.eat();
                args = Some(Node::<List<Node<Member>>>::parse(parser)?);
                let Some(Token::Close(Grouping::Paren)) = parser.cur() else {
                    return Err(ParseError::ExpectedToken(
                        "while parsing function member",
                        Token::Close(Grouping::Paren),
                        parser.cur_loc().cloned(),
                    ));
                };
                closing_paren = Some(Loc::from_token(parser.cur_loc().unwrap()));
                parser.eat();
            }
            if matches!(parser.cur(), Some(Token::Colon)) {
                parser.eat();
                let expr = Node::<Expr>::parse(parser)?;
                let loc = Loc::combine(ident.loc, expr.loc);
                match args {
                    Some(args) => Ok(Member::NamedFunc(ident, args, expr).node(loc)),
                    None => Ok(Member::Named(ident, expr).node(loc)),
                }
            } else if matches!(parser.cur(), Some(Token::Separator(_) | Token::Close(_))) {
                let loc = ident.loc;
                let expr = Expr::Ident(ident).node(loc);
                match args {
                    Some(args) => {
                        let loc = Loc::combine(
                            Loc::combine(loc, args.loc),
                            Loc::combine(opening_paren, closing_paren),
                        );
                        Ok(Member::Expr(Expr::Call(Box::new(expr), args).node(loc)).node(loc))
                    }
                    None => Ok(Member::Expr(expr).node(loc)),
                }
            } else {
                Err(ParseError::UnexpectedToken(
                    "while parsing member",
                    parser.cur_loc().cloned(),
                ))
            }
        } else {
            let expr = Node::<Expr>::parse(parser)?;
            let loc = expr.loc;
            Ok(Member::Expr(expr).node(loc))
        }
    }
}
