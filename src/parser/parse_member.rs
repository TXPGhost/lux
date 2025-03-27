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
            if matches!(parser.peek(), Some(Token::Open(Grouping::Paren))) {
                // function members (e.g. `func(x): x + 2`) _or_ function call expressions (e.g. `func(x)`)
                let ident = Node::<Ident>::parse(parser)?;
                let opening_paren = Some(Loc::from_token(parser.cur_loc().unwrap()));
                parser.eat();
                let args = Node::<List<Node<Member>>>::parse(parser)?;
                let Some(Token::Close(Grouping::Paren)) = parser.cur() else {
                    return Err(ParseError::ExpectedToken(
                        "while parsing function member",
                        Token::Close(Grouping::Paren),
                        parser.cur_loc().cloned(),
                    ));
                };
                let closing_paren = Some(Loc::from_token(parser.cur_loc().unwrap()));
                parser.eat();
                match parser.cur() {
                    Some(Token::Colon) => {
                        let colon = Some(Loc::from_token(parser.cur_loc().unwrap()));
                        parser.eat();
                        let expr = Node::<Expr>::parse(parser)?;
                        let loc = Loc::combine(
                            Loc::combine(ident.loc, args.loc),
                            Loc::combine(opening_paren, Loc::combine(closing_paren, colon)),
                        );
                        Ok(Member::NamedFunc(ident, args, expr).node(loc))
                    }
                    _ => {
                        let loc = Loc::combine(
                            Loc::combine(ident.loc, args.loc),
                            Loc::combine(opening_paren, closing_paren),
                        );
                        let ident_loc = ident.loc;
                        Ok(Member::Expr(
                            Expr::Call(Box::new(Expr::Ident(ident).node(ident_loc)), args)
                                .node(loc),
                        )
                        .node(loc))
                    }
                }
            } else if matches!(parser.peek(), Some(Token::Colon)) {
                // named members (e.g. `val: 42`)
                let ident = Node::<Ident>::parse(parser)?;
                parser.eat();
                let expr = Node::<Expr>::parse(parser)?;
                let loc = Loc::combine(ident.loc, expr.loc);
                Ok(Member::Named(ident, expr).node(loc))
            } else {
                // unnamed members (e.g. `42`)
                let expr = Node::<Expr>::parse(parser)?;
                let loc = expr.loc;
                Ok(Member::Expr(expr).node(loc))
            }
        } else {
            let expr = Node::<Expr>::parse(parser)?;
            let loc = expr.loc;
            Ok(Member::Expr(expr).node(loc))
        }
    }
}
