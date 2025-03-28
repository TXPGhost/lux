use crate::{
    ast::*,
    lexer::{Grouping, Operator, Token},
    parser::ParseError,
};

use super::Parse;

impl Parse for Node<Expr> {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        Self::parse_prec(parser, 0)
    }
}

impl Node<Expr> {
    fn parse_unop(
        parser: &mut super::Parser<'_>,
        prec: usize,
        operators: &[Operator],
    ) -> Result<Self, super::ParseError> {
        if let Some(Token::Operator(operator)) = parser.cur() {
            for op in operators {
                if op == operator {
                    let expr = Node::<Expr>::parse_prec(parser, prec + 1)?;
                    let loc = expr.loc;
                    return Ok(Expr::Unop(Unop::new(*op, expr).node(loc)).node(loc));
                }
            }
        }
        Node::<Expr>::parse_prec(parser, prec + 1)
    }

    fn parse_binop_next_prec(
        parser: &mut super::Parser<'_>,
        prec: usize,
        next_prec: usize,
        operators: &[Operator],
    ) -> Result<Self, super::ParseError> {
        let expr = Node::<Expr>::parse_prec(parser, prec + 1)?;
        if let Some(Token::Operator(operator)) = parser.cur() {
            for op in operators {
                if op == operator {
                    parser.eat();
                    let rhs = Node::<Expr>::parse_prec(parser, next_prec)?;
                    let loc = Loc::combine(expr.loc, rhs.loc);
                    return Ok(Expr::Binop(Binop::new(expr, *op, rhs).node(loc)).node(loc));
                }
            }
        }
        Ok(expr)
    }

    fn parse_binop_no_assoc(
        parser: &mut super::Parser<'_>,
        prec: usize,
        operators: &[Operator],
    ) -> Result<Self, super::ParseError> {
        Self::parse_binop_next_prec(parser, prec, prec + 1, operators)
    }

    fn parse_binop_right_assoc(
        parser: &mut super::Parser<'_>,
        prec: usize,
        operators: &[Operator],
    ) -> Result<Self, super::ParseError> {
        Self::parse_binop_next_prec(parser, prec, prec, operators)
    }

    fn parse_binop_left_assoc(
        parser: &mut super::Parser<'_>,
        prec: usize,
        operators: &[Operator],
    ) -> Result<Self, super::ParseError> {
        let mut expr = Node::<Expr>::parse_prec(parser, prec + 1)?;
        while let Some(Token::Operator(operator)) = parser.cur() {
            let mut proceed = false;
            for op in operators {
                if op == operator {
                    parser.eat();
                    let rhs = Node::<Expr>::parse_prec(parser, prec + 1)?;
                    let loc = Loc::combine(expr.loc, rhs.loc);
                    expr = Expr::Binop(Binop::new(expr, *op, rhs).node(loc)).node(loc);
                    proceed = true;
                    break;
                }
            }
            if !proceed {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_prec(parser: &mut super::Parser<'_>, prec: usize) -> Result<Self, super::ParseError> {
        match prec {
            // lambda operator (`=>`)
            0 => {
                let lambda = Self::parse_binop_right_assoc(parser, prec, &[Operator::FatArrow])?;
                let loc = lambda.loc;
                if let Expr::Binop(binop) = &lambda.val {
                    if let Operator::FatArrow = &binop.val.op {
                        if let Expr::Struct(fields) = &binop.val.lhs.val {
                            return Ok(Expr::Func(fields.clone(), binop.val.rhs.clone()).node(loc));
                        }
                    }
                }
                Ok(lambda)
            }
            // comparison operators (`==`, etc.)
            1 => Self::parse_binop_no_assoc(
                parser,
                prec,
                &[
                    Operator::DoubleEquals,
                    Operator::NotEquals,
                    Operator::Greater,
                    Operator::Less,
                    Operator::GreaterEquals,
                    Operator::LessEquals,
                ],
            ),
            // type difference (`\`)
            2 => Self::parse_binop_left_assoc(parser, prec, &[Operator::Backslash]),
            // type union (`|`)
            3 => Self::parse_binop_right_assoc(parser, prec, &[Operator::Or]),
            // type intersection (`&`)
            4 => Self::parse_binop_right_assoc(parser, prec, &[Operator::And]),
            // addition and subtraction (`+`, `-`, `++`)
            5 => Self::parse_binop_left_assoc(
                parser,
                prec,
                &[Operator::Plus, Operator::Minus, Operator::Concat],
            ),
            // multiplication, division, and modulo (`*`, `/`, `%`, `**`)
            6 => Self::parse_binop_left_assoc(
                parser,
                prec,
                &[
                    Operator::Times,
                    Operator::Divide,
                    Operator::Modulo,
                    Operator::Repeat,
                ],
            ),
            // pipe operator (`->`)
            7 => Self::parse_binop_left_assoc(parser, prec, &[Operator::ThinArrow]),
            // unary operators (`-`, `#`, etc.)
            8 => Self::parse_unop(
                parser,
                prec,
                &[
                    Operator::Plus,
                    Operator::Minus,
                    Operator::Length,
                    Operator::And,
                    Operator::Times,
                ],
            ),
            // fields indexes, and calls (`.`, `[]`, `()`)
            9 => {
                let mut expr = Node::<Expr>::parse_prec(parser, prec + 1)?;
                loop {
                    match parser.cur() {
                        Some(Token::Operator(Operator::Dot)) => {
                            parser.eat();
                            let field = Node::<Field>::parse(parser)?;
                            let loc = Loc::combine(expr.loc, field.loc);
                            expr = Expr::Field(Box::new(expr), field).node(loc);
                            continue;
                        }
                        Some(Token::Open(Grouping::Paren)) => {
                            parser.eat();
                            let args = Node::<Vec<Node<Member>>>::parse(parser)?;
                            let loc = Loc::combine(expr.loc, args.loc);
                            expr = Expr::Call(Box::new(expr), args).node(loc);
                            if !matches!(parser.cur(), Some(Token::Close(Grouping::Paren))) {
                                return Err(ParseError::ExpectedToken(
                                    "while parsing index",
                                    Token::Close(Grouping::Bracket),
                                    parser.cur_loc().cloned(),
                                ));
                            }
                            parser.eat();
                            continue;
                        }
                        Some(Token::Open(Grouping::Bracket)) => {
                            parser.eat();
                            let index = Node::<Expr>::parse(parser)?;
                            let loc = Loc::combine(expr.loc, index.loc);
                            expr = Expr::Index(Box::new(expr), Box::new(index)).node(loc);
                            if !matches!(parser.cur(), Some(Token::Close(Grouping::Bracket))) {
                                return Err(ParseError::ExpectedToken(
                                    "while parsing index",
                                    Token::Close(Grouping::Bracket),
                                    parser.cur_loc().cloned(),
                                ));
                            }
                            parser.eat();
                            continue;
                        }
                        _ => break,
                    }
                }
                Ok(expr)
            }
            // explicit grouping (`()`, `[]`, `<>`)
            10 => match parser.cur() {
                Some(Token::Open(Grouping::Paren)) => {
                    let open_loc = Loc::from_token(parser.cur_loc().unwrap());
                    parser.eat();
                    let fields = Node::<Vec<Node<Member>>>::parse(parser)?;
                    if !matches!(parser.cur(), Some(Token::Close(Grouping::Paren))) {
                        return Err(ParseError::ExpectedToken(
                            "while parsing struct",
                            Token::Close(Grouping::Bracket),
                            parser.cur_loc().cloned(),
                        ));
                    }
                    let close_loc = Loc::from_token(parser.cur_loc().unwrap());
                    parser.eat();
                    let loc =
                        Loc::combine(fields.loc, Loc::combine(Some(open_loc), Some(close_loc)));
                    Ok(Expr::Struct(fields).node(loc))
                }
                Some(Token::Open(Grouping::Angle)) => {
                    let open_loc = Loc::from_token(parser.cur_loc().unwrap());
                    parser.eat();
                    let variants = Node::<Vec<Node<Member>>>::parse(parser)?;
                    if !matches!(parser.cur(), Some(Token::Close(Grouping::Angle))) {
                        return Err(ParseError::ExpectedToken(
                            "while parsing enum",
                            Token::Close(Grouping::Bracket),
                            parser.cur_loc().cloned(),
                        ));
                    }
                    let close_loc = Loc::from_token(parser.cur_loc().unwrap());
                    parser.eat();
                    let loc =
                        Loc::combine(variants.loc, Loc::combine(Some(open_loc), Some(close_loc)));
                    Ok(Expr::Enum(variants).node(loc))
                }
                Some(Token::Open(Grouping::Curly)) => {
                    parser.eat();
                    let open_loc = Loc::from_token(parser.cur_loc().unwrap());
                    let stmts = Node::<Vec<Node<Stmt>>>::parse(parser)?;
                    if !matches!(parser.cur(), Some(Token::Close(Grouping::Curly))) {
                        return Err(ParseError::ExpectedToken(
                            "while parsing block",
                            Token::Close(Grouping::Bracket),
                            parser.cur_loc().cloned(),
                        ));
                    }
                    let close_loc = Loc::from_token(parser.cur_loc().unwrap());
                    parser.eat();
                    let loc =
                        Loc::combine(stmts.loc, Loc::combine(Some(open_loc), Some(close_loc)));
                    Ok(Expr::Block(stmts).node(loc))
                }
                Some(Token::Open(Grouping::Bracket)) => {
                    let open_loc = Loc::from_token(parser.cur_loc().unwrap());
                    parser.eat();
                    let mut elements = Node::<Vec<Node<Expr>>>::parse(parser)?;
                    if !matches!(parser.cur(), Some(Token::Close(Grouping::Bracket))) {
                        return Err(ParseError::ExpectedToken(
                            "while parsing list",
                            Token::Close(Grouping::Bracket),
                            parser.cur_loc().cloned(),
                        ));
                    }
                    let close_loc = Loc::from_token(parser.cur_loc().unwrap());
                    parser.eat();
                    let loc =
                        Loc::combine(elements.loc, Loc::combine(Some(open_loc), Some(close_loc)));
                    if matches!(
                        parser.cur(),
                        Some(Token::VIdent(_) | Token::TIdent(_) | Token::Open(Grouping::Paren))
                    ) {
                        let ty = Node::<Expr>::parse(parser)?;
                        let loc = Loc::combine(loc, ty.loc);
                        match elements.val.len() {
                            0 => Ok(Expr::ArrayType(None, Box::new(ty)).node(loc)),
                            1 => Ok(Expr::ArrayType(
                                Some(Box::new(elements.val.pop().unwrap())),
                                Box::new(ty),
                            )
                            .node(loc)),
                            n => Err(ParseError::IllegalListType("multiple list sizes", n)),
                        }
                    } else {
                        Ok(Expr::Array(elements).node(loc))
                    }
                }
                _ => Self::parse_prec(parser, prec + 1),
            },
            // identifiers and literals
            11 => match parser.cur() {
                Some(Token::VIdent(_) | Token::TIdent(_) | Token::Operator(Operator::Caret)) => {
                    let ident = Node::<Ident>::parse(parser)?;
                    let loc = ident.loc;
                    Ok(Expr::Ident(ident).node(loc))
                }
                Some(Token::Number(n)) => {
                    let loc = Loc::from_token(parser.cur_loc().unwrap());
                    parser.eat();
                    Ok(Expr::Primitive(Primitive::U64Val(
                        n.parse().map_err(ParseError::NumberParseError)?,
                    ))
                    .loc(loc))
                }
                _ => Err(ParseError::UnexpectedToken(
                    "while parsing expr",
                    parser.cur_loc().cloned(),
                )),
            },
            _ => unreachable!(),
        }
    }
}
