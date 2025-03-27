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
                    let expr = Expr::Binop(Node {
                        value: Binop {
                            lhs: Box::new(expr),
                            rhs: Box::new(rhs),
                            op: *op,
                        },
                        loc,
                    });
                    return Ok(Node { value: expr, loc });
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
                    let rhs = Node::<Expr>::parse_prec(parser, prec)?;
                    let loc = Loc::combine(expr.loc, rhs.loc);
                    expr = Node {
                        value: Expr::Binop(Node {
                            value: Binop {
                                lhs: Box::new(expr),
                                rhs: Box::new(rhs),
                                op: *op,
                            },
                            loc,
                        }),
                        loc,
                    };
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
                if let Expr::Binop(binop) = &lambda.value {
                    if let Operator::FatArrow = &binop.value.op {
                        if let Expr::Struct(members) = &binop.value.lhs.value {
                            return Ok(Node {
                                value: Expr::Func(members.clone(), binop.value.rhs.clone()),
                                loc,
                            });
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
            8 => Self::parse_binop_right_assoc(
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
                            expr = Node {
                                value: Expr::Field(Box::new(expr), field),
                                loc,
                            };
                            continue;
                        }
                        Some(Token::Open(Grouping::Paren)) => {
                            parser.eat();
                            let args = Node::<List<Node<Member>>>::parse(parser)?;
                            let loc = Loc::combine(expr.loc, args.loc);
                            expr = Node {
                                value: Expr::Call(Box::new(expr), args),
                                loc,
                            };
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
                            expr = Node {
                                value: Expr::Index(Box::new(expr), Box::new(index)),
                                loc,
                            };
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
                    let members = Node::<List<Node<Member>>>::parse(parser)?;
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
                        Loc::combine(members.loc, Loc::combine(Some(open_loc), Some(close_loc)));
                    Ok(Node {
                        value: Expr::Struct(members),
                        loc,
                    })
                }
                Some(Token::Open(Grouping::Angle)) => {
                    let open_loc = Loc::from_token(parser.cur_loc().unwrap());
                    parser.eat();
                    let variants = Node::<List<Node<Member>>>::parse(parser)?;
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
                    Ok(Node {
                        value: Expr::Enum(variants),
                        loc,
                    })
                }
                Some(Token::Open(Grouping::Curly)) => {
                    parser.eat();
                    let open_loc = Loc::from_token(parser.cur_loc().unwrap());
                    let stmts = Node::<List<Node<Stmt>>>::parse(parser)?;
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
                    Ok(Node {
                        value: Expr::Block(stmts),
                        loc,
                    })
                }
                Some(Token::Open(Grouping::Bracket)) => {
                    let open_loc = Loc::from_token(parser.cur_loc().unwrap());
                    parser.eat();
                    let mut elements = Node::<List<Node<Expr>>>::parse(parser)?;
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
                        match elements.value.elements.len() {
                            0 => Ok(Node {
                                value: Expr::ArrayType(None, Box::new(ty)),
                                loc,
                            }),
                            1 => Ok(Node {
                                value: Expr::ArrayType(
                                    Some(Box::new(elements.value.elements.pop().unwrap())),
                                    Box::new(ty),
                                ),
                                loc,
                            }),
                            n => Err(ParseError::IllegalListType("multiple list sizes", n)),
                        }
                    } else {
                        Ok(Node {
                            value: Expr::Array(elements),
                            loc,
                        })
                    }
                }
                _ => Self::parse_prec(parser, prec + 1),
            },
            // identifiers and literals
            11 => match parser.cur() {
                Some(Token::VIdent(_) | Token::TIdent(_)) => {
                    let ident = Node::<Ident>::parse(parser)?;
                    let loc = ident.loc;
                    Ok(Node {
                        value: Expr::Ident(ident),
                        loc,
                    })
                }
                Some(Token::Number(n)) => {
                    let loc = Loc::from_token(parser.cur_loc().unwrap());
                    parser.eat();
                    Ok(Node {
                        value: Expr::Number(n.parse().map_err(ParseError::NumberParseError)?),
                        loc: Some(loc),
                    })
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
