use crate::{
    ast::*,
    lexer::{Grouping, Operator, Token},
    parser::ParseError,
};

use super::Parse;

impl Parse for ASTExpr {
    fn parse(parser: &mut super::Parser<'_>) -> Result<Self, super::ParseError> {
        Self::parse_prec(parser, 0)
    }
}

impl ASTExpr {
    fn parse_binop_next_prec(
        parser: &mut super::Parser<'_>,
        prec: usize,
        next_prec: usize,
        operators: &[Operator],
    ) -> Result<Self, super::ParseError> {
        let expr = ASTExpr::parse_prec(parser, prec + 1)?;
        if let Some(Token::Operator(operator)) = parser.cur() {
            for op in operators {
                if op == operator {
                    parser.eat()?;
                    let rhs = ASTExpr::parse_prec(parser, next_prec)?;
                    return Ok(ASTExpr::Binop(ASTBinop {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                        op: *op,
                    }));
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
        let mut expr = ASTExpr::parse_prec(parser, prec + 1)?;
        while let Some(Token::Operator(operator)) = parser.cur() {
            let mut proceed = false;
            for op in operators {
                if op == operator {
                    parser.eat()?;
                    let rhs = ASTExpr::parse_prec(parser, prec)?;
                    expr = ASTExpr::Binop(ASTBinop {
                        lhs: Box::new(expr),
                        rhs: Box::new(rhs),
                        op: *op,
                    });
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
            0 => Self::parse_binop_right_assoc(parser, prec, &[Operator::FatArrow]),
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
                let mut expr = ASTExpr::parse_prec(parser, prec + 1)?;
                loop {
                    match parser.cur() {
                        Some(Token::Operator(Operator::Dot)) => {
                            parser.eat()?;
                            let rhs = ASTExpr::parse_prec(parser, prec)?;
                            expr = ASTExpr::Binop(ASTBinop {
                                lhs: Box::new(expr),
                                rhs: Box::new(rhs),
                                op: Operator::Dot,
                            });
                            continue;
                        }
                        Some(Token::Open(Grouping::Paren)) => {
                            parser.eat()?;
                            let args = ASTList::parse(parser)?;
                            expr = ASTExpr::Func(Box::new(expr), args);
                            if !matches!(parser.cur(), Some(Token::Close(Grouping::Paren))) {
                                return Err(ParseError::ExpectedToken(
                                    "while parsing index",
                                    Token::Close(Grouping::Bracket),
                                    parser.cur_loc().cloned(),
                                ));
                            }
                            parser.eat()?;
                            continue;
                        }
                        Some(Token::Open(Grouping::Bracket)) => {
                            parser.eat()?;
                            let index = ASTExpr::parse(parser)?;
                            expr = ASTExpr::Index(Box::new(expr), Box::new(index));
                            if !matches!(parser.cur(), Some(Token::Close(Grouping::Bracket))) {
                                return Err(ParseError::ExpectedToken(
                                    "while parsing index",
                                    Token::Close(Grouping::Bracket),
                                    parser.cur_loc().cloned(),
                                ));
                            }
                            parser.eat()?;
                            continue;
                        }
                        _ => break,
                    }
                }
                Ok(expr)
            }
            // explicit grouping (`()`)
            10 => match parser.cur() {
                Some(Token::Open(Grouping::Paren)) => {
                    println!("PARSING EXPLICIT PARENS...");
                    parser.eat()?;
                    let list = ASTList::parse(parser)?;
                    if !matches!(parser.cur(), Some(Token::Close(Grouping::Paren))) {
                        return Err(ParseError::ExpectedToken(
                            "while parsing struct",
                            Token::Close(Grouping::Bracket),
                            parser.cur_loc().cloned(),
                        ));
                    }
                    parser.eat()?;
                    Ok(ASTExpr::Struct(list))
                }
                Some(Token::Open(Grouping::Curly)) => {
                    println!("PARSING EXPLICIT CURLYS...");
                    parser.eat()?;
                    let list = ASTList::parse(parser)?;
                    if !matches!(parser.cur(), Some(Token::Close(Grouping::Curly))) {
                        return Err(ParseError::ExpectedToken(
                            "while parsing block",
                            Token::Close(Grouping::Bracket),
                            parser.cur_loc().cloned(),
                        ));
                    }
                    parser.eat()?;
                    Ok(ASTExpr::Block(list))
                }
                _ => Self::parse_prec(parser, prec + 1),
            },
            // identifiers
            11 => Ok(ASTExpr::Ident(ASTIdent::parse(parser)?)),
            prec => Err(ParseError::IllegalPrecedenceLevel(
                prec,
                parser.cur_loc().cloned(),
            )),
        }
    }
}
