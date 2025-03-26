use std::collections::HashMap;

use crate::{ast::*, lexer::Operator};

pub struct Context {
    definitions: Vec<Node<Expr>>,
    associations: HashMap<Ident, usize>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            definitions: Vec::new(),
            associations: HashMap::new(),
        }
    }

    pub fn lookup(&self, ident: &Ident) -> Option<&Node<Expr>> {
        let index = self.associations.get(ident)?;
        let expr = self.definitions.get(*index)?;
        Some(expr)
    }

    pub fn add(&mut self, ident: Ident, expr: Node<Expr>) {
        self.associations.insert(ident, self.definitions.len());
        self.definitions.push(expr);
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

pub enum InterpretError {
    UndefinedSymbol(Ident),
    TypeMismatch(String),
    IllegalFieldOperation(String),
}

pub trait Interpret {
    type Output;
    fn interpret(self, context: &mut Context) -> Result<Self::Output, InterpretError>;
}

impl Interpret for Node<Expr> {
    type Output = Node<Expr>;
    fn interpret(self, context: &mut Context) -> Result<Self::Output, InterpretError> {
        match self.value {
            Expr::Ident(node) => match context.lookup(&node.value) {
                Some(expr) => Ok(expr.clone()),
                None => Err(InterpretError::UndefinedSymbol(node.value)),
            },
            Expr::Number(_) => Ok(self),
            Expr::Binop(binop) => {
                let lhs = binop.value.lhs.interpret(context)?;
                let rhs = binop.value.rhs.interpret(context)?;

                if let (Expr::Number(x), Expr::Number(y)) = (&lhs.value, &rhs.value) {
                    let loc = Loc::combine(lhs.loc, rhs.loc);
                    return match binop.value.op {
                        Operator::Plus => Ok(Node {
                            value: Expr::Number(x + y),
                            loc,
                        }),
                        Operator::Minus => Ok(Node {
                            value: Expr::Number(x - y),
                            loc,
                        }),
                        Operator::Times => Ok(Node {
                            value: Expr::Number(x * y),
                            loc,
                        }),
                        Operator::Divide => Ok(Node {
                            value: Expr::Number(x / y),
                            loc,
                        }),
                        Operator::Modulo => Ok(Node {
                            value: Expr::Number(x % y),
                            loc,
                        }),
                        Operator::DoubleEquals => todo!("comparison"),
                        Operator::NotEquals => todo!("comparison"),
                        Operator::Greater => todo!("comparison"),
                        Operator::Less => todo!("comparison"),
                        Operator::GreaterEquals => todo!("comparison"),
                        Operator::LessEquals => todo!("comparison"),
                        _ => Err(InterpretError::TypeMismatch(format!(
                            "illegal operation {:?} on numbers",
                            binop.value.op
                        ))),
                    };
                }

                match binop.value.op {
                    Operator::Dot => {
                        let Expr::Struct(members) = lhs.value else {
                            return Err(InterpretError::IllegalFieldOperation(format!(
                                "cannot get field of a non-struct expression {:?}",
                                lhs
                            )));
                        };
                        match rhs.value {
                            Expr::Ident(ident) => {
                                for member in members.value.elements {
                                    match member.value {
                                        Member::Expr(_) => continue,
                                        Member::Named(member_ident, expr) => {
                                            if ident.value == member_ident.value {
                                                return Ok(expr);
                                            }
                                        }
                                        Member::NamedFunc(member_ident, args, expr) => {
                                            if ident.value == member_ident.value {
                                                let loc = Loc::combine(args.loc, expr.lco);
                                                return Ok(Node {
                                                    value: Expr::Func(args, Box::new(expr)),
                                                    loc,
                                                });
                                            }
                                        }
                                    }
                                }
                                todo!()
                            }
                            Expr::Number(number) => {
                                if number >= 
                            }
                            _ => Err(InterpretError::IllegalFieldOperation(format!(
                                "struct field must be an identifier or a number {:?}",
                                rhs
                            ))),
                        }
                    }
                    Operator::ThinArrow => todo!(),
                    Operator::FatArrow => todo!(),
                    _ => todo!(),
                }
            }
            Expr::Func(args, body) => todo!()
            Expr::Index(node, node1) => todo!(),
            Expr::Struct(node) => todo!(),
            Expr::Enum(node) => todo!(),
            Expr::Call(node, node1) => todo!(),
            Expr::Block(node) => todo!(),
            Expr::List(node) => todo!(),
            Expr::ListType(node, node1) => todo!(),
        }
    }
}
