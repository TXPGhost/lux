use std::{collections::HashMap, fmt::Debug};

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

impl<T: Interpret + Clone + Debug> Interpret for Node<List<T>>
where
    T::Output: Clone + Debug,
{
    type Output = Node<List<T::Output>>;
    fn interpret(self, context: &mut Context) -> Result<Self::Output, InterpretError> {
        let loc = self.loc;
        let mut new_elements = Vec::with_capacity(self.value.elements.len());
        for element in self.value.elements {
            new_elements.push(element.interpret(context)?)
        }
        Ok(Node {
            value: List {
                elements: new_elements,
            },
            loc,
        })
    }
}

impl Interpret for Node<Expr> {
    type Output = Node<Expr>;
    fn interpret(self, context: &mut Context) -> Result<Self::Output, InterpretError> {
        let loc = self.loc;
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
                    Operator::ThinArrow => todo!(),
                    Operator::FatArrow => todo!(),
                    _ => todo!(),
                }
            }
            Expr::Func(args, body) => todo!(),
            Expr::Index(node, node1) => todo!(),
            Expr::Field(expr, field) => todo!(),
            Expr::Struct(members) => {
                let loc = members.loc;
                Ok(Node {
                    value: Expr::Struct(members.interpret(context)?),
                    loc,
                })
            }
            Expr::Enum(variants) => {
                let loc = variants.loc;
                Ok(Node {
                    value: Expr::Enum(variants.interpret(context)?),
                    loc,
                })
            }
            Expr::Call(node, node1) => todo!(),
            Expr::Block(node) => todo!(),
            Expr::List(node) => todo!(),
            Expr::ListType(node, node1) => todo!(),
        }
    }
}

impl Interpret for Node<Member> {
    type Output = Node<Member>;

    fn interpret(self, context: &mut Context) -> Result<Self::Output, InterpretError> {
        let loc = self.loc;
        match self.value {
            Member::Expr(expr) => Ok(Node {
                value: Member::Expr(expr.interpret(context)?),
                loc,
            }),
            Member::Named(ident, expr) => Ok(Node {
                value: Member::Named(ident, expr.interpret(context)?),
                loc,
            }),
            Member::NamedFunc(ident, args, expr) => Ok(Node {
                value: Member::Named(
                    ident,
                    Node {
                        value: Expr::Func(
                            args.interpret(context)?,
                            Box::new(expr.interpret(context)?),
                        ),
                        loc,
                    },
                ),
                loc,
            }),
        }
    }
}
