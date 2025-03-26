use super::*;

#[derive(Clone, Debug)]
pub enum DExpr {
    Ident(Node<DUniqueIdent>),
    Number(u64),
    Index(Box<Node<DExpr>>, Box<Node<DExpr>>),
    Field(Box<Node<DExpr>>, Node<DUniqueIdent>),
    Struct(Node<List<Node<DExpr>>>),
    Enum(Node<List<Node<DExpr>>>),
    Func(Node<List<Node<DExpr>>>, Box<Node<DExpr>>),
    Call(Box<Node<DExpr>>, Node<List<Node<DExpr>>>),
    Block(Node<List<Node<DStmt>>>),
    List(Node<List<Node<DExpr>>>),
    ListType(Option<Box<Node<DExpr>>>, Box<Node<DExpr>>),
}

#[derive(Clone, Debug)]
pub enum DUniqueIdent {
    TIdent(Rc<str>, usize),
    VIdent(Rc<str>, usize),
}

#[derive(Clone, Debug)]
pub enum DStmt {
    Expr(Node<DExpr>),
    Binding(Node<DUniqueIdent>),
}

pub trait Desugar: Clone + Debug {
    type Desugared;
    fn desugar(self) -> Self::Desugared;
}

impl<T: Desugar> Desugar for Node<T>
where
    T::Desugared: Clone + Debug,
{
    type Desugared = Node<T::Desugared>;
    fn desugar(self) -> Self::Desugared {
        Node {
            value: self.value.desugar(),
            loc: self.loc,
        }
    }
}

impl<T: Desugar> Desugar for List<T>
where
    T::Desugared: Clone + Debug,
{
    type Desugared = List<T::Desugared>;
    fn desugar(self) -> Self::Desugared {
        Self::Desugared {
            elements: self
                .elements
                .into_iter()
                .map(|element| element.desugar())
                .collect(),
        }
    }
}

impl Desugar for Node<Member> {
    type Desugared = Node<DExpr>;
    fn desugar(self) -> Self::Desugared {
        Node {
            value: match self.value {
                Member::Expr(expr) => expr.value.desugar(),
                Member::Named(_, node1) => todo!(),
                Member::NamedFunc(node, node1, node2) => todo!(),
            },
            loc: self.loc,
        }
    }
}

impl Desugar for Expr {
    type Desugared = DExpr;
    fn desugar(self) -> Self::Desugared {
        todo!()
    }
}
