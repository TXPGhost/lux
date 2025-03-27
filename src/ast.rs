use std::{fmt::Debug, sync::Arc};

use crate::lexer::{LocatedToken, Operator};

use lazy_static::lazy_static;

/// An AST node, with a value and a source file location
#[derive(Clone, Debug)]
pub struct Node<T: Clone + Debug> {
    /// The value of the AST node
    pub val: T,

    /// The AST node's location
    pub loc: Option<Loc>,
}

/// A source file location "box" given by a range of lines and columns
#[derive(Copy, Clone, Debug)]
pub struct Loc {
    /// The minimum line number of the box
    pub line_min: usize,

    /// The maximum line number of the box
    pub line_max: usize,

    /// The minimum column number of the box
    pub col_min: usize,

    /// The maximum column number of the box
    pub col_max: usize,
}

impl Loc {
    /// Combines two locations into the smallest enclosing box
    pub fn combine(lhs: Option<Self>, rhs: Option<Self>) -> Option<Self> {
        match (lhs, rhs) {
            (Some(lhs), Some(rhs)) => Some(Self {
                line_min: lhs.line_min.min(rhs.line_min),
                line_max: lhs.line_max.max(rhs.line_max),
                col_min: lhs.col_min.min(rhs.col_min),
                col_max: lhs.col_max.max(rhs.col_max),
            }),
            (Some(lhs), None) => Some(lhs),
            (None, Some(rhs)) => Some(rhs),
            (None, None) => None,
        }
    }

    /// Constructs a box of minimal size surrounding a token
    pub fn from_token(token: &LocatedToken) -> Self {
        Self {
            line_min: token.line,
            line_max: token.line,
            col_min: token.col_start,
            col_max: token.col_end,
        }
    }
}

/// A list of AST elements
#[derive(Clone, Debug)]
pub struct List<T> {
    /// The elements of the list
    pub elements: Vec<T>,
}

impl<T> List<T> {
    /// Constructs a new [List<T>] from the given elements
    pub fn new(elements: impl Into<Vec<T>>) -> Self {
        Self {
            elements: elements.into(),
        }
    }

    /// Constructs an empty [List<T>]
    pub fn empty() -> Self {
        Self {
            elements: Vec::new(),
        }
    }
}

/// A member AST node, used within structs and enums
#[derive(Clone, Debug)]
pub enum Member {
    /// An unnamed member, represented just as an expression
    Expr(Node<Expr>),

    /// A named member, represented as an identifier and an expression
    Named(Node<Ident>, Node<Expr>),

    /// A named function member, syntax sugar for a lambda function
    NamedFunc(Node<Ident>, Node<List<Node<Member>>>, Node<Expr>),
}

/// A generic expression
#[derive(Clone, Debug)]
pub enum Expr {
    /// An identifier (e.g. `Vector3` or `x`)
    Ident(Node<Ident>),

    /// A binary operation (e.g. `x + y`)
    Binop(Node<Binop>),

    /// An array index (e.g. `array[4]`)
    Index(Box<Node<Expr>>, Box<Node<Expr>>),

    /// A struct field or enum variant (e.g. `person.height`)
    Field(Box<Node<Expr>>, Node<Field>),

    /// A struct (e.g. `('x', 'y', 'z')` or `(x: 1.0, y: 2.0)`)
    Struct(Node<List<Node<Member>>>),

    /// An enum (e.g. `<VariantA, VariantB>` or `<some: I32, None: ()>`)
    Enum(Node<List<Node<Member>>>),

    /// A function call or a constructor (e.g. `func(x, y)` or `Vector3(1.0, 2.0, 3.0)`)
    Call(Box<Node<Expr>>, Node<List<Node<Member>>>),

    /// A function declaration (e.g. `(x: I32) => x * 2`)
    Func(Node<List<Node<Member>>>, Box<Node<Expr>>),

    /// A code block (e.g. `{ x = 10, y = 20, x + y }`)
    Block(Node<List<Node<Stmt>>>),

    /// An array (e.g. `[1, 2, 3, 4, 5]`)
    Array(Node<List<Node<Expr>>>),

    /// An array type (e.g. `[4]I32` or `[]String`)
    ArrayType(Option<Box<Node<Expr>>>, Box<Node<Expr>>),

    /// A builtin primitive (e.g. numeric types)
    Primitive(Primitive),
}

impl Expr {
    /// Generates a unit expression `()`
    pub fn unit() -> Self {
        Expr::Struct(List::empty().unloc())
    }

    /// Generates a never expression `<>`
    pub fn never() -> Self {
        Expr::Enum(List::empty().unloc())
    }
}

/// An identifier
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ident {
    /// A value identifier written in snake case (e.g. `foo_bar`)
    VIdent(Arc<str>),

    /// A type identifier written in pascal case (e.g. `FooBar`)
    TIdent(Arc<str>),

    /// A hoisted identifier to access previously-shadowed identifiers (e.g. `^old_x`)
    Hoist(Box<Self>),
}

lazy_static! {
    static ref IDENT_VOID: Arc<str> = "_".into();
}

impl Ident {
    /// The void identifier `_`, which discards its value
    pub fn void() -> Self {
        Self::VIdent(IDENT_VOID.clone())
    }

    /// Returns [true] if this identifier is the void identifier `_`
    pub fn is_void(&self) -> bool {
        match self {
            Ident::VIdent(vident) => vident.as_ref() == "_",
            Ident::TIdent(tident) => tident.as_ref() == "_",
            Ident::Hoist(ident) => ident.is_void(),
        }
    }
}

/// A binary operation (e.g. `x + y`)
#[derive(Clone, Debug)]
pub struct Binop {
    /// The left-hand-side of the operation
    pub lhs: Box<Node<Expr>>,

    /// The right-hand-side of the operation
    pub rhs: Box<Node<Expr>>,

    /// The operator to use
    pub op: Operator,
}

impl Binop {
    /// Constructs a new [Binop] with the given [Operator] and operands
    pub fn new(lhs: Node<Expr>, op: Operator, rhs: Node<Expr>) -> Self {
        Self {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
        }
    }
}

/// A unary operation (e.g. `-x`)
#[derive(Clone, Debug)]
pub struct Unop {
    /// The (left-hand-size) expression
    pub expr: Box<Node<Expr>>,

    /// The operator to use
    pub op: Operator,
}

/// A statement, used within code blocks
#[derive(Clone, Debug)]
pub enum Stmt {
    /// An expression statement
    Expr(Node<Expr>),

    /// A local binding (e.g. `x = 5`)
    Binding(Node<Ident>, Option<Node<Expr>>, Node<Expr>),
}

/// A field, used to get members of structs and enums
#[derive(Clone, Debug)]
pub enum Field {
    /// An identified field, used to access named members
    Ident(Ident),

    /// A numbered field, used to access unnamed members
    Number(u64),
}

/// A compiler-intrinsic primitive type
#[derive(Clone, Debug)]
pub enum Primitive {
    /// The unsigned 64-bit integer type
    U64Ty,

    /// An unsigned 64-bit integer value
    U64Val(u64),

    /// The boolean type
    Bool,

    /// The true value
    True,

    /// The false value
    False,

    /// A helper to debug-print values
    DebugPrint,
}

/// A helper trait for constructing [Node]s
pub trait NodeExt: Clone + Debug {
    /// Constructs a located [Node] from [self] and the given [Loc]
    fn loc(self, loc: Loc) -> Node<Self> {
        Node {
            val: self,
            loc: Some(loc),
        }
    }

    /// Constructs a located [Node] from [self] and the given [Option<Loc>]
    fn node(self, loc: Option<Loc>) -> Node<Self> {
        Node { val: self, loc }
    }

    /// Constructs an unlocated [Node] from [self] (i.e. with `loc` set to [None])
    fn unloc(self) -> Node<Self> {
        Node {
            val: self,
            loc: None,
        }
    }
}

impl<T: Clone + Debug> NodeExt for T {}
