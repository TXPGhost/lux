use std::fmt::Debug;

use crate::lexer::{LocatedToken, Operator};

/// The direct output of the parser (IR level 1)
pub mod parse_tree;

/// The desugared and flattened AST (IR level 2)
pub mod flatten;

/// An AST node, with a value and a source file location
#[derive(Clone, Debug)]
pub struct Node<T: Clone + Debug> {
    /// The value of the AST node
    pub val: T,

    /// The AST node's location
    pub loc: Option<Loc>,
}

impl<T: Clone + Debug> Node<T> {
    /// Swaps the value of this [Node] for another
    pub fn with_val<U: Clone + Debug>(self, callback: impl FnOnce(T) -> U) -> Node<U> {
        Node {
            val: callback(self.val),
            loc: self.loc,
        }
    }
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

/// A helper trait for constructing [Node]s
pub trait NodeExt: Clone + Debug {
    /// Constructs a located [Node] from [self] and the given [Loc]
    fn loc(self, loc: Loc) -> Node<Self> {
        Node {
            val: self,
            loc: Some(loc),
        }
    }

    /// Constructs a located [Node] from [self] and the given optional [Loc]
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

/// A compiler-intrinsic primitive type
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Primitive {
    /// The unsigned 64-bit integer type
    U64Ty,

    /// An unsigned 64-bit integer value
    U64Val(u64),

    /// The ascii character type
    CharTy,

    /// An ascii character value
    CharVal(u8),

    /// The boolean type
    Bool,

    /// The true value
    True,

    /// The false value
    False,

    /// A helper to debug-print values
    DebugPrint,

    /// A helper to assert equality
    Assert(Assertion),

    /// A binary operation,
    Binop(Operator),

    /// A unary operation
    Unop(Operator),
}

/// A compiler assertion
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Assertion {
    /// Assert that two values are equal
    Eq,

    /// Assert that two values are not equal
    Ne,

    /// Assert that the left-hand-side is a subtype of the right-hand-side
    Subtype,

    /// Assert that the left-hand-side is a supertype of the right-hand-side
    Supertype,
}
