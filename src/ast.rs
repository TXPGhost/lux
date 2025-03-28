use std::fmt::Debug;

use crate::lexer::LocatedToken;

/// The direct output of the parser
pub mod parse_tree;

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
