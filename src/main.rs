#![warn(missing_docs)]

use std::fs::File;

use interpreter::{Context, Interpret};
use lexer::Lexer;
use parser::Parser;

/// The abstract syntax tree
pub mod ast;

/// Evaluates programs using the interpreter
pub mod interpreter;

/// Tokenizes an input stream of text
pub mod lexer;

/// Produces an abstract syntax tree by parsing the lexer output
pub mod parser;

fn main() {
    let mut file = File::open("test.lx").unwrap();
    let lexer = Lexer::new_from_file(&mut file).unwrap();
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(&tokens);
    let ast = parser.parse().unwrap();

    let mut context = Context::default();
    let answer = ast.interp(&mut context).unwrap();
    dbg!(answer);
}
