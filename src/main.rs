//! Compiler for the Lux programming language

#![warn(missing_docs)]

use std::{fs::File, path::PathBuf};

use ast::{List, Member, Node};
use interpreter::{Context, Interpret, InterpretError};
use lexer::{LexError, Lexer};
use parser::{ParseError, Parser};

/// The abstract syntax tree
pub mod ast;

/// Evaluates programs using the interpreter
pub mod interpreter;

/// Tokenizes an input stream of text
pub mod lexer;

/// Produces an abstract syntax tree by parsing the lexer output
pub mod parser;

/// An error that can occur during testing
#[derive(Debug)]
#[allow(dead_code)]
enum TestError {
    /// An io error
    Io(std::io::Error),

    /// A lexing error
    Lex(LexError),

    /// A parsing error
    Parse(ParseError),

    /// An interpreter error
    Interpret(InterpretError),
}

fn test_file(path: PathBuf) -> Result<Node<List<Node<Member>>>, TestError> {
    let mut file = File::open(path).map_err(TestError::Io)?;
    let lexer = Lexer::new_from_file(&mut file).map_err(TestError::Io)?;
    let tokens = lexer.tokenize().map_err(TestError::Lex)?;
    let mut parser = Parser::new(&tokens);
    let ast = parser.parse().map_err(TestError::Parse)?;
    let mut context = Context::default();
    let answer = ast.interp(&mut context).map_err(TestError::Interpret)?;
    Ok(answer)
}

fn main() {
    let paths = std::fs::read_dir("tests").unwrap();
    for path in paths {
        let path = path.unwrap().path();
        print!("RUNNING TEST {} ... ", path.display());
        match test_file(path) {
            Ok(_) => println!("PASS"),
            Err(TestError::Io(e)) => println!("FAIL_IO: {:?}", e),
            Err(TestError::Lex(e)) => println!("FAIL_LEX: {:?}", e),
            Err(TestError::Parse(e)) => println!("FAIL_PARSE: {:?}", e),
            Err(TestError::Interpret(e)) => println!("FAIL_INTERPRET: {:?}", e),
        }
    }
}
