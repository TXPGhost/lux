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

/// Checks whether values are subtypes/supertypes of one another
pub mod type_checker;

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
    let (mut pass, mut fail, mut total) = (0, 0, 0);
    for path in paths {
        let path = path.unwrap().path();
        print!("RUNNING TEST {:.<35}", path.display());
        match test_file(path) {
            Ok(_) => {
                pass += 1;
                total += 1;
                println!("PASS");
            }
            Err(e) => {
                fail += 1;
                total += 1;
                match e {
                    TestError::Io(e) => println!("{:<15}: {:?}", "FAIL_IO", e),
                    TestError::Lex(e) => println!("{:<15}: {:?}", "FAIL_LEX", e),
                    TestError::Parse(e) => println!("{:<15}: {:?}", "FAIL_PARSE", e),
                    TestError::Interpret(e) => println!("{:<15}: {:?}", "FAIL_INTERPRET", e),
                }
            }
        }
    }

    println!();
    println!(
        "PASS: {}/{} ({:.1}%)",
        pass,
        total,
        (pass as f32 / total as f32) * 100.0
    );
    println!(
        "FAIL: {}/{} ({:.1}%)",
        fail,
        total,
        (fail as f32 / total as f32) * 100.0
    );
}
