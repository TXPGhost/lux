//! Compiler for the Lux programming language

#![warn(missing_docs)]

use std::{fs::File, path::PathBuf};

use ast::{List, Member, Node};
use interpreter::{Context, Interpret, InterpretError};
use lexer::{LexError, Lexer};
use parser::{ParseError, Parser};

use colored::Colorize;

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
    println!("\n{}\n", "RUNNING TEST SUITE...".blue().bold());

    let paths = std::fs::read_dir("tests").unwrap();
    let (mut pass, mut fail, mut total) = (0, 0, 0);
    for path in paths {
        let path = path.unwrap().path();
        println!(
            "{} {}",
            "TEST".cyan().bold(),
            format!("\"{}\"", path.display()).purple()
        );

        match test_file(path) {
            Ok(_) => {
                pass += 1;
                total += 1;
                println!("\t{}", "PASS".green());
            }
            Err(e) => {
                fail += 1;
                total += 1;
                match e {
                    TestError::Io(e) => println!("\t{:<15}: {:?}", "FAIL_IO".red(), e),
                    TestError::Lex(e) => println!("\t{:<15}: {:?}", "FAIL_LEX".red(), e),
                    TestError::Parse(e) => println!("\t{:<15}: {:?}", "FAIL_PARSE".red(), e),
                    TestError::Interpret(e) => {
                        println!("\t{:<15}: {:?}", "FAIL_INTERPRET".red(), e)
                    }
                }
            }
        }

        println!();
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
