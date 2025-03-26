use std::fs::File;

use interpreter::{Context, Interpret};
use lexer::Lexer;
use parser::Parser;

pub mod ast;
pub mod interpreter;
pub mod lexer;
pub mod parser;

fn main() {
    let mut file = File::open("test.lx").unwrap();
    let lexer = Lexer::new_from_file(&mut file).unwrap();
    let tokens = lexer.tokenize().unwrap();
    let mut parser = Parser::new(&tokens);
    let ast = parser.parse().unwrap();

    let mut context = Context::default();
    let answer = ast.eval(&mut context).unwrap();
    dbg!(answer);
}
