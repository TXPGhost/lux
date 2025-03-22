use std::fs::File;

use lexer::Lexer;
use parser::Parser;

pub mod ast;
pub mod lexer;
pub mod parser;

fn main() {
    let mut file = File::open("test.lx").unwrap();
    let lexer = Lexer::new_from_file(&mut file).unwrap();
    let tokens = lexer.tokenize().unwrap();
    println!(
        "{}",
        tokens
            .iter()
            .map(|token| token.token.to_string())
            .reduce(|acc, x| acc + &x)
            .unwrap_or_default()
    );
    let mut parser = Parser::new(&tokens);
    let ast = parser.parse().unwrap();
    dbg!(ast);
}
