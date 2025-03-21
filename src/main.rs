use std::fs::File;

use lexer::Lexer;

pub mod lexer;

fn main() {
    let mut file = File::open("test.lx").unwrap();
    let lexer = Lexer::new_from_file(&mut file).unwrap();
    let tokens = lexer.tokenize().unwrap();
    dbg!(&tokens);
    println!(
        "{}",
        tokens
            .into_iter()
            .map(|token| token.to_string())
            .reduce(|acc, x| acc + &x)
            .unwrap_or_default()
    );
}
