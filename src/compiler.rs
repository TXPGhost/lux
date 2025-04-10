use logos::Logos;

use crate::lexer::*;

pub enum CompilationError {
    Lex(),
}

pub fn compile(source: &str) -> Result<(), CompilationError> {
    let lexer = Token::lexer(source);
    for token in lexer {
        println!("{:?}", token);
    }

    Ok(())
}
