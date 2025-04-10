use std::io::Write;

use crate::compiler;

pub fn repl() {
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();
    let mut code = String::new();
    let mut last_was_empty = false;
    loop {
        print!("$ ");
        stdout.flush().unwrap();

        let len = stdin.read_line(&mut code).unwrap();
        if len == 1 {
            if last_was_empty {
                let result = compiler::compile(&code);
                code.clear();
                last_was_empty = false;
                continue;
            } else {
                code.pop();
                last_was_empty = true;
            }
        }
    }
}
