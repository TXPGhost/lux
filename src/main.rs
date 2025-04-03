//! Compiler for the Lux programming language

#![warn(missing_docs)]

use std::{
    fs::{DirEntry, File},
    path::PathBuf,
};

use ast::{
    desugar::{Desugar, DesugarArena, DesugarError, LookupError, Parent, Resolve},
    parse_tree::{Expr, Ident, Member},
};
use ast::{Node, NodeExt};
use interpreter::{Context, Interpret, InterpretError, InterpretStrategy};
use lexer::{LexError, Lexer};
use parser::{ParseError, Parser};

use colored::Colorize;

/// An arena data structure
pub mod arena;

/// The abstract syntax tree
pub mod ast;

/// Evaluates programs using the interpreter
pub mod interpreter;

/// Tokenizes an input stream of text
pub mod lexer;

/// Produces an abstract syntax tree by parsing the lexer output
pub mod parser;

/// Pretty-prints an abstract syntax tree
pub mod pretty_print;

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

    /// A desugaring error
    Desugar(DesugarError),

    /// An identifier resolution error
    Resolve(LookupError),

    /// An interpreter error at the simplify stage
    Simplify(InterpretError),

    /// An interpreter error at the evaluation stage of the `main` function
    Eval(InterpretError),
}

fn test_file(path: PathBuf) -> Result<Option<Node<Expr>>, TestError> {
    let mut file = File::open(path).map_err(TestError::Io)?;
    let lexer = Lexer::new_from_file(&mut file).map_err(TestError::Io)?;
    let tokens = lexer.tokenize().map_err(TestError::Lex)?;
    let mut parser = Parser::new(&tokens);
    let parse_tree = parser.parse().map_err(TestError::Parse)?;
    let (mut arena, prelude) = DesugarArena::new_prelude();
    let desugared = parse_tree
        .desugar(&mut arena, Some(prelude))
        .map_err(TestError::Desugar)?;
    desugared
        .resolve(&mut arena, Parent::MemberList(desugared))
        .map_err(TestError::Resolve)?;
    //let mut context = Context::default();
    //let members = parse_tree
    //    .interp(&mut context)
    //    .map_err(TestError::Simplify)?;
    //for member in members.val {
    //    if let Member::Named(ident, _) = member.val {
    //        let main = Ident::VIdent("main".into());
    //        if ident.val == main {
    //            let mut context = context.frame(InterpretStrategy::Eval);
    //            let main_call = Expr::Call(
    //                Box::new(Expr::Ident(main.unloc()).unloc()),
    //                Vec::new().unloc(),
    //            )
    //            .unloc();
    //            let result = main_call.interp(&mut context).map_err(TestError::Eval)?;
    //            return Ok(Some(result));
    //        }
    //    }
    //}
    Ok(None)
}

fn main() {
    println!("\n{}\n", "RUNNING TEST SUITE...".blue().bold());

    let mut args: Vec<String> = std::env::args().collect();
    args.remove(0);

    let paths = std::fs::read_dir("tests").unwrap();
    let (mut pass, mut fail, mut total) = (0, 0, 0);
    let mut paths: Vec<DirEntry> = paths.into_iter().map(|path| path.unwrap()).collect();
    paths.sort_by_key(|a| a.path());
    for path in paths {
        let path = path.path();

        // filter tests if arguments passed
        let filename = path.file_name().unwrap();
        let mut skip = false;
        if !args.is_empty() {
            skip = true;
            for arg in &args {
                if (arg.to_owned() + ".lx") == filename.to_str().unwrap() {
                    skip = false;
                }
            }
        }
        if skip {
            continue;
        };

        // print out test name nicely
        println!(
            "{} {}",
            "TEST".cyan().bold(),
            format!("\"{}\"", path.display()).purple()
        );

        // run the test and print results
        match test_file(path) {
            Ok(result) => {
                if let Some(result) = result {
                    println!("\t==> {:?}", result.val);
                }

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
                    TestError::Desugar(e) => println!("\t{:<15}: {:?}", "FAIL_DESUGAR".red(), e),
                    TestError::Resolve(e) => println!("\t{:<15}: {:?}", "FAIL_RESOLVE".red(), e),
                    TestError::Simplify(e) => {
                        println!("\t{:<15}: {:?}", "FAIL_SIMPLIFY".red(), e)
                    }
                    TestError::Eval(e) => {
                        println!("\t{:<15}: {:?}", "FAIL_EVAL".red(), e)
                    }
                }
            }
        }

        println!();
    }

    // summarize results at the end
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
