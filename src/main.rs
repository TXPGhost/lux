//! Compiler for the Lux programming language

#![warn(missing_docs)]

use std::{
    fs::{DirEntry, File},
    path::PathBuf,
};

use ast::{
    desugar::{lookup::LookupError, resolve::Resolve},
    Node,
};
use ast::{
    desugar::{Desugar, DesugarArena, DesugarError, Parent},
    parse_tree::Expr,
};
use lexer::{LexError, Lexer};
use parser::{ParseError, Parser};

use colored::Colorize;
use pretty_print::PrettyPrint;
use type_check::{AssignTypes, TypeArena, TypeError};

/// An arena data structure
pub mod arena;

/// The abstract syntax tree
pub mod ast;

/// Type checking module
pub mod type_check;

/// Tokenizes an input stream of text
pub mod lexer;

/// Produces an abstract syntax tree by parsing the lexer output
pub mod parser;

/// Pretty-prints an abstract syntax tree
pub mod pretty_print;

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

    /// A type checking error
    Type(TypeError),
}

struct TestResult {
    return_expr: Option<Node<Expr>>,
    parse_tree_str: String,
    desugared_str: String,
    arena_str: String,
    resolved_arena: DesugarArena,
}

fn test_file(path: PathBuf) -> Result<TestResult, TestError> {
    let mut file = File::open(path).map_err(TestError::Io)?;
    let lexer = Lexer::new_from_file(&mut file).map_err(TestError::Io)?;
    let tokens = lexer.tokenize().map_err(TestError::Lex)?;
    let mut parser = Parser::new(&tokens);
    let parse_tree = parser.parse().map_err(TestError::Parse)?;
    let parse_tree_str = format!("{}", parse_tree.val.printable(&()));
    let (mut arena, prelude) = DesugarArena::new_prelude();
    let ast = parse_tree
        .desugar(&mut arena, Some(prelude))
        .map_err(TestError::Desugar)?;
    ast.resolve(&mut arena, Parent::MemberList(ast))
        .map_err(TestError::Resolve)?;
    let desugared_str = format!("{}", ast.printable(&arena));
    let arena_str = format!("{}", arena.printable(&()));
    ast.resolve(&mut arena, prelude)
        .map_err(TestError::Resolve)?;
    let mut types = TypeArena::new_empty();
    ast.assign_types(&arena, &mut types)
        .map_err(TestError::Type)?;
    //let flattened = desugared.flatten();
    Ok(TestResult {
        return_expr: None,
        parse_tree_str,
        desugared_str,
        arena_str,
        resolved_arena: arena,
    })
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
        let run_all = args.is_empty();
        if !run_all {
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
        print!(
            "{} {} ",
            "TEST".cyan().bold(),
            format!("\"{}\"", path.display()).purple(),
        );

        // run the test and print results
        match test_file(path) {
            Ok(result) => {
                if !run_all {
                    if let Some(return_expr) = result.return_expr {
                        println!("\t==> {:?}", return_expr.val);
                    }
                }

                pass += 1;
                total += 1;
                println!("{}", "PASS".green());

                if !run_all {
                    println!(
                        "\n{}\n\n{}",
                        "Parse Tree".yellow(),
                        result.parse_tree_str.bright_black()
                    );
                    println!(
                        "\n{}\n\n{}",
                        "Desugared".yellow(),
                        result.desugared_str.bright_black()
                    );
                    println!(
                        "\n{}\n\n{}",
                        "Resolved Arena".yellow(),
                        result.arena_str.bright_black()
                    );
                }
            }
            Err(e) => {
                fail += 1;
                total += 1;
                match e {
                    TestError::Io(e) => println!("{}: {:?}", "FAIL_IO".red(), e),
                    TestError::Lex(e) => println!("{}: {:?}", "FAIL_LEX".red(), e),
                    TestError::Parse(e) => println!("{}: {:?}", "FAIL_PARSE".red(), e),
                    TestError::Desugar(e) => println!("{}: {:?}", "FAIL_DESUGAR".red(), e),
                    TestError::Resolve(e) => println!("{}: {:?}", "FAIL_RESOLVE".red(), e),
                    TestError::Type(e) => println!("{}: {:?}", "FAIL_TYPECHECK".red(), e),
                }
            }
        }
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
