//! Compiler for the Lux programming language

#![warn(missing_docs)]
#![allow(unused)]

use std::{
    fs::{DirEntry, File},
    path::PathBuf,
};

use ast::{
    desugar::{lookup::LookupError, resolve::Resolve},
    parse_tree, Node,
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

#[derive(Default)]
struct TestResult {
    return_expr: Option<Node<Expr>>,
    parse_tree_str: Option<String>,
    desugared_str: Option<String>,
    arena_str: Option<String>,
    resolved_arena: Option<DesugarArena>,
    err: Option<TestError>,
}

fn test_file(path: PathBuf) -> TestResult {
    let mut result = TestResult::default();

    let file = File::open(path);
    let mut file = match file {
        Ok(file) => file,
        Err(err) => {
            result.err = Some(TestError::Io(err));
            return result;
        }
    };

    let lexer = Lexer::new_from_file(&mut file);
    let lexer = match lexer {
        Ok(lexer) => lexer,
        Err(err) => {
            result.err = Some(TestError::Io(err));
            return result;
        }
    };

    let tokens = lexer.tokenize();
    let tokens = match tokens {
        Ok(tokens) => tokens,
        Err(err) => {
            result.err = Some(TestError::Lex(err));
            return result;
        }
    };

    let mut parser = Parser::new(&tokens);
    let parse_tree = parser.parse();
    let parse_tree = match parse_tree {
        Ok(parse_tree) => parse_tree,
        Err(err) => {
            result.err = Some(TestError::Parse(err));
            return result;
        }
    };

    result.parse_tree_str = Some(format!("{}", parse_tree.val.printable(&())));
    let (mut arena, prelude) = DesugarArena::new_prelude();
    let ast = parse_tree.desugar(&mut arena, Some(prelude));
    let ast = match ast {
        Ok(ast) => ast,
        Err(err) => {
            result.err = Some(TestError::Desugar(err));
            return result;
        }
    };

    result.desugared_str = Some(format!("{}", ast.printable(&arena)));
    result.arena_str = Some(format!("{}", arena.printable(&())));

    match ast.resolve(&mut arena, Parent::MemberList(ast)) {
        Ok(()) => (),
        Err(err) => {
            result.err = Some(TestError::Resolve(err));
            return result;
        }
    };

    result.resolved_arena = Some(arena.clone());

    let mut types = TypeArena::new();
    match ast.assign_types(&arena, &mut types) {
        Ok(()) => (),
        Err(err) => {
            result.err = Some(TestError::Type(err));
            return result;
        }
    };

    result
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
        let result = test_file(path);

        if !run_all {
            if let Some(return_expr) = result.return_expr {
                println!("\t==> {:?}", return_expr.val);
            }
        }

        if !run_all {
            println!();
            if let Some(parse_tree_str) = result.parse_tree_str {
                println!(
                    "\n{}\n\n{}",
                    "Parse Tree".yellow(),
                    parse_tree_str.bright_black()
                );
            }
            if let Some(desugared_str) = result.desugared_str {
                println!(
                    "\n{}\n\n{}",
                    "Desugared".yellow(),
                    desugared_str.bright_black()
                );
            }
            if let Some(arena_str) = result.arena_str {
                println!(
                    "\n{}\n\n{}",
                    "Resolved Arena".yellow(),
                    arena_str.bright_black()
                );
            }
        }

        match result.err {
            None => {
                pass += 1;
                total += 1;
                println!("{}", "PASS".green());
            }
            Some(err) => {
                fail += 1;
                total += 1;
                match err {
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

    if total == 0 {
        println!("{}", "NO TESTS RUN".red());
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
