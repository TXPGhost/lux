#![allow(unused)]

use clap::{Parser, Subcommand};

pub mod compiler;
pub mod lexer;
pub mod parser;
pub mod repl;

#[derive(Subcommand)]
enum Commands {
    Repl,
    Run(Run),
}

#[derive(Parser)]
struct Run {
    file: String,
}

#[derive(Parser)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

fn main() {
    let args = Args::parse();
    match args.command {
        Commands::Repl => repl::repl(),
        Commands::Run(run) => todo!(),
    }
}
