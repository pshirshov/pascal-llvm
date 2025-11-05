/// Pascal-like language compiler built with Rust and Inkwell (LLVM)
mod ast;
mod lexer;
mod parser;

use clap::Parser as ClapParser;
use std::fs;
use std::path::PathBuf;

#[derive(ClapParser, Debug)]
#[command(name = "pascalc")]
#[command(author = "Your Name")]
#[command(version = "0.1.0")]
#[command(about = "Pascal-like language compiler built on LLVM", long_about = None)]
struct Args {
    /// Input Pascal source file
    input: PathBuf,

    /// Output LLVM IR file
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Print the AST and exit (don't generate code)
    #[arg(long)]
    print_ast: bool,

    /// Print tokens and exit (don't parse or generate code)
    #[arg(long)]
    print_tokens: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Read input file
    let input_contents = fs::read_to_string(&args.input)?;

    // Tokenize
    if args.print_tokens {
        let mut lexer = lexer::Lexer::new(&input_contents);
        let tokens = lexer.tokenize();
        for token in tokens {
            println!("{}", token);
        }
        return Ok(());
    }

    // Parse
    let mut parser = parser::Parser::new(&input_contents);
    let program = parser.parse_program().map_err(|e| {
        anyhow::anyhow!("Parse error: {}", e)
    })?;

    if args.print_ast {
        println!("{:#?}", program);
        return Ok(());
    }

    // TODO: Type checking
    // TODO: Code generation

    eprintln!("Type checking and code generation not yet implemented");
    std::process::exit(1);
}
