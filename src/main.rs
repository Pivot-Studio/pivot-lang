mod ast;
mod lsp;
mod nomparser;
mod utils;
use std::path::Path;

use ast::compiler::{self, Compiler};
use clap::{CommandFactory, Parser, Subcommand};
use inkwell::OptimizationLevel;
use lsp::start_lsp;

/// Pivot Lang compiler program
#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    /// Name of the source file
    #[clap(value_parser)]
    name: Option<String>,

    /// output file
    #[clap(long, value_parser, default_value = "out.plb")]
    out: String,

    /// verbose
    #[clap(short, long)]
    verbose: bool,

    /// print ast
    #[clap(long)]
    printast: bool,

    /// generate ir
    #[clap(long)]
    genir: bool,

    /// optimization level, 0-3
    #[clap(short, value_parser, default_value = "0")]
    optimization: u64,

    #[clap(subcommand)]
    command: Option<RunCommand>,
}

#[derive(Subcommand)]
enum RunCommand {
    /// JIT run the compiled program
    Run {
        /// Name of the compiled file
        #[clap(value_parser)]
        name: String,
    },
    /// Start the language server
    Lsp {},
}

fn main() {
    let cli = Cli::parse();
    let opt = match cli.optimization {
        0 => OptimizationLevel::None,
        1 => OptimizationLevel::Less,
        2 => OptimizationLevel::Default,
        3 => OptimizationLevel::Aggressive,
        _ => panic!("optimization level must be 0-3"),
    };

    // You can check the value provided by positional arguments, or option arguments
    if let Some(name) = cli.name.as_deref() {
        let c = Compiler::new();
        c.compile(
            name,
            lsp::mem_docs::MemDocs::new(),
            &cli.out,
            compiler::Options {
                verbose: cli.verbose,
                genir: cli.genir,
                printast: cli.printast,
                optimization: opt,
            },
        );
    } else if let Some(command) = cli.command {
        match command {
            RunCommand::Run { name } => {
                #[cfg(feature = "jit")]
                Compiler::run(Path::new(name.as_str()), opt);
                #[cfg(not(feature = "jit"))]
                println!("feature jit is not enabled, cannot use run command");
            }
            RunCommand::Lsp {} => {
                start_lsp().unwrap();
            }
        }
    } else {
        println!("No file provided");
        Cli::into_app().print_help().unwrap();
    }
}
