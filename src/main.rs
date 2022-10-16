#[salsa::jar(db = Db)]
pub struct Jar(
    nomparser::SourceProgram,
    nomparser::parse,
    mem_docs::MemDocsInput,
    mem_docs::MemDocsInput_get_file_content,
    mem_docs::MemDocsInput_get_emit_params,
    mem_docs::EmitParams,
    compiler::compile,
    compiler::compile_dry,
    accumulators::Diagnostics,
    accumulators::PLReferences,
    accumulators::GotoDef,
    accumulators::Completions,
    accumulators::PLSemanticTokens,
    accumulators::PLHover,
    program::Program,
    program::Program_emit,
    program::ProgramNodeWrapper,
);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

mod ast;
mod db;
mod lsp;
mod nomparser;
mod utils;
use std::{cell::RefCell, path::Path, sync::Arc};

use ast::{
    accumulators,
    compiler::{self, ActionType, HashOptimizationLevel},
    node::program,
};
use clap::{CommandFactory, Parser, Subcommand};
use db::Database;
use lsp::{
    mem_docs::{self, MemDocsInput},
    start_lsp,
};

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
        0 => HashOptimizationLevel::None,
        1 => HashOptimizationLevel::Less,
        2 => HashOptimizationLevel::Default,
        3 => HashOptimizationLevel::Aggressive,
        _ => panic!("optimization level must be 0-3"),
    };

    // You can check the value provided by positional arguments, or option arguments
    if let Some(name) = cli.name.as_deref() {
        let db = Database::default();
        let filepath = Path::new(name);
        let abs = dunce::canonicalize(filepath).unwrap();
        let op = compiler::Options {
            verbose: cli.verbose,
            genir: cli.genir,
            printast: cli.printast,
            optimization: opt,
        };
        let mem = MemDocsInput::new(
            &db,
            Arc::new(RefCell::new(mem_docs::MemDocs::new())),
            abs.to_str().unwrap().to_string(),
            op,
            ActionType::Diagnostic,
            None,
        );
        compiler::compile(&db, mem, cli.out.clone(), op);
    } else if let Some(command) = cli.command {
        match command {
            RunCommand::Run { name } => {
                #[cfg(feature = "jit")]
                compiler::run(Path::new(name.as_str()), opt.to_llvm());
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
