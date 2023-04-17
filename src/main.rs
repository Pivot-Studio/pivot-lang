#![allow(clippy::vec_box)]
#![allow(clippy::clone_double_ref)]
#![allow(clippy::derive_ord_xor_partial_ord)]
#![allow(clippy::missing_safety_doc)]
#[salsa::jar(db = Db)]
pub struct Jar(
    nomparser::SourceProgram,
    nomparser::parse,
    mem_docs::MemDocsInput,
    mem_docs::MemDocsInput_get_current_file_content,
    mem_docs::MemDocsInput_get_file_content,
    mem_docs::MemDocsInput_get_file_params,
    mem_docs::EmitParams,
    mem_docs::FileCompileInput,
    mem_docs::FileCompileInput_get_file_content,
    mem_docs::FileCompileInput_get_emit_params,
    compiler::compile,
    compiler::compile_dry,
    compiler::compile_dry_file,
    accumulators::Diagnostics,
    accumulators::PLReferences,
    accumulators::GotoDef,
    accumulators::Completions,
    accumulators::PLSemanticTokens,
    accumulators::PLHover,
    accumulators::ModBuffer,
    accumulators::PLFormat,
    accumulators::Hints,
    accumulators::DocSymbols,
    accumulators::PLSignatureHelp,
    program::Program,
    program::Program_emit,
    program::ProgramNodeWrapper,
    program::ModWrapper,
    program::ProgramEmitParam,
    program::emit_file,
    program::LspParams,
    utils::read_config::get_config,
);

pub trait Db: salsa::DbWithJar<Jar> {
    fn set_ref_str(&self, ref_str: Option<String>);
    fn get_ref_str(&self) -> Option<String>;
}

mod ast;
mod db;
mod flow;
mod lsp;
mod nomparser;
mod utils;
use std::{
    cell::RefCell,
    path::Path,
    sync::{Arc, Mutex},
};

use ast::{
    accumulators,
    compiler::{self, ActionType, HashOptimizationLevel},
    node::program,
};
use clap::{CommandFactory, FromArgMatches, Parser, Subcommand};
use db::Database;
use lsp::{
    mem_docs::{self, MemDocsInput},
    start_lsp,
};

/// Pivot Lang compiler program
#[derive(Parser)]
#[command(author, version, about=r#"
_______   __                        __            __                                    
|       \ |  \                      |  \          |  \                                   
| $$$$$$$\ \$$ __     __   ______  _| $$_         | $$       ______   _______    ______  
| $$__/ $$|  \|  \   /  \ /      \|   $$ \        | $$      |      \ |       \  /      \ 
| $$    $$| $$ \$$\ /  $$|  $$$$$$\\$$$$$$        | $$       \$$$$$$\| $$$$$$$\|  $$$$$$\
| $$$$$$$ | $$  \$$\  $$ | $$  | $$ | $$ __       | $$      /      $$| $$  | $$| $$  | $$
| $$      | $$   \$$ $$  | $$__/ $$ | $$|  \      | $$_____|  $$$$$$$| $$  | $$| $$__| $$
| $$      | $$    \$$$    \$$    $$  \$$  $$      | $$     \\$$    $$| $$  | $$ \$$    $$
 \$$       \$$     \$      \$$$$$$    \$$$$        \$$$$$$$$ \$$$$$$$ \$$   \$$ _\$$$$$$$
                                                                               |  \__| $$
                                                                                \$$    $$
                                                                                 \$$$$$$ 
"#, long_about = None)]
struct Cli {
    /// Name of the source file
    #[arg(value_parser)]
    name: Option<String>,

    /// output file
    #[arg(short, long, value_parser, default_value = "out")]
    out: String,

    /// verbose level
    /// - default: error and warning
    /// - v: error, warning and info
    /// - vv: error, warning, info and debug
    /// - vvv: error, warning, info, debug and trace
    #[arg(
        short,
        long,
        default_value = "1",
        help = r"verbose level
- 0: only error
- 1(default): error and warning
- 2: error, warning and info
- 3: error, warning, info and debug
- 4: error, warning, info, debug and trace"
    )]
    verbose: u8,

    /// quiet mode
    #[arg(long, default_value = "false")]
    quiet: bool,

    /// print ast
    #[arg(long)]
    printast: bool,

    /// generate flow chart
    #[arg(long)]
    flow: bool,

    /// generate ir
    #[arg(long)]
    genir: bool,

    /// jit compile
    #[arg(long)]
    jit: bool,

    /// optimization level, 0-3
    #[arg(short = 'O', value_parser, default_value = "0")]
    optimization: u64,

    /// print source fmt
    #[command(subcommand)]
    command: Option<RunCommand>,
}

#[derive(Subcommand)]
enum RunCommand {
    /// JIT run the compiled program
    Run {
        /// Name of the compiled file
        #[arg(value_parser)]
        name: String,
    },
    /// Start the language server
    Lsp,
    /// Format current project
    Fmt,
    /// Make a new pl package at path
    New {
        #[clap(value_parser)]
        name: String,
    },
}

fn main() {
    let mut matches = Cli::command()
        .color(clap::ColorChoice::Always)
        .get_matches();
    let res = Cli::from_arg_matches_mut(&mut matches);
    let cli = match res {
        Ok(s) => s,
        Err(e) => {
            e.format(&mut Cli::command()).exit();
        }
    };
    let opt = match cli.optimization {
        0 => HashOptimizationLevel::None,
        1 => HashOptimizationLevel::Less,
        2 => HashOptimizationLevel::Default,
        3 => HashOptimizationLevel::Aggressive,
        _ => panic!("optimization level must be 0-3"),
    };

    let mut logger = stderrlog::new();
    logger
        .module(module_path!())
        .quiet(cli.quiet)
        .verbosity(cli.verbose as usize);

    let fmt = matches!(cli.command, Some(RunCommand::Fmt));
    // You can check the value provided by positional arguments, or option arguments
    if let Some(name) = cli.name.as_deref() {
        logger.timestamp(stderrlog::Timestamp::Off).init().unwrap();
        let db = Database::default();
        let filepath = Path::new(name);
        let abs = dunce::canonicalize(filepath).unwrap();
        let op = compiler::Options {
            genir: cli.genir,
            printast: cli.printast,
            flow: cli.flow,
            fmt,
            optimization: opt,
            jit: cli.jit,
        };
        let action = if cli.flow {
            ActionType::Flow
        } else if cli.printast {
            ActionType::PrintAst
        } else if fmt {
            ActionType::Fmt
        } else {
            ActionType::Compile
        };
        let mem = MemDocsInput::new(
            &db,
            Arc::new(Mutex::new(RefCell::new(mem_docs::MemDocs::default()))),
            abs.to_str().unwrap().to_string(),
            op,
            action,
            None,
            None,
        );
        compiler::compile(&db, mem, cli.out.clone(), op);
    } else if let Some(command) = cli.command {
        match command {
            RunCommand::Run { name } => {
                #[cfg(feature = "jit")]
                compiler::run(Path::new(name.as_str()), opt.to_llvm());
                #[cfg(not(feature = "jit"))]
                println!(
                    "feature jit is not enabled, cannot use run command, failed to compile {}",
                    name
                );
            }
            RunCommand::Lsp {} => {
                logger
                    .timestamp(stderrlog::Timestamp::Microsecond)
                    .init()
                    .unwrap();
                start_lsp().unwrap();
            }
            RunCommand::Fmt {} => {
                println!("fmt command is not implemented yet");
            }
            RunCommand::New { name } => {
                utils::plc_new::init_package(name);
            }
        }
    } else {
        println!("No file provided");
        Cli::command()
            .color(clap::ColorChoice::Always)
            .print_help()
            .unwrap();
    }
}
