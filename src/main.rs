#![allow(clippy::vec_box)]
#![allow(suspicious_double_ref_op)]
#![allow(clippy::derive_ord_xor_partial_ord)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::single_match)]
#![allow(clippy::option_map_unit_fn)]

mod jar;
pub use jar::*;

mod ast;
mod db;
mod flow;
mod inference;
mod lsp;
mod nomparser;
mod utils;
mod version;
use std::{
    cell::RefCell,
    path::Path,
    sync::{Arc, Mutex},
};

use version::VergenInfo;

use ast::compiler::{self, convert, ActionType};
use clap::{Parser, Subcommand};
use db::Database;
use lsp::mem_docs::{self, MemDocsInput};

#[cfg(not(target_arch = "wasm32"))]
use lsp::start_lsp;

fn main() {
    #[cfg(target_arch = "wasm32")]
    unimplemented!("compiler on wasm32 is not supported yet");
    let mut cli = Cli::parse();
    cli.logger = stderrlog::new();
    cli.logger
        .module(module_path!())
        .quiet(cli.quiet)
        .verbosity(cli.verbose as usize);

    match cli.command {
        Some(ref cmd) => match cmd {
            RunCommand::Run { name } => cli.run(name.clone()),
            RunCommand::Lsp {} => cli.lsp(),
            RunCommand::Fmt { name } => cli.fmt(name.clone()),
            RunCommand::New { name } => cli.new_project(name.clone()),
            RunCommand::Version => cli.version(),
            RunCommand::Build { name } => cli.build(name.clone()),
        },
        // todo(griffin): refine it in the future, technically it should compile one file only instead of a whole project.
        None => {
            if cli.name.as_deref().is_none() {
                cli.version();
                return;
            }

            let name = cli.name.as_deref().unwrap();
            cli.build(name.to_string())
        }
    }
}

/// Pivot Lang compiler program
#[derive(Parser)]
#[command(author, version = get_version(), about=r#"
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
"#, long_about = None, styles = get_styles())]
struct Cli {
    /// Name of the source file, we will use it as an entry path to find and build a pl project with a kagari.toml
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

    #[clap(skip)]
    logger: stderrlog::StdErrLog,
}

impl Cli {
    // todo(griffin): make the input name more generic
    // currently it support file path input only,
    pub fn build(&mut self, name: String) {
        self.logger
            .timestamp(stderrlog::Timestamp::Off)
            .init()
            .unwrap();

        let db = Database::default();
        let filepath = Path::new(&name);
        let abs = crate::utils::canonicalize(filepath).unwrap();

        let op = compiler::Options {
            genir: self.genir,
            printast: self.printast,
            flow: self.flow,
            fmt: false,
            optimization: convert(self.optimization),
            jit: self.jit,
        };

        let action = if self.flow {
            ActionType::Flow
        } else if self.printast {
            ActionType::PrintAst
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
        compiler::compile(&db, mem, self.out.clone(), op);
    }
    pub fn version(&self) {
        let mut v = VergenInfo::new();
        v.build_semver = "alpha".to_string();
        println!("{}", v)
    }
    pub fn run(&self, name: String) {
        #[cfg(feature = "jit")]
        compiler::run(
            Path::new(name.as_str()),
            convert(self.optimization).to_llvm(),
        );

        #[cfg(not(feature = "jit"))]
        println!(
            "feature jit is not enabled, cannot use run command, failed to compile {}",
            name
        );
    }
    pub fn new_project(&self, name: String) {
        utils::plc_new::init_package(name);
    }
    pub fn lsp(&mut self) {
        self.logger
            .timestamp(stderrlog::Timestamp::Microsecond)
            .init()
            .unwrap();

        #[cfg(not(target_arch = "wasm32"))]
        start_lsp().unwrap();
    }
    /// fmt formats the input file with desired pivot-language format
    /// currently, it doesn't support to format a whole project.
    pub fn fmt(&mut self, name: String) {
        if name.is_empty() {
            println!("a file is required to pass as fmt command is not fully implemented yet");
            return;
        }

        self.logger
            .timestamp(stderrlog::Timestamp::Off)
            .init()
            .unwrap();

        let db = Database::default();
        let filepath = Path::new(&name);
        let abs = crate::utils::canonicalize(filepath).unwrap();

        let op = compiler::Options {
            genir: self.genir,
            printast: self.printast,
            flow: self.flow,
            fmt: true,
            optimization: convert(self.optimization),
            jit: self.jit,
        };

        let action = if self.flow {
            ActionType::Flow
        } else if self.printast {
            ActionType::PrintAst
        } else {
            ActionType::Fmt
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
        compiler::compile(&db, mem, self.out.clone(), op);
    }
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
    Fmt {
        #[clap(value_parser, default_value = "")]
        // the file name to be formatted
        // currently format all files under executing path is supported yet, so we don't allow it empty.
        name: String,
    },
    /// Make a new pl package at path
    New {
        #[clap(value_parser)]
        name: String,
    },
    /// Get the whole version infomation
    Version,

    /// Build the source code written in pivot language
    /// todo(griffin): inside this command, we should enquery the kagari.toml in all children files,
    /// instead of find it among files under its ancestor
    Build {
        #[arg(value_parser)]
        // Name of a file for a project entry
        name: String,
    },
}

pub fn get_styles() -> clap::builder::Styles {
    clap::builder::Styles::styled()
        .header(
            anstyle::Style::new()
                .bold()
                .underline()
                .fg_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::Green))),
        )
        .literal(
            anstyle::Style::new()
                .bold()
                .fg_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::BrightGreen))),
        )
}

pub fn get_version() -> clap::builder::Str {
    format!("alpha-{}", env!("VERGEN_GIT_SHA")).into()
}
