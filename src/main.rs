mod ast;
mod nomparser;
mod utils;
use std::fs::{read_to_string};
use std::path::Path;

use ast::compiler::Compiler;
use clap::{Parser, Subcommand};

/// Simple program to greet a person
#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
   /// Name of the source file
   #[clap(value_parser)]
   name: Option<String>,

   /// output file
   #[clap(short, long, value_parser, default_value = "out.plb")]
   out: String,
   #[clap(subcommand)]
    command: Option<RunCommand>,
}

#[derive(Subcommand)]
enum RunCommand {
    Run {
        /// Name of the compiled file
        #[clap(value_parser)]
        name: String,
    }
}

fn main() {
    let cli = Cli::parse();

    // You can check the value provided by positional arguments, or option arguments
    if let Some(name) = cli.name.as_deref() {
        let str = read_to_string(Path::new(name)).unwrap();
        let s = str.as_str();
        let mut c = Compiler::new(s);
        c.compile(cli.out.as_str());    
    } else if let Some(command) = cli.command {
        match command {
            RunCommand::Run { name } => {
                Compiler::run(Path::new(name.as_str()));  
            }
        }
    } else{
        println!("No file provided");
    }
}
