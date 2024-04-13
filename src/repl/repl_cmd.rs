use std::path::PathBuf;

use clap::{Parser, Subcommand};

/// REPL Command Line Interface
#[derive(Parser)]
#[command(version, about, long_about = None, name = "@repl", bin_name = "@repl")]
pub struct REPLCli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    Load {
        /// loads a Pivot Lang project into repl
        proj_path: PathBuf,
        /// sets the project name
        #[arg(long = "as")]
        _as: String,
    },
    LoadDeps {
        /// loads the dependencies of a Pivot Lang project into repl
        proj_path: PathBuf,
    },
}

#[cfg(test)]
#[test]
fn test_repl_cli() {
    REPLCli::try_parse_from(&["@repl", "load", r#""test""#, "--as", "proj"]).unwrap();
    REPLCli::try_parse_from(&["@repl", "load-deps", r#""test""#]).unwrap();
}
