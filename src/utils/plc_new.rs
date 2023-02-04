use std::fs;
use std::path::PathBuf;

#[derive(Debug)]
pub struct NewOptions {
    /// Absolute path to the directory for the new package
    pub path: PathBuf,
    pub name: String,
}

impl NewOptions {
    pub fn new(path: PathBuf, name: String) -> NewOptions {
        NewOptions { path, name }
    }
}

pub fn init_package(name: String) {
    let mut opts = NewOptions::new(PathBuf::from(&name), name);

    let path = &opts.path;
    if path.exists() {
        println!("destination `{}` already exists\n", path.display());
        return;
    }

    mk(&mut opts);
}

fn mk(opts: &mut NewOptions) {
    let path = &opts.path;
    let name = &opts.name;

    fs::create_dir_all(&path).unwrap();

    fs::write(
        &path.join("Kagari.toml"),
        format!(
            r#"entry = "main.pi"
project = "{}"
"#,
            name,
        )
        .as_bytes(),
    )
    .unwrap();

    fs::write(
        &path.join("main.pi"),
        format!(
            r#"use std::io;
fn main() i64 {{
    io::printi64ln(666);
    return 0;
}}
"#
        )
        .as_bytes(),
    )
    .unwrap();
}

#[cfg(test)]
pub mod tests {
    pub static TEST_COMPILE_MUTEX: std::sync::Mutex<()> = std::sync::Mutex::new(());
    use super::*;
    use crate::ast::compiler::{self, ActionType, HashOptimizationLevel};
    use crate::db::Database;
    use crate::lsp::mem_docs::{self, MemDocsInput};
    use std::{
        cell::RefCell,
        sync::{Arc, Mutex},
    };

    #[test]
    fn test_init_package() {
        let _l = TEST_COMPILE_MUTEX.lock().unwrap();
        let package_name = format!("plc_new_testfile");
        // test init_package
        init_package(package_name.clone());
        assert!(fs::metadata("plc_new_testfile").is_ok());

        // test package_compile
        let db = Database::default();
        let op = compiler::Options {
            genir: false,
            printast: false,
            flow: false,
            fmt: false,
            optimization: HashOptimizationLevel::Aggressive,
        };

        let input = MemDocsInput::new(
            &db,
            Arc::new(Mutex::new(RefCell::new(mem_docs::MemDocs::new()))),
            "plc_new_testfile/main.pi".to_string(),
            Default::default(),
            ActionType::Compile,
            None,
            None,
        );
        compiler::compile(&db, input, format!("plc_new_testout"), op);
        #[cfg(target_os = "linux")]
        assert!(fs::metadata("plc_new_testout").is_ok());

        #[cfg(target_os = "macos")]
        assert!(fs::metadata("plc_new_testout").is_ok());

        #[cfg(target_os = "windows")]
        assert!(fs::metadata("plc_new_testout.exe").is_ok());

        assert!(fs::metadata("plc_new_testout.bc").is_ok());
    }
}
