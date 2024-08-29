#[cfg(not(feature = "unittest"))]
use std::env;
#[cfg(not(feature = "unittest"))]
use std::fs::DirEntry;
#[cfg(not(feature = "unittest"))]
use std::path::{Path, PathBuf};
#[cfg(not(feature = "unittest"))]
use std::process::Command;

// static LIBUV_VERSION: &str = "1.48.0";

#[cfg(not(feature = "unittest"))]
fn run<F>(name: &str, mut configure: F)
where
    F: FnMut(&mut Command) -> &mut Command,
{
    let mut command = Command::new(name);
    let configured = configure(&mut command);
    if !configured.status().is_ok() {
        let err = configured.status().unwrap_err();
        panic!("failed to execute {:?}: {}", configured, err);
    }
}

#[cfg(not(feature = "unittest"))]
const LIBUV_REPO: &str = "https://github.com/libuv/libuv.git";
#[cfg(not(feature = "unittest"))]
const LIBUV_DIR: &str = "libuv";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    // hack from https://github.com/rust-lang/cargo/issues/4789#issuecomment-2308131243
    #[cfg(not(feature = "unittest"))]
    {
        let out_dir = env::var("OUT_DIR").unwrap();
        let mut uv_src = PathBuf::from(&out_dir);
        uv_src.push(LIBUV_DIR);
        if !uv_src.exists() {
            run("git", |cmd| cmd.arg("clone").arg(LIBUV_REPO).arg(&uv_src));
        }

        let dst = cmake::Config::new(&uv_src).build();
        let target_dir = std::path::Path::new(&out_dir)
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .parent()
            .unwrap();
        // ensure path exists
        std::fs::create_dir_all(target_dir).unwrap();
        let lib_path = dst.join("lib");
        // bin may not exist
        let bin_path = dst.join("bin");
        std::fs::create_dir_all(&bin_path).unwrap();

        // cp all file name contains `uv` from out_dir to target_dir
        let dirs = std::fs::read_dir(&lib_path)
            .unwrap()
            .chain(std::fs::read_dir(&bin_path).unwrap());
        // order it by size and remain only the largest static lib
        let mut dirs_filtered = vec![];
        for dir in dirs {
            if dir.is_err() {
                continue;
            }
            let dir = dir.unwrap();
            if dir.path().is_dir() {
                continue;
            }
            dirs_filtered.push(dir);
        }
        dirs_filtered.sort_by(|a, b| {
            let a = a.path();
            let b = b.path();
            a.metadata()
                .unwrap()
                .len()
                .cmp(&b.metadata().unwrap().len())
        });
        // only take largest static lib and dylib
        // only take largest static lib and dylib
        let mut largest_static_lib: Option<&DirEntry> = None;
        let mut largest_dylib: Option<&DirEntry> = None;

        for dir in &dirs_filtered {
            let path = dir.path();
            if let Some(extension) = path.extension() {
                if extension == "a" || extension == "lib" {
                    largest_static_lib = Some(dir);
                } else if extension == "so" || extension == "dylib" || extension == "dll" {
                    largest_dylib = Some(dir);
                }
            }
        }

        if let Some(static_lib) = largest_static_lib {
            cp_to_out_dir(static_lib, target_dir);
        }

        if let Some(dylib) = largest_dylib {
            cp_to_out_dir(dylib, target_dir);
        }

        // add lib_path to library search path
        println!("cargo:rustc-link-search=native={}", target_dir.display());
        // link libuv
        #[cfg(target_os = "windows")]
        println!("cargo:rustc-link-lib=static=libuv");
        #[cfg(not(target_os = "windows"))]
        println!("cargo:rustc-link-lib=static=uv");
    }
}

#[cfg(not(feature = "unittest"))]
fn cp_to_out_dir(dir: &DirEntry, target_dir: &Path) {
    let path = dir.path();
    if path.file_name().unwrap().to_str().unwrap().contains("uv") {
        std::fs::copy(&path, target_dir.join(&path.file_name().unwrap())).unwrap_or_else(|e| {
            println!("copy error: {}", e);
            panic!("from {} to {}", path.display(), target_dir.display());
        });
    }
}
