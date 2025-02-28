use core::fmt;
use mun_target::spec::{self, LinkerFlavor};
use std::env;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use thiserror::Error;

use crate::apple::get_apple_sdk_root;

#[derive(Error, Debug)]
pub enum LinkerError {
    /// Error emitted by the linker
    LinkError(String),

    /// Error in path conversion
    PathError(PathBuf),

    /// Could not locate platform SDK
    PlatformSdkMissing(String),
}

impl fmt::Display for LinkerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            LinkerError::LinkError(e) => write!(f, "{}", e),
            LinkerError::PathError(path) => write!(
                f,
                "path contains invalid UTF-8 characters: {}",
                path.display()
            ),
            LinkerError::PlatformSdkMissing(err) => {
                write!(f, "could not find platform sdk: {}", err)
            }
        }
    }
}

pub fn create_with_target(target: &spec::Target) -> Box<dyn Linker> {
    match target.options.linker_flavor {
        LinkerFlavor::Ld => Box::new(LdLinker::new(target)),
        LinkerFlavor::Ld64 => Box::new(Ld64Linker::new(target)),
        LinkerFlavor::Msvc => Box::new(MsvcLinker::new(target)),
    }
}

pub trait Linker {
    fn add_object(&mut self, path: &Path) -> Result<(), LinkerError>;
    fn push_args(&mut self, arg: &str);
    fn finalize(&mut self) -> Result<(), LinkerError>;
}

impl dyn Linker {
    pub fn output_to(&mut self, path: &str) {
        #[cfg(target_os = "windows")]
        self.push_args(&format!("/OUT:{}", path));
        #[cfg(not(target_os = "windows"))]
        {
            self.push_args("-o");
            self.push_args(path);
        }
    }
}

struct LdLinker {
    args: Vec<String>,
}

impl LdLinker {
    fn new(target: &spec::Target) -> Self {
        let mut args = vec![];
        // this is lld flag, not ld flag.
        // #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        // {
        //     // https://github.com/flamegraph-rs/flamegraph
        //     // https://crbug.com/919499#c16
        //     args.push("--no-rosegment".to_owned());
        // }
        args.extend(target.options.pre_link_args.iter().map(|x| x.to_string()));
        LdLinker { args }
    }
}

fn get_linux_lib_paths() -> Vec<String> {
    let mut paths = vec![];
    if let Ok(libpath) = env::var("LD_LIBRARY_PATH") {
        paths.extend(libpath.split(':').map(|s| s.to_string()));
    }
    [
        "/lib",
        "/usr/lib",
        "/lib64",
        "/usr/lib64",
        "/usr/lib/x86_64-linux-gnu",
    ]
    .iter()
    .for_each(|path| {
        paths.push(path.to_string());
    });
    paths
}

// fn get_libgcc_path() -> Result<String, String> {
//     let base_path = "/usr/lib/gcc/x86_64-linux-gnu";
//     let entries = std::fs::read_dir(base_path).map_err(|e| format!("Failed to read directory {}: {}", base_path, e))?;

//     for entry in entries {
//         let entry = entry.map_err(|e| format!("Failed to read directory entry: {}", e))?;
//         if entry.path().is_dir() {
//             let version = entry.file_name().into_string().map_err(|e| format!("Failed to convert OsString to String: {:?}", e))?;
//             return Ok(format!("{}/{}", base_path, version));
//         }
//     }

//     Err("No valid gcc version directory found".to_string())
// }

impl Linker for LdLinker {
    fn add_object(&mut self, path: &Path) -> Result<(), LinkerError> {
        let path_str = path
            .to_str()
            .ok_or_else(|| LinkerError::PathError(path.to_owned()))?
            .to_owned();
        self.args.push(path_str);
        Ok(())
    }
    fn finalize(&mut self) -> Result<(), LinkerError> {
        // libpath
        let paths = get_linux_lib_paths();
        paths.iter().for_each(|lib| {
            self.push_args(&format!("-L{}", lib.as_str()));
        });
        // // Add the path to the libgcc library
        // match get_libgcc_path() {
        //     Ok(libgcc_dir) => {
        //         self.push_args(&format!("-L{}", libgcc_dir));
        //     }
        //     Err(e) => {
        //         return Err(LinkerError::LinkError(format!(
        //             "Failed to get libgcc path: {}",
        //             e
        //         )));
        //     }
        // }
        // libs and link args
        [
            "-no-pie",
            "-zrelro",
            "--hash-style=gnu",
            "--build-id",
            "--eh-frame-hdr",
            "-melf_x86_64",
            "-dynamic-linker=/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2",
            "/lib/x86_64-linux-gnu/Scrt1.o",
            "/lib/x86_64-linux-gnu/crti.o",
            // "/usr/lib/gcc/x86_64-linux-gnu/<version>/crtbeginS.o",
            "-lc",
            "-lm",
            "-lpthread",
            "-lunwind",
            "--no-as-needed",
            "-ldl",
            // "-lgcc",
            // "/usr/lib/gcc/x86_64-linux-gnu/<version>/crtendS.o",
            "/lib/x86_64-linux-gnu/crtn.o",
        ]
        .iter()
        .for_each(|arg| {
            self.push_args(arg);
        });
        // lld_rs::link(lld_rs::LldFlavor::Elf, &self.args)
        //     .ok()
        //     .map_err(LinkerError::LinkError)

        // lld is so buggy that we have to use ld.
        // lld linked exeutable doesn't seems to respect `llvm.global_ctors` while ld works fine.
        let re = Command::new("ld").args(&self.args).output();
        if let Ok(re) = re {
            if !re.status.success() {
                eprintln!(
                    "link failed\nargs: {:?}\nld stdout: {}, stderr: {}",
                    self.args,
                    String::from_utf8_lossy(&re.stdout),
                    String::from_utf8_lossy(&re.stderr)
                );
                Err(LinkerError::LinkError("link failed".to_string()))
            } else {
                Ok(())
            }
        } else {
            Err(LinkerError::LinkError(format!(
                "link failed: {:?}",
                re.err()
            )))
        }
    }

    fn push_args(&mut self, arg: &str) {
        self.args.push(arg.to_owned());
    }
}

struct Ld64Linker {
    args: Vec<String>,
    target: spec::Target,
}

impl Ld64Linker {
    fn new(target: &spec::Target) -> Self {
        let args: Vec<String> = target
            .options
            .pre_link_args
            .iter()
            .map(|x| x.to_string())
            .collect();
        // let (a, b, c) = target.options.min_os_version.unwrap();
        // args.push(format!("-platform_version"));
        // args.push(format!("{}", target.options.os));
        // args.push(format!("{}.{}.{}", a, b, c));
        // args.push(format!("{}.{}.{}", a, b + 1, c));

        Ld64Linker {
            args,
            target: target.clone(),
        }
    }

    fn add_apple_sdk(&mut self) -> Result<(), LinkerError> {
        let arch = &self.target.arch;
        let os = &self.target.options.os;
        let llvm_target = &self.target.llvm_target;

        let sdk_name = match (arch.as_ref(), os.as_ref()) {
            ("aarch64", "tvos") => "appletvos",
            ("x86_64", "tvos") => "appletvsimulator",
            ("arm", "ios") => "iphoneos",
            ("aarch64", "ios") if llvm_target.contains("macabi") => "macosx",
            ("aarch64", "ios") if llvm_target.ends_with("-simulator") => "iphonesimulator",
            ("aarch64", "ios") => "iphoneos",
            ("x86", "ios") => "iphonesimulator",
            ("x86_64", "ios") if llvm_target.contains("macabi") => "macosx",
            ("x86_64", "ios") => "iphonesimulator",
            ("x86_64", "watchos") => "watchsimulator",
            ("arm64_32", "watchos") => "watchos",
            ("aarch64", "watchos") if llvm_target.ends_with("-simulator") => "watchsimulator",
            ("aarch64", "watchos") => "watchos",
            ("arm", "watchos") => "watchos",
            (_, "macos") => "macosx",
            _ => {
                return Err(LinkerError::PlatformSdkMissing(format!(
                    "unsupported arch `{}` for os `{}`",
                    arch, os
                )));
            }
        };

        let sdk_root = get_apple_sdk_root(sdk_name).map_err(LinkerError::PlatformSdkMissing)?;
        self.args.push(String::from("-syslibroot"));
        self.args.push(format!("{}", sdk_root.display()));
        Ok(())
    }
}

impl Linker for Ld64Linker {
    fn add_object(&mut self, path: &Path) -> Result<(), LinkerError> {
        let path_str = path
            .to_str()
            .ok_or_else(|| LinkerError::PathError(path.to_owned()))?
            .to_owned();
        self.args.push(path_str);
        Ok(())
    }

    fn finalize(&mut self) -> Result<(), LinkerError> {
        self.add_apple_sdk()?;
        self.args.push("-lSystem".to_owned());
        self.args.insert(0, "ld64.lld".to_owned());
        lld_rs::link(lld_rs::LldFlavor::MachO, &self.args)
            .ok()
            .map_err(LinkerError::LinkError)
    }

    fn push_args(&mut self, arg: &str) {
        self.args.push(arg.to_owned());
    }
}

struct MsvcLinker {
    args: Vec<String>,
}

impl MsvcLinker {
    fn new(target: &spec::Target) -> Self {
        MsvcLinker {
            args: target
                .options
                .pre_link_args
                .iter()
                .map(|x| x.to_string())
                .collect(),
        }
    }
}

impl Linker for MsvcLinker {
    fn add_object(&mut self, path: &Path) -> Result<(), LinkerError> {
        let path_str = path
            .to_str()
            .ok_or_else(|| LinkerError::PathError(path.to_owned()))?
            .to_owned();
        self.args.push(path_str);
        Ok(())
    }

    fn finalize(&mut self) -> Result<(), LinkerError> {
        // self.push_args("-defaultlib:libucrt");
        self.push_args("-defaultlib:libcmt");
        self.push_args("-defaultlib:oldnames");
        if let Some(libs) = env::var_os("LIB") {
            for lib in env::split_paths(&libs) {
                self.push_args(&format!("-libpath:{}", lib.to_str().unwrap()));
            }
        }
        let libs = get_win_sdk_lib_paths();
        libs.iter().for_each(|p| {
            self.push_args(&format!(
                "-libpath:{}",
                p.to_str()
                    .unwrap()
                    .trim_end_matches('\\')
                    .replace('\\', r"\\")
            ));
        });

        self.push_args("/NOLOGO");
        // ESSENTIAL: we need to disable incremental linking, otherwise
        // the function pointer in the generated code might print to
        // jump thunk instead of the real function address, making the
        // gc stack map recording wrong function address
        self.push_args("/INCREMENTAL:NO");
        self.push_args("/DEBUG");
        self.push_args("ws2_32.lib");
        self.push_args("bcrypt.lib");
        self.push_args("userenv.lib");
        self.push_args("advapi32.lib");
        self.push_args("ntdll.lib");
        self.push_args("psapi.lib");
        self.push_args("PowrProf.lib");
        self.push_args("user32.lib"); // Add user32.lib
        self.push_args("dbghelp.lib"); // Add dbghelp.lib
        self.push_args("ole32.lib"); // Add ole32.lib
        self.push_args("shell32.lib"); // Add shell32.lib
        self.push_args("iphlpapi.lib"); // Add iphlpapi.lib

        self.args.insert(0, "lld-link".to_owned());
        lld_rs::link(lld_rs::LldFlavor::Coff, &self.args)
            .ok()
            .map_err(LinkerError::LinkError)
    }

    fn push_args(&mut self, arg: &str) {
        self.args.push(arg.to_owned());
    }
}

fn get_win_sdk_lib_paths() -> Vec<PathBuf> {
    let mut paths = vec![];
    let sdkroot = PathBuf::from(r"C:\Program Files (x86)\Windows Kits\");
    assert!(sdkroot.is_dir(), "Windows SDK not found");
    for dir in sdkroot.read_dir().unwrap().flatten() {
        if dir.path().is_symlink() || !dir.path().is_dir() {
            continue;
        }
        let mut p = dir.path();
        p.push("Lib");
        if p.is_dir() {
            for d in p.read_dir().unwrap().flatten() {
                if d.path().is_dir() {
                    let mut p = d.path();
                    p.push("ucrt\\x64");
                    if p.exists() {
                        paths.push(p);
                    }
                    let mut p = d.path();
                    p.push("um\\x64");
                    if p.exists() {
                        paths.push(p);
                    }
                }
            }
        }
    }
    paths
}
