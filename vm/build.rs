use std::env;
// use std::error;
// use std::fmt;
// use std::fs::OpenOptions;
// use std::io;
// use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

// static LIBUV_VERSION: &str = "1.48.0";

// #[derive(Debug)]
// enum Error {
//     BindgenError,
//     PathError(String, io::Error),
// }

// impl error::Error for Error {
//     fn source(&self) -> Option<&(dyn error::Error + 'static)> {
//         match self {
//             Error::PathError(_, err) => Some(err),
//             _ => None,
//         }
//     }
// }

// impl fmt::Display for Error {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             Error::BindgenError => write!(f, "Failed to generate ffi bindings"),
//             Error::PathError(dir, source) => write!(f, "Path error for `{}`: {}", dir, source),
//         }
//     }
// }

// type Result<T> = std::result::Result<T, Error>;

// fn build_pkgconfig_max_version() -> String {
//     let dotidx = LIBUV_VERSION.find('.').unwrap();
//     let dotidx2 = LIBUV_VERSION[(dotidx + 1)..].find('.').unwrap() + dotidx + 1;
//     let next_minor_version = LIBUV_VERSION[(dotidx + 1)..dotidx2]
//         .parse::<usize>()
//         .unwrap()
//         + 1;
//     format!("{}.{}.0", &LIBUV_VERSION[..dotidx], next_minor_version)
// }

fn build<P: AsRef<Path>>(source_path: &P) {
    let src_path = source_path.as_ref().join("src");
    let unix_path = src_path.join("unix");

    let target = env::var("TARGET").unwrap();
    let android = target.ends_with("-android") || target.ends_with("-androideabi");
    let apple = target.contains("-apple-");
    let dragonfly = target.ends_with("-dragonfly");
    let freebsd = target.ends_with("-freebsd");
    let linux = target.contains("-linux-");
    let netbsd = target.ends_with("-netbsd");
    let openbsd = target.ends_with("-openbsd");
    let solaris = target.ends_with("-solaris");

    // based on libuv's CMakeLists.txt
    let mut build = cc::Build::new();
    let compiler = build.get_compiler();
    let clang = compiler.is_like_clang();
    let gnu = compiler.is_like_gnu();
    let msvc = compiler.is_like_msvc();
    build
        .include(source_path.as_ref().join("include"))
        .include(&src_path);

    if msvc {
        build
            .flag("/W4")
            .flag("/wd4100") // no-unused-parameter
            .flag("/wd4127") // no-conditional-constant
            .flag("/wd4201") // no-nonstandard
            .flag("/wd4206") // no-nonstandard-empty-tu
            .flag("/wd4210") // no-nonstandard-file-scope
            .flag("/wd4232") // no-nonstandard-nonstatic-dlimport
            .flag("/wd4456") // no-hides-local
            .flag("/wd4457") // no-hides-param
            .flag("/wd4459") // no-hides-global
            .flag("/wd4706") // no-conditional-assignment
            .flag("/wd4996") // no-unsafe
            .flag("/utf-8"); // utf8
    } else if apple || clang || gnu {
        build
            .flag("-fvisibility=hidden")
            .flag("--std=gnu89")
            .flag("-Wall")
            .flag("-Wextra")
            .flag("-Wstrict-prototypes")
            .flag("-Wno-unused-parameter");
    }
    if gnu {
        build.flag("-fno-strict-aliasing");
    }

    build
        .file(src_path.join("fs-poll.c"))
        .file(src_path.join("idna.c"))
        .file(src_path.join("inet.c"))
        .file(src_path.join("random.c"))
        .file(src_path.join("strscpy.c"))
        .file(src_path.join("strtok.c"))
        .file(src_path.join("thread-common.c"))
        .file(src_path.join("threadpool.c"))
        .file(src_path.join("timer.c"))
        .file(src_path.join("uv-common.c"))
        .file(src_path.join("uv-data-getter-setters.c"))
        .file(src_path.join("version.c"));

    if cfg!(windows) {
        println!("cargo:rustc-link-lib=shell32");
        println!("cargo:rustc-link-lib=psapi");
        println!("cargo:rustc-link-lib=user32");
        println!("cargo:rustc-link-lib=advapi32");
        println!("cargo:rustc-link-lib=iphlpapi");
        println!("cargo:rustc-link-lib=userenv");
        println!("cargo:rustc-link-lib=ws2_32");
        println!("cargo:rustc-link-lib=dbghelp");
        println!("cargo:rustc-link-lib=ole32");

        let win_path = src_path.join("win");
        build
            .define("_WIN32_WINNT", "0x0602")
            .define("WIN32_LEAN_AND_MEAN", None)
            .define("_CRT_DECLARE_NONSTDC_NAMES", "0")
            .file(win_path.join("async.c"))
            .file(win_path.join("core.c"))
            .file(win_path.join("detect-wakeup.c"))
            .file(win_path.join("dl.c"))
            .file(win_path.join("error.c"))
            .file(win_path.join("fs.c"))
            .file(win_path.join("fs-event.c"))
            .file(win_path.join("getaddrinfo.c"))
            .file(win_path.join("getnameinfo.c"))
            .file(win_path.join("handle.c"))
            .file(win_path.join("loop-watcher.c"))
            .file(win_path.join("pipe.c"))
            .file(win_path.join("thread.c"))
            .file(win_path.join("poll.c"))
            .file(win_path.join("process.c"))
            .file(win_path.join("process-stdio.c"))
            .file(win_path.join("signal.c"))
            .file(win_path.join("snprintf.c"))
            .file(win_path.join("stream.c"))
            .file(win_path.join("tcp.c"))
            .file(win_path.join("tty.c"))
            .file(win_path.join("udp.c"))
            .file(win_path.join("util.c"))
            .file(win_path.join("winapi.c"))
            .file(win_path.join("winsock.c"));
    } else {
        // CMakeLists.txt also checks that it's not OS/390 and not QNX
        if !android {
            println!("cargo:rustc-link-lib=pthread");
        }

        build
            .define("_FILE_OFFSET_BITS", "64")
            .define("_LARGEFILE_SOURCE", None)
            .file(unix_path.join("async.c"))
            .file(unix_path.join("core.c"))
            .file(unix_path.join("dl.c"))
            .file(unix_path.join("fs.c"))
            .file(unix_path.join("getaddrinfo.c"))
            .file(unix_path.join("getnameinfo.c"))
            .file(unix_path.join("loop-watcher.c"))
            .file(unix_path.join("loop.c"))
            .file(unix_path.join("pipe.c"))
            .file(unix_path.join("poll.c"))
            .file(unix_path.join("process.c"))
            .file(unix_path.join("random-devurandom.c"))
            .file(unix_path.join("signal.c"))
            .file(unix_path.join("stream.c"))
            .file(unix_path.join("tcp.c"))
            .file(unix_path.join("thread.c"))
            .file(unix_path.join("tty.c"))
            .file(unix_path.join("udp.c"));
    }

    // CMakeLists.txt has some special additions for AIX here; how do I test for it?

    if android {
        println!("cargo:rustc-link-lib=dl");
        build
            .define("_GNU_SOURCE", None)
            .file(unix_path.join("linux.c"))
            .file(unix_path.join("procfs-exepath.c"))
            .file(unix_path.join("random-getentropy.c"))
            .file(unix_path.join("random-getrandom.c"))
            .file(unix_path.join("random-sysctl-linux.c"));
    }

    if apple || android || linux {
        build.file(unix_path.join("proctitle.c"));
    }

    if dragonfly || freebsd {
        build.file(unix_path.join("freebsd.c"));
    }

    if dragonfly || freebsd || netbsd || openbsd {
        build
            .file(unix_path.join("posix-hrtime.c"))
            .file(unix_path.join("bsd-proctitle.c"));
    }

    if apple || dragonfly || freebsd || netbsd || openbsd {
        build
            .file(unix_path.join("bsd-ifaddrs.c"))
            .file(unix_path.join("kqueue.c"));
    }

    if freebsd {
        build.file(unix_path.join("random-getrandom.c"));
    }

    if apple || openbsd {
        build.file(unix_path.join("random-getentropy.c"));
    }

    if apple {
        build
            .define("_DARWIN_UNLIMITED_SELECT", "1")
            .define("_DARWIN_USE_64_BIT_INODE", "1")
            .file(unix_path.join("darwin-proctitle.c"))
            .file(unix_path.join("darwin.c"))
            .file(unix_path.join("fsevents.c"));
    }

    // CMakeLists.txt has a check for GNU here

    if linux {
        build
            .define("_GNU_SOURCE", None)
            .define("_POSIX_C_SOURCE", "200112")
            .file(unix_path.join("linux.c"))
            .file(unix_path.join("procfs-exepath.c"))
            .file(unix_path.join("random-getrandom.c"))
            .file(unix_path.join("random-sysctl-linux.c"));
        println!("cargo:rustc-link-lib=dl");
        println!("cargo:rustc-link-lib=rt");
    }

    if netbsd {
        build.file(unix_path.join("netbsd.c"));
        println!("cargo:rustc-link-lib=kvm");
    }

    if openbsd {
        build.file(unix_path.join("openbsd.c"));
    }

    // CMakeLists.txt has a check for OS/390 and OS/400 here

    if solaris {
        // CMakeLists.txt has a check for a specific version of Solaris here (v5.10)
        build
            .define("__EXTENSIONS__", None)
            .define("_XOPEN_SOURCE", "500")
            .define("_REENTRANT", None)
            .file(unix_path.join("no-proctitle.c"))
            .file(unix_path.join("sunos.c"));
        println!("cargo:rustc-link-lib=kstat");
        println!("cargo:rustc-link-lib=nsl");
        println!("cargo:rustc-link-lib=sendfile");
        println!("cargo:rustc-link-lib=socket");
    }

    // CMakeLists.txt has checks for Haiku, QNX, Cygwin, and MSYS here

    build.compile("uv");
}

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

const LIBUV_REPO: &str = "https://github.com/libuv/libuv.git";
const LIBUV_DIR: &str = "libuv";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    let out_dir = env::var("OUT_DIR").unwrap();
    let mut uv_src = PathBuf::from(out_dir);
    uv_src.push(LIBUV_DIR);
    if !uv_src.exists() {
        run("git", |cmd| cmd.arg("clone").arg(LIBUV_REPO).arg(&uv_src));

        // run("git", |cmd| {
        //     cmd.arg("clone")
        //         .arg(BOEHM_ATOMICS_REPO)
        //         .current_dir(&boehm_src)
        // });

        // env::set_current_dir(&boehm_src).unwrap();
        // run("cmake", |cmd| cmd.arg("."));
        // run("cmake", |cmd| {
        //     cmd.args(&["--build", ".", "--config", "Release"])
        // });
    }

    build(&uv_src);
    #[cfg(target_os = "linux")]
    cc::Build::new().file("dso_handle.c").compile("dso_handle");

    // println!("cargo:rustc-link-lib=static:+whole-archive=uv");
}
