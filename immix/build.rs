fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    let dst = cmake::build("llvm");
    println!(
        "cargo:rustc-link-search=native={}/build",
        dst.display().to_string()
    );
    println!("cargo:rustc-link-lib=static=plimmix_plugin");
}
