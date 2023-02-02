fn main() {
    #[cfg(all(feature = "llvm_stackmap",feature = "llvm_gc_plugin"))]
    {
        // println!("cargo:rerun-if-changed=build.rs");
        // println!("cargo:rerun-if-changed=llvm/plimmixprinter.cpp");
        // println!("cargo:rerun-if-changed=llvm/plimmix.cpp");
        // println!("cargo:rerun-if-changed=llvm/CMakeLists.txt");
        let dst = cmake::build("llvm");
        println!(
            "cargo:rustc-link-search=native={}/build",
            dst.display().to_string()
        );
        println!("cargo:rustc-link-lib=static=plimmix_plugin");

    }
}
