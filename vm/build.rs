fn main() {
    println!("cargo:rustc-link-lib=static:+whole-archive=uv");
}
