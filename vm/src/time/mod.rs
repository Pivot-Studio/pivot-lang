use internal_macro::is_runtime;

#[is_runtime]
fn unixtime() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

#[cfg(feature = "jit")]
pub fn reg() {
    add_symbol_unixtime();
}
