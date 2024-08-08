use internal_macro::is_runtime;

#[is_runtime]
fn unixtime() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

#[is_runtime]
fn pl_clock_gettime(sec: *mut i64, nano: *mut u32) {
    let t = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap();
    unsafe {
        *sec = t.as_secs() as i64;
        *nano = t.subsec_nanos();
    }
}
