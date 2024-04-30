use internal_macro::is_runtime;

#[is_runtime]
fn unixtime() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

struct Timespec {
    tv_sec: i64,
    tv_nsec: u32,
}

#[repr(C)]
struct TimespecC {
    tv_sec: i64,
    tv_nsec: u32,
}

#[is_runtime]
fn pl_clock_gettime() -> TimespecC {
    let t = std::time::Instant::now();
    let t: Timespec = std::mem::transmute(t);
    TimespecC {
        tv_sec: t.tv_sec,
        tv_nsec: t.tv_nsec as _,
    }
}

#[cfg(feature = "jit")]
pub fn reg() {
    add_symbol_unixtime();
}
