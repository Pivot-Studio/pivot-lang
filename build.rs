// build.rs

use vergen::{vergen, Config};
fn main() {
    let mut cfg = Config::default();
    *cfg.sysinfo_mut().name_mut() = false;
    vergen(cfg).expect("Fail to generate version info");
}
