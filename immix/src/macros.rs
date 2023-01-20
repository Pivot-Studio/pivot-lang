/// This macro spins until the expression $cond evaluates to true.
/// It yields every 100 iterations to prevent starving other tasks.
#[macro_export]
macro_rules! spin_until {
    ($cond:expr) => {
        let mut i = 0;
        while !$cond {
            core::hint::spin_loop();
            i += 1;
            if i % 100 == 0 {
                std::thread::yield_now();
            }
        }
    };
}
