/// This macro spins until the expression $cond evaluates to true.
/// It yields every 100 iterations to prevent starving other tasks.
#[macro_export]
macro_rules! spin_until {
    ($cond:expr) => {
        let mut i: i32 = 0;
        while !$cond {
            core::hint::spin_loop();
            let (re, _) = i.overflowing_add(1);
            i = re;
            if i % 100 == 0 {
                std::thread::yield_now();
            }
        }
    };
}

#[macro_export]
macro_rules! round_n_up {
    ($v:expr, $n:expr) => {
        $v + $n - ($v - 1) % $n - 1
    };
}
