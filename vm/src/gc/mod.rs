#[cfg(feature = "immix")]
mod _immix {
    use std::process::exit;

    use crate::logger::SimpleLogger;
    use backtrace::Backtrace;
    use immix::{gc_malloc_fast_unwind, gc_malloc_no_collect};
    use internal_macro::is_runtime;
    use log::trace;

    #[cfg(feature = "jit")]
    pub fn reg() {
        // rust在release模式会优化掉我们的ctor函数，所以我们在这里调用一下
        add_symbol_impl_diogc();
        add_symbol_immix_gc_init();
    }

    #[is_runtime]
    fn immix_gc_init(ptr: *mut u8) {
        SimpleLogger::init_from_env_default("GC_LOG", log::LevelFilter::Error);
        trace!("immix gc init, stackmap: {:p}", ptr);
        // immix::gc_disable_auto_collect();
        #[cfg(not(feature = "jit"))]
        immix::gc_init(ptr)
    }

    #[repr(C, packed)]
    #[derive(Debug, Clone, Copy)]
    pub(crate) struct StackMapHeader {
        version: u8,
        _reserved1: u8,
        _reserved2: u16,
        num_functions: u32,
        num_constants: u32,
        num_records: u32,
    }
    #[used]
    #[no_mangle]
    pub static _IMMIX_OBJTYPE_ATOMIC: u8 = 0;
    #[used]
    #[no_mangle]
    pub static _IMMIX_OBJTYPE_TRAIT: u8 = 1;
    #[used]
    #[no_mangle]
    pub static _IMMIX_OBJTYPE_COMPLEX: u8 = 2;
    #[used]
    #[no_mangle]
    pub static _IMMIX_OBJTYPE_POINTER: u8 = 3;

    struct DioGC();

    #[is_runtime] // jit注册
    impl DioGC {
        pub unsafe fn register_global(p: *mut u8) {
            immix::register_global(p);
        }
        pub unsafe fn malloc(size: u64, obj_type: u8, rsp: *mut *mut u8) -> *mut u8 {
            // asm read sp
            // let mut rust_sp: *mut u8;
            // asm!("mov {}, sp", out(reg) rust_sp);
            // eprintln!("rsp {:p}, rsp_load: {:p}", rsp, *rsp.offset(-1));
            trace!("malloc: {} {}", size, obj_type);
            #[cfg(any(test, debug_assertions))] // enable eager gc in test mode
            immix::gc_collect_fast_unwind(rsp as _);
            // gc_collect();
            let re = gc_malloc_fast_unwind(size as usize, obj_type, rsp as _);
            // let re = gc_malloc(size as usize, obj_type);
            if re.is_null() && size != 0 {
                eprintln!("gc malloc failed! (OOM)");
                let bt = Backtrace::new();
                println!("{:?}", bt);
                exit(1);
            }
            // eprintln!("malloc: {:p}", re);
            re
        }

        pub unsafe fn malloc_no_collect(size: u64, obj_type: u8) -> *mut u8 {
            trace!("malloc: {} {}", size, obj_type);
            let re = gc_malloc_no_collect(size as usize, obj_type);
            if re.is_null() && size != 0 {
                eprintln!("gc malloc failed! (OOM)");
                let bt = Backtrace::new();
                println!("{:?}", bt);
                exit(1);
            }
            // eprintln!("malloc_no_collect: {:p}", re);
            re
        }

        pub unsafe fn disable_auto_collect() {
            immix::gc_disable_auto_collect();
        }

        pub unsafe fn enable_auto_collect() {
            immix::gc_enable_auto_collect();
        }

        pub unsafe fn stuck_begin(sp: *mut u8) {
            immix::thread_stuck_start_fast(sp);
        }
        pub unsafe fn stuck_end() {
            immix::thread_stuck_end();
        }

        pub unsafe fn collect() {
            trace!("manual collect");
            immix::gc_collect()
        }

        pub fn get_stw_num() -> i64 {
            immix::get_gc_stw_num() as _
        }

        pub fn set_eva(eva: bool) {
            immix::set_evacuation(eva);
        }

        pub fn safepoint(sp: *mut u8) {
            immix::safepoint_fast_unwind(sp)
        }

        pub fn about() {
            let dio = "
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⢹⡀⠀⠀⠀⠀⠀⠘⠀⣷⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⡀⠀⠀⠀⠙⢦⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⡼⠃⠀⠀⠀⠀⠀⣤⠀⢏⠀⠀⠀⠀⢠⣠⡆⠀⠀⣦⡀⠀⠳⡀⠀⠀⠀⠀⠑⢄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈
        ⠀⠀⠀⠀⠀⠀⠀⠀⠐⣇⡀⠀⠀⠀⠀⠀⠘⠂⢈⣦⠀⣀⡀⠈⣟⢷⣄⠀⠘⣷⣄⠀⠹⣆⠀⠀⠀⠀⠀⠙⢦⣀⠀⠀⠀⠀⠀⠀⠀⢤
        ⠀⠀⠀⠀⠐⣶⠦⠤⠴⠋⠁⠀⠀⠀⠀⡜⢷⣧⣸⣿⡀⡟⠹⡄⢹⠀⣹⣷⣤⡘⣄⠙⠲⢬⣿⣉⡉⠉⠉⠉⠉⢉⣥⣀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠈⠳⠤⢤⡀⠀⠀⠀⠀⠀⢹⡾⣿⠛⠉⣧⡇⠀⢱⣸⡔⢡⠏⠀⠉⢻⣦⣤⠀⠈⠹⣿⣂⡀⣠⠔⢉⡤⠾⡆⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⢀⡞⣧⠀⠀⢠⠈⣇⢀⣿⠃⠀⠀⠸⣿⣠⣼⣟⣠⣯⣴⡿⠷⣿⠟⠁⠀⠀⠀⠀⠀⣇⡇⠀⡿⠦⡀⣇⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⣾⡼⡇⠀⠀⠘⡇⣿⣿⣿⢦⣄⣧⠀⣯⣿⣼⣿⣿⠋⢿⣽⣶⡏⠀⠀⠀⠀⠀⠀⠀⢻⠇⢀⡇⣠⠇⢸⡄⠀⠀⠀⣠
        ⠀⠀⠀⠀⠀⠀⠀⠙⠓⠳⣤⣶⠀⣿⠛⣿⢻⣷⣮⣽⡆⠈⠿⠟⠻⠛⠉⠉⠋⠉⠀⠀⠀⠀⠀⠀⠀⠀⠙⠀⠘⢿⠃⠀⣼⠁⠀⠀⠀⡱
        ⠀⠀⠀⠀⠀⠀⠀⢀⣠⡴⣺⣿⢠⣍⡀⠘⡿⢿⡿⠿⣷⡄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⡈⢀⡾⠃⠀⠀⠀⠘⢄
        ⠀⠀⠀⠀⠀⠀⠀⠀⠉⠉⠁⢸⡟⣾⡷⣄⢹⠀⠀⠀⣿⠁⣀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡏⡏⠉⠀⠀⠀⠀⠀⡐⠪
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠃⠈⠃⠀⠙⣇⠀⠀⠙⠦⠉⠉⠁⠀⠀⠀⠀⠀⢠⡆⠀⠀⠀⠀⠀⠀⠀⢸⠃⠹⡄⠀⠀⠀⠀⠀⠠⡀
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⣆⠀⠀⢠⣤⣤⡤⢒⣊⣩⣽⣿⣿⠀⠀⠀⠀⠀⠀⠀⠀⢸⡄⠀⠙⣿⠀⡄⠀⠀⠀⠙
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⢦⠀⠈⠹⣶⠛⣩⠔⠋⠉⠁⣸⠀⠀⠀⠀⠀⠀⠀⣠⢞⡁⠀⠀⡞⣸⠃⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠳⣄⠀⠈⣿⣇⣀⣀⣀⢴⡿⠀⠀⠀⠀⠀⣠⠞⠁⣸⠀⢀⡼⠟⠹⡀⠀⠀⠀⠀
        ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠳⡄⠙⠲⠤⠥⢖⡋⠀⠀⠀⠀⡠⠊⠁⠀⢠⠇⠀⠀⠀⠀⠀⢹⣉⡉⢰⡎
        ⠀⣀⣤⠖⠒⢲⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⣆⠀⠛⠋⠉⠀⠀⢀⡤⠊⠀⠀⠀⠀⠞⠀⠀⠀⠀⠀⠀⠀⢳⡼⠋⠀
        ⠋⡝⠁⠀⠀⠀⢱⡀⢀⡴⠊⠉⠉⠙⣇⠀⠀⠀⠀⠀⠀⠀⠀⠀⢘⣄⣀⣀⣀⡤⠖⠋⠀⠀⠀⠀⠀⠀⠀⠀⣀⣀⠤⠤⠖⠊⢁⡠⠖⠋
        ⠉⠉⠉⠉⠙⡆⠀⢷⠋⠀⠀⢀⡴⠚⠁⠀⠀⠀⠀⠀⠀⣠⠴⣚⠭⠜⠛⢯⠀⡇⠀⠀⣀⣀⠤⠄⠒⠒⠉⠉⠀⣀⣀⠤⠔⠊⠁⠀⠀⠀
        ⠳⠄⠀⠀⠀⡇⢀⡼⢦⡀⣰⠋⠀⠀⠀⠀⠀⠀⠀⠀⢸⣏⣛⠓⠤⠤⡀⠘⡆⢇⣠⠞⢁⣠⠤⠤⠖⠒⠒⠉⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⠈⠀⠀⠀⡟⠋⠀⠀⣹⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠹⡈⠉⠙⠢⡝⡄⠳⡼⠃⡴⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⢀⠀⢀⡴⠃⠀⠀⡸⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⠇⠀⠀⠀⠙⢸⡞⢠⠞⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⣻⠒⠋⠀⠀⠀⡰⠃⠀⠀⠀⠀⠀⠀⠀⣀⣀⠠⠤⠤⠼⡀⠀⠀⠀⠀⡞⢠⠏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠘⠁⠀⠀⠀⠀⡰⠁⠀⠀⢀⣠⠄⠒⠊⠉⠀⠀⠀⠀⠀⠀⠈⢢⡀⠀⢰⢡⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⠀⠀⢀⣼⣁⠤⠖⠊⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣽⣴⡾⠟⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
        ⠀⠀⢀⣠⠞⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣼⡟⠋⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀";
            println!("{}", dio);
            println!("        Dio gc");
        }
    }
}
#[cfg(feature = "immix")]
pub use _immix::*;
