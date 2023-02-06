//! # Simple GC
//!
//! 尽管早期确实帮了我们忙，但是他最终还是被淘汰惹
//!
//! 放在这里作为历史遗产，还有时不时拿出来跑个benchmark证明我们新gc的优越性
//!
//! 尽管新老gc的接口目前是一致的，可以切换使用，但是任何情况都不建议在生产中切换此gc。
//! 此gc目前教学意义大于实用意义
#![cfg(feature = "simple_gc")]
#![deprecated(
    note = "Simple gc is deprecated, use immix gc instead. Simple gc does not support multiple threads"
)]
use std::{
    cell::RefCell,
    collections::{btree_map::BTreeMap, HashSet},
    env,
    ops::Bound::*,
};

use internal_macro::is_runtime;
use libc::{c_void, malloc, memset, size_t};

thread_local! {
    static DIO_GC: RefCell<DioGC> = {
        let gc = DioGC::new();
        RefCell::new(gc)
    };
}
#[repr(C)]
pub struct Mem {
    size: i64,
    marked: bool,
}

#[cfg(feature = "jit")]
// bypass rust warning
pub fn reg() {
    // 防止被优化
    add_symbol_impl_diogc();
}

impl Mem {
    fn mark(&mut self) {
        self.marked = true;
    }
    fn unmark(&mut self) {
        self.marked = false;
    }
    fn is_marked(&self) -> bool {
        self.marked
    }
}

#[repr(C)]
#[derive(Default)]
pub struct DioGC {
    memtable: BTreeMap<isize, Mem>,
    size: i64,
    roots: HashSet<*mut c_void>,
    debug: bool,
}

unsafe fn get_region_ptr(
    memtable: &mut BTreeMap<isize, Mem>,
    ptr: *mut c_void,
) -> Option<(*mut c_void, &mut Mem)> {
    // 只获取当前区间左侧的指针
    let left_mem_tuple = memtable
        .range_mut((Unbounded, Included(ptr as isize)))
        .last();
    if let Some((lptr, mem)) = left_mem_tuple {
        if *lptr + mem.size as isize > ptr as isize {
            return Some((*lptr as *mut c_void, mem));
        }
    }
    None
}

#[is_runtime] // jit注册
impl DioGC {
    pub fn new() -> DioGC {
        DioGC {
            size: 0,
            memtable: BTreeMap::new(),
            roots: HashSet::new(),
            debug: env::var("DIO_DEBUG").is_ok(),
        }
    }

    pub unsafe fn new_ptr() -> *mut DioGC {
        Box::into_raw(Box::new(DioGC::new()))
    }

    pub unsafe fn malloc(size: i64, _obj_type: u8) -> *mut c_void {
        DIO_GC.with(|gc| {
            let gc = &mut *gc.borrow_mut();
            let ptr = malloc(size as size_t);
            memset(ptr, 0, size as size_t);
            gc.memtable.insert(
                ptr as isize,
                Mem {
                    size,
                    marked: false,
                },
            );
            gc.size += size;
            ptr
        })
    }

    pub unsafe fn free(&mut self, ptr: *mut c_void) {
        if let Some(mem) = self.memtable.remove(&(ptr as isize)) {
            self.size -= mem.size;
        }
    }
    pub fn get_size() -> i64 {
        DIO_GC.with(|gc| {
            let gc = &mut *gc.borrow_mut();
            gc.size
        })
    }

    pub fn add_root(ptr: *mut c_void, _obj_type: u8) {
        DIO_GC.with(|gc| {
            let gc = &mut *gc.borrow_mut();

            gc.roots.insert(ptr);
        })
    }

    pub fn rm_root(ptr: *mut c_void) {
        DIO_GC.with(|gc| {
            let gc = &mut *gc.borrow_mut();
            gc.roots.remove(&ptr);
        })
    }

    pub unsafe fn collect() {
        DIO_GC.with(|gc| {
            let gc = &mut *gc.borrow_mut();
            debug_log(&gc, || {
                println!("collect start, heap size: {}", gc.size);
            });
            gc.mark();
            gc.sweep();
            debug_log(&gc, || {
                println!("collect end, heap size: {}", gc.size);
            });
        })
    }
    pub unsafe fn sweep(&mut self) {
        let mut rm = Vec::new();
        for (ptr, mem) in self.memtable.iter_mut() {
            if !mem.marked {
                rm.push(*ptr);
            } else {
                mem.unmark();
            }
        }
        for ptr in rm {
            self.free(ptr as *mut c_void);
        }
    }

    pub unsafe fn mark(&mut self) {
        for ptr in self.roots.iter() {
            let p = *ptr;
            let pointto = *(p as *mut *mut c_void);
            Self::mark_ptr(&mut self.memtable, &pointto);
        }
    }
    pub unsafe fn mark_ptr(memtable: &mut BTreeMap<isize, Mem>, ptr: &*mut c_void) {
        let mem = get_region_ptr(memtable, *ptr);
        if let Some((ptr, mem)) = mem {
            if mem.is_marked() {
                return;
            }
            mem.mark();
            let p = ptr;
            let data = p as *mut *mut c_void;
            let size = mem.size / 8;
            for i in 0..size {
                let ptr = data.offset(i as isize).read();
                Self::mark_ptr(memtable, &ptr);
            }
        }
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

fn debug_log<F>(gc: &DioGC, logf: F)
where
    F: FnOnce() -> (),
{
    if gc.debug {
        logf();
    }
}

#[cfg(test)]
unsafe fn set_point_to(ptr1: *mut c_void, ptr2: *mut c_void, offset: i64) {
    (ptr1 as *mut *mut c_void)
        .offset(offset as isize)
        .write(ptr2);
}
#[cfg(test)]
#[test]
fn test_basic_gc() {
    unsafe {
        // allocate 4 pointers
        let mut ptr1 = DioGC::malloc(64, 0);
        let ptr2 = DioGC::malloc(64, 0);
        let _ = DioGC::malloc(64, 0);
        let _ = DioGC::malloc(64, 0);
        let size = DioGC::get_size();
        // get rust stack pointer point to ptr1
        let rustptr = (&mut ptr1) as *mut *mut c_void as *mut c_void;
        assert_eq!(size, 256);
        DioGC::add_root(rustptr, 0);
        // set ptr1 point to ptr2
        set_point_to(ptr1, ptr2, 0);
        DioGC::collect();
        assert_eq!(DioGC::get_size(), 128);
        // set ptr1 empty
        *(ptr1 as *mut i64) = 0;
        DioGC::collect();
        assert_eq!(DioGC::get_size(), 64);
        // remove gc root
        DioGC::rm_root(rustptr);
        DioGC::collect();
        assert_eq!(DioGC::get_size(), 0);
    }
}

#[cfg(test)]
#[test]
fn test_complicated_gc() {
    // allocate 1001 pointers
    // which randomly point to each other
    use rand::Rng;
    unsafe {
        DioGC::about();
        println!("start test_complicated_gc");
        // allocate first pointers
        // 这里看起来是分配了个堆指针，实际上ptrfirst是个rust变量，底层是一个栈指针，它指向的值是堆指针
        let mut ptrfirst = DioGC::malloc(64, 0);
        println!("mark ptr: {:p}", ptrfirst);
        let mut size = 64;
        // get rust stack pointer point to ptr1（获取栈指针
        let rustptr = (&mut ptrfirst) as *mut *mut c_void as *mut c_void;
        println!("stack ptr: {:p}", rustptr);
        println!("mark ptr: {:p}", *(rustptr as *mut *mut c_void));
        // important!
        // 如果直接给ptr1赋值，会写入原本的栈指针指向的空间，导致测试的gcroot指向最后一个赋值对象
        // 而不是第一个ptr1，所以这里在栈上声明一个新的ptr1
        let mut ptr1 = ptrfirst;
        for _ in 0..1000 {
            let ptr = DioGC::malloc(64, 0);
            let n = rand::thread_rng().gen_range(0..2);
            if n == 0 {
                size += 64;
                let offset = rand::thread_rng().gen_range(0..8);
                set_point_to(ptr1, ptr, offset);
                ptr1 = ptr;
                println!("mark ptr: {:p}", ptr1);
            }
        }
        println!(
            "allocated size: {}, amoung which {} should be chained to gc_root",
            1001 * 64,
            size
        );
        println!("testing current gc size");
        assert_eq!(DioGC::get_size(), 64 * 1001);
        println!("current gc size is correct: {}", DioGC::get_size());
        DioGC::add_root(rustptr, 0);
        println!("try to force gc collect");
        use std::time::Instant;
        let now = Instant::now();
        DioGC::collect();
        let spent = now.elapsed();
        println!(
            "gc collect finished, time spent: {:#?}, testing gc size after collection",
            spent
        );
        assert_eq!(DioGC::get_size(), size);
        println!("gc size after collection is correct: {}", DioGC::get_size());
        println!("change gc root to origin ptr with offset");
        ptrfirst = ptrfirst.offset(1);
        let rustptr = (&mut ptrfirst) as *mut *mut c_void as *mut c_void;
        println!("stack ptr: {:p}", rustptr);
        DioGC::collect();
        assert_eq!(DioGC::get_size(), size);
        println!("gc size after collection is correct: {}", DioGC::get_size());
    }
}
