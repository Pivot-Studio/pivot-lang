use std::{
    collections::{btree_map::BTreeMap, HashSet},
    env,
    ops::Bound::*,
};

use internal_macro::is_runtime;
use libc::{c_void, malloc, memset, size_t};

#[repr(C)]
pub struct Mem {
    size: i64,
    marked: bool,
}

// bypass rust warning
pub fn reg() {
    // 防止被优化
    DioGC {
        ..Default::default()
    };
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
    memtable: BTreeMap<*mut c_void, Mem>,
    size: i64,
    roots: HashSet<*mut c_void>,
    debug: bool,
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

    pub unsafe fn get_region_ptr(&mut self, ptr: *mut c_void) -> *mut c_void {
        // 只获取当前区间左侧的指针
        let left_mem_tuple = self.memtable.range((Unbounded, Included(ptr))).last();

        if let Some(mem_tuple) = left_mem_tuple {
            let left_ptr = *(mem_tuple.0);
            let size = (mem_tuple.1).size / 8;

            let data = left_ptr as *mut *mut c_void;
            for i in 0..size {
                let cur_ptr = data.offset(i as isize).read();
                if ptr == cur_ptr {
                    return left_ptr
                }
            }
        }     
        ptr.clone()
    }

    pub unsafe fn malloc(&mut self, size: i64) -> *mut c_void {
        let ptr = malloc(size as size_t);
        memset(ptr, 0, size as size_t);
        self.memtable.insert(
            ptr,
            Mem {
                size,
                marked: false,
            },
        );
        self.size += size;
        ptr
    }

    pub unsafe fn free(&mut self, ptr: *mut c_void) {
        if let Some(mem) = self.memtable.remove(&ptr) {
            self.size -= mem.size;
        }
    }
    pub fn get_size(&self) -> i64 {
        self.size
    }

    pub fn add_root(&mut self, ptr: *mut c_void) {
        self.roots.insert(ptr);
    }

    pub fn rm_root(&mut self, ptr: *mut c_void) {
        self.roots.remove(&ptr);
    }

    pub unsafe fn collect(&mut self) {
        debug_log(&self, || {
            println!("collect start, heap size: {}", self.size);
        });
        self.mark();
        self.sweep();
        debug_log(&self, || {
            println!("collect end, heap size: {}", self.size);
        });
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
            self.free(ptr);
        }
    }

    pub unsafe fn mark(&mut self) {
        for ptr in self.roots.iter() {
            let p = *ptr;
            let pointto = *(p as *mut *mut c_void);
            Self::mark_ptr(&mut self.memtable, &pointto);
        }
    }
    pub unsafe fn mark_ptr(memtable: &mut BTreeMap<*mut c_void, Mem>, ptr: &*mut c_void) {
        let mem = memtable.get_mut(ptr);
        if let Some(mem) = mem {
            if mem.is_marked() {
                return;
            }
            mem.mark();
            let p = *ptr;
            let data = p as *mut *mut c_void;
            let size = mem.size / 8;
            for i in 0..size {
                let ptr = data.offset(i as isize).read();
                Self::mark_ptr(memtable, &ptr);
            }
        }
    }
    pub fn about(&self) {
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
        let mut gc = DioGC::new();
        gc.about();
        // allocate 4 pointers
        let mut ptr1 = gc.malloc(64);
        let ptr2 = gc.malloc(64);
        let _ = gc.malloc(64);
        let _ = gc.malloc(64);
        let size = gc.get_size();
        // get rust stack pointer point to ptr1
        let rustptr = (&mut ptr1) as *mut *mut c_void as *mut c_void;
        assert_eq!(size, 256);
        gc.add_root(rustptr);
        // set ptr1 point to ptr2
        set_point_to(ptr1, ptr2, 0);
        gc.collect();
        assert_eq!(gc.get_size(), 128);
        // set ptr1 empty
        *(ptr1 as *mut i64) = 0;
        gc.collect();
        assert_eq!(gc.get_size(), 64);
        // remove gc root
        gc.rm_root(rustptr);
        gc.collect();
        assert_eq!(gc.get_size(), 0);
    }
}

#[cfg(test)]
#[test]
fn test_complicated_gc() {
    // allocate 1001 pointers
    // which randomly point to each other
    use rand::Rng;
    unsafe {
        let mut gc = DioGC::new();
        gc.about();
        println!("start test_complicated_gc");
        // allocate first pointers
        let mut ptr1 = gc.malloc(64);
        println!("mark ptr: {:p}", ptr1);
        let mut size = 64;
        // get rust stack pointer point to ptr1
        let rustptr = (&mut ptr1) as *mut *mut c_void as *mut c_void;
        println!("stack ptr: {:p}", rustptr);
        println!("mark ptr: {:p}", *(rustptr as *mut *mut c_void));
        // important!
        // 如果直接给ptr1赋值，会写入原本的栈指针指向的空间，导致测试的gcroot指向最后一个赋值对象
        // 而不是第一个ptr1，所以这里在栈上声明一个新的ptr1
        let mut ptr1 = ptr1;
        for _ in 0..1000 {
            let ptr = gc.malloc(64);
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
        assert_eq!(gc.get_size(), 64 * 1001);
        println!("current gc size is correct: {}", gc.get_size());
        gc.add_root(rustptr);
        println!("try to force gc collect");
        use std::time::Instant;
        let now = Instant::now();
        gc.collect();
        let spent = now.elapsed();
        println!(
            "gc collect finished, time spent: {:#?}, testing gc size after collection",
            spent
        );
        assert_eq!(gc.get_size(), size);
        println!("gc size after collection is correct: {}", gc.get_size());

        println!("testing ptr change");
        println!("ptr1: {:p}", ptr1);
        let offset_ptr = ptr1.offset(4);
        println!("offset_ptr: {:p}", offset_ptr);
        set_point_to(ptr1, offset_ptr, 1);
        let region_ptr = gc.get_region_ptr(offset_ptr);
        assert_eq!(ptr1, region_ptr);
        gc.rm_root(ptr1);
        gc.add_root(offset_ptr);
        gc.collect();
        println!("gc size after collection is correct: {}", gc.get_size());

    }
}
