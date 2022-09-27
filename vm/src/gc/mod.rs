pub mod color3;
use std::collections::{HashMap, HashSet};

use add_symbol::is_runtime;
use libc::{c_void, malloc, memset, size_t};

struct Mem {
    size: i64,
    marked: bool,
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

struct DioGC {
    memtable: HashMap<*mut c_void, Mem>,
    size: i64,
    roots: HashSet<*mut c_void>,
}

#[is_runtime]
impl DioGC {
    pub fn new() -> DioGC {
        DioGC {
            size: 0,
            memtable: HashMap::new(),
            roots: HashSet::new(),
        }
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
        self.mark();
        self.sweep();
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
            let pointto = *(*ptr as *mut i64);
            let p = pointto as *mut c_void;
            Self::mark_ptr(&mut self.memtable, &p);
        }
    }
    pub unsafe fn mark_ptr(memtable: &mut HashMap<*mut c_void, Mem>, ptr: &*mut c_void) {
        let mem = memtable.get_mut(ptr);
        if let Some(mem) = mem {
            if mem.is_marked() {
                return;
            }
            mem.mark();
            let p = *ptr;
            let data = p as *mut i64;
            let size = mem.size / 8;
            for i in 0..size {
                let ptr = data.offset(i as isize);
                let i1 = *ptr;
                let ptr = i1 as *mut c_void;
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

unsafe fn set_point_to(ptr1: *mut c_void, ptr2: *mut c_void, offset: i64) {
    let v1 = ptr2 as i64;
    *((ptr1 as *mut i64).offset(offset as isize)) = v1;
}

#[test]
fn test_basic_gc() {
    unsafe {
        let mut gc = DioGC::new();
        gc.about();
        // allocate 4 pointers
        let ptr1 = gc.malloc(64);
        let ptr2 = gc.malloc(64);
        let ptr3 = gc.malloc(64);
        let ptr4 = gc.malloc(64);
        let size = gc.get_size();
        // get rust stack pointer point to ptr1
        let rustptr = &(ptr1 as i64) as *const _ as *mut c_void;
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
        let mut size = 64;
        // get rust stack pointer point to ptr1
        let rustptr = &(ptr1 as i64) as *const _ as *mut c_void;
        for i in 0..1000 {
            let ptr = gc.malloc(64);
            let n= rand::thread_rng().gen_range(0..2);
            if n == 0 {
                size += 64;
                let offset = rand::thread_rng().gen_range(0..8);
                set_point_to(ptr1, ptr, offset);
                ptr1 = ptr;
            }
        }
        println!("allocated size: {}, amoung which {} should be chained to gc_root", 1001 * 64, size);
        println!("testing current gc size");
        assert_eq!(gc.get_size(), 64*1001);
        println!("current gc size is correct: {}", gc.get_size());
        gc.add_root(rustptr);
        println!("try to force gc collect");
        use std::time::Instant;
        let now = Instant::now();
        gc.collect();
        let spent = now.elapsed();
        println!("gc collect finished, time spent: {:#?}, testing gc size after collection", spent);
        assert_eq!(gc.get_size(), size);
        println!("gc size after collection is correct: {}", gc.get_size());
    }
}
