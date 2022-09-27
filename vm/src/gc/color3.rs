// 3 color gc
// https://draveness.me/golang/docs/part3-runtime/ch07-memory/golang-garbage-collector/
// 从灰色对象的集合中选择一个灰色对象并将其标记成黑色；
// 将黑色对象指向的所有对象都标记成灰色，保证该对象和被该对象引用的对象都不会被回收；
// 重复上述两个步骤直到对象图中不存在灰色对象；

use add_symbol::is_runtime;
use libc::{c_void, malloc, memset, size_t};
use std::collections::{HashMap, HashSet};

enum Color {
    White, // possible garbage
    Black, // reachable
    Grey,  // reachable, check
}

struct Mem {
    size: i64,
    marked: Color,
}

// save the object to corresponding color map
struct Color3GC {
    memtable: HashMap<*mut c_void, Mem>,
    size: i64,
    roots: HashSet<*mut c_void>,
    // TODO: use a better data structure
    colorset: Vec<HashSet<*mut c_void>>,
}

#[is_runtime]
impl Color3GC {
    pub fn new() -> Color3GC {
        Color3GC {
            size: 0,
            memtable: HashMap::new(),
            roots: HashSet::new(),
            colorset: vec![HashSet::new(), HashSet::new(), HashSet::new()],
        }
    }

    pub fn add_root(&mut self, ptr: *mut c_void) {
        self.roots.insert(ptr);
    }

    pub fn rm_root(&mut self, ptr: *mut c_void) {
        self.roots.remove(&ptr);
    }

    pub unsafe fn malloc(&mut self, size: i64) -> *mut c_void {
        let ptr = malloc(size as size_t);
        memset(ptr, 0, size as size_t);
        self.memtable.insert(
            ptr,
            Mem {
                size,
                marked: Color::White,
            },
        );
        self.size += size;
        self.colorset[Color::White as usize].insert(ptr);
        ptr
    }

    pub unsafe fn free(&mut self, ptr: *mut c_void) {
        if let Some(mem) = self.memtable.remove(&ptr) {
            self.size -= mem.size;
        }
    }

    pub unsafe fn collect(&mut self) {
        self.mark();
        self.sweep();
    }

    pub unsafe fn sweep(&mut self) {
        let mut rm = Vec::new();
        for (ptr, mem) in self.memtable.iter_mut() {
            match mem.marked {
                Color::White => {
                    rm.push(*ptr);
                }
                Color::Black => {
                    mem.marked = Color::White;
                }
                Color::Grey => {
                    mem.marked = Color::White;
                }
            }
        }
        for ptr in rm {
            self.free(ptr);
        }
    }

    // mark object with color
    pub unsafe fn mark_object(
        memtable: &mut HashMap<*mut c_void, Mem>,
        colorset: &mut Vec<HashSet<*mut c_void>>,
        ptr: *mut c_void,
        color: Color,
    ) {
        // remove from old color set
        let old_color = &memtable.get(&ptr).unwrap().marked;
        colorset[*old_color as usize].remove(&ptr);
        // add to new color set
        colorset[color as usize].insert(ptr);
        // update color
        memtable.get_mut(&ptr).unwrap().marked = color;
    }

    pub unsafe fn get_children(
        memtable: &mut HashMap<*mut c_void, Mem>,
        ptr: &*mut c_void,
    ) -> Vec<*mut c_void> {
        let mut children = Vec::new();
        let mut i = 0;
        let mem = memtable.get_mut(ptr);
        if let Some(mem) = mem {
            // if mem.is_marked() {
            //     return;
            // }
            // mem.mark();
            let p = *ptr;
            let data = p as *mut i64;
            let size = mem.size / 8;
            for i in 0..size {
                let ptr = data.offset(i as isize);
                let i1 = *ptr;
                let child_ptr = i1 as *mut c_void;
                // if in memtable, mark as grey
                if memtable.contains_key(&child_ptr) {
                    children.push(child_ptr);
                }
            }
        }
        children
    }

    pub fn print_color(colorset: Vec<HashSet<*mut c_void>>) {
        // print color set
        for (i, color) in colorset.iter().enumerate() {
            println!("color {}: {:?}", i, color);
        }
    }

    pub unsafe fn mark(&mut self) {
        // mark all roots
        for root in self.roots.iter() {
            let pointto = *(*root as *mut i64);
            let rootptr = pointto as *mut c_void;
            // mark all roots with grey
            Self::mark_object(&mut self.memtable, &mut self.colorset, rootptr, Color::Grey);
        }
        // iter grey roots
        while !self.colorset[Color::Grey as usize].is_empty() {
            // get one grey root
            let grey_root = self.colorset[Color::Grey as usize].iter().next().unwrap();
            let tmp = *grey_root; // Here
            let ptr_addr = &tmp;

            // mark grey root with black
            Self::mark_object(
                &mut self.memtable,
                &mut self.colorset,
                *ptr_addr,
                Color::Black,
            );
            // mark all children with grey
            let children = Self::get_children(&mut self.memtable, ptr_addr);
            for child in children.iter() {
                Self::mark_object(&mut self.memtable, &mut self.colorset, *child, Color::Grey);
            }
        }
    }

    pub fn get_size(&self) -> i64 {
        self.size
    }
}

unsafe fn set_point_to(ptr1: *mut c_void, ptr2: *mut c_void, offset: i64) {
    let v1 = ptr2 as i64;
    *((ptr1 as *mut i64).offset(offset as isize)) = v1;
}

#[test]
fn test_basic_gc() {
    unsafe {
        let mut gc = Color3GC::new();
        println!("size: {}", gc.get_size());

        // allocate 4 pointers
        let ptr1 = gc.malloc(64);
        let ptr2 = gc.malloc(64);
        let ptr3 = gc.malloc(64);
        let ptr4 = gc.malloc(64);
        let size = gc.get_size();

        assert_eq!(size, 256);

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
        let mut gc = Color3GC::new();
        println!("start test_complicated_gc");
        // allocate first pointers
        let mut ptr1 = gc.malloc(64);
        let mut size = 64;
        // get rust stack pointer point to ptr1
        let rustptr = &(ptr1 as i64) as *const _ as *mut c_void;
        for i in 0..1000 {
            let ptr = gc.malloc(64);
            let n = rand::thread_rng().gen_range(0..2);
            if n == 0 {
                size += 64;
                let offset = rand::thread_rng().gen_range(0..8);
                set_point_to(ptr1, ptr, offset);
                ptr1 = ptr;
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
    }
}
