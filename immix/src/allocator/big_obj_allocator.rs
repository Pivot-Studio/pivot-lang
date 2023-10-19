use std::cell::RefCell;

use parking_lot::ReentrantMutex;

use crate::{bigobj::BigObj, mmap::Mmap, round_n_up, ALIGN, BIG_OBJ_ALIGN};

pub struct BigObjAllocator {
    mmap: Mmap,
    current: *mut u8,
    heap_start: *mut u8,
    heap_end: *mut u8,
    unused_chunks: Vec<*mut BigObj>,
    lock: ReentrantMutex<()>,
    commited_to: RefCell<*mut u8>,
}

impl BigObjAllocator {
    pub fn new(size: usize) -> Self {
        let mmap = Mmap::new(size);
        Self {
            current: mmap.aligned(ALIGN),
            heap_start: mmap.aligned(ALIGN),
            commited_to: RefCell::new(mmap.aligned(ALIGN)),
            heap_end: mmap.end(),
            mmap,
            unused_chunks: Vec::new(),
            // used_objs: Vec::new(),
            lock: ReentrantMutex::new(()),
        }
    }

    pub fn state(&self) {
        println!("current: {:p}", self.current);
        println!("heap_start: {:p}", self.heap_start);
        println!("heap_end: {:p}", self.heap_end);
    }

    pub fn alloc_chunk(&self, size: usize) -> Option<*mut BigObj> {
        // let size = round_n_up!(size, ALIGN);
        let _lock = self.lock.lock();
        let current = self.current;
        let heap_end = self.heap_end;
        let next = unsafe { current.add(size) };

        if next >= heap_end {
            panic!("big object mmap out of memory");
        }
        unsafe {
            let end = current.add(size);
            let offset = current.align_offset(ALIGN);
            let dest = *self.commited_to.borrow();

            if end > dest && !self.mmap.commit(dest, size - offset) {
                panic!("big object mmap out of memory");
            }
            self.commited_to.replace(end.add(end.align_offset(ALIGN)));
        }

        // self.current = next;
        let obj = BigObj::new(current, size);

        Some(obj)
    }

    pub fn get_chunk(&mut self, size: usize) -> *mut BigObj {
        let size = round_n_up!(size + 16, BIG_OBJ_ALIGN); // 16 is the size of BigObj
        let _lock = self.lock.lock();
        for i in 0..self.unused_chunks.len() {
            let unused_obj = self.unused_chunks[i];
            let unused_size = unsafe { (*unused_obj).size };
            match unused_size.cmp(&size) {
                std::cmp::Ordering::Less => {}
                std::cmp::Ordering::Equal => {
                    self.unused_chunks.remove(i);
                    // self.mmap.commit(unused_obj as *mut u8, size);
                    // println!(
                    //     "get_chunk: {:p}[reused {}/{}]",
                    //     unused_obj, size, unused_size
                    // );
                    return unused_obj;
                }
                std::cmp::Ordering::Greater => {
                    let ptr = unsafe { (unused_obj as *mut u8).add(unused_size - size) };
                    let new_obj = BigObj::new(ptr, size);
                    unsafe {
                        (*unused_obj).size -= size;
                    }
                    // self.mmap.commit(new_obj as *mut BigObj as *mut u8, size);
                    // println!("get_chunk: {:p}[reused {}/{}]", new_obj, size, unused_size);
                    return new_obj;
                }
            };
        }

        let chunk = self.alloc_chunk(size).unwrap();
        unsafe { self.current = self.current.add(size) };
        println!("get_chunk: {:p}[new {}]", chunk, size);
        chunk
    }

    pub fn return_chunk(&mut self, obj: *mut BigObj) {
        let _lock = self.lock.lock();
        let size = unsafe { (*obj).size };
        println!("ret_chunk: {:p}[size {}]", obj, size);
        let mut merged = false;
        // 合并相邻free_obj
        for i in 0..self.unused_chunks.len() {
            let unused_obj = self.unused_chunks[i];
            let unused_obj_ptr = unused_obj as *mut u8;
            if unsafe { unused_obj_ptr.sub(size) } == obj as *mut u8 {
                // |    return_obj    |  unused_obj  |
                // after
                // |    return_obj                   |
                unsafe {
                    (*obj).size += (*unused_obj).size;
                }
                self.unused_chunks.remove(i);
                self.unused_chunks.push(obj);
                println!("merge_chunks: {:p} {:p}", obj, unused_obj);
                merged = true;
            } else if unsafe { unused_obj_ptr.add(size) } == obj as *mut u8 {
                // |  unused_obj  |    return_obj    |
                // after
                // |  unused_obj                     |
                unsafe {
                    (*unused_obj).size += size;
                }
                println!("merge_chunks: {:p} {:p}", unused_obj, obj);
                merged = true;
            }
        }
        if !merged {
            self.unused_chunks.push(obj);
        }
    }

    /// # in_heap
    ///
    /// Check if a pointer is in the heap.
    pub fn in_heap(&self, ptr: *mut u8) -> bool {
        ptr >= self.heap_start && ptr < self.heap_end
    }
}
