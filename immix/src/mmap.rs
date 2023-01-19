#[cfg(windows)]
mod _win {
    use crate::consts::*;
    use core::{ptr::null_mut, usize};

    use winapi::um::{
        memoryapi::{VirtualAlloc, VirtualFree},
        winnt::{MEM_COMMIT, MEM_DECOMMIT, MEM_RELEASE, MEM_RESERVE, PAGE_READWRITE},
    };
    pub struct Mmap {
        start: *mut u8,
        end: *mut u8,
        size: usize,
    }
    impl Mmap {
        pub fn new(size: usize) -> Self {
            unsafe {
                let mem = VirtualAlloc(null_mut(), size, MEM_RESERVE, PAGE_READWRITE);
                let mem = mem as *mut u8;

                let end = mem.add(size);

                Self {
                    start: mem,
                    end,
                    size,
                }
            }
        }
        /// Return a `BLOCK_SIZE` aligned pointer to the mmap'ed region.
        pub fn aligned(&self) -> *mut u8 {
            let offset = BLOCK_SIZE - (self.start as usize) % BLOCK_SIZE;
            unsafe { self.start.add(offset) as *mut u8 }
        }

        // pub fn start(&self) -> *mut u8 {
        //     self.start
        // }
        pub fn end(&self) -> *mut u8 {
            self.end
        }

        pub fn dontneed(&self, page: *mut u8, size: usize) {
            unsafe {
                //DiscardVirtualMemory(page.cast(), size as _);
                VirtualFree(page.cast(), size, MEM_DECOMMIT);
            }
        }

        pub fn commit(&self, page: *mut u8, size: usize) {
            unsafe {
                VirtualAlloc(page.cast(), size, MEM_COMMIT, PAGE_READWRITE);
            }
        }
    }

    impl Drop for Mmap {
        fn drop(&mut self) {
            unsafe {
                VirtualFree(self.start.cast(), self.size, MEM_RELEASE);
            }
        }
    }
}

#[cfg(unix)]
mod _unix {
    use crate::consts::*;
    pub struct Mmap {
        start: *mut u8,
        end: *mut u8,
        size: usize,
    }

    impl Mmap {
        pub fn new(size: usize) -> Self {
            unsafe {
                let map = libc::mmap(
                    core::ptr::null_mut(),
                    size as _,
                    libc::PROT_READ | libc::PROT_WRITE,
                    libc::MAP_PRIVATE | libc::MAP_ANON,
                    -1,
                    0,
                );
                libc::madvise(map, size, libc::MADV_SEQUENTIAL);
                if map == libc::MAP_FAILED {
                    panic!("mmap failed");
                }
                Self {
                    start: map as *mut u8,
                    end: (map as usize + size) as *mut u8,
                    size,
                }
            }
        }
        /// Return a `BLOCK_SIZE` aligned pointer to the mmap'ed region.
        pub fn aligned(&self) -> *mut u8 {
            let offset = BLOCK_SIZE - (self.start as usize) % BLOCK_SIZE;
            unsafe { self.start.offset(offset as isize) as *mut u8 }
        }

        pub fn start(&self) -> *mut u8 {
            self.start
        }
        pub fn end(&self) -> *mut u8 {
            self.end
        }

        pub fn dontneed(&self, page: *mut u8, size: usize) {
            unsafe {
                libc::madvise(page as *mut _, size as _, libc::MADV_DONTNEED);
            }
        }

        pub fn commit(&self, page: *mut u8, size: usize) {
            unsafe {
                libc::madvise(
                    page as *mut _,
                    size as _,
                    libc::MADV_WILLNEED | libc::MADV_SEQUENTIAL,
                );
            }
        }
    }

    impl Drop for Mmap {
        fn drop(&mut self) {
            unsafe {
                libc::munmap(self.start() as *mut _, self.size as _);
            }
        }
    }
}

#[cfg(unix)]
pub use _unix::*;
#[cfg(windows)]
pub use _win::*;
