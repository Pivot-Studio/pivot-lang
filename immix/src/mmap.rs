#[cfg(windows)]
mod _win {
    use crate::round_n_up;
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
        /// Return a `align` aligned pointer to the mmap'ed region.
        pub fn aligned(&self, align: usize) -> *mut u8 {
            round_n_up!(self.start as usize, align) as *mut u8
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

        pub fn commit(&self, page: *mut u8, size: usize) -> bool {
            unsafe {
                VirtualAlloc(page.cast(), size, MEM_COMMIT, PAGE_READWRITE) == std::ptr::null_mut()
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
    use crate::round_n_up;
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
                let code = libc::madvise(map, size, libc::MADV_SEQUENTIAL);
                if map == libc::MAP_FAILED {
                    panic!("mmap failed, code: {}", code);
                }
                Self {
                    start: map as *mut u8,
                    end: (map as usize + size) as *mut u8,
                    size,
                }
            }
        }
        /// Return a `align` aligned pointer to the mmap'ed region.
        pub fn aligned(&self, align: usize) -> *mut u8 {
            round_n_up!(self.start as usize, align) as *mut u8
        }

        pub fn start(&self) -> *mut u8 {
            self.start
        }
        pub fn end(&self) -> *mut u8 {
            self.end
        }

        pub fn dontneed(&self, page: *mut u8, size: usize) {
            unsafe {
                #[cfg(all(target_os = "linux", feature = "madv_free"))]
                libc::madvise(page as *mut _, size as _, libc::MADV_FREE);
                #[cfg(all(target_os = "macos", feature = "madv_free"))]
                libc::madvise(page as *mut _, size as _, libc::MADV_FREE_REUSE);
                #[cfg(feature = "madv_dontneed")]
                libc::madvise(page as *mut _, size as _, libc::MADV_DONTNEED);
            }
        }

        pub fn commit(&self, page: *mut u8, size: usize) -> bool {
            unsafe {
                libc::madvise(
                    page as *mut _,
                    size as _,
                    libc::MADV_WILLNEED | libc::MADV_SEQUENTIAL,
                ) == 0
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
