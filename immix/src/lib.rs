use std::cell::RefCell;

use collector::Collector;

mod allocator;
mod block;
mod collector;
mod consts;
mod mmap;

thread_local! {
    pub static SPACE: RefCell< Collector> = {
        let gc = Collector::new(1024 * 1024 * 1024);
        RefCell::new(gc)
    };
}
