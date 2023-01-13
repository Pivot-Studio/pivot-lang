use std::collections::VecDeque;

use vec_map::VecMap;

use crate::allocator::ThreadLocalAllocator;

/// # Collector
/// The collector is responsible for collecting garbage. It is the entry point for
/// the garbage collection process. It is also responsible for allocating new
/// blocks and for allocating new objects.
///
/// One thread has a collector associated with it. The collector is thread-local.
///
/// ## Fields
/// * `thread_local_allocator` - thread-local allocator
/// * `roots` - gc roots
/// * `queue` - gc queue
pub struct Collector {
    thread_local_allocator: ThreadLocalAllocator,
    roots: VecMap<()>,
    queue: VecDeque<*mut u8>,
}

impl Collector {
    /// # new
    /// Create a new collector.
    ///
    /// ## Parameters
    /// * `thread_local_allocator` - thread-local allocator
    pub fn new(thread_local_allocator: ThreadLocalAllocator) -> Self {
        Self {
            thread_local_allocator,
            roots: VecMap::new(),
            queue: VecDeque::new(),
        }
    }

    /// # add_root
    /// Add a root to the collector.
    ///
    /// ## Parameters
    /// * `root` - root
    pub fn add_root(&mut self, root: *mut u8) {
        self.roots.insert(root as usize, ());
    }

    /// # remove_root
    /// Remove a root from the collector.
    ///
    /// ## Parameters
    /// * `root` - root
    pub fn remove_root(&mut self, root: *mut u8) {
        self.roots.remove(root as usize);
    }

    /// # mark
    /// From gc roots, mark all reachable objects.
    pub fn mark(&mut self) {}
}
