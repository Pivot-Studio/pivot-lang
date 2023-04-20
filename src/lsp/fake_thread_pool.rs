//! A fake thread pool that executes jobs immediately on the current thread.
//!
//! This is used for wasm32 targets where threads are not supported.

pub(crate) struct ThreadPool {}

impl ThreadPool {
    pub fn new(n: usize) -> Self {
        Self {}
    }

    pub fn execute<F>(&self, job: F)
    where
        F: FnOnce() + Send + 'static,
    {
        job();
    }
}
