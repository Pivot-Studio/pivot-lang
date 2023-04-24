use std::{mem::size_of, thread::available_parallelism, time::Duration};

use criterion::{criterion_group, criterion_main, Criterion};
use immix::*;

fn immix_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("plimmixgc");
    group.sample_size(10);
    gc_enable_auto_collect();
    // 这个测试中并没有记录所有的gcroot，如果启用evacuation
    // 可能会导致部分指针驱逐后不自愈
    set_evacuation(false);
    #[cfg(feature = "shadow_stack")]
    {
        group.bench_function(
            &"singlethread gc stress benchmark small objects".to_string(),
            |b| {
                b.iter_custom(|i| {
                    let mut total = Duration::new(0, 0);
                    for _ in 0..i {
                        total += SPACE.with(|space| {
                            let mut space = space.borrow_mut();

                            // t.elapsed()
                            gcbench(&mut space)
                        });
                    }
                    total
                });
            },
        );
        no_gc_thread();
        group.bench_function("multi-thread gc stress benchmark small objects", |b| {
            b.iter(|| {
                let mut threads = Vec::with_capacity(4);
                for _ in 0..get_threads() {
                    threads.push(std::thread::spawn(move || {
                        SPACE.with(|space| {
                            let mut space = space.borrow_mut();

                            // t.elapsed()
                            gcbench(&mut space)
                        })
                    }));
                }

                while let Some(th) = threads.pop() {
                    th.join().unwrap();
                }
            });
        });
    }
}

#[repr(C)]
struct GCTestObj {
    _vtable: VtableFunc,
    b: *mut GCTestObj,
    d: *mut u64,
    e: *mut GCTestObj,
}
extern "C" fn gctest_vtable(
    ptr: *mut u8,
    gc: &Collector,
    mark_ptr: VisitFunc,
    _mark_complex: VisitFunc,
    _mark_trait: VisitFunc,
) {
    let obj = ptr as *mut GCTestObj;
    unsafe {
        mark_ptr(gc, (&mut (*obj).b) as *mut *mut GCTestObj as *mut u8);
        mark_ptr(gc, (&mut (*obj).d) as *mut *mut u64 as *mut u8);
        mark_ptr(gc, (&mut (*obj).e) as *mut *mut GCTestObj as *mut u8);
    }
}

unsafe fn alloc_test_obj(gc: &mut Collector) -> *mut GCTestObj {
    let a = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
    a.write(GCTestObj {
        _vtable: gctest_vtable,
        b: std::ptr::null_mut(),
        d: std::ptr::null_mut(),
        e: std::ptr::null_mut(),
    });
    a
}
#[cfg(feature = "shadow_stack")]
fn gcbench(space: &mut Collector) -> Duration {
    unsafe {
        let t = std::time::Instant::now();
        let mut long_lived = alloc_test_obj(space);
        let rustptr = (&mut long_lived) as *mut *mut GCTestObj as *mut u8;
        space.add_root(rustptr, ObjectType::Pointer);
        populate(K_LONG_LIVED_TREE_DEPTH, long_lived, space);
        let mut d = K_MIN_TREE_DEPTH;
        while d <= K_MAX_TREE_DEPTH {
            time_construction(d, space);
            d += 2;
        }
        space.remove_root(rustptr);

        t.elapsed()
    }
}

fn tree_size(i: i32) -> i32 {
    (1 << (i + 1)) - 1
}

fn num_iters(i: i32) -> i32 {
    2 * tree_size(K_STRETCH_TREE_DEPTH) / tree_size(i)
}
#[cfg(feature = "shadow_stack")]
unsafe fn populate(idepth: i32, thisnode: *mut GCTestObj, space: &mut Collector) {
    if idepth <= 0 {
        return;
    }
    (*thisnode).b = alloc_test_obj(space);
    (*thisnode).e = alloc_test_obj(space);
    populate(idepth - 1, (*thisnode).e, space);
    populate(idepth - 1, (*thisnode).b, space);
}
#[cfg(feature = "shadow_stack")]
unsafe fn make_tree(idepth: i32, space: &mut Collector) -> *mut GCTestObj {
    if idepth <= 0 {
        alloc_test_obj(space)
    } else {
        let mut left = make_tree(idepth - 1, space);
        let rustptr1 = (&mut left) as *mut *mut GCTestObj as *mut u8;
        space.add_root(rustptr1, ObjectType::Pointer);
        let mut right = make_tree(idepth - 1, space);
        let rustptr2 = (&mut right) as *mut *mut GCTestObj as *mut u8;
        space.add_root(rustptr2, ObjectType::Pointer);
        let mut result = alloc_test_obj(space);
        let rustptr3 = (&mut result) as *mut *mut GCTestObj as *mut u8;
        space.add_root(rustptr3, ObjectType::Pointer);
        (*result).b = left;
        (*result).e = right;
        space.remove_root(rustptr2);
        space.remove_root(rustptr1);
        space.remove_root(rustptr3);
        result
    }
}
#[inline(never)]
#[cfg(feature = "shadow_stack")]
unsafe fn time_construction(depth: i32, space: &mut Collector) {
    let i_num_iters = num_iters(depth);

    for _ in 0..i_num_iters {
        let mut temp_tree = alloc_test_obj(space);
        let rustptr = (&mut temp_tree) as *mut *mut GCTestObj as *mut u8;
        space.add_root(rustptr, ObjectType::Pointer);
        populate(depth, temp_tree, space);
        space.remove_root(rustptr);

        // destroy tempTree
    }

    for _ in 0..i_num_iters {
        let _temp_tree = make_tree(depth, space);
    }
}
const K_STRETCH_TREE_DEPTH: i32 = 18;
const K_LONG_LIVED_TREE_DEPTH: i32 = 16;
// const K_ARRAY_SIZE: i32 = 500000;
const K_MIN_TREE_DEPTH: i32 = 4;
const K_MAX_TREE_DEPTH: i32 = 16;

fn get_threads() -> usize {
    available_parallelism().unwrap().get()
}

criterion_group!(benches, immix_benchmark,);
criterion_main!(benches);
