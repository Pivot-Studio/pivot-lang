use std::{mem::size_of, ops::Add, time::Duration};

use criterion::{criterion_group, criterion_main, Criterion};
use immix::*;
use rand::random;

fn immix_benchmark_multi_thread(c: &mut Criterion) {
    c.bench_function(
        &format!(
            "multithreads({}) gc benchmark--10000 small objects",
            THREADS
        ),
        |b| {
            b.iter_custom(|t| {
                let mut total = Duration::new(0, 0);
                for _ in 0..t {
                    total = total.add(test_complecated_multiple_thread_gc());
                }
                total
            });
        },
    );
}
fn immix_benchmark_single_thread(c: &mut Criterion) {
    c.bench_function(
        &format!("singlethread gc benchmark--10000 small objects"),
        |b| {
            b.iter_custom(|t| {
                let mut total = Duration::new(0, 0);
                for _ in 0..t {
                    total = total.add(test_complecated_single_thread_gc());
                }
                total
            });
        },
    );
}

struct GCTestObj {
    _vtable: VtableFunc,
    b: *mut GCTestObj,
    d: *mut u64,
    e: *mut GCTestObj,
}
fn gctest_vtable(
    ptr: *mut u8,
    gc: &Collector,
    mark_ptr: VisitFunc,
    _mark_complex: VisitFunc,
    _mark_trait: VisitFunc,
) {
    let obj = ptr as *mut GCTestObj;
    unsafe {
        mark_ptr(gc, (*obj).b as *mut u8);
        mark_ptr(gc, (*obj).d as *mut u8);
        mark_ptr(gc, (*obj).e as *mut u8);
    }
}

unsafe fn alloc_test_obj(gc: &mut Collector) -> *mut GCTestObj {
    let a = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
    (*a)._vtable = gctest_vtable;
    a
}

fn test_complecated_single_thread_gc() -> Duration {
    SPACE.with(|gc| unsafe {
        let mut gc = gc.borrow_mut();
        // println!("thread1 gcid = {}", gc.get_id());
        let mut first_obj = alloc_test_obj(&mut gc);
        let rustptr = (&mut first_obj) as *mut *mut GCTestObj as *mut u8;
        let mut live_obj = 1;
        let mut unused_objs = vec![&mut (*first_obj).b, &mut (*first_obj).e];
        for _ in 0..10000 {
            let obj = alloc_test_obj(&mut gc);
            if random() {
                live_obj += 1;
                let father_ptr = unused_objs.pop().unwrap();
                *father_ptr = obj;
                unused_objs.push(&mut (*obj).b);
                unused_objs.push(&mut (*obj).e);
            }
        }
        gc.add_root(rustptr, ObjectType::Atomic);
        let size1 = gc.get_size();
        assert_eq!(size1, 10001);
        let time = std::time::Instant::now();
        gc.collect();
        let ctime = time.elapsed();
        // println!("gc{} gc time = {:?}", gc.get_id(), ctime);
        let size2 = gc.get_size();
        assert_eq!(live_obj, size2);
        gc.remove_root(rustptr);
        gc.collect();
        let size3 = gc.get_size();
        assert_eq!(size3, 0);
        ctime
    })
}

static THREADS: usize = 8;
fn test_complecated_multiple_thread_gc() -> Duration {
    let mut handles = vec![];
    for _ in 0..THREADS {
        let t = std::thread::spawn(|| test_complecated_single_thread_gc());
        handles.push(t);
    }
    let mut total = Duration::new(0, 0);
    for h in handles {
        total = total.add(h.join().unwrap());
    }
    total / 10
}

criterion_group!(
    benches,
    immix_benchmark_multi_thread,
    immix_benchmark_single_thread
);
criterion_main!(benches);
