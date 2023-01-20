use std::{mem::size_of, thread::available_parallelism, time::Duration};

use criterion::{criterion_group, criterion_main, Criterion};
use immix::*;
use rand::random;

fn immix_benchmark_multi_thread(c: &mut Criterion) {
    bench_n_threads(get_threads())(c);
}

fn bench_n_threads(n: usize) -> impl Fn(&mut Criterion) {
    move |c: &mut Criterion| {
        c.bench_function(
            &format!(
                "multithreads({}) gc benchmark--{} small objects",
                n, OBJ_NUM
            ),
            |b| {
                b.iter_custom(|t| {
                    let total = test_complecated_multiple_thread_gc(t as usize, n);
                    total
                });
            },
        );
    }
}

fn immix_benchmark_single_thread(c: &mut Criterion) {
    c.bench_function(
        &format!("singlethread gc benchmark--{} small objects", OBJ_NUM),
        |b| {
            b.iter_custom(|t| {
                let total = test_complecated_single_thread_gc(t as usize);
                total
            });
        },
    );
}

const OBJ_NUM: usize = 10000;

fn get_threads() -> usize {
    available_parallelism().unwrap().get()
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

fn test_complecated_single_thread_gc(num_iter: usize) -> Duration {
    let t = SPACE.with(|gc| unsafe {
        let mut gc = gc.borrow_mut();
        let mut total = Duration::new(0, 0);
        for _ in 0..num_iter {
            let mut first_obj = alloc_test_obj(&mut gc);
            let rustptr = (&mut first_obj) as *mut *mut GCTestObj as *mut u8;
            let mut live_obj = 1;
            let mut unused_objs = vec![&mut (*first_obj).b, &mut (*first_obj).e];
            for _ in 0..OBJ_NUM {
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
            assert_eq!(size1, OBJ_NUM + 1);
            let ctime = gc.collect();
            // println!("gc{} gc time = {:?}", gc.get_id(), ctime);
            let size2 = gc.get_size();
            assert_eq!(live_obj, size2);
            gc.remove_root(rustptr);
            gc.collect();
            let size3 = gc.get_size();
            assert_eq!(size3, 0);
            total += ctime;
        }
        total
    });
    t
}

fn test_complecated_multiple_thread_gc(num_iter: usize, threads: usize) -> Duration {
    let mut handles = vec![];
    for _ in 0..threads {
        let t = std::thread::spawn(move || test_complecated_single_thread_gc(num_iter));
        handles.push(t);
    }
    let mut times = vec![];
    for h in handles {
        times.push(h.join().unwrap());
    }
    times.sort();
    times.pop().unwrap()
}

criterion_group!(
    benches,
    immix_benchmark_multi_thread,
    immix_benchmark_single_thread
);
criterion_main!(benches);
