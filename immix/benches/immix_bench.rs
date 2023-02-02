use std::{mem::size_of, ptr::null_mut, thread::available_parallelism, time::Duration};

use criterion::{criterion_group, criterion_main, Criterion};
use immix::*;
use libc::malloc;
use rand::random;
extern crate bindeps;

fn immix_benchmark_multi_thread(c: &mut Criterion) {
    bench_n_threads(get_threads())(c);
}

fn bench_n_threads(n: usize) -> impl Fn(&mut Criterion) {
    move |c: &mut Criterion| {
        c.bench_function(
            &format!(
                "multithreads({}) gc benchmark--{} small objects(per thread)",
                n, OBJ_NUM
            ),
            |b| {
                b.iter_custom(|t| {
                    let total = test_complecated_multiple_thread_gc(t as usize, n);
                    total.0 + total.1
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
                total.0 + total.1
            });
        },
    );
}

fn immix_benchmark_single_thread_mark(c: &mut Criterion) {
    c.bench_function(
        &format!("singlethread gc mark benchmark--{} small objects", OBJ_NUM),
        |b| {
            b.iter_custom(|t| {
                let total = test_complecated_single_thread_gc(t as usize);
                total.0
            });
        },
    );
}
fn immix_benchmark_single_thread_sweep(c: &mut Criterion) {
    c.bench_function(
        &format!("singlethread gc sweep benchmark--{} small objects", OBJ_NUM),
        |b| {
            b.iter_custom(|t| {
                let total = test_complecated_single_thread_gc(t as usize);
                total.1
            });
        },
    );
}

fn immix_benchmark_single_thread_alloc(c: &mut Criterion) {
    let mut g = c.benchmark_group("allocation bench");
    if let None = option_env!("PL_IMMIX_HEAP_SIZE") {
        return;
    }
    g.bench_function(
        &format!("singlethread gc alloc benchmark small objects"),
        |b| b.iter(bench_allocation),
    );
    g.bench_function(&format!("malloc benchmark small objects"), |b| {
        b.iter(bench_malloc)
    });
}

const OBJ_NUM: usize = 65535;

fn get_threads() -> usize {
    available_parallelism().unwrap().get()
}

#[repr(C)]
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
        mark_ptr(gc, (&mut (*obj).b) as *mut *mut GCTestObj as *mut u8);
        mark_ptr(gc, (&mut (*obj).d) as *mut *mut u64 as *mut u8);
        mark_ptr(gc, (&mut (*obj).e) as *mut *mut GCTestObj as *mut u8);
    }
}

unsafe fn alloc_test_obj(gc: &mut Collector) -> *mut GCTestObj {
    let a = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
    a.write(GCTestObj {
        _vtable: gctest_vtable,
        b: null_mut(),
        d: null_mut(),
        e: null_mut(),
    });
    a
}

fn test_complecated_single_thread_gc(num_iter: usize) -> (Duration, Duration) {
    #[cfg(feature = "shadow_stack")]
    {
        let t = SPACE.with(|gc| unsafe {
            let mut gc = gc.borrow_mut();
            let mut total_mark = Duration::new(0, 0);
            let mut total_sweep = Duration::new(0, 0);
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
                gc.add_root(rustptr, ObjectType::Pointer);
                let size1 = gc.get_size();
                assert_eq!(size1, OBJ_NUM + 1);
                let ctime = gc.collect();
                // println!("gc{} gc time = {:?}", gc.get_id(), ctime);
                let size2 = gc.get_size();
                assert_eq!(live_obj, size2);
                // let ctime = gc.collect();
                gc.remove_root(rustptr);
                gc.collect();
                let size3 = gc.get_size();
                assert_eq!(size3, 0);
                total_mark += ctime.0;
                total_sweep += ctime.1;
            }
            (total_mark, total_sweep)
        });
        t
    }
    eprintln!("shadow stack is not enabled, skip the bench");
    (Duration::new(0, 100), Duration::new(0, 100))
}

fn test_complecated_multiple_thread_gc(num_iter: usize, threads: usize) -> (Duration, Duration) {
    let mut handles = vec![];
    for _ in 0..threads {
        let t = std::thread::spawn(move || test_complecated_single_thread_gc(num_iter));
        handles.push(t);
    }
    let mut times = vec![];
    for h in handles {
        times.push(h.join().unwrap());
    }
    times.sort_by(|k1, k2| (k1.0 + k1.1).cmp(&(k2.0 + k2.1)));
    times.pop().unwrap()
}

fn bench_allocation() -> *mut GCTestObj {
    SPACE.with(|gc| {
        let gc = gc.borrow();
        gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj
    })
}

#[inline(never)]
fn bench_malloc() -> *mut GCTestObj {
    unsafe { malloc(size_of::<GCTestObj>()) as *mut GCTestObj }
}

criterion_group!(
    benches,
    immix_benchmark_multi_thread,
    immix_benchmark_single_thread,
    immix_benchmark_single_thread_mark,
    immix_benchmark_single_thread_sweep,
    immix_benchmark_single_thread_alloc,
);
criterion_main!(benches);
