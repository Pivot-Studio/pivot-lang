use criterion::{criterion_group, criterion_main, Criterion};

#[cfg(feature = "simple_gc")]
mod _bench {
    use libc::c_void;
    use rand::random;
    use std::{mem::size_of, time::Duration};
    use vm::gc::DioGC;

    struct GCTestObj {
        _vtable: fn(*mut u8, &DioGC),
        b: *mut GCTestObj,
        _d: *mut u64,
        e: *mut GCTestObj,
    }
    fn gctest_vtable(ptr: *mut u8, _gc: &DioGC) {
        let _obj = ptr as *mut GCTestObj;
    }

    unsafe fn alloc_test_obj(gc: &mut DioGC) -> *mut GCTestObj {
        let a = DioGC::malloc(size_of::<GCTestObj>() as i64, 0) as *mut GCTestObj;
        (*a)._vtable = gctest_vtable;
        a
    }
    pub fn simple_gc_bench() -> Duration {
        unsafe {
            let mut gc = DioGC::new();
            let mut first_obj = alloc_test_obj(&mut gc);
            let rustptr = (&mut first_obj) as *mut *mut GCTestObj as *mut u8;
            let mut unused_objs = vec![&mut (*first_obj).b, &mut (*first_obj).e];
            for _ in 0..10000 {
                let obj = alloc_test_obj(&mut gc);
                if random() {
                    let father_ptr = unused_objs.pop().unwrap();
                    *father_ptr = obj;
                    unused_objs.push(&mut (*obj).b);
                    unused_objs.push(&mut (*obj).e);
                }
            }
            DioGC::add_root(rustptr as *mut c_void, 0);
            let now = std::time::Instant::now();
            DioGC::collect();
            now.elapsed()
        }
    }
}

#[cfg(feature = "simple_gc")]
use _bench::simple_gc_bench;

fn criterion_benchmark(_c: &mut Criterion) {
    #[cfg(feature = "simple_gc")]
    _c.bench_function("simple gc bench--10000 small objects", |b| {
        b.iter_custom(|i| {
            use std::time::Duration;
            let mut sum = Duration::new(0, 0);
            for _ in 0..i {
                sum += simple_gc_bench();
            }
            sum
        })
    });
    #[cfg(not(feature = "simple_gc"))]
    println!("simple_gc feature is not enabled, skipping its benchmark");
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
