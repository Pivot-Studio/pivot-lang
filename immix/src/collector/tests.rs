use std::{mem::size_of, ptr::null_mut, thread::sleep, time::Duration};

use lazy_static::lazy_static;
use parking_lot::Mutex;
use rand::random;
use rustc_hash::FxHashSet;

use crate::{gc_disable_auto_collect, no_gc_thread, round_n_up, BIG_OBJ_ALIGN, BLOCK_SIZE, SPACE};

use super::*;

const LINE_SIZE_OBJ: usize = 1;

#[repr(C)]
struct GCTestObj {
    _vtable: VtableFunc,
    b: *mut GCTestObj,
    c: u64,
    // _arr: [u8; 128],
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

#[repr(C)]
struct GCTestBigObj {
    _vtable: VtableFunc,
    b: *mut GCTestBigObj,
    _arr: [u8; BLOCK_SIZE],
    d: *mut GCTestBigObj,
}
const BIGOBJ_ALLOC_SIZE: usize = round_n_up!(size_of::<GCTestBigObj>() + 16, 128);
extern "C" fn gctest_vtable_big(
    ptr: *mut u8,
    gc: &Collector,
    mark_ptr: VisitFunc,
    _mark_complex: VisitFunc,
    _mark_trait: VisitFunc,
) {
    let obj = ptr as *mut GCTestBigObj;
    unsafe {
        mark_ptr(gc, (&mut (*obj).b) as *mut *mut GCTestBigObj as *mut u8);
        mark_ptr(gc, (&mut (*obj).d) as *mut *mut GCTestBigObj as *mut u8);
    }
}
#[test]
fn test_basic_multiple_thread_gc() {
    ENABLE_EVA.store(false, Ordering::SeqCst);
    let _lock = THE_RESOURCE.lock();
    let mut handles = vec![];
    gc_disable_auto_collect();
    no_gc_thread();
    for _ in 0..10 {
        let t = std::thread::spawn(|| {
            SPACE.with(|gc| unsafe {
                let mut gc = gc.borrow_mut();
                println!("thread1 gcid = {}", gc.get_id());
                let mut a = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
                let rustptr = (&mut a) as *mut *mut GCTestObj as *mut u8;
                (*a).b = null_mut();
                (*a).c = 1;
                (*a).d = null_mut();
                (*a).e = null_mut();
                (*a)._vtable = gctest_vtable;
                gc.add_root(rustptr, ObjectType::Pointer);
                let b = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
                gc.print_stats();
                (*a).b = b;
                (*b)._vtable = gctest_vtable;
                (*b).c = 2;
                (*b).d = null_mut();
                (*b).e = null_mut();
                (*b).b = null_mut();
                let size1 = gc.get_size();
                let time = std::time::Instant::now();
                gc.collect();
                println!("gc{} gc time = {:?}", gc.get_id(), time.elapsed());
                let size2 = gc.get_size();
                assert_eq!(size2, LINE_SIZE_OBJ * 2, "gc {}", gc.get_id());
                assert_eq!(size1, size2, "gc {}", gc.get_id());
                let d = gc.alloc(size_of::<u64>(), ObjectType::Atomic) as *mut u64;
                (*b).d = d;
                (*d) = 3;
                gc.collect();
                let size3 = gc.get_size();
                assert_eq!(size3, LINE_SIZE_OBJ * 3);
                assert!(size3 > size2, "gc {}", gc.get_id());
                (*a).d = d;
                gc.collect();
                let size4 = gc.get_size();
                assert_eq!(size3, size4, "gc {}", gc.get_id());
                (*a).b = a;
                gc.collect();
                let size5 = gc.get_size();
                assert!(size5 < size4);
                (*a).d = core::ptr::null_mut();
                gc.collect();
                let size6 = gc.get_size();
                assert!(size5 > size6);
                gc.remove_root(rustptr);
                gc.collect();
                let size7 = gc.get_size();
                assert_eq!(0, size7);
                let mut a = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
                (*a).b = null_mut();
                (*a).c = 1;
                (*a).d = null_mut();
                (*a).e = null_mut();
                (*a)._vtable = gctest_vtable;
                let rustptr = (&mut a) as *mut *mut GCTestObj as *mut u8;
                gc.add_root(rustptr, ObjectType::Pointer);
                let b = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
                (*a).b = b;
                (*a).c = 1;
                (*a)._vtable = gctest_vtable;
                (*b)._vtable = gctest_vtable;
                (*b).c = 2;
                (*b).d = null_mut();
                (*b).e = null_mut();
                (*b).b = null_mut();
                let size1 = gc.get_size();
                assert_eq!(size1, 2 * LINE_SIZE_OBJ);
                // drop(gc);
                // no_gc_thread();
            });
        });
        handles.push(t);
    }
    for h in handles {
        h.join().unwrap();
    }
}

#[test]
fn test_big_obj_gc() {
    gc_disable_auto_collect();
    let _lock = THE_RESOURCE.lock();
    SPACE.with(|gc| unsafe {
        let mut gc = gc.borrow_mut();
        println!("thread1 gcid = {}", gc.get_id());
        let mut a = gc.alloc(size_of::<GCTestBigObj>(), ObjectType::Complex) as *mut GCTestBigObj;
        let b = gc.alloc(size_of::<GCTestBigObj>(), ObjectType::Complex) as *mut GCTestBigObj;
        (*a).b = b;
        (*a).d = null_mut();
        (*a)._vtable = gctest_vtable_big;
        (*b)._vtable = gctest_vtable_big;
        let rustptr = (&mut a) as *mut *mut GCTestBigObj as *mut u8;
        gc.add_root(rustptr, ObjectType::Pointer);
        let size1 = gc.get_bigobjs_size();
        assert_eq!(size1, round_n_up!(2 * BIGOBJ_ALLOC_SIZE, BIG_OBJ_ALIGN));
        let time = std::time::Instant::now();
        gc.collect(); // 不回收，剩余 a b
        println!("gc{} gc time = {:?}", gc.get_id(), time.elapsed());
        let size2 = gc.get_bigobjs_size();
        assert_eq!(size1, size2);
        (*a).b = a;
        gc.collect(); // 回收，剩余 a
        let size3 = gc.get_bigobjs_size();
        assert!(size3 < size2);
        gc.remove_root(rustptr);

        gc.collect(); // 回收，不剩余
        let size4 = gc.get_bigobjs_size();
        assert_eq!(0, size4);
        let mut a = gc.alloc(size_of::<GCTestBigObj>(), ObjectType::Complex) as *mut GCTestBigObj;
        let b = gc.alloc(size_of::<GCTestBigObj>(), ObjectType::Complex) as *mut GCTestBigObj;
        let c = gc.alloc(size_of::<GCTestBigObj>(), ObjectType::Complex) as *mut GCTestBigObj;
        (*a).b = b;
        (*a).d = c;
        (*a)._vtable = gctest_vtable_big;
        (*b)._vtable = gctest_vtable_big;
        (*c)._vtable = gctest_vtable_big;
        let rustptr = (&mut a) as *mut *mut GCTestBigObj as *mut u8;
        gc.add_root(rustptr, ObjectType::Pointer);
        //  32896       32896       32896
        // |  b  | <-- |  a  | --> |  c  |
        //               ^ rustptr
        let size1 = gc.get_bigobjs_size();
        assert_eq!(size1, round_n_up!(3 * BIGOBJ_ALLOC_SIZE, BIG_OBJ_ALIGN));
        (*a).b = null_mut();
        (*a).d = null_mut();
        gc.collect(); // 回收，剩余 a
        gc.remove_root(rustptr);
        gc.collect(); // 回收，不剩余, b a merge，a c merge。
    });
}

unsafe fn alloc_test_obj(gc: &mut Collector) -> *mut GCTestObj {
    let a = gc.alloc(size_of::<GCTestObj>(), ObjectType::Complex) as *mut GCTestObj;
    a.write(GCTestObj {
        _vtable: gctest_vtable,
        b: null_mut(),
        c: 0,
        d: null_mut(),
        e: null_mut(),
    });
    a
}

lazy_static! {
    static ref TEST_OBJ_QUEUE: Mutex<Vec<TestObjWrap>> = Mutex::new(vec![]);
}
#[derive(Debug, PartialEq, Eq, Hash)]
struct TestObjWrap(*mut *mut GCTestObj);

unsafe impl Send for TestObjWrap {}
unsafe impl Sync for TestObjWrap {}

struct SingleThreadResult {
    size1: usize,
    expect_size1: usize,
    size2: usize,
    expect_size2: usize,
    size3: usize,
    expect_size3: usize,
    expect_objs: usize,
    set: FxHashSet<TestObjWrap>,
    set2: FxHashSet<TestObjWrap>,
}

fn single_thread(obj_num: usize) -> SingleThreadResult {
    gc_disable_auto_collect();
    // let _lock = THE_RESOURCE.lock();
    SPACE.with(|gc| unsafe {
        // 睡眠一秒保证所有线程创建
        let mut gc = gc.borrow_mut();
        println!("thread1 gcid = {}", gc.get_id());
        sleep(Duration::from_secs(1));
        let mut first_obj = alloc_test_obj(&mut gc);
        println!("first_obj = {:p}", first_obj);
        let rustptr = (&mut first_obj) as *mut *mut GCTestObj as *mut u8;
        gc.add_root(rustptr, ObjectType::Pointer);
        println!(
            "gcid = {} rustptr point to {:p}",
            gc.get_id(),
            *(rustptr as *mut *mut GCTestObj)
        );
        let mut live_obj = 1;
        TEST_OBJ_QUEUE
            .lock()
            .push(TestObjWrap(&mut (*first_obj).b as *mut *mut GCTestObj));
        TEST_OBJ_QUEUE
            .lock()
            .push(TestObjWrap(&mut (*first_obj).e as *mut *mut GCTestObj));
        // let mut unused_objs = vec![&mut (*first_obj).b, &mut (*first_obj).e];
        let mut has_loop = false;
        for _ in 0..obj_num {
            let obj = alloc_test_obj(&mut gc);
            if random() {
                live_obj += 1;
                let father_ptr = TEST_OBJ_QUEUE.lock().pop().unwrap().0;
                *father_ptr = obj;
                if random() && random() && !has_loop {
                    // 循环引用
                    (*obj).b = first_obj;
                    println!("live_obj = {} has loop", live_obj);
                    has_loop = true;
                } else {
                    TEST_OBJ_QUEUE
                        .lock()
                        .push(TestObjWrap(&mut (*obj).b as *mut *mut GCTestObj));
                }
                TEST_OBJ_QUEUE
                    .lock()
                    .push(TestObjWrap(&mut (*obj).e as *mut *mut GCTestObj));
            }
        }
        let size1 = gc.get_size();

        let mut set2 = FxHashSet::default();
        let mut ii = 0;
        gc.iter(|ptr| {
            ii += 1;
            // if set2.contains(&TestObjWrap(ptr as *mut *mut GCTestObj)) {
            //     panic!("repeat ptr = {:p}", ptr);
            // }
            set2.insert(TestObjWrap(
                Block::from_obj_ptr(ptr) as *mut Block as *mut *mut GCTestObj
            ));
        });
        println!("gc{} itered ", gc.get_id());

        // assert_eq!(size1, 1001 * LINE_SIZE_OBJ);
        let time = std::time::Instant::now();
        gc.collect();
        println!("gc{} gc time = {:?}", gc.get_id(), time.elapsed());
        let size2 = gc.get_size();

        // assert_eq!(live_obj * LINE_SIZE_OBJ, size2);
        gc.collect();
        let size3 = gc.get_size();
        // assert_eq!(ii, size3);
        // assert_eq!(set2.len(), size3);
        let first_obj = *(rustptr as *mut *mut GCTestObj);
        println!("new first_obj = {:p}", first_obj);
        let mut set = FxHashSet::default();
        let objs = walk_obj(first_obj, &mut set);
        // assert_eq!(objs, live_obj);
        assert_eq!(objs, set.len());
        gc.remove_root(rustptr);
        gc.collect();
        // evacuation might be triggered, so it mat not be zero
        // let size4 = gc.get_size();
        // assert_eq!(size4, 0);
        SingleThreadResult {
            size1,
            expect_size1: (obj_num + 1) * LINE_SIZE_OBJ,
            size2,
            expect_size2: live_obj * LINE_SIZE_OBJ,
            size3,
            expect_size3: size2,
            set,
            expect_objs: live_obj,
            set2,
        }
    })
}

lazy_static! {
    /// gc测试不能并发进行，需要加锁
    static ref THE_RESOURCE: Mutex<()> = Mutex::new(());
}

#[test]
fn test_complecated_single_thread_gc() {
    gc_disable_auto_collect();
    // 这个测试和test_complecated_multiple_thread_gc不能同时跑，用了全局变量会相互干扰
    let _lock = THE_RESOURCE.lock();
    TEST_OBJ_QUEUE.lock().clear();
    let re = single_thread(1000);
    no_gc_thread();
    assert_eq!(re.size1, re.expect_size1);
    assert_eq!(re.size2, re.expect_size2);
    assert_eq!(re.size3, re.size2);
    assert_eq!(re.size3, re.expect_size3);
    assert_eq!(re.set.len(), re.expect_objs);
}

fn walk_obj(obj: *mut GCTestObj, set: &mut FxHashSet<TestObjWrap>) -> usize {
    unsafe {
        let b = Block::from_obj_ptr(obj as *mut u8);
        let (h, _) = b.get_line_header_from_addr(obj as *mut u8);
        assert!(h.get_used());
    }
    if set.contains(&TestObjWrap(obj as *mut *mut GCTestObj)) {
        return 0;
    }
    let mut count = 0;
    set.insert(TestObjWrap(obj as *mut *mut GCTestObj));
    unsafe {
        if !(*obj).b.is_null() {
            count += walk_obj((*obj).b, set);
        }
        if !(*obj).e.is_null() {
            count += walk_obj((*obj).e, set);
        }
    }
    count + 1
}
#[test]
fn test_complecated_multiple_thread_gc() {
    gc_disable_auto_collect();
    let _lock = THE_RESOURCE.lock();
    TEST_OBJ_QUEUE.lock().clear();
    let mut handles = vec![];
    for _ in 0..10 {
        let t = std::thread::Builder::new()
            .spawn(|| single_thread(1000))
            .unwrap();
        handles.push(t);
    }
    let mut total_size1 = 0;
    let mut total_target_size1 = 0;
    let mut total_size2 = 0;
    let mut total_target_size2 = 0;
    let mut total_size3 = 0;
    let mut total_target_size3 = 0;
    let mut total_target_objs = 0;
    let mut set = FxHashSet::default();
    let mut set2 = FxHashSet::default();
    for h in handles {
        let mut re = h.join().unwrap();
        total_size1 += re.size1;
        total_target_size1 += re.expect_size1;
        total_size2 += re.size2;
        total_target_size2 += re.expect_size2;
        total_size3 += re.size3;
        total_target_size3 += re.expect_size3;
        set.extend(re.set);
        for s in re.set2.drain() {
            if set2.contains(&s) {
                println!("repeat ptr = {:p}", s.0);
            }
            set2.insert(s);
        }
        total_target_objs += re.expect_objs;
    }
    assert_eq!(total_size1, total_target_size1);
    assert_eq!(total_size2, total_target_size2);
    assert_eq!(total_size3, total_size2);
    assert_eq!(total_size3, total_target_size3);
    for k in set2.iter() {
        if !set.contains(k) {
            println!("set2 not in set {:p}", k.0);
        }
    }
    println!("set2 len {}", set2.len());
    println!("size2 {} size3 {}", total_size2, total_size3);
    assert_eq!(set.len(), total_target_objs);
}
