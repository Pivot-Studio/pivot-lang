use std::rc::Rc;

use int_enum::IntEnum;
use rustc_hash::FxHashMap;

use crate::ObjectType;

#[no_mangle]
pub fn print_stack_map(mapptr: *const u8) {
    let format_version = get_format_version(mapptr);
    println!("format_version: {}", format_version);
    let num_functions = get_num_functions(mapptr);
    println!("num_functions: {}", num_functions);
    let mut map = FxHashMap::default();
    build_root_maps(mapptr, &mut map);
    for (addr, func) in map.iter() {
        println!("addr: {:p}", *addr);
        println!("size: {}", func.root_size);
        println!("roots: {:?}", func.roots);
        println!("frame_size: {}", func.frame_size);
        println!("arg_num: {}", func.arg_num);
    }
}
// example stackmap:
// __GC_MAP_:
// 	.long	1                               ; plimmix stackmap format version
// 	.p2align	3
// 	.long	8                               ; function numbers
// 	.p2align	3
// 	.quad	_main                           ; function address
// 	.long	1                               ; live root count
// 	.long	8                               ; stack index (offset / wordsize)
// 	.p2align	3
// 	.quad	_fuck_it                        ; function address
// 	.long	0                               ; live root count
// 	.p2align	3
// 	.quad	_fuck_it_all                    ; function address
// 	.long	0                               ; live root count
// 	.p2align	3
// 	.quad	_ff                             ; function address
// 	.long	0                               ; live root count

pub(crate) fn get_format_version(mapptr: *const u8) -> i32 {
    unsafe {
        let ptr = mapptr as *const i32;
        *ptr
    }
}

fn get_num_functions(mapptr: *const u8) -> i32 {
    unsafe {
        let ptr = mapptr as *const i32;
        *ptr.offset(2)
    }
}

fn get_first_function_addr(mapptr: *const u8) -> *const u8 {
    unsafe {
        let ptr = mapptr as *const u8;
        ptr.offset(16)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub addr: *const u8,
    pub root_size: i32,
    pub roots: Vec<(i32, ObjectType)>, // offset / wordsize
    pub frame_size: i32,
    pub arg_num: i32,
    pub safe_points: Rc<Vec<*const u8>>,
}

impl Function {
    pub fn new(ptr: *const u8) -> (Function, *const u8) {
        let (frame_size, arg_num, root_size) = get_function_meta(ptr);
        // println!("frame_size: {}", frame_size);
        // println!("arg_num: {}", arg_num);
        // println!("root size: {}", root_size);
        let ptr = unsafe { ptr.add(20) };
        // load safe points
        let safepoint_num = unsafe { *(ptr as *const i32) };
        // println!("safepoint_num: {}", safepoint_num);
        let ptr = unsafe { ptr.add(4) };
        let mut safepoints = vec![];
        let mut ptr = ptr as *const *const u8;
        for _ in 0..safepoint_num {
            let safepoint = unsafe { *ptr };
            // println!("safepoint: {:p}", safepoint);
            safepoints.push(safepoint);
            ptr = unsafe { ptr.add(1) };
        }
        let (roots, ptr) = get_function_roots(ptr as *const _, root_size);
        (
            Function {
                addr: unsafe { *(ptr as *const *const u8) },
                root_size,
                roots,
                frame_size,
                arg_num,
                safe_points: Rc::new(safepoints),
            },
            ptr,
        )
    }
    pub fn iter_roots(&self) -> impl Iterator<Item = (i32, ObjectType)> + '_ {
        self.roots.iter().copied()
    }
}

fn get_function_meta(ptr: *const u8) -> (i32, i32, i32) {
    unsafe {
        let ptr = ptr as *const i32;
        (*ptr.offset(2), *ptr.offset(3), *ptr.offset(4))
    }
}

fn get_function_roots(ptr: *const u8, num: i32) -> (Vec<(i32, ObjectType)>, *const u8) {
    unsafe {
        let mut ptr = ptr as *const i32;
        let mut roots = Vec::new();
        for _ in 0..num as isize {
            // println!("root: {} {:p}", *ptr,ptr);
            // println!("type: {} {:p}", *ptr.offset(1),ptr.offset(1));
            roots.push((
                *ptr,
                ObjectType::from_int((*ptr.offset(1)).try_into().unwrap()).unwrap(),
            ));
            ptr = ptr.offset(2);
        }
        (roots, ptr as *const _)
    }
}

pub fn build_root_maps(mapptr: *const u8, roots: &mut FxHashMap<*const u8, Function>) {
    let num_functions = get_num_functions(mapptr);
    let mut ptr = get_first_function_addr(mapptr);
    for _ in 0..num_functions {
        let (function, next_ptr) = Function::new(ptr);
        function.safe_points.iter().for_each(|&safe_point| {
            roots
                .insert(safe_point, function.clone())
                .and_then(|_| -> Option<()> { panic!("duplicate safe point: {:p}", safe_point) });
        });
        // roots.insert(function.addr, function);
        ptr = next_ptr;
    }
}

// fn align_up_to(n: usize, align: usize) -> usize {
//     (n + align - 1) & !(align - 1)
// }

#[cfg(feature = "llvm_gc_plugin")]
extern "C" {
    fn LLVMLinkPLImmixGC();
}

/// Register the LLVM GC plugins.
///
/// the plugin contains `plimmix` gc strategy, and a
/// corresponding stackmap emitter.
///
/// A call to this function is required to compile the
/// code using the `plimmix` gc strategy.
#[cfg(feature = "llvm_gc_plugin")]
pub fn register_llvm_gc_plugins() {
    unsafe {
        LLVMLinkPLImmixGC();
    }
}
