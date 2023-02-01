use rustc_hash::FxHashMap;

#[no_mangle]
pub fn print_stack_map(mapptr: *const u8) {
    let format_version = get_format_version(mapptr);
    println!("format_version: {}", format_version);
    let num_functions = get_num_functions(mapptr);
    println!("num_functions: {}", num_functions);
    let map = build_function_maps(mapptr);
    for (addr, func) in map.iter() {
        println!("addr: {:p}", *addr);
        println!("size: {}", func.size);
        println!("roots: {:?}", func.roots);
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

#[derive(Debug)]
pub struct Function {
    addr: *const u8,
    size: i32,
    roots: Vec<i32>, // offset / wordsize
}

impl Function {
    pub fn new(ptr: *const u8) -> Function {
        let size = get_function_size(ptr);
        let roots = get_function_roots(ptr, size);
        Function {
            addr: unsafe { *(ptr as *const *const u8) },
            size,
            roots,
        }
    }
}

fn get_function_size(ptr: *const u8) -> i32 {
    unsafe {
        let ptr = ptr as *const i32;
        *ptr.offset(2)
    }
}

fn get_function_roots(ptr: *const u8, num: i32) -> Vec<i32> {
    unsafe {
        let ptr = ptr as *const i32;
        let mut roots = Vec::new();
        for i in 0..num {
            roots.push(*ptr.offset((3 + i) as isize));
        }
        roots
    }
}

pub fn build_function_maps(mapptr: *const u8) -> FxHashMap<*const u8, Function> {
    let mut functions = FxHashMap::default();
    let num_functions = get_num_functions(mapptr);
    let mut ptr = get_first_function_addr(mapptr);
    for _ in 0..num_functions {
        let function = Function::new(ptr);
        let size = function.size;
        functions.insert(function.addr, function);
        ptr = unsafe { ptr.add(align_up_to((12 + size * 4) as usize, 8)) };
    }
    functions
}

fn align_up_to(n: usize, align: usize) -> usize {
    (n + align - 1) & !(align - 1)
}

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
pub fn register_llvm_gc_plugins() {
    unsafe {
        LLVMLinkPLImmixGC();
    }
}
