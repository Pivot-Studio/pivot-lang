#![allow(dead_code)]
use int_enum::IntEnum;
use rustc_hash::FxHashMap;

use crate::ObjectType;
use std::marker::PhantomData;

const SAFE_POINT_ID: u64 = 2882400000;
// const GC_ROOT_ID: u64 = 114514;

#[no_mangle]
pub fn print_stack_map(mapptr: *const u8) {
    // let format_version = get_format_version(mapptr);
    // println!("format_version: {}", format_version);
    // let num_functions = get_num_functions(mapptr);
    // println!("num_functions: {}", num_functions);
    let mut map = FxHashMap::default();
    let mut global_roots = vec![];
    build_root_maps(mapptr, &mut map, &mut global_roots);
    // for (addr, func) in map.iter() {
    //     println!("addr: {:p}", *addr);
    //     // println!("size: {}", func.root_size);
    //     println!("roots: {:?}", func.roots);
    //     println!("frame_size: {}", func.frame_size);
    //     // println!("arg_num: {}", func.arg_num);
    // }
    // println!("global_roots: {:?}", global_roots);
}

// pub(crate) fn get_format_version(mapptr: *const u8) -> i32 {
//     unsafe {
//         let ptr = mapptr as *const i32;
//         *ptr
//     }
// }

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
    // pub addr: *const u8,
    // pub offset:u32,
    // pub root_size: i32,
    pub roots: Vec<(i32, ObjectType)>, // offset / wordsize
    pub frame_size: i32,
    // pub arg_num: i32,
    // pub safe_points: Rc<Vec<*const u8>>,
}

impl Function {
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

#[repr(packed)]
#[derive(Debug, Clone, Copy)]
struct Header {
    format_version: u8,
    reserved1: u8,
    reserved2: u16,
}

#[repr(packed)]
#[derive(Debug, Clone, Copy)]
struct Meta {
    num_functions: u32,
    num_constants: u32,
    num_records: u32,
}

#[repr(packed)]
#[derive(Debug, Clone, Copy)]
struct StkSizeRecord {
    addr: u64,
    stack_size: u64,
    record_count: u64,
}

#[repr(packed)]
#[derive(Debug, Clone, Copy)]
struct Constants {
    large_constant: u64,
}

#[repr(packed)]
#[derive(Debug, Clone, Copy)]
struct StkMapRecord {
    patch_point_id: u64,
    instruction_offset: u32,
    reserved: u16,
    num_locations: u16,
    rest: PhantomData<()>,
}

#[repr(packed)]
#[derive(Debug, Clone, Copy)]
struct Location {
    tp: u8,
    reserved: u8,
    size: u16,
    dwarf_reg_num: u16,
    reserved2: u16,
    offset_or_small_const: i32,
}

#[repr(packed)]
#[derive(Debug, Clone, Copy)]
struct LiveOuts {
    dwarf_reg_num: u16,
    reserved: u8,
    size: u8,
}

/// see https://llvm.org/docs/StackMaps.html#stack-map-format
/// Header {
///   uint8  : Stack Map Version (current version is 3)
///   uint8  : Reserved (expected to be 0)
///   uint16 : Reserved (expected to be 0)
/// }
/// uint32 : NumFunctions
/// uint32 : NumConstants
/// uint32 : NumRecords
/// StkSizeRecord[NumFunctions] {
///   uint64 : Function Address
///   uint64 : Stack Size (or UINT64_MAX if not statically known)
///   uint64 : Record Count
/// }
/// Constants[NumConstants] {
///   uint64 : LargeConstant
/// }
/// StkMapRecord[NumRecords] {
///   uint64 : PatchPoint ID
///   uint32 : Instruction Offset
///   uint16 : Reserved (record flags)
///   uint16 : NumLocations
///   Location[NumLocations] {
///     uint8  : Register | Direct | Indirect | Constant | ConstantIndex
///     uint8  : Reserved (expected to be 0)
///     uint16 : Location Size
///     uint16 : Dwarf RegNum
///     uint16 : Reserved (expected to be 0)
///     int32  : Offset or SmallConstant
///   }
///   uint32 : Padding (only if required to align to 8 byte)
///   uint16 : Padding
///   uint16 : NumLiveOuts
///   LiveOuts[NumLiveOuts]
///     uint16 : Dwarf RegNum
///     uint8  : Reserved
///     uint8  : Size in Bytes
///   }
///   uint32 : Padding (only if required to align to 8 byte)
/// }
pub fn build_root_maps(
    mapptr: *const u8,
    roots: &mut FxHashMap<*const u8, Function>,
    _global_roots: &mut Vec<*const u8>,
) {
    let header_ptr = mapptr as *const Header;
    let header = unsafe { *header_ptr };
    if header.format_version != 3 {
        return;
    }
    // println!("header: {:?}", header);
    let meta_ptr = unsafe { header_ptr.add(1) } as *const Meta;
    let meta = unsafe { *meta_ptr };
    // eprintln!("meta: {:?}", meta);
    // println!("meta: {:?}", meta);
    let ptr = unsafe { meta_ptr.add(1) } as *const StkSizeRecord;
    // eprintln!("stk: {:?}", unsafe {
    //     *ptr
    // });
    let stk_sizes_slice = unsafe { std::slice::from_raw_parts(ptr, meta.num_functions as usize) };
    // println!("stk_sizes: {:?}", stk_sizes_slice);
    let ptr = unsafe { ptr.add(meta.num_functions as usize) } as *const Constants;
    // let constants_slice = unsafe { std::slice::from_raw_parts(ptr, meta.num_constants as usize) };
    // println!("constants: {:?}", constants_slice);
    let mut start_ptr = unsafe { ptr.add(meta.num_constants as usize) } as *const StkMapRecord;
    for f in stk_sizes_slice {
        for _ in 0..f.record_count {
            let record = unsafe { *start_ptr };
            // println!("record: {:?}", record);
            let ptr = unsafe { start_ptr.add(1) } as *const Location;
            let locations_slice =
                unsafe { std::slice::from_raw_parts(ptr, record.num_locations as usize) };
            #[cfg(debug_assertions)]
            for loc in locations_slice {
                debug_assert!(
                    loc.tp == 1 || loc.tp == 2 || loc.tp == 3 || loc.tp == 4 || loc.tp == 5,
                    "loc.tp: {}",
                    loc.tp
                );
                let s = loc.size;
                debug_assert_eq!(s, 8_u16);
            }
            // if record.patch_point_id == GC_ROOT_ID {
            //     let mut size = 0;
            //     for loc in locations_slice {
            //         fn_roots.push((record.instruction_offset as i32 + loc.offset_or_small_const + size, ObjectType::Pointer));
            //         size += loc.size as i32;
            //     }
            // }
            if record.patch_point_id == SAFE_POINT_ID {
                let mut fn_roots: Vec<(i32, ObjectType)> = vec![];
                for loc in locations_slice {
                    if loc.tp == 3 {
                        fn_roots.push((loc.offset_or_small_const, ObjectType::Pointer));
                    } else if loc.tp == 2 || loc.tp == 1 {
                        panic!("tp = {}", loc.tp);
                    }
                }
                roots.insert(
                    unsafe { (f.addr as *const u8).add(record.instruction_offset as usize) },
                    Function {
                        roots: fn_roots,
                        frame_size: f.stack_size as i32,
                        // offset: record.instruction_offset,
                        // addr:f.addr as _,
                    },
                );
            }
            // println!("locations: {:?}", locations_slice);
            // paddings
            // pad i32 if need to align to 8 bytes
            let ptr = unsafe { ptr.add(record.num_locations as usize) } as *const u32;
            let ptr = if (ptr as usize) % 8 == 0 {
                ptr
            } else {
                unsafe { ptr.add(1) }
            };
            // pad u16 directly
            let ptr = unsafe { (ptr as *const u16).add(1) };
            let num_live_outs = unsafe { *ptr };
            let ptr = unsafe { ptr.add(1) } as *const LiveOuts;
            // let _live_outs_slice =
            //     unsafe { std::slice::from_raw_parts(ptr, num_live_outs as usize) };
            // pad i32 if need to align to 8 bytes
            let ptr = unsafe { ptr.add(num_live_outs as usize) } as *const u32;
            let ptr = if (ptr as usize) % 8 == 0 {
                ptr
            } else {
                unsafe { ptr.add(1) }
            };
            start_ptr = ptr as *const StkMapRecord;
        }
    }

    build_root_maps(start_ptr as _, roots, _global_roots);

    // let num_functions = get_num_functions(mapptr);
    // let mut ptr = get_first_function_addr(mapptr);
    // for _ in 0..num_functions {
    //     let (function, next_ptr) = Function::new(ptr);
    //     function.safe_points.iter().for_each(|&safe_point| {
    //         roots
    //             .insert(safe_point, function.clone())
    //             .and_then(|_| -> Option<()> { panic!("duplicate safe point: {:p}", safe_point) });
    //     });
    //     // roots.insert(function.addr, function);
    //     ptr = next_ptr;
    // }
    // build_global_roots(ptr, global_roots);
}

fn build_global_roots(ptr: *const u8, global_roots: &mut Vec<*const u8>) {
    let num_globals = unsafe { *(ptr as *const i32) };
    let ptr = unsafe { ptr.add(4) };
    let ptr = align_up_to(ptr as usize, 8) as *const u8;
    let mut ptr = ptr as *const *const u8;
    for _ in 0..num_globals {
        let global = unsafe { *ptr };
        global_roots.push(global);
        ptr = unsafe { ptr.add(1) };
    }
}

fn align_up_to(n: usize, align: usize) -> usize {
    (n + align - 1) & !(align - 1)
}

#[cfg(feature = "llvm_gc_plugin")]
extern "C" {
    fn LLVMLinkPLImmixGC();
    pub fn CreatePLJITEngine(
        engine: *mut u8,
        module: *mut u8,
        opt: u32,
        cb: extern "C" fn(map: *mut u8),
    );
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
