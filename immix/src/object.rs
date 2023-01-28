use std::marker::PhantomData;

use crate::{ObjectType, LINE_SIZE};

#[repr(C)]
pub struct ImmixObject {
    pub head: ObjHeader,
    pub _phantom: PhantomData<usize>,
}

impl ImmixObject {
    pub fn new(obj_type: ObjectType, obj_size: usize) -> ImmixObject {
        ImmixObject {
            head: ObjHeader::new(obj_type, obj_size as u32, false),
            _phantom: PhantomData,
        }
    }
    pub fn heap_size(&self) -> usize {
        (self.head.obj_size + 8) as usize
    }
    pub fn init_header(&self, ptr: *mut u8) {
        unsafe {
            std::ptr::copy_nonoverlapping(self as *const ImmixObject as *const u8, ptr, 8);
        }
    }

    pub fn from_mutator_ptr(ptr: *mut u8) -> *mut ImmixObject {
        unsafe { ptr.sub(8) as *mut ImmixObject }
    }

    pub fn get_mutator_ptr(&self) -> *mut u8 {
        unsafe { (self as *const ImmixObject as *const u8).add(8) as *mut u8 }
    }

    pub fn get_line_size(&self) -> usize {
        let size = self.heap_size();
        (size - 1) / LINE_SIZE + 1
    }
}

#[repr(C)]
pub struct ObjHeader {
    pub obj_type: ObjectType,
    pub is_forward: bool,
    pub obj_size: u32,
}

impl ObjHeader {
    pub fn new(obj_type: ObjectType, obj_size: u32, is_forward: bool) -> ObjHeader {
        ObjHeader {
            obj_type,
            obj_size,
            is_forward,
        }
    }
}
