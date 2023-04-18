#[cfg(feature = "llvm_gc_plugin")]
pub type VisitF = extern "C" fn(root: *mut u8, obj_type: *mut u8);

#[cfg(feature = "llvm_gc_plugin")]
extern "C" {
    pub fn visitGCRoots(visitor: VisitF);
    pub fn SetShadowStackAddr(addr: *mut u8);
}
