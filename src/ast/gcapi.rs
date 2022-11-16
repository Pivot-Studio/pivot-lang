use inkwell::{
    builder::Builder,
    values::{BasicValueEnum, PointerValue},
    AddressSpace,
};

use super::ctx::{Ctx, Mod};
use super::pltype::FNType;

impl<'a, 'ctx> Ctx<'a, 'ctx> {
    pub fn mv2heap(&self, val: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        if !self.usegc {
            return val;
        }
        let gcmod = self.get_gc_plmod();
        let f: FNType = gcmod
            .get_type("DioGC__malloc")
            .unwrap()
            .borrow()
            .clone()
            .try_into()
            .unwrap();
        let f = f.get_or_insert_fn(self);
        let gc = self.builder.build_load(self.get_gc(), "gc");
        let td = self.targetmachine.get_target_data();
        let loaded = self.builder.build_load(val.into_pointer_value(), "loaded");
        let size = td.get_store_size(&loaded.get_type());
        let size = self.context.i64_type().const_int(size as u64, true);
        let heapptr = self
            .builder
            .build_call(f, &[gc.into(), size.into()], "gc_malloc")
            .try_as_basic_value()
            .left()
            .unwrap();
        let heapptr = self.builder.build_pointer_cast(
            heapptr.into_pointer_value(),
            val.get_type().into_pointer_type(),
            "heapptr",
        );
        self.builder.build_store(heapptr, loaded);
        heapptr.into()
    }
    // pub fn gc_free(&self) -> FNType {
    //     let gcmod = self.get_gc_plmod();
    //     gcmod.get_type("DioGC__free").unwrap().try_into().unwrap()
    // }
    // pub fn gc_new_ptr(&self) -> FNType {
    //     let gcmod = self.get_gc_plmod();
    //     gcmod
    //         .get_type("DioGC__new_ptr")
    //         .unwrap()
    //         .try_into()
    //         .unwrap()
    // }
    pub fn gc_add_root(&self, stackptr: BasicValueEnum<'ctx>, builder: &'a Builder<'ctx>) {
        if !self.usegc {
            return;
        }
        let gcmod = self.get_gc_plmod();
        let f: FNType = gcmod
            .get_type("DioGC__add_root")
            .unwrap()
            .borrow()
            .clone()
            .try_into()
            .unwrap();
        let f = f.get_or_insert_fn(self);
        let gc = builder.build_load(self.get_gc(), "gc");
        let stackptr = builder.build_pointer_cast(
            stackptr.into_pointer_value(),
            self.context.i64_type().ptr_type(AddressSpace::Generic),
            "stackptr",
        );
        builder.build_call(f, &[gc.into(), stackptr.into()], "add_root");
    }
    pub fn gc_rm_root(&self, stackptr: BasicValueEnum<'ctx>) {
        if !self.usegc {
            return;
        }
        let block = self.builder.get_insert_block().unwrap();
        if let Some(inst) = block.get_first_instruction() {
            self.builder.position_before(&inst);
        }
        self.gc_rm_root_current(stackptr);
    }
    pub fn gc_rm_root_current(&self, stackptr: BasicValueEnum<'ctx>) {
        if !self.usegc {
            return;
        }
        let gcmod = self.get_gc_plmod();
        let f: FNType = gcmod
            .get_type("DioGC__rm_root")
            .unwrap()
            .borrow()
            .clone()
            .try_into()
            .unwrap();
        let f = f.get_or_insert_fn(self);
        let gc = self.builder.build_load(self.get_gc(), "gc");
        let stackptr = self.builder.build_pointer_cast(
            stackptr.into_pointer_value(),
            self.context.i64_type().ptr_type(AddressSpace::Generic),
            "stackptr",
        );
        self.builder
            .build_call(f, &[gc.into(), stackptr.into()], "rm_root");
    }
    pub fn gc_collect(&self) {
        if !self.usegc {
            return;
        }
        let gcmod = self.get_gc_plmod();
        let f: FNType = gcmod
            .get_type("DioGC__collect")
            .unwrap()
            .borrow()
            .clone()
            .try_into()
            .unwrap();
        let f = f.get_or_insert_fn(self);
        let gc = self.builder.build_load(self.get_gc(), "gc");
        self.builder.build_call(f, &[gc.into()], "collect");
    }
    // pub fn gc_get_size(&self) -> FNType {
    //     let gcmod = self.get_gc_plmod();
    //     gcmod
    //         .get_type("DioGC__get_size")
    //         .unwrap()
    //         .try_into()
    //         .unwrap()
    // }
    fn get_gc_plmod(&self) -> &Mod {
        self.plmod.submods.get("gc").unwrap()
    }
    fn get_gc(&self) -> PointerValue<'ctx> {
        let gcmod = self.get_gc_plmod();
        let gc = gcmod.get_global_symbol("diogc").unwrap();
        self.get_or_add_global("diogc", gcmod, gc.tp.clone())
    }
}
