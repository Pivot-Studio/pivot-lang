use inkwell::{values::BasicValueEnum, AddressSpace};

use super::ctx::{Ctx, FNType};

impl<'a, 'ctx> Ctx<'a, 'ctx> {
    pub fn mv2heap(&self, val: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        let f: FNType = self
            .plmod
            .submods
            .get("gc")
            .unwrap()
            .get_type("DioGC__malloc")
            .unwrap()
            .try_into()
            .unwrap();
        let f = f.get_value(self);
        let gc = self.module.get_global("diogc").unwrap();
        let gc = self.builder.build_load(gc.as_pointer_value(), "gc");
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
    pub fn gc_free(&self) -> FNType {
        self.plmod
            .submods
            .get("gc")
            .unwrap()
            .get_type("DioGC__free")
            .unwrap()
            .try_into()
            .unwrap()
    }
    pub fn gc_new_ptr(&self) -> FNType {
        self.plmod
            .submods
            .get("gc")
            .unwrap()
            .get_type("DioGC__new_ptr")
            .unwrap()
            .try_into()
            .unwrap()
    }
    pub fn gc_add_root(&self) -> FNType {
        self.plmod
            .submods
            .get("gc")
            .unwrap()
            .get_type("DioGC__add_root")
            .unwrap()
            .try_into()
            .unwrap()
    }
    pub fn gc_rm_root(&self) -> FNType {
        self.plmod
            .submods
            .get("gc")
            .unwrap()
            .get_type("DioGC__rm_root")
            .unwrap()
            .try_into()
            .unwrap()
    }
    pub fn gc_collect(&self) -> FNType {
        self.plmod
            .submods
            .get("gc")
            .unwrap()
            .get_type("DioGC__collect")
            .unwrap()
            .try_into()
            .unwrap()
    }
    pub fn gc_get_size(&self) -> FNType {
        self.plmod
            .submods
            .get("gc")
            .unwrap()
            .get_type("DioGC__get_size")
            .unwrap()
            .try_into()
            .unwrap()
    }
    pub fn add_global_gc(&self) {
        let tp = self.context.i64_type().ptr_type(AddressSpace::Generic);
        self.module.add_global(tp, None, "diogc");
    }
    pub fn init_global_gc(&self) {
        let f = self.gc_new_ptr().get_value(&self);
        let gc = self.module.get_global("diogc").unwrap();
        gc.set_initializer(
            &self
                .context
                .i64_type()
                .ptr_type(AddressSpace::Generic)
                .const_zero(),
        );
        let v = self.builder.build_call(f, &[], "gc_init_call");
        self.builder.build_store(
            gc.as_pointer_value(),
            v.try_as_basic_value().left().unwrap(),
        );
    }
}
