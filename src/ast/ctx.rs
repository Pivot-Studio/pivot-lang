use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;

pub struct Ctx<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
}

impl Ctx<'_> {
    pub fn new<'ctx>(context: &'ctx Context) -> Ctx<'ctx> {
        let module = context.create_module("main");
        let builder = context.create_builder();
        Ctx {
            context,
            module,
            builder,
        }
    }
}
