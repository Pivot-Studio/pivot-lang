use std::collections::HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::PointerValue;

pub struct Ctx<'a, 'ctx> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: &'a Builder<'ctx>,
}

/// # MutCtx
/// 存储可能需要变化的信息
///
/// 为了解决borrow问题，在只读的emit调用中应该clone
/// mut的ctx并且传入只读ctx，而pointervalue因为可能会
/// 被返回需要单独的生命周期
#[derive(Debug, Clone)]
pub struct MutCtx<'a, 'b> {
    pub table: HashMap<String, PointerValue<'b>>,
    pub father: Option<&'a MutCtx<'a, 'b>>,
    pub basic_block: BasicBlock<'b>,
}

impl<'b, 'c> MutCtx<'b, 'c> {
    pub fn new(context: &'c Context, module: & Module<'c>) -> MutCtx<'b, 'c> {
        let i64_type = context.i64_type();
        let fn_type = i64_type.fn_type(&[], false);
        let function = module.add_function("main", fn_type, None);
        let basic_block = context.append_basic_block(function, "entry");
        MutCtx {
            table: HashMap::new(),
            father: None,
            basic_block,
        }
    }
    pub fn new_child(&'c self) -> MutCtx<'b, 'c> {
        MutCtx {
            table: HashMap::new(),
            father: Some(self),
            basic_block: self.basic_block,
        }
    }

    /// # get_symbol
    /// search in current and all father symbol tables
    pub fn get_symbol(&'b self, name: &str) -> Option<&PointerValue<'c>> {
        let v = self.table.get(name);
        if let Some(pv) = v {
            return Some(pv);
        }
        if let Some(father) = self.father {
            return father.get_symbol(name);
        }
        None
    }

    pub fn add_symbol(&mut self, name: String, pv: PointerValue<'c>) {
        if self.table.contains_key(&name) {
            todo!() // TODO 报错
        }
        self.table.insert(name, pv);
    }
}

impl<'a, 'ctx> Ctx<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
    ) -> Ctx<'a, 'ctx> {
        Ctx {
            context,
            module: module,
            builder: builder,
        }
    }
}
