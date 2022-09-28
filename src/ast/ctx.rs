use crate::ast::node::Value;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use std::collections::HashMap;
#[derive(Debug, Clone)]
pub struct Ctx<'a, 'ctx> {
    pub table: HashMap<String, PointerValue<'ctx>>,
    pub father: Option<&'a Ctx<'a, 'ctx>>,
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub function: FunctionValue<'ctx>,
}

impl<'a, 'ctx> Ctx<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
    ) -> Ctx<'a, 'ctx> {
        let i64_type = context.i64_type();
        let fn_type = i64_type.fn_type(&[], false);
        let function = module.add_function("main", fn_type, None);
        let basic_block = context.append_basic_block(function, "entry");
        builder.position_at_end(basic_block);
        Ctx {
            table: HashMap::new(),
            father: None,
            context,
            module,
            builder,
            function,
        }
    }
    pub fn new_child(&'a self) -> Ctx<'a, 'ctx> {
        Ctx {
            table: HashMap::new(),
            father: Some(self),
            context: self.context,
            module: self.module,
            builder: self.builder,
            function: self.function,
        }
    }

    /// # get_symbol
    /// search in current and all father symbol tables
    pub fn get_symbol(&self, name: &str) -> Option<&PointerValue<'ctx>> {
        let v = self.table.get(name);
        if let Some(pv) = v {
            return Some(pv);
        }
        if let Some(father) = self.father {
            return father.get_symbol(name);
        }
        None
    }

    pub fn add_symbol(&mut self, name: String, pv: PointerValue<'ctx>) {
        if self.table.contains_key(&name) {
            todo!() // TODO 报错
        }
        self.table.insert(name, pv);
    }
    pub fn try_load(&mut self, v: Value<'ctx>) -> Value<'ctx> {
        match v {
            Value::VarValue(v) => {
                let v = self.builder.build_load(v, "loadtmp");
                match v {
                    BasicValueEnum::IntValue(v) => match v.get_type().get_bit_width() {
                        1 => Value::BoolValue(v),
                        64 => Value::IntValue(v),
                        _ => todo!(),
                    },
                    BasicValueEnum::FloatValue(v) => Value::FloatValue(v),
                    _ => todo!(),
                }
            }
            _ => v,
        }
    }
}
