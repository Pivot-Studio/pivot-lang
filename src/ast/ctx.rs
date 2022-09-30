use crate::ast::node::Value;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::types::FunctionType;
use inkwell::types::StructType;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use std::collections::HashMap;
#[derive(Debug, Clone)]
pub struct Ctx<'a, 'ctx> {
    pub table: HashMap<String, PointerValue<'ctx>>,
    pub types: HashMap<String, PLType<'ctx>>,
    pub father: Option<&'a Ctx<'a, 'ctx>>,
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub function: FunctionValue<'ctx>,
    pub block: Option<BasicBlock<'ctx>>,
    pub continue_block: Option<BasicBlock<'ctx>>,
    pub break_block: Option<BasicBlock<'ctx>>,
}

#[derive(Debug, Clone)]
pub enum PLType<'ctx> {
    FN(FNType<'ctx>),
    STRUCT(STType<'ctx>),
    PRIMITIVE(BasicTypeEnum<'ctx>),
}
impl<'ctx> PLType<'ctx> {
    pub fn get_basic_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            PLType::FN(f) => f
                .fntype
                .ptr_type(inkwell::AddressSpace::Global)
                .as_basic_type_enum(),
            PLType::STRUCT(s) => s.struct_type.as_basic_type_enum(),
            PLType::PRIMITIVE(t) => *t,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FNType<'ctx> {
    pub name: &'ctx str,
    pub fntype: FunctionType<'ctx>,
}
#[derive(Debug, Clone)]
pub struct STType<'ctx> {
    pub name: &'ctx str,
    pub fields: HashMap<&'ctx str, PLType<'ctx>>,
    pub struct_type: StructType<'ctx>,
}

fn add_primitive_types<'a, 'ctx>(
    context: &'ctx Context,
    table: &'a mut HashMap<String, PLType<'ctx>>,
) {
    table.insert(
        "i8".to_string(),
        PLType::PRIMITIVE(context.i8_type().as_basic_type_enum()),
    );
    table.insert(
        "i16".to_string(),
        PLType::PRIMITIVE(context.i16_type().as_basic_type_enum()),
    );
    table.insert(
        "i32".to_string(),
        PLType::PRIMITIVE(context.i32_type().as_basic_type_enum()),
    );
    table.insert(
        "i64".to_string(),
        PLType::PRIMITIVE(context.i64_type().as_basic_type_enum()),
    );
    table.insert(
        "i128".to_string(),
        PLType::PRIMITIVE(context.i128_type().as_basic_type_enum()),
    );
    table.insert(
        "u8".to_string(),
        PLType::PRIMITIVE(context.i8_type().as_basic_type_enum()),
    );
    table.insert(
        "u16".to_string(),
        PLType::PRIMITIVE(context.i16_type().as_basic_type_enum()),
    );
    table.insert(
        "u32".to_string(),
        PLType::PRIMITIVE(context.i32_type().as_basic_type_enum()),
    );
    table.insert(
        "u64".to_string(),
        PLType::PRIMITIVE(context.i64_type().as_basic_type_enum()),
    );
    table.insert(
        "u128".to_string(),
        PLType::PRIMITIVE(context.i128_type().as_basic_type_enum()),
    );
    table.insert(
        "f32".to_string(),
        PLType::PRIMITIVE(context.f32_type().as_basic_type_enum()),
    );
    table.insert(
        "f64".to_string(),
        PLType::PRIMITIVE(context.f64_type().as_basic_type_enum()),
    );
    table.insert(
        "bool".to_string(),
        PLType::PRIMITIVE(context.bool_type().as_basic_type_enum()),
    );
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
        let mut types = HashMap::new();
        add_primitive_types(context, &mut types);
        Ctx {
            table: HashMap::new(),
            types,
            father: None,
            context,
            module,
            builder,
            function,
            block: Some(basic_block),
            continue_block: None,
            break_block: None,
        }
    }
    pub fn new_child(&'a self) -> Ctx<'a, 'ctx> {
        let mut types = HashMap::new();
        add_primitive_types(self.context, &mut types);
        Ctx {
            table: HashMap::new(),
            types,
            father: Some(self),
            context: self.context,
            builder: self.builder,
            module: self.module,
            function: self.function,
            block: self.block,
            continue_block: self.continue_block,
            break_block: self.break_block,
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

    pub fn get_type(&self, name: &str) -> Option<&PLType<'ctx>> {
        let v = self.types.get(name);
        if let Some(pv) = v {
            return Some(pv);
        }
        if let Some(father) = self.father {
            return father.get_type(name);
        }
        None
    }

    pub fn add_type(&mut self, name: String, tp: PLType<'ctx>) {
        if self.types.contains_key(&name) {
            todo!() // TODO 报错
        }
        self.types.insert(name, tp);
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
