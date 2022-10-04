use crate::ast::node::Value;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::debug_info::*;
use inkwell::module::Module;
use inkwell::targets::TargetMachine;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::types::FunctionType;
use inkwell::types::StructType;
use inkwell::types::VoidType;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use std::collections::HashMap;

use super::compiler::get_target_machine;
use super::node::types::TypeNameNode;
use super::range::Pos;
#[derive(Debug, Clone)]
pub struct Ctx<'a, 'ctx> {
    pub table: HashMap<String, PointerValue<'ctx>>,
    pub types: HashMap<String, PLType<'a, 'ctx>>,
    pub father: Option<&'a Ctx<'a, 'ctx>>,
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub dibuilder: &'a DebugInfoBuilder<'ctx>,
    pub diunit: &'a DICompileUnit<'ctx>,
    pub function: Option<FunctionValue<'ctx>>,
    pub block: Option<BasicBlock<'ctx>>,
    pub continue_block: Option<BasicBlock<'ctx>>,
    pub break_block: Option<BasicBlock<'ctx>>,
    pub targetmachine: &'a TargetMachine,
    pub discope: DIScope<'ctx>,
    pub nodebug_builder: &'a Builder<'ctx>,
}

#[derive(Debug, Clone)]
pub enum PLType<'a, 'ctx> {
    FN(FNType<'ctx>),
    STRUCT(STType<'a, 'ctx>),
    PRIMITIVE(BasicTypeEnum<'ctx>),
    VOID(VoidType<'ctx>),
}
impl<'a, 'ctx> PLType<'a, 'ctx> {
    pub fn get_basic_type(&self) -> BasicTypeEnum<'ctx> {
        match self {
            PLType::FN(f) => f
                .fntype
                .get_type()
                .ptr_type(inkwell::AddressSpace::Global)
                .as_basic_type_enum(),
            PLType::STRUCT(s) => s.struct_type.as_basic_type_enum(),
            PLType::PRIMITIVE(t) => *t,
            PLType::VOID(_) => panic!("void type"),
        }
    }

    pub fn get_ret_type(&self) -> RetTypeEnum<'ctx> {
        match self {
            PLType::VOID(x) => RetTypeEnum::VOID(*x),
            _ => RetTypeEnum::BASIC(self.get_basic_type()),
        }
    }
}

pub enum RetTypeEnum<'ctx> {
    VOID(VoidType<'ctx>),
    BASIC(BasicTypeEnum<'ctx>),
}

impl<'ctx> RetTypeEnum<'ctx> {
    pub fn fn_type(
        &self,
        param_types: &[BasicMetadataTypeEnum<'ctx>],
        is_var_args: bool,
    ) -> FunctionType<'ctx> {
        match self {
            RetTypeEnum::VOID(t) => t.fn_type(param_types, is_var_args),
            RetTypeEnum::BASIC(t) => t.fn_type(param_types, is_var_args),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Field<'a, 'ctx> {
    pub index: u32,
    pub tp: PLType<'a, 'ctx>,
    pub typename: &'a TypeNameNode,
    pub name: String,
}

impl<'a, 'ctx> Field<'a, 'ctx> {
    pub fn get_di_type(&self, ctx: &mut Ctx<'a, 'ctx>) -> DIType<'ctx> {
        let tp = self.typename.get_debug_type(ctx).unwrap();
        ctx.dibuilder
            .create_member_type(
                ctx.discope,
                &self.name,
                ctx.diunit.get_file(),
                self.typename.range.start.line as u32,
                tp.get_size_in_bits(),
                tp.get_align_in_bits(),
                tp.get_offset_in_bits(),
                DIFlags::PUBLIC,
                tp,
            )
            .as_type()
    }
}

#[derive(Debug, Clone)]
pub struct FNType<'ctx> {
    pub name: String,
    pub fntype: FunctionValue<'ctx>,
}
#[derive(Debug, Clone)]
pub struct STType<'a, 'ctx> {
    pub name: String,
    pub fields: HashMap<String, Field<'a, 'ctx>>,
    pub struct_type: StructType<'ctx>,
}

fn add_primitive_types<'a, 'ctx>(context: &'ctx Context) -> HashMap<String, PLType<'a, 'ctx>> {
    let mut table = HashMap::<String, PLType<'a, 'ctx>>::new();
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
    table.insert("void".to_string(), PLType::VOID(context.void_type()));
    table
}

pub fn create_ctx_info<'ctx>(
    context: &'ctx Context,
    dir: &str,
    file: &str,
) -> (
    Module<'ctx>,
    Builder<'ctx>,
    DebugInfoBuilder<'ctx>,
    DICompileUnit<'ctx>,
    TargetMachine,
    Builder<'ctx>,
) {
    let builder = context.create_builder();
    let module = context.create_module("main");
    let (dibuilder, compile_unit) = module.create_debug_info_builder(
        true,
        DWARFSourceLanguage::C,
        file,
        dir,
        "plc frontend",
        false,
        "",
        0,
        "",
        DWARFEmissionKind::Full,
        0,
        false,
        false,
        "",
        "",
    );
    let tm = get_target_machine(inkwell::OptimizationLevel::None);
    (
        module,
        builder,
        dibuilder,
        compile_unit,
        tm,
        context.create_builder(),
    )
}

impl<'a, 'ctx> Ctx<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        dibuilder: &'a DebugInfoBuilder<'ctx>,
        diunit: &'a DICompileUnit<'ctx>,
        tm: &'a TargetMachine,
        nodbg_builder: &'a Builder<'ctx>,
    ) -> Ctx<'a, 'ctx> {
        let types = add_primitive_types(context);
        Ctx {
            table: HashMap::new(),
            types,
            father: None,
            context,
            module,
            builder,
            function: None,
            block: None,
            continue_block: None,
            break_block: None,
            dibuilder,
            diunit,
            targetmachine: tm,
            discope: diunit.as_debug_info_scope(),
            nodebug_builder: nodbg_builder,
        }
    }
    pub fn new_child(&'a self, start: Pos) -> Ctx<'a, 'ctx> {
        let types = add_primitive_types(self.context);
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
            dibuilder: self.dibuilder,
            diunit: self.diunit,
            targetmachine: self.targetmachine,
            discope: self
                .dibuilder
                .create_lexical_block(
                    self.discope,
                    self.diunit.get_file(),
                    start.line as u32,
                    start.column as u32,
                )
                .as_debug_info_scope(),
            nodebug_builder: self.nodebug_builder,
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

    pub fn get_type(&self, name: &str) -> Option<&PLType<'a, 'ctx>> {
        let v = self.types.get(name);
        if let Some(pv) = v {
            return Some(pv);
        }
        if let Some(father) = self.father {
            return father.get_type(name);
        }
        None
    }

    pub fn add_type(&mut self, name: String, tp: PLType<'a, 'ctx>) {
        if self.types.contains_key(&name) {
            todo!() // TODO 报错
        }
        self.types.insert(name, tp);
    }

    pub fn try_load(&mut self, v: Value<'ctx>) -> Value<'ctx> {
        match v.as_basic_value_enum() {
            BasicValueEnum::PointerValue(v) => {
                let v = self.builder.build_load(v, "loadtmp");
                match v {
                    BasicValueEnum::IntValue(v) => match v.get_type().get_bit_width() {
                        1 => Value::BoolValue(v),
                        64 => Value::IntValue(v),
                        _ => todo!(),
                    },
                    BasicValueEnum::FloatValue(v) => Value::FloatValue(v),
                    _ => Value::LoadValue(v),
                }
            }
            _ => v,
        }
    }
}
