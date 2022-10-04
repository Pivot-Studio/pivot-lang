use crate::ast::ctx::Ctx;
use crate::ast::range::RangeTrait;

use as_any::AsAny;
use inkwell::basic_block::BasicBlock;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, IntValue, PointerValue};

pub mod control;
pub mod function;
pub mod operator;
pub mod primary;
pub mod program;
pub mod ret;
pub mod statement;
pub mod types;
/// # Value
/// 表达每个ast在计算之后产生的值  
///
/// 只有expression才会产生值，而statement不会产生值。对于statement，
/// 我们可以用None来表示
#[derive(Debug)]
pub enum Value<'a> {
    BoolValue(IntValue<'a>),
    IntValue(IntValue<'a>),
    FloatValue(FloatValue<'a>),
    VarValue(PointerValue<'a>),
    TypeValue(BasicTypeEnum<'a>),
    LoadValue(BasicValueEnum<'a>),
    StructFieldValue((String, BasicValueEnum<'a>)),
    None,
}

impl<'a> Value<'a> {
    pub fn as_basic_value_enum(&self) -> BasicValueEnum<'a> {
        match self {
            Value::IntValue(v) => v.as_basic_value_enum(),
            Value::FloatValue(v) => v.as_basic_value_enum(),
            Value::VarValue(v) => v.as_basic_value_enum(),
            Value::BoolValue(v) => v.as_basic_value_enum(),
            Value::None => panic!("not implemented"),
            Value::TypeValue(_) => panic!("not implemented"),
            Value::LoadValue(v) => *v,
            Value::StructFieldValue((_, v)) => *v,
        }
    }
}

pub trait Node: RangeTrait + AsAny {
    fn string(&self, tabs: usize) -> String;
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx>;
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Num {
    INT(u64),
    FLOAT(f64),
}

pub fn position_at_end<'a, 'b>(ctx: &mut Ctx<'b, 'a>, block: BasicBlock<'a>) {
    ctx.builder.position_at_end(block);
    ctx.block = Some(block);
}

pub fn alloc<'a, 'ctx>(
    ctx: &Ctx<'a, 'ctx>,
    tp: BasicTypeEnum<'ctx>,
    name: &str,
) -> PointerValue<'ctx> {
    let builder = ctx.nodebug_builder;
    match ctx.function.unwrap().get_first_basic_block() {
        Some(entry) => {
            builder.position_at_end(entry);
            let p = builder.build_alloca(tp, name);
            p
        }
        None => panic!("alloc get entry failed!"),
    }
}

#[macro_export]
macro_rules! handle_calc {
    ($ctx:ident, $op:ident, $opf:ident  ,$left:ident, $right:ident) => {
        item! {
            match ($left, $right) {
                (Value::IntValue(left), Value::IntValue(right)) => {
                    return Value::IntValue($ctx.builder.[<build_int_$op>](
                        left, right, "addtmp"));
                },
                (Value::FloatValue(left), Value::FloatValue(right)) => {
                    return Value::FloatValue($ctx.builder.[<build_$opf>](
                        left, right, "addtmp"));
                },
                _ => panic!("not implemented")
            }
        }
    };
}
