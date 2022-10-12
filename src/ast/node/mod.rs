use crate::ast::ctx::Ctx;
use crate::ast::range::RangeTrait;

use as_any::AsAny;
use inkwell::basic_block::BasicBlock;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, IntValue, PointerValue};

use super::ctx::PLDiag;
use super::range::Pos;

pub mod control;
pub mod error;
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
    LoadValue(BasicValueEnum<'a>),
    RefValue(PointerValue<'a>),
    StructFieldValue((String, BasicValueEnum<'a>)),
    None,
}

impl<'a> Value<'a> {
    pub fn as_basic_value_enum(&self) -> BasicValueEnum<'a> {
        self.as_basic_value_enum_op().unwrap()
    }
    pub fn as_basic_value_enum_op(&self) -> Option<BasicValueEnum<'a>> {
        match self {
            Value::IntValue(v) => Some(v.as_basic_value_enum()),
            Value::FloatValue(v) => Some(v.as_basic_value_enum()),
            Value::VarValue(v) => Some(v.as_basic_value_enum()),
            Value::RefValue(v) => Some(v.as_basic_value_enum()),
            Value::BoolValue(v) => Some(v.as_basic_value_enum()),
            Value::LoadValue(v) => Some(*v),
            Value::StructFieldValue((_, v)) => Some(*v),
            Value::None => None,
        }
    }
}

pub trait Node: RangeTrait + AsAny {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>);
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx>;
}

type NodeResult<'ctx> = Result<(Value<'ctx>, Option<String>), PLDiag>;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Num {
    INT(u64),
    FLOAT(f64),
}

impl<'a, 'ctx> Ctx<'ctx, 'a> {
    pub fn position_at_end(&mut self, block: BasicBlock<'a>) {
        position_at_end(self, block)
    }
    pub fn build_dbg_location(&mut self, pos: Pos) {
        let loc = self.dibuilder.create_debug_location(
            self.context,
            pos.line as u32,
            pos.column as u32,
            self.discope,
            None,
        );
        self.builder.set_current_debug_location(self.context, loc);
    }
}
pub fn deal_line(tabs: usize, line: &mut Vec<bool>, end: bool) {
    if tabs == line.len() {
        line.push(end);
    } else {
        line[tabs] = end;
    }
}
pub fn tab(tabs: usize, line: Vec<bool>, end: bool) {
    for i in 0..tabs {
        if line[i] {
            print!("    ");
        } else {
            print!(" │  ");
        }
    }
    if end {
        print!(" └─ ");
    } else {
        print!(" ├─ ");
    }
}

pub fn position_at_end<'a, 'b>(ctx: &mut Ctx<'b, 'a>, block: BasicBlock<'a>) {
    ctx.builder.position_at_end(block);
    ctx.block = Some(block);
    ctx.nodebug_builder.position_at_end(block);
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
            builder.position_at_end(ctx.block.unwrap());
            p
        }
        None => panic!("alloc get entry failed!"),
    }
}

#[macro_export]
macro_rules! handle_calc {
    ($ctx:ident, $op:ident, $opf:ident  ,$left:ident, $right:ident, $range: expr) => {
        item! {
            match ($left, $right) {
                (Value::IntValue(left), Value::IntValue(right)) => {
                    return Ok((Value::IntValue($ctx.builder.[<build_int_$op>](
                        left, right, "addtmp")),Some("i64".to_string())));
                },
                (Value::FloatValue(left), Value::FloatValue(right)) => {
                    return Ok((Value::FloatValue($ctx.builder.[<build_$opf>](
                        left, right, "addtmp")),Some("f64".to_string())));
                },
                _ =>  return Err($ctx.add_err(
                    $range,
                    crate::ast::error::ErrorCode::UNRECOGNIZED_BIN_OPERATOR,
                ))
            }
        }
    };
}
