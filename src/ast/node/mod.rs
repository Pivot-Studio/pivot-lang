use crate::ast::ctx::Ctx;

use as_any::AsAny;
use enum_dispatch::enum_dispatch;
use inkwell::basic_block::BasicBlock;
use inkwell::types::{BasicTypeEnum, StructType};
use inkwell::values::{
    BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue,
};

use self::comment::CommentNode;
use self::control::*;
use self::error::*;
use self::function::*;
use self::global::*;
use self::operator::*;
use self::pkg::{ExternIDNode, UseNode};
use self::primary::*;
use self::ret::*;
use self::statement::*;
use self::types::*;

use super::ctx::{PLDiag, PLType};
use super::range::{Pos, Range};

pub mod comment;
pub mod control;
pub mod error;
pub mod function;
pub mod global;
pub mod operator;
pub mod pkg;
pub mod primary;
pub mod program;
pub mod ret;
pub mod statement;
pub mod types;
#[derive(Debug, Clone, Copy)]
pub enum TerminatorEnum {
    NONE,
    RETURN,
    BREAK,
    CONTINUE,
}
impl TerminatorEnum {
    pub fn is_none(self) -> bool {
        if let TerminatorEnum::NONE = self {
            true
        } else {
            false
        }
    }
    pub fn is_return(self) -> bool {
        if let TerminatorEnum::RETURN = self {
            true
        } else {
            false
        }
    }
}
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
    FnValue(FunctionValue<'a>),
    STValue(StructType<'a>),
    ExFnValue((FunctionValue<'a>, PLType)),
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
            Value::STValue(_) => None,
            Value::FnValue(_) => None,
            Value::ExFnValue(_) => None,
            Value::None => None,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
#[enum_dispatch(Node, RangeTrait)]
pub enum NodeEnum {
    Def(DefNode),
    Ret(RetNode),
    Assign(AssignNode),
    If(IfNode),
    While(WhileNode),
    For(ForNode),
    Break(BreakNode),
    Continue(ContinueNode),
    Expr(BinOpNode),
    FuncDef(FuncDefNode),
    FuncCall(FuncCallNode),
    StructDef(StructDefNode),
    StructInit(StructInitNode),
    Take(TakeOpNode),
    Un(UnaryOpNode),
    Num(NumNode),
    Bool(BoolConstNode),
    Var(VarNode),
    Err(ErrorNode),
    STS(StatementsNode),
    Empty(EmptyNode),
    Comment(CommentNode),
    Program(program::ProgramNode),
    STInitField(StructInitFieldNode),
    STErrorNode(STErrorNode),
    Global(GlobalNode),
    UseNode(UseNode),
    ExternIDNode(ExternIDNode),
}

#[enum_dispatch]
pub trait RangeTrait {
    fn range(&self) -> Range;
}

#[enum_dispatch]
pub trait Node: RangeTrait + AsAny {
    fn format(&self, tabs: usize, prefix: &str) -> String;
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>);
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx>;
}

type NodeResult<'ctx> = Result<
    (
        Value<'ctx>,
        Option<String>, //type
        TerminatorEnum,
        bool, // isconst
    ),
    PLDiag,
>;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Num {
    INT(u64),
    FLOAT(f64),
}

impl Eq for Num {
    // FIXME: NaN https://stackoverflow.com/questions/39638363/how-can-i-use-a-hashmap-with-f64-as-key-in-rust
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
/**
 * 函数参数format
 */
pub fn print_params(paralist: &Vec<Box<TypedIdentifierNode>>) -> String {
    let mut str = String::new();
    let mut len = 0;
    for param in paralist.iter() {
        let name = &param.id.name;
        let mut id = String::new();
        if param.tp.is_ref {
            let ref_id = format!("&{}", &param.tp.id);
            id.push_str(&ref_id);
        } else {
            id.push_str(&param.tp.id)
        }
        str.push_str(name);
        str.push_str(": ");
        str.push_str(&id);
        len += 1;
        if len < paralist.len() {
            str.push_str(", ")
        }
    }
    return str;
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
                        left, right, "addtmp")),Some("i64".to_string()),TerminatorEnum::NONE,false));
                },
                (Value::FloatValue(left), Value::FloatValue(right)) => {
                    return Ok((Value::FloatValue($ctx.builder.[<build_$opf>](
                        left, right, "addtmp")),Some("f64".to_string()),TerminatorEnum::NONE,false));
                },
                _ =>  return Err($ctx.add_err(
                    $range,
                    crate::ast::diag::ErrorCode::UNRECOGNIZED_BIN_OPERATOR,
                ))
            }
        }
    };
}
