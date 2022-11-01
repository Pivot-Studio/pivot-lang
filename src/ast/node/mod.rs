use crate::ast::ctx::Ctx;

use as_any::AsAny;
use enum_dispatch::enum_dispatch;
use inkwell::basic_block::BasicBlock;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{AnyValueEnum, BasicValue, BasicValueEnum, PointerValue};

use self::comment::CommentNode;
use self::control::*;
use self::error::*;
use self::function::*;
use self::global::*;
use self::operator::*;
use self::pkg::{ExternIDNode, UseNode};
use self::pointer::PointerOpNode;
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
pub mod pointer;
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

#[derive(Debug, Clone, PartialEq, Eq)]
#[enum_dispatch(TypeNode, RangeTrait)]
pub enum TypeNodeEnum {
    BasicTypeNode(TypeNameNode),
    ArrayTypeNode(ArrayTypeNameNode),
    PointerTypeNode(PointerTypeNode),
}
#[enum_dispatch]
pub trait TypeNode: RangeTrait + AsAny {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>);
    fn get_type<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> TypeNodeResult<'ctx>;
    fn emit_highlight<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>);
}
type TypeNodeResult<'ctx> = Result<PLType, PLDiag>;

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
    ArrayInitNode(ArrayInitNode),
    ArrayElementNode(ArrayElementNode),
    PointerOpNode(PointerOpNode),
}
// ANCHOR: range
#[enum_dispatch]
pub trait RangeTrait {
    fn range(&self) -> Range;
}
// ANCHOR_END: range

// ANCHOR: node
#[enum_dispatch]
pub trait Node: RangeTrait + AsAny {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>);
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx>;
}
// ANCHOR_END: node
type NodeResult<'ctx> = Result<
    (
        Option<AnyValueEnum<'ctx>>,
        Option<PLType>, //type
        TerminatorEnum,
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
            ctx.gc_add_root(p.as_basic_value_enum(), builder);
            ctx.roots.borrow_mut().push(p.as_basic_value_enum());
            builder.position_at_end(ctx.block.unwrap());
            p
        }
        None => panic!("alloc get entry failed!"),
    }
}

#[macro_export]
macro_rules! handle_calc {
    ($ctx:ident, $op:ident, $opf:ident, $lpltype:ident, $left:ident, $right:ident, $range: expr) => {
        item! {
            match $lpltype.unwrap() {
                PLType::PRIMITIVE(PriType::I64) => {
                    return Ok((Some($ctx.builder.[<build_int_$op>](
                        $left.into_int_value(), $right.into_int_value(), "addtmp").into()),Some(PLType::PRIMITIVE(PriType::I64)),TerminatorEnum::NONE));
                },
                PLType::PRIMITIVE(PriType::F64) => {
                    return Ok((Some($ctx.builder.[<build_$opf>](
                        $left.into_float_value(), $right.into_float_value(), "addtmp").into()),Some(PLType::PRIMITIVE(PriType::F64)),TerminatorEnum::NONE));
                },
                _ =>  return Err($ctx.add_err(
                    $range,
                    crate::ast::diag::ErrorCode::UNRECOGNIZED_BIN_OPERATOR,
                ))
            }
        }
    };
}
