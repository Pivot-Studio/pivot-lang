use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::ctx::Ctx;

use as_any::AsAny;
use enum_dispatch::enum_dispatch;
use inkwell::basic_block::BasicBlock;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{
    AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, FloatValue, FunctionValue, IntValue,
    PointerValue,
};
use lsp_types::SemanticTokenType;

use self::comment::CommentNode;
use self::control::*;
use self::error::*;
use self::function::*;
use self::global::*;
use self::implement::ImplNode;
use self::operator::*;
use self::pkg::{ExternIdNode, UseNode};
use self::pointer::PointerOpNode;
use self::primary::*;
use self::ret::*;
use self::statement::*;
use self::string_literal::StringNode;
use self::types::*;

use super::ctx::PLDiag;
use super::diag::ErrorCode;
use super::fmt::FmtBuilder;
use super::pltype::PLType;
use super::range::{Pos, Range};

pub mod comment;
pub mod control;
pub mod error;
pub mod function;
pub mod global;
pub mod implement;
pub mod operator;
pub mod pkg;
pub mod pointer;
pub mod primary;
pub mod program;
pub mod ret;
pub mod statement;
pub mod string_literal;
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
#[enum_dispatch(TypeNode, RangeTrait, FmtTrait)]
pub enum TypeNodeEnum {
    BasicTypeNode(TypeNameNode),
    ArrayTypeNode(ArrayTypeNameNode),
    PointerTypeNode(PointerTypeNode),
}
#[enum_dispatch]
pub trait TypeNode: RangeTrait + AsAny + FmtTrait {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>);
    /// 重要：这个函数不要干lsp相关操作，只用来获取type
    fn get_type<'a, 'ctx>(&self, ctx: &mut Ctx<'a, 'ctx>) -> TypeNodeResult<'ctx>;
    fn emit_highlight<'a, 'ctx>(&self, ctx: &mut Ctx<'a, 'ctx>);
    fn eq_or_infer<'a, 'ctx>(
        &self,
        ctx: &mut Ctx<'a, 'ctx>,
        pltype: Rc<RefCell<PLType>>,
    ) -> Result<bool, PLDiag>;
}
type TypeNodeResult<'ctx> = Result<Rc<RefCell<PLType>>, PLDiag>;

#[derive(Clone, PartialEq, Eq, Debug)]
#[enum_dispatch(Node, RangeTrait, FmtTrait)]
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
    Primary(PrimaryNode),
    Num(NumNode),
    Bool(BoolConstNode),
    Err(ErrorNode),
    STS(StatementsNode),
    Empty(EmptyNode),
    Comment(CommentNode),
    Program(program::ProgramNode),
    STInitField(StructInitFieldNode),
    StErrorNode(StErrorNode),
    Global(GlobalNode),
    UseNode(UseNode),
    ExternIdNode(ExternIdNode),
    ArrayInitNode(ArrayInitNode),
    ArrayElementNode(ArrayElementNode),
    PointerOpNode(PointerOpNode),
    ParanthesesNode(ParanthesesNode),
    ImplNode(ImplNode),
    StringNode(StringNode),
}
// ANCHOR: range
#[enum_dispatch]
pub trait RangeTrait {
    fn range(&self) -> Range;
}
// ANCHOR_END: range

// ANCHOR: fmtnode
#[enum_dispatch]
pub trait FmtTrait {
    fn format(&self, builder: &mut FmtBuilder);
}
// ANCHOR_END: fmtnode

// ANCHOR: node
#[enum_dispatch]
pub trait Node: RangeTrait + AsAny + FmtTrait {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>);
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx>;
}
// ANCHOR_END: node
pub type NodeResult<'ctx> = Result<
    (
        Option<PLValue<'ctx>>,
        Option<Rc<RefCell<PLType>>>, //type
        TerminatorEnum,
    ),
    PLDiag,
>;
pub struct PLValue<'ctx> {
    pub value: AnyValueEnum<'ctx>,
    pub is_const: bool,
    pub receiver: Option<PointerValue<'ctx>>,
}
impl<'ctx> PLValue<'ctx> {
    pub fn into_pointer_value(&self) -> PointerValue<'ctx> {
        self.value.into_pointer_value()
    }
    pub fn into_int_value(&self) -> IntValue<'ctx> {
        self.value.into_int_value()
    }
    pub fn into_function_value(&self) -> FunctionValue<'ctx> {
        self.value.into_function_value()
    }
    pub fn set_const(&mut self, is_const: bool) {
        self.is_const = is_const;
    }
}
macro_rules! impl_plvalue_into {
    ($($args:ident),*) => (
        $(
            impl<'ctx> From<$args<'ctx>> for PLValue<'ctx> {
                fn from(value: $args<'ctx>) -> Self {
                    Self {
                        value: value.into(),
                        is_const: false,
                        receiver: None,
                    }
                }
            }
        )*
    );
}
impl_plvalue_into!(
    FunctionValue,
    FloatValue,
    IntValue,
    PointerValue,
    AnyValueEnum,
    BasicValueEnum
);
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Num {
    INT(u64),
    FLOAT(f64),
}

impl Eq for Num {
    // FIXME: NaN https://stackoverflow.com/questions/39638363/how-can-i-use-a-hashmap-with-f64-as-key-in-rust
}

impl<'a, 'ctx> Ctx<'a, 'ctx> {
    pub fn position_at_end(&mut self, block: BasicBlock<'ctx>) {
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
    fn emit_comment_highlight(&mut self, comments: &Vec<Box<NodeEnum>>) {
        for com in comments {
            self.push_semantic_token(com.range(), SemanticTokenType::COMMENT, 0);
        }
    }
    fn emit_with_expectation(
        &mut self,
        node: &mut Box<NodeEnum>,
        expect: Option<Rc<RefCell<PLType>>>,
    ) -> NodeResult<'ctx> {
        if expect.is_none() {
            return node.emit(self);
        }
        let expect = expect.unwrap();
        let range = node.range();
        let pri: Result<PrimaryNode, _> = (*node.clone()).try_into();
        if let Ok(pri) = pri {
            let num: Result<NumNode, _> = (*pri.value.clone()).try_into();
            if let Ok(numnode) = num {
                self.emit_comment_highlight(&pri.comments[0]);
                let num = numnode.value;
                // TODO: check overflow
                let v = match num {
                    Num::INT(i) => {
                        if !expect.borrow().get_basic_type(self).is_int_type() {
                            return Err(self.add_err(node.range(), ErrorCode::TYPE_MISMATCH));
                        }
                        let int = expect
                            .borrow()
                            .get_basic_type(self)
                            .into_int_type()
                            .const_int(i, false);
                        int.as_any_value_enum()
                    }
                    Num::FLOAT(f) => {
                        if !expect.borrow().get_basic_type(self).is_float_type() {
                            return Err(self.add_err(node.range(), ErrorCode::TYPE_MISMATCH));
                        }
                        let float = expect
                            .borrow()
                            .get_basic_type(self)
                            .into_float_type()
                            .const_float(f);
                        float.as_any_value_enum()
                    }
                };
                self.push_semantic_token(numnode.range(), SemanticTokenType::NUMBER, 0);
                self.emit_comment_highlight(&pri.comments[1]);
                return Ok((
                    Some(PLValue {
                        value: v,
                        is_const: true,
                        receiver: None,
                    }),
                    Some(expect),
                    TerminatorEnum::NONE,
                ));
            }
        }
        let re = node.emit(self)?;
        let (value, ty, term) = re;
        if value.is_some() {
            if let Some(ty) = ty.clone() {
                if ty != expect {
                    return Err(self.add_err(range, ErrorCode::TYPE_MISMATCH));
                }
            }
        }
        Ok((value, ty, term))
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
pub fn print_params(paralist: &[Box<TypedIdentifierNode>]) -> String {
    let mut str = String::new();
    if paralist.is_empty() {
        return str;
    }
    if paralist[0].id.name == "self" {
        return print_params(&paralist[1..]);
    } else {
        str += &format!(
            "{}: {}",
            paralist[0].id.name,
            FmtBuilder::generate_node(&paralist[0].typenode)
        );
    }
    for i in 1..paralist.len() {
        str += &format!(
            ", {}: {}",
            paralist[i].id.name,
            FmtBuilder::generate_node(&paralist[i].typenode)
        );
    }
    return str;
}

pub fn alloc<'a, 'ctx>(
    ctx: &mut Ctx<'a, 'ctx>,
    tp: BasicTypeEnum<'ctx>,
    name: &str,
) -> PointerValue<'ctx> {
    let builder = ctx.nodebug_builder;
    ctx.block = Some(builder.get_insert_block().unwrap());
    match ctx.function.unwrap().get_first_basic_block() {
        Some(alloca) => {
            builder.position_at_end(alloca);
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
            match *$lpltype.clone().unwrap().borrow() {
                PLType::PRIMITIVE(PriType::I128|PriType::I64|PriType::I32|PriType::I16|PriType::I8|
                    PriType::U128|PriType::U64|PriType::U32|PriType::U16|PriType::U8) => {
                    return Ok((Some($ctx.builder.[<build_int_$op>](
                        $left.into_int_value(), $right.into_int_value(), "addtmp").into()),Some($lpltype.unwrap()),TerminatorEnum::NONE));
                },
                PLType::PRIMITIVE(PriType::F64|PriType::F32) => {
                    return Ok((Some($ctx.builder.[<build_$opf>](
                        $left.into_float_value(), $right.into_float_value(), "addtmp").into()),Some($lpltype.unwrap()),TerminatorEnum::NONE));
                },
                _ =>  return Err($ctx.add_err(
                    $range,
                    crate::ast::diag::ErrorCode::UNRECOGNIZED_BIN_OPERATOR,
                ))
            }
        }
    };
}
