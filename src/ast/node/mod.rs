use std::cell::RefCell;

use std::sync::Arc;

use crate::ast::ctx::Ctx;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use enum_dispatch::enum_dispatch;

use lsp_types::SemanticTokenType;

use self::cast::AsNode;
use self::comment::CommentNode;
use self::control::*;
use self::error::*;
use self::function::*;
use self::global::*;
use self::implement::ImplNode;
use self::interface::TraitDefNode;
use self::macro_nodes::MacroCallNode;
use self::macro_nodes::MacroLoopStatementNode;
use self::macro_nodes::MacroNode;
use self::operator::*;
use self::pkg::{ExternIdNode, UseNode};
use self::pointer::PointerOpNode;
use self::primary::*;
use self::ret::*;
use self::statement::*;
use self::string_literal::StringNode;
use self::types::*;
use self::union::UnionDefNode;

use super::builder::ValueHandle;
use super::ctx::EqRes;
use super::diag::ErrorCode;
use super::diag::PLDiag;
use super::fmt::FmtBuilder;
use super::pltype::{PLType, PriType};
use super::range::{Pos, Range};

pub mod cast;
pub mod comment;
pub mod control;
pub mod error;
pub mod function;
pub mod global;
pub mod implement;
pub mod interface;
pub mod macro_nodes;
pub mod operator;
pub mod pkg;
pub mod pointer;
pub mod primary;
pub mod program;
pub mod ret;
pub mod statement;
pub mod string_literal;
pub mod types;
pub mod union;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TerminatorEnum {
    None,
    Return,
    Break,
    Continue,
}
impl TerminatorEnum {
    pub fn is_none(self) -> bool {
        self == TerminatorEnum::None
    }
    pub fn is_return(self) -> bool {
        self == TerminatorEnum::Return
    }
}
#[macro_export]
macro_rules! plv {
    ($e:expr) => {
        PLValue {
            value: $e,
            is_const: false,
            receiver: None,
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[enum_dispatch(TypeNode, RangeTrait, FmtTrait, PrintTrait)]
pub enum TypeNodeEnum {
    Basic(TypeNameNode),
    Array(ArrayTypeNameNode),
    Pointer(PointerTypeNode),
    Func(FuncDefNode),
}
#[enum_dispatch]
pub trait TypeNode: RangeTrait + FmtTrait + PrintTrait {
    /// 重要：这个函数不要干lsp相关操作，只用来获取type
    fn get_type<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> TypeNodeResult;
    fn emit_highlight(&self, ctx: &mut Ctx);
    fn eq_or_infer<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        pltype: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<EqRes, PLDiag>;
}
type TypeNodeResult = Result<Arc<RefCell<PLType>>, PLDiag>;

#[derive(Clone, PartialEq, Eq, Debug)]
#[enum_dispatch(Node, RangeTrait, FmtTrait, PrintTrait)]
pub enum NodeEnum {
    Var(VarNode),
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
    Sts(StatementsNode),
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
    TraitDefNode(TraitDefNode),
    MacroLoopStatementNode(MacroLoopStatementNode),
    MacroNode(MacroNode),
    MacroCallNode(MacroCallNode),
    UnionDefNode(UnionDefNode),
    AsNode(AsNode),
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
pub trait Node: RangeTrait + FmtTrait + PrintTrait {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult;
}

#[enum_dispatch]
pub trait PrintTrait {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>);
}

// ANCHOR_END: node
pub type NodeResult = Result<
    (
        Option<PLValue>,
        Option<Arc<RefCell<PLType>>>, //type
        TerminatorEnum,
    ),
    PLDiag,
>;
pub struct PLValue {
    pub value: ValueHandle,
    pub is_const: bool,
    pub receiver: Option<(ValueHandle, Option<Arc<RefCell<PLType>>>)>,
}
impl PLValue {
    pub fn set_const(&mut self, is_const: bool) {
        self.is_const = is_const;
    }
}
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Num {
    Int(u64),
    Float(f64),
}

impl Eq for Num {
    // FIXME: NaN https://stackoverflow.com/questions/39638363/how-can-i-use-a-hashmap-with-f64-as-key-in-rust
}
#[macro_export]
macro_rules! mismatch_err {
    ($self:ident, $range:expr,$exprange:expr, $expect:expr,$got:expr) => {
        $self.add_diag(
            $range
                .new_err(ErrorCode::TYPE_MISMATCH)
                .add_label(
                    $range,
                    $self.get_file(),
                    $crate::format_label!("type `{}`", $got.get_name()),
                )
                .add_label(
                    $exprange,
                    $self.get_file(),
                    $crate::format_label!("type `{}`", $expect.get_name()),
                )
                .clone(),
        )
    };
}

impl<'a, 'ctx> Ctx<'a> {
    fn emit_comment_highlight(&mut self, comments: &Vec<Box<NodeEnum>>) {
        for com in comments {
            self.push_semantic_token(com.range(), SemanticTokenType::COMMENT, 0);
        }
    }

    fn emit_with_expectation<'b>(
        &'b mut self,
        node: &mut Box<NodeEnum>,
        expect: Option<Arc<RefCell<PLType>>>,
        expectrange: Range,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        if expect.is_none() {
            return node.emit(self, builder);
        }
        let expect = expect.unwrap();
        let range = node.range();
        let pri: Result<PrimaryNode, _> = (*node.clone()).try_into();
        // basic type implicit cast
        if let Ok(pri) = pri {
            let num: Result<NumNode, _> = (*pri.value.clone()).try_into();
            if let Ok(numnode) = num {
                self.emit_comment_highlight(&pri.comments[0]);
                let num = numnode.value;
                // TODO: check overflow
                let v = match num {
                    Num::Int(i) => match &*expect.borrow() {
                        PLType::Primitive(tp) => {
                            let sign_ext = match tp {
                                PriType::I8
                                | PriType::I16
                                | PriType::I32
                                | PriType::I64
                                | PriType::I128 => true,
                                PriType::U128
                                | PriType::U64
                                | PriType::U32
                                | PriType::U16
                                | PriType::U8 => false,
                                _ => {
                                    return Err(mismatch_err!(
                                        self,
                                        range,
                                        expectrange,
                                        expect.borrow(),
                                        &PriType::I64
                                    ))
                                }
                            };

                            builder.int_value(tp, i, sign_ext)
                        }
                        _ => {
                            return Err(mismatch_err!(
                                self,
                                range,
                                expectrange,
                                expect.borrow(),
                                &PriType::I64
                            ))
                        }
                    },
                    Num::Float(f) => match &*expect.borrow() {
                        PLType::Primitive(tp) => match tp {
                            PriType::F32 | PriType::F64 => builder.float_value(tp, f),
                            _ => {
                                return Err(mismatch_err!(
                                    self,
                                    range,
                                    expectrange,
                                    expect.borrow(),
                                    &PriType::F64
                                ))
                            }
                        },
                        _ => {
                            return Err(mismatch_err!(
                                self,
                                range,
                                expectrange,
                                expect.borrow(),
                                &PriType::F64
                            ))
                        }
                    },
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
                    TerminatorEnum::None,
                ));
            }
        }
        let (value, ty, term) = node.emit(self, builder)?;
        if let (Some(value), Some(ty)) = (&value, &ty) {
            if *ty != expect {
                let handle = self.up_cast(
                    expect.clone(),
                    ty.clone(),
                    expectrange,
                    range,
                    value.value,
                    builder,
                )?;
                return Ok((
                    Some(PLValue {
                        value: handle,
                        is_const: true,
                        receiver: None,
                    }),
                    Some(expect),
                    term,
                ));
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
    for item in line.iter().take(tabs) {
        if *item {
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
    for item in paralist.iter().skip(1) {
        str += &format!(
            ", {}: {}",
            item.id.name,
            FmtBuilder::generate_node(&item.typenode)
        );
    }
    str
}

#[macro_export]
macro_rules! handle_calc {
    ($ctx:ident, $op:ident, $opf:ident, $lpltype:ident, $left:ident, $right:ident, $range: expr,$builder:ident) => {
        paste::item! {
            match *$lpltype.clone().unwrap().borrow() {
                PLType::Primitive(PriType::I128|PriType::I64|PriType::I32|PriType::I16|PriType::I8|
                    PriType::U128|PriType::U64|PriType::U32|PriType::U16|PriType::U8) => {
                    return Ok((Some(plv!( $builder.[<build_int_$op>](
                        $left, $right, "addtmp"))),Some($lpltype.unwrap()),TerminatorEnum::None));
                },
                PLType::Primitive(PriType::F64|PriType::F32) => {
                    return Ok((Some(plv!( $builder.[<build_$opf>](
                        $left, $right, "addtmp"))),Some($lpltype.unwrap()),TerminatorEnum::None));
                },
                _ =>  return Err($ctx.add_diag(
                    $range.new_err(
                    $crate::ast::diag::ErrorCode::UNRECOGNIZED_BIN_OPERATOR),
                ))
            }
        }
    };
}
