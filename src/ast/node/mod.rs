use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::ctx::Ctx;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use enum_dispatch::enum_dispatch;

use lsp_types::SemanticTokenType;

use self::comment::CommentNode;
use self::control::*;
use self::error::*;
use self::function::*;
use self::global::*;
use self::implement::ImplNode;
use self::interface::TraitDefNode;
use self::operator::*;
use self::pkg::{ExternIdNode, UseNode};
use self::pointer::PointerOpNode;
use self::primary::*;
use self::ret::*;
use self::statement::*;
use self::string_literal::StringNode;
use self::types::*;

use super::builder::ValueHandle;
use super::diag::ErrorCode;
use super::diag::PLDiag;
use super::fmt::FmtBuilder;
use super::pltype::{PLType, PriType};
use super::range::{Pos, Range};

pub mod comment;
pub mod control;
pub mod error;
pub mod function;
pub mod global;
pub mod implement;
pub mod interface;
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
    BasicTypeNode(TypeNameNode),
    ArrayTypeNode(ArrayTypeNameNode),
    PointerTypeNode(PointerTypeNode),
    FuncTypeNode(FuncDefNode),
}
#[enum_dispatch]
pub trait TypeNode: RangeTrait + FmtTrait + PrintTrait {
    /// 重要：这个函数不要干lsp相关操作，只用来获取type
    fn get_type<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> TypeNodeResult;
    fn emit_highlight<'a, 'ctx>(&self, ctx: &mut Ctx<'a>);
    fn eq_or_infer<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        pltype: Rc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<bool, PLDiag>;
}
type TypeNodeResult = Result<Rc<RefCell<PLType>>, PLDiag>;

#[derive(Clone, PartialEq, Eq, Debug)]
#[enum_dispatch(Node, RangeTrait, FmtTrait, PrintTrait)]
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
    TraitDefNode(TraitDefNode),
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
        Option<Rc<RefCell<PLType>>>, //type
        TerminatorEnum,
    ),
    PLDiag,
>;
pub struct PLValue {
    pub value: ValueHandle,
    pub is_const: bool,
    pub receiver: Option<ValueHandle>,
}
impl PLValue {
    pub fn set_const(&mut self, is_const: bool) {
        self.is_const = is_const;
    }
}
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Num {
    INT(u64),
    FLOAT(f64),
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
                    Some(("type `{}`".to_string(), vec![$got.get_name()])),
                )
                .add_label(
                    $exprange,
                    Some(("type `{}`".to_string(), vec![$expect.get_name()])),
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
        expect: Option<Rc<RefCell<PLType>>>,
        expectrange: Range,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        if expect.is_none() {
            return node.emit(self, builder);
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
                    Num::INT(i) => match &*expect.borrow() {
                        PLType::PRIMITIVE(tp) => {
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
                            let int = builder.int_value(tp, i, sign_ext);
                            int
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
                    Num::FLOAT(f) => match &*expect.borrow() {
                        PLType::PRIMITIVE(tp) => match tp {
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
                    TerminatorEnum::NONE,
                ));
            }
        }
        let re = node.emit(self, builder)?;
        let (value, ty, term) = re;
        if value.is_some() {
            if let Some(ty) = ty.clone() {
                if ty != expect {
                    return Err(mismatch_err!(
                        self,
                        range,
                        expectrange,
                        expect.borrow(),
                        ty.borrow()
                    ));
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

#[macro_export]
macro_rules! handle_calc {
    ($ctx:ident, $op:ident, $opf:ident, $lpltype:ident, $left:ident, $right:ident, $range: expr,$builder:ident) => {
        item! {
            match *$lpltype.clone().unwrap().borrow() {
                PLType::PRIMITIVE(PriType::I128|PriType::I64|PriType::I32|PriType::I16|PriType::I8|
                    PriType::U128|PriType::U64|PriType::U32|PriType::U16|PriType::U8) => {
                    return Ok((Some(plv!( $builder.[<build_int_$op>](
                        $left, $right, "addtmp"))),Some($lpltype.unwrap()),TerminatorEnum::NONE));
                },
                PLType::PRIMITIVE(PriType::F64|PriType::F32) => {
                    return Ok((Some(plv!( $builder.[<build_$opf>](
                        $left, $right, "addtmp"))),Some($lpltype.unwrap()),TerminatorEnum::NONE));
                },
                _ =>  return Err($ctx.add_diag(
                    $range.new_err(
                    crate::ast::diag::ErrorCode::UNRECOGNIZED_BIN_OPERATOR),
                ))
            }
        }
    };
}
