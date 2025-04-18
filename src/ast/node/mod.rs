use std::cell::RefCell;

use std::sync::Arc;

use crate::ast::ctx::Ctx;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use cast::ImplCastNode;
use enum_dispatch::enum_dispatch;

use intermediate_node::IntermediateNode;
use lsp_types::SemanticTokenType;

use self::cast::AsNode;
use self::cast::IsNode;
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
use self::node_result::NodeOutput;
use self::node_result::NodeResult;
use self::node_result::NodeValue;
use self::operator::*;
use self::pkg::{ExternIdNode, UseNode};
use self::pointer::PointerOpNode;
use self::primary::*;
use self::ret::*;
use self::statement::*;
use self::string_literal::StringNode;
use self::tuple::TupleInitNode;
use self::tuple::TupleTypeNode;
use self::types::*;
use self::union::UnionDefNode;

use super::ctx::EqRes;
use super::diag::ErrorCode;
use super::diag::PLDiag;
use super::fmt::FmtBuilder;
use super::pltype::get_type_deep;
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
pub mod intermediate_node;
pub mod macro_nodes;
pub mod node_result;
pub mod operator;
pub mod pkg;
pub mod pointer;
pub mod primary;
pub mod program;
pub mod ret;
pub mod statement;
pub mod string_literal;
pub mod tuple;
pub mod types;
pub mod union;

#[derive(Debug, Clone, PartialEq, Eq)]
#[enum_dispatch(TypeNode, RangeTrait, FmtTrait, PrintTrait)]
pub enum TypeNodeEnum {
    Basic(TypeNameNode),
    Array(ArrayTypeNameNode),
    Pointer(PointerTypeNode),
    Func(FuncDefNode),
    Tuple(TupleTypeNode),
    Closure(ClosureTypeNode),
    Custom(CustomTypeNode),
}
#[enum_dispatch]
pub trait TypeNode: RangeTrait + FmtTrait + PrintTrait {
    /// 重要：这个函数不要干lsp相关操作，只用来获取type
    fn get_type<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        gen_code: bool,
    ) -> TypeNodeResult;
    fn emit_highlight(&self, ctx: &mut Ctx);
    fn eq_or_infer<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        pltype: Arc<RefCell<PLType>>,
        builder: &'b BuilderEnum<'a, '_>,
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
    IsNode(IsNode),
    ImplCastNode(ImplCastNode),
    TupleInitNode(TupleInitNode),
    ClosureNode(ClosureNode),
    GlobalConstNode(GlobalConstNode),
    MatchNode(MatchNode),
    InterNode(IntermediateNode),
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult;
}

/// print trait to report the AST node structure
#[enum_dispatch]
pub trait PrintTrait {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>);
}

// ANCHOR_END: node

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Num {
    Int(u64),
    Float(f64),
    Char(char),
}

impl Eq for Num {
    // FIXME: NaN https://stackoverflow.com/questions/39638363/how-can-i-use-a-hashmap-with-f64-as-key-in-rust
}

#[macro_export]
macro_rules! mismatch_err {
    ($ctx:ident, $range:expr,$exprange:expr, $expect:expr,$got:expr) => {
        $ctx.add_diag(
            $range
                .new_err(ErrorCode::TYPE_MISMATCH)
                .add_label(
                    $range,
                    $ctx.get_file(),
                    $crate::format_label!("type `{}`", $got.get_name()),
                )
                .add_label(
                    $exprange,
                    $ctx.get_file(),
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

    /// # emit_with_expectation
    ///
    /// it emits the node and asserts the result with expected type
    fn emit_with_expectation<'b>(
        &'b mut self,
        node: &mut Box<NodeEnum>,
        expect: Arc<RefCell<PLType>>,
        expectrange: Range,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        self.expect_ty = Some(expect.clone());
        let range = node.range();
        let pri: Result<PrimaryNode, _> = (*node.clone()).try_into();
        // basic type implicit cast
        if let Ok(pri) = pri {
            let num: Result<NumNode, _> = (*pri.value.clone()).try_into();
            if let Ok(numnode) = num {
                self.expect_ty = None;
                self.emit_comment_highlight(&pri.comments[0]);
                let num = numnode.value;
                // TODO: check overflow
                let v = match num {
                    Num::Int(i) => match &*get_type_deep(expect.clone()).borrow() {
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
                    Num::Char(c) => builder.int_value(&PriType::CHAR, c as u64, false),
                };
                self.push_semantic_token(numnode.range(), SemanticTokenType::NUMBER, 0);
                self.emit_comment_highlight(&pri.comments[1]);

                return Ok(NodeOutput::new_value(NodeValue::new_const(v, expect)));
            }
        }
        let re = node.emit(self, builder);
        if let Err(re) = re {
            self.expect_ty = None;
            return Err(re);
        }
        let re = re.unwrap();
        self.expect_ty = None;
        if let Some(nv) = re.get_value() {
            let value = nv.get_value();
            let ty = nv.get_ty();
            // TODO: better way to check pltype eq with generic
            let ty = get_type_deep(ty);
            let expect = get_type_deep(expect);
            if ty != expect {
                // eprintln!("{:?}\n-------------\n{:?}", ty, expect);
                let handle =
                    self.up_cast(expect.clone(), ty, expectrange, range, value, builder)?;
                return Ok(NodeOutput::new_value(NodeValue::new_const(handle, expect)));
            }
        }
        Ok(re)
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
            match *$lpltype.clone().borrow() {
                PLType::Primitive(PriType::I128|PriType::I64|PriType::I32|PriType::I16|PriType::I8|
                    PriType::U128|PriType::U64|PriType::U32|PriType::U16|PriType::U8|PriType::CHAR) => {
                    return Ok(NodeOutput::new_value(NodeValue::new($builder.[<build_int_$op>](
                        $left, $right, "addtmp"), $lpltype)));
                },
                PLType::Primitive(PriType::F64|PriType::F32) => {
                    return Ok(NodeOutput::new_value(NodeValue::new($builder.[<build_$opf>](
                        $left, $right, "addtmp"), $lpltype)));
                },
                _ =>  return Err($ctx.add_diag(
                    $range.new_err(
                    $crate::ast::diag::ErrorCode::UNRECOGNIZED_BIN_OPERATOR),
                ))
            }
        }
    };
}
