use super::primary::VarNode;
use super::*;
use crate::ast::ctx::Ctx;
use crate::ast::pltype::PLType;
use crate::ast::pltype::PriType;
use crate::ast::tokens::TokenType;

use crate::ast::diag::ErrorCode;
use crate::handle_calc;
use inkwell::IntPredicate;
use internal_macro::range;
use lsp_types::SemanticTokenType;
use paste::item;
#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct UnaryOpNode {
    pub op: TokenType,
    pub exp: Box<NodeEnum>,
}
// 单目运算符
impl Node for UnaryOpNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::new();
        format_res.push_str(TokenType::get_str(&self.op));
        format_res.push_str(&self.exp.format(tabs, prefix));
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("UnaryOpNode");
        tab(tabs + 1, line.clone(), end);
        println!("{:?}", self.op);
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let exp_range = self.exp.range();
        let (exp, pltype, _) = self.exp.emit(ctx)?;
        if pltype.is_none() {
            return Err(ctx.add_err(self.range, ErrorCode::INVALID_UNARY_EXPRESSION));
        }
        let pltype = pltype.unwrap();
        let exp = ctx.try_load2var(exp_range, exp.unwrap())?;
        return Ok(match (&*pltype.borrow(), self.op) {
            (
                PLType::PRIMITIVE(
                    PriType::I128 | PriType::I64 | PriType::I32 | PriType::I16 | PriType::I8,
                ),
                TokenType::MINUS,
            ) => (
                Some(
                    ctx.builder
                        .build_int_neg(exp.into_int_value(), "negtmp")
                        .into(),
                ),
                Some(pltype.clone()),
                TerminatorEnum::NONE,
            ),
            (PLType::PRIMITIVE(PriType::F64 | PriType::F32), TokenType::MINUS) => (
                Some(
                    ctx.builder
                        .build_float_neg(exp.into_float_value(), "negtmp")
                        .into(),
                ),
                Some(pltype.clone()),
                TerminatorEnum::NONE,
            ),
            (PLType::PRIMITIVE(PriType::BOOL), TokenType::NOT) => (
                {
                    let bool_origin = ctx.builder.build_int_compare(
                        IntPredicate::EQ,
                        exp.into_int_value(),
                        ctx.context.i8_type().const_int(false as u64, true),
                        "nottmp",
                    );
                    Some(
                        ctx.builder
                            .build_int_z_extend(bool_origin, ctx.context.i8_type(), "zexttemp")
                            .into(),
                    )
                },
                Some(pltype.clone()),
                TerminatorEnum::NONE,
            ),
            (_exp, _op) => {
                return Err(ctx.add_err(self.range, ErrorCode::INVALID_UNARY_EXPRESSION));
            }
        });
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BinOpNode {
    pub left: Box<NodeEnum>,
    pub op: TokenType,
    pub right: Box<NodeEnum>,
}
impl Node for BinOpNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::new();
        format_res.push_str(&self.left.format(tabs, prefix));
        format_res.push_str(" ");
        format_res.push_str(TokenType::get_str(&self.op));
        format_res.push_str(" ");
        format_res.push_str(&self.right.format(tabs, prefix));
        return format_res;
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("BinOpNode");
        self.left.print(tabs + 1, false, line.clone());
        tab(tabs + 1, line.clone(), false);
        println!("{:?}", self.op);
        self.right.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let (lrange, rrange) = (self.left.range(), self.right.range());
        let (lv, lpltype, _) = self.left.emit(ctx)?;
        let (rv, _, _) = ctx.emit_with_expectation(&mut self.right, lpltype.clone())?;
        if lv.is_none() || rv.is_none() {
            return Err(ctx.add_err(self.range, ErrorCode::EXPECT_VALUE));
        }
        let left = ctx.try_load2var(lrange, lv.unwrap())?;
        let right = ctx.try_load2var(rrange, rv.unwrap())?;
        Ok(match self.op {
            TokenType::PLUS => {
                handle_calc!(ctx, add, float_add, lpltype, left, right, self.range)
            }
            TokenType::MINUS => {
                handle_calc!(ctx, sub, float_sub, lpltype, left, right, self.range)
            }
            TokenType::MUL => {
                handle_calc!(ctx, mul, float_mul, lpltype, left, right, self.range)
            }
            TokenType::DIV => {
                handle_calc!(ctx, signed_div, float_div, lpltype, left, right, self.range)
            }
            TokenType::EQ
            | TokenType::NE
            | TokenType::LEQ
            | TokenType::GEQ
            | TokenType::GREATER
            | TokenType::LESS => match *lpltype.unwrap().borrow() {
                PLType::PRIMITIVE(
                    PriType::I128
                    | PriType::I64
                    | PriType::I32
                    | PriType::I16
                    | PriType::I8
                    | PriType::U128
                    | PriType::U64
                    | PriType::U32
                    | PriType::U16
                    | PriType::U8,
                ) => (
                    {
                        let bool_origin = ctx.builder.build_int_compare(
                            self.op.get_op(),
                            left.into_int_value(),
                            right.into_int_value(),
                            "cmptmp",
                        );
                        Some(
                            ctx.builder
                                .build_int_z_extend(bool_origin, ctx.context.i8_type(), "zexttemp")
                                .into(),
                        )
                    },
                    Some(Rc::new(RefCell::new(PLType::PRIMITIVE(PriType::BOOL)))),
                    TerminatorEnum::NONE,
                ),
                PLType::PRIMITIVE(PriType::F64 | PriType::F32) => (
                    {
                        let bool_origin = ctx.builder.build_float_compare(
                            self.op.get_fop(),
                            left.into_float_value(),
                            right.into_float_value(),
                            "cmptmp",
                        );
                        Some(
                            ctx.builder
                                .build_int_z_extend(bool_origin, ctx.context.i8_type(), "zexttemp")
                                .into(),
                        )
                    },
                    Some(Rc::new(RefCell::new(PLType::PRIMITIVE(PriType::BOOL)))),
                    TerminatorEnum::NONE,
                ),
                _ => return Err(ctx.add_err(self.range, ErrorCode::VALUE_NOT_COMPARABLE)),
            },
            TokenType::AND => match *lpltype.unwrap().borrow() {
                PLType::PRIMITIVE(PriType::BOOL) => (
                    {
                        let bool_origin = ctx.builder.build_and(
                            left.into_int_value(),
                            right.into_int_value(),
                            "andtmp",
                        );
                        Some(
                            ctx.builder
                                .build_int_z_extend(bool_origin, ctx.context.i8_type(), "zext_temp")
                                .into(),
                        )
                    },
                    Some(Rc::new(RefCell::new(PLType::PRIMITIVE(PriType::BOOL)))),
                    TerminatorEnum::NONE,
                ),
                _ => return Err(ctx.add_err(self.range, ErrorCode::LOGIC_OP_NOT_BOOL)),
            },
            TokenType::OR => match *lpltype.unwrap().borrow() {
                PLType::PRIMITIVE(PriType::BOOL) => (
                    {
                        let bool_origin = ctx.builder.build_or(
                            left.into_int_value(),
                            right.into_int_value(),
                            "ortmp",
                        );
                        Some(
                            ctx.builder
                                .build_int_z_extend(bool_origin, ctx.context.i8_type(), "zext_temp")
                                .into(),
                        )
                    },
                    Some(Rc::new(RefCell::new(PLType::PRIMITIVE(PriType::BOOL)))),
                    TerminatorEnum::NONE,
                ),
                _ => {
                    return Err(
                        ctx.add_err(self.range, crate::ast::diag::ErrorCode::LOGIC_OP_NOT_BOOL)
                    )
                }
            },
            _ => return Err(ctx.add_err(self.range, ErrorCode::UNRECOGNIZED_BIN_OPERATOR)),
        })
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TakeOpNode {
    pub head: Box<NodeEnum>,
    pub field: Option<Box<VarNode>>,
}

impl Node for TakeOpNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::new();
        format_res.push_str(&self.head.format(tabs, prefix));
        for id in &self.field {
            format_res.push_str(".");
            format_res.push_str(&id.format(tabs, prefix));
        }
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TakeOpNode");
        self.head
            .print(tabs + 1, self.field.is_none(), line.clone());
        if let Some(id) = &self.field {
            id.print(tabs + 1, true, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let head = self.head.emit(ctx)?;
        let (mut res, pltype, _) = head;
        if pltype.is_none() {
            return Err(ctx.add_err(self.range, ErrorCode::INVALID_GET_FIELD));
        }
        let mut pltype = pltype.unwrap();
        if let Some(id) = &self.field {
            res = match res.unwrap().value {
                AnyValueEnum::PointerValue(s) => {
                    let (tp, s) = ctx.auto_deref(pltype, s);
                    let headptr = s;
                    pltype = tp;
                    let etype = s.get_type().get_element_type();
                    let index;
                    if etype.is_struct_type() {
                        // end with ".", gen completions
                        ctx.if_completion(|ctx, (pos, trigger)| {
                            if pos.is_in(id.range)
                                && trigger.is_some()
                                && trigger.as_ref().unwrap() == "."
                            {
                                if let PLType::STRUCT(s) = &*pltype.clone().borrow() {
                                    let completions = s.get_completions(ctx);
                                    ctx.completion_items.set(completions);
                                    ctx.action = None;
                                }
                            }
                        });
                        let range = id.range();
                        if let PLType::STRUCT(s) = &*pltype.clone().borrow() {
                            let field = s.fields.get(&id.name);
                            let method = s.find_method(&ctx, &id.name);
                            if let Some(field) = field {
                                ctx.push_semantic_token(range, SemanticTokenType::PROPERTY, 0);
                                index = field.index;
                                ctx.set_if_refs(field.refs.clone(), range);
                                ctx.send_if_go_to_def(range, field.range, s.path.clone());
                                pltype = field.typenode.get_type(ctx).unwrap().clone();
                            } else if let Some(mthd) = method {
                                ctx.push_semantic_token(range, SemanticTokenType::METHOD, 0);
                                let mut mthd = mthd.clone();
                                mthd.param_pltypes
                                    .insert(0, pltype.borrow().get_typenode(ctx));
                                ctx.send_if_go_to_def(
                                    range,
                                    mthd.range,
                                    mthd.llvmname.split("..").nth(0).unwrap().to_string(),
                                );
                                return Ok((
                                    Some(PLValue {
                                        value: mthd.get_or_insert_fn(ctx).into(),
                                        is_const: false,
                                        receiver: Some(headptr),
                                    }),
                                    Some(Rc::new(RefCell::new(PLType::FN(mthd)))),
                                    TerminatorEnum::NONE,
                                ));
                            } else {
                                return Err(
                                    ctx.add_err(id.range, ErrorCode::STRUCT_FIELD_NOT_FOUND)
                                );
                            }
                        } else {
                            unreachable!()
                        }
                    } else {
                        return Err(ctx.add_err(id.range, ErrorCode::INVALID_GET_FIELD));
                    }
                    Some(
                        ctx.builder
                            .build_struct_gep(s, index, "structgep")
                            .unwrap()
                            .into(),
                    )
                }
                _ => return Err(ctx.add_err(id.range, ErrorCode::ILLEGAL_GET_FIELD_OPERATION)),
            }
        }
        if self.field.is_none() {
            // end with ".", gen completions
            let tp = ctx.auto_deref_tp(pltype);
            ctx.if_completion(|ctx, (pos, trigger)| {
                if pos.is_in(self.range) && trigger.is_some() && trigger.as_ref().unwrap() == "." {
                    if let PLType::STRUCT(s) = &*tp.borrow() {
                        let completions = s.get_completions(&ctx);
                        ctx.completion_items.set(completions);
                        ctx.action = None;
                    }
                }
            });
            return Err(ctx.add_err(self.range, crate::ast::diag::ErrorCode::COMPLETION));
        }
        Ok((res, Some(pltype.clone()), TerminatorEnum::NONE))
    }
}
