use super::primary::VarNode;
use super::*;
use crate::ast::ctx::Ctx;
use crate::ast::ctx::PLType;
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let (exp, pltype, _, is_const) = self.exp.emit(ctx)?;
        if let (&Value::VarValue(_), TokenType::REF) = (&exp, self.op) {
            if is_const {
                return Err(ctx.add_err(self.range, ErrorCode::REF_CONST));
            }
            if let Value::VarValue(exp) = ctx.try_load2ptr(exp) {
                return Ok((Value::RefValue(exp), pltype, TerminatorEnum::NONE, false));
            }
            todo!()
        }
        let exp = ctx.try_load2var(exp);
        return Ok(match (exp, self.op) {
            (Value::IntValue(exp), TokenType::MINUS) => (
                Value::IntValue(ctx.builder.build_int_neg(exp, "negtmp")),
                pltype,
                TerminatorEnum::NONE,
                is_const,
            ),
            (Value::FloatValue(exp), TokenType::MINUS) => (
                Value::FloatValue(ctx.builder.build_float_neg(exp, "negtmp")),
                pltype,
                TerminatorEnum::NONE,
                is_const,
            ),
            (Value::BoolValue(exp), TokenType::NOT) => (
                Value::BoolValue({
                    let bool_origin = ctx.builder.build_int_compare(
                        IntPredicate::EQ,
                        exp,
                        ctx.context.i8_type().const_int(false as u64, true),
                        "nottmp",
                    );
                    ctx.builder
                        .build_int_z_extend(bool_origin, ctx.context.i8_type(), "zexttemp")
                }),
                pltype,
                TerminatorEnum::NONE,
                is_const,
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let (lv, lpltype, _, _) = self.left.emit(ctx)?;
        let left = ctx.try_load2var(lv);
        let (rv, rpltype, _, _) = self.right.emit(ctx)?;
        let right = ctx.try_load2var(rv);
        if lpltype != rpltype {
            return Err(ctx.add_err(
                self.range,
                crate::ast::diag::ErrorCode::BIN_OP_TYPE_MISMATCH,
            ));
        }
        Ok(match self.op {
            TokenType::PLUS => handle_calc!(ctx, add, float_add, left, right, self.range),
            TokenType::MINUS => handle_calc!(ctx, sub, float_sub, left, right, self.range),
            TokenType::MUL => handle_calc!(ctx, mul, float_mul, left, right, self.range),
            TokenType::DIV => handle_calc!(ctx, signed_div, float_div, left, right, self.range),
            TokenType::EQ
            | TokenType::NE
            | TokenType::LEQ
            | TokenType::GEQ
            | TokenType::GREATER
            | TokenType::LESS => match (left, right) {
                (Value::IntValue(lhs), Value::IntValue(rhs)) => (
                    Value::BoolValue({
                        let bool_origin =
                            ctx.builder
                                .build_int_compare(self.op.get_op(), lhs, rhs, "cmptmp");
                        ctx.builder.build_int_z_extend(
                            bool_origin,
                            ctx.context.i8_type(),
                            "zexttemp",
                        )
                    }),
                    Some("bool".to_string()),
                    TerminatorEnum::NONE,
                    true,
                ),
                (Value::FloatValue(lhs), Value::FloatValue(rhs)) => (
                    Value::BoolValue({
                        let bool_origin =
                            ctx.builder
                                .build_float_compare(self.op.get_fop(), lhs, rhs, "cmptmp");
                        ctx.builder.build_int_z_extend(
                            bool_origin,
                            ctx.context.i8_type(),
                            "zexttemp",
                        )
                    }),
                    Some("bool".to_string()),
                    TerminatorEnum::NONE,
                    true,
                ),
                _ => {
                    return Err(ctx.add_err(
                        self.range,
                        crate::ast::diag::ErrorCode::VALUE_NOT_COMPARABLE,
                    ))
                }
            },
            TokenType::AND => match (left, right) {
                (Value::BoolValue(lhs), Value::BoolValue(rhs)) => (
                    Value::BoolValue({
                        let bool_origin = ctx.builder.build_and(lhs, rhs, "andtmp");
                        ctx.builder.build_int_z_extend(
                            bool_origin,
                            ctx.context.i8_type(),
                            "zext_temp",
                        )
                    }),
                    Some("bool".to_string()),
                    TerminatorEnum::NONE,
                    true,
                ),
                _ => {
                    return Err(
                        ctx.add_err(self.range, crate::ast::diag::ErrorCode::LOGIC_OP_NOT_BOOL)
                    )
                }
            },
            TokenType::OR => match (left, right) {
                (Value::BoolValue(lhs), Value::BoolValue(rhs)) => (
                    Value::BoolValue({
                        let bool_origin = ctx.builder.build_or(lhs, rhs, "ortmp");
                        ctx.builder.build_int_z_extend(
                            bool_origin,
                            ctx.context.i8_type(),
                            "zext_temp",
                        )
                    }),
                    Some("bool".to_string()),
                    TerminatorEnum::NONE,
                    true,
                ),
                _ => {
                    return Err(
                        ctx.add_err(self.range, crate::ast::diag::ErrorCode::LOGIC_OP_NOT_BOOL)
                    )
                }
            },
            _ => {
                return Err(ctx.add_err(
                    self.range,
                    crate::ast::diag::ErrorCode::UNRECOGNIZED_BIN_OPERATOR,
                ))
            }
        })
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TakeOpNode {
    pub head: Box<NodeEnum>,
    pub ids: Vec<Box<VarNode>>,
    pub complete: bool,
}

impl Node for TakeOpNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::new();
        format_res.push_str(&self.head.format(tabs, prefix));
        for id in &self.ids {
            format_res.push_str(".");
            format_res.push_str(&id.format(tabs, prefix));
        }
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TakeOpNode");
        let mut i = self.ids.len();
        self.head.print(tabs + 1, i == 0, line.clone());
        for id in &self.ids {
            i -= 1;
            id.print(tabs + 1, i == 0, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let mut range = self.head.range();
        let head = self.head.emit(ctx)?;
        let (mut res, mut pltype, _, is_const) = head;
        if self.ids.len() != 0 {
            res = ctx.try_load2ptr(res);
        }
        for id in &self.ids {
            res = match res.as_basic_value_enum() {
                BasicValueEnum::PointerValue(s) => {
                    let etype = s.get_type().get_element_type();
                    let index;
                    if etype.is_struct_type() {
                        let st = etype.into_struct_type();
                        let tpname = &ctx
                            .plmod
                            .get_short_name(st.get_name().unwrap().to_str().unwrap());
                        // end with ".", gen completions
                        ctx.if_completion(|ctx, (pos, trigger)| {
                            if pos.is_in(id.range)
                                && trigger.is_some()
                                && trigger.as_ref().unwrap() == "."
                            {
                                let tp = ctx.get_type(&pltype.unwrap(), self.range).unwrap();
                                if let PLType::STRUCT(s) = tp {
                                    let completions = s.get_completions();
                                    ctx.completion_items.set(completions);
                                    ctx.action = None;
                                }
                            }
                        });
                        let tp = ctx.get_type(tpname, range).expect(tpname);
                        range = id.range();
                        ctx.push_semantic_token(range, SemanticTokenType::PROPERTY, 0);
                        if let PLType::STRUCT(s) = tp {
                            let field = s.fields.get(&id.name);
                            if let Some(field) = field {
                                index = field.index;
                                pltype = Some(field.typename.id.clone());
                                ctx.set_if_refs(field.refs.clone(), range);
                                ctx.send_if_go_to_def(range, field.range, ctx.plmod.path.clone());
                            } else {
                                return Err(ctx.add_err(
                                    id.range,
                                    crate::ast::diag::ErrorCode::STRUCT_FIELD_NOT_FOUND,
                                ));
                            }
                        } else {
                            panic!("not implemented");
                        }
                    } else {
                        return Err(
                            ctx.add_err(id.range, crate::ast::diag::ErrorCode::INVALID_GET_FIELD)
                        );
                    }
                    Value::VarValue(ctx.builder.build_struct_gep(s, index, "structgep").unwrap())
                }
                _ => panic!("not implemented {:?}", res),
            }
        }
        if !self.complete {
            // end with ".", gen completions
            ctx.if_completion(|ctx, (pos, trigger)| {
                if pos.is_in(self.range) && trigger.is_some() && trigger.as_ref().unwrap() == "." {
                    let tp = ctx.get_type(&pltype.unwrap(), self.range).unwrap();
                    if let PLType::STRUCT(s) = tp {
                        let completions = s.get_completions();
                        ctx.completion_items.set(completions);
                        ctx.action = None;
                    }
                }
            });

            return Err(ctx.add_err(self.range, crate::ast::diag::ErrorCode::COMPLETION));
        }
        Ok((res, pltype, TerminatorEnum::NONE, is_const))
    }
}
