use super::primary::VarNode;
use super::*;
use crate::ast::ctx::Ctx;
use crate::ast::ctx::PLType;
use crate::ast::tokens::TokenType;

use crate::handle_calc;
use inkwell::IntPredicate;
use internal_macro::range;
use paste::item;

#[range]
pub struct UnaryOpNode {
    pub op: TokenType,
    pub exp: Box<dyn Node>,
}
impl Node for UnaryOpNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("UnaryOpNode");
        tab(tabs + 1, line.clone(), end);
        println!("{:?}", self.op);
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
        let (exp, pltype) = self.exp.emit(ctx);
        let exp = ctx.try_load(exp);
        return match (exp, self.op) {
            (Value::IntValue(exp), TokenType::MINUS) => (
                Value::IntValue(ctx.builder.build_int_neg(exp, "negtmp")),
                pltype,
            ),
            (Value::FloatValue(exp), TokenType::MINUS) => (
                Value::FloatValue(ctx.builder.build_float_neg(exp, "negtmp")),
                pltype,
            ),
            (Value::BoolValue(exp), TokenType::NOT) => (
                Value::BoolValue(ctx.builder.build_int_compare(
                    IntPredicate::EQ,
                    exp,
                    ctx.context.bool_type().const_int(false as u64, true),
                    "nottmp",
                )),
                pltype,
            ),
            (_exp, _op) => (
                Value::Err(ctx.add_err(
                    self.range,
                    crate::ast::error::ErrorCode::INVALID_UNARY_EXPRESSION,
                )),
                None,
            ),
        };
    }
}

#[range]
pub struct BinOpNode {
    pub left: Box<dyn Node>,
    pub op: TokenType,
    pub right: Box<dyn Node>,
}
impl Node for BinOpNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("BinOpNode");
        self.left.print(tabs + 1, false, line.clone());
        tab(tabs + 1, line.clone(), false);
        println!("{:?}", self.op);
        self.right.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
        let (lv, lpltype) = self.left.emit(ctx);
        let left = ctx.try_load(lv);
        let (rv, rpltype) = self.right.emit(ctx);
        let right = ctx.try_load(rv);
        if lpltype != rpltype {
            return (
                Value::Err(ctx.add_err(
                    self.range,
                    crate::ast::error::ErrorCode::BIN_OP_TYPE_MISMATCH,
                )),
                None,
            );
        }
        match self.op {
            TokenType::PLUS => handle_calc!(ctx, add, float_add, left, right),
            TokenType::MINUS => handle_calc!(ctx, sub, float_sub, left, right),
            TokenType::MUL => handle_calc!(ctx, mul, float_mul, left, right),
            TokenType::DIV => handle_calc!(ctx, signed_div, float_div, left, right),
            TokenType::EQ
            | TokenType::NE
            | TokenType::LEQ
            | TokenType::GEQ
            | TokenType::GREATER
            | TokenType::LESS => match (left, right) {
                (Value::IntValue(lhs), Value::IntValue(rhs)) => (
                    Value::BoolValue(ctx.builder.build_int_compare(
                        self.op.get_op(),
                        lhs,
                        rhs,
                        "cmptmp",
                    )),
                    Some("bool".to_string()),
                ),
                (Value::FloatValue(lhs), Value::FloatValue(rhs)) => (
                    Value::BoolValue(ctx.builder.build_float_compare(
                        self.op.get_fop(),
                        lhs,
                        rhs,
                        "cmptmp",
                    )),
                    Some("bool".to_string()),
                ),
                _ => (
                    Value::Err(ctx.add_err(
                        self.range,
                        crate::ast::error::ErrorCode::VALUE_NOT_COMPARABLE,
                    )),
                    None,
                ),
            },
            TokenType::AND => match (left, right) {
                (Value::BoolValue(lhs), Value::BoolValue(rhs)) => (
                    Value::BoolValue(ctx.builder.build_and(lhs, rhs, "andtmp")),
                    Some("bool".to_string()),
                ),
                _ => (
                    Value::Err(
                        ctx.add_err(self.range, crate::ast::error::ErrorCode::LOGIC_OP_NOT_BOOL),
                    ),
                    None,
                ),
            },
            TokenType::OR => match (left, right) {
                (Value::BoolValue(lhs), Value::BoolValue(rhs)) => (
                    Value::BoolValue(ctx.builder.build_or(lhs, rhs, "ortmp")),
                    Some("bool".to_string()),
                ),
                _ => (
                    Value::Err(
                        ctx.add_err(self.range, crate::ast::error::ErrorCode::LOGIC_OP_NOT_BOOL),
                    ),
                    None,
                ),
            },
            _ => (
                Value::Err(ctx.add_err(
                    self.range,
                    crate::ast::error::ErrorCode::UNRECOGNIZED_BIN_OPERATOR,
                )),
                None,
            ),
        }
    }
}

#[range]
pub struct TakeOpNode {
    pub head: Box<dyn Node>,
    pub ids: Vec<Box<VarNode>>,
}

impl Node for TakeOpNode {
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
        let head = self.head.emit(ctx);
        // let head = ctx.try_load(head);
        let (mut res, mut pltype) = head;
        for id in &self.ids {
            res = match res.as_basic_value_enum() {
                BasicValueEnum::PointerValue(s) => {
                    let etype = s.get_type().get_element_type();
                    let index;
                    if etype.is_struct_type() {
                        let st = etype.into_struct_type();
                        let tpname = st.get_name().unwrap().to_str().unwrap();
                        let (tp, _) = ctx.get_type(tpname).unwrap();
                        if let PLType::STRUCT(s) = tp {
                            let field = s.fields.get(&id.name);
                            if let Some(field) = field {
                                index = field.index;
                                pltype = Some(field.typename.id.clone());
                            } else {
                                return (
                                    Value::Err(ctx.add_err(
                                        id.range,
                                        crate::ast::error::ErrorCode::STRUCT_FIELD_NOT_FOUND,
                                    )),
                                    None,
                                );
                            }
                        } else {
                            panic!("not implemented");
                        }
                    } else {
                        return (
                            Value::Err(ctx.add_err(
                                id.range,
                                crate::ast::error::ErrorCode::INVALID_GET_FIELD,
                            )),
                            None,
                        );
                    }
                    Value::VarValue(ctx.builder.build_struct_gep(s, index, "structgep").unwrap())
                }
                _ => panic!("not implemented {:?}", res),
            }
        }
        (res, pltype)
    }
}
