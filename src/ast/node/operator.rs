use super::*;
use crate::ast::ctx::Ctx;
use crate::utils::tabs;
use crate::ast::tokens::TokenType;

use string_builder::Builder;
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
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(UnaryOpNode ");
        tabs::print_tabs(&mut builder, tabs + 1);
        builder.append(format!("{:?}", self.op));
        builder.append(self.exp.string(tabs + 1));
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let exp = self.exp.emit(ctx);
        let exp = ctx.try_load(exp);
        return match (exp, self.op) {
            (Value::IntValue(exp), TokenType::MINUS) => {
                Value::IntValue(ctx.builder.build_int_neg(exp, "negtmp"))
            }
            (Value::FloatValue(exp), TokenType::MINUS) => {
                Value::FloatValue(ctx.builder.build_float_neg(exp, "negtmp"))
            }
            (Value::BoolValue(exp), TokenType::NOT) => {
                Value::BoolValue(ctx.builder.build_int_compare(
                    IntPredicate::EQ,
                    exp,
                    ctx.context.bool_type().const_int(false as u64, true),
                    "nottmp",
                ))
            }
            (exp, op) => panic!("not implemented,get exp {:?},op {:?}", exp, op),
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
    fn string(&self, tabs: usize) -> String {
        let mut builder = Builder::default();
        tabs::print_tabs(&mut builder, tabs);
        builder.append("(BinOpNode ");
        builder.append(self.left.string(tabs + 1));
        tabs::print_tabs(&mut builder, tabs + 1);
        builder.append(format!("{:?}", self.op));
        builder.append(self.right.string(tabs + 1));
        tabs::print_tabs(&mut builder, tabs);
        builder.append(")");
        builder.string().unwrap()
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let lv = self.left.emit(ctx);
        let left = ctx.try_load(lv);
        let rv = self.right.emit(ctx);
        let right = ctx.try_load(rv);
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
                (Value::IntValue(lhs), Value::IntValue(rhs)) => Value::BoolValue(
                    ctx.builder
                        .build_int_compare(self.op.get_op(), lhs, rhs, "cmptmp"),
                ),
                (Value::FloatValue(lhs), Value::FloatValue(rhs)) => Value::BoolValue(
                    ctx.builder
                        .build_float_compare(self.op.get_fop(), lhs, rhs, "cmptmp"),
                ),
                _ => panic!("not implemented"),
            },
            TokenType::AND => match (left, right) {
                (Value::BoolValue(lhs), Value::BoolValue(rhs)) => {
                    Value::BoolValue(ctx.builder.build_and(lhs, rhs, "andtmp"))
                }
                _ => panic!("not implemented"),
            },
            TokenType::OR => match (left, right) {
                (Value::BoolValue(lhs), Value::BoolValue(rhs)) => {
                    Value::BoolValue(ctx.builder.build_or(lhs, rhs, "ortmp"))
                }
                _ => panic!("not implemented"),
            },
            op => panic!("expected op token but found {:?}", op),
        }
    }
}
