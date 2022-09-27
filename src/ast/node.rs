use crate::ast::ctx::Ctx;
use crate::ast::range::RangeTrait;
use crate::ast::tokens::TokenType;
use as_any::AsAny;
use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, IntValue, PointerValue};
use inkwell::IntPredicate;
use internal_macro::range;
use paste::item;
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
    None,
}

impl<'a> Value<'a> {
    fn as_basic_value_enum(&self) -> BasicValueEnum<'a> {
        match self {
            Value::IntValue(v) => v.as_basic_value_enum(),
            Value::FloatValue(v) => v.as_basic_value_enum(),
            Value::VarValue(v) => v.as_basic_value_enum(),
            Value::BoolValue(v) => v.as_basic_value_enum(),
            Value::None => panic!("not implemented"),
        }
    }
}

pub trait Node: RangeTrait + AsAny {
    fn print(&self);
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx>;
}
macro_rules! handle_calc {
    ($ctx:ident, $op:ident, $opf:ident  ,$left:ident, $right:ident) => {
        item! {
            match ($left, $right) {
                (Value::IntValue(left), Value::IntValue(right)) => {
                    return Value::IntValue($ctx.builder.[<build_int_$op>](
                        left, right, "addtmp"));
                },
                (Value::FloatValue(left), Value::FloatValue(right)) => {
                    return Value::FloatValue($ctx.builder.[<build_$opf>](
                        left, right, "addtmp"));
                },
                _ => panic!("not implemented")
            }
        }
    };
}
#[range]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct BoolConstNode {
    pub value: bool,
}

impl Node for BoolConstNode {
    fn print(&self) {
        println!("BoolConstNode:");
        println!("{:?}", self.value)
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        Value::BoolValue(ctx.context.bool_type().const_int(self.value as u64, true))
    }
}
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Num {
    INT(u64),
    FLOAT(f64),
}
#[range]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct NumNode {
    pub value: Num,
}
impl Node for NumNode {
    fn print(&self) {
        println!("NumNode:");
        println!("{:?}", self.value)
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        if let Num::INT(x) = self.value {
            let b = ctx.context.i64_type().const_int(x, true);
            return Value::IntValue(b);
        } else if let Num::FLOAT(x) = self.value {
            let b = ctx.context.f64_type().const_float(x);
            return Value::FloatValue(b);
        }
        panic!("not implemented")
    }
}
#[range]
pub struct StatementsNode {
    pub statements: Vec<Box<dyn Node>>,
}
impl Node for StatementsNode {
    fn print(&self) {
        println!("StatementsNode:");
        for e in self.statements.iter() {
            e.print();
        }
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let child = & mut ctx.new_child();
        for m in self.statements.iter_mut() {
            m.emit(child);
        }
        Value::None
    }
}

#[range]
pub struct ProgramNode {
    pub statements: Vec<Box<dyn Node>>,
}
impl Node for ProgramNode {
    fn print(&self) {
        println!("ProgramNode:");
        for e in self.statements.iter() {
            e.print();
        }
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        for m in self.statements.iter_mut() {
            m.emit(ctx);
        }
        Value::None
    }
}

#[range]
pub struct NLNode {}

impl Node for NLNode {
    fn print(&self) {
        println!("NLNode");
    }

    fn emit<'a, 'ctx>(&'a mut self, _: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        Value::None
    }
}

#[range]
pub struct IfNode {
    pub cond: Box<dyn Node>,
    pub then: Box<dyn Node>,
}

impl Node for IfNode {
    fn print(&self) {
        println!("IfNode:");
        self.cond.print();
        self.then.print();
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let cond_block = ctx.context.append_basic_block(ctx.function, "cond");
        let then_block = ctx.context.append_basic_block(ctx.function, "then");
        let after_block = ctx.context.append_basic_block(ctx.function, "after");
        ctx.builder.build_unconditional_branch(cond_block);
        ctx.builder.position_at_end(cond_block);
        let cond = self.cond.emit(ctx);
        let cond = match cond {
            Value::BoolValue(v) => v,
            _ => panic!("not implemented"),
        };
        ctx.builder
            .build_conditional_branch(cond, then_block, after_block);
        ctx.builder.position_at_end(then_block);
        self.then.emit(ctx);
        ctx.builder.build_unconditional_branch(after_block);
        ctx.builder.position_at_end(after_block);
        Value::None
    }
}

#[range]
pub struct WhileNode {
    pub cond: Box<dyn Node>,
    pub body: Box<dyn Node>,
}

impl Node for WhileNode {
    fn print(&self) {
        println!("WhileNode:");
        self.cond.print();
        self.body.print();
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let cond_block = ctx.context.append_basic_block(ctx.function, "cond");
        let body_block = ctx.context.append_basic_block(ctx.function, "body");
        let after_block = ctx.context.append_basic_block(ctx.function, "after");
        ctx.builder.build_unconditional_branch(cond_block);
        ctx.builder.position_at_end(cond_block);
        let cond = self.cond.emit(ctx);
        let cond = match cond {
            Value::BoolValue(v) => v,
            _ => panic!("not implemented"),
        };
        ctx.builder
            .build_conditional_branch(cond, body_block, after_block);
        ctx.builder.position_at_end(body_block);
        self.body.emit(ctx);
        ctx.builder.build_unconditional_branch(cond_block);
        ctx.builder.position_at_end(after_block);
        Value::None
    }
}

#[range]
pub struct UnaryOpNode {
    pub op: TokenType,
    pub exp: Box<dyn Node>,
}
impl Node for UnaryOpNode {
    fn print(&self) {
        println!("UnaryOpNode");
        println!("{:?}", self.op);
        self.exp.print();
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
    fn print(&self) {
        println!("BinOpNode");
        self.left.print();
        println!("{:?}", self.op);
        self.right.print();
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
#[range]
#[derive(Debug, PartialEq, Clone)]
pub struct VarNode {
    pub name: String,
}
impl Node for VarNode {
    fn print(&self) {
        println!("VarNode:");
        println!("{}", self.name)
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let v = ctx.get_symbol(&self.name);
        if let Some(v) = v {
            return Value::VarValue(*v);
        }
        todo!(); // TODO: 未定义的变量
    }
}
#[range]
pub struct DefNode {
    pub var: VarNode,
    pub exp: Box<dyn Node>,
}
impl Node for DefNode {
    fn print(&self) {
        println!("DefNode:");
        self.var.print();
        println!("=");
        self.exp.print();
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let v = self.exp.emit(ctx);
        let e = v.as_basic_value_enum();
        let tp = e.get_type();
        let p = ctx.builder.build_alloca(tp, &self.var.name);
        ctx.builder.build_store(p, e);

        ctx.add_symbol(self.var.name.clone(), p);
        Value::None
    }
}
#[range]
pub struct AssignNode {
    pub var: VarNode,
    pub exp: Box<dyn Node>,
}
impl Node for AssignNode {
    fn print(&self) {
        println!("AssignNode:");
        self.var.print();
        println!("=");
        self.exp.print();
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let pt = self.var.emit(ctx);
        let value = self.exp.emit(ctx);
        if let Value::VarValue(ptr) = pt {
            let e = value.as_basic_value_enum();
            ctx.builder.build_store(ptr, e);
            return Value::None;
        }

        todo!()
    }
}
