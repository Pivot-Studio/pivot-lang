use crate::ast::ctx::Ctx;
use crate::ast::range::RangeTrait;
use crate::ast::tokens::TokenType;
use as_any::AsAny;
use inkwell::values::{FloatValue, IntValue, PointerValue};
use paste::item;
use range_marco::range;
/// # Value
/// 表达每个ast在计算之后产生的值  
///
/// 只有expression才会产生值，而statement不会产生值。对于statement，
/// 我们可以用None来表示
pub enum Value<'a> {
    IntValue(IntValue<'a>),
    FloatValue(FloatValue<'a>),
    VarValue(PointerValue<'a>),
    None,
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
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Num {
    INT(i64),
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
            let b = ctx.context.i64_type().const_int(x as u64, false);
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
        for m in self.statements.iter_mut() {
            m.emit(ctx);
        }
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
        return match exp {
            Value::IntValue(exp) => Value::IntValue(ctx.builder.build_int_neg(exp, "negtmp")),
            Value::FloatValue(exp) => Value::FloatValue(ctx.builder.build_float_neg(exp, "negtmp")),
            _ => panic!("not implemented"),
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
        let pt;
        let v = self.exp.emit(ctx);
        match v {
            Value::IntValue(v) => {
                let tp = v.get_type();
                let p = ctx.builder.build_alloca(tp, &self.var.name);
                pt = p;
                ctx.builder.build_store(pt, v);
            }
            Value::FloatValue(v) => {
                let p = ctx.builder.build_alloca(v.get_type(), &self.var.name);
                pt = p;
                ctx.builder.build_store(pt, v);
            }
            _ => todo!(),
        }

        ctx.add_symbol(self.var.name.clone(), pt);
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
            match value {
                Value::IntValue(v) => {
                    ctx.builder.build_store(ptr, v);
                }
                Value::FloatValue(v) => {
                    ctx.builder.build_store(ptr, v);
                }
                _ => todo!(),
            }
            return Value::None;
        }

        todo!()
    }
}
