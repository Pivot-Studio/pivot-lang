use self::ctx::Ctx;
use crate::lexer::{pos::RangeTrait, types::Operator};
use inkwell::values::{AnyValue, FloatValue, GenericValue, IntValue};
use paste::item;
use range_marco::range;

pub mod ctx;
#[range]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct NumNode {
    pub value: Num,
}
#[range]
pub struct UnaryOpNode {
    pub op: Operator,
    pub exp: Box<dyn Node>,
}
#[range]
pub struct BinOpNode {
    pub left: Box<dyn Node>,
    pub op: Operator,
    pub right: Box<dyn Node>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Num {
    INT(i64),
    FLOAT(f64),
}

/// # Value
/// 表达每个ast在计算之后产生的值  
/// 
/// 只有expression才会产生值，而statement不会产生值。对于statement，
/// 我们可以用None来表示
pub enum Value<'a> {
    IntValue(IntValue<'a>),
    FloatValue(FloatValue<'a>),
    None,
}

pub trait Node: RangeTrait {
    fn print(&self);
    fn emit<'b>(&'b mut self, ctx: &'b Ctx) -> Value<'b>;
}

impl Node for NumNode {
    fn print(&self) {
        println!("{:?}", self.value)
    }
    fn emit<'b>(&'b mut self, ctx: &'b Ctx) -> Value<'b> {
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

impl Node for BinOpNode {
    fn print(&self) {
        println!("BinOpNode");
        self.left.print();
        println!("{:?}", self.op);
        self.right.print();
    }
    fn emit<'b>(&'b mut self, ctx: &'b Ctx) -> Value<'b> {
        let left = self.left.emit(ctx);
        let right = self.right.emit(ctx);
        match self.op {
            Operator::PLUS => handle_calc!(ctx, add, float_add, left, right),
            Operator::MINUS => handle_calc!(ctx, sub, float_sub, left, right),
            Operator::MUL => handle_calc!(ctx, mul, float_mul, left, right),
            Operator::DIV => handle_calc!(ctx, signed_div, float_div, left, right),
        }
    }
}

impl Node for UnaryOpNode {
    fn print(&self) {
        println!("UnaryOpNode");
        println!("{:?}", self.op);
        self.exp.print();
    }
    fn emit<'b>(&'b mut self, ctx: &'b Ctx) -> Value<'b> {
        let exp = self.exp.emit(ctx);
        return match exp {
            Value::IntValue(exp) => Value::IntValue(ctx.builder.build_int_neg(exp, "negtmp")),
            Value::FloatValue(exp) => Value::FloatValue(ctx.builder.build_float_neg(exp, "negtmp")),
            _ => panic!("not implemented"),
        };
    }
}

#[test]
fn test_ast() {
    use crate::parser::Parser;
    use inkwell::context::Context;
    let mut parser = Parser::new("4+11*(8--2)");
    let mut node = parser.parse().unwrap();
    let tp = &Context::create();
    let context = ctx::Ctx::new(tp);
    let re = node.emit(&context);
    if let Value::IntValue(re) = re {
        assert!(re.print_to_string().to_string() == "i64 114")
    } else {
        panic!("not implemented")
    }
}

#[test]
fn test_nom() {
    use crate::nomparser::Parser;
    use inkwell::context::Context;
    let mut parser = Parser::new("4+11*(8--2)");
    let (_, mut node) = parser.parse().unwrap();
    let tp = &Context::create();
    let context = ctx::Ctx::new(tp);
    let re = node.emit(&context);
    if let Value::IntValue(re) = re {
        assert!(re.print_to_string().to_string() == "i64 114")
    } else {
        panic!("not implemented")
    }
}
