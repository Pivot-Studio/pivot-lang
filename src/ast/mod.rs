use self::ctx::Ctx;
use crate::lexer::{pos::Range, types::Operator};
use inkwell::values::AnyValue;
use paste::item;

pub mod ctx;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct NumNode {
    pub value: Num,
    pub range: Range,
}

pub struct UnaryOpNode {
    pub op: Operator,
    pub exp: Box<dyn Node>,
    pub range: Range,
}

pub struct BinOpNode {
    pub left: Box<dyn Node>,
    pub op: Operator,
    pub right: Box<dyn Node>,
    pub range: Range,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Num {
    INT(i64),
    FLOAT(f64),
}

pub trait Node {
    fn print(&self);
    fn emit<'b>(&'b mut self, ctx: &'b Ctx) -> Option<Box<dyn AnyValue + 'b>>;
    fn range(&self) -> Range;
}

impl Node for NumNode {
    fn print(&self) {
        println!("{:?}", self.value)
    }
    fn emit<'b>(&'b mut self, ctx: &'b Ctx) -> Option<Box<dyn AnyValue + 'b>> {
        if let Num::INT(x) = self.value {
            let b = Box::new(ctx.context.i64_type().const_int(x as u64, false));
            return Some(b);
        }
        panic!("not implemented")
    }
    fn range(&self) -> Range {
        self.range
    }
}

macro_rules! handle_calc {
    ($ctx:ident, $op:ident, $opf:ident  ,$left:ident, $right:ident) => {
        item! {
            if let (Some(left), Some(right)) = ($left, $right) {
                let lefte = left.as_any_value_enum();
                let righte = right.as_any_value_enum();
                if lefte.is_int_value() && righte.is_int_value() {
                    return Some(Box::new($ctx.builder.[<build_int_$op>](
                        lefte.into_int_value(), righte.into_int_value(), "addtmp")));
                }
                else if lefte.is_float_value() && righte.is_float_value() {
                    return Some(Box::new($ctx.builder.[<build_$opf>](
                        lefte.into_float_value(), righte.into_float_value(), "addtmp")));
                }
                else{
                    panic!("not implemented")
                }
            }else {
                panic!("not implemented")
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
    fn emit<'b>(&'b mut self, ctx: &'b Ctx) -> Option<Box<dyn AnyValue + 'b>> {
        let left = self.left.emit(ctx);
        let right = self.right.emit(ctx);
        match self.op {
            Operator::PLUS => handle_calc!(ctx, add, float_add, left, right),
            Operator::MINUS => handle_calc!(ctx, sub, float_sub, left, right),
            Operator::MUL => handle_calc!(ctx, mul, float_mul, left, right),
            Operator::DIV => handle_calc!(ctx, signed_div, float_div, left, right),
        }
    }
    fn range(&self) -> Range {
        self.range
    }
}

impl Node for UnaryOpNode {
    fn print(&self) {
        println!("UnaryOpNode");
        println!("{:?}", self.op);
        self.exp.print();
    }
    fn emit<'b>(&'b mut self, ctx: &'b Ctx) -> Option<Box<dyn AnyValue + 'b>> {
        let exp = self.exp.emit(ctx);
        if let Some(exp) = exp {
            let exp = exp.as_any_value_enum();
            if exp.is_int_value() {
                return Some(Box::new(
                    ctx.builder.build_int_neg(exp.into_int_value(), "negtmp"),
                ));
            } else if exp.is_float_value() {
                return Some(Box::new(
                    ctx.builder
                        .build_float_neg(exp.into_float_value(), "negtmp"),
                ));
            } else {
                panic!("not implemented")
            }
        }
        panic!("not implemented")
    }
    fn range(&self) -> Range {
        self.range
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
    if let Some(re) = re {
        assert!(re.print_to_string().to_string() == "i64 114")
    } else {
        panic!("not implemented")
    }
}
