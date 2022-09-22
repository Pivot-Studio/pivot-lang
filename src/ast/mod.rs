use self::ctx::{Ctx, MutCtx};
use as_any::AsAny;
use inkwell::{
    execution_engine::JitFunction,
    types::BasicType,
    values::{BasicValue, FloatValue, IntValue, PointerValue},
};
use nom_locate::LocatedSpan;
use paste::item;
use range_marco::range;
type Span<'a> = LocatedSpan<&'a str>;
use lazy_static::lazy_static;
use std::{borrow::Cow, collections::HashMap, hash::Hash};
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Pos {
    pub line: usize,   // 1based
    pub column: usize, // 1based
    pub offset: usize, // 0based
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Range {
    pub start: Pos,
    pub end: Pos,
}
pub trait RangeTrait {
    fn range(&self) -> Range;
}
impl Pos {
    pub fn to(&self, end: Pos) -> Range {
        Range { start: *self, end }
    }
}

impl Range {
    pub fn new(start: Span, end: Span) -> Range {
        Range {
            start: Pos {
                line: start.location_line() as usize,
                column: start.get_column(),
                offset: start.location_offset(),
            },
            end: Pos {
                line: end.location_line() as usize,
                column: end.get_column(),
                offset: end.location_offset(),
            },
        }
    }
}
macro_rules! define_tokens {
    ($(
        $ident:ident : $string_keyword:expr
    ),*) => {
        #[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
        pub enum TokenType {
            $($ident),*
        }
        $(pub const $ident: &'static str = $string_keyword;)*
        lazy_static! {
            pub static ref TOKEN_TYPE_MAP: HashMap<TokenType, &'static str> = {
                let mut mp = HashMap::new();
                $(mp.insert(TokenType::$ident, $ident);)*
                mp
            };
        }
    };
}
define_tokens!(
    PLUS : "+",
    MINUS : "-",  // -
    MUL : "*",    // *
    DIV : "/",    // /
    LPAREN : "(", // (
    RPAREN : ")", // )
    ASSIGN : "=", // =
    LET : "let" // let
);
impl TokenType {
    pub fn get_str(&self) -> &'static str {
        TOKEN_TYPE_MAP[self]
    }
}
pub mod ctx;
#[range]
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct NumNode {
    pub value: Num,
}

#[range]
pub struct StatementsNode {
    pub statements: Vec<Box<dyn Node>>,
}
#[range]
pub struct UnaryOpNode {
    pub op: TokenType,
    pub exp: Box<dyn Node>,
}

impl Node for StatementsNode {
    fn print(&self) {
        println!("StatementsNode:");
        for e in self.statements.iter() {
            e.print();
        }
    }

    fn emit<'a, 'ctx>(
        &'a mut self,
        ctx: &'a Ctx<'a, 'ctx>,
        mutctx: &mut MutCtx<'a, 'ctx>,
    ) -> Value<'ctx> {
        for m in self.statements.iter_mut() {
            m.emit(ctx, mutctx);
        }
        Value::None
    }
}

#[range]
pub struct BinOpNode {
    pub left: Box<dyn Node>,
    pub op: TokenType,
    pub right: Box<dyn Node>,
}

#[range]
#[derive(Debug, PartialEq, Clone)]
pub struct VarNode {
    pub name: String,
}

// fn test<'a: 'c, 'b: 'c + 'd, 'c, 'd>(mutctx: &'a mut MutCtx< 'd>, n: &'b Box<dyn Node>) {}

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

    fn emit<'a, 'ctx>(
        &'a mut self,
        ctx: &'a Ctx<'a, 'ctx>,
        mutctx: &mut MutCtx<'a, 'ctx>,
    ) -> Value<'ctx> {
        let pt;
        let v = self.exp.emit(ctx, mutctx);
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

        mutctx.add_symbol(self.var.name.clone(), pt);
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

    fn emit<'a, 'ctx>(
        &'a mut self,
        ctx: &'a Ctx<'a, 'ctx>,
        mutctx: &mut MutCtx<'a, 'ctx>,
    ) -> Value<'ctx> {
        let pt = self.var.emit(ctx, mutctx);
        let value = self.exp.emit(ctx, mutctx);
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
        }

        todo!()
    }
}

impl Node for VarNode {
    fn print(&self) {
        println!("VarNode:");
        println!("{}", self.name)
    }
    fn emit<'a, 'ctx>(
        &'a mut self,
        ctx: &'a Ctx<'a, 'ctx>,
        mutctx: &mut MutCtx<'a, 'ctx>,
    ) -> Value<'ctx> {
        let v = mutctx.get_symbol(&self.name);
        if let Some(v) = v {
            return Value::VarValue(*v);
        }
        todo!(); // TODO: 未定义的变量
    }
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
    VarValue(PointerValue<'a>),
    None,
}

pub trait Node: RangeTrait + AsAny {
    fn print(&self);
    fn emit<'a, 'ctx>(
        &'a mut self,
        ctx: &'a Ctx<'a, 'ctx>,
        mutctx: &mut MutCtx<'a, 'ctx>,
    ) -> Value<'ctx>;
}

impl Node for NumNode {
    fn print(&self) {
        println!("NumNode:");
        println!("{:?}", self.value)
    }
    fn emit<'a, 'ctx>(
        &'a mut self,
        ctx: &'a Ctx<'a, 'ctx>,
        mutctx: &mut MutCtx<'a, 'ctx>,
    ) -> Value<'ctx> {
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
    fn emit<'a, 'ctx>(
        &'a mut self,
        ctx: &'a Ctx<'a, 'ctx>,
        mutctx: &mut MutCtx<'a, 'ctx>,
    ) -> Value<'ctx> {
        let left = self.left.emit(ctx, mutctx);
        let right = self.right.emit(ctx, mutctx);
        match self.op {
            TokenType::PLUS => handle_calc!(ctx, add, float_add, left, right),
            TokenType::MINUS => handle_calc!(ctx, sub, float_sub, left, right),
            TokenType::MUL => handle_calc!(ctx, mul, float_mul, left, right),
            TokenType::DIV => handle_calc!(ctx, signed_div, float_div, left, right),
            op => panic!("expected op token but found {:?}", op),
        }
    }
}

impl Node for UnaryOpNode {
    fn print(&self) {
        println!("UnaryOpNode");
        println!("{:?}", self.op);
        self.exp.print();
    }
    fn emit<'a, 'ctx>(
        &'a mut self,
        ctx: &'a Ctx<'a, 'ctx>,
        mutctx: &mut MutCtx<'a, 'ctx>,
    ) -> Value<'ctx> {
        let exp = self.exp.emit(ctx, mutctx);
        return match exp {
            Value::IntValue(exp) => Value::IntValue(ctx.builder.build_int_neg(exp, "negtmp")),
            Value::FloatValue(exp) => Value::FloatValue(ctx.builder.build_float_neg(exp, "negtmp")),
            _ => panic!("not implemented"),
        };
    }
}

type MainFunc = unsafe extern "C" fn() -> i64;
#[test]
fn test_nom() {
    use crate::nomparser::PLParser;
    use inkwell::context::Context;
    use inkwell::values::AnyValue;
    let mut parser = PLParser::new("let a = 2*(3+2)+5\n");
    let (_, mut node) = parser.parse().unwrap();
    let tp = &Context::create();
    let bd = tp.create_builder();
    let mo = tp.create_module("test");
    let context = ctx::Ctx::new(tp, &bd, &mo);
    let mut mc = ctx::MutCtx::new(context.context, context.module);
    context.builder.position_at_end(mc.basic_block);
    let ctx = &context;
    let m = &mut mc;
    node.print();
    let re = node.emit(ctx, m);
    // if let Value::IntValue(re) = re {
    //     assert!(re.print_to_string().to_string() == "i64 114")
    // } else {
    //     panic!("not implemented")
    // }
    println!("emit succ");
    let v = mc.get_symbol("a");
    let v = v.unwrap();
    let load = context.builder.build_load(*v, "load");
    context.builder.build_return(Some(&load));
    println!( "{}",context.module.to_string());
    
    let execution_engine = context
        .module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .unwrap();
    unsafe {
        let f = execution_engine.get_function::<MainFunc>("main").unwrap();
        let ret = f.call();
        println!("a = {}",ret);
        assert_eq!(ret, 15)
    }
}
