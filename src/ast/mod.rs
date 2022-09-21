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
use std::{collections::HashMap, hash::Hash, borrow::Cow};
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
        todo!()
    }

    fn emit<'a, 'b: 'a>(
        &'b mut self,
        ctx: &'b Ctx,
        mutctx: Option<&'b mut MutCtx<'b, 'b>>,
        immutctx: Option<MutCtx<'a, 'b>>,
    ) -> Value<'b> {
        let t = &mut mutctx.unwrap().table;
        for m in self.statements.iter_mut() {
            let mut n = MutCtx::new(ctx.context, ctx.module.clone());
            n.table = t.clone();
            m.emit(ctx, Some(& mut n), None);
            for v in n.table.iter() {
                t.insert(v.0.clone(), v.1.clone());
            }
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

#[range]
pub struct DefNode {
    pub var: VarNode,
    pub exp: Box<dyn Node>,
}

impl Node for DefNode {
    fn print(&self) {
        todo!()
    }

    fn emit<'a, 'b: 'a>(
        &'b mut self,
        ctx: &'b Ctx,
        mutctx: Option<&'b mut MutCtx<'b, 'b>>,
        immutctx: Option<MutCtx<'a, 'b>>,
    ) -> Value<'b> {
        let c = mutctx.unwrap();
        let immu = c.clone();
        let pt;
        let v = self.exp.emit(ctx, None, Some(immu));
        match v {
            Value::IntValue(v) => {
                let p = ctx.builder.build_alloca(v.get_type(), &self.var.name);
                pt = p.clone();
                ctx.builder.build_store(pt, v);
            }
            Value::FloatValue(v) => {
                let p = ctx.builder.build_alloca(v.get_type(), &self.var.name);
                pt = p.clone();
                ctx.builder.build_store(pt, v);
            }
            _ => todo!(),
        }
        c.add_symbol(&self.var.name, pt);
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
        todo!()
    }

    fn emit<'a, 'b: 'a>(
        &'b mut self,
        ctx: &'b Ctx,
        mutctx: Option<&'b mut MutCtx<'b, 'b>>,
        immutctx: Option<MutCtx<'a, 'b>>,
    ) -> Value<'b> {
        let immu = get_immut(mutctx, immutctx);
        let pt = self.var.emit(ctx, None, immu.clone());
        let value = self.exp.emit(ctx, None, immu);
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
        println!("var: {}", self.name)
    }
    fn emit<'a, 'b: 'a>(
        &'b mut self,
        ctx: &'b Ctx,
        mutctx: Option<&'b mut MutCtx<'b, 'b>>,
        immutctx: Option<MutCtx<'a, 'b>>,
    ) -> Value<'b> {
        let c = get_immut(mutctx, immutctx).unwrap();
        let v = c.get_symbol(&self.name);
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
    fn emit<'a, 'b: 'a>(
        &'b mut self,
        ctx: &'b Ctx,
        mutctx: Option<&'b mut MutCtx<'b, 'b>>,
        immutctx: Option<MutCtx<'a, 'b>>,
    ) -> Value<'b>;
}

impl Node for NumNode {
    fn print(&self) {
        println!("{:?}", self.value)
    }
    fn emit<'a, 'b: 'a>(
        &'b mut self,
        ctx: &'b Ctx,
        mutctx: Option<&'b mut MutCtx<'b, 'b>>,
        immutctx: Option<MutCtx<'a, 'b>>,
    ) -> Value<'b> {
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
    fn emit<'a, 'b: 'a>(
        &'b mut self,
        ctx: &'b Ctx,
        mutctx: Option<&'b mut MutCtx<'b, 'b>>,
        immutctx: Option<MutCtx<'a, 'b>>,
    ) -> Value<'b> {
        let c = get_immut(mutctx, immutctx);
        let left = self.left.emit(ctx, None, c.clone());
        let right = self.right.emit(ctx, None, c);
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
    fn emit<'a, 'b: 'a>(
        &'b mut self,
        ctx: &'b Ctx,
        mutctx: Option<&'b mut MutCtx<'b, 'b>>,
        immutctx: Option<MutCtx<'a, 'b>>,
    ) -> Value<'b> {
        let exp = self.exp.emit(ctx, mutctx, immutctx);
        return match exp {
            Value::IntValue(exp) => Value::IntValue(ctx.builder.build_int_neg(exp, "negtmp")),
            Value::FloatValue(exp) => Value::FloatValue(ctx.builder.build_float_neg(exp, "negtmp")),
            _ => panic!("not implemented"),
        };
    }
}

fn get_immut<'a, 'b: 'a>(
    mutctx: Option<&'b mut MutCtx<'b, 'b>>,
    immutctx: Option<MutCtx<'a, 'b>>,
) -> Option<MutCtx<'a, 'b>> {
    if let Some(x) = mutctx {
        return Some(x.clone());
    }
    immutctx
}
type MainFunc = unsafe extern "C" fn() -> i64;
#[test]
fn test_nom() {
    use crate::nomparser::PLParser;
    use inkwell::context::Context;
    use inkwell::values::AnyValue;
    let mut parser = PLParser::new("4+11*(8--2)");
    let (_, mut node) = parser.parse().unwrap();
    let tp = &Context::create();
    let context = ctx::Ctx::new(tp);
    let mut mc = ctx::MutCtx::new(context.context.clone(), context.module.clone());
    context.builder.position_at_end(mc.basic_block);
    let re = node.emit(&context, Some(&mut mc), None);
    if let Value::IntValue(re) = re {
        assert!(re.print_to_string().to_string() == "i64 114")
    } else {
        panic!("not implemented")
    }
    let execution_engine = context
        .module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .unwrap();
    unsafe {
        let f = execution_engine.get_function::<MainFunc>("main").unwrap();
        f.call();
    }
}
