use self::ctx::Ctx;
use inkwell::values::{FloatValue, IntValue};
use nom_locate::LocatedSpan;
use paste::item;
use range_marco::range;
type Span<'a> = LocatedSpan<&'a str>;
use lazy_static::lazy_static;
use std::collections::HashMap;
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
        Range {
            start: *self,
            end: end,
        }
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
    RPAREN : ")"// )
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
pub struct UnaryOpNode {
    pub op: TokenType,
    pub exp: Box<dyn Node>,
}
#[range]
pub struct BinOpNode {
    pub left: Box<dyn Node>,
    pub op: TokenType,
    pub right: Box<dyn Node>,
}

#[range]
pub struct VarNode {
    pub name: String,
}

impl Node for VarNode {
    fn print(&self) {
        println!("var: {}", self.name)
    }
    fn emit<'b>(&'b mut self, _: &'b Ctx) -> Value<'b> {
        Value::VarName(self.name.clone())
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
    VarName(String),
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
fn test_nom() {
    use crate::nomparser::PLParser;
    use inkwell::context::Context;
    use inkwell::values::AnyValue;
    let mut parser = PLParser::new("4+11*(8--2)");
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
