mod ast;
mod nomparser;
mod utils;
use ast::{ctx::Ctx, Value};
use inkwell::context::Context;
use inkwell::values::AnyValue;
use nomparser::Parser;
fn main() {
    let mut parser = Parser::new("4+11*(8--2)");
    let (_, mut node) = parser.parse().unwrap();
    let tp = &Context::create();
    let context = Ctx::new(tp);
    let re = node.emit(&context);
    if let Value::IntValue(re) = re {
        assert!(re.print_to_string().to_string() == "i64 114")
    } else {
        panic!("not implemented")
    }
}
