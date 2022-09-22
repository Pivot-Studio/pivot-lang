mod ast;
mod nomparser;
mod utils;
use ast::ctx::MutCtx;
use ast::{ctx::Ctx, Value};
use inkwell::context::Context;
use inkwell::values::AnyValue;
use nomparser::PLParser;
fn main() {
    let mut parser = PLParser::new("4+11*(8--2)");
    let (_, mut node) = parser.parse().unwrap();
    let tp = &Context::create();
    let context = Ctx::new(tp);
    let mut mc = MutCtx::new(context.context.clone(), context.module.clone());
    let re = node.emit(&context, &mut mc);
    if let Value::IntValue(re) = re {
        assert!(re.print_to_string().to_string() == "i64 114")
    } else {
        panic!("not implemented")
    }
}
