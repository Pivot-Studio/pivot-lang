pub mod compiler;
pub mod ctx;
pub mod error;
pub mod node;
pub mod range;
pub mod tokens;

#[test]
#[cfg(feature = "jit")]
fn test_nom() {
    vm::reg();
    use std::cell::RefCell;

    use crate::{ast::ctx::create_ctx_info, nomparser::PLParser};
    use inkwell::context::Context;
    let mut parser = PLParser::new(
        "struct test1{
            a:a
    ",
    );
    let mut node = parser.parse().unwrap();
    let context = &Context::create();
    let (a, b, c, d, e, f) = create_ctx_info(context, "", "");
    let v = RefCell::new(Vec::new());
    let mut ctx = ctx::Ctx::new(context, &a, &b, &c, &d, &e, &f, "test", &v, None, None);
    let m = &mut ctx;
    node.print(0, false, vec![]);
    let _re = node.emit(m);
    println!("emit succ");
    println!("{}", ctx.module.to_string());

    let execution_engine = ctx
        .module
        .create_jit_execution_engine(inkwell::OptimizationLevel::Default)
        .unwrap();
    unsafe {
        let f = execution_engine.get_function_value("main").unwrap();
        let ret = execution_engine.run_function_as_main(f, &[]);
        println!("ret = {}", ret);
        assert_eq!(ret, 66)
    }
}
