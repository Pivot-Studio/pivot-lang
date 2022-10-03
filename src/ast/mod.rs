pub mod compiler;
pub mod ctx;
pub mod node;
pub mod range;
pub mod tokens;

#[test]
fn test_nom() {
    use crate::nomparser::PLParser;
    use inkwell::context::Context;
    type MainFunc = unsafe extern "C" fn() -> i64;
    let mut parser = PLParser::new(
        "fn test_vm_link() i64

    fn main() i64 {
        return 0
    }
    
    ",
    );
    let (_, mut node) = parser.parse().unwrap();
    let context = &Context::create();
    let builder = &context.create_builder();
    let module = &context.create_module("test");
    let mut ctx = ctx::Ctx::new(context, module, builder);
    let m = &mut ctx;
    println!("{}", node.string(0));
    let _re = node.emit(m);
    // if let Value::IntValue(re) = re {
    //     assert!(re.print_to_string().to_string() == "i64 114")
    // } else {
    //     panic!("not implemented")
    // }
    println!("emit succ");
    println!("{}", ctx.module.to_string());
    let v = ctx.get_symbol("a");
    let v = v.unwrap();
    let load = ctx.builder.build_load(*v, "load");
    ctx.builder.build_return(Some(&load));

    let execution_engine = ctx
        .module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .unwrap();
    unsafe {
        let f = execution_engine.get_function::<MainFunc>("main").unwrap();
        let ret = f.call();
        println!("a = {}", ret);
        assert_eq!(ret, 21000)
    }
}
