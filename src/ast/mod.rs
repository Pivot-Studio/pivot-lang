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
        "struct test {
            a : i64
            b : i64
        }
        
        fn call() i64 {
            return 55
        }
        
        
        fn main() i64 {
            let x = 1
            let s = test{a:10,b:100,}
            if s.b>s.a {
                x = s.a
            }
            return x
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
    println!("emit succ");
    println!("{}", ctx.module.to_string());

    let execution_engine = ctx
        .module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .unwrap();
    unsafe {
        let f = execution_engine.get_function::<MainFunc>("main").unwrap();
        let ret = f.call();
        println!("ret = {}", ret);
        assert_eq!(ret, 10)
    }
}
