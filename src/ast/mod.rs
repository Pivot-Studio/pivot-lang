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
        "struct test {
            a : i64
            b : i64
        }let a = 0
        
        fn main() i64 {
            let x = 1
            let s = test{a:10,b:100,}
            if s.b>s.a {
                x = s.a
            }
            while x < 100 {
                if x == 50 {
                    x = x + 2
                }else if x == 99 {
                    break
                }else {
                    x = x + 1
                    continue
                }
                x = 101
            }
            for let i = 0; i < 5; i = i + 1{
                let b = 0
                b = i
                printi64ln(b)
            }
            printi64ln(x)
            printi64ln(call())
            return test_vm_link()
        }
        fn test_vm_link() i64
        
        fn call() i64 {
            return 55
        }
        
        fn printi64ln(i: i64) void
    ",
    );
    let mut node = parser.parse().unwrap();
    let context = &Context::create();
    let (a, b, c, d, e, f) = create_ctx_info(context, "", "");
    let v = RefCell::new(Vec::new());
    let mut ctx = ctx::Ctx::new(context, &a, &b, &c, &d, &e, &f, "test", &v);
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
