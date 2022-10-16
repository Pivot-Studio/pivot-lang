pub mod accumulators;
pub mod compiler;
pub mod ctx;
pub mod diag;
pub mod node;
pub mod range;
pub mod tokens;

#[test]
#[cfg(feature = "jit")]
fn test_nom() {
    vm::reg();
    use std::cell::RefCell;

    use crate::{
        ast::{ctx::create_ctx_info, node::Node},
        db,
        nomparser::{parse, SourceProgram},
    };
    use inkwell::context::Context;
    let input = "struct test {
            a : i64;
            b : i64;
            c : name;
        }
        
        struct name {
            a : i64;
        }
        
        fn add(a: test) i64 {
            for let i = 0; i < 5; i = i + 1{
                a.a = a.a + 1;
            }
            return a.a+a.b;
        }
        
        fn main() i64 {
            printi64ln(0);
            let s = test{a:10,b:100,};
            printi64ln(add(s));
            let sb = s.c.a;
            sb = 0;
            let x = 1;
            let xx = (2+2)>0;
            if 2  > 0 {
                x = s.a;
            }
            while x<10 {
                x = x + 1;
                printi64ln(x);
            }
            for let i = 0; i < 5; i = i + 1{
                let x = 0;
                x = i;
                printi64ln(x);
            }
            printi64ln(x);
            printi64ln(call());
            return test_vm_link();
        }
        fn test_vm_link() i64;
        
        fn call() i64 {
            printi64ln(99);
            return 55;
        }
        
        
        
        fn printi64ln(i: i64) void;
        
    ";

    let db = db::Database::default();
    let parse_result = parse(&db, SourceProgram::new(&db, input.to_string()));
    let mut node = parse_result.unwrap();
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
