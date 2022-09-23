use add_symbol::is_runtime;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;

/// Convenience type alias for the `sum` function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type DemoFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn jit_compile_fn(&self, fnname: &str) -> Option<JitFunction<DemoFunc>> {
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
        let function = self.module.add_function("sum", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let x = function.get_nth_param(0)?.into_int_value();
        let y = function.get_nth_param(1)?.into_int_value();
        let z = function.get_nth_param(2)?.into_int_value();

        let demo = self.module.add_function(fnname, fn_type, None);
        let sum = self
            .builder
            .build_call(demo, &[x.into(), y.into(), z.into()], "xxx");

        self.builder
            .build_return(Some(&sum.try_as_basic_value().left().unwrap()));
        unsafe {
            let demo = self.execution_engine.get_function("sum");
            if let Ok(demo1) = demo {
                return Some(demo1);
            } else {
                println!("{}", demo.err().unwrap());
                return None;
            }
        }
    }
}

struct Demo {}

#[is_runtime("test")]
impl Demo {
    pub fn demo(a: u64, b: u64, c: u64) -> u64 {
        a + b + c
    }
    pub fn demo1(a: u64, b: u64, c: u64) -> u64 {
        a + b + c
    }
}

#[is_runtime("demo")]
pub fn demo(a: u64, b: u64, c: u64) -> u64 {
    a + b + c
}

#[test]
fn test_add_symbol() -> Result<(), Box<dyn std::error::Error>> {
    use inkwell::OptimizationLevel;
    use llvm_sys::target::LLVM_InitializeNativeTarget;
    let context = Context::create();
    let module = context.create_module("sum");

    unsafe {
        LLVM_InitializeNativeTarget();
    }
    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
    };
    let names = vec!["demo", "test_demo", "test_demo1"];

    for v in names {
        let demo = codegen
            .jit_compile_fn(v)
            .ok_or("Unable to JIT compile `sum`")?;

        let x = 1u64;
        let y = 2u64;
        let z = 3u64;

        unsafe {
            println!("{} + {} + {} = {}", x, y, z, demo.call(x, y, z));
            assert_eq!(demo.call(x, y, z), x + y + z);
        }
    }
    return Ok(());
}
