use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::debug_info::*;
use inkwell::module::Module;
use inkwell::targets::TargetMachine;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;

pub struct PLLLVMBuilder<'a, 'ctx>  {
    pub context: &'ctx Context,            // llvm context
    pub builder: &'a Builder<'ctx>,        // llvm builder
    pub module: &'a Module<'ctx>,          // llvm module
    pub dibuilder: &'a DebugInfoBuilder<'ctx>, // debug info builder
    pub diunit: &'a DICompileUnit<'ctx>,   // debug info unit
    pub function: Option<FunctionValue<'ctx>>, // current function
    pub block: Option<BasicBlock<'ctx>>,   // current block
    pub continue_block: Option<BasicBlock<'ctx>>, // the block to jump when continue
    pub break_block: Option<BasicBlock<'ctx>>, // the block to jump to when break
    pub return_block: Option<(BasicBlock<'ctx>, Option<PointerValue<'ctx>>)>, // the block to jump to when return and value
    pub targetmachine: &'a TargetMachine, // might be used in debug info
    pub discope: DIScope<'ctx>,           // debug info scope
    pub nodebug_builder: &'a Builder<'ctx>, // builder without debug info
}
