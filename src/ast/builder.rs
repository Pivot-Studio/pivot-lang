use self::llvmbuilder::PLLLVMBuilder;

use super::{node::{NodeResult, primary::{NumNode, VarNode}}, ctx::Ctx};

pub mod llvmbuilder;
pub trait IRBuilder<'a, 'ctx> : NumBuilder<'a, 'ctx> + VarBuilder<'a, 'ctx> {
}

pub trait NumBuilder<'a, 'ctx> {
    fn build_num(&mut self, ctx: &mut Ctx<'a, 'ctx>, node: & NumNode)  -> NodeResult<'ctx> ;
}

pub trait VarBuilder<'a, 'ctx> {
    fn build_var(&mut self, ctx: &mut Ctx<'a, 'ctx>, node: & VarNode)  -> NodeResult<'ctx> ;
}

impl <'a, 'ctx> IRBuilder<'a, 'ctx>  for PLLLVMBuilder<'a, 'ctx> {
}

