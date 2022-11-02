use super::*;
use crate::ast::diag::ErrorCode;
use inkwell::debug_info::AsDIScope;
use internal_macro::range;
use lsp_types::SemanticTokenType;
#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct GlobalNode {
    pub var: VarNode,
    pub exp: Box<NodeEnum>,
}
impl Node for GlobalNode {
    fn format(&self,tabs:usize,prefix: &str) -> String {
        return "global hello".to_string();
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("GlobalNode");
        self.var.print(tabs + 1, false, line.clone());
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.var.range, SemanticTokenType::VARIABLE, 0);
        self.exp.emit(ctx)?;
        Ok((None, None, TerminatorEnum::NONE))
    }
}
impl GlobalNode {
    pub fn emit_global<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Result<(), PLDiag> {
        let exp_range = self.exp.range();
        if ctx.get_symbol(&self.var.name).is_some() {
            return Err(ctx.add_err(self.var.range, ErrorCode::REDEFINE_SYMBOL));
        }
        // use nodebug builder to emit
        let builder = ctx.builder;
        ctx.builder = ctx.nodebug_builder;
        let (value, pltype_opt, _) = self.exp.emit(ctx)?;
        ctx.builder = builder;
        if pltype_opt.is_none() {
            return Err(ctx.add_err(self.range, ErrorCode::UNDEFINED_TYPE));
        }
        let pltype = pltype_opt.unwrap();
        let ditype = pltype.get_ditype(ctx);
        let base_value = ctx.try_load2var(exp_range, value.unwrap())?;
        let base_type = base_value.get_type();
        let globalptr = ctx.module.add_global(base_type, None, &self.var.name);
        globalptr.set_initializer(&base_type.const_zero());
        globalptr.set_constant(false);
        ctx.nodebug_builder
            .build_store(globalptr.as_pointer_value(), base_value);
        let exp = ctx.dibuilder.create_global_variable_expression(
            ctx.diunit.as_debug_info_scope(),
            &self.var.name,
            "",
            ctx.diunit.get_file(),
            self.var.range.start.line as u32,
            ditype.unwrap(),
            false,
            None,
            None,
            ditype.unwrap().get_align_in_bits(),
        );
        globalptr.set_metadata(exp.as_metadata_value(ctx.context), 0);
        ctx.add_symbol(
            self.var.name.clone(),
            globalptr.as_pointer_value(),
            pltype,
            self.range,
            true,
        )?;
        Ok(())
    }
}
