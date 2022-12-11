use super::*;
use crate::ast::diag::ErrorCode;
use inkwell::debug_info::AsDIScope;
use internal_macro::{fmt, range};
use lsp_types::SemanticTokenType;
#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct GlobalNode {
    pub var: VarNode,
    pub exp: Box<NodeEnum>,
}
impl Node for GlobalNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("GlobalNode");
        self.var.print(tabs + 1, false, line.clone());
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult {
        let entry = ctx
            .llbuilder
            .borrow()
            .get_last_basic_block(ctx.llbuilder.borrow().get_init_fn());

        ctx.position_at_end(entry);
        let exp_range = self.exp.range();
        ctx.push_semantic_token(self.var.range, SemanticTokenType::VARIABLE, 0);

        let (value, pltype, _) = self.exp.emit(ctx)?;
        ctx.push_type_hints(self.var.range, pltype.unwrap());
        let (base_value, tp) = ctx.try_load2var(exp_range, value.unwrap(), pltype.unwrap())?;
        let res = ctx.get_symbol(&self.var.name);
        if res.is_none() {
            return Ok((None, None, TerminatorEnum::NONE));
        }
        let (globalptr, _, _, _, _) = res.unwrap();
        ctx.position_at_end(entry);
        ctx.llbuilder.borrow().build_store(globalptr, base_value);
        Ok((None, None, TerminatorEnum::NONE))
    }
}
impl GlobalNode {
    pub fn emit_global<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> Result<(), PLDiag> {
        let exp_range = self.exp.range();
        if ctx.get_symbol(&self.var.name).is_some() {
            return Err(ctx.add_err(self.var.range, ErrorCode::REDEFINE_SYMBOL));
        }
        // use nodebug builder to emit
        let (value, pltype_opt, _) = self.exp.emit(ctx)?;
        if pltype_opt.is_none() {
            return Err(ctx.add_err(self.range, ErrorCode::UNDEFINED_TYPE));
        }
        let pltype = pltype_opt.unwrap();
        let (_, tp) = ctx.try_load2var(exp_range, value.unwrap(), pltype)?;
        let base_type = tp;
        let globalptr = ctx.llbuilder.borrow().add_global(
            &ctx.plmod.get_full_name(&self.var.name),
            base_type,
            ctx,
            self.var.range.start.line as u32,
            &pltype.borrow(),
        );
        ctx.add_symbol(
            self.var.name.clone(),
            globalptr,
            pltype,
            self.var.range,
            true,
        )?;
        Ok(())
    }
}
