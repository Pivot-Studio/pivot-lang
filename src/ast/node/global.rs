use super::*;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::diag::ErrorCode;

use internal_macro::node;
use lsp_types::SemanticTokenType;
#[node]
pub struct GlobalNode {
    pub var: VarNode,
    pub exp: Box<NodeEnum>,
}

impl PrintTrait for GlobalNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("GlobalNode");
        self.var.print(tabs + 1, false, line.clone());
        self.exp.print(tabs + 1, true, line.clone());
    }
}

impl Node for GlobalNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let entry = builder.get_last_basic_block(ctx.init_func.unwrap());

        ctx.position_at_end(entry, builder);
        let exp_range = self.exp.range();
        ctx.push_semantic_token(self.var.range, SemanticTokenType::VARIABLE, 0);

        let (value, pltype, _) = self.exp.emit(ctx, builder)?;
        ctx.push_type_hints(self.var.range, pltype.clone().unwrap());
        let base_value = ctx.try_load2var(exp_range, value.unwrap(), builder)?;
        let res = ctx.get_symbol(&self.var.name, builder);
        if res.is_none() {
            return Ok((None, None, TerminatorEnum::NONE));
        }
        let (globalptr, _, _, _, _) = res.unwrap();
        ctx.position_at_end(entry, builder);
        builder.build_store(globalptr, base_value);
        Ok((None, None, TerminatorEnum::NONE))
    }
}
impl GlobalNode {
    pub fn emit_global<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<(), PLDiag> {
        let exp_range = self.exp.range();
        if ctx.get_symbol(&self.var.name, builder).is_some() {
            return Err(ctx.add_diag(self.var.range.new_err(ErrorCode::REDEFINE_SYMBOL)));
        }
        // use nodebug builder to emit
        let (value, pltype_opt, _) = self.exp.emit(ctx, builder)?;
        if pltype_opt.is_none() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::UNDEFINED_TYPE)));
        }
        let pltype = pltype_opt.unwrap();
        ctx.try_load2var(exp_range, value.unwrap(), builder)?;
        let globalptr = builder.add_global(
            &ctx.plmod.get_full_name(&self.var.name),
            pltype.clone(),
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
