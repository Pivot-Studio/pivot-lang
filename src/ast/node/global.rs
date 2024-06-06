use super::*;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::diag::ErrorCode;
use crate::ast::plmod::G_COUNTER;
use crate::inference::InferenceCtx;
use crate::repl;

use internal_macro::node;
use lsp_types::SemanticTokenType;
use ustr::ustr;

#[node]
pub struct GlobalConstNode {
    pub constant: Box<TypedIdentifierNode>,
}

impl PrintTrait for GlobalConstNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("GlobalConstNode");
        self.constant.print(tabs + 1, true, line.clone());
    }
}

impl Node for GlobalConstNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.constant.id.range, SemanticTokenType::VARIABLE, 0);
        ctx.push_semantic_token(self.constant.typenode.range(), SemanticTokenType::TYPE, 0);
        let pltype = self.constant.typenode.get_type(ctx, builder, true)?;
        let globalptr = builder.global_const(&self.constant.id.name, &pltype.borrow(), ctx);
        ctx.add_symbol(
            self.constant.id.name,
            globalptr,
            pltype,
            self.constant.range,
            true,
            true,
        )?;
        Ok(Default::default())
    }
}

#[node]
/// GlobalNode stands for the code starts with `var` keyword
pub struct GlobalNode {
    /// var is the variable defined by `var` keyword
    pub var: VarNode,

    /// exp is the value of the variable
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        builder.rm_curr_debug_location();
        let entry = builder.get_last_basic_block(ctx.init_func.unwrap());
        let mut infer_ctx = InferenceCtx::new(ctx.unify_table.clone());
        infer_ctx.inference(&mut self.exp, ctx, builder);
        ctx.position_at_end(entry, builder);
        let exp_range = self.exp.range();
        ctx.push_semantic_token(self.var.range, SemanticTokenType::VARIABLE, 0);

        let v = self.exp.emit(ctx, builder)?.get_value();
        let v = v.unwrap();
        ctx.push_type_hints(self.var.range, v.get_ty());
        let base_value =
            ctx.try_load2var(exp_range, v.get_value(), builder, &v.get_ty().borrow())?;
        let res = ctx.get_symbol(&self.var.name, builder);
        if res.is_none() {
            return Ok(Default::default());
        }
        let global = res.unwrap().get_data();
        ctx.position_at_end(entry, builder);
        builder.build_store(global.value, base_value);
        Ok(Default::default())
    }
}
impl GlobalNode {
    pub fn emit_global<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<(), PLDiag> {
        let mut infer_ctx = InferenceCtx::new(ctx.unify_table.clone());
        infer_ctx.inference(&mut self.exp, ctx, builder);
        if {
            #[cfg(feature = "repl")]
            {
                ctx.get_file() != repl::REPL_VIRTUAL_ENTRY
            }
            #[cfg(not(feature = "repl"))]
            {
                true
            }
        } && ctx.get_symbol(&self.var.name, builder).is_some()
        {
            return Err(ctx.add_diag(self.var.range.new_err(ErrorCode::REDEFINE_SYMBOL)));
        }
        *ctx.need_highlight.borrow_mut() += 1;
        let v = self.exp.emit(ctx, builder)?.get_value();
        *ctx.need_highlight.borrow_mut() -= 1;
        if v.is_none() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::UNDEFINED_TYPE)));
        }
        let v = v.unwrap();
        let pltype = v.get_ty();
        let globalptr = builder.add_global(
            // &ctx.plmod.get_full_name(self.var.name),
            &ctx.plmod.get_full_name(ustr(&format!(
                "{}@{}",
                self.var.name,
                G_COUNTER.load(std::sync::atomic::Ordering::SeqCst)
            ))),
            pltype.clone(),
            ctx,
            self.var.range.start.line as u32,
            &pltype.borrow(),
        );
        ctx.add_symbol(
            self.var.name,
            globalptr,
            pltype.clone(),
            self.var.range,
            true,
            false,
        )?;
        Ok(())
    }
}
