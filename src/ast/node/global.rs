use super::*;

use crate::ast::builder::no_op_builder::NoOpBuilder;
use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::diag::ErrorCode;

use internal_macro::node;
use lsp_types::SemanticTokenType;

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
            self.constant.id.name.clone(),
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
        let noop = BuilderEnum::NoOp(NoOpBuilder::default());
        // get it's pointer
        let noop_ptr = &noop as *const BuilderEnum<'a, '_>;
        let noop = unsafe { noop_ptr.as_ref().unwrap() };
        if ctx.get_symbol(&self.var.name, builder).is_some() {
            return Err(ctx.add_diag(self.var.range.new_err(ErrorCode::REDEFINE_SYMBOL)));
        }
        let v = self.exp.emit(ctx, noop)?.get_value();
        if v.is_none() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::UNDEFINED_TYPE)));
        }
        let v = v.unwrap();
        let pltype = v.get_ty();
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
            pltype.clone(),
            self.var.range,
            true,
            false,
        )?;
        // for gc reason, globals must be pointer
        if !matches!(&*pltype.borrow(), PLType::Pointer(_)) {
            return Err(ctx.add_diag(self.var.range.new_err(ErrorCode::GLOBAL_MUST_BE_POINTER)));
        }
        Ok(())
    }
}
