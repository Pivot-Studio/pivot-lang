use super::node_result::TerminatorEnum;
use super::statement::StatementsNode;
use super::*;
use crate::ast::ctx::Ctx;
use crate::ast::diag::ErrorCode;
use crate::ast::pltype::PriType;
use internal_macro::node;

#[node(comment)]
pub struct IfNode {
    pub cond: Box<NodeEnum>,
    pub then: Box<StatementsNode>,
    pub els: Option<Box<NodeEnum>>,
}

impl PrintTrait for IfNode {
    // ANCHOR: print
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("IfNode");
        self.cond.print(tabs + 1, false, line.clone());
        if let Some(el) = &self.els {
            self.then.print(tabs + 1, false, line.clone());
            el.print(tabs + 1, true, line.clone());
        } else {
            self.then.print(tabs + 1, true, line.clone());
        }
    }
    // ANCHOR_END: print
}

impl Node for IfNode {
    // ANCHOR: emit
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let cond_block = builder.append_basic_block(ctx.function.unwrap(), "if.cond");
        let then_block = builder.append_basic_block(ctx.function.unwrap(), "if.then");
        let else_block = builder.append_basic_block(ctx.function.unwrap(), "if.else");
        let after_block = builder.append_basic_block(ctx.function.unwrap(), "if.after");
        builder.build_unconditional_branch(cond_block);
        ctx.position_at_end(cond_block, builder);
        let condrange = self.cond.range();
        let v = self.cond.emit(ctx, builder)?.get_value();
        let code = ErrorCode::IF_CONDITION_MUST_BE_BOOL;
        check_bool(&v, ctx, condrange, code)?;
        let v = v.unwrap();
        let cond = v.get_value();
        let cond = ctx.try_load2var(condrange, cond, builder)?;
        let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");
        builder.build_conditional_branch(cond, then_block, else_block);
        // then block
        ctx.position_at_end(then_block, builder);
        let then_terminator = self.then.emit_child(ctx, builder)?.get_term();
        if then_terminator.is_none() {
            builder.build_unconditional_branch(after_block);
        }
        ctx.position_at_end(else_block, builder);
        let terminator = if let Some(el) = &mut self.els {
            let mut child = ctx.new_child(el.range().start, builder);
            let else_terminator = el.emit(&mut child, builder)?.get_term();
            if else_terminator.is_none() {
                builder.build_unconditional_branch(after_block);
            }
            if then_terminator.is_return() && else_terminator.is_return() {
                TerminatorEnum::Return
            } else {
                TerminatorEnum::None
            }
        } else {
            builder.build_unconditional_branch(after_block);
            TerminatorEnum::None
        };
        ctx.position_at_end(after_block, builder);
        if terminator.is_return() {
            builder.build_unconditional_branch(after_block);
        }
        ctx.emit_comment_highlight(&self.comments[0]);
        NodeOutput::default().with_term(terminator).to_result()
    }
    // ANCHOR_END: emit
}

fn check_bool(
    v: &Option<NodeValue>,
    ctx: &mut Ctx,
    condrange: Range,
    code: ErrorCode,
) -> Result<(), PLDiag> {
    if v.is_none() || !v.as_ref().unwrap().get_ty().borrow().is(&PriType::BOOL) {
        return Err(ctx.add_diag(
            condrange
                .new_err(code)
                .add_help("use a bool variable instead")
                .clone(),
        ));
    }
    Ok(())
}

#[node(comment)]
pub struct WhileNode {
    pub cond: Box<NodeEnum>,
    pub body: Box<StatementsNode>,
}

impl PrintTrait for WhileNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("WhileNode");
        self.cond.print(tabs + 1, false, line.clone());
        self.body.print(tabs + 1, true, line.clone());
    }
}

impl Node for WhileNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let ctx = &mut ctx.new_child(self.range.start, builder);
        let cond_block = builder.append_basic_block(ctx.function.unwrap(), "while.cond");
        let body_block = builder.append_basic_block(ctx.function.unwrap(), "while.body");
        let after_block = builder.append_basic_block(ctx.function.unwrap(), "while.after");
        ctx.break_block = Some(after_block);
        ctx.continue_block = Some(cond_block);
        builder.build_unconditional_branch(cond_block);
        ctx.position_at_end(cond_block, builder);
        let condrange = self.cond.range();
        let start = self.cond.range().start;
        let v = self.cond.emit(ctx, builder)?.get_value();

        check_bool(&v, ctx, condrange, ErrorCode::WHILE_CONDITION_MUST_BE_BOOL)?;
        let cond = v.unwrap().get_value();
        let cond = ctx.try_load2var(condrange, cond, builder)?;
        let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");
        builder.build_conditional_branch(cond, body_block, after_block);
        ctx.position_at_end(body_block, builder);
        let terminator = self.body.emit_child(ctx, builder)?.get_term();
        builder.build_dbg_location(start);
        builder.build_unconditional_branch(cond_block);
        ctx.position_at_end(after_block, builder);
        ctx.emit_comment_highlight(&self.comments[0]);
        NodeOutput::default()
            .with_term(if terminator.is_return() {
                TerminatorEnum::Return
            } else {
                TerminatorEnum::None
            })
            .to_result()
    }
}

#[node(comment)]
pub struct ForNode {
    pub pre: Option<Box<NodeEnum>>,
    pub cond: Box<NodeEnum>,
    pub opt: Option<Box<NodeEnum>>,
    pub body: Box<StatementsNode>,
}

impl PrintTrait for ForNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ForNode");
        if let Some(pre) = &self.pre {
            pre.print(tabs + 1, false, line.clone());
        }
        self.cond.print(tabs + 1, false, line.clone());
        if let Some(opt) = &self.opt {
            opt.print(tabs + 1, false, line.clone());
        }
        self.body.print(tabs + 1, true, line.clone());
    }
}

impl Node for ForNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let ctx = &mut ctx.new_child(self.range.start, builder);
        let pre_block = builder.append_basic_block(ctx.function.unwrap(), "for.pre");
        let cond_block = builder.append_basic_block(ctx.function.unwrap(), "for.cond");
        let opt_block = builder.append_basic_block(ctx.function.unwrap(), "for.opt");
        let body_block = builder.append_basic_block(ctx.function.unwrap(), "for.body");
        let after_block = builder.append_basic_block(ctx.function.unwrap(), "for.after");
        ctx.break_block = Some(after_block);
        ctx.continue_block = Some(opt_block);
        builder.rm_curr_debug_location();
        builder.build_unconditional_branch(pre_block);
        ctx.position_at_end(pre_block, builder);
        if let Some(pr) = &mut self.pre {
            _ = pr.emit(ctx, builder);
        }
        builder.build_unconditional_branch(cond_block);
        ctx.position_at_end(cond_block, builder);
        builder.build_dbg_location(self.cond.range().start);
        let condrange = self.cond.range();
        let cond_start = self.cond.range().start;
        let v = self.cond.emit(ctx, builder)?.get_value();
        check_bool(&v, ctx, condrange, ErrorCode::FOR_CONDITION_MUST_BE_BOOL)?;
        let cond = v.unwrap().get_value();
        let cond = ctx.try_load2var(condrange, cond, builder)?;
        let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");
        builder.build_dbg_location(self.body.range().start);
        builder.build_conditional_branch(cond, body_block, after_block);
        ctx.position_at_end(opt_block, builder);
        if let Some(op) = &mut self.opt {
            builder.build_dbg_location(op.range().start);
            _ = op.emit(ctx, builder);
        }
        builder.build_dbg_location(cond_start);
        builder.build_unconditional_branch(cond_block);
        ctx.position_at_end(body_block, builder);
        let terminator = self.body.emit_child(ctx, builder)?.get_term();
        builder.build_unconditional_branch(opt_block);
        ctx.position_at_end(after_block, builder);
        ctx.emit_comment_highlight(&self.comments[0]);
        NodeOutput::default()
            .with_term(if terminator == TerminatorEnum::Return {
                terminator
            } else {
                TerminatorEnum::None
            })
            .to_result()
    }
}

#[node(comment)]
pub struct BreakNode {}

impl PrintTrait for BreakNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("BreakNode");
    }
}

impl Node for BreakNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        ctx.emit_comment_highlight(&self.comments[0]);
        if let Some(b) = ctx.break_block {
            builder.build_unconditional_branch(b);
            builder.clear_insertion_position();
        } else {
            let err = ctx.add_diag(self.range.new_err(ErrorCode::BREAK_MUST_BE_IN_LOOP));
            return Err(err);
        }
        NodeOutput::default()
            .with_term(TerminatorEnum::Break)
            .to_result()
    }
}

#[node(comment)]
pub struct ContinueNode {}

impl PrintTrait for ContinueNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("ContinueNode");
    }
}

impl Node for ContinueNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        if let Some(b) = ctx.continue_block {
            builder.build_unconditional_branch(b);
            builder.clear_insertion_position();
        } else {
            let err = ctx.add_diag(self.range.new_err(ErrorCode::CONTINUE_MUST_BE_IN_LOOP));
            return Err(err);
        }
        NodeOutput::default()
            .with_term(TerminatorEnum::Continue)
            .to_result()
    }
}
