use super::statement::StatementsNode;
use super::*;
use crate::ast::ctx::Ctx;
use crate::ast::diag::ErrorCode;
use internal_macro::range;

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct IfNode {
    pub cond: Box<NodeEnum>,
    pub then: Box<NodeEnum>,
    pub els: Option<Box<NodeEnum>>,
}

impl Node for IfNode {
    fn format(&self, tabs: usize, prefix: &str) {
        println!("hello");
    }
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let cond_block = ctx
            .context
            .append_basic_block(ctx.function.unwrap(), "if.cond");
        let then_block = ctx
            .context
            .append_basic_block(ctx.function.unwrap(), "if.then");
        let else_block = ctx
            .context
            .append_basic_block(ctx.function.unwrap(), "if.else");
        let after_block = ctx
            .context
            .append_basic_block(ctx.function.unwrap(), "if.after");
        ctx.builder.build_unconditional_branch(cond_block);
        position_at_end(ctx, cond_block);
        let condrange = self.cond.range();
        let (cond, _, _) = self.cond.emit(ctx)?;
        let cond = ctx.try_load2var(cond);
        let con;
        if let Value::BoolValue(value) = cond {
            con = value;
        } else {
            let err = ctx.add_err(condrange, ErrorCode::IF_CONDITION_MUST_BE_BOOL);
            return Err(err);
        }
        let con = ctx
            .builder
            .build_int_truncate(con, ctx.context.bool_type(), "trunctemp");
        ctx.builder
            .build_conditional_branch(con, then_block, else_block);
        // then block
        position_at_end(ctx, then_block);
        let (_, _, then_terminator) = self.then.emit(ctx)?;
        if then_terminator.is_none() {
            ctx.builder.build_unconditional_branch(after_block);
        }
        position_at_end(ctx, else_block);
        let terminator = if let Some(el) = &mut self.els {
            let (_, _, else_terminator) = el.emit(ctx)?;
            if else_terminator.is_none() {
                ctx.builder.build_unconditional_branch(after_block);
            }
            if then_terminator.is_return() && else_terminator.is_return() {
                TerminatorEnum::RETURN
            } else {
                TerminatorEnum::NONE
            }
        } else {
            ctx.builder.build_unconditional_branch(after_block);
            TerminatorEnum::NONE
        };
        position_at_end(ctx, after_block);
        if terminator.is_return() {
            ctx.builder.build_unconditional_branch(after_block);
        }
        Ok((Value::None, None, terminator))
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct WhileNode {
    pub cond: Box<NodeEnum>,
    pub body: Box<StatementsNode>,
}

impl Node for WhileNode {
    fn format(&self, tabs: usize, prefix: &str) {
        println!("hello");
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("WhileNode");
        self.cond.print(tabs + 1, false, line.clone());
        self.body.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let ctx = &mut ctx.new_child(self.range.start);
        let cond_block = ctx
            .context
            .append_basic_block(ctx.function.unwrap(), "while.cond");
        let body_block = ctx
            .context
            .append_basic_block(ctx.function.unwrap(), "while.body");
        let after_block = ctx
            .context
            .append_basic_block(ctx.function.unwrap(), "while.after");
        ctx.break_block = Some(after_block);
        ctx.continue_block = Some(cond_block);
        ctx.builder.build_unconditional_branch(cond_block);
        position_at_end(ctx, cond_block);
        let condrange = self.cond.range();
        let start = self.cond.range().start;
        let cond = self.cond.emit(ctx)?;
        let con;
        if let Value::BoolValue(value) = cond.0 {
            con = value;
        } else {
            let err = ctx.add_err(condrange, ErrorCode::WHILE_CONDITION_MUST_BE_BOOL);
            return Err(err);
        }
        let con = ctx
            .builder
            .build_int_truncate(con, ctx.context.bool_type(), "trunctemp");
        ctx.builder
            .build_conditional_branch(con, body_block, after_block);
        position_at_end(ctx, body_block);
        let (_, _, terminator) = self.body.emit_child(ctx)?;
        ctx.build_dbg_location(start);
        ctx.builder.build_unconditional_branch(cond_block);
        position_at_end(ctx, after_block);
        Ok((
            Value::None,
            None,
            if terminator.is_return() {
                TerminatorEnum::RETURN
            } else {
                TerminatorEnum::NONE
            },
        ))
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ForNode {
    pub pre: Option<Box<NodeEnum>>,
    pub cond: Box<NodeEnum>,
    pub opt: Option<Box<NodeEnum>>,
    pub body: Box<StatementsNode>,
}

impl Node for ForNode {
    fn format(&self, tabs: usize, prefix: &str) {
        println!("hello");
    }
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let ctx = &mut ctx.new_child(self.range.start);
        let pre_block = ctx
            .context
            .append_basic_block(ctx.function.unwrap(), "for.pre");
        let cond_block = ctx
            .context
            .append_basic_block(ctx.function.unwrap(), "for.cond");
        let opt_block = ctx
            .context
            .append_basic_block(ctx.function.unwrap(), "for.opt");
        let body_block = ctx
            .context
            .append_basic_block(ctx.function.unwrap(), "for.body");
        let after_block = ctx
            .context
            .append_basic_block(ctx.function.unwrap(), "for.after");
        ctx.break_block = Some(after_block);
        ctx.continue_block = Some(cond_block);
        ctx.nodebug_builder.build_unconditional_branch(pre_block);
        position_at_end(ctx, pre_block);
        if let Some(pr) = &mut self.pre {
            _ = pr.emit(ctx);
        }
        ctx.builder.build_unconditional_branch(cond_block);
        position_at_end(ctx, cond_block);
        ctx.build_dbg_location(self.cond.range().start);
        let condrange = self.cond.range();
        let cond_start = self.cond.range().start;
        let cond = self.cond.emit(ctx)?;
        let con;
        if let Value::BoolValue(value) = cond.0 {
            con = value;
        } else {
            let err = ctx.add_err(condrange, ErrorCode::FOR_CONDITION_MUST_BE_BOOL);
            return Err(err);
        }
        let con = ctx
            .builder
            .build_int_truncate(con, ctx.context.bool_type(), "trunctemp");
        ctx.build_dbg_location(self.body.range().start);
        ctx.builder
            .build_conditional_branch(con, body_block, after_block);
        position_at_end(ctx, opt_block);
        if let Some(op) = &mut self.opt {
            ctx.build_dbg_location(op.range().start);
            _ = op.emit(ctx);
        }
        ctx.build_dbg_location(cond_start);
        ctx.builder.build_unconditional_branch(cond_block);
        position_at_end(ctx, body_block);
        let (_, _, terminator) = self.body.emit_child(ctx)?;
        ctx.builder.build_unconditional_branch(opt_block);
        position_at_end(ctx, after_block);
        Ok((
            Value::None,
            None,
            if terminator.is_return() {
                TerminatorEnum::RETURN
            } else {
                TerminatorEnum::NONE
            },
        ))
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BreakNode {}

impl Node for BreakNode {
    fn format(&self, tabs: usize, prefix: &str) {
        println!("hello");
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("BreakNode");
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        if let Some(b) = ctx.break_block {
            ctx.builder.build_unconditional_branch(b);
            // add dead block to avoid double br
            position_at_end(
                ctx,
                ctx.context
                    .append_basic_block(ctx.function.unwrap(), "dead"),
            );
        } else {
            let err = ctx.add_err(self.range, ErrorCode::BREAK_MUST_BE_IN_LOOP);
            return Err(err);
        }
        Ok((Value::None, None, TerminatorEnum::BREAK))
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ContinueNode {}

impl Node for ContinueNode {
    fn format(&self, tabs: usize, prefix: &str) {
        println!("hello");
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("ContinueNode");
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        if let Some(b) = ctx.continue_block {
            ctx.builder.build_unconditional_branch(b);
            position_at_end(
                ctx,
                // add dead block to avoid double br
                ctx.context
                    .append_basic_block(ctx.function.unwrap(), "dead"),
            );
        } else {
            let err = ctx.add_err(self.range, ErrorCode::CONTINUE_MUST_BE_IN_LOOP);
            return Err(err);
        }
        Ok((Value::None, None, TerminatorEnum::CONTINUE))
    }
}
