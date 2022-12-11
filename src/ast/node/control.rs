use super::statement::StatementsNode;
use super::*;
use crate::ast::ctx::Ctx;
use crate::ast::diag::ErrorCode;
use crate::ast::pltype::PriType;
use internal_macro::{comments, fmt, range};

#[range]
#[fmt]
#[comments]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct IfNode {
    pub cond: Box<NodeEnum>,
    pub then: Box<StatementsNode>,
    pub els: Option<Box<NodeEnum>>,
}

impl Node for IfNode {
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
    // ANCHOR: emit
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult {
        let cond_block = ctx.llbuilder.borrow().append_basic_block("if.cond");
        let then_block = ctx.llbuilder.borrow().append_basic_block("if.then");
        let else_block = ctx.llbuilder.borrow().append_basic_block("if.else");
        let after_block = ctx.llbuilder.borrow().append_basic_block("if.after");
        ctx.llbuilder
            .borrow()
            .build_unconditional_branch(cond_block);
        ctx.position_at_end(cond_block);
        let condrange = self.cond.range();
        let (cond, pltype, _) = self.cond.emit(ctx)?;
        if pltype.is_none() || !pltype.unwrap().borrow().is(&PriType::BOOL) {
            return Err(ctx.add_err(condrange, ErrorCode::IF_CONDITION_MUST_BE_BOOL));
        }
        let (cond, _) = ctx.try_load2var(condrange, cond.unwrap(), pltype.unwrap())?;
        let cond = ctx
            .llbuilder
            .borrow()
            .build_int_truncate(cond, &PriType::BOOL, "trunctemp");
        ctx.llbuilder
            .borrow()
            .build_conditional_branch(cond, then_block, else_block);
        // then block
        ctx.position_at_end(then_block);
        let (_, _, then_terminator) = self.then.emit_child(ctx)?;
        if then_terminator.is_none() {
            ctx.llbuilder
                .borrow()
                .build_unconditional_branch(after_block);
        }
        ctx.position_at_end(else_block);
        let terminator = if let Some(el) = &mut self.els {
            let mut child = ctx.new_child(el.range().start);
            let (_, _, else_terminator) = el.emit(&mut child)?;
            if else_terminator.is_none() {
                ctx.llbuilder
                    .borrow()
                    .build_unconditional_branch(after_block);
            }
            if then_terminator.is_return() && else_terminator.is_return() {
                TerminatorEnum::RETURN
            } else {
                TerminatorEnum::NONE
            }
        } else {
            ctx.llbuilder
                .borrow()
                .build_unconditional_branch(after_block);
            TerminatorEnum::NONE
        };
        ctx.position_at_end(after_block);
        if terminator.is_return() {
            ctx.llbuilder
                .borrow()
                .build_unconditional_branch(after_block);
        }
        ctx.emit_comment_highlight(&self.comments[0]);
        Ok((None, None, terminator))
    }
    // ANCHOR_END: emit
}

#[range]
#[fmt]
#[comments]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct WhileNode {
    pub cond: Box<NodeEnum>,
    pub body: Box<StatementsNode>,
}

impl Node for WhileNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("WhileNode");
        self.cond.print(tabs + 1, false, line.clone());
        self.body.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult {
        let ctx = &mut ctx.new_child(self.range.start);
        let cond_block = ctx.llbuilder.borrow().append_basic_block("while.cond");
        let body_block = ctx.llbuilder.borrow().append_basic_block("while.body");
        let after_block = ctx.llbuilder.borrow().append_basic_block("while.after");
        ctx.break_block = Some(after_block);
        ctx.continue_block = Some(cond_block);
        ctx.llbuilder
            .borrow()
            .build_unconditional_branch(cond_block);
        ctx.position_at_end(cond_block);
        let condrange = self.cond.range();
        let start = self.cond.range().start;
        let (cond, pltype, _) = self.cond.emit(ctx)?;
        if pltype.is_none() || !pltype.unwrap().borrow().is(&PriType::BOOL) {
            return Err(ctx.add_err(condrange, ErrorCode::WHILE_CONDITION_MUST_BE_BOOL));
        }
        let (cond, _) = ctx.try_load2var(condrange, cond.unwrap(), pltype.unwrap())?;
        let cond = ctx
            .llbuilder
            .borrow()
            .build_int_truncate(cond, &PriType::BOOL, "trunctemp");
        ctx.llbuilder
            .borrow()
            .build_conditional_branch(cond, body_block, after_block);
        ctx.position_at_end(body_block);
        let (_, _, terminator) = self.body.emit_child(ctx)?;
        ctx.llbuilder.borrow().build_dbg_location(start);
        ctx.llbuilder
            .borrow()
            .build_unconditional_branch(cond_block);
        ctx.position_at_end(after_block);
        ctx.emit_comment_highlight(&self.comments[0]);
        Ok((
            None,
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
#[fmt]
#[comments]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ForNode {
    pub pre: Option<Box<NodeEnum>>,
    pub cond: Box<NodeEnum>,
    pub opt: Option<Box<NodeEnum>>,
    pub body: Box<StatementsNode>,
}

impl Node for ForNode {
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
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult {
        let ctx = &mut ctx.new_child(self.range.start);
        let pre_block = ctx.llbuilder.borrow().append_basic_block("for.pre");
        let cond_block = ctx.llbuilder.borrow().append_basic_block("for.cond");
        let opt_block = ctx.llbuilder.borrow().append_basic_block("for.opt");
        let body_block = ctx.llbuilder.borrow().append_basic_block("for.body");
        let after_block = ctx.llbuilder.borrow().append_basic_block("for.after");
        ctx.break_block = Some(after_block);
        ctx.continue_block = Some(cond_block);
        ctx.llbuilder.borrow().rm_curr_debug_location();
        ctx.llbuilder.borrow().build_unconditional_branch(pre_block);
        ctx.position_at_end(pre_block);
        if let Some(pr) = &mut self.pre {
            _ = pr.emit(ctx);
        }
        ctx.llbuilder
            .borrow()
            .build_unconditional_branch(cond_block);
        ctx.position_at_end(cond_block);
        ctx.llbuilder
            .borrow()
            .build_dbg_location(self.cond.range().start);
        let condrange = self.cond.range();
        let cond_start = self.cond.range().start;
        let (cond, pltype, _) = self.cond.emit(ctx)?;
        if pltype.is_none() || !pltype.unwrap().borrow().is(&PriType::BOOL) {
            return Err(ctx.add_err(condrange, ErrorCode::FOR_CONDITION_MUST_BE_BOOL));
        }
        let (cond, _) = ctx.try_load2var(condrange, cond.unwrap(), pltype.unwrap())?;
        let cond = ctx
            .llbuilder
            .borrow()
            .build_int_truncate(cond, &PriType::BOOL, "trunctemp");
        ctx.llbuilder
            .borrow()
            .build_dbg_location(self.body.range().start);
        ctx.llbuilder
            .borrow()
            .build_conditional_branch(cond, body_block, after_block);
        ctx.position_at_end(opt_block);
        if let Some(op) = &mut self.opt {
            ctx.llbuilder.borrow().build_dbg_location(op.range().start);
            _ = op.emit(ctx);
        }
        ctx.llbuilder.borrow().build_dbg_location(cond_start);
        ctx.llbuilder
            .borrow()
            .build_unconditional_branch(cond_block);
        ctx.position_at_end(body_block);
        let (_, _, terminator) = self.body.emit_child(ctx)?;
        ctx.llbuilder.borrow().build_unconditional_branch(opt_block);
        ctx.position_at_end(after_block);
        ctx.emit_comment_highlight(&self.comments[0]);
        Ok((
            None,
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
#[comments]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BreakNode {}

impl Node for BreakNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("BreakNode");
    }

    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult {
        ctx.emit_comment_highlight(&self.comments[0]);
        if let Some(b) = ctx.break_block {
            ctx.llbuilder.borrow().build_unconditional_branch(b);
            ctx.llbuilder.borrow().clear_insertion_position();
        } else {
            let err = ctx.add_err(self.range, ErrorCode::BREAK_MUST_BE_IN_LOOP);
            return Err(err);
        }
        Ok((None, None, TerminatorEnum::BREAK))
    }
}

#[range]
#[fmt]
#[comments]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ContinueNode {}

impl Node for ContinueNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("ContinueNode");
    }

    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult {
        if let Some(b) = ctx.continue_block {
            ctx.llbuilder.borrow().build_unconditional_branch(b);
            ctx.llbuilder.borrow().clear_insertion_position();
        } else {
            let err = ctx.add_err(self.range, ErrorCode::CONTINUE_MUST_BE_IN_LOOP);
            return Err(err);
        }
        Ok((None, None, TerminatorEnum::CONTINUE))
    }
}
