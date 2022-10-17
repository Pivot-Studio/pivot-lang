use super::statement::StatementsNode;
use super::*;
use crate::ast::ctx::Ctx;
use internal_macro::range;

#[range]
pub struct IfNode {
    pub cond: Box<dyn Node>,
    pub then: Box<dyn Node>,
    pub els: Option<Box<dyn Node>>,
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
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
        let (cond, _) = self.cond.emit(ctx);
        let cond = ctx.try_load(cond);
        let cond = match cond {
            Value::BoolValue(v) => v,
            _ => panic!("not implemented"),
        };
        ctx.builder
            .build_conditional_branch(cond, then_block, else_block);
        // then block
        position_at_end(ctx, then_block);
        self.then.emit(ctx);
        ctx.builder.build_unconditional_branch(after_block);
        position_at_end(ctx, else_block);
        if let Some(el) = &mut self.els {
            el.emit(ctx);
        }
        ctx.builder.build_unconditional_branch(after_block);
        position_at_end(ctx, after_block);
        (Value::None, None)
    }
}

#[range]
pub struct WhileNode {
    pub cond: Box<dyn Node>,
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
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
        let start = self.cond.range().start;
        let cond = self.cond.emit(ctx);
        let cond = match cond {
            (Value::BoolValue(v), _) => v,
            _ => panic!("not implemented"),
        };
        ctx.builder
            .build_conditional_branch(cond, body_block, after_block);
        position_at_end(ctx, body_block);
        self.body.emit_child(ctx);
        ctx.build_dbg_location(start);
        ctx.builder.build_unconditional_branch(cond_block);
        position_at_end(ctx, after_block);
        (Value::None, None)
    }
}

#[range]
pub struct ForNode {
    pub pre: Option<Box<dyn Node>>,
    pub cond: Box<dyn Node>,
    pub opt: Option<Box<dyn Node>>,
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
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
        // ctx.builder.build_unconditional_branch(pre_block);
        position_at_end(ctx, pre_block);
        if let Some(pr) = &mut self.pre {
            pr.emit(ctx);
        }
        ctx.builder.build_unconditional_branch(cond_block);
        position_at_end(ctx, cond_block);
        ctx.build_dbg_location(self.cond.range().start);
        let cond_start = self.cond.range().start;
        let cond = self.cond.emit(ctx);
        let cond = match cond {
            (Value::BoolValue(v), _) => v,
            _ => panic!("not implemented"),
        };
        ctx.build_dbg_location(self.body.range().start);
        ctx.builder
            .build_conditional_branch(cond, body_block, after_block);
        position_at_end(ctx, opt_block);
        if let Some(op) = &mut self.opt {
            ctx.build_dbg_location(op.range().start);
            op.emit(ctx);
        }
        ctx.build_dbg_location(cond_start);
        ctx.builder.build_unconditional_branch(cond_block);
        position_at_end(ctx, body_block);
        self.body.emit_child(ctx);
        ctx.builder.build_unconditional_branch(opt_block);
        position_at_end(ctx, after_block);
        (Value::None, None)
    }
}

#[range]
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

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
        if let Some(b) = ctx.break_block {
            ctx.builder.build_unconditional_branch(b);
            // add dead block to avoid double br
            position_at_end(
                ctx,
                ctx.context
                    .append_basic_block(ctx.function.unwrap(), "dead"),
            );
        } else {
            panic!("break not in loop");
        }
        (Value::None, None)
    }
}

#[range]
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

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> (Value<'ctx>, Option<String>) {
        if let Some(b) = ctx.continue_block {
            ctx.builder.build_unconditional_branch(b);
            position_at_end(
                ctx,
                // add dead block to avoid double br
                ctx.context
                    .append_basic_block(ctx.function.unwrap(), "dead"),
            );
        } else {
            panic!("continue not in loop");
        }
        (Value::None, None)
    }
}
