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
    fn print(&self) {
        println!("IfNode:");
        self.cond.print();
        self.then.print();
        if let Some(el) = &self.els {
            el.print();
        }
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let cond_block = ctx.context.append_basic_block(ctx.function, "cond");
        let then_block = ctx.context.append_basic_block(ctx.function, "then");
        let else_block = ctx.context.append_basic_block(ctx.function, "else");
        let after_block = ctx.context.append_basic_block(ctx.function, "after");
        ctx.builder.build_unconditional_branch(cond_block);
        position_at_end(ctx, cond_block);
        let cond = self.cond.emit(ctx);
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
        Value::None
    }
}

#[range]
pub struct WhileNode {
    pub cond: Box<dyn Node>,
    pub body: Box<dyn Node>,
}

impl Node for WhileNode {
    fn print(&self) {
        println!("WhileNode:");
        self.cond.print();
        self.body.print();
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        let cond_block = ctx.context.append_basic_block(ctx.function, "cond");
        let body_block = ctx.context.append_basic_block(ctx.function, "body");
        let after_block = ctx.context.append_basic_block(ctx.function, "after");
        ctx.break_block = Some(after_block);
        ctx.continue_block = Some(cond_block);
        ctx.builder.build_unconditional_branch(cond_block);
        position_at_end(ctx, cond_block);
        let cond = self.cond.emit(ctx);
        let cond = match cond {
            Value::BoolValue(v) => v,
            _ => panic!("not implemented"),
        };
        ctx.builder
            .build_conditional_branch(cond, body_block, after_block);
        position_at_end(ctx, body_block);
        self.body.emit(ctx);
        ctx.builder.build_unconditional_branch(cond_block);
        position_at_end(ctx, after_block);
        Value::None
    }
}

#[range]
pub struct BreakNode {}

impl Node for BreakNode {
    fn print(&self) {
        println!("BreakNode");
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        if let Some(b) = ctx.break_block {
            ctx.builder.build_unconditional_branch(b);
            // add dead block to avoid double br
            position_at_end(ctx, ctx.context.append_basic_block(ctx.function, "dead"));
        } else {
            panic!("break not in loop");
        }
        Value::None
    }
}

#[range]
pub struct ContinueNode {}

impl Node for ContinueNode {
    fn print(&self) {
        println!("ContinueNode");
    }

    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> Value<'ctx> {
        if let Some(b) = ctx.continue_block {
            ctx.builder.build_unconditional_branch(b);
            position_at_end(
                ctx,
                // add dead block to avoid double br
                ctx.context.append_basic_block(ctx.function, "dead"),
            );
        } else {
            panic!("continue not in loop");
        }
        Value::None
    }
}
