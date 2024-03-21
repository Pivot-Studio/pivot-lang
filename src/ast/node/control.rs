use std::path::Path;

use super::node_result::TerminatorEnum;
use super::statement::StatementsNode;
use super::*;
use crate::ast::builder::{BlockHandle, IntPredicate, ValueHandle};
use crate::ast::ctx::Ctx;
use crate::ast::diag::ErrorCode;
use crate::ast::pltype::{PriType, STType};
use crate::ast::traits::CustomType;
use crate::format_label;
use crate::inference::unknown_arc;
use internal_macro::node;

#[node(comment)]
/// IfNode is consisted by a 'if' clause and a 'else' clause.
/// the 'else' clause is allowed to embed another IfNode
pub struct IfNode {
    /// condition is the bool expression for the if keyword
    /// there is no type check in the AST stage, but we did check it when lowering ast
    pub cond: Box<NodeEnum>,
    /// then is the logic to be executed if the cond is true
    pub then: Box<StatementsNode>,
    /// els stands for the left part of the condition clause
    /// it might be another IfNode or a statement
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
        let merge_block = builder.append_basic_block(ctx.function.unwrap(), "if.after");
        builder.build_unconditional_branch(cond_block);
        ctx.position_at_end(cond_block, builder);

        let cond_range = self.cond.range();
        let cond_val = self.cond.emit(ctx, builder)?.get_value();
        check_bool(
            &cond_val,
            ctx,
            cond_range,
            ErrorCode::IF_CONDITION_MUST_BE_BOOL,
        )?;

        let v = cond_val.unwrap();
        let cond = v.get_value();
        let cond = ctx.try_load2var(cond_range, cond, builder, &v.get_ty().borrow())?;
        let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");

        //
        builder.build_conditional_branch(cond, then_block, else_block);

        // emit the else logic into the then block
        ctx.position_at_end(then_block, builder);
        // emit the code inside a child context because it belongs to a sub-block
        let then_terminator = self.then.emit_child(ctx, builder)?.get_term();
        if then_terminator.is_none() {
            // there is no terminator(like return, yield and so forth) in the statement
            // create an unconditional branch to merge block to finish off the "then" block
            builder.build_unconditional_branch(merge_block);
        }

        // emit the else logic into the else block
        ctx.position_at_end(else_block, builder);
        let terminator = if let Some(el) = &mut self.els {
            let mut child = ctx.new_child(el.range().start, builder);
            let else_terminator = el.emit(&mut child, builder)?.get_term();
            if else_terminator.is_none() {
                // create an unconditional branch only if no terminator is detected
                // otherwise, the code to be executed might be the others instead of merge block
                // for example, if there is a 'return' statement in the if-then-else clause,
                // it won't execute the merge block as it returns directly
                builder.build_unconditional_branch(merge_block);
            }

            if then_terminator.is_return() && else_terminator.is_return() {
                TerminatorEnum::Return
            } else {
                TerminatorEnum::None
            }
        } else {
            builder.build_unconditional_branch(merge_block);
            TerminatorEnum::None
        };

        ctx.position_at_end(merge_block, builder);
        if terminator.is_return() {
            builder.build_unconditional_branch(merge_block);
        }
        ctx.emit_comment_highlight(&self.comments[0]);

        NodeOutput::default().with_term(terminator).to_result()
    }
    // ANCHOR_END: emit
}

/// # check_bool
///
/// it ensures the input NodeValue represents a [PriType::BOOL],
/// otheriwse it returns an error with the range and error code.
fn check_bool(
    v: &Option<NodeValue>,
    ctx: &mut Ctx,
    range: Range,
    code: ErrorCode,
) -> Result<(), PLDiag> {
    if v.is_none() || !v.as_ref().unwrap().get_ty().borrow().is(&PriType::BOOL) {
        return Err(ctx.add_diag(
            range
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
        let v = v.unwrap();
        let cond = v.get_value();
        let cond = ctx.try_load2var(condrange, cond, builder, &v.get_ty().borrow())?;
        let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");
        builder.build_conditional_branch(cond, body_block, after_block);
        ctx.position_at_end(body_block, builder);
        builder.place_safepoint(ctx);
        let terminator = self.body.emit_child(ctx, builder)?.get_term();
        builder.build_dbg_location(start);
        builder.build_unconditional_branch(cond_block);
        ctx.position_at_end(after_block, builder);
        ctx.emit_comment_highlight(&self.comments[0]);
        NodeOutput::default()
            .with_term(if terminator.is_return() {
                terminator
            } else {
                TerminatorEnum::None
            })
            .to_result()
    }
}

#[node(comment)]
/// ForNode is consisted by four parts: pre,cond, opt and body in the format of `for pre;cond;opt body`.
///
/// The pre and opt are optional, but the semi-colons are compulsory.
///
/// For example:
/// ```pi
/// for let i = 0; i < 5; i = i + 1{
///  // ^pre       ^cond  ^opt        
///  
///  println!(i)
///  // ^body
/// }
///
/// ```
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
        let node_value = &v.unwrap();
        let cond = node_value.get_value();
        let cond = ctx.try_load2var(condrange, cond, builder, &node_value.get_ty().borrow())?;
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
        builder.place_safepoint(ctx);
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

#[node]
pub struct MatchNode {
    pub value: Box<NodeEnum>,
    pub arms: Vec<(MatchArmCondition, StatementsNode)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MatchArmCondition {
    /// match all other cases
    Discard(Range),
    /// match all other cases to a new variable
    Var(VarNode),
    Literal(Literal),
    /// A type with `(xxx)`
    TypedVar(TypeNodeEnum, Box<MatchArmCondition>),
    TypedDeconstruct(TypeNodeEnum, Vec<STMatchField>),
    /// when matching a struct, type can be omitted in match arms
    Deconstruct(Vec<STMatchField>),
    Tuple(Vec<MatchArmCondition>),
}

impl MatchArmCondition {

    fn range(&self) -> Range {
        match self {
            MatchArmCondition::Discard(r) => *r,
            MatchArmCondition::Var(v) => v.range,
            MatchArmCondition::Literal(l) => {
                match l {
                    Literal::Number(n) => n.range,
                    Literal::String(s) => s.range,
                    Literal::Bool(b) => b.range,
                }
            
            },
            MatchArmCondition::TypedVar(t, c) => t.range().start.to(c.range().end),
            MatchArmCondition::TypedDeconstruct(t, f) => {
                t.range().start.to(f.last().map(|(_,f)|f.range().end).unwrap_or(t.range().end))
            },
            MatchArmCondition::Deconstruct(fields) => {
                let start = fields.first().map(|(v, _)|v.range.start).unwrap_or_default();
                let end = fields.last().map(|(_, c)|c.range().end).unwrap_or_default();
                start.to(end)
            },
            MatchArmCondition::Tuple(fields) => {
                let start = fields.first().map(|c|c.range().start).unwrap_or_default();
                let end = fields.last().map(|c|c.range().end).unwrap_or_default();
                start.to(end)
            },
        }
    }
    fn add_matched_bb<'a, 'b>(
        cond: ValueHandle,
        not_matched: BlockHandle,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) {
        let matched_bb = builder.append_basic_block(ctx.function.unwrap(), "matched");
        builder.build_conditional_branch(cond, matched_bb, not_matched);
        builder.position_at_end_block(matched_bb);
    }
    fn is_matched<'a, 'b>(
        &self,
        v: ValueHandle,
        ty: Arc<RefCell<PLType>>,
        not_matched: BlockHandle,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) {
        match self {
            MatchArmCondition::Discard(_) => (),
            MatchArmCondition::Var(a) => {
                _ = ctx.add_symbol(a.name.clone(), v, ty, a.range, false, false);
                let i = builder.int_value(&PriType::BOOL, 1, false);
                Self::add_matched_bb(i, not_matched, ctx, builder);
            }
            MatchArmCondition::Literal(lit) => match lit {
                Literal::Number(n) => match n.value {
                    Num::Int(i) => match &*ty.borrow() {
                        PLType::Primitive(p) => {
                            if p.is_int() {
                                let i_v = builder.int_value(&p, i, false);
                                let v = ctx.try_load2var(Default::default(), v, builder, &ty.borrow()).unwrap();
                                let i = builder.build_int_compare(
                                    crate::ast::builder::IntPredicate::EQ,
                                    i_v,
                                    v,
                                    "eq",
                                );
                                Self::add_matched_bb(i, not_matched, ctx, builder);
                            }else {
                                ctx.position_at_end(not_matched, builder);
                            }
                        }
                        _ => {
                            ctx.position_at_end(not_matched, builder);
                        },
                    },
                    Num::Float(f) => match &*ty.borrow() {
                        PLType::Primitive(p) => {
                            if !p.is_int() {
                                let f_v = builder.float_value(&p, f);
                                let v = ctx.try_load2var(Default::default(), v, builder, &ty.borrow()).unwrap();
                                let i = builder.build_float_compare(
                                    crate::ast::builder::FloatPredicate::OEQ,
                                    f_v,
                                    v,
                                    "eq",
                                );
                                Self::add_matched_bb(i, not_matched, ctx, builder);
                            }else {
                                ctx.position_at_end(not_matched, builder);
                            }
                        }
                        _ => {
                            ctx.position_at_end(not_matched, builder);
                        },
                    },
                },
                Literal::String(_) => todo!(),
                Literal::Bool(b) => match &*ty.borrow() {
                    PLType::Primitive(PriType::BOOL) => {
                        let b_v =
                            builder.int_value(&PriType::BOOL, if b.value { 1 } else { 0 }, false);
                        let v = ctx.try_load2var(Default::default(), v, builder, &ty.borrow()).unwrap();
                        let i = builder.build_int_compare(
                            crate::ast::builder::IntPredicate::EQ,
                            b_v,
                            v,
                            "eq",
                        );
                        Self::add_matched_bb(i, not_matched, ctx, builder);
                    }
                    _ => {
                        ctx.position_at_end(not_matched, builder);
                    },
                },
            },
            MatchArmCondition::TypedVar(tp, c) => {
                tp.emit_highlight(ctx);
                let match_ty = tp.get_type(ctx, builder, false).unwrap_or(unknown_arc());
                match &*ty.borrow() {
                    PLType::Union(u) => {
                        if let Some(tag) = u.has_type(&match_ty.borrow(), ctx, builder) {
                            let tag_v = builder
                                .build_struct_gep(v, 0, "tag", &ty.borrow(), ctx)
                                .unwrap();
                            let tag_v = builder.build_load(tag_v, "tag", &PLType::new_i64(), ctx);
                            let cond = builder.build_int_compare(
                                IntPredicate::EQ,
                                tag_v,
                                builder.int_value(&PriType::U64, tag as u64, false),
                                "tag.eq",
                            );
                            let cond = builder
                                .try_load2var(
                                    Default::default(),
                                    cond,
                                    &PLType::Primitive(PriType::BOOL),
                                    ctx,
                                )
                                .unwrap();
                            let cond =
                                builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");

                            let matched_b =
                                builder.append_basic_block(ctx.function.unwrap(), "matched");

                            builder.build_conditional_branch(cond, matched_b, not_matched);
                            ctx.position_at_end(matched_b, builder);
                            let v_ptr = builder
                                .build_struct_gep(v, 1, "v", &ty.borrow(), ctx)
                                .unwrap();
                            let v_ptr = builder.build_load(v_ptr, "v", &PLType::new_i8_ptr(), ctx);
                            c.is_matched(v_ptr, match_ty.clone(), not_matched, ctx, builder);
                        }else {
                            ctx.position_at_end(not_matched, builder);
                        }
                    }
                    _ => {
                        ctx.position_at_end(not_matched, builder);
                    },
                }
            }
            MatchArmCondition::TypedDeconstruct(_, _) => todo!(),
            MatchArmCondition::Deconstruct(fields) => {
                match &*ty.borrow() {
                    PLType::Struct(s @ STType{is_tuple:false, ..}) => {
                        for (f, c) in fields.iter() {
                            if let Some(f) = s.fields.get(&f.name) {
                                let v_ptr = builder
                                    .build_struct_gep(v, f.index, "v", &ty.borrow(), ctx)
                                    .unwrap();
                                c.is_matched(v_ptr, f.typenode.get_type(ctx, builder, false).unwrap_or(unknown_arc()), not_matched, ctx, builder);
                            } else {
                                f.range
                                    .new_err(ErrorCode::STRUCT_FIELD_NOT_FOUND)
                                    .add_label(
                                        s.range,
                                        s.get_path(),
                                        format_label!("struct `{}` is defined here", &s.name),
                                    )
                                    .add_to_ctx(ctx);
                            }
                        }
                    }
                    _ => {
                        ctx.position_at_end(not_matched, builder);
                    },
                };
            }
            MatchArmCondition::Tuple(fields) => {
                match &*ty.borrow() {
                    PLType::Struct( tuple @ STType{is_tuple:true, ..}) => {
                        for (i, c) in fields.iter().enumerate() {
                            if i >= tuple.fields.len(){
                                c.range().new_err(ErrorCode::TUPLE_WRONG_DECONSTRUCT_PARAM_LEN).add_to_ctx(ctx);
                                continue;
                            }
                            let mut offset = 0;
                            if !tuple.is_atomic() {
                                offset = 1;
                            }
                            let v_ptr = builder
                                .build_struct_gep(v, (i+offset) as u32, "v", &ty.borrow(), ctx)
                                .unwrap();
                            c.is_matched(v_ptr, tuple.fields.get(&i.to_string()).unwrap().typenode.get_type(ctx, builder, false).unwrap_or(unknown_arc()), not_matched, ctx, builder);
                        }
                    }
                    _ => {
                        ctx.position_at_end(not_matched, builder);
                    },
                };
            },
        };
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Number(NumNode),
    String(StringNode),
    Bool(BoolConstNode),
}

pub type STMatchField = (VarNode, MatchArmCondition);


impl PrintTrait for MatchNode {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>) {
        todo!()
    }
}

impl Node for MatchNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let value = self.value.emit(ctx, builder)?.get_value();
        let value = value.unwrap();
        let ty = value.get_ty();
        let value = value.get_value();
        let matchend_b = builder.append_basic_block(ctx.function.unwrap(), "matchend");

        // let mut matched = builder.int_value(&PriType::BOOL, 0, false);
        for (cond, body) in self.arms.iter_mut() {
            let ctx = &mut ctx.new_child(self.range.start, builder);
            let not_matched_b = builder.append_basic_block(ctx.function.unwrap(), "not_matched");
            cond.is_matched(value, ty.clone(), not_matched_b, ctx, builder);
            let _ = body.emit_child(ctx, builder);
            builder.build_unconditional_branch(matchend_b);

            ctx.position_at_end(not_matched_b, builder);
        }
        builder.build_unconditional_branch(matchend_b);
        ctx.position_at_end(matchend_b, builder);
        // ctx.emit_comment_highlight(&self.comments[0]);
        // builder.print_to_file(&Path::new("match.ll"));
        NodeOutput::default()
            .with_term(TerminatorEnum::None)
            .to_result()
    }
}
