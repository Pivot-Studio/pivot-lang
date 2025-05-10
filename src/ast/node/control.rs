use super::node_result::TerminatorEnum;
use super::statement::StatementsNode;
use super::*;
use crate::ast::builder::{BlockHandle, IntPredicate, ValueHandle};
use crate::ast::ctx::Ctx;
use crate::ast::diag::ErrorCode;
use crate::ast::pltype::{PriType, STType};
use crate::ast::tokens::TokenType;
use crate::ast::traits::CustomType;
use crate::format_label;
use crate::inference::unknown_arc;
use internal_macro::node;
use node_result::CompileTimeResult;
use rustc_hash::FxHashMap;
use ustr::ustr;


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

        let (gen_def, skip_body) = cond_pre_process(
            &mut self.cond,
            ctx,
            builder,
            then_block,
            else_block,
            cond_range,
        );

        // emit the else logic into the then block
        ctx.position_at_end(then_block, builder);
        let mut then_terminator = TerminatorEnum::None;
        // emit the code inside a child context because it belongs to a sub-block
        let mut child = ctx.new_child(self.then.range().start, builder);
        if !skip_body || matches!(builder, BuilderEnum::NoOp(_)) {
            if let Some(mut def) = gen_def {
                def.emit(&mut child, builder)?;
            }
            then_terminator = self.then.emit(&mut child, builder)?.get_term();
        }

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

fn cond_pre_process<'a>(
    cond: &mut NodeEnum,
    ctx: &mut Ctx<'a>,
    builder: &BuilderEnum<'a, '_>,
    then_block: usize,
    else_block: usize,
    cond_range: Range,
) -> (Option<DefNode>, bool) {
    let (gen_def, skip_body) = if let NodeEnum::Def(def) = &*cond {
        // check if it is a `let ... = ... as ...`
        let re = if let Some(e) = &def.value_expression {
            if let NodeEnum::AsNode(a) = &**e {
                if let Some((_, r)) = &a.tail {
                    // tail not allowed in `if let .. as ..`
                    ctx.add_diag(
                        r.new_err(ErrorCode::IF_LET_DOES_NOT_EXPECT_TAIL)
                            .add_help("remove the tailling symbol")
                            .clone(),
                    );
                }

                // we need to evaluate the expr first, to avoid it run twice
                let inter = IntermediateNode::new(a.expr.clone().emit(ctx, builder));
                let mut transformed_is = NodeEnum::IsNode(IsNode {
                    expr: Box::new(NodeEnum::InterNode(inter.clone())),
                    target_type: a.target_type.clone(),
                    range: a.range(),
                });
                _ = build_cond(
                    &mut transformed_is,
                    ctx,
                    builder,
                    cond_range,
                    then_block,
                    else_block,
                    ErrorCode::IF_CONDITION_MUST_BE_BOOL,
                );
                ctx.position_at_end(then_block, builder);
                let transformed_as = NodeEnum::AsNode(AsNode {
                    expr: Box::new(NodeEnum::InterNode(inter)),
                    target_type: a.target_type.clone(),
                    range: a.range(),
                    tail: Some((TokenType::NOT, Default::default())),
                });
                let mut def = def.clone();
                def.value_expression = Some(Box::new(transformed_as));
                (Some(def), false)
            } else if let NodeEnum::ImplCastNode(a) = &**e {
                if let Some((_, r)) = &a.tail {
                    // tail not allowed in `if let .. impl ..`
                    ctx.add_diag(
                        r.new_err(ErrorCode::IF_LET_DOES_NOT_EXPECT_TAIL)
                            .add_help("remove the tailling symbol")
                            .clone(),
                    );
                }
                // we need to evaluate the expr first, to avoid it run twice
                let inter = IntermediateNode::new(a.expr.clone().emit(ctx, builder));
                let mut transformed_is = NodeEnum::ImplCastNode(ImplCastNode {
                    expr: Box::new(NodeEnum::InterNode(inter.clone())),
                    target_type: a.target_type.clone(),
                    range: a.range(),
                    tail: Some((TokenType::QUESTION, Default::default())),
                });
                let re = build_cond(
                    &mut transformed_is,
                    ctx,
                    builder,
                    cond_range,
                    then_block,
                    else_block,
                    ErrorCode::IF_CONDITION_MUST_BE_BOOL,
                );
                let skip_body = matches!(
                    re,
                    Ok(NodeOutput {
                        compile_time_result: CompileTimeResult::ConstBool(false),
                        ..
                    })
                );
                ctx.position_at_end(then_block, builder);
                let transformed_as = NodeEnum::ImplCastNode(ImplCastNode {
                    expr: Box::new(NodeEnum::InterNode(inter)),
                    target_type: a.target_type.clone(),
                    range: a.range(),
                    tail: Some((TokenType::NOT, Default::default())),
                });
                let mut def = def.clone();
                def.value_expression = Some(Box::new(transformed_as));
                (Some(def), skip_body)
            } else {
                (None, false)
            }
        } else {
            (None, false)
        };
        if re.0.is_none() {
            def.range()
                .new_err(ErrorCode::EXPECT_IF_LET_AS)
                .add_help("adding as expression might be a solution")
                .add_to_ctx(ctx);
        }
        re
    } else {
        _ = build_cond(
            cond,
            ctx,
            builder,
            cond_range,
            then_block,
            else_block,
            ErrorCode::IF_CONDITION_MUST_BE_BOOL,
        );
        (None, false)
    };
    (gen_def, skip_body)
}
fn build_cond<'a>(
    cond: &mut NodeEnum,
    ctx: &mut Ctx<'a>,
    builder: &BuilderEnum<'a, '_>,
    cond_range: Range,
    then_block: usize,
    else_block: usize,
    err_code: ErrorCode,
) -> Result<NodeOutput, PLDiag> {
    cond.emit(ctx, builder).and_then(|o| {
        let cond_val = o.get_value();
        check_bool(&cond_val, ctx, cond_range, err_code)?;

        let v = cond_val.unwrap();
        let cond = v.get_value();
        let cond = ctx.try_load2var(cond_range, cond, builder, &v.get_ty().borrow())?;
        let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");

        builder.build_conditional_branch(cond, then_block, else_block);
        Ok(o)
    })
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

        let (gen_def, skip_body) = cond_pre_process(
            &mut self.cond,
            ctx,
            builder,
            body_block,
            after_block,
            condrange,
        );

        ctx.position_at_end(body_block, builder);
        // builder.place_safepoint(ctx);
        // let terminator = self.body.emit_child(ctx, builder)?.get_term();

        // emit the code inside a child context because it belongs to a sub-block
        let mut child = ctx.new_child(self.body.range().start, builder);
        if !skip_body || matches!(builder, BuilderEnum::NoOp(_)) {
            if let Some(mut def) = gen_def {
                def.emit(&mut child, builder)?;
            }
            let terminator = self.body.emit(&mut child, builder)?.get_term();
            if !terminator.is_return() {
                builder.build_unconditional_branch(cond_block);
            }
        }
        builder.build_dbg_location(start);
        ctx.position_at_end(after_block, builder);
        ctx.emit_comment_highlight(&self.comments[0]);
        NodeOutput::default()
            .with_term(TerminatorEnum::None)
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
        _ = self.cond.emit(ctx, builder).and_then(|o| {
            let v = o.get_value();
            check_bool(&v, ctx, condrange, ErrorCode::FOR_CONDITION_MUST_BE_BOOL)?;
            let node_value = &v.unwrap();
            let cond = node_value.get_value();
            let cond = ctx.try_load2var(condrange, cond, builder, &node_value.get_ty().borrow())?;
            let cond = builder.build_int_truncate(cond, &PriType::BOOL, "trunctemp");
            builder.build_dbg_location(self.body.range().start);
            builder.build_conditional_branch(cond, body_block, after_block);
            Ok(())
        });

        ctx.position_at_end(opt_block, builder);
        if let Some(op) = &mut self.opt {
            builder.build_dbg_location(op.range().start);
            _ = op.emit(ctx, builder);
        }
        builder.build_dbg_location(cond_start);
        builder.build_unconditional_branch(cond_block);
        ctx.position_at_end(body_block, builder);
        builder.place_safepoint(ctx);
        _ = self.body.emit_child(ctx, builder);
        builder.build_unconditional_branch(opt_block);
        ctx.position_at_end(after_block, builder);
        ctx.emit_comment_highlight(&self.comments[0]);
        NodeOutput::default()
            .with_term(TerminatorEnum::None)
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
    Tuple(Vec<MatchArmCondition>, Range),
}

impl MatchArmCondition {
    fn range(&self) -> Range {
        match self {
            MatchArmCondition::Discard(r) => *r,
            MatchArmCondition::Var(v) => v.range,
            MatchArmCondition::Literal(l) => match l {
                Literal::Number(n) => n.range,
                Literal::String(s) => s.range,
                Literal::Bool(b) => b.range,
            },
            MatchArmCondition::TypedVar(t, c) => t.range().start.to(c.range().end),
            MatchArmCondition::TypedDeconstruct(t, f) => t.range().start.to(f
                .last()
                .map(|(_, f)| f.range().end)
                .unwrap_or(t.range().end)),
            MatchArmCondition::Deconstruct(fields) => {
                let start = fields
                    .first()
                    .map(|(v, _)| v.range.start)
                    .unwrap_or_default();
                let end = fields
                    .last()
                    .map(|(_, c)| c.range().end)
                    .unwrap_or_default();
                start.to(end)
            }
            MatchArmCondition::Tuple(_, r) => *r,
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
        &mut self,
        v: ValueHandle,
        ty: Arc<RefCell<PLType>>,
        not_matched: BlockHandle,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) {
        let range = self.range();
        match self {
            MatchArmCondition::Discard(range) => {
                ctx.push_semantic_token(*range, SemanticTokenType::VARIABLE, 0);
            }
            MatchArmCondition::Var(a) => {
                ctx.push_semantic_token(a.range, SemanticTokenType::VARIABLE, 0);
                let v = if ctx.generator_data.is_some() {
                    // special case: in generator, every variable needs to be stored in the context
                    let alloca = builder.alloc(
                        a.name.as_str(),
                        &PLType::new_i8_ptr(),
                        ctx,
                        Some(range.start),
                    );
                    builder.build_store(alloca, v);
                    alloca
                } else {
                    v
                };
                _ = ctx.add_symbol(a.name, v, ty.clone(), a.range, false, false);
                ctx.push_type_hints(a.range, ty);
                let i = builder.int_value(&PriType::BOOL, 1, false);
                Self::add_matched_bb(i, not_matched, ctx, builder);
            }
            MatchArmCondition::Literal(lit) => match lit {
                Literal::Number(n) => {
                    ctx.push_semantic_token(n.range, SemanticTokenType::NUMBER, 0);
                    match n.value {
                        Num::Int(i) => match &*ty.borrow() {
                            PLType::Primitive(p) => {
                                if p.is_int() {
                                    let i_v = builder.int_value(p, i, false);
                                    let v = ctx
                                        .try_load2var(Default::default(), v, builder, &ty.borrow())
                                        .unwrap();
                                    let i = builder.build_int_compare(
                                        crate::ast::builder::IntPredicate::EQ,
                                        i_v,
                                        v,
                                        "eq",
                                    );
                                    Self::add_matched_bb(i, not_matched, ctx, builder);
                                } else {
                                    n.range().new_err(ErrorCode::ILLEGAL_MATCH_ARM_CONDITION)
                                .add_label(n.range, ctx.get_file(), format_label!("match condition is of type `{}`, imcompatible with type `{}`", "int", "float"))
                                .add_to_ctx(ctx);
                                }
                            }
                            _ => {
                                n.range().new_err(ErrorCode::ILLEGAL_MATCH_ARM_CONDITION)
                            .add_label(n.range, ctx.get_file(), format_label!("match condition is of type `{}`, imcompatible with type `{}`", "int", ty.borrow().get_name()))
                            .add_to_ctx(ctx);
                            }
                        },
                        Num::Float(f) => match &*ty.borrow() {
                            PLType::Primitive(p) => {
                                if !p.is_int() {
                                    let f_v = builder.float_value(p, f);
                                    let v = ctx
                                        .try_load2var(Default::default(), v, builder, &ty.borrow())
                                        .unwrap();
                                    let i = builder.build_float_compare(
                                        crate::ast::builder::FloatPredicate::OEQ,
                                        f_v,
                                        v,
                                        "eq",
                                    );
                                    Self::add_matched_bb(i, not_matched, ctx, builder);
                                } else {
                                    n.range().new_err(ErrorCode::ILLEGAL_MATCH_ARM_CONDITION)
                                .add_label(n.range, ctx.get_file(), format_label!("match condition is of type `{}`, imcompatible with type `{}`", "float", "int"))
                                .add_to_ctx(ctx);
                                }
                            }
                            _ => {
                                n.range().new_err(ErrorCode::ILLEGAL_MATCH_ARM_CONDITION)
                            .add_label(n.range, ctx.get_file(), format_label!("match condition is of type `{}`, imcompatible with type `{}`", "float", ty.borrow().get_name()))
                            .add_to_ctx(ctx);
                            }
                        },
                        Num::Char(c) => match &*ty.borrow() {
                            PLType::Primitive(PriType::CHAR) => {
                                let c_v = builder.int_value(&PriType::CHAR, c as u64, false);
                                let v = ctx
                                    .try_load2var(Default::default(), v, builder, &ty.borrow())
                                    .unwrap();
                                let i = builder.build_int_compare(
                                    crate::ast::builder::IntPredicate::EQ,
                                    c_v,
                                    v,
                                    "eq",
                                );
                                Self::add_matched_bb(i, not_matched, ctx, builder);
                            }
                            _ => {
                                n.range().new_err(ErrorCode::ILLEGAL_MATCH_ARM_CONDITION)
                                .add_label(n.range, ctx.get_file(), format_label!("match condition is of type `{}`, imcompatible with type `{}`", "char", ty.borrow().get_name()))
                                .add_to_ctx(ctx);
                            }
                        },
                    }
                }
                Literal::String(s) => {
                    match &*ty.borrow() {
                        PLType::Struct(STType { name: n, .. }) if *n == "string" => (),
                        _ => {
                            s.range.new_err(ErrorCode::ILLEGAL_MATCH_ARM_CONDITION)
                            .add_label(s.range, ctx.get_file(), format_label!("match condition is of type `{}`, imcompatible with type `{}`", "string", ty.borrow().get_name()))
                            .add_to_ctx(ctx);
                        }
                    }
                    let f = ctx.get_gc_mod_f(builder, &"string_eq".into());
                    let s = s
                        .emit(ctx, builder)
                        .unwrap()
                        .get_value()
                        .unwrap()
                        .get_value();
                    let i = builder
                        .build_call(f, &[s, v], &PLType::Primitive(PriType::BOOL), ctx, None)
                        .unwrap();
                    Self::add_matched_bb(i, not_matched, ctx, builder);
                }
                Literal::Bool(b) => {
                    match &*ty.borrow() {
                        PLType::Primitive(PriType::BOOL) => {
                            let b_v = builder.int_value(
                                &PriType::BOOL,
                                if b.value { 1 } else { 0 },
                                false,
                            );
                            let v = ctx
                                .try_load2var(Default::default(), v, builder, &ty.borrow())
                                .unwrap();
                            let i = builder.build_int_compare(
                                crate::ast::builder::IntPredicate::EQ,
                                b_v,
                                v,
                                "eq",
                            );
                            Self::add_matched_bb(i, not_matched, ctx, builder);
                        }
                        _ => {
                            b.range().new_err(ErrorCode::ILLEGAL_MATCH_ARM_CONDITION)
                        .add_label(b.range, ctx.get_file(), format_label!("match condition is of type `{}`, imcompatible with type `{}`", "bool", ty.borrow().get_name()))
                        .add_to_ctx(ctx);
                        }
                    }
                }
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
                        } else {
                            tp.range()
                                .new_err(ErrorCode::ILLEGAL_MATCH_ARM_CONDITION)
                                .add_label(
                                    tp.range(),
                                    ctx.get_file(),
                                    format_label!(
                                        "match condition is of \
                            type `{}`, expected to be one of: `{}`",
                                        match_ty.borrow().get_name(),
                                        ustr(
                                            &u.get_sum_types(ctx, builder)
                                                .iter()
                                                .map(|t| t.borrow().get_name().to_string())
                                                .collect::<Vec<_>>()
                                                .join(", ")
                                        )
                                    ),
                                )
                                .add_to_ctx(ctx);
                        }
                    }
                    _ => {
                        tp.range()
                            .new_err(ErrorCode::ILLEGAL_MATCH_ARM_CONDITION)
                            .add_label(
                                tp.range(),
                                ctx.get_file(),
                                format_label!(
                                    "match condition like `{}` can \
                        only be used while maching through union types. Here `{}` \
                        is of type `{}`",
                                    "type(cond)",
                                    ty.borrow().get_name(),
                                    ty.borrow().get_kind_name()
                                ),
                            )
                            .add_to_ctx(ctx);
                    }
                }
            }
            MatchArmCondition::TypedDeconstruct(_, _) => todo!(),
            MatchArmCondition::Deconstruct(fields) => {
                match &*ty.borrow() {
                    PLType::Struct(
                        s @ STType {
                            is_tuple: false, ..
                        },
                    ) => {
                        for (f, c) in fields.iter_mut() {
                            ctx.push_semantic_token(f.range, SemanticTokenType::PROPERTY, 0);
                            if let Some(f) = s.fields.get(&f.name) {
                                let v_ptr = builder
                                    .build_struct_gep(v, f.index, "v", &ty.borrow(), ctx)
                                    .unwrap();
                                c.is_matched(
                                    v_ptr,
                                    f.typenode
                                        .get_type(ctx, builder, false)
                                        .unwrap_or(unknown_arc()),
                                    not_matched,
                                    ctx,
                                    builder,
                                );
                            } else {
                                f.range
                                    .new_err(ErrorCode::STRUCT_FIELD_NOT_FOUND)
                                    .add_label(
                                        s.range,
                                        s.get_path(),
                                        format_label!("struct `{}` is defined here", s.name),
                                    )
                                    .add_to_ctx(ctx);
                            }
                        }
                    }
                    _ => {
                        self.range()
                            .new_err(ErrorCode::ILLEGAL_MATCH_ARM_CONDITION)
                            .add_label(
                                self.range(),
                                ctx.get_file(),
                                format_label!(
                                    "match condition like `{}` can \
                        only be used while maching through struct types. Here `{}` \
                        is of type `{}`",
                                    "{field:cond ...}",
                                    ty.borrow().get_name(),
                                    ty.borrow().get_kind_name()
                                ),
                            )
                            .add_to_ctx(ctx);
                    }
                };
            }
            MatchArmCondition::Tuple(fields, _) => {
                match &*ty.borrow() {
                    PLType::Struct(tuple @ STType { is_tuple: true, .. }) => {
                        if fields.len() != tuple.fields.len() {
                            range
                                .new_err(ErrorCode::TUPLE_ELM_SIZE_MISS_MATCH)
                                .add_label(
                                    range,
                                    ctx.get_file(),
                                    format_label!(
                                        "found {} elements here, expect {} elements",
                                        fields.len().to_string(),
                                        tuple.fields.len().to_string(),
                                    ),
                                )
                                .add_to_ctx(ctx);
                        }
                        for (i, c) in fields.iter_mut().enumerate() {
                            if i >= tuple.fields.len() {
                                c.range()
                                    .new_err(ErrorCode::TUPLE_ELM_SIZE_MISS_MATCH)
                                    .add_to_ctx(ctx);
                                continue;
                            }
                            let mut offset = 0;
                            if !tuple.is_atomic() {
                                offset = 1;
                            }
                            let v_ptr = builder
                                .build_struct_gep(v, (i + offset) as u32, "v", &ty.borrow(), ctx)
                                .unwrap();
                            c.is_matched(
                                v_ptr,
                                tuple
                                    .fields
                                    .get(&i.to_string().into())
                                    .unwrap()
                                    .typenode
                                    .get_type(ctx, builder, false)
                                    .unwrap_or(unknown_arc()),
                                not_matched,
                                ctx,
                                builder,
                            );
                        }
                    }
                    _ => {
                        self.range()
                            .new_err(ErrorCode::ILLEGAL_MATCH_ARM_CONDITION)
                            .add_label(
                                self.range(),
                                ctx.get_file(),
                                format_label!(
                                    "match condition like `{}` can \
                        only be used while maching through union types. Here `{}` \
                        is of type `{}`",
                                    "(cond, ...)",
                                    ty.borrow().get_name(),
                                    ty.borrow().get_kind_name()
                                ),
                            )
                            .add_to_ctx(ctx);
                    }
                };
            }
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
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("MatchNode");
        self.value.print(tabs + 1, false, line.clone());
        for (cond, body) in self.arms.iter() {
            cond.print(tabs + 1, false, line.clone());
            body.print(tabs + 1, false, line.clone());
        }
    }
}

impl PrintTrait for MatchArmCondition {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        match self {
            MatchArmCondition::Discard(_) => {
                deal_line(tabs, &mut line, end);
                tab(tabs, line.clone(), end);
                println!("Discard");
            }
            MatchArmCondition::Var(v) => {
                deal_line(tabs, &mut line, end);
                tab(tabs, line.clone(), end);
                println!("Var");
                v.print(tabs + 1, end, line);
            }
            MatchArmCondition::Literal(l) => {
                deal_line(tabs, &mut line, end);
                tab(tabs, line.clone(), end);
                println!("Literal");
                match l {
                    Literal::Number(n) => n.print(tabs + 1, end, line),
                    Literal::String(s) => s.print(tabs + 1, end, line),
                    Literal::Bool(b) => b.print(tabs + 1, end, line),
                }
            }
            MatchArmCondition::TypedVar(t, c) => {
                deal_line(tabs, &mut line, end);
                tab(tabs, line.clone(), end);
                println!("TypedVar");
                t.print(tabs + 1, false, line.clone());
                c.print(tabs + 1, end, line);
            }
            MatchArmCondition::TypedDeconstruct(t, f) => {
                deal_line(tabs, &mut line, end);
                tab(tabs, line.clone(), end);
                println!("TypedDeconstruct");
                t.print(tabs + 1, false, line.clone());
                for (v, c) in f {
                    v.print(tabs + 1, false, line.clone());
                    c.print(tabs + 1, false, line.clone());
                }
            }
            MatchArmCondition::Deconstruct(f) => {
                deal_line(tabs, &mut line, end);
                tab(tabs, line.clone(), end);
                println!("Deconstruct");
                for (v, c) in f {
                    v.print(tabs + 1, false, line.clone());
                    c.print(tabs + 1, false, line.clone());
                }
            }
            MatchArmCondition::Tuple(f, _) => {
                deal_line(tabs, &mut line, end);
                tab(tabs, line.clone(), end);
                println!("Tuple");
                for c in f {
                    c.print(tabs + 1, false, line.clone());
                }
            }
        }
    }
}

impl Node for MatchNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let value = self
            .value
            .emit(ctx, builder)
            .unwrap_or_default()
            .get_value();
        let value = value.unwrap_or_default();
        let ty = get_type_deep(value.get_ty());
        let value = value.get_value();
        match &*ty.borrow() {
            PLType::Struct(_) | PLType::Primitive(_) | PLType::Union(_) => (),
            _ => {
                self.value
                    .range()
                    .new_err(ErrorCode::ILLEGAL_MATCH_VALUE)
                    .add_label(
                        self.value.range(),
                        ctx.get_file(),
                        format_label!(
                            "type `{}` of kind `{}` is not allowed to be matched",
                            ty.borrow().get_name(),
                            ty.borrow().get_kind_name()
                        ),
                    )
                    .add_to_ctx(ctx);
            }
        }
        
        // 执行穷尽性检查
        self.check_exhaustiveness(ctx, &ty);
        
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

impl MatchNode {
    /// 检查match表达式是否穷尽（覆盖了所有可能情况）
    fn check_exhaustiveness<'a, 'b>(&self, ctx: &'b mut Ctx<'a>, ty: &Arc<RefCell<PLType>>) {
        // 如果已有通配符匹配，则一定是穷尽的
        if self.has_wildcard_pattern() {
            return;
        }

        // 确定匹配表达式的结束位置，用于添加quick fix
        let match_end_pos = if let Some((_, last_arm)) = self.arms.last() {
            last_arm.range().end
        } else {
            // 如果没有匹配分支，使用整个匹配表达式的结束位置
            self.range.end
        };

        // 计算适当的缩进
        let indent = self.calculate_indent(ctx);

        match &*ty.borrow() {
            PLType::Primitive(p) => self.check_primitive_exhaustiveness(ctx, p, match_end_pos, &indent),
            PLType::Union(u) => self.check_union_exhaustiveness(ctx, u, ty, match_end_pos, &indent),
            PLType::Struct(s) if s.is_tuple => self.check_tuple_exhaustiveness(ctx, s, ty, match_end_pos, &indent),
            PLType::Struct(s) => self.check_struct_exhaustiveness(ctx, s, ty, match_end_pos, &indent),
            _ => {
                // 非枚举类型，如果没有通配符匹配，则发出警告
                if !self.has_wildcard_pattern() {
                    let mut diag = self.range
                        .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
                    let mut diag = diag
                        .add_label(
                            self.range,
                            ctx.get_file(),
                            format_label!(
                                "此匹配不穷尽，需要添加 `_` 捕获所有其他情况"
                            ),
                        );
                        
                    // 添加quick fix - 使用动态计算的缩进
                    let quick_fix_text = format!("\n{}_=> {{}}", indent);
                    diag.add_edit(match_end_pos.to(match_end_pos), quick_fix_text);
                    
                    diag.add_to_ctx(ctx);
                }
            }
        }
    }

    /// 计算基于现有模式的正确缩进
    fn calculate_indent(&self, ctx: &mut Ctx) -> String {
        // 获取第一个匹配分支的缩进
        if let Some((first_arm, _)) = self.arms.first() {
            // 获取该arm的起始位置
            let start_pos = first_arm.range().start;
            
            // 基于列号计算缩进（减1是因为列号从1开始）
            return " ".repeat(start_pos.column - 1);
        }
        
        // 默认缩进
        "    ".to_string()
    }

    /// 检查是否包含通配符模式
    fn has_wildcard_pattern(&self) -> bool {
        self.arms.iter().any(|(cond, _)| self.is_wildcard_pattern(cond))
    }

    /// 检查是否包含变量绑定模式
    fn has_variable_binding(&self) -> bool {
        self.arms.iter().any(|(cond, _)| self.is_variable_binding(cond))
    }

    /// 递归检查一个模式是否为变量绑定（包括嵌套模式）
    fn is_variable_binding(&self, cond: &MatchArmCondition) -> bool {
        match cond {
            MatchArmCondition::Discard(_) => false,
            MatchArmCondition::Var(_) => true, // 变量绑定模式
            MatchArmCondition::Literal(_) => false,
            MatchArmCondition::TypedVar(_, inner) => self.is_variable_binding(inner),
            MatchArmCondition::TypedDeconstruct(_, fields) => {
                fields.iter().any(|(_, c)| self.is_variable_binding(c))
            },
            MatchArmCondition::Deconstruct(fields) => {
                fields.iter().any(|(_, c)| self.is_variable_binding(c))
            },
            MatchArmCondition::Tuple(fields, _) => {
                fields.iter().any(|c| self.is_variable_binding(c))
            }
        }
    }

    /// 递归检查一个模式是否为通配符（包括嵌套模式）
    fn is_wildcard_pattern(&self, cond: &MatchArmCondition) -> bool {
        match cond {
            MatchArmCondition::Discard(_) => true,
            MatchArmCondition::Var(_) => false, // 变量绑定不应被视作通配符匹配
            MatchArmCondition::Literal(_) => false,
            MatchArmCondition::TypedVar(_, inner) => self.is_wildcard_pattern(inner),
            MatchArmCondition::TypedDeconstruct(_, fields) => {
                fields.iter().all(|(_, c)| self.is_wildcard_pattern(c))
            },
            MatchArmCondition::Deconstruct(fields) => {
                fields.iter().all(|(_, c)| self.is_wildcard_pattern(c))
            },
            MatchArmCondition::Tuple(fields, _) => {
                fields.iter().all(|c| self.is_wildcard_pattern(c))
            }
        }
    }

    /// 检查简单类型（如布尔、字符）的穷尽性
    fn check_primitive_exhaustiveness<'a, 'b>(&self, ctx: &'b mut Ctx<'a>, p: &PriType, match_end_pos: Pos, indent: &str) {
        match p {
            PriType::BOOL => {
                // 布尔类型需要检查true和false是否都被覆盖
                let has_true = self.arms.iter().any(|(cond, _)| {
                    self.matches_literal_bool(cond, true)
                });
                
                let has_false = self.arms.iter().any(|(cond, _)| {
                    self.matches_literal_bool(cond, false)
                });

                if !has_true || !has_false {
                    let mut diag = self.range
                        .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
                    let mut diag = diag
                        .add_label(
                            self.range,
                            ctx.get_file(),
                            format_label!(
                                "此匹配不穷尽，布尔类型需要匹配 'true' 和 'false'"
                            ),
                        );
                    
                    // 添加quick fix - 使用动态计算的缩进
                    let mut missing_patterns = Vec::new();
                    if !has_true {
                        missing_patterns.push(format!("true => {{}}"));
                    }
                    if !has_false {
                        missing_patterns.push(format!("false => {{}}"));
                    }
                    
                    let quick_fix_text = format!("\n{}{}", indent, missing_patterns.join(format!("\n{}", indent).as_str()));
                    diag.add_edit(match_end_pos.to(match_end_pos), quick_fix_text);
                    
                    diag.add_to_ctx(ctx);
                }
            }
            _ => {
                // 对于其他原始类型，如果既没有通配符，也没有变量绑定，就需要警告
                if !self.has_wildcard_pattern() && !self.has_variable_binding() {
                    let mut diag = self.range
                        .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
                    let mut diag = diag
                        .add_label(
                            self.range,
                            ctx.get_file(),
                            format_label!(
                                "此匹配不穷尽，需要添加 `_` 或变量绑定捕获所有其他值"
                            ),
                        );
                    
                    // 添加quick fix - 使用动态计算的缩进
                    let quick_fix_text = format!("\n{}_ => {{}}", indent);
                    diag.add_edit(match_end_pos.to(match_end_pos), quick_fix_text);
                    
                    diag.add_to_ctx(ctx);
                }
            }
        }
    }

    /// 递归检查是否匹配特定布尔值
    fn matches_literal_bool(&self, cond: &MatchArmCondition, value: bool) -> bool {
        match cond {
            MatchArmCondition::Discard(_) => true, // 通配符匹配任何值
            MatchArmCondition::Var(_) => true, // 变量绑定匹配任何值
            MatchArmCondition::Literal(Literal::Bool(b)) => b.value == value,
            MatchArmCondition::TypedVar(_, inner) => self.matches_literal_bool(inner, value),
            MatchArmCondition::TypedDeconstruct(_, _) => false, // 结构拆解不匹配布尔值
            MatchArmCondition::Deconstruct(_) => false, // 结构拆解不匹配布尔值
            MatchArmCondition::Tuple(_, _) => false, // 元组不匹配布尔值
            _ => false,
        }
    }

    /// 检查联合类型的穷尽性
    fn check_union_exhaustiveness<'a, 'b>(
        &self, 
        ctx: &'b mut Ctx<'a>, 
        u: &crate::ast::pltype::UnionType,
        ty: &Arc<RefCell<PLType>>,
        match_end_pos: Pos,
        indent: &str
    ) {
        let sum_types = u.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
        
        // 收集所有已匹配的类型和完整匹配
        let mut matched_exact_types = Vec::new();  // 精确匹配的类型路径
        let mut matched_full_types = Vec::new();   // 完整匹配的联合类型（包括其所有子类型）
        
        // 收集每种匹配模式的源码位置，用于错误提示
        let mut pattern_locations = FxHashMap::<String, Range>::default();
        
        // 检查直接匹配的类型
        for (cond, _) in &self.arms {
            self.collect_matched_union_types_with_loc(ctx, cond, &mut matched_exact_types, &mut matched_full_types, &mut pattern_locations);
        }
        
        // 检查是否所有联合类型成员都被匹配
        let mut missing_types = Vec::new();  // 完全未匹配的类型
        let mut partially_matched_types = Vec::new();  // 部分匹配的类型及其未匹配子类型
        
        for sum_type in &sum_types {
            let type_name = sum_type.borrow().get_name().to_string();
            
            // 如果整个类型被完整匹配了，则跳过其子类型的检查
            if matched_full_types.contains(&type_name) {
                continue;
            }
            
            // 检查是否直接匹配了这个类型
            if matched_exact_types.iter().any(|t| *t == type_name) {
                continue;
            }
            
            // 对于嵌套的联合类型，检查其子类型
            if let PLType::Union(inner_union) = &*sum_type.borrow() {
                let inner_types = inner_union.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                let mut all_inner_matched = true;
                let mut unmatched_inner_types = Vec::new();
                
                // 检查这个联合类型的每个子类型是否被匹配
                for inner_type in inner_types {
                    let inner_type_name = inner_type.borrow().get_name().to_string();
                    let full_path = format!("{}({})", type_name, inner_type_name);
                    
                    // 检查是否有直接匹配此嵌套类型的模式
                    if !matched_exact_types.iter().any(|path| *path == full_path) {
                        all_inner_matched = false;
                        unmatched_inner_types.push(inner_type_name);
                    }
                }
                
                // 如果内部所有类型都被匹配，则认为外部类型也被匹配
                if all_inner_matched {
                    continue;
                }
                
                // 如果有部分子类型被匹配，则记录为部分匹配
                if !unmatched_inner_types.is_empty() && unmatched_inner_types.len() < inner_union.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default())).len() {
                    partially_matched_types.push((type_name, unmatched_inner_types));
                    continue;
                }
            }
            
            // 如果所有检查都未通过，则记录这个未匹配的类型
            missing_types.push(type_name);
        }
        
        // 如果有未匹配的类型且没有通配符，报错
        if (!missing_types.is_empty() || !partially_matched_types.is_empty()) && !self.has_wildcard_pattern() {
            let mut error_msg = String::new();
            
            if !missing_types.is_empty() {
                error_msg.push_str(&format!("未处理这些类型: {}", missing_types.join(", ")));
            }
            
            if !partially_matched_types.is_empty() {
                if !error_msg.is_empty() {
                    error_msg.push_str("; ");
                }
                
                for (parent_type, missing_subtypes) in &partially_matched_types {
                    error_msg.push_str(&format!("类型{}缺少处理这些子类型: {}", parent_type, missing_subtypes.join(", ")));
                }
            }
            
            // 创建主错误诊断
            let mut diag = self.range
                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
            let mut diag = diag
                .add_label(
                    self.range,
                    ctx.get_file(),
                    format_label!(
                        "此匹配不穷尽。{}",
                        error_msg
                    ),
                );
            
            // 为每个已匹配的模式添加标签，以便更清晰地看到哪些模式已经被匹配
            for (type_path, loc) in pattern_locations.iter() {
                diag.add_label(
                    *loc,
                    ctx.get_file(),
                    format_label!(
                        "此模式匹配类型 `{}`",
                        type_path.clone()
                    )
                );
            }
            
            // 为每个未匹配的类型添加建议
            if !missing_types.is_empty() {
                let suggestions = missing_types.iter()
                    .map(|t| format!("{}(_)", t))
                    .collect::<Vec<_>>()
                    .join(" 或 ");
                
                diag.add_help(&format!("请考虑添加以下模式: {}", suggestions));
                
                // 添加quick fix - 在匹配表达式末尾添加缺失的模式
                let match_end_pos = if let Some((_, last_arm)) = self.arms.last() {
                    last_arm.range().end
                } else {
                    self.range.end
                };
                
                // 为每个缺失的类型生成一个匹配分支
                let mut quick_fix_text = String::new();
                for missing_type in &missing_types {
                    quick_fix_text.push_str(&format!("\n{}{}(_) => {{}}", indent, missing_type));
                }
                
                diag.add_edit(match_end_pos.to(match_end_pos), quick_fix_text);
            }
            
            // 为每个部分匹配的类型添加建议
            if !partially_matched_types.is_empty() {
                for (parent_type, missing_subtypes) in &partially_matched_types {
                    let suggestions = missing_subtypes.iter()
                        .map(|t| format!("{}{}({})", parent_type, indent, t))
                        .collect::<Vec<_>>()
                        .join(" or ");
                    
                    diag.add_help(&format!("请考虑添加以下模式: {}", suggestions));
                    
                    // 添加quick fix - 在匹配表达式末尾添加缺失的模式
                    let match_end_pos = if let Some((_, last_arm)) = self.arms.last() {
                        last_arm.range().end
                    } else {
                        self.range.end
                    };
                    
                    // 为每个缺失的子类型生成一个匹配分支
                    let mut quick_fix_text = String::new();
                    for missing_subtype in missing_subtypes {
                        quick_fix_text.push_str(&format!("\n{}{}({}(_)) => {{}}", indent, parent_type, missing_subtype));
                    }
                    
                    diag.add_edit(match_end_pos.to(match_end_pos), quick_fix_text);
                }
            }
            
            // 添加到上下文
            diag.add_to_ctx(ctx);
        }
    }
    
    /// 递归收集所有匹配的联合类型，同时记录模式位置
    fn collect_matched_union_types_with_loc<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        cond: &MatchArmCondition,
        matched_exact_types: &mut Vec<String>,
        matched_full_types: &mut Vec<String>,
        pattern_locations: &mut FxHashMap<String, Range>
    ) {
        match cond {
            MatchArmCondition::Discard(_) => {
                // 通配符匹配所有类型，但这在has_wildcard_pattern中已经检查
            },
            MatchArmCondition::Var(_) => {
                // 变量绑定匹配所有类型，但不是特定类型
            },
            MatchArmCondition::TypedVar(type_node, sub_cond) => {
                if let Ok(t) = type_node.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                    let type_name = t.borrow().get_name().to_string();
                    let range = type_node.range();
                    
                    // 记录类型位置
                    pattern_locations.insert(type_name.clone(), range);
                    
                    // 检查是否为联合类型，如果是且有子条件为通配符，则认为匹配了所有子类型
                    let is_wildcard_subcond = match &**sub_cond {
                        MatchArmCondition::Var(_) | MatchArmCondition::Discard(_) => true,
                        _ => false
                    };
                    
                    if is_wildcard_subcond {
                        // 如果子条件是通配符，则认为完全匹配了此类型
                        matched_full_types.push(type_name.clone());
                        matched_exact_types.push(type_name.clone());
                        
                        // 对于联合类型，添加所有可能的子类型组合
                        if let PLType::Union(inner_union) = &*t.borrow() {
                            let inner_types = inner_union.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                            for inner_type in inner_types {
                                let inner_name = inner_type.borrow().get_name().to_string();
                                matched_exact_types.push(inner_name);
                            }
                        }
                    } else {
                        // 子条件不是通配符，处理嵌套类型
                        match &**sub_cond {
                            MatchArmCondition::TypedVar(inner_type, _) => {
                                // 显式的嵌套类型匹配，例如 A1(i32(x))
                                if let Ok(inner_t) = inner_type.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                                    let inner_name = inner_t.borrow().get_name().to_string();
                                    let inner_range = inner_type.range();
                                    let full_path = format!("{}({})", type_name, inner_name);
                                    
                                    // 记录嵌套类型位置
                                    pattern_locations.insert(full_path.clone(), inner_range);
                                    
                                    // 记录完整的类型路径
                                    matched_exact_types.push(full_path);
                                    
                                    // 特殊处理: 如果这是一个子类型，记录它对父类型的贡献
                                    // 例如: 如果我们有A1(i32)和A1(i64)，那么A1本身也被完全匹配了
                                    if let PLType::Union(inner_union) = &*t.borrow() {
                                        let inner_types = inner_union.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                                        let mut all_matched = true;
                                        let mut matched_inner_names = vec![inner_name];
                                        
                                        // 检查此联合类型的所有成员是否都被匹配
                                        for other_arm in &self.arms {
                                            if let (MatchArmCondition::TypedVar(other_type, other_subcond), _) = other_arm {
                                                if let Ok(other_t) = other_type.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                                                    if other_t.borrow().get_name() == t.borrow().get_name() {
                                                        // 同一联合类型的另一个分支
                                                        if let MatchArmCondition::TypedVar(other_inner, _) = &**other_subcond {
                                                            if let Ok(other_inner_t) = other_inner.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                                                                matched_inner_names.push(other_inner_t.borrow().get_name().to_string());
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        
                                        // 检查是否所有子类型都被匹配
                                        for inner_type in inner_types {
                                            let inner_type_name = inner_type.borrow().get_name().to_string();
                                            if !matched_inner_names.contains(&inner_type_name) {
                                                all_matched = false;
                                                break;
                                            }
                                        }
                                        
                                        if all_matched {
                                            // 如果该联合类型的所有成员都被匹配了，记录该类型为完全匹配
                                            matched_full_types.push(type_name.clone());
                                            matched_exact_types.push(type_name);
                                        }
                                    }
                                }
                            },
                            _ => {
                                // 其他类型的匹配，只记录外层类型
                                matched_exact_types.push(type_name);
                            }
                        }
                    }
                    
                    // 继续递归检查
                    self.collect_matched_union_types_with_loc(ctx, sub_cond, matched_exact_types, matched_full_types, pattern_locations);
                }
            },
            MatchArmCondition::TypedDeconstruct(type_node, fields) => {
                if let Ok(t) = type_node.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                    let type_name = t.borrow().get_name().to_string();
                    let range = type_node.range();
                    
                    // 记录类型位置
                    pattern_locations.insert(type_name.clone(), range);
                    matched_exact_types.push(type_name);
                    
                    // 递归检查字段条件
                    for (_, sub_cond) in fields {
                        self.collect_matched_union_types_with_loc(ctx, sub_cond, matched_exact_types, matched_full_types, pattern_locations);
                    }
                }
            },
            MatchArmCondition::Literal(_) => {
                // 字面量不直接匹配联合类型
            },
            MatchArmCondition::Deconstruct(fields) => {
                // 对于结构体解构，递归检查每个字段
                for (_, sub_cond) in fields {
                    self.collect_matched_union_types_with_loc(ctx, sub_cond, matched_exact_types, matched_full_types, pattern_locations);
                }
            },
            MatchArmCondition::Tuple(fields, _) => {
                // 对于元组，递归检查每个元素
                for sub_cond in fields {
                    self.collect_matched_union_types_with_loc(ctx, sub_cond, matched_exact_types, matched_full_types, pattern_locations);
                }
            }
        }
    }
    
    /// 检查元组类型的穷尽性
    fn check_tuple_exhaustiveness<'a, 'b>(
        &self, 
        ctx: &'b mut Ctx<'a>, 
        s: &crate::ast::pltype::STType,
        ty: &Arc<RefCell<PLType>>,
        match_end_pos: Pos,
        indent: &str
    ) {
        // 检查是否有元组模式
        let tuple_patterns: Vec<_> = self.arms.iter()
            .filter_map(|(cond, _)| {
                if let MatchArmCondition::Tuple(fields, _) = cond {
                    Some(fields)
                } else {
                    None
                }
            })
            .collect();
        
        // 如果没有元组模式但有通配符，则可能是穷尽的
        if tuple_patterns.is_empty() {
            if !self.has_wildcard_pattern() {
                self.range
                    .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                    .add_label(
                        self.range,
                        ctx.get_file(),
                        format_label!(
                            "此元组匹配不穷尽，需要添加模式或 `_` 捕获所有情况"
                        ),
                    )
                    .add_to_ctx(ctx);
            }
            return;
        }
        
        // 检查元组字段是否匹配完整
        for (i, _) in s.fields.iter() {
            let field_idx = i.parse::<usize>().unwrap_or(0);
            
            // 检查每个元组模式中对应索引的字段是否都有穷尽匹配
            let mut all_fields_matched = true;
            
            for fields in &tuple_patterns {
                if field_idx >= fields.len() || !self.is_wildcard_pattern(&fields[field_idx]) {
                    all_fields_matched = false;
                    break;
                }
            }
            
            if !all_fields_matched && !self.has_wildcard_pattern() {
                self.range
                    .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                    .add_label(
                        self.range,
                        ctx.get_file(),
                        format_label!(
                            "此元组匹配不穷尽，第{}个元素的所有可能值未被完全匹配",
                            (field_idx + 1).to_string()
                        ),
                    )
                    .add_to_ctx(ctx);
                return;
            }
        }
    }
    
    /// 检查结构体类型的穷尽性
    fn check_struct_exhaustiveness<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        s: &crate::ast::pltype::STType,
        ty: &Arc<RefCell<PLType>>,
        match_end_pos: Pos,
        indent: &str
    ) {
        // 如果有通配符模式，则一定是穷尽的
        if self.has_wildcard_pattern() {
            return;
        }

        // 收集所有结构体解构模式
        let struct_patterns: Vec<_> = self.arms.iter()
            .filter_map(|(cond, _)| {
                if let MatchArmCondition::Deconstruct(fields) = cond {
                    Some(fields)
                } else {
                    None
                }
            })
            .collect();
        
        // 如果没有结构体解构模式，则不穷尽
        if struct_patterns.is_empty() {
            let mut diag = self.range
                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
            let mut diag = diag
                .add_label(
                    self.range,
                    ctx.get_file(),
                    format_label!(
                        "此结构体匹配不穷尽，需要添加解构模式或 `_` 捕获所有情况"
                    ),
                );

            // 添加结构体类型名称和建议
            let type_name = s.name.clone();
            diag.add_help(&format!(
                "结构体类型 `{}` 需要匹配模式。请考虑添加 `{{ field1: pattern1, ... }}` 形式的模式或通配符 `_`", 
                type_name
            ));
            
            // 添加quick fix - 在匹配表达式末尾添加通配符模式
            let match_end_pos = if let Some((_, last_arm)) = self.arms.last() {
                last_arm.range().end
            } else {
                // 如果没有匹配分支，使用整个匹配表达式的结束位置
                self.range.end
            };
            let quick_fix_text = "\n    _ => {}";
            diag.add_edit(match_end_pos.to(match_end_pos), quick_fix_text.to_string());
            
            diag.add_to_ctx(ctx);
            return;
        }
        
        // 检查每个结构体字段是否都被正确匹配
        let mut missing_fields = Vec::new();
        
        // 遍历结构体的所有字段
        for (field_name, field_info) in &s.fields {
            let field_type_result = field_info.typenode.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false);
            if field_type_result.is_err() {
                continue; // 忽略无法获取类型的字段
            }
            let field_type = field_type_result.unwrap();
            
            // 检查是否所有模式都包含此字段
            let all_patterns_have_field = struct_patterns.iter().all(|pattern| {
                pattern.iter().any(|(field_var, _)| field_var.name == *field_name)
            });
            
            if !all_patterns_have_field {
                missing_fields.push(field_name.to_string());
                continue;
            }
            
            // 对于每个字段，检查其类型的穷尽性
            match &*field_type.clone().borrow() {
                PLType::Primitive(PriType::BOOL) => {
                    // 布尔字段需要检查true和false是否都被覆盖
                    let patterns_with_field: Vec<_> = struct_patterns.iter()
                        .filter_map(|pattern| {
                            pattern.iter()
                                .find(|(field_var, _)| field_var.name == *field_name)
                                .map(|(_, cond)| cond)
                        })
                        .collect();
                    
                    let has_true = patterns_with_field.iter().any(|cond| {
                        self.matches_literal_bool(cond, true)
                    });
                    
                    let has_false = patterns_with_field.iter().any(|cond| {
                        self.matches_literal_bool(cond, false)
                    });
                    
                    if !has_true || !has_false {
                        self.range
                            .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                            .add_label(
                                self.range,
                                ctx.get_file(),
                                format_label!(
                                    "此结构体匹配不穷尽，字段 `{}` 是布尔类型，需要匹配 'true' 和 'false'",
                                    *field_name
                                ),
                            )
                            .add_to_ctx(ctx);
                        return;
                    }
                },
                PLType::Union(_) => {
                    // 联合类型字段需要检查所有可能的变体
                    let patterns_with_field: Vec<_> = struct_patterns.iter()
                        .filter_map(|pattern| {
                            pattern.iter()
                                .find(|(field_var, _)| field_var.name == *field_name)
                                .map(|(_, cond)| cond)
                        })
                        .collect();
                    
                    // 检查是否至少有一个模式是通配符或变量绑定
                    let has_wildcard = patterns_with_field.iter().any(|cond| {
                        self.is_wildcard_pattern(cond)
                    });
                    
                    if !has_wildcard {
                        // 收集所有已匹配的类型
                        let mut matched_types = Vec::new();
                        let mut matched_full_types = Vec::new();
                        
                        // 检查所有结构体模式中的这个字段的所有条件
                        for cond in &patterns_with_field {
                            self.collect_matched_union_types_with_loc(ctx, cond, &mut matched_types, &mut matched_full_types, &mut FxHashMap::default());
                        }
                        
                        // 获取联合类型的所有可能变体
                        if let PLType::Union(union_type) = &*field_type.borrow() {
                            let sum_types = union_type.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                            let mut unmatched_types = Vec::new();
                            
                            // 进行穷尽性检查
                            for sum_type in &sum_types {
                                let type_name = sum_type.borrow().get_name().to_string();
                                
                                // 如果这个类型已被完全匹配(例如A1类型被A1(i32)和A1(i64)完全覆盖)，则跳过
                                if matched_full_types.contains(&type_name) {
                                    continue;
                                }
                                
                                // 检查是否直接匹配了这个类型
                                if matched_types.iter().any(|t| *t == type_name) {
                                    continue;
                                }
                                
                                // 对于嵌套的联合类型(如A1 = i32|i64)，检查其所有成员是否都被匹配
                                if let PLType::Union(inner_union) = &*sum_type.borrow() {
                                    let inner_types = inner_union.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                                    let mut all_inner_matched = true;
                                    let mut missing_inner_types = Vec::new();
                                    
                                    for inner_type in &inner_types {
                                        let inner_name = inner_type.borrow().get_name().to_string();
                                        let full_path = format!("{}({})", type_name, inner_name);
                                        
                                        if !matched_types.iter().any(|t| *t == full_path) {
                                            all_inner_matched = false;
                                            missing_inner_types.push(inner_name);
                                        }
                                    }
                                    
                                    // 如果内部所有类型都被匹配，则认为外部类型也被匹配
                                    if all_inner_matched {
                                        continue;
                                    }
                                    
                                    // 如果只有部分内部类型未匹配，添加它们到未匹配类型列表中
                                    if !missing_inner_types.is_empty() {
                                        let formatted_missing = missing_inner_types.iter()
                                            .map(|t| format!("{}({})", type_name, t))
                                            .collect::<Vec<_>>();
                                        unmatched_types.extend(formatted_missing);
                                        continue;
                                    }
                                }
                                
                                // 如果所有检查都未通过，则记录这个未匹配的类型
                                unmatched_types.push(type_name);
                            }
                            
                            if !unmatched_types.is_empty() {
                                // 创建更详细的错误诊断
                                let mut diag = self.range.new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
                                
                                // 添加主标签，解释具体是哪个字段不穷尽及缺少哪些类型
                                diag.add_label(
                                    self.range,
                                    ctx.get_file(),
                                    format_label!(
                                        "此结构体匹配不穷尽，字段 `{}` 是联合类型，未匹配这些可能的类型: {}",
                                        *field_name,
                                        unmatched_types.join(", ")
                                    ),
                                );
                                
                                // 为每个已匹配的模式添加标签
                                for (i, pattern) in struct_patterns.iter().enumerate() {
                                    if let Some((field_var, cond)) = pattern.iter().find(|(fv, _)| fv.name == *field_name) {
                                        diag.add_label(
                                            cond.range(),
                                            ctx.get_file(), 
                                            format_label!(
                                                "模式 #{} 中字段 `{}` 的匹配模式",
                                                (i + 1).to_string(),
                                                *field_name
                                            ),
                                        );
                                    }
                                }
                                
                                // 提取第一个匹配分支的格式
                                let mut template_arm = None;
                                if let Some(pattern) = struct_patterns.first() {
                                    // 复制完整的结构体解构模式，稍后会替换特定字段
                                    let mut fields_str = Vec::new();
                                    for (var, cond) in *pattern {
                                        // 对于目标字段，我们会在后面替换
                                        if var.name == *field_name {
                                            fields_str.push(format!("{}:PLACEHOLDER", var.name));
                                        } else {
                                            // 保留其他字段的原始模式
                                            fields_str.push(format!("{}:{}", var.name, get_pattern_text(cond,ctx)));
                                        }
                                    }
                                    template_arm = Some(format!("{{{}}}", fields_str.join(",")));
                                }
                                
                                // 生成quick fix
                                let mut quick_fix_text = String::new();
                                
                                for unmatched_type in &unmatched_types {
                                    if let Some(ref template) = template_arm {
                                        // 为每个未匹配的类型创建一个新的匹配分支
                                        let mut new_arm = template.clone();
                                        
                                        // 替换目标字段的占位符
                                        let field_pattern = if unmatched_type.contains("(") {
                                            // 已经是嵌套格式，如 "A1(i64)"
                                            format!("{}:{}", field_name, unmatched_type)
                                        } else {
                                            // 需要添加变量绑定，如 "A1(x)"
                                            format!("{}:{}(x)", field_name, unmatched_type)
                                        };
                                        
                                        new_arm = new_arm.replace(&format!("{}:PLACEHOLDER", field_name), &field_pattern);
                                        quick_fix_text.push_str(&format!("\n{}{} => {{}}", indent, new_arm));
                                    } else {
                                        // 如果没有模板，使用简单格式
                                        let field_pattern = if unmatched_type.contains("(") {
                                            // 已经是嵌套格式
                                            format!("{}:{}", field_name, unmatched_type)
                                        } else {
                                            format!("{}:{}(x)", field_name, unmatched_type)
                                        };
                                        
                                        quick_fix_text.push_str(&format!("\n{}{{{},a:a,c:_}} => {{}}", indent, field_pattern));
                                    }
                                }
                                
                                // 添加建议文本
                                let suggestions = unmatched_types.iter()
                                    .map(|t| {
                                        if t.contains("(") {
                                            // 对于嵌套联合类型，给出更具体的示例
                                            format!("{}: {}", field_name, t)
                                        } else {
                                            format!("{}: {}(x)", field_name, t)
                                        }
                                    })
                                    .collect::<Vec<_>>()
                                    .join(" 或 ");
                                
                                diag.add_help(&format!("请考虑添加以下模式: {}", suggestions));
                                
                                // 添加quick fix
                                diag.add_edit(match_end_pos.to(match_end_pos), quick_fix_text);
                                diag.add_to_ctx(ctx);
                                return;
                            }
                        }
                    }
                },
                PLType::Struct(inner_s) if inner_s.is_tuple => {
                    // 对于元组类型的字段，递归检查
                    let patterns_with_field: Vec<_> = struct_patterns.iter()
                        .filter_map(|pattern| {
                            pattern.iter()
                                .find(|(field_var, _)| field_var.name == *field_name)
                                .map(|(_, cond)| cond)
                        })
                        .collect();
                    
                    let has_inner_wildcard = patterns_with_field.iter().any(|cond| {
                        self.is_wildcard_pattern(cond)
                    });
                    
                    if !has_inner_wildcard {
                        // 检查元组模式是否包含所有必要的元素
                        let tuple_patterns: Vec<_> = patterns_with_field.iter()
                            .filter_map(|cond| {
                                if let MatchArmCondition::Tuple(fields, _) = cond {
                                    Some(fields)
                                } else {
                                    None
                                }
                            })
                            .collect();
                        
                        if tuple_patterns.is_empty() {
                            let mut diag = self.range
                                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
                            let mut diag = diag
                                .add_label(
                                    self.range,
                                    ctx.get_file(),
                                    format_label!(
                                        "此结构体匹配不穷尽，字段 `{}` 是元组类型，需要添加元组模式",
                                        *field_name
                                    ),
                                );
                            
                            // 根据元组的元素数量生成建议
                            let element_count = inner_s.fields.len();
                            let mut element_placeholders = Vec::new();
                            for i in 0..element_count {
                                element_placeholders.push("_".to_string());
                            }
                            
                            let suggestion = format!("{}: ({})", field_name, element_placeholders.join(", "));
                            diag.add_help(&format!("请考虑添加以下模式: {}", suggestion));
                            
                            // 添加quick fix - 在匹配表达式末尾添加包含元组模式的匹配分支
                            let match_end_pos = if let Some((_, last_arm)) = self.arms.last() {
                                last_arm.range().end
                            } else {
                                self.range.end
                            };
                            let quick_fix_text = format!("\n    {} => {{}}", suggestion);
                            diag.add_edit(match_end_pos.to(match_end_pos), quick_fix_text);
                            
                            diag.add_to_ctx(ctx);
                            return;
                        }
                        
                        // 检查元组每个元素是否都被正确匹配
                        for (i, _) in inner_s.fields.iter() {
                            let element_idx = i.parse::<usize>().unwrap_or(0);
                            
                            // 检查每个元组模式是否都包含此元素
                            for fields in &tuple_patterns {
                                if element_idx >= fields.len() {
                                    let mut diag = self.range
                                    .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
                                    let mut diag = diag
                                        .add_label(
                                            self.range,
                                            ctx.get_file(),
                                            format_label!(
                                                "此结构体匹配不穷尽，字段 `{}` 的元组第{}个元素未被匹配",
                                                *field_name,
                                                (element_idx + 1).to_string()
                                            ),
                                        );
                                    
                                    // 构建一个完整的元组模式示例
                                    let mut element_placeholders = Vec::new();
                                    for j in 0..inner_s.fields.len() {
                                        element_placeholders.push("_".to_string());
                                    }
                                    
                                    let suggestion = format!("{}: ({})", field_name, element_placeholders.join(", "));
                                    diag.add_help(&format!("请考虑使用完整的元组模式: {}", suggestion));
                                    diag.add_to_ctx(ctx);
                                    return;
                                }
                            }
                        }
                    }
                },
                PLType::Struct(inner_s) => {
                    // 对于嵌套结构体，递归进行同样的检查
                    let patterns_with_field: Vec<_> = struct_patterns.iter()
                        .filter_map(|pattern| {
                            pattern.iter()
                                .find(|(field_var, _)| field_var.name == *field_name)
                                .map(|(_, cond)| cond)
                        })
                        .collect();
                    
                    let has_inner_wildcard = patterns_with_field.iter().any(|cond| {
                        self.is_wildcard_pattern(cond)
                    });
                    
                    if !has_inner_wildcard {
                        // 递归检查内部结构体的解构模式
                        let inner_deconstruct_patterns: Vec<_> = patterns_with_field.iter()
                            .filter_map(|cond| {
                                if let MatchArmCondition::Deconstruct(fields) = cond {
                                    Some(fields)
                                } else {
                                    None
                                }
                            })
                            .collect();
                        
                        if inner_deconstruct_patterns.is_empty() {
                            let mut diag = self.range
                                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
                            let mut diag = diag
                                .add_label(
                                    self.range,
                                    ctx.get_file(),
                                    format_label!(
                                        "此结构体匹配不穷尽，字段 `{}` 是结构体类型，需要添加解构模式",
                                        *field_name
                                    ),
                                );
                            
                            // 生成一个简单的结构体模式建议
                            let suggestion = format!("{}: {{ ... }}", field_name);
                            diag.add_help(&format!("请考虑添加解构模式: {}", suggestion));
                            
                            // 添加quick fix - 添加结构体解构模式
                            let match_end_pos = if let Some((_, last_arm)) = self.arms.last() {
                                last_arm.range().end
                            } else {
                                self.range.end
                            };
                            
                            // 尝试构建一个包含内部结构体所有字段的解构模式
                            let mut field_placeholders = Vec::new();
                            for (inner_field_name, _) in &inner_s.fields {
                                field_placeholders.push(format!("{}: _", inner_field_name));
                            }
                            
                            let detailed_suggestion = if !field_placeholders.is_empty() {
                                format!("{}: {{ {} }}", field_name, field_placeholders.join(", "))
                            } else {
                                format!("{}: {{ }}", field_name)
                            };
                            
                            let quick_fix_text = format!("\n    {} => {{}}", detailed_suggestion);
                            diag.add_edit(match_end_pos.to(match_end_pos), quick_fix_text);
                            
                            diag.add_to_ctx(ctx);
                            return;
                        }
                        
                        // 使用递归函数进行真正的递归检查，传递当前字段路径信息
                        self.check_struct_field_exhaustiveness_recursive(
                            ctx,
                            field_name,
                            inner_s,
                            &inner_deconstruct_patterns
                        );
                    }
                },
                _ => {
                    // 对于其他类型字段，检查是否有通配符或变量绑定
                    let patterns_with_field: Vec<_> = struct_patterns.iter()
                        .filter_map(|pattern| {
                            pattern.iter()
                                .find(|(field_var, _)| field_var.name == *field_name)
                                .map(|(_, cond)| cond)
                        })
                        .collect();
                    
                    // 先检查是否有变量绑定模式，这种模式可以匹配任何值
                    let has_var_binding = patterns_with_field.iter().any(|cond| {
                        self.is_variable_binding(cond)
                    });
                    
                    // 再检查是否有通配符模式
                    let has_inner_wildcard = patterns_with_field.iter().any(|cond| {
                        self.is_wildcard_pattern(cond)
                    });
                    
                    // 如果既没有通配符也没有变量绑定
                    if !has_inner_wildcard && !has_var_binding {
                        // 获取当前字段的类型名称，用于更有用的错误信息
                        let type_name = field_type.borrow().get_name().to_string();
                        
                        // 创建更详细的错误诊断
                        let mut diag = self.range.new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
                        
                        // 添加主标签，解释具体是哪个字段不穷尽
                        diag.add_label(
                            self.range,
                            ctx.get_file(),
                            format_label!(
                                "此结构体匹配不穷尽，字段 `{}` 的类型为 `{}`，需要添加通配符或变量绑定来匹配所有可能的值",
                                *field_name,
                                type_name
                            ),
                        );
                        
                        // 添加帮助信息，提供更明确的修复建议
                        diag.add_help(&format!(
                            "请考虑在匹配模式中为字段 `{}` 添加一个变量绑定 (如 `{}:x`) 或通配符 (如 `{}:_`)",
                            *field_name, *field_name, *field_name
                        ));
                        
                        // 如果有匹配模式，为每个模式添加标签
                        for (i, pattern) in struct_patterns.iter().enumerate() {
                            if let Some((field_var, cond)) = pattern.iter().find(|(fv, _)| fv.name == *field_name) {
                                diag.add_label(
                                    cond.range(),
                                    ctx.get_file(),
                                    format_label!(
                                        "在模式 #{} 中，字段 `{}` 使用此模式，但不足以匹配所有可能值",
                                        (i + 1).to_string(),
                                        *field_name
                                    ),
                                );
                            }
                        }
                        
                        diag.add_to_ctx(ctx);
                        return;
                    }
                }
            }
        }
        
        // 如果有缺失的字段，提示错误
        if !missing_fields.is_empty() {
            let mut diag = self.range
                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
            let mut diag = diag
                .add_label(
                    self.range,
                    ctx.get_file(),
                    format_label!(
                        "此结构体匹配不穷尽，缺少这些字段: {}",
                        missing_fields.join(", ")
                    ),
                );
            
            // 添加更具体的修复建议
            let mut field_suggestions = Vec::new();
            for field in &missing_fields {
                field_suggestions.push(format!("{}: _", field));
            }
            let suggestion = field_suggestions.join(", ");
            
            diag.add_help(&format!(
                "请在匹配模式中添加缺失的字段: {{ {} }}",
                suggestion
            ));
            
            // 添加quick fix - 在匹配表达式末尾添加一个包含所有缺失字段的模式
            let match_end_pos = if let Some((_, last_arm)) = self.arms.last() {
                last_arm.range().end
            } else {
                self.range.end
            };
            let quick_fix_text = format!("\n{}{{ {} }} => {{}}", indent, suggestion);
            diag.add_edit(match_end_pos.to(match_end_pos), quick_fix_text);
            
            diag.add_to_ctx(ctx);
        }
    }
    
    /// 递归检查结构体字段的穷尽性，可以处理任意深度的嵌套结构体
    fn check_struct_field_exhaustiveness_recursive<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        field_path: &str,
        struct_type: &crate::ast::pltype::STType,
        deconstruct_patterns: &Vec<&Vec<(VarNode, MatchArmCondition)>>
    ) {
        // 检查结构体的每个字段
        for (field_name, field_info) in &struct_type.fields {
            // 检查每个解构模式是否都包含这个字段
            let missing_field = deconstruct_patterns.iter().any(|fields| {
                !fields.iter().any(|(field_var, _)| field_var.name == *field_name)
            });
            
            // 生成当前字段的完整路径，使用更直观的格式
            let current_field_path = if field_path.is_empty() {
                field_name.to_string()
            } else {
                format!("{} → {}", field_path, field_name)
            };
            
            if missing_field {
                let mut diag = self.range
                    .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
                let mut diag = diag
                    .add_label(
                        self.range,
                        ctx.get_file(),
                        format_label!(
                            "此结构体匹配不穷尽，字段 `{}` 在某些模式中缺失",
                            current_field_path
                        ),
                    )
                    .add_to_ctx(ctx);
                return;
            }
            
            // 获取字段的类型
            let field_type_result = field_info.typenode.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false);
            if field_type_result.is_err() {
                continue; // 忽略无法获取类型的字段
            }
            let field_type = field_type_result.unwrap();
            
            // 为字段收集所有匹配模式
            let field_patterns: Vec<_> = deconstruct_patterns.iter()
                .filter_map(|fields| {
                    fields.iter()
                        .find(|(field_var, _)| field_var.name == *field_name)
                        .map(|(_, cond)| cond)
                })
                .collect();
            
            // 检查是否有通配符模式
            let has_wildcard = field_patterns.iter().any(|cond| {
                self.is_wildcard_pattern(cond)
            });
            
            // 首先检查是否所有模式都是TypedVar，如果是，这可能是一个联合类型
            let all_typed_var = field_patterns.iter().all(|cond| {
                match cond {
                    MatchArmCondition::TypedVar(_, _) => true,
                    _ => false
                }
            });
            
            // 如果全是TypedVar模式，则按照联合类型处理
            if all_typed_var && !field_patterns.is_empty() {
                // 收集所有可能匹配的类型
                let mut matched_types = Vec::new();
                let mut matched_full_types = Vec::new();
                
                for cond in &field_patterns {
                    if let MatchArmCondition::TypedVar(type_node, _) = cond {
                        if let Ok(t) = type_node.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                            let type_name = t.borrow().get_name().to_string();
                            matched_types.push(type_name.clone());
                            matched_full_types.push(type_name);
                        }
                    }
                }
                
                // 尝试从字段类型判断是否可能是联合类型的包装
                if let PLType::Struct(s) = &*field_type.borrow() {
                    // 可能性1: 字段是联合类型别名
                    if let Some(typedef_name) = field_type.borrow().get_name().strip_prefix("type ") {
                        // 检查是否有匹配该类型的所有变体
                        if s.name.contains("|") {
                            let variants: Vec<&str> = s.name.split('|').collect();
                            for variant in variants {
                                let variant = variant.trim();
                                if !matched_types.iter().any(|t| t == variant) {
                                    self.range
                                        .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                                        .add_label(
                                            self.range,
                                            ctx.get_file(),
                                            format_label!(
                                                "此结构体匹配不穷尽，字段 `{}` 是联合类型，未匹配变体 `{}`",
                                                current_field_path,
                                                variant
                                            ),
                                        )
                                        .add_to_ctx(ctx);
                                    return;
                                }
                            }
                        }
                    }
                    
                    // 可能性2: 检查字段名和类型是否表明这是联合类型
                    let type_info = field_info.typenode.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false);
                    if let Ok(t) = type_info {
                        if let PLType::Union(u) = &*t.borrow() {
                            let sum_types = u.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                            
                            // 检查每个联合类型成员是否都被匹配
                            let mut unmatched_types = Vec::new();
                            
                            for sum_type in &sum_types {
                                let type_name = sum_type.borrow().get_name().to_string();
                                
                                if !matched_types.iter().any(|t| *t == type_name) {
                                    unmatched_types.push(type_name);
                                }
                            }
                            
                            if !unmatched_types.is_empty() {
                                self.range
                                    .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                                    .add_label(
                                        self.range,
                                        ctx.get_file(),
                                        format_label!(
                                            "此结构体匹配不穷尽，字段 `{}` 是联合类型，未匹配这些类型: {}",
                                            current_field_path,
                                            unmatched_types.join(", ")
                                        ),
                                    )
                                    .add_to_ctx(ctx);
                                return;
                            }
                        }
                    }
                }
                
                // 如果以上检查都没有问题，则认为匹配是穷尽的
                continue;
            }
            
            // 如果没有通配符，则根据字段类型进行相应的检查
            if !has_wildcard {
                match &*field_type.borrow() {
                    PLType::Primitive(PriType::BOOL) => {
                        // 布尔字段需要检查true和false是否都被覆盖
                        let has_true = field_patterns.iter().any(|cond| {
                            self.matches_literal_bool(cond, true)
                        });
                        
                        let has_false = field_patterns.iter().any(|cond| {
                            self.matches_literal_bool(cond, false)
                        });
                        
                        if !has_true || !has_false {
                            self.range
                                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                                .add_label(
                                    self.range,
                                    ctx.get_file(),
                                    format_label!(
                                        "此结构体匹配不穷尽，字段 `{}` 是布尔类型，需要匹配 'true' 和 'false'",
                                        current_field_path
                                    ),
                                )
                                .add_to_ctx(ctx);
                            return;
                        }
                    },
                    PLType::Union(u) => {
                        // 联合类型需要检查所有可能的变体
                        let sum_types = u.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                        
                        // 收集所有已匹配的类型
                        let mut matched_types = Vec::new();
                        let mut matched_full_types = Vec::new();
                        
                        for cond in &field_patterns {
                            self.collect_matched_union_types_with_loc(ctx, cond, &mut matched_types, &mut matched_full_types, &mut FxHashMap::default());
                        }
                        
                        // 检查每个联合类型成员是否都被匹配
                        let mut unmatched_types = Vec::new();
                        
                        for sum_type in &sum_types {
                            let type_name = sum_type.borrow().get_name().to_string();
                            
                            // 如果这个类型已被完全匹配，则跳过
                            if matched_full_types.contains(&type_name) {
                                continue;
                            }
                            
                            // 检查是否直接匹配了这个类型
                            if matched_types.iter().any(|t| *t == type_name) {
                                continue;
                            }
                            
                            // 对于嵌套的联合类型，检查其所有成员是否都被匹配
                            if let PLType::Union(inner_union) = &*sum_type.borrow() {
                                let inner_types = inner_union.get_sum_types(ctx, &BuilderEnum::NoOp(Default::default()));
                                let mut all_inner_matched = true;
                                let mut missing_inner_types = Vec::new();
                                
                                for inner_type in &inner_types {
                                    let inner_name = inner_type.borrow().get_name().to_string();
                                    let full_path = format!("{}({})", type_name, inner_name);
                                    
                                    if !matched_types.iter().any(|t| *t == full_path) {
                                        all_inner_matched = false;
                                        missing_inner_types.push(inner_name);
                                    }
                                }
                                
                                // 如果内部所有类型都被匹配，则认为外部类型也被匹配
                                if all_inner_matched {
                                    continue;
                                }
                                
                                // 如果只有部分内部类型未匹配，添加它们到未匹配类型列表中
                                if !missing_inner_types.is_empty() {
                                    let formatted_missing = missing_inner_types.iter()
                                        .map(|t| format!("{}({})", type_name, t))
                                        .collect::<Vec<_>>();
                                    unmatched_types.extend(formatted_missing);
                                    continue;
                                }
                            }
                            
                            // 如果所有检查都未通过，则记录这个未匹配的类型
                            unmatched_types.push(type_name);
                        }
                        
                        if !unmatched_types.is_empty() {
                            let mut diag = self.range
                                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS);
                            let mut diag = diag
                                .add_label(
                                    self.range,
                                    ctx.get_file(),
                                    format_label!(
                                        "此结构体匹配不穷尽，字段 `{}` 的这些类型没有被匹配: {}",
                                        current_field_path,
                                        unmatched_types.join(", ")
                                    ),
                                );
                            
                            // 生成具体的修复建议
                            let suggestions = unmatched_types.iter()
                                .map(|t| {
                                    if t.contains("(") {
                                        // 对于嵌套联合类型，给出更具体的示例
                                        format!("{}: {}(_)", field_name, t)
                                    } else {
                                        format!("{}: {}(_)", field_name, t)
                                    }
                                })
                                .collect::<Vec<_>>()
                                .join(" 或 ");
                            
                            diag.add_help(&format!("请考虑添加以下模式: {}", suggestions));
                            diag.add_to_ctx(ctx);
                            return;
                        }
                    },
                    PLType::Struct(inner_s) if inner_s.is_tuple => {
                        // 检查元组模式是否包含所有必要的元素
                        let tuple_patterns: Vec<_> = field_patterns.iter()
                            .filter_map(|cond| {
                                if let MatchArmCondition::Tuple(fields, _) = cond {
                                    Some(fields)
                                } else {
                                    None
                                }
                            })
                            .collect();
                        
                        if tuple_patterns.is_empty() {
                            self.range
                                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                                .add_label(
                                    self.range,
                                    ctx.get_file(),
                                    format_label!(
                                        "此结构体匹配不穷尽，字段 `{}` 是元组类型，需要添加元组模式",
                                        current_field_path
                                    ),
                                )
                                .add_to_ctx(ctx);
                            return;
                        }
                        
                        // 检查元组每个元素是否都被正确匹配
                        for (i, _) in inner_s.fields.iter() {
                            let element_idx = i.parse::<usize>().unwrap_or(0);
                            
                            // 检查每个元组模式是否都包含此元素
                            for fields in &tuple_patterns {
                                if element_idx >= fields.len() {
                                    self.range
                                        .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                                        .add_label(
                                            self.range,
                                            ctx.get_file(),
                                            format_label!(
                                                "此结构体匹配不穷尽，字段 `{}` 的元组第{}个元素未被匹配",
                                                current_field_path,
                                                (element_idx + 1).to_string()
                                            ),
                                        )
                                        .add_to_ctx(ctx);
                                    return;
                                }
                            }
                        }
                    },
                    PLType::Struct(inner_s) => {
                        // 对于嵌套结构体，继续递归检查
                        // 首先收集所有解构模式
                        let nested_deconstruct_patterns: Vec<_> = field_patterns.iter()
                            .filter_map(|cond| {
                                if let MatchArmCondition::Deconstruct(fields) = cond {
                                    Some(fields)
                                } else {
                                    None
                                }
                            })
                            .collect();
                        
                        if nested_deconstruct_patterns.is_empty() {
                            self.range
                                .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                                .add_label(
                                    self.range,
                                    ctx.get_file(),
                                    format_label!(
                                        "此结构体匹配不穷尽，字段 `{}` 是结构体类型，需要添加解构模式",
                                        current_field_path
                                    ),
                                )
                                .add_to_ctx(ctx);
                            return;
                        }
                        
                        // 递归检查更深层次的嵌套结构体（使用更新后的路径格式）
                        self.check_struct_field_exhaustiveness_recursive(
                            ctx,
                            &current_field_path,
                            inner_s,
                            &nested_deconstruct_patterns
                        );
                    },
                    _ => {
                        // 对于其他类型字段，检查是否有通配符或变量绑定
                        self.range
                            .new_err(ErrorCode::NON_EXHAUSTIVE_PATTERNS)
                            .add_label(
                                self.range,
                                ctx.get_file(),
                                format_label!(
                                    "此结构体匹配不穷尽，字段 `{}` 需要通配符或变量绑定",
                                    current_field_path
                                ),
                            )
                            .add_to_ctx(ctx);
                        return;
                    }
                }
            }
        }
    }
}

// 新增一个辅助方法，用于获取匹配分支的格式
fn get_arm_format(cond: &MatchArmCondition, field_path: &str) -> String {
    match cond {
        MatchArmCondition::Deconstruct(fields) => {
            // 完整复制结构体解构模式的格式
            let mut fields_str = Vec::new();
            for (var, _) in fields {
                fields_str.push(format!("{}:_", var.name));
            }
            format!("{{{}}}", fields_str.join(","))
        },
        // 其他类型的条件，返回空字符串或简单格式
        _ => "{".to_string()
    }
}

// 辅助方法，用于获取匹配条件的文本表示
fn get_pattern_text<'a>(cond: &MatchArmCondition, ctx: &mut Ctx<'a>) -> String {
    match cond {
        MatchArmCondition::Discard(_) => "_".to_string(),
        MatchArmCondition::Var(v) => v.name.to_string(),
        MatchArmCondition::Literal(lit) => match lit {
            Literal::Number(n) => n.value.to_string(),
            Literal::String(s) => format!("\"{}\"", s.content),
            Literal::Bool(b) => b.value.to_string(),
        },
        MatchArmCondition::TypedVar(t, c) => {
            if let Ok(ty) = t.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                let type_name = ty.borrow().get_name().to_string();
                match &**c {
                    MatchArmCondition::Var(v) => format!("{}({})", type_name, v.name),
                    _ => format!("{}({})", type_name, get_pattern_text(c,ctx)),
                }
            } else {
                "unknown_type".to_string()
            }
        },
        MatchArmCondition::Tuple(fields, _) => {
            let mut fields_text = Vec::new();
            for field in fields {
                fields_text.push(get_pattern_text(field,ctx));
            }
            format!("({})", fields_text.join(","))
        },
        MatchArmCondition::Deconstruct(fields) => {
            let mut fields_text = Vec::new();
            for (var, cond) in fields {
                fields_text.push(format!("{}:{}", var.name, get_pattern_text(cond,ctx)));
            }
            format!("{{{}}}", fields_text.join(","))
        },
        MatchArmCondition::TypedDeconstruct(t, fields) => {
            if let Ok(ty) = t.get_type(ctx, &BuilderEnum::NoOp(Default::default()), false) {
                let type_name = ty.borrow().get_name().to_string();
                let mut fields_text = Vec::new();
                for (var, cond) in fields {
                    fields_text.push(format!("{}:{}", var.name, get_pattern_text(cond,ctx)));
                }
                format!("{}({{{}}})", type_name, fields_text.join(","))
            } else {
                "unknown_type".to_string()
            }
        }
    }
}
