use super::node_result::TerminatorEnum;
use super::*;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::ctx::Ctx;
use crate::ast::diag::{ErrorCode, WarnCode};
use crate::format_label;

use crate::inference::unknown_arc;
use crate::modifier_set;

use indexmap::IndexMap;
use internal_macro::node;
use internal_macro::range;
use lsp_types::SemanticTokenType;

#[node(comment)]
/// DefNode stands for a let statement to define a new variable
///
pub struct DefNode {
    /// var is the definition of a variable
    pub var: Box<DefVar>,
    /// variable_type is the type provided during defining the variable
    pub variable_type: Option<Box<TypeNodeEnum>>,

    /// value_expression is the value of the var
    pub value_expression: Option<Box<NodeEnum>>,
}

#[range]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TupleDeconstructNode {
    pub var: Vec<Box<DefVar>>,
}

impl PrintTrait for TupleDeconstructNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("TupleDeconstructNode");
        for (i, v) in self.var.iter().enumerate() {
            v.print(tabs + 1, i == self.var.len() - 1, line.clone());
        }
    }
}

#[range]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDeconstructNode {
    pub var: Vec<StructFieldDeconstructEnum>,
}

impl PrintTrait for StructDeconstructNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StructDeconstructNode");
        for (i, v) in self.var.iter().enumerate() {
            deal_line(tabs + 1, &mut line, i == self.var.len() - 1);
            v.print(tabs + 1, i == self.var.len() - 1, line.clone());
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StructFieldDeconstructEnum {
    Var(VarNode),                // normal field deconstruct like: let {a} = s;
    Taged(VarNode, Box<DefVar>), // taged field deconstruct like: let {a: b} = s;
}

impl RangeTrait for StructFieldDeconstructEnum {
    fn range(&self) -> Range {
        match self {
            StructFieldDeconstructEnum::Var(v) => v.range(),
            StructFieldDeconstructEnum::Taged(v, _) => v.range(),
        }
    }
}

impl PrintTrait for StructFieldDeconstructEnum {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>) {
        match self {
            StructFieldDeconstructEnum::Var(v) => v.print(tabs, end, line),
            StructFieldDeconstructEnum::Taged(v, d) => {
                v.print(tabs, false, line.clone());
                d.print(tabs + 1, true, line);
            }
        }
    }
}

impl PrintTrait for DefNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("DefNode");
        self.var.print(tabs + 1, false, line.clone());
        if let Some(tp) = &self.variable_type {
            tp.print(tabs + 1, true, line.clone());
        } else if let Some(e) = self.value_expression.as_ref() {
            e.print(tabs + 1, true, line.clone());
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// DefVar stands for the variables defined by `let` keyword
/// it might be a single variable, a deconstructed tuple, or a deconstructed structure.
pub enum DefVar {
    Identifier(VarNode),
    TupleDeconstruct(TupleDeconstructNode),
    StructDeconstruct(StructDeconstructNode),
}

impl DefVar {
    pub fn format(&self, builder: &mut FmtBuilder) {
        match self {
            DefVar::Identifier(v) => v.format(builder),
            DefVar::TupleDeconstruct(node) => {
                builder.l_paren();
                for (i, v) in node.var.iter().enumerate() {
                    v.format(builder);
                    if i != node.var.len() - 1 {
                        builder.comma();
                        builder.space();
                    }
                }
                builder.r_paren();
            }
            DefVar::StructDeconstruct(node) => {
                builder.l_brace();
                for (i, v) in node.var.iter().enumerate() {
                    match v {
                        StructFieldDeconstructEnum::Var(v) => v.format(builder),
                        StructFieldDeconstructEnum::Taged(v, d) => {
                            v.format(builder);
                            builder.colon();
                            builder.space();
                            d.format(builder);
                        }
                    }
                    if i != node.var.len() - 1 {
                        builder.comma();
                        builder.space();
                    }
                }
                builder.r_brace();
            }
        }
    }
}

impl RangeTrait for DefVar {
    fn range(&self) -> Range {
        match self {
            DefVar::Identifier(v) => v.range(),
            DefVar::TupleDeconstruct(v) => v.range(),
            DefVar::StructDeconstruct(v) => v.range(),
        }
    }
}

impl PrintTrait for DefVar {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>) {
        match self {
            DefVar::Identifier(v) => v.print(tabs, end, line),
            DefVar::TupleDeconstruct(v) => v.print(tabs, end, line),
            DefVar::StructDeconstruct(v) => v.print(tabs, end, line),
        }
    }
}

impl Node for DefNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.var.range(), SemanticTokenType::VARIABLE, 0);
        let mut pltype = None;
        if self.variable_type.is_none() {
            let mut tp = Arc::new(RefCell::new(PLType::Unknown));
            if let DefVar::Identifier(i) = &*self.var {
                if let Some(id) = i.id {
                    let v = ctx.unify_table.borrow_mut().probe(id);
                    tp = v.get_type(ctx, builder, &mut ctx.unify_table.clone().borrow_mut());
                }
            }

            pltype = Some(tp);
        }
        let mut expv = None;
        if let Some(tp) = &self.variable_type {
            tp.emit_highlight(ctx);
            let pltp = tp.get_type(ctx, builder, true)?;
            pltype = Some(pltp);
        }
        if self.value_expression.is_some()
            && matches!(pltype.clone(), Some(tp) if matches!(&*tp.borrow(), PLType::Unknown))
        {
            pltype = None;
        }
        if let Some(exp) = &mut self.value_expression {
            let re = if let Some(expect) = pltype.clone() {
                ctx.emit_with_expectation(exp, expect, self.var.range(), builder)
            } else {
                exp.emit(ctx, builder)
            };

            // for err tolerate
            if let Ok(v) = re {
                if let Some(re) = v.get_value() {
                    let mut tp = re.get_ty();
                    let v = if let PLType::Fn(f) = &*tp.clone().borrow() {
                        let oritp = tp;
                        let c =
                            Arc::new(RefCell::new(PLType::Closure(f.to_closure_ty(ctx, builder))));
                        tp = c.clone();
                        ctx.up_cast(
                            c,
                            oritp,
                            Default::default(),
                            Default::default(),
                            re.get_value(),
                            builder,
                        )
                        .unwrap()
                    } else {
                        re.get_value()
                    };
                    if self.variable_type.is_none() && pltype.is_none() {
                        pltype = Some(tp);
                    }
                    expv = Some(v);
                }
            }
        }
        let pltype = pltype.unwrap_or(unknown_arc());
        if self.variable_type.is_none() {
            ctx.push_type_hints(self.var.range(), pltype.clone());
        }
        let mut gm = IndexMap::new();
        if let PLType::Trait(t) = &*pltype.borrow() {
            gm = t.generic_map.clone();
        }
        if !pltype.borrow().is_complete() {
            ctx.add_diag(
                self.var
                    .range()
                    .new_err(ErrorCode::TYPE_CANNOT_BE_FULLY_INFERRED)
                    .add_to_ctx(ctx),
            );
        }
        ctx.protect_generic_context(&gm, |ctx| {
            handle_deconstruct(
                self.range(),
                self.value_expression.as_ref().map(|e| e.range()),
                builder,
                pltype.clone(),
                ctx,
                expv,
                &self.var,
                true,
                0,
            )
        })?;
        Ok(Default::default())
    }
}

#[allow(clippy::too_many_arguments)]
fn handle_deconstruct<'a, 'b>(
    range: Range,
    exp_range: Option<Range>,
    builder: &'b BuilderEnum<'a, '_>,
    pltype: Arc<RefCell<PLType>>,
    ctx: &'b mut Ctx<'a>,
    expv: Option<usize>,
    def_var: &DefVar,
    is_def: bool,
    semantic_idx: usize,
) -> Result<(), PLDiag> {
    match def_var {
        DefVar::Identifier(var) => {
            let ptr2value = if is_def {
                let ptr2value = builder.alloc(
                    &var.name,
                    &pltype.borrow(),
                    ctx,
                    Some(def_var.range().start),
                );
                ctx.add_symbol(
                    var.name.clone(),
                    ptr2value,
                    pltype.clone(),
                    def_var.range(),
                    false,
                    false,
                )?;
                ptr2value
            } else if let Some(s) = ctx.get_symbol(&var.name, builder) {
                if s.is_captured() {
                    ctx.plmod
                        .semantic_tokens_builder
                        .borrow_mut()
                        .set_modifier(semantic_idx, modifier_set!(CAPTURED));
                }
                s.get_data_ref().value
            } else {
                return Err(var
                    .range()
                    .new_err(ErrorCode::VAR_NOT_FOUND)
                    .add_to_ctx(ctx));
            };
            if let Some(exp) = expv {
                builder.build_dbg_location(def_var.range().start);
                let ptr2value = if ctx.generator_data.is_some() {
                    builder.build_load(ptr2value, "load_generator_var", &PLType::new_i8_ptr(), ctx)
                } else {
                    ptr2value
                };
                builder.build_store(
                    ptr2value,
                    ctx.try_load2var(range, exp, builder, &pltype.borrow())?,
                );
            }
        }
        DefVar::TupleDeconstruct(TupleDeconstructNode {
            var,
            range: dec_range,
        }) => {
            if expv.is_none() {
                return Err(ctx.add_diag(
                    range
                        .new_err(ErrorCode::DEF_DECONSTRUCT_MUST_HAVE_VALUE)
                        .add_to_ctx(ctx),
                ));
            }
            let expv = expv.unwrap();
            if let PLType::Struct(st) = &*pltype.borrow() {
                if st.is_tuple {
                    if var.len() != st.fields.len() {
                        return Err(ctx.add_diag(
                            range
                                .new_err(ErrorCode::TUPLE_WRONG_DECONSTRUCT_PARAM_LEN)
                                .add_label(
                                    *dec_range,
                                    ctx.get_file(),
                                    format_label!(
                                        "expected length {}",
                                        st.fields.len().to_string()
                                    ),
                                )
                                .add_label(
                                    def_var.range(),
                                    ctx.get_file(),
                                    format_label!("actual length {}", var.len().to_string()),
                                )
                                .add_to_ctx(ctx),
                        ));
                    }
                    for (i, (_, f)) in st.fields.iter().enumerate() {
                        let ftp = f.typenode.get_type(ctx, builder, false)?;
                        let expv = builder
                            .build_struct_gep(expv, f.index, "_deconstruct", &pltype.borrow(), ctx)
                            .unwrap();
                        let deconstruct_v = var[i].as_ref();
                        handle_deconstruct(
                            range,
                            exp_range,
                            builder,
                            ftp,
                            ctx,
                            Some(expv),
                            deconstruct_v,
                            is_def,
                            semantic_idx,
                        )?
                    }
                    return Ok(());
                }
            }
            return Err(ctx.add_diag(
                range
                    .new_err(ErrorCode::TYPE_MISMATCH)
                    .add_label(
                        *dec_range,
                        ctx.get_file(),
                        format_label!("expected type {}", "tuple"),
                    )
                    .add_label(
                        exp_range.unwrap(),
                        ctx.get_file(),
                        format_label!("real type {}", pltype.borrow().get_name()),
                    )
                    .add_to_ctx(ctx),
            ));
        }
        DefVar::StructDeconstruct(StructDeconstructNode {
            var,
            range: dec_range,
        }) => {
            if expv.is_none() {
                return Err(ctx.add_diag(
                    range
                        .new_err(ErrorCode::DEF_DECONSTRUCT_MUST_HAVE_VALUE)
                        .add_to_ctx(ctx),
                ));
            }
            let expv = expv.unwrap();
            if let PLType::Struct(st) = &*pltype.borrow() {
                if !st.is_tuple {
                    for deconstruct_field in var.iter() {
                        let (expv, ftp) = match deconstruct_field {
                            StructFieldDeconstructEnum::Var(v)
                            | StructFieldDeconstructEnum::Taged(v, _) => {
                                // check if field exists
                                if st.fields.get(&v.name).is_none() {
                                    return Err(dec_range
                                        .new_err(ErrorCode::STRUCT_FIELD_NOT_EXISTS)
                                        .add_label(
                                            v.range(),
                                            ctx.get_file(),
                                            format_label!("expect field {}", &v.name),
                                        )
                                        .add_to_ctx(ctx));
                                }
                                let f = st.fields.get(&v.name).unwrap();
                                let expv = builder
                                    .build_struct_gep(
                                        expv,
                                        f.index,
                                        "_deconstruct",
                                        &pltype.borrow(),
                                        ctx,
                                    )
                                    .unwrap();
                                let ftp = f.typenode.get_type(ctx, builder, false)?;
                                (expv, ftp)
                            }
                        };
                        match deconstruct_field {
                            StructFieldDeconstructEnum::Var(v) => handle_deconstruct(
                                range,
                                exp_range,
                                builder,
                                ftp,
                                ctx,
                                Some(expv),
                                &DefVar::Identifier(v.clone()),
                                is_def,
                                semantic_idx,
                            )?,
                            StructFieldDeconstructEnum::Taged(_, dec) => handle_deconstruct(
                                range,
                                exp_range,
                                builder,
                                ftp,
                                ctx,
                                Some(expv),
                                dec,
                                is_def,
                                semantic_idx,
                            )?,
                        }
                    }
                    return Ok(());
                }
            }
            return Err(ctx.add_diag(
                range
                    .new_err(ErrorCode::TYPE_MISMATCH)
                    .add_label(
                        *dec_range,
                        ctx.get_file(),
                        format_label!("expected type {}", "struct"),
                    )
                    .add_label(
                        exp_range.unwrap(),
                        ctx.get_file(),
                        format_label!("real type {}", pltype.borrow().get_name()),
                    )
                    .add_to_ctx(ctx),
            ));
        }
    };
    Ok(())
}
#[node]
pub struct AssignNode {
    /// var is the variable to be assigned at the left
    pub var: AssignVar,
    /// exp is the value to assign the var
    pub exp: Box<NodeEnum>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignVar {
    Pointer(Box<NodeEnum>),
    Raw(Box<DefVar>),
}

impl PrintTrait for AssignVar {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>) {
        match self {
            AssignVar::Pointer(p) => {
                p.print(tabs, end, line);
            }
            AssignVar::Raw(r) => {
                r.print(tabs, end, line);
            }
        }
    }
}

impl RangeTrait for AssignVar {
    fn range(&self) -> Range {
        match self {
            AssignVar::Pointer(p) => p.range(),
            AssignVar::Raw(r) => r.range(),
        }
    }
}
impl PrintTrait for AssignNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("AssignNode");
        self.var.print(tabs + 1, false, line.clone());
        self.exp.print(tabs + 1, true, line.clone());
    }
}

impl Node for AssignNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let exp_range = self.exp.range();
        match &mut self.var {
            AssignVar::Pointer(var) => {
                let rel = var.emit(ctx, builder)?.get_value();
                if rel.is_none() {
                    return Err(ctx.add_diag(self.var.range().new_err(ErrorCode::NOT_ASSIGNABLE)));
                }
                let rel = rel.unwrap();
                let ptr = rel.get_value();
                let lpltype = rel.get_ty();
                // 要走转换逻辑，所以不和下方分支统一
                let value = ctx
                    .emit_with_expectation(
                        &mut self.exp,
                        lpltype.clone(),
                        self.var.range(),
                        builder,
                    )?
                    .get_value()
                    .unwrap()
                    .get_value();
                if rel.is_const() {
                    return Err(ctx.add_diag(self.range.new_err(ErrorCode::ASSIGN_CONST)));
                }
                let load = ctx.try_load2var(exp_range, value, builder, &lpltype.borrow())?;
                builder.build_store(ptr, load);
                Ok(Default::default())
            }
            AssignVar::Raw(def) => {
                let idx = ctx.plmod.semantic_tokens_builder.borrow().get_data_len();
                ctx.push_semantic_token(def.range(), SemanticTokenType::VARIABLE, 0);
                let v = self.exp.emit(ctx, builder)?.get_value().unwrap();
                let expv = v.get_value();
                let pltype = v.get_ty();
                handle_deconstruct(
                    self.range,
                    Some(self.exp.range()),
                    builder,
                    pltype,
                    ctx,
                    Some(expv),
                    def,
                    false,
                    idx,
                )?;
                Ok(Default::default())
            }
        }
    }
}

#[node(comment)]
pub struct EmptyNode {}

impl PrintTrait for EmptyNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("EmptyNode");
    }
}

impl Node for EmptyNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        ctx.emit_comment_highlight(&self.comments[0]);
        Ok(Default::default())
    }
}

#[node]
/// StatementsNode stands for several statements
pub struct StatementsNode {
    /// statements is a vector of all statements under a statement block
    /// the order is same of the code, from top to bottom
    pub statements: Vec<Box<NodeEnum>>,
}

impl PrintTrait for StatementsNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StatementsNode");
        let mut i = self.statements.len();
        for statement in &self.statements {
            i -= 1;
            statement.print(tabs + 1, i == 0, line.clone());
        }
    }
}

impl Node for StatementsNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let mut terminator = TerminatorEnum::None;
        for m in self.statements.iter_mut() {
            if let NodeEnum::Empty(_) = **m {
                continue;
            }
            builder.set_di_file(&ctx.get_file());
            if !terminator.is_none() {
                if let NodeEnum::Comment(c) = &**m {
                    ctx.push_semantic_token(c.range, SemanticTokenType::COMMENT, 0);
                    continue;
                }
                ctx.add_diag(
                    m.range()
                        .new_warn(WarnCode::UNREACHABLE_STATEMENT)
                        .add_help(
                            "This statement will never be executed, because the previous \
                            statements contains a terminator. Try to remove it.",
                        )
                        .clone(),
                );
                continue;
            }
            let pos = m.range().start;
            builder.build_dbg_location(pos);
            let re = m.emit(ctx, builder);
            if re.is_err() {
                continue;
            }
            terminator = re.unwrap().get_term();
        }
        if ctx.need_highlight.borrow().eq(&0) {
            for (v, symbol) in &ctx.table {
                if let Some(refs) = &symbol.refs {
                    if refs.borrow().len() <= 1 && v != "self" && !v.starts_with('_') {
                        symbol
                            .range
                            .new_warn(WarnCode::UNUSED_VARIABLE)
                            .set_source(&ctx.get_file())
                            .add_label(
                                symbol.range,
                                ctx.get_file(),
                                format_label!("Unused variable `{}`", v),
                            )
                            .add_to_ctx(ctx);
                    }
                }
            }
        }
        NodeOutput::new_term(terminator).to_result()
    }
}

impl StatementsNode {
    /// # emit_child
    ///
    /// it creates a new context by current ctx to emit the current statement
    pub fn emit_child<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let child = &mut ctx.new_child(self.range.start, builder);
        self.emit(child, builder)
    }
}
