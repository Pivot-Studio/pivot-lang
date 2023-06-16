use super::node_result::TerminatorEnum;
use super::*;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::ctx::Ctx;
use crate::ast::diag::{ErrorCode, WarnCode};
use crate::format_label;

use internal_macro::node;
use internal_macro::range;
use lsp_types::SemanticTokenType;
#[node(comment)]
pub struct DefNode {
    pub var: Box<DefVar>,
    pub tp: Option<Box<TypeNodeEnum>>,
    pub exp: Option<Box<NodeEnum>>,
}

#[range]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TupleDeconstructNode {
    pub var: Vec<Box<DefVar>>,
}

impl PrintTrait for TupleDeconstructNode {
    fn print(&self, tabs: usize, _end: bool, line: Vec<bool>) {
        for (i, v) in self.var.iter().enumerate() {
            v.print(tabs, i == self.var.len() - 1, line.clone());
        }
    }
}

#[range]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDeconstructNode {
    pub var: Vec<StructFieldDeconstructEnum>,
}

impl PrintTrait for StructDeconstructNode {
    fn print(&self, tabs: usize, _end: bool, line: Vec<bool>) {
        for (i, v) in self.var.iter().enumerate() {
            v.print(tabs, i == self.var.len() - 1, line.clone());
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
        if let Some(tp) = &self.tp {
            tp.print(tabs + 1, true, line.clone());
        } else {
            self.exp
                .as_ref()
                .unwrap()
                .print(tabs + 1, true, line.clone());
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
        let range = self.range();
        ctx.push_semantic_token(self.var.range(), SemanticTokenType::VARIABLE, 0);
        if self.exp.is_none() && self.tp.is_none() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::UNDEFINED_TYPE)));
        }
        let mut pltype = None;
        let mut expv = None;
        if let Some(tp) = &self.tp {
            tp.emit_highlight(ctx);
            let pltp = tp.get_type(ctx, builder, true)?;
            pltype = Some(pltp);
        }
        if let Some(exp) = &mut self.exp {
            let re = if let Some(pltype) = pltype.clone() {
                ctx.emit_with_expectation(exp, pltype, self.var.range(), builder)?
                    .get_value()
            } else {
                exp.emit(ctx, builder)?.get_value()
            };

            // for err tolerate
            if re.is_none() {
                return Err(ctx.add_diag(self.range.new_err(ErrorCode::UNDEFINED_TYPE)));
            }
            let re = re.unwrap();
            let mut tp = re.get_ty();
            let v = if let PLType::Fn(f) = &*tp.clone().borrow() {
                let oritp = tp;
                let c = Arc::new(RefCell::new(PLType::Closure(f.to_closure_ty(ctx, builder))));
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
            if pltype.is_none() {
                ctx.push_type_hints(self.var.range(), tp.clone());
                pltype = Some(tp);
            }
            expv = Some(v);
        }
        let pltype = pltype.unwrap();
        match &*self.var {
            DefVar::Identifier(var) => {
                let ptr2value = builder.alloc(
                    &var.name,
                    &pltype.borrow(),
                    ctx,
                    Some(self.var.range().start),
                );
                ctx.add_symbol(var.name.clone(), ptr2value, pltype, self.var.range(), false)?;
                if let Some(exp) = expv {
                    builder.build_dbg_location(self.var.range().start);
                    builder.build_store(ptr2value, ctx.try_load2var(range, exp, builder)?);
                }
            }
            _ => todo!(),
        };
        Ok(Default::default())
    }
}
#[node]
pub struct AssignNode {
    pub var: Box<NodeEnum>,
    pub exp: Box<NodeEnum>,
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
        let rel = self.var.emit(ctx, builder)?.get_value();
        if rel.is_none() {
            return Err(ctx.add_diag(self.var.range().new_err(ErrorCode::NOT_ASSIGNABLE)));
        }
        let rel = rel.unwrap();
        let ptr = rel.get_value();
        let lpltype = rel.get_ty();
        let value = ctx
            .emit_with_expectation(&mut self.exp, lpltype, self.var.range(), builder)?
            .get_value()
            .unwrap()
            .get_value();
        if rel.is_const() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::ASSIGN_CONST)));
        }
        let load = ctx.try_load2var(exp_range, value, builder)?;
        builder.build_store(ptr, load);
        Ok(Default::default())
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
pub struct StatementsNode {
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
        NodeOutput::new_term(terminator).to_result()
    }
}

impl StatementsNode {
    pub fn emit_child<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let child = &mut ctx.new_child(self.range.start, builder);
        self.emit(child, builder)
    }
}
