use std::sync::Arc;

use super::*;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::ctx::Ctx;
use crate::ast::ctx::MacroReplaceNode;
use crate::ast::diag::ErrorCode;
use crate::ast::pltype::{PLType, PriType};
use crate::plv;
use internal_macro::node;
use lsp_types::SemanticTokenType;

#[node(comment)]
pub struct PrimaryNode {
    pub value: Box<NodeEnum>,
}

impl PrintTrait for PrimaryNode {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>) {
        self.value.print(tabs, end, line);
    }
}

impl Node for PrimaryNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        ctx.emit_comment_highlight(&self.comments[0]);
        let res = self.value.emit(ctx, builder);
        ctx.emit_comment_highlight(&self.comments[1]);
        res
    }
}
// ANCHOR: bool
#[node]
pub struct BoolConstNode {
    pub value: bool,
}
// ANCHOR_END: bool

impl PrintTrait for BoolConstNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("BoolConstNode: {}", self.value);
    }
}

impl Node for BoolConstNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.range, SemanticTokenType::KEYWORD, 0);
        Ok((
            Some(plv!(builder.int_value(
                &PriType::BOOL,
                self.value as u64,
                true
            ))),
            Some(Arc::new(RefCell::new(PLType::PRIMITIVE(PriType::BOOL)))),
            TerminatorEnum::NONE,
        ))
    }
}

#[node(copy)]
pub struct NumNode {
    pub value: Num,
}
impl PrintTrait for NumNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("NumNode: {:?}", self.value);
    }
}
impl Node for NumNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.range, SemanticTokenType::NUMBER, 0);
        if let Num::INT(x) = self.value {
            let b = builder.int_value(&PriType::I64, x, true);
            return Ok((
                Some(plv!(b)),
                Some(Arc::new(RefCell::new(PLType::PRIMITIVE(PriType::I64)))),
                TerminatorEnum::NONE,
            ));
        } else if let Num::FLOAT(x) = self.value {
            let b = builder.float_value(&PriType::F64, x);
            return Ok((
                Some(plv!(b)),
                Some(Arc::new(RefCell::new(PLType::PRIMITIVE(PriType::F64)))),
                TerminatorEnum::NONE,
            ));
        }
        panic!("not implemented")
    }
}

#[node]
pub struct VarNode {
    pub name: String,
}
impl VarNode {
    pub fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("VarNode: {}", self.name);
    }
    fn is_macro_var(&self) -> bool {
        self.name.starts_with("$")
    }
    pub fn get_name(&self, ctx: &Ctx) -> String {
        if self.is_macro_var() {
            let re = ctx.macro_vars.get(&self.name[1..]).unwrap();
            if let MacroReplaceNode::VarNode(v) = re {
                return v.name.clone();
            } else {
                todo!()
            }
        } else {
            self.name.clone()
        }
    }

    pub fn emit<'a, 'ctx, 'b>(
        &self,
        ctx: &'b Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        if self.is_macro_var() {
            let re = ctx.macro_vars.get(&self.name[1..]).unwrap();
            if let MacroReplaceNode::VarNode(v) = re {
                return v.emit(ctx, builder);
            } else {
                todo!()
            }
        }
        ctx.if_completion(self.range, || ctx.get_completions());
        let v = ctx.get_symbol(&self.name, builder);
        if let Some((v, pltype, dst, refs, is_const)) = v {
            ctx.push_semantic_token(self.range, SemanticTokenType::VARIABLE, 0);
            let o = Ok((
                Some({
                    let mut res: PLValue = plv!(v);
                    res.set_const(is_const);
                    res
                }),
                Some(pltype),
                TerminatorEnum::NONE,
            ));
            ctx.send_if_go_to_def(self.range, dst, ctx.plmod.path.clone());
            refs.map(|refs| {
                ctx.set_if_refs(refs, self.range);
            })
            .or_else(|| {
                ctx.set_glob_refs(&ctx.plmod.get_full_name(&self.name), self.range);
                Some(())
            });
            return o;
        }
        if let Ok(tp) = ctx.get_type(&self.name, self.range) {
            match &*tp.borrow() {
                PLType::FN(f) => {
                    ctx.send_if_go_to_def(self.range, f.range, ctx.plmod.path.clone());
                    ctx.push_semantic_token(self.range, SemanticTokenType::FUNCTION, 0);
                    return Ok((None, Some(tp.clone()), TerminatorEnum::NONE));
                }
                _ => return Err(ctx.add_diag(self.range.new_err(ErrorCode::VAR_NOT_FOUND))),
            }
        }
        Err(ctx.add_diag(self.range.new_err(ErrorCode::VAR_NOT_FOUND)))
    }
    pub fn get_type<'a, 'ctx>(&'a self, ctx: &Ctx<'a>) -> NodeResult {
        if self.is_macro_var() {
            let re = ctx.macro_vars.get(&self.name[1..]).unwrap();
            if let MacroReplaceNode::VarNode(v) = re {
                return v.get_type(ctx);
            } else {
                todo!()
            }
        }
        ctx.if_completion(self.range, || ctx.get_completions());

        if let Ok(tp) = ctx.get_type(&self.name, self.range) {
            match *tp.borrow() {
                PLType::STRUCT(_)
                | PLType::TRAIT(_)
                | PLType::PRIMITIVE(_)
                | PLType::VOID
                | PLType::GENERIC(_)
                | PLType::PLACEHOLDER(_) => {
                    if let PLType::STRUCT(st) | PLType::TRAIT(st) = &*tp.clone().borrow() {
                        ctx.send_if_go_to_def(self.range, st.range, ctx.plmod.path.clone());
                        // ctx.set_if_refs(st.refs.clone(), self.range);
                    }
                    return Ok((None, Some(tp.clone()), TerminatorEnum::NONE));
                }
                _ => return Err(ctx.add_diag(self.range.new_err(ErrorCode::UNDEFINED_TYPE))),
            }
        }
        Err(ctx.add_diag(self.range.new_err(ErrorCode::UNDEFINED_TYPE)))
    }
}

#[node(comment)]
pub struct ArrayElementNode {
    pub arr: Box<NodeEnum>,
    pub index: Box<NodeEnum>,
}

impl PrintTrait for ArrayElementNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ArrayElementNode");
        self.arr.print(tabs + 1, false, line.clone());
        self.index.print(tabs + 1, true, line);
    }
}

impl Node for ArrayElementNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let (arr, pltype, _) = self.arr.emit(ctx, builder)?;
        if let PLType::ARR(arrtp) = &*pltype.unwrap().borrow() {
            let arr = arr.unwrap();
            // TODO: check if index is out of bounds
            let index_range = self.index.range();
            let (index, index_pltype, _) = self.index.emit(ctx, builder)?;
            let (index, _) = ctx.try_load2var(
                index_range,
                index.unwrap(),
                index_pltype.clone().unwrap(),
                builder,
            )?;
            if index_pltype.is_none() || !index_pltype.unwrap().borrow().is(&PriType::I64) {
                return Err(ctx.add_diag(self.range.new_err(ErrorCode::ARRAY_INDEX_MUST_BE_INT)));
            }
            let elemptr = {
                let index = &[builder.int_value(&PriType::I64, 0, false), index];
                let real_arr = builder.build_struct_gep(arr.value, 1, "real_arr").unwrap();
                builder.build_in_bounds_gep(real_arr, index, "element_ptr")
            };
            ctx.emit_comment_highlight(&self.comments[0]);
            return Ok((
                Some(plv!(elemptr)),
                Some(arrtp.element_type.clone()),
                TerminatorEnum::NONE,
            ));
        }
        Err(ctx.add_diag(self.range.new_err(ErrorCode::CANNOT_INDEX_NON_ARRAY)))
    }
}

#[node]
pub struct ParanthesesNode {
    pub node: Box<NodeEnum>,
}

impl PrintTrait for ParanthesesNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ParanthesesNode");
        self.node.print(tabs + 1, true, line);
    }
}

impl Node for ParanthesesNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        self.node.emit(ctx, builder)
    }
}
