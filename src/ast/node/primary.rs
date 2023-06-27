use std::sync::Arc;

use super::node_result::NodeResultBuilder;
use super::*;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::ctx::Ctx;
use crate::ast::ctx::MacroReplaceNode;
use crate::ast::ctx::BUILTIN_FN_NAME_MAP;
use crate::ast::diag::ErrorCode;
use crate::ast::pltype::{PLType, PriType};
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.range, SemanticTokenType::KEYWORD, 0);
        builder
            .int_value(&PriType::BOOL, self.value as u64, true)
            .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
            .to_result()
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        ctx.push_semantic_token(self.range, SemanticTokenType::NUMBER, 0);
        if let Num::Int(x) = self.value {
            let b = builder.int_value(&PriType::I64, x, true);
            return b
                .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::I64))))
                .set_const()
                .to_result();
        } else if let Num::Float(x) = self.value {
            let b = builder.float_value(&PriType::F64, x);
            return b
                .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::F64))))
                .set_const()
                .to_result();
        }
        panic!("not implemented")
    }
}

#[node]
pub struct VarNode {
    pub name: String,
}

impl Node for VarNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        if self.is_macro_var() {
            let re = ctx
                .find_macro_symbol(&self.name[1..])
                .cloned()
                .or_else(|| {
                    if ctx.macro_loop {
                        Some(MacroReplaceNode::LoopNodeEnum(vec![]))
                    } else {
                        None
                    }
                })
                .ok_or_else(|| {
                    self.range
                        .new_err(ErrorCode::MACRO_VAR_NOT_FOUND)
                        .add_help(&format!(
                            "add a macro var named `{}` in the macro definition",
                            self.name
                        ))
                        .add_to_ctx(ctx)
                })?;
            match re {
                MacroReplaceNode::NodeEnum(mut n) => {
                    ctx.macro_skip_level += 1;
                    let re = n.emit(ctx, builder);
                    ctx.macro_skip_level -= 1;
                    return re;
                }
                MacroReplaceNode::LoopNodeEnum(mut loop_var) => {
                    if !ctx.macro_loop {
                        return Err(self
                            .range
                            .new_err(ErrorCode::MACRO_LOOP_VAR_USED_OUT_OF_LOOP)
                            .add_help("add a `macro loop` surrounding the macro body like $($var)*")
                            .add_to_ctx(ctx));
                    }
                    ctx.macro_skip_level += 1;
                    let re = loop_var[ctx.macro_loop_idx].emit(ctx, builder);
                    ctx.macro_skip_level -= 1;
                    ctx.macro_loop_len = loop_var.len();
                    return re;
                }
            }
        }
        ctx.if_completion(self.range, || ctx.get_completions());
        if let Some((symbol, is_const)) = ctx.get_symbol(&self.name, builder) {
            ctx.push_semantic_token(self.range, SemanticTokenType::VARIABLE, 0);
            let o = symbol
                .value
                .new_output(symbol.pltype)
                .set_const_v(is_const)
                .to_result();
            ctx.send_if_go_to_def(self.range, symbol.range, ctx.plmod.path.clone());
            symbol
                .refs
                .map(|refs| {
                    ctx.set_local_refs(refs, self.range);
                })
                .or_else(|| {
                    ctx.set_glob_refs(&ctx.plmod.get_full_name(&self.name), self.range);
                    Some(())
                });
            return o;
        }
        if let Some(builtin) = BUILTIN_FN_NAME_MAP.get(&self.name as &str) {
            ctx.push_semantic_token(self.range, SemanticTokenType::FUNCTION, 0);
            return builtin
                .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                .to_result();
        }
        if let Ok(tp) = ctx.get_type(&self.name, self.range) {
            match &*tp.borrow() {
                PLType::Fn(f) => {
                    ctx.send_if_go_to_def(self.range, f.range, f.path.clone());
                    ctx.push_semantic_token(self.range, SemanticTokenType::FUNCTION, 0);
                    if !f.fntype.generic {
                        let handle = builder.get_or_insert_fn_handle(f, ctx);
                        return handle.new_output(tp.tp.clone()).to_result();
                    }
                    return usize::MAX.new_output(tp.tp.clone()).to_result();
                }
                _ => return Err(ctx.add_diag(self.range.new_err(ErrorCode::VAR_NOT_FOUND))),
            }
        }
        Err(ctx.add_diag(self.range.new_err(ErrorCode::VAR_NOT_FOUND)))
    }
}

impl PrintTrait for VarNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("VarNode: {}", self.name);
    }
}

impl VarNode {
    fn is_macro_var(&self) -> bool {
        self.name.starts_with('$')
    }
    pub fn get_name(&self, ctx: &Ctx) -> String {
        if self.is_macro_var() {
            let re = ctx.macro_vars.get(&self.name[1..]).unwrap();
            if let MacroReplaceNode::NodeEnum(NodeEnum::Var(v)) = re {
                v.name.clone()
            } else {
                todo!()
            }
        } else {
            self.name.clone()
        }
    }

    pub fn get_type(&self, ctx: &Ctx) -> NodeResult {
        if self.is_macro_var() {
            let re = ctx.macro_vars.get(&self.name[1..]).unwrap();
            if let MacroReplaceNode::NodeEnum(NodeEnum::Var(v)) = re {
                return v.get_type(ctx);
            } else {
                todo!()
            }
        }
        ctx.if_completion(self.range, || ctx.get_completions());

        if let Ok(tp) = ctx.get_type(&self.name, self.range) {
            match *tp.borrow() {
                PLType::Struct(_)
                | PLType::Trait(_)
                | PLType::Primitive(_)
                | PLType::Void
                | PLType::Generic(_)
                | PLType::PlaceHolder(_)
                | PLType::Union(_) => {
                    if let PLType::Struct(st) | PLType::Trait(st) = &*tp.clone().borrow() {
                        ctx.send_if_go_to_def(self.range, st.range, st.path.clone());
                        // ctx.set_if_refs(st.refs.clone(), self.range);
                    } else if let PLType::Union(u) = &*tp.clone().borrow() {
                        ctx.send_if_go_to_def(self.range, u.range, u.path.clone());
                    }
                    return usize::MAX.new_output(tp.tp.clone()).to_result();
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let v = self.arr.emit(ctx, builder)?.get_value().unwrap();
        let pltype = v.get_ty();
        if let PLType::Arr(arrtp) = &*pltype.borrow() {
            let arr = v.get_value();
            // TODO: check if index is out of bounds
            let index_range = self.index.range();
            let v = self.index.emit(ctx, builder)?.get_value().unwrap();
            let index = v.get_value();
            let index = ctx.try_load2var(index_range, index, builder)?;
            if !v.get_ty().borrow().is(&PriType::I64) {
                return Err(ctx.add_diag(self.range.new_err(ErrorCode::ARRAY_INDEX_MUST_BE_INT)));
            }
            let elemptr = {
                let index = &[builder.int_value(&PriType::I64, 0, false), index];
                let real_arr = builder.build_struct_gep(arr, 1, "real_arr").unwrap();
                builder.build_in_bounds_gep(real_arr, index, "element_ptr")
            };
            ctx.emit_comment_highlight(&self.comments[0]);
            return elemptr.new_output(arrtp.element_type.clone()).to_result();
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        self.node.emit(ctx, builder)
    }
}
