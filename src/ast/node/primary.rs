use std::sync::Arc;

use super::node_result::NodeResultBuilder;
use super::*;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::builder::ValueHandle;
use crate::ast::ctx::Ctx;
use crate::ast::ctx::MacroReplaceNode;
use crate::ast::ctx::BUILTIN_FN_NAME_MAP;
use crate::ast::diag::ErrorCode;
use crate::ast::pltype::{PLType, PriType};
use crate::inference::TyVariable;
use crate::modifier_set;
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

        match self.value {
            Num::Int(x) => {
                let b = builder.int_value(&PriType::I64, x, true);
                return b
                    .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::I64))))
                    .set_const()
                    .to_result();
            }
            Num::Float(x) => {
                let b = builder.float_value(&PriType::F64, x);
                return b
                    .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::F64))))
                    .set_const()
                    .to_result();
            }
            Num::Char(x) => {
                let b = builder.int_value(&PriType::CHAR, x as u64, false);
                return b
                    .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::CHAR))))
                    .set_const()
                    .to_result();
            }
        }
    }
}

#[node]
pub struct VarNode {
    /// identifier name of a symbol, which could be either a variable or a type
    pub name: String,
    pub id: Option<TyVariable>,
}

impl Node for VarNode {
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        if self.is_macro_var() {
            // verify whether the macro is a valid macro
            let re = ctx
                // the parser ensures the name is more than 1 charactor
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
                            self.name,
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

        ctx.generate_completion_if(ctx.should_gen(self.range), || ctx.completion_alternatives());

        // symbol which is generated else where already
        if let Some(symbol) = ctx.get_symbol(&self.name, builder) {
            ctx.push_semantic_token(
                self.range,
                SemanticTokenType::VARIABLE,
                if symbol.is_captured() {
                    modifier_set!(CAPTURED)
                } else {
                    0
                },
            );
            ctx.save_if_var_hover(self.range, &symbol.get_data_ref().pltype.borrow());

            let symbol_data = symbol.get_data();
            let symbol_value = match ctx.generator_data {
                None => symbol_data.value,
                _ => builder.build_load(
                    symbol_data.value,
                    "load_generator_var",
                    &PLType::new_i8_ptr(),
                    ctx,
                ),
            };

            ctx.send_if_go_to_def(self.range, symbol_data.range, ctx.plmod.path.clone());
            symbol_data
                .refs
                .map(|refs| {
                    ctx.set_local_refs(refs, self.range);
                })
                .or_else(|| {
                    ctx.set_glob_refs(&ctx.plmod.get_full_name(&self.name), self.range);
                    Some(())
                });

            let o = symbol_value.new_output(symbol_data.pltype).to_result();
            return o;
        }

        // buildtin symbol
        if let Some(builtin) = BUILTIN_FN_NAME_MAP.get(&self.name as &str) {
            ctx.push_semantic_token(self.range, SemanticTokenType::FUNCTION, 0);
            return builtin
                .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                .to_result();
        }

        // var node might refer to a function as well
        if let Ok(tp) = ctx.get_type(&self.name, self.range) {
            match &*tp.borrow() {
                PLType::Fn(f) => {
                    ctx.send_if_go_to_def(self.range, f.range, f.path.clone());
                    ctx.push_semantic_token(self.range, SemanticTokenType::FUNCTION, 0);
                    if !f.fntype.generic {
                        let handle = builder.get_or_insert_fn_handle(f, ctx).0;
                        return handle.new_output(tp.typ.clone()).to_result();
                    }
                    return usize::MAX.new_output(tp.typ.clone()).to_result();
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
        ctx.generate_completion_if(ctx.should_gen(self.range), || ctx.completion_alternatives());

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
                    } else if let PLType::Union(u) = &*tp.clone().borrow() {
                        ctx.send_if_go_to_def(self.range, u.range, u.path.clone());
                    }
                    return usize::MAX.new_output(tp.typ.clone()).to_result();
                }
                _ => return Err(ctx.add_diag(self.range.new_err(ErrorCode::UNDEFINED_TYPE))),
            }
        }
        Err(self
            .range
            .new_err(ErrorCode::UNDEFINED_TYPE)
            .set_source(&ctx.get_file())
            .add_to_ctx(ctx))
    }
}

/// One element object inside a whole array
#[node(comment)]
pub struct ArrayElementNode {
    /// the whole array of the element
    pub arr: Box<NodeEnum>,
    /// the index of current element inside the whole array
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
        let array_val = self.arr.emit(ctx, builder)?.get_value().unwrap();

        let pltype = array_val.get_ty();
        if let PLType::Arr(arrtp) = &*pltype.borrow() {
            let arr: ValueHandle = array_val.get_value();
            // TODO: check if index is out of bounds
            let v: NodeValue = self.index.emit(ctx, builder)?.get_value().unwrap();
            let index_val: ValueHandle = ctx.try_load2var(
                self.index.range(),
                v.get_value(),
                builder,
                &v.get_ty().borrow(),
            )?;

            // the index of an array must be an i64 type
            // REVIEW: this is a potential problem, what if user uses a I32 or I8 type?
            if !v.get_ty().borrow().is(&PriType::I64) {
                return Err(ctx.add_diag(self.range.new_err(ErrorCode::ARRAY_INDEX_MUST_BE_INT)));
            }

            let elemptr: ValueHandle = {
                let index: &[ValueHandle; 1] = &[index_val];
                let real_arr: ValueHandle = builder
                    .build_struct_gep(arr, 1, "real_arr", &pltype.borrow(), ctx)
                    .unwrap();
                let real_arr = builder.build_load(real_arr, "load_arr", &PLType::new_i8_ptr(), ctx);
                builder.build_in_bounds_gep(
                    real_arr,
                    index,
                    "element_ptr",
                    &arrtp.element_type.borrow(),
                    ctx,
                )
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
