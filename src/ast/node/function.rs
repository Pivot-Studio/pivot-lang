use super::interface::TraitBoundNode;
use super::node_result::NodeResultBuilder;
use super::statement::StatementsNode;
use super::*;
use super::{types::TypedIdentifierNode, Node, TypeNode};
use crate::ast::builder::ValueHandle;
use crate::ast::diag::ErrorCode;
use crate::ast::node::{deal_line, tab};

use crate::ast::pltype::{get_type_deep, ClosureType, FNValue, FnType, PLType};
use crate::ast::tokens::TokenType;
use indexmap::IndexMap;
use internal_macro::node;
use lsp_types::SemanticTokenType;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

use std::sync::atomic::{AtomicI32, Ordering};
use std::vec;
#[node(comment)]
pub struct FuncCallNode {
    pub generic_params: Option<Box<GenericParamNode>>,
    pub callee: Box<NodeEnum>,
    pub paralist: Vec<Box<NodeEnum>>,
}

impl PrintTrait for FuncCallNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("FuncCallNode");
        let mut i = self.paralist.len();
        self.callee.print(tabs + 1, false, line.clone());
        for para in &self.paralist {
            i -= 1;
            para.print(tabs + 1, i == 0, line.clone());
        }
    }
}

impl FuncCallNode {
    fn handle_closure_call<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
        c: &ClosureType,
        v: ValueHandle,
    ) -> NodeResult {
        let data = builder.build_struct_gep(v, 1, "closure_data").unwrap();
        let data = builder.build_load(data, "loaded_closure_data");
        let mut para_values = vec![data];
        let mut value_pltypes = vec![];
        if self.paralist.len() != c.arg_types.len() {
            return Err(self
                .range
                .new_err(ErrorCode::PARAMETER_LENGTH_NOT_MATCH)
                .add_to_ctx(ctx));
        }
        for (i, para) in self.paralist.iter_mut().enumerate() {
            let pararange = para.range();
            let v = ctx
                .emit_with_expectation(
                    para,
                    c.arg_types[i].clone(),
                    c.arg_types[i].borrow().get_range().unwrap_or_default(),
                    builder,
                )?
                .get_value();
            if v.is_none() {
                return Ok(Default::default());
            }
            let v = v.unwrap();
            let value_pltype = v.get_ty();
            let value_pltype = get_type_deep(value_pltype);
            let load = ctx.try_load2var(pararange, v.get_value(), builder)?;
            para_values.push(load);
            value_pltypes.push((value_pltype, pararange));
        }
        let re = builder.build_struct_gep(v, 0, "real_fn").unwrap();
        let re = builder.build_load(re, "real_fn");
        let ret = builder.build_call(re, &para_values, &c.ret_type.borrow(), ctx);
        handle_ret(ret, c.ret_type.clone())
    }

    fn build_params<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
        para_values: &mut Vec<usize>,
        value_pltypes: &mut Vec<(Arc<RefCell<PLType>>, Range)>,
    ) -> Result<(), PLDiag> {
        for para in self.paralist.iter_mut() {
            let pararange = para.range();
            let v = para.emit(ctx, builder)?.get_value();
            if v.is_none() {
                return Ok(());
            }
            let v = v.unwrap();
            let value_pltype = v.get_ty();
            let value_pltype = get_type_deep(value_pltype);
            let load = ctx.try_load2var(pararange, v.get_value(), builder)?;
            para_values.push(load);
            value_pltypes.push((value_pltype, pararange));
        }
        Ok(())
    }

    fn build_hint(&mut self, ctx: &mut Ctx, fnvalue: &FNValue, skip: u32) {
        for (i, para) in self.paralist.iter_mut().enumerate() {
            let pararange = para.range();
            ctx.push_param_hint(pararange, fnvalue.param_names[i + skip as usize].clone());
            ctx.set_if_sig(
                para.range(),
                fnvalue.name.clone().split("::").last().unwrap().to_string()
                    + "("
                    + fnvalue
                        .param_names
                        .iter()
                        .enumerate()
                        .map(|(i, s)| {
                            s.clone()
                                + ": "
                                + FmtBuilder::generate_node(&fnvalue.fntype.param_pltypes[i])
                                    .as_str()
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                        .as_str()
                    + ")",
                &fnvalue.param_names,
                i as u32 + skip,
            );
        }
    }
}

impl Node for FuncCallNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let id_range = self.callee.range();
        let v = self.callee.emit(ctx, builder)?.get_value();
        if v.is_none() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::FUNCTION_NOT_FOUND)));
        }
        let v = v.unwrap();

        let pltype = v.get_ty();
        let mut fnvalue = match &*pltype.borrow() {
            PLType::Fn(f) => {
                let mut res = f.clone();
                res.fntype = res.fntype.new_pltype();
                res
            }
            PLType::Closure(c) => {
                return self.handle_closure_call(ctx, builder, c, v.get_value());
            }
            _ => return Err(ctx.add_diag(self.range.new_err(ErrorCode::FUNCTION_NOT_FOUND))),
        };

        if let Some(generic_params) = &self.generic_params {
            let generic_params_range = generic_params.range;
            generic_params.emit_highlight(ctx);
            if generic_params.generics.len() != fnvalue.fntype.generics_size {
                return Err(ctx.add_diag(
                    generic_params_range.new_err(ErrorCode::GENERIC_PARAM_LEN_MISMATCH),
                ));
            }
            let generic_types = generic_params.get_generic_types(ctx, builder)?;
            for (i, (_, pltype)) in fnvalue.fntype.generic_map.iter().enumerate() {
                if i >= fnvalue.fntype.generics_size {
                    break;
                }
                if generic_types[i].is_some()
                    && !ctx
                        .eq(pltype.clone(), generic_types[i].as_ref().unwrap().clone())
                        .eq
                {
                    return Err(ctx.add_diag(
                        generic_params.generics[i]
                            .as_ref()
                            .unwrap()
                            .range()
                            .new_err(ErrorCode::TYPE_MISMATCH),
                    ));
                }
            }
        }
        let mut skip = 0;
        let mut para_values = vec![];
        let mut receiver_type = None;
        if let Some((receiver, tp)) = v.get_receiver() {
            (skip, para_values, receiver_type) = (1, vec![receiver], tp);
        }
        let fn_handle = v.get_value();
        if fnvalue.fntype.param_pltypes.len() - skip as usize != self.paralist.len() {
            return Err(self
                .range
                .new_err(ErrorCode::PARAMETER_LENGTH_NOT_MATCH)
                .add_to_ctx(ctx));
        }
        self.build_hint(ctx, &fnvalue, skip);
        let mut value_pltypes = vec![];
        self.build_params(ctx, builder, &mut para_values, &mut value_pltypes)?;
        // value check and generic infer
        let res = ctx.protect_generic_context(&fnvalue.fntype.generic_map.clone(), |ctx| {
            ctx.run_in_type_mod_mut(&mut fnvalue, |ctx, fnvalue| {
                if let Some(receiver_pltype) = &receiver_type {
                    if !fnvalue.fntype.param_pltypes[0]
                        .eq_or_infer(ctx, receiver_pltype.clone(), builder)?
                        .eq
                    {
                        return Err(
                            ctx.add_diag(self.range.new_err(ErrorCode::RECEIVER_CANNOT_BE_INFER))
                        );
                    }
                }
                check_and_cast_params(
                    &value_pltypes,
                    &fnvalue.fntype.param_pltypes,
                    skip,
                    ctx,
                    builder,
                    &mut para_values,
                )?;
                Ok(())
            })?;
            if !fnvalue.fntype.generic_map.is_empty() {
                if fnvalue.fntype.need_gen_code() {
                    fnvalue = ctx.run_in_type_mod_mut(&mut fnvalue, |ctx, fnvalue| {
                        fnvalue.generic_infer_pltype(ctx, builder)
                    })?;
                } else {
                    return Err(
                        ctx.add_diag(self.range.new_err(ErrorCode::GENERIC_CANNOT_BE_INFER))
                    );
                }
            }
            let function = if fn_handle != usize::MAX {
                fn_handle
            } else {
                builder.get_or_insert_fn_handle(&fnvalue, ctx)
            };
            builder.try_set_fn_dbg(self.range.start, ctx.function.unwrap());
            let rettp = ctx.run_in_type_mod_mut(&mut fnvalue, |ctx, fnvalue| {
                fnvalue.fntype.ret_pltype.get_type(ctx, builder)
            })?;
            let ret = builder.build_call(function, &para_values, &rettp.borrow(), ctx);
            ctx.save_if_comment_doc_hover(id_range, Some(fnvalue.doc.clone()));
            handle_ret(ret, rettp)
        });
        ctx.set_if_refs_tp(pltype, id_range);
        ctx.emit_comment_highlight(&self.comments[0]);
        res
    }
}

fn check_and_cast_params<'a, 'ctx, 'b>(
    value_pltypes: &[(Arc<RefCell<PLType>>, Range)],
    param_types: &[Box<TypeNodeEnum>],
    skip: u32,
    ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, 'ctx>,
    para_values: &mut [usize],
) -> Result<(), PLDiag> {
    for (i, (value_pltype, pararange)) in value_pltypes.iter().enumerate() {
        let eqres =
            param_types[i + skip as usize].eq_or_infer(ctx, value_pltype.clone(), builder)?;
        if !eqres.eq {
            return Err(ctx.add_diag(pararange.new_err(ErrorCode::PARAMETER_TYPE_NOT_MATCH)));
        }
        if eqres.need_up_cast {
            let mut value = para_values[i + skip as usize];
            let ptr2v = builder.alloc("tmp_up_cast_ptr", &value_pltype.borrow(), ctx, None);
            builder.build_store(ptr2v, value);
            let trait_pltype = param_types[i + skip as usize].get_type(ctx, builder)?;
            value = ctx.up_cast(
                trait_pltype,
                value_pltype.clone(),
                param_types[i + skip as usize].range(),
                *pararange,
                ptr2v,
                builder,
            )?;
            value = ctx.try_load2var(*pararange, value, builder)?;
            para_values[i + skip as usize] = value;
        }
    }
    Ok(())
}

fn handle_ret(ret: Option<usize>, rettp: Arc<RefCell<PLType>>) -> Result<NodeOutput, PLDiag> {
    match ret {
        Some(v) => v
            .new_output(match &*rettp.clone().borrow() {
                PLType::Generic(g) => g.curpltype.as_ref().unwrap().clone(),
                _ => rettp,
            })
            .to_result(),
        None => usize::MAX.new_output(rettp).to_result(),
    }
}
#[node]
pub struct FuncDefNode {
    pub id: Box<VarNode>,
    pub paralist: Vec<Box<TypedIdentifierNode>>,
    pub ret: Box<TypeNodeEnum>,
    pub doc: Vec<Box<NodeEnum>>,
    pub pre_comments: Vec<Box<NodeEnum>>,
    pub declare: bool,
    pub generics: Option<Box<GenericDefNode>>,
    pub body: Option<StatementsNode>,
    pub modifier: Option<(TokenType, Range)>,
    pub generics_size: usize, // the size of generics except the generics from impl node
    pub trait_bounds: Option<Vec<Box<TraitBoundNode>>>,
}

type OptFOnce<'a> = Option<Box<dyn FnOnce(&mut Ctx) + 'a>>; // Thank u, Rust!

impl TypeNode for FuncDefNode {
    fn get_type<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> TypeNodeResult {
        let child = &mut ctx.new_child(self.range.start, builder);
        let generic_map = if let Some(generics) = &self.generics {
            let mp = generics.gen_generic_type(ctx);
            generics.set_traits(child, builder, &mp)?;
            mp
        } else {
            IndexMap::default()
        };
        if let Some(trait_bounds) = &self.trait_bounds {
            for trait_bound in trait_bounds.iter() {
                trait_bound.set_traits(child, builder, &generic_map)?;
            }
        }
        let (pltype, flater) = child.protect_generic_context(&generic_map, |child| {
            let mut flater: OptFOnce = None;
            let mut param_pltypes = Vec::new();
            let mut param_name = Vec::new();
            let method = !self.paralist.is_empty() && self.paralist[0].id.name == "self";
            generic_map
                .iter()
                .for_each(|(_, pltype)| match &mut *pltype.borrow_mut() {
                    PLType::Generic(g) => {
                        g.set_place_holder(child);
                    }
                    _ => unreachable!(),
                });
            for para in self.paralist.iter() {
                _ = para.typenode.get_type(child, builder)?;
                param_pltypes.push(para.typenode.clone());
                param_name.push(para.id.name.clone());
            }
            let fnvalue = FNValue {
                name: self.id.name.clone(),
                param_names: param_name,
                range: self.id.range(),
                doc: self.doc.clone(),
                llvmname: if self.declare {
                    self.id.name.clone()
                } else {
                    child.plmod.get_full_name(&self.id.name)
                },
                path: child.plmod.path.clone(),
                fntype: FnType {
                    ret_pltype: self.ret.clone(),
                    param_pltypes,
                    method,
                    generic_map: generic_map.clone(),
                    generic: self.generics.is_some(),
                    modifier: self.modifier,
                    generics_size: self.generics_size,
                },
                generic_infer: Arc::new(RefCell::new(IndexMap::default())),
                node: Some(Box::new(self.clone())),
                body_range: self.range,
            };
            if self.generics.is_none() {
                builder.get_or_insert_fn_handle(&fnvalue, child);
            }
            let pltype = Arc::new(RefCell::new(PLType::Fn(fnvalue.clone())));
            child.set_if_refs_tp(pltype.clone(), self.id.range);
            child.add_doc_symbols(pltype.clone());
            if method {
                let receiver_pltype = self
                    .paralist
                    .first()
                    .unwrap()
                    .typenode
                    .get_type(child, builder)
                    .unwrap();
                if let PLType::Pointer(s) = &*receiver_pltype.borrow() {
                    match &*s.borrow() {
                        PLType::Struct(s) => {
                            let fullname = s.get_st_full_name_except_generic();
                            flater = Some(Box::new(move |ctx: &mut Ctx| {
                                ctx.add_method(
                                    &fullname,
                                    self.id.name.split("::").last().unwrap(),
                                    fnvalue.clone(),
                                    self.id.range,
                                );
                            }));
                        }
                        PLType::Union(u) => {
                            let fullname = u.get_full_name_except_generic();
                            flater = Some(Box::new(move |ctx: &mut Ctx| {
                                ctx.add_method(
                                    &fullname,
                                    self.id.name.split("::").last().unwrap(),
                                    fnvalue.clone(),
                                    self.id.range,
                                );
                            }));
                        }
                        _ => (), // 在impl的emit里会检查类型报错，这里不处理
                    }
                };
            }
            Ok((pltype, flater))
        })?;
        if let Some(flater) = flater {
            flater(ctx);
        }
        Ok(pltype)
    }

    fn emit_highlight(&self, ctx: &mut Ctx) {
        ctx.emit_comment_highlight(&self.pre_comments);
        ctx.push_semantic_token(self.id.range, SemanticTokenType::FUNCTION, 0);
        if let Some(generics) = &self.generics {
            generics.emit_highlight(ctx);
        }
        for para in self.paralist.iter() {
            ctx.push_semantic_token(para.id.range, SemanticTokenType::PARAMETER, 0);
            ctx.push_semantic_token(para.typenode.range(), SemanticTokenType::TYPE, 0);
        }
        ctx.push_semantic_token(self.ret.range(), SemanticTokenType::TYPE, 0);
    }

    fn eq_or_infer<'a, 'ctx, 'b>(
        &self,
        _ctx: &'b mut Ctx<'a>,
        _pltype: Arc<RefCell<PLType>>,
        _builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<EqRes, PLDiag> {
        todo!()
    }
}

impl FuncDefNode {
    pub fn gen_snippet(&self) -> String {
        self.id.name.clone()
            + "("
            + &self
                .paralist
                .iter()
                .skip_while(|v| v.id.name == "self")
                .enumerate()
                .map(|(i, v)| format!("${{{}:{}}}", i + 1, v.id.name))
                .collect::<Vec<_>>()
                .join(", ")
            + ")$0"
    }
    pub fn emit_func_def<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<(), PLDiag> {
        if ctx.get_type(self.id.name.as_str(), self.id.range).is_ok() {
            return Err(ctx.add_diag(self.id.range.new_err(ErrorCode::REDEFINE_SYMBOL)));
        }
        let pltype = self.get_type(ctx, builder)?;
        ctx.add_type(self.id.name.clone(), pltype, self.id.range)?;
        Ok(())
    }
    pub fn gen_fntype<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        first: bool,
        builder: &'b BuilderEnum<'a, 'ctx>,
        fnvalue: FNValue,
    ) -> Result<(), PLDiag> {
        let child = &mut ctx.new_child(self.range.start, builder);
        child.protect_generic_context(&fnvalue.fntype.generic_map, |child| {
            if first && fnvalue.fntype.generic {
                fnvalue.fntype.generic_map.iter().for_each(|(_, pltype)| {
                    match &mut *pltype.borrow_mut() {
                        PLType::Generic(g) => {
                            g.set_place_holder(child);
                        }
                        _ => unreachable!(),
                    }
                });
                let mut place_holder_fn = fnvalue.clone();
                let name = place_holder_fn.append_name_with_generic(place_holder_fn.name.clone());
                place_holder_fn.llvmname = place_holder_fn
                    .llvmname
                    .replace(&place_holder_fn.name, &name);
                place_holder_fn.name = name.clone();
                place_holder_fn.fntype.generic_map.clear();
                place_holder_fn.generic_infer = Arc::new(RefCell::new(IndexMap::default()));
                fnvalue.generic_infer.borrow_mut().insert(
                    name,
                    Arc::new(RefCell::new(PLType::Fn(place_holder_fn.clone()))),
                );
            }
            let funcvalue = builder.get_or_insert_fn_handle(&fnvalue, child);
            child.function = Some(funcvalue);
            builder.build_sub_program(
                self.paralist.clone(),
                fnvalue.fntype.ret_pltype.clone(),
                &fnvalue,
                funcvalue,
                child,
            )?;
            // add block
            let allocab = builder.append_basic_block(funcvalue, "alloc");
            let entry = builder.append_basic_block(funcvalue, "entry");
            let return_block = builder.append_basic_block(funcvalue, "return");
            child.position_at_end(entry, builder);
            let ret_value_ptr = match &*fnvalue.fntype.ret_pltype.get_type(child, builder)?.borrow()
            {
                PLType::Void => None,
                other => {
                    builder.rm_curr_debug_location();
                    let retv = builder.alloc("retvalue", other, child, None);
                    // 返回值不能在函数结束时从root表移除
                    child.roots.borrow_mut().pop();
                    Some(retv)
                }
            };
            child.position_at_end(return_block, builder);
            child.return_block = Some((return_block, ret_value_ptr));
            if let Some(ptr) = ret_value_ptr {
                let value = builder.build_load(ptr, "load_ret_tmp");
                builder.build_return(Some(value));
            } else {
                builder.build_return(None);
            };
            child.position_at_end(entry, builder);
            // alloc para
            for (i, para) in fnvalue.fntype.param_pltypes.iter().enumerate() {
                let tp = para.get_type(child, builder)?;
                let b = tp.clone();
                let basetype = b.borrow();
                let alloca = builder.alloc(&fnvalue.param_names[i], &basetype, child, None);
                // add alloc var debug info
                builder.create_parameter_variable(
                    &fnvalue,
                    self.paralist[i].range.start,
                    i,
                    child,
                    funcvalue,
                    alloca,
                    allocab,
                );
                let parapltype = tp;
                child
                    .add_symbol(
                        fnvalue.param_names[i].clone(),
                        alloca,
                        parapltype,
                        self.paralist[i].id.range,
                        false,
                    )
                    .unwrap();
            }
            // emit body
            builder.rm_curr_debug_location();
            if self.id.name == "main" {
                if let Some(inst) = builder.get_first_instruction(allocab) {
                    builder.position_at(inst);
                } else {
                    child.position_at_end(allocab, builder);
                }
                child.init_global(builder);
                child.position_at_end(entry, builder);
            }
            child.rettp = Some(fnvalue.fntype.ret_pltype.get_type(child, builder)?);
            let terminator = self.body.as_mut().unwrap().emit(child, builder)?.get_term();
            if !terminator.is_return() {
                return Err(
                    child.add_diag(self.range.new_err(ErrorCode::FUNCTION_MUST_HAVE_RETURN))
                );
            }
            child.position_at_end(allocab, builder);
            builder.build_unconditional_branch(entry);
            Ok(())
        })
    }
}

impl PrintTrait for FuncDefNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("FuncDefNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id.name);
        for c in self.pre_comments.iter() {
            c.print(tabs + 1, false, line.clone());
        }
        for p in self.paralist.iter() {
            p.print(tabs + 1, false, line.clone());
        }
        // tab(tabs + 1, line.clone(), false);
        self.ret.print(tabs + 1, false, line.clone());
        if let Some(body) = &self.body {
            body.print(tabs + 1, true, line.clone());
        }
    }
}

impl Node for FuncDefNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        // hightlight
        ctx.save_if_comment_doc_hover(self.id.range, Some(self.doc.clone()));
        ctx.emit_comment_highlight(&self.pre_comments);
        ctx.push_semantic_token(self.id.range, SemanticTokenType::FUNCTION, 0);
        if let Some(generics) = &mut self.generics {
            generics.emit_highlight(ctx);
        }
        for para in self.paralist.iter() {
            ctx.push_semantic_token(para.id.range, SemanticTokenType::PARAMETER, 0);
            para.typenode.emit_highlight(ctx);
        }
        self.ret.emit_highlight(ctx);
        if let Some(trait_bounds) = &self.trait_bounds {
            trait_bounds
                .iter()
                .for_each(|trait_bound| trait_bound.emit_highlight(ctx));
        }
        let pltype = ctx.get_type(&self.id.name, self.id.range)?;
        if pltype.borrow().get_range() != Some(self.id.range) {
            return Err(self.id.range.new_err(ErrorCode::REDEFINE_SYMBOL));
        }
        if self.body.is_some() {
            let fntype = match &*pltype.borrow() {
                PLType::Fn(f) => {
                    let mut res = f.clone();
                    res.fntype = res.fntype.new_pltype();
                    res
                }
                _ => return Ok(Default::default()),
            };
            self.gen_fntype(ctx, true, builder, fntype)?;
        }
        usize::MAX.new_output(pltype).to_result()
    }
}

#[node]
pub struct ClosureNode {
    pub paralist: Vec<(Box<VarNode>, Option<Box<TypeNodeEnum>>)>,
    pub body: StatementsNode,
    pub ret: Option<Box<TypeNodeEnum>>,
}

static CLOSURE_COUNT: AtomicI32 = AtomicI32::new(0);

impl Node for ClosureNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        // 设计： https://github.com/Pivot-Studio/pivot-lang/issues/284
        let closure_name = format!("closure_{}", CLOSURE_COUNT.fetch_add(1, Ordering::Relaxed));
        builder.opaque_struct_type(&closure_name);
        let ret_tp = if let Some(ret) = &self.ret {
            ret.get_type(ctx, builder)?
        } else {
            todo!()
        };
        let mut paratps = vec![];
        for (para, typenode) in self.paralist.iter_mut() {
            let tp = if let Some(typenode) = typenode {
                typenode.get_type(ctx, builder)?
            } else {
                todo!()
            };
            paratps.push(tp);
        }
        let f = builder.create_closure_fn(ctx, &closure_name, &paratps, &ret_tp.borrow());
        let child = &mut ctx.new_child(self.range.start, builder);
        child.function = Some(f.clone());
        child.closure_table = Some(FxHashMap::default());
        // add block
        let allocab = builder.append_basic_block(f, "alloc");
        let entry = builder.append_basic_block(f, "entry");
        let return_block = builder.append_basic_block(f, "return");
        child.position_at_end(entry, builder);
        let ret_value_ptr = match &*ret_tp.borrow() {
            PLType::Void => None,
            other => {
                builder.rm_curr_debug_location();
                let retv = builder.alloc("retvalue", other, child, None);
                // 返回值不能在函数结束时从root表移除
                child.roots.borrow_mut().pop();
                Some(retv)
            }
        };
        child.position_at_end(return_block, builder);
        child.return_block = Some((return_block, ret_value_ptr));
        if let Some(ptr) = ret_value_ptr {
            let value = builder.build_load(ptr, "load_ret_tmp");
            builder.build_return(Some(value));
        } else {
            builder.build_return(None);
        };
        child.position_at_end(entry, builder);
        // emit body
        let terminator = self.body.emit(child, builder)?.get_term();
        if !terminator.is_return() {
            return Err(child.add_diag(self.range.new_err(ErrorCode::FUNCTION_MUST_HAVE_RETURN)));
        }
        child.position_at_end(allocab, builder);
        builder.build_unconditional_branch(entry);
        let tps = child
            .closure_table
            .as_ref()
            .unwrap()
            .iter()
            .map(|(_, v)| v.pltype.clone())
            .collect::<Vec<_>>();
        builder.add_body_to_struct_type_raw(&closure_name, &tps, ctx);
        ClosureType {
            arg_types: todo!(),
            ret_type: todo!(),
            range: todo!(),
        };
        todo!()
    }
}

impl PrintTrait for ClosureNode {
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>) {
        todo!()
    }
}
