use super::interface::TraitBoundNode;
use super::node_result::NodeResultBuilder;
use super::statement::StatementsNode;
use super::*;
use super::{types::TypedIdentifierNode, Node, TypeNode};
use crate::ast::accumulators::PLCodeLens;
use crate::ast::builder::no_op_builder::NoOpBuilder;
use crate::ast::builder::ValueHandle;
use crate::ast::ctx::{ClosureCtxData, CtxFlag, BUILTIN_FN_MAP};
use crate::ast::diag::ErrorCode;
use crate::ast::node::{deal_line, tab};

use crate::ast::pltype::{get_type_deep, ClosureType, FNValue, Field, FnType, PLType, STType};
use crate::ast::tokens::TokenType;
use indexmap::IndexMap;
use internal_macro::node;
use linked_hash_map::LinkedHashMap;
use lsp_types::{CodeLens, Command, SemanticTokenType};
use std::cell::RefCell;

use std::sync::atomic::{AtomicI32, Ordering};
use std::vec;

mod generator;
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
        self.callee
            .print(tabs + 1, self.paralist.is_empty(), line.clone());
        for para in &self.paralist {
            i -= 1;
            para.print(tabs + 1, i == 0, line.clone());
        }
    }
}

impl FuncCallNode {
    fn handle_closure_call<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
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
        builder.try_set_fn_dbg(self.range.start, ctx.function.unwrap());
        handle_ret(ret, c.ret_type.clone())
    }

    fn build_params<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        para_values: &mut Vec<usize>,
        value_pltypes: &mut Vec<(Arc<RefCell<PLType>>, Range)>,
        param_types: &[Box<TypeNodeEnum>],
        is_generic: bool,
    ) -> Result<(), PLDiag> {
        for (para, expect_ty) in self.paralist.iter_mut().zip(param_types.iter()) {
            let pararange = para.range();
            if !is_generic {
                if let TypeNodeEnum::Closure(_) = &**expect_ty {
                    _ = expect_ty
                        .get_type(ctx, builder, true)
                        .map(|t| ctx.expect_ty = Some(t));
                }
            }
            let v = para
                .emit(ctx, builder)
                .map(|re| {
                    ctx.expect_ty = None;
                    re
                })?
                .get_value();
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        let id_range = self.callee.range();
        let v = self.callee.emit(ctx, builder)?.get_value();
        if v.is_none() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::FUNCTION_NOT_FOUND)));
        }
        let v = v.unwrap();
        if let Some(builtin) = BUILTIN_FN_MAP.get(&v.get_value()) {
            return builtin(self, ctx, builder);
        }
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
            ctx.protect_generic_context(&fnvalue.fntype.generic_map.clone(), |ctx| {
                for (i, (_, pltype)) in fnvalue.fntype.generic_map.iter().enumerate() {
                    if i >= fnvalue.fntype.generics_size {
                        break;
                    }
                    if generic_types[i].is_some()
                        && !ctx
                            .eq(pltype.clone(), generic_types[i].as_ref().unwrap().clone())
                            .eq
                    {
                        let r = generic_params.generics[i].as_ref().unwrap().range();
                        return Err(r.new_err(ErrorCode::TYPE_MISMATCH).add_to_ctx(ctx));
                    }
                }
                Ok(())
            })?;
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
        self.build_params(
            ctx,
            builder,
            &mut para_values,
            &mut value_pltypes,
            &fnvalue.fntype.param_pltypes,
            fnvalue.fntype.generic,
        )?;
        let bb = builder.get_cur_basic_block();
        // value check and generic infer
        let res = ctx.protect_generic_context(&fnvalue.fntype.generic_map.clone(), |ctx| {
            let rettp = ctx.run_in_type_mod_mut(&mut fnvalue, |ctx, fnvalue| {
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
                fnvalue.fntype.ret_pltype.get_type(ctx, builder, true)
            })?;
            if !fnvalue.fntype.generic_map.is_empty() {
                if fnvalue.fntype.need_gen_code() {
                    fnvalue = ctx.run_in_type_mod_mut(&mut fnvalue, |ctx, fnvalue| {
                        // actual code gen happens here
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
                builder.get_or_insert_fn_handle(&fnvalue, ctx).0
            };
            builder.try_set_fn_dbg(self.range.start, ctx.function.unwrap());
            // let rettp = ctx.run_in_type_mod_mut(&mut fnvalue, |ctx, fnvalue| {
            //     fnvalue.fntype.ret_pltype.get_type(ctx, builder, true)
            // })?;
            builder.position_at_end_block(bb);
            let ret = builder.build_call(function, &para_values, &rettp.borrow(), ctx);
            ctx.save_if_comment_doc_hover(id_range, Some(fnvalue.doc.clone()));
            handle_ret(ret, rettp)
        });
        ctx.set_if_refs_tp(pltype, id_range);
        ctx.emit_comment_highlight(&self.comments[0]);
        res
    }
}

fn check_and_cast_params<'a, 'b>(
    value_pltypes: &[(Arc<RefCell<PLType>>, Range)],
    param_types: &[Box<TypeNodeEnum>],
    skip: u32,
    ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, '_>,
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
            let trait_pltype = param_types[i + skip as usize].get_type(ctx, builder, true)?;
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
    pub impl_trait: Option<(Box<TypeNodeEnum>, (TokenType, Range), bool)>, // bool是是否impl有generic
    pub is_method: bool,
    pub in_trait_def: bool,
    pub target_range: Range,
    pub generator: bool,
}

type OptFOnce<'a> = Option<Box<dyn FnOnce(&mut Ctx) -> Result<(), PLDiag> + 'a>>; // Thank u, Rust!

impl TypeNode for FuncDefNode {
    fn get_type<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
        _: bool,
    ) -> TypeNodeResult {
        let child = &mut ctx.new_child(self.range.start, builder);
        let generic_map = if let Some(generics) = &self.generics {
            // generics.set_traits(child, builder, &mp)?;
            generics.gen_generic_type(ctx)
        } else {
            IndexMap::default()
        };
        let (pltype, flater) = child.protect_generic_context(&generic_map, |child| {
            if let Some(generics) = &self.generics {
                generics.set_traits(child, &generic_map)?;
            }
            if let Some(trait_bounds) = &self.trait_bounds {
                for trait_bound in trait_bounds.iter() {
                    trait_bound.set_traits(child, &generic_map)?;
                }
            }
            let mut flater: OptFOnce = None;
            let mut param_pltypes = Vec::new();
            let mut param_name = Vec::new();
            let method = self.is_method;
            let (trait_tp, generic) = if let Some((v, (_, r), generic)) = &self.impl_trait {
                {
                    let re = v.clone().get_type(child, builder, false)?;
                    // child.set_self_type(re.clone());
                    (Some((re, *r)), *generic)
                }
            } else {
                (None, false)
            };
            generic_map
                .iter()
                .map(|(_, pltype)| match &*pltype.clone().borrow() {
                    PLType::Generic(g) => (pltype, g.set_place_holder(child, builder)),
                    _ => unreachable!(),
                })
                .for_each(|(g, tp)| match &mut *g.borrow_mut() {
                    PLType::Generic(g) => {
                        g.curpltype = Some(tp);
                    }
                    _ => unreachable!(),
                });
            // let mut first = true;
            for para in self.paralist.iter() {
                _ = para.typenode.get_type(child, builder, true)?;
                param_pltypes.push(para.typenode.clone());
                param_name.push(para.id.name.clone());
                // if first && method {
                //     if let PLType::Pointer(p) = &*tp.borrow() {
                //         child.set_self_type(p.clone());
                //     }
                // }
                // first = false;
            }
            self.ret.get_type(child, builder, true)?;
            let fnvalue = FNValue {
                name: self.id.name.clone(),
                param_names: param_name,
                range: self.id.range(),
                doc: self.doc.clone(),
                llvmname: if self.declare {
                    if self.id.name.starts_with('|') {
                        return Err(self
                            .range
                            .new_err(ErrorCode::METHODS_MUST_HAVE_BODY)
                            .add_to_ctx(ctx));
                    }
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
                    modifier: self.modifier.or_else(|| {
                        trait_tp.clone().and_then(|(t, _)| match &*t.borrow() {
                            PLType::Trait(t) => t.modifier,
                            _ => unreachable!(),
                        })
                    }),
                    generics_size: self.generics_size,
                },
                generic_infer: Arc::new(RefCell::new(IndexMap::default())),
                node: Some(Box::new(self.clone())),
                body_range: self.range,
                in_trait: self.in_trait_def,
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
                    .get_type(child, builder, true)
                    .unwrap();
                if let PLType::Pointer(s) = &*receiver_pltype.borrow() {
                    let s = s.clone();
                    flater = Some(Box::new(move |ctx: &mut Ctx| {
                        ctx.add_method(
                            &s.borrow(),
                            self.id.name.split("::").last().unwrap(),
                            fnvalue,
                            trait_tp,
                            generic,
                            self.target_range,
                        )
                    }));
                };
            }
            Ok((pltype, flater))
        })?;
        if let Some(flater) = flater {
            flater(ctx)?;
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
        if let TypeNodeEnum::Basic(n) = &*self.ret {
            if let Some(v) = n.generic_params.as_ref() {
                ctx.push_semantic_token(v.range, SemanticTokenType::TYPE_PARAMETER, 0);
            }
        }
    }

    fn eq_or_infer<'a, 'b>(
        &self,
        _ctx: &'b mut Ctx<'a>,
        _pltype: Arc<RefCell<PLType>>,
        _builder: &'b BuilderEnum<'a, '_>,
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
    pub fn emit_func_def<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<(), PLDiag> {
        if ctx.get_type(self.id.name.as_str(), self.id.range).is_ok() {
            return Err(ctx.add_diag(self.id.range.new_err(ErrorCode::REDEFINE_SYMBOL)));
        }
        let pltype = self.get_type(ctx, builder, true)?;
        ctx.add_type(self.id.name.clone(), pltype, self.id.range)?;
        Ok(())
    }
    pub fn gen_fntype<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        first: bool,
        mut builder: &'b BuilderEnum<'a, '_>,
        fnvalue: FNValue,
    ) -> Result<(), PLDiag> {
        ctx.run_as_root_ctx(|ctx| {
            let noop = BuilderEnum::NoOp(NoOpBuilder::default());
            // get it's pointer
            let noop_ptr = &noop as *const BuilderEnum<'a, '_>;
            let i8ptr = PLType::Pointer(ctx.get_type("i8", Default::default()).unwrap().tp);
            let child = &mut ctx.new_child(self.range.start, builder);
            child.protect_generic_context(&fnvalue.fntype.generic_map, |child| {
                if first && fnvalue.fntype.generic {
                    match builder {
                        BuilderEnum::NoOp(_) => (),
                        _ => return Ok(()),
                    }
                    builder = unsafe { &*(noop_ptr as *const BuilderEnum<'a, '_>) };
                    fnvalue
                        .fntype
                        .generic_map
                        .iter()
                        .map(|(_, pltype)| match &*pltype.clone().borrow() {
                            PLType::Generic(g) => (pltype, g.set_place_holder(child, builder)),
                            _ => unreachable!(),
                        })
                        .for_each(|(g, tp)| match &mut *g.borrow_mut() {
                            PLType::Generic(g) => {
                                g.curpltype = Some(tp);
                            }
                            _ => unreachable!(),
                        });
                    let mut place_holder_fn = fnvalue.clone();
                    let name =
                        place_holder_fn.append_name_with_generic(place_holder_fn.name.clone());
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
                let (mut funcvalue, exists) = builder.get_or_insert_fn_handle(&fnvalue, child);
                if exists {
                    return Ok(());
                }
                child.function = Some(funcvalue);
                let mut sttp_opt = None;
                let mut generator_alloca_b = 0;
                if self.generator {
                    generator::init_generator(
                        child,
                        &i8ptr,
                        &fnvalue,
                        builder,
                        &mut funcvalue,
                        &mut generator_alloca_b,
                        &mut sttp_opt,
                    )?;
                }

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
                if self.generator {
                    generator::save_generator_init_block(builder, child, entry);
                }
                let return_block = builder.append_basic_block(funcvalue, "return");
                child.position_at_end(entry, builder);
                let ret_value_ptr = if self.generator {
                    generator::build_generator_ret(builder, child, &fnvalue, entry)?
                } else {
                    match &*fnvalue
                        .fntype
                        .ret_pltype
                        .get_type(child, builder, true)?
                        .borrow()
                    {
                        PLType::Void => None,
                        other => {
                            builder.rm_curr_debug_location();
                            let retv = builder.alloc("retvalue", other, child, None);
                            Some(retv)
                        }
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
                if self.generator {
                    // 设置flag，该flag影响alloc逻辑
                    child.ctx_flag = CtxFlag::InGeneratorYield;
                }
                // alloc para
                for (i, para) in fnvalue.fntype.param_pltypes.iter().enumerate() {
                    let tp = para.get_type(child, builder, true)?;
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
                        &basetype,
                    );
                    let parapltype = tp;
                    child
                        .add_symbol(
                            fnvalue.param_names[i].clone(),
                            alloca,
                            parapltype,
                            self.paralist[i].id.range,
                            false,
                            false,
                        )
                        .unwrap();
                }
                // emit body
                builder.rm_curr_debug_location();
                if self.id.name == "main" && ctx.is_active_file() {
                    PLCodeLens::push(
                        ctx.db,
                        CodeLens {
                            range: self.id.range.to_diag_range(),
                            command: Some(Command::new(
                                "run".to_owned(),
                                "pivot-lang.run_current".to_owned(),
                                None,
                            )),
                            data: None,
                        },
                    );
                    PLCodeLens::push(
                        ctx.db,
                        CodeLens {
                            range: self.id.range.to_diag_range(),
                            command: Some(Command::new(
                                "debug".to_owned(),
                                "pivot-lang.debug_current".to_owned(),
                                None,
                            )),
                            data: None,
                        },
                    );
                    if let Some(inst) = builder.get_first_instruction(entry) {
                        builder.position_at(inst);
                    } else {
                        child.position_at_end(entry, builder);
                    }
                    child.init_global(builder);
                    child.position_at_end(entry, builder);
                }
                if !self.generator {
                    child.rettp = Some(fnvalue.fntype.ret_pltype.get_type(child, builder, true)?);
                }

                // trait method with generic

                if self.body.is_none() {
                    return Ok(());
                }
                // body generation
                let terminator = self
                    .body
                    .as_mut()
                    .expect(&self.id.name)
                    .emit(child, builder)?
                    .get_term();
                if !terminator.is_return() && !self.generator {
                    return Err(child.add_diag(
                        self.range
                            .end
                            .to(self.range.end)
                            .new_err(ErrorCode::FUNCTION_MUST_HAVE_RETURN),
                    ));
                }
                if self.generator {
                    return generator::end_generator(
                        child,
                        builder,
                        i8ptr.clone(),
                        sttp_opt,
                        funcvalue,
                        generator_alloca_b,
                        allocab,
                    );
                }
                child.position_at_end(allocab, builder);
                builder.build_unconditional_branch(entry);
                Ok(())
            })
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
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
            builder.set_di_file(&ctx.get_file());
        }
        usize::MAX.new_output(pltype.tp).to_result()
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
    fn emit<'a, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        // 设计： https://github.com/Pivot-Studio/pivot-lang/issues/284
        let i8ptr = PLType::Pointer(ctx.get_type("i8", Default::default()).unwrap().tp);
        let closure_name = format!("closure_{}", CLOSURE_COUNT.fetch_add(1, Ordering::Relaxed));
        let mut st_tp = STType {
            name: closure_name.clone(),
            path: ctx.plmod.path.clone(),
            fields: LinkedHashMap::default(),
            range: Default::default(),
            doc: vec![],
            generic_map: Default::default(),
            derives: vec![],
            modifier: Some((TokenType::PUB, Default::default())),
            body_range: Default::default(),
            is_trait: false,
            is_tuple: true,
            generic_infer_types: Default::default(),
            // generic_infer: Default::default(),
            methods: Default::default(),
            trait_methods_impl: Default::default(),
        };

        builder.opaque_struct_type(&st_tp.get_full_name());
        builder.add_body_to_struct_type(&st_tp.get_full_name(), &st_tp, ctx);
        let mut paratps = vec![];
        for (i, (v, typenode)) in self.paralist.iter_mut().enumerate() {
            ctx.push_semantic_token(v.range(), SemanticTokenType::PARAMETER, 0);
            let tp = if let Some(typenode) = typenode {
                typenode.emit_highlight(ctx);
                typenode.get_type(ctx, builder, true)?
            } else if let Some(exp_ty) = &ctx.expect_ty {
                match &*exp_ty.borrow() {
                    PLType::Closure(c) => {
                        if i >= c.arg_types.len() {
                            return Err(v
                                .range()
                                .new_err(ErrorCode::CLOSURE_PARAM_TYPE_UNKNOWN)
                                .add_help("try manually specify the parameter type of the closure")
                                .add_to_ctx(ctx));
                        }
                        ctx.push_type_hints(v.range(), c.arg_types[i].clone());
                        c.arg_types[i].clone()
                    }
                    _ => {
                        return Err(v
                            .range()
                            .new_err(ErrorCode::CLOSURE_PARAM_TYPE_UNKNOWN)
                            .add_help("try manually specify the parameter type of the closure")
                            .add_to_ctx(ctx));
                    }
                }
            } else {
                return Err(v
                    .range()
                    .new_err(ErrorCode::CLOSURE_PARAM_TYPE_UNKNOWN)
                    .add_help("try manually specify the parameter type of the closure")
                    .add_to_ctx(ctx));
            };
            paratps.push(tp);
        }
        let ret_tp = if let Some(ret) = &self.ret {
            ret.emit_highlight(ctx);
            ret.get_type(ctx, builder, true)?
        } else if let Some(exp_ty) = &ctx.expect_ty {
            match &*exp_ty.borrow() {
                PLType::Closure(c) => c.ret_type.clone(),
                _ => {
                    return Err(self
                        .range
                        .new_err(ErrorCode::CLOSURE_RET_TYPE_UNKNOWN)
                        .add_help("try manually specify the return type of the closure")
                        .add_to_ctx(ctx));
                }
            }
        } else {
            return Err(self
                .range
                .new_err(ErrorCode::CLOSURE_RET_TYPE_UNKNOWN)
                .add_help("try manually specify the return type of the closure")
                .add_to_ctx(ctx));
        };
        let cur = builder.get_cur_basic_block();
        ctx.try_set_closure_alloca_bb(builder.get_first_basic_block(ctx.function.unwrap()));
        let f = builder.create_closure_fn(ctx, &closure_name, &paratps, &ret_tp.borrow());
        let child = &mut ctx.new_child(self.range.start, builder);
        child.generator_data = None;
        child.ctx_flag = CtxFlag::Normal;
        child.function = Some(f);
        let stpltp = PLType::Struct(st_tp.clone());
        let ptr_tp = PLType::Pointer(Arc::new(RefCell::new(stpltp)));
        let mut all_tps = vec![Arc::new(RefCell::new(i8ptr.clone()))];
        all_tps.extend(paratps.clone());
        builder.build_sub_program_by_pltp(
            &all_tps,
            ret_tp.clone(),
            &closure_name,
            self.range.start.line as _,
            f,
            child,
        );
        // add block
        let allocab = builder.append_basic_block(f, "alloc");
        let entry = builder.append_basic_block(f, "entry");
        let return_block = builder.append_basic_block(f, "return");
        builder.rm_curr_debug_location();
        child.position_at_end(entry, builder);
        let ret_value_ptr = match &*ret_tp.borrow() {
            PLType::Void => None,
            other => {
                builder.rm_curr_debug_location();
                let retv = builder.alloc("retvalue", other, child, None);
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
        child.position_at_end(allocab, builder);
        // let closure_data_alloca = builder.alloc("closure_data", &stpltp, child, None);
        let data_raw = builder.get_nth_param(f, 0);
        let casted_data = builder.bitcast(child, data_raw, &ptr_tp, "casted_data");
        // let alloca = builder.alloc("closure_data",&ptr_tp, child, None);
        // builder.build_store(alloca, casted_data);
        child.position_at_end(entry, builder);
        child.closure_data = Some(RefCell::new(ClosureCtxData {
            table: LinkedHashMap::default(),
            data_handle: casted_data,
            alloca_bb: None,
        }));
        // alloc para
        for (i, tp) in paratps.iter().enumerate() {
            let b = tp.clone();
            let basetype = b.borrow();
            let alloca = builder.alloc(
                &self.paralist[i].0.name,
                &basetype,
                child,
                Some(self.paralist[i].0.range.start),
            );

            let parapltype = tp;
            builder.create_closure_parameter_variable(i as u32 + 1, f, alloca);
            child
                .add_symbol(
                    self.paralist[i].0.name.clone(),
                    alloca,
                    parapltype.to_owned(),
                    self.paralist[i].0.range,
                    false,
                    false,
                )
                .unwrap();
        }
        child.rettp = Some(ret_tp.clone());
        // emit body
        let terminator = self.body.emit(child, builder)?.get_term();
        if !terminator.is_return() {
            return Err(child.add_diag(
                self.range
                    .end
                    .to(self.range.end)
                    .new_err(ErrorCode::FUNCTION_MUST_HAVE_RETURN),
            ));
        }
        child.position_at_end(allocab, builder);
        let mut i = 1;
        let mut tps = vec![];
        for (k, (v, _)) in &child.closure_data.as_ref().unwrap().borrow().table {
            let pltp = PLType::Pointer(v.pltype.to_owned());
            st_tp.fields.insert(
                k.to_owned(),
                Field {
                    index: i,
                    typenode: pltp.get_typenode(&child.plmod.path),
                    name: k.to_owned(),
                    range: Default::default(),
                    modifier: None,
                },
            );
            tps.push(Arc::new(RefCell::new(pltp)));
            i += 1;
        }
        let stpltp = PLType::Struct(st_tp.clone());
        let ptr_tp = PLType::Pointer(Arc::new(RefCell::new(stpltp)));
        builder.create_parameter_variable_dbg(
            &ptr_tp,
            self.range.start,
            0,
            child,
            casted_data,
            allocab,
            "captured_closure_data",
        );
        // builder.insert_var_declare("captured_closure_data", self.range.start, &PLType::Struct(st_tp.clone()), casted_data, child);
        builder.build_unconditional_branch(entry);
        let closure_table = child.closure_data.as_ref().unwrap();
        ctx.position_at_end(cur, builder);
        builder.try_set_fn_dbg(self.range.start, ctx.function.unwrap());
        builder.gen_st_visit_function(ctx, &st_tp, &tps);
        let closure_f_tp = ClosureType {
            arg_types: paratps,
            ret_type: ret_tp,
            range: self.range,
        };
        let closure_f_tp = PLType::Closure(closure_f_tp);
        let closure_alloca = builder.alloc("closure", &closure_f_tp, ctx, None);
        let closure_data_alloca =
            builder.alloc("closure_data", &PLType::Struct(st_tp.clone()), ctx, None);
        for (k, (_, ori_v)) in &closure_table.borrow().table {
            let field = st_tp.fields.get(k).unwrap();
            let alloca: usize = builder
                .build_struct_gep(closure_data_alloca, field.index, k)
                .unwrap();
            builder.build_store(alloca, *ori_v);
        }
        let f_field = builder
            .build_struct_gep(closure_alloca, 0, "closure_f")
            .unwrap();
        builder.build_store(f_field, f);
        let d_field = builder
            .build_struct_gep(closure_alloca, 1, "closure_d")
            .unwrap();
        let d_casted = builder.bitcast(ctx, closure_data_alloca, &i8ptr, "casted_closure_d");
        builder.build_store(d_field, d_casted);
        closure_alloca
            .new_output(Arc::new(RefCell::new(closure_f_tp)))
            .to_result()
    }
}

impl PrintTrait for ClosureNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ClosureNode");
        deal_line(tabs + 1, &mut line, false);
        tab(tabs + 1, line.clone(), false);
        println!("paralist:");
        for (i, p) in self.paralist.iter().enumerate() {
            deal_line(tabs + 2, &mut line, i == self.paralist.len() - 1);
            p.0.print(tabs + 2, i == self.paralist.len() - 1, line.clone());
            if let Some(x) = &p.1 {
                deal_line(tabs + 3, &mut line, true);
                x.print(tabs + 3, true, line.clone())
            }
        }
        self.body.print(tabs + 1, true, line.clone());
    }
}
