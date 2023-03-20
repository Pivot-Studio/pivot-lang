use super::statement::StatementsNode;
use super::*;
use super::{types::TypedIdentifierNode, Node, TypeNode};
use crate::ast::diag::ErrorCode;
use crate::ast::node::{deal_line, tab};

use crate::ast::pltype::{eq, get_type_deep, FNType, FNValue, PLType};
use crate::ast::tokens::TokenType;
use crate::plv;
use indexmap::IndexMap;
use internal_macro::node;
use lsp_types::SemanticTokenType;
use std::cell::RefCell;

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

impl Node for FuncCallNode {
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> NodeResult {
        let id_range = self.callee.range();
        let (plvalue, pltype, _) = self.callee.emit(ctx, builder)?;
        if pltype.is_none() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::FUNCTION_NOT_FOUND)));
        }
        let pltype = pltype.unwrap();
        let mut fnvalue = match &*pltype.borrow() {
            PLType::FN(f) => {
                let mut res = f.clone();
                res.fntype = res.fntype.new_pltype();
                res
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
            let mut i = 0;
            for (_, pltype) in fnvalue.fntype.generic_map.iter() {
                if i >= fnvalue.fntype.generics_size {
                    break;
                }
                if generic_types[i].is_some() {
                    eq(pltype.clone(), generic_types[i].as_ref().unwrap().clone());
                }
                i += 1;
            }
        }
        let mut skip = 0;
        let mut para_values = vec![];
        let mut receiver_type = None;
        let fn_handle = plvalue.and_then(|plvalue| {
            if let Some((receiver, tp)) = plvalue.receiver {
                (skip, para_values, receiver_type) = (1, vec![receiver], tp);
            }
            Some(plvalue.value)
        });
        if fnvalue.fntype.param_pltypes.len() - skip as usize != self.paralist.len() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::PARAMETER_LENGTH_NOT_MATCH)));
        }
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
        let mut value_pltypes = vec![];
        for para in self.paralist.iter_mut() {
            let pararange = para.range();
            let (value, value_pltype, _) = para.emit(ctx, builder)?;
            if value.is_none() || value_pltype.is_none() {
                return Ok((None, None, TerminatorEnum::NONE));
            }
            let value_pltype = value_pltype.unwrap();
            let value_pltype = get_type_deep(value_pltype);
            let (load, _) =
                ctx.try_load2var(pararange, value.unwrap(), value_pltype.clone(), builder)?;
            para_values.push(load);
            value_pltypes.push((value_pltype, pararange));
        }
        // value check and generic infer
        let res = ctx.protect_generic_context(&fnvalue.fntype.generic_map.clone(), |ctx| {
            ctx.run_in_fn_mod_mut(&mut fnvalue, |ctx, fnvalue| {
                if let Some(receiver_pltype) = &receiver_type {
                    if !fnvalue.fntype.param_pltypes[0].eq_or_infer(
                        ctx,
                        receiver_pltype.clone(),
                        builder,
                    )? {
                        return Err(
                            ctx.add_diag(self.range.new_err(ErrorCode::RECEIVER_CANNOT_BE_INFER))
                        );
                    }
                }
                for (i, (value_pltype, pararange)) in value_pltypes.iter().enumerate() {
                    if !fnvalue.fntype.param_pltypes[i + skip as usize].eq_or_infer(
                        ctx,
                        value_pltype.clone(),
                        builder,
                    )? {
                        return Err(
                            ctx.add_diag(pararange.new_err(ErrorCode::PARAMETER_TYPE_NOT_MATCH))
                        );
                    }
                }
                Ok(())
            })?;
            if !fnvalue.fntype.generic_map.is_empty() {
                if fnvalue.fntype.need_gen_code() {
                    fnvalue = ctx.run_in_fn_mod_mut(&mut fnvalue, |ctx, fnvalue| {
                        fnvalue.generic_infer_pltype(ctx, builder)
                    })?;
                } else {
                    return Err(
                        ctx.add_diag(self.range.new_err(ErrorCode::GENERIC_CANNOT_BE_INFER))
                    );
                }
            }
            let function = if fn_handle.map_or(false, |x| x != usize::MAX) {
                fn_handle.unwrap()
            } else {
                builder.get_or_insert_fn_handle(&fnvalue, ctx)
            };
            builder.try_set_fn_dbg(self.range.start, ctx.function.unwrap());
            let ret = builder.build_call(function, &para_values);
            ctx.save_if_comment_doc_hover(id_range, Some(fnvalue.doc.clone()));
            let rettp = ctx.run_in_fn_mod_mut(&mut fnvalue, |ctx, fnvalue| {
                fnvalue.fntype.ret_pltype.get_type(ctx, builder)
            })?;
            match ret {
                Some(v) => Ok((
                    {
                        builder.rm_curr_debug_location();
                        let ptr = builder.alloc("ret_alloc_tmp", &rettp.borrow(), ctx, None);
                        let v = builder.build_load(v, "raw_ret");
                        builder.build_store(ptr, v);
                        Some(plv!(ptr))
                    },
                    Some({
                        match &*rettp.clone().borrow() {
                            PLType::GENERIC(g) => g.curpltype.as_ref().unwrap().clone(),
                            _ => rettp,
                        }
                    }),
                    TerminatorEnum::NONE,
                )),
                None => Ok((None, Some(rettp), TerminatorEnum::NONE)),
            }
        });
        ctx.set_if_refs_tp(pltype, id_range);
        ctx.emit_comment_highlight(&self.comments[0]);
        res
    }
}
#[node]
pub struct FuncDefNode {
    pub id: Box<VarNode>,
    pub paralist: Vec<Box<TypedIdentifierNode>>,
    pub ret: Box<TypeNodeEnum>,
    pub doc: Vec<Box<NodeEnum>>,
    pub precom: Vec<Box<NodeEnum>>,
    pub declare: bool,
    pub generics: Option<Box<GenericDefNode>>,
    pub body: Option<StatementsNode>,
    pub modifier: Option<(TokenType, Range)>,
    pub generics_size: usize, // the size of generics except the generics from impl node
}

impl TypeNode for FuncDefNode {
    fn get_type<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> TypeNodeResult {
        let child = &mut ctx.new_child(self.range.start, builder);
        let generic_map = self
            .generics
            .as_ref()
            .map_or(IndexMap::default(), |generics| generics.gen_generic_type());
        let (pltype, flater) = child.protect_generic_context(&generic_map, |child| {
            let mut flater = None;
            let mut param_pltypes = Vec::new();
            let mut param_name = Vec::new();
            let method = !self.paralist.is_empty() && self.paralist[0].id.name == "self";
            generic_map
                .iter()
                .for_each(|(_, pltype)| match &mut *pltype.borrow_mut() {
                    PLType::GENERIC(g) => {
                        g.set_place_holder(child);
                    }
                    _ => unreachable!(),
                });
            for para in self.paralist.iter() {
                let paramtype = para.typenode.get_type(child, builder)?;
                child.set_if_refs_tp(paramtype.clone(), para.typenode.range());
                param_pltypes.push(para.typenode.clone());
                param_name.push(para.id.name.clone());
            }
            let fnvalue = FNValue {
                name: self.id.name.clone(),
                param_names: param_name,
                range: self.range,
                doc: self.doc.clone(),
                llvmname: if self.declare {
                    self.id.name.clone()
                } else {
                    child.plmod.get_full_name(&self.id.name)
                },
                path: child.plmod.path.clone(),
                fntype: FNType {
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
            };
            if self.generics.is_none() {
                builder.get_or_insert_fn_handle(&fnvalue, child);
            }
            let pltype = Arc::new(RefCell::new(PLType::FN(fnvalue.clone())));
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
                if let PLType::POINTER(s) = &*receiver_pltype.borrow() {
                    if let PLType::STRUCT(s) = &*s.borrow() {
                        let fullname = s.get_st_full_name_except_generic();
                        flater = Some(move |ctx: &mut Ctx| {
                            ctx.add_method(
                                &fullname,
                                self.id.name.split("::").last().unwrap(),
                                fnvalue.clone(),
                                self.id.range,
                            );
                        });
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

    fn emit_highlight<'a, 'ctx>(&self, ctx: &mut Ctx<'a>) {
        ctx.emit_comment_highlight(&self.precom);
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
    ) -> Result<bool, PLDiag> {
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
        if let Ok(_) = ctx.get_type(self.id.name.as_str(), self.id.range) {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::REDEFINE_SYMBOL)));
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
        return child.protect_generic_context(&fnvalue.fntype.generic_map, |child| {
            if first && fnvalue.fntype.generic {
                fnvalue.fntype.generic_map.iter().for_each(|(_, pltype)| {
                    match &mut *pltype.borrow_mut() {
                        PLType::GENERIC(g) => {
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
                    Arc::new(RefCell::new(PLType::FN(place_holder_fn.clone()))),
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
                PLType::VOID => None,
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
            let (_, _, terminator) = self.body.as_mut().unwrap().emit(child, builder)?;
            if !terminator.is_return() {
                return Err(
                    child.add_diag(self.range.new_err(ErrorCode::FUNCTION_MUST_HAVE_RETURN))
                );
            }
            child.position_at_end(allocab, builder);
            builder.build_unconditional_branch(entry);
            return Ok(());
        });
    }
}

impl PrintTrait for FuncDefNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("FuncDefNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.id.name);
        for c in self.precom.iter() {
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
        ctx.emit_comment_highlight(&self.precom);
        ctx.push_semantic_token(self.id.range, SemanticTokenType::FUNCTION, 0);
        if let Some(generics) = &mut self.generics {
            generics.emit_highlight(ctx);
        }
        for para in self.paralist.iter() {
            ctx.push_semantic_token(para.id.range, SemanticTokenType::PARAMETER, 0);
            ctx.push_semantic_token(para.typenode.range(), SemanticTokenType::TYPE, 0);
        }
        ctx.push_semantic_token(self.ret.range(), SemanticTokenType::TYPE, 0);
        let pltype = ctx.get_type(&self.id.name, self.range)?;
        if pltype.borrow().get_range() != Some(self.range) {
            return Err(self.id.range.new_err(ErrorCode::REDEFINE_SYMBOL));
        }
        if self.body.is_some() {
            let fntype = match &*pltype.borrow() {
                PLType::FN(f) => {
                    let mut res = f.clone();
                    res.fntype = res.fntype.new_pltype();
                    res
                }
                _ => return Ok((None, None, TerminatorEnum::NONE)),
            };
            self.gen_fntype(ctx, true, builder, fntype)?;
        }
        Ok((None, Some(pltype), TerminatorEnum::NONE))
    }
}
