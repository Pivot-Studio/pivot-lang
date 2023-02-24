use super::statement::StatementsNode;
use super::*;
use super::{types::TypedIdentifierNode, Node, TypeNode};
use crate::ast::diag::ErrorCode;
use crate::ast::node::{deal_line, tab};

use crate::ast::pltype::{eq, get_type_deep, FNType, PLType};
use crate::plv;
use indexmap::IndexMap;
use internal_macro::{comments, fmt, range};
use lsp_types::SemanticTokenType;
use std::cell::RefCell;

use std::vec;
#[range]
#[fmt]
#[comments]
#[derive(Clone, PartialEq, Eq, Debug)]
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
        // let currscope = ctx.discope;
        let mp = ctx.move_generic_types();
        let id_range = self.callee.range();
        let mut para_values = Vec::new();
        let (plvalue, pltype, _) = self.callee.emit(ctx, builder)?;
        if pltype.is_none() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::FUNCTION_NOT_FOUND)));
        }
        let pltype = pltype.unwrap().clone();
        let mut fntype = match &*pltype.borrow() {
            PLType::FN(f) => f.new_pltype(),
            _ => return Err(ctx.add_diag(self.range.new_err(ErrorCode::FUNCTION_NOT_FOUND))),
        };
        if let Some(generic_params) = &self.generic_params {
            let generic_params_range = generic_params.range.clone();
            generic_params.emit_highlight(ctx);
            if generic_params.generics.len() != fntype.generic_map.len() {
                return Err(ctx.add_diag(
                    generic_params_range.new_err(ErrorCode::GENERIC_PARAM_LEN_MISMATCH),
                ));
            }
            let generic_types = generic_params.get_generic_types(ctx, builder)?;
            let mut i = 0;
            for (_, pltype) in fntype.generic_map.iter() {
                if generic_types[i].is_some() {
                    eq(pltype.clone(), generic_types[i].as_ref().unwrap().clone());
                }
                i = i + 1;
            }
        }
        let mut skip = 0;
        let mut function = None;

        if plvalue.is_some() {
            let v = plvalue.unwrap();
            if let Some(receiver) = v.receiver {
                para_values.push(receiver);
                skip = 1;
            }
            function = Some(v.value);
        }
        // funcvalue must use fntype to get a new one,can not use the return  plvalue of id node emit
        if fntype.param_pltypes.len() - skip as usize != self.paralist.len() {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::PARAMETER_LENGTH_NOT_MATCH)));
        }
        for (i, para) in self.paralist.iter_mut().enumerate() {
            let pararange = para.range();
            ctx.push_param_hint(
                pararange.clone(),
                fntype.param_names[i + skip as usize].clone(),
            );
            ctx.set_if_sig(
                para.range(),
                fntype.name.clone().split("::").last().unwrap().to_string()
                    + "("
                    + fntype
                        .param_names
                        .iter()
                        .enumerate()
                        .map(|(i, s)| {
                            s.clone()
                                + ": "
                                + FmtBuilder::generate_node(&fntype.param_pltypes[i]).as_str()
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                        .as_str()
                    + ")",
                &fntype.param_names,
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
        fntype.add_generic_type(ctx)?;
        for (i, (value_pltype, pararange)) in value_pltypes.iter().enumerate() {
            if !fntype.param_pltypes[i + skip as usize]
                .clone()
                .eq_or_infer(ctx, value_pltype.clone(), builder)?
            {
                return Err(ctx.add_diag(pararange.new_err(ErrorCode::PARAMETER_TYPE_NOT_MATCH)));
            }
        }
        if !fntype.generic_map.is_empty() {
            if fntype.need_gen_code() {
                fntype = fntype.generic_infer_pltype(ctx, builder)?
            } else {
                return Err(ctx.add_diag(self.range.new_err(ErrorCode::GENERIC_CANNOT_BE_INFER)));
            }
        }
        let function = if function.is_some() {
            function.unwrap()
        } else {
            builder.get_or_insert_fn_handle(&fntype, ctx)
        };
        if let Some(f) = ctx.function {
            builder.try_set_fn_dbg(self.range.start, f);
        };
        let ret = builder.build_call(function, &para_values);
        ctx.save_if_comment_doc_hover(id_range, Some(fntype.doc.clone()));
        let rettp = fntype.ret_pltype.get_type(ctx, builder)?;
        let res = match ret {
            Some(v) => Ok((
                {
                    builder.rm_curr_debug_location();
                    let ptr = builder.alloc("ret_alloc_tmp", &rettp.borrow(), ctx, None);
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
        };
        ctx.set_if_refs_tp(pltype.clone(), id_range);
        ctx.reset_generic_types(mp);
        ctx.emit_comment_highlight(&self.comments[0]);
        return res;
    }
}
#[range]
#[fmt]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FuncDefNode {
    pub id: Box<VarNode>,
    pub paralist: Vec<Box<TypedIdentifierNode>>,
    pub ret: Box<TypeNodeEnum>,
    pub doc: Vec<Box<NodeEnum>>,
    pub precom: Vec<Box<NodeEnum>>,
    pub declare: bool,
    pub generics: Option<Box<GenericDefNode>>,
    pub body: Option<StatementsNode>,
}

impl TypeNode for FuncDefNode {
    fn get_type<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> TypeNodeResult {
        self.emit_pl_tp(ctx, builder)
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

    pub fn emit_pl_tp<'a, 'ctx, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<Arc<RefCell<PLType>>, PLDiag> {
        let mut param_pltypes = Vec::new();
        let mut param_name = Vec::new();
        let mut method = false;
        let mut first = true;
        let mut generic_map = IndexMap::default();
        if let Some(generics) = &self.generics {
            generic_map = generics.gen_generic_type(ctx);
        }
        let mp = ctx.move_generic_types();
        for (name, pltype) in generic_map.iter() {
            ctx.add_generic_type(
                name.clone(),
                pltype.clone(),
                pltype.clone().borrow().get_range().unwrap(),
            );
        }
        for para in self.paralist.iter() {
            let paramtype = para.typenode.get_type(ctx, builder)?;
            ctx.set_if_refs_tp(paramtype.clone(), para.typenode.range());
            if first && para.id.name == "self" {
                method = true;
            }
            first = false;
            param_pltypes.push(para.typenode.clone());
            param_name.push(para.id.name.clone());
        }
        // let refs = RwVec::new();
        let mut ftp = FNType {
            name: self.id.name.clone(),
            ret_pltype: self.ret.clone(),
            param_pltypes,
            param_names: param_name,
            range: self.range,
            // refs: Arc::new(refs),
            doc: self.doc.clone(),
            llvmname: if self.declare {
                self.id.name.clone()
            } else {
                ctx.plmod.get_full_name(&self.id.name)
            },
            method,
            generic_map,
            generic_infer: Arc::new(RefCell::new(IndexMap::default())),
            generic: self.generics.is_some(),
            node: Some(Box::new(self.clone())),
        };
        if self.generics.is_none() {
            builder.get_or_insert_fn_handle(&ftp, ctx);
        }
        let pltype = Arc::new(RefCell::new(PLType::FN(ftp.clone())));
        ctx.set_if_refs_tp(pltype.clone(), self.id.range);
        ctx.add_doc_symbols(pltype.clone());
        if method {
            let a = self
                .paralist
                .first()
                .unwrap()
                .typenode
                .get_type(ctx, builder)
                .unwrap();
            let mut b = a.borrow_mut();
            if let PLType::POINTER(s) = &mut *b {
                if let PLType::STRUCT(s) = &mut *s.borrow_mut() {
                    ftp.param_pltypes = ftp.param_pltypes[1..].to_vec();
                    ctx.add_method(
                        s,
                        self.id.name.split("::").last().unwrap(),
                        ftp.clone(),
                        self.id.range,
                    );
                }
            }
        }
        ctx.reset_generic_types(mp);
        Ok(pltype)
    }
    pub fn emit_func_def<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<(), PLDiag> {
        if let Ok(_) = ctx.get_type(&self.id.name.as_str(), self.id.range) {
            return Err(ctx.add_diag(self.range.new_err(ErrorCode::REDEFINE_SYMBOL)));
        }
        let pltype = self.emit_pl_tp(ctx, builder)?;
        ctx.add_type(self.id.name.clone(), pltype, self.id.range)?;
        Ok(())
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
        self.gen_fntype(ctx, true, builder, None)
    }
}
impl FuncDefNode {
    pub fn gen_fntype<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        first: bool,
        builder: &'b BuilderEnum<'a, 'ctx>,
        fntype_opt: Option<FNType>,
    ) -> NodeResult {
        ctx.save_if_comment_doc_hover(self.id.range, Some(self.doc.clone()));
        ctx.emit_comment_highlight(&self.precom);
        ctx.push_semantic_token(self.id.range, SemanticTokenType::FUNCTION, 0);
        if let Some(generics) = &mut self.generics {
            generics.emit(ctx, builder)?;
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
        let paras = self.paralist.clone();
        let ret = self.ret.clone();
        if let Some(body) = self.body.as_mut() {
            let mut fntype = match &*pltype.borrow() {
                PLType::FN(fntype) => fntype.clone(),
                _ => return Ok((None, None, TerminatorEnum::NONE)),
            };
            if fntype_opt.is_some() {
                fntype = fntype_opt.unwrap();
            }
            // add function
            let child = &mut ctx.new_child(self.range.start, builder);
            let mp = child.move_generic_types();
            let funcvalue = {
                fntype.add_generic_type(child)?;
                if first {
                    if fntype.generic {
                        fntype.generic_map.iter_mut().for_each(|(_, pltype)| {
                            match &mut *pltype.borrow_mut() {
                                PLType::GENERIC(g) => {
                                    g.set_place_holder(child);
                                }
                                _ => unreachable!(),
                            }
                        });
                        let mut place_holder_fn = fntype.clone();
                        let name =
                            place_holder_fn.append_name_with_generic(place_holder_fn.name.clone());
                        place_holder_fn.llvmname = place_holder_fn
                            .llvmname
                            .replace(&place_holder_fn.name, &name)
                            .to_string();
                        place_holder_fn.name = name.clone();
                        place_holder_fn.generic_map.clear();
                        place_holder_fn.generic_infer = Arc::new(RefCell::new(IndexMap::default()));
                        fntype.generic_infer.borrow_mut().insert(
                            name,
                            Arc::new(RefCell::new(PLType::FN(place_holder_fn.clone()))),
                        );
                    }
                }
                builder.get_or_insert_fn_handle(&fntype, child)
            };
            child.function = Some(funcvalue);
            builder.build_sub_program(paras, ret, &fntype, funcvalue, child)?;
            // add block
            let allocab = builder.append_basic_block(funcvalue, "alloc");
            let entry = builder.append_basic_block(funcvalue, "entry");
            let return_block = builder.append_basic_block(funcvalue, "return");
            child.position_at_end(allocab, builder);
            let ret_value_ptr = match &*fntype.ret_pltype.get_type(child, builder)?.borrow() {
                PLType::VOID => None,
                _ => {
                    let pltype = self.ret.get_type(child, builder)?;
                    builder.rm_curr_debug_location();
                    let retv = builder.alloc("retvalue", &pltype.borrow(), child, None);
                    // 返回值不能在函数结束时从root表移除
                    child.roots.borrow_mut().pop();
                    Some(retv)
                }
            };
            child.position_at_end(return_block, builder);
            child.return_block = Some((return_block, ret_value_ptr));
            if let Some(ptr) = ret_value_ptr {
                let value = builder.build_load(ptr, "load_ret_tmp");
                // builder.gc_collect(child);
                // builder.gc_rm_root_current(ptr, child);
                builder.build_return(Some(value));
            } else {
                // builder.gc_collect(child);
                builder.build_return(None);
            };
            child.position_at_end(entry, builder);
            // alloc para
            for (i, para) in fntype.param_pltypes.iter().enumerate() {
                let tp = para.get_type(child, builder)?;
                let b = tp.clone();
                let basetype = b.borrow();
                let alloca = builder.alloc(&fntype.param_names[i], &basetype, child, None);
                // let stack_root = builder.get_stack_root(alloca);
                // add alloc var debug info
                builder.create_parameter_variable(
                    &fntype,
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
                        fntype.param_names[i].clone(),
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
            child.rettp = Some(fntype.ret_pltype.get_type(child, builder)?);
            let (_, _, terminator) = body.emit(child, builder)?;
            if !terminator.is_return() {
                return Err(
                    child.add_diag(self.range.new_err(ErrorCode::FUNCTION_MUST_HAVE_RETURN))
                );
            }
            child.position_at_end(allocab, builder);
            builder.build_unconditional_branch(entry);
            // child.discope = discope;
            child.reset_generic_types(mp);
            return Ok((None, Some(pltype.clone()), TerminatorEnum::NONE));
        }
        Ok((None, Some(pltype.clone()), TerminatorEnum::NONE))
    }
}
