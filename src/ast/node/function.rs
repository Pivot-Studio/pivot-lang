use super::statement::StatementsNode;
use super::*;
use super::{types::TypedIdentifierNode, Node, TypeNode};
use crate::ast::diag::ErrorCode;
use crate::ast::node::{deal_line, tab};
use crate::ast::pltype::{eq, FNType, PLType};
use crate::plv;
use indexmap::IndexMap;
use inkwell::debug_info::*;
use internal_macro::{comments, fmt, range};
use lsp_types::SemanticTokenType;
use std::cell::RefCell;
use std::fmt::format;
use std::rc::Rc;
use std::vec;
#[range]
#[fmt]
#[comments]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FuncCallNode {
    pub generic_params: Option<Box<GenericParamNode>>,
    pub id: Box<NodeEnum>,
    pub paralist: Vec<Box<NodeEnum>>,
}

impl Node for FuncCallNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("FuncCallNode");
        let mut i = self.paralist.len();
        self.id.print(tabs + 1, false, line.clone());
        for para in &self.paralist {
            i -= 1;
            para.print(tabs + 1, i == 0, line.clone());
        }
    }
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b LLVMBuilder<'a, 'ctx>,
    ) -> NodeResult {
        // let currscope = ctx.discope;
        let mp = ctx.move_generic_types();
        let id_range = self.id.range();
        let mut para_values = Vec::new();
        let (plvalue, pltype, _) = self.id.emit(ctx, builder)?;
        if pltype.is_none() {
            return Err(ctx.add_err(self.range, ErrorCode::FUNCTION_NOT_FOUND));
        }
        let pltype = pltype.unwrap().clone();
        let mut fntype = match &*pltype.borrow() {
            PLType::FN(f) => f.clone(),
            _ => return Err(ctx.add_err(self.range, ErrorCode::FUNCTION_NOT_FOUND)),
        };
        fntype.add_generic_type(ctx)?;
        fntype.clear_generic();
        if let Some(generic_params) = &self.generic_params {
            let generic_params_range = generic_params.range.clone();
            generic_params.emit_highlight(ctx);
            if generic_params.generics.len() != fntype.generic_map.len() {
                return Err(
                    ctx.add_err(generic_params_range, ErrorCode::GENERIC_PARAM_LEN_MISMATCH)
                );
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
        if plvalue.is_some() {
            if let Some(receiver) = plvalue.unwrap().receiver {
                para_values.push(receiver.into());
                skip = 1;
            }
        }
        // funcvalue must use fntype to get a new one,can not use the return  plvalue of id node emit
        if fntype.param_pltypes.len() - skip as usize != self.paralist.len() {
            return Err(ctx.add_err(self.range, ErrorCode::PARAMETER_LENGTH_NOT_MATCH));
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
        // value check and generic infer
        for (i, para) in self.paralist.iter_mut().enumerate() {
            let pararange = para.range();
            let (value, value_pltype, _) = para.emit(ctx, builder)?;
            if value.is_none() || value_pltype.is_none() {
                return Ok((None, None, TerminatorEnum::NONE));
            }
            let (load, _) = ctx.try_load2var(
                pararange,
                value.unwrap(),
                value_pltype.clone().unwrap(),
                builder,
            )?;
            let value_pltype = value_pltype.unwrap();
            if !fntype.param_pltypes[i + skip as usize]
                .clone()
                .eq_or_infer(ctx, value_pltype.clone(), builder)?
            {
                return Err(ctx.add_err(pararange, ErrorCode::PARAMETER_TYPE_NOT_MATCH));
            }
            para_values.push(load);
        }
        if fntype.need_gen_code() {
            let block = ctx.block;
            ctx.need_highlight = false;
            let (_, pltype, _) = fntype.node.gen_fntype(ctx, false, builder)?;
            ctx.need_highlight = true;
            ctx.position_at_end(block.unwrap(), builder);
            let pltype = pltype.unwrap();
            match &*pltype.borrow() {
                PLType::FN(f) => {
                    fntype = f.clone();
                }
                _ => unreachable!(),
            };
        }
        let function = builder.get_or_insert_fn_handle(&fntype, ctx);
        if let Some(f) = ctx.function {
            builder.try_set_fn_dbg(self.range.start, f);
        };
        let ret = builder.build_call(function, false, para_values.iter());
        ctx.save_if_comment_doc_hover(id_range, Some(fntype.doc.clone()));
        let res = match ret {
            Some(v) => Ok((
                {
                    builder.rm_curr_debug_location();
                    let ptr = builder.alloc_vtp("ret_alloc_tmp", v, ctx);
                    builder.build_store(ptr, v);
                    Some(plv!(ptr))
                },
                Some({
                    match &*fntype.ret_pltype.get_type(ctx, builder)?.borrow() {
                        PLType::GENERIC(g) => g.curpltype.as_ref().unwrap().clone(),
                        _ => fntype.ret_pltype.get_type(ctx, builder)?,
                    }
                }),
                TerminatorEnum::NONE,
            )),
            None => Ok((
                None,
                Some(fntype.ret_pltype.get_type(ctx, builder)?),
                TerminatorEnum::NONE,
            )),
        };
        fntype.clear_generic();
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
impl FuncDefNode {
    pub fn emit_pl_tp<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b LLVMBuilder<'a, 'ctx>,
    ) -> Result<Rc<RefCell<PLType>>, PLDiag> {
        let mut param_pltypes = Vec::new();
        let mut param_name = Vec::new();
        let mut method = false;
        let mut first = true;
        let mut generic_map = IndexMap::default();
        if let Some(generics) = &mut self.generics {
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
        let refs = vec![];
        let mut ftp = FNType {
            name: self.id.name.clone(),
            ret_pltype: self.ret.clone(),
            param_pltypes,
            param_names: param_name,
            range: self.range,
            refs: Rc::new(RefCell::new(refs)),
            doc: self.doc.clone(),
            llvmname: if self.declare {
                self.id.name.clone()
            } else {
                ctx.plmod.get_full_name(&self.id.name)
            },
            method,
            generic_map,
            generic: self.generics.is_some(),
            node: Box::new(self.clone()),
        };
        if self.generics.is_none() {
            builder.get_or_insert_fn_handle(&ftp, ctx);
        }
        let pltype = Rc::new(RefCell::new(PLType::FN(ftp.clone())));
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
        builder: &'b LLVMBuilder<'a, 'ctx>,
    ) -> Result<(), PLDiag> {
        if let Ok(_) = ctx.get_type(&self.id.name.as_str(), self.id.range) {
            return Err(ctx.add_err(self.range, ErrorCode::REDEFINE_SYMBOL));
        }
        let pltype = self.emit_pl_tp(ctx, builder)?;
        ctx.add_type(self.id.name.clone(), pltype, self.id.range)?;
        Ok(())
    }
}
impl Node for FuncDefNode {
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
    fn emit<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b LLVMBuilder<'a, 'ctx>,
    ) -> NodeResult {
        self.gen_fntype(ctx, true, builder)
    }
}
impl FuncDefNode {
    fn gen_fntype<'a, 'ctx, 'b>(
        &mut self,
        ctx: &'b mut Ctx<'a>,
        first: bool,
        builder: &'b LLVMBuilder<'a, 'ctx>,
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
            return Err(PLDiag::new_error(self.id.range, ErrorCode::REDEFINE_SYMBOL));
        }
        let paras = self.paralist.clone();
        let ret = self.ret.clone();
        if let Some(body) = self.body.as_mut() {
            // add function
            let child = &mut ctx.new_child(self.range.start);
            let mp = child.move_generic_types();
            let mut fntype = match &*pltype.borrow() {
                PLType::FN(fntype) => fntype.clone(),
                _ => return Ok((None, None, TerminatorEnum::NONE)),
            };
            let funcvalue = {
                fntype.add_generic_type(child)?;
                if first {
                    fntype.generic_map.iter_mut().for_each(|(_, pltype)| {
                        match &mut *pltype.borrow_mut() {
                            PLType::GENERIC(g) => {
                                g.set_place_holder(child);
                            }
                            _ => unreachable!(),
                        }
                    })
                }
                builder.get_or_insert_fn_handle(&fntype, child)
            };
            // debug info
            // let subroutine_type = child.dibuilder.create_subroutine_type(
            //     child.diunit.get_file(),
            //     self.ret.get_type(child)?.borrow().get_ditype(child),
            //     &param_ditypes,
            //     DIFlags::PUBLIC,
            // );
            // let subprogram = child.dibuilder.create_function(
            //     child.diunit.get_file().as_debug_info_scope(),
            //     &fntype.append_name_with_generic(fntype.name.clone()),
            //     None,
            //     child.diunit.get_file(),
            //     self.range.start.line as u32,
            //     subroutine_type,
            //     true,
            //     true,
            //     self.range.start.line as u32,
            //     DIFlags::PUBLIC,
            //     false,
            // );
            // funcvalue.set_subprogram(subprogram);
            child.function = Some(funcvalue);

            // // let discope = child.discope;
            // child.discope = subprogram.as_debug_info_scope().clone();
            builder.build_sub_program(paras, ret, &fntype, funcvalue, child)?;
            // add block
            let allocab = builder.append_basic_block(funcvalue, "alloc");
            let entry = builder.append_basic_block(funcvalue, "entry");
            let return_block = builder.append_basic_block(funcvalue, "return");
            child.position_at_end(return_block, builder);
            let ret_value_ptr = match &*fntype.ret_pltype.get_type(child, builder)?.borrow() {
                PLType::VOID => None,
                _ => {
                    let pltype = self.ret.get_type(child, builder)?;
                    builder.rm_curr_debug_location();
                    let retv = builder.alloc("retvalue", &pltype.borrow(), child);
                    // 返回值不能在函数结束时从root表移除
                    child.roots.borrow_mut().pop();
                    Some(retv)
                }
            };

            child.return_block = Some((return_block, ret_value_ptr));
            if let Some(ptr) = ret_value_ptr {
                let value = builder.build_load(ptr, "load_ret_tmp");
                child.position_at_end(return_block, builder);
                builder.gc_collect(child);
                builder.gc_rm_root_current(ptr, child);
                builder.build_return(Some(value));
            } else {
                builder.gc_collect(child);
                builder.build_return(None);
            };
            child.position_at_end(entry, builder);
            // alloc para
            for (i, para) in fntype.param_pltypes.iter().enumerate() {
                let tp = para.get_type(child, builder)?;
                let basetype = tp.borrow();
                let alloca = builder.alloc(&fntype.param_names[i], &basetype, child);
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
                let parapltype = para.get_type(child, builder)?.clone();
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
            let (_, _, terminator) = body.emit(child, builder)?;
            if !terminator.is_return() {
                return Err(child.add_err(self.range, ErrorCode::FUNCTION_MUST_HAVE_RETURN));
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
