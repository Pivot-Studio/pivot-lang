use super::statement::StatementsNode;
use super::*;
use super::{alloc, types::TypedIdentifierNode, Node, TypeNode};
use crate::ast::diag::ErrorCode;
use crate::ast::node::{deal_line, tab};
use crate::ast::pltype::{FNType, PLType};
use crate::utils::read_config::enter;
use inkwell::debug_info::*;
use internal_macro::range;
use lsp_types::SemanticTokenType;
use std::cell::RefCell;
use std::fmt::format;
use std::rc::Rc;
use std::vec;

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FuncDefNode {
    pub typenode: FuncTypeNode,
    pub body: Option<StatementsNode>,
}

impl Node for FuncDefNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let paralist = &self.typenode.paralist;
        let params_print = print_params(&paralist);
        let mut doc_str = String::new();
        for c in self.typenode.doc.iter() {
            doc_str.push_str(&prefix.repeat(tabs));
            doc_str.push_str(&c.format(tabs, prefix));
        }
        let mut format_res = String::new();
        format_res.push_str(enter());
        let mut ret_type = String::new();
        ret_type.push_str(&self.typenode.ret.format(tabs, prefix));
        format_res.push_str(&doc_str);
        format_res.push_str(&prefix.repeat(tabs));
        format_res.push_str("fn ");
        format_res.push_str(&self.typenode.id.name.split("::").last().unwrap());
        format_res.push_str("(");
        format_res.push_str(&params_print);
        format_res.push_str(") ");
        format_res.push_str(&ret_type);
        match &self.body {
            Some(body) => {
                format_res.push_str(" {");
                format_res.push_str(&body.format(tabs + 1, prefix));
                format_res.push_str(&prefix.repeat(tabs));
                format_res.push_str("}");
            }
            None => {
                format_res.push_str(";");
            }
        }
        format_res.push_str(enter());
        format_res
    }

    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("FuncDefNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.typenode.id.name);
        for c in self.typenode.doc.iter() {
            c.print(tabs + 1, false, line.clone());
        }
        for p in self.typenode.paralist.iter() {
            p.print(tabs + 1, false, line.clone());
        }
        // tab(tabs + 1, line.clone(), false);
        self.typenode.ret.print(tabs + 1, false, line.clone());
        if let Some(body) = &self.body {
            body.print(tabs + 1, true, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.save_if_comment_doc_hover(self.typenode.id.range, Some(self.typenode.doc.clone()));
        for c in self.typenode.doc.iter_mut() {
            c.emit(ctx)?;
        }
        let mut para_pltypes = Vec::new();
        let mut para_names = Vec::new();
        let mut param_ditypes = Vec::new();
        ctx.push_semantic_token(self.typenode.id.range, SemanticTokenType::FUNCTION, 0);
        for para in self.typenode.paralist.iter() {
            ctx.push_semantic_token(para.id.range, SemanticTokenType::PARAMETER, 0);
            ctx.push_semantic_token(para.tp.range(), SemanticTokenType::TYPE, 0);
            para_names.push(para.id.clone());
            let pltype = para.tp.get_type(ctx)?;
            match &*pltype.borrow() {
                PLType::VOID => {
                    return Err(ctx.add_err(
                        para.range,
                        crate::ast::diag::ErrorCode::VOID_TYPE_CANNOT_BE_PARAMETER,
                    ))
                }
                pltype => {
                    para_pltypes.push(pltype.clone());
                    let di_type = pltype.get_ditype(ctx);
                    let di_type = di_type.unwrap();
                    param_ditypes.push(di_type);
                }
            };
        }
        ctx.push_semantic_token(self.typenode.ret.range(), SemanticTokenType::TYPE, 0);
        let res = ctx.get_type(self.typenode.id.name.as_str(), self.typenode.id.range);
        if res.is_err() {
            let diag = res.unwrap_err();
            ctx.add_diag(diag.clone());
            return Err(diag);
        }
        let pltype = res.unwrap().clone();
        if let Some(body) = self.body.as_mut() {
            let subroutine_type = ctx.dibuilder.create_subroutine_type(
                ctx.diunit.get_file(),
                self.typenode.ret.get_type(ctx)?.borrow().get_ditype(ctx),
                &param_ditypes,
                DIFlags::PUBLIC,
            );
            let subprogram = ctx.dibuilder.create_function(
                ctx.diunit.get_file().as_debug_info_scope(),
                self.typenode.id.name.as_str(),
                None,
                ctx.diunit.get_file(),
                self.range.start.line as u32,
                subroutine_type,
                true,
                true,
                self.range.start.line as u32,
                DIFlags::PUBLIC,
                false,
            );
            // add function
            let funcvalue = match &*pltype.borrow() {
                PLType::FN(fntype) => fntype.get_or_insert_fn(ctx),
                _ => return Ok((None, None, TerminatorEnum::NONE)),
            };
            funcvalue.set_subprogram(subprogram);
            ctx.function = Some(funcvalue);
            let mut ctx = ctx.new_child(self.range.start);
            ctx.discope = subprogram.as_debug_info_scope();
            // copy para type
            let mut para_tps = Vec::new();
            for i in 0..para_names.len() {
                let para_type = funcvalue.get_nth_param(i as u32);
                if para_type.is_none() {
                    return Err(
                        ctx.add_err(self.typenode.paralist[i].range, ErrorCode::EXPECT_TYPE)
                    );
                }
                para_tps.push(para_type.unwrap());
            }
            // add block
            let allocab = ctx.context.append_basic_block(funcvalue, "alloc");
            let entry = ctx.context.append_basic_block(funcvalue, "entry");
            let return_block = ctx.context.append_basic_block(funcvalue, "return");
            ctx.position_at_end(return_block);
            let ret_value_ptr = if funcvalue.get_type().get_return_type().is_some() {
                let pltype = self.typenode.ret.get_type(&mut ctx)?;
                let ret_type = {
                    let op = pltype.borrow().get_basic_type_op(&ctx);
                    if op.is_none() {
                        return Ok((None, None, TerminatorEnum::NONE));
                    }
                    op.unwrap()
                };
                let retv = alloc(&mut ctx, ret_type, "retvalue");
                // 返回值不能在函数结束时从root表移除
                ctx.roots.borrow_mut().pop();
                Some(retv)
            } else {
                None
            };

            ctx.return_block = Some((return_block, ret_value_ptr));
            if let Some(ptr) = ret_value_ptr {
                let value = ctx.nodebug_builder.build_load(ptr, "load_ret_tmp");
                ctx.builder.position_at_end(return_block);
                ctx.gc_collect();
                ctx.gc_rm_root_current(ptr.as_basic_value_enum());
                ctx.nodebug_builder.build_return(Some(&value));
            } else {
                ctx.gc_collect();
                ctx.nodebug_builder.build_return(None);
            };
            ctx.position_at_end(entry);
            // alloc para
            for (i, para) in para_tps.iter_mut().enumerate() {
                let alloca = alloc(&mut ctx, para.get_type(), &para_names[i].name);
                // add alloc var debug info
                let divar = ctx.dibuilder.create_parameter_variable(
                    ctx.discope,
                    para_names[i].name.as_str(),
                    i as u32,
                    ctx.diunit.get_file(),
                    self.range.start.line as u32,
                    param_ditypes[i],
                    false,
                    DIFlags::PUBLIC,
                );
                ctx.build_dbg_location(self.typenode.paralist[i].range.start);
                ctx.dibuilder.insert_declare_at_end(
                    alloca,
                    Some(divar),
                    None,
                    ctx.builder.get_current_debug_location().unwrap(),
                    allocab,
                );
                ctx.builder.build_store(alloca, *para);
                ctx.add_symbol(
                    para_names[i].name.clone(),
                    alloca,
                    Rc::new(RefCell::new(para_pltypes[i].clone())),
                    self.typenode.paralist[i].id.range,
                    false,
                )
                .unwrap();
            }
            // emit body
            ctx.builder.unset_current_debug_location();
            if self.typenode.id.name == "main" {
                if let Some(inst) = allocab.get_first_instruction() {
                    ctx.builder.position_at(allocab, &inst);
                    ctx.nodebug_builder.position_at(allocab, &inst);
                } else {
                    ctx.position_at_end(allocab);
                }
                ctx.init_global();
                ctx.builder.position_at_end(entry);
                ctx.nodebug_builder.position_at_end(entry);
            }
            let (_, _, terminator) = body.emit(&mut ctx)?;
            if !terminator.is_return() {
                return Err(ctx.add_err(self.range, ErrorCode::FUNCTION_MUST_HAVE_RETURN));
            }
            ctx.nodebug_builder.position_at_end(allocab);
            ctx.nodebug_builder.build_unconditional_branch(entry);
            return Ok((None, Some(pltype.clone()), TerminatorEnum::NONE));
        }
        Ok((None, Some(pltype.clone()), TerminatorEnum::NONE))
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FuncCallNode {
    pub id: Box<NodeEnum>,
    pub paralist: Vec<Box<NodeEnum>>,
}

impl Node for FuncCallNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::new();
        let mut param_str = String::new();
        match Some(&self.paralist) {
            Some(paralist) => {
                let mut len = 0;
                for param in paralist {
                    len += 1;
                    param_str.push_str(&param.format(tabs, prefix));
                    if len < paralist.len() {
                        param_str.push_str(", ");
                    }
                }
            }
            None => (),
        }
        format_res.push_str(&self.id.format(tabs, prefix));
        format_res.push_str("(");
        format_res.push_str(&param_str);
        format_res.push_str(")");
        format_res
    }
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let id_range = self.id.range();
        let mut para_values = Vec::new();
        let (plvalue, pltype, _) = self.id.emit(ctx)?;
        if pltype.is_none() || !matches!(*RefCell::borrow(&pltype.clone().unwrap()), PLType::FN(_))
        {
            return Err(ctx.add_err(self.range, ErrorCode::FUNCTION_NOT_FOUND));
        }
        let pltype = pltype.unwrap().clone();
        if let PLType::FN(fntype) = &*pltype.borrow() {
            let mut skip = 0;
            if plvalue.is_some() {
                if let Some(receiver) = plvalue.unwrap().receiver {
                    para_values.push(receiver.into());
                    skip = 1;
                }
            }
            // funcvalue must use fntype to get a new one,can not use the return  plvalue of id node emit
            let funcvalue = fntype.get_or_insert_fn(ctx);
            if funcvalue.count_params() - skip != self.paralist.len() as u32 {
                return Err(ctx.add_err(self.range, ErrorCode::PARAMETER_LENGTH_NOT_MATCH));
            }
            let mut prevpos = id_range.end;
            for (i, para) in self.paralist.iter_mut().enumerate() {
                let sigrange = prevpos.to(para.range().end);
                prevpos = para.range().end;
                let pararange = para.range();
                ctx.push_param_hint(pararange.clone(), fntype.param_name[i].clone());
                ctx.set_if_sig(
                    sigrange,
                    fntype.name.clone().split("::").last().unwrap().to_string()
                        + "("
                        + fntype
                            .param_name
                            .iter()
                            .enumerate()
                            .map(|(i, s)| {
                                s.clone() + ": " + &fntype.param_pltypes[i].borrow().get_name()
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                            .as_str()
                        + ")",
                    &fntype.param_name,
                    i as u32 + skip,
                );
                let re = para.emit(ctx);
                if re.is_err() {
                    continue;
                }
                let (value, _, _) = re.unwrap();
                if value.is_none() {
                    continue;
                }
                let load = ctx.try_load2var(pararange, value.unwrap())?;
                let param = funcvalue.get_nth_param(i as u32 + skip).unwrap();
                if load.get_type() != param.get_type() {
                    _ = ctx.add_err(pararange, ErrorCode::PARAMETER_TYPE_NOT_MATCH);
                }
                para_values.push(load.as_basic_value_enum().into());
            }
            let ret = ctx.builder.build_call(
                funcvalue,
                &para_values,
                format(format_args!("call_{}", RefCell::borrow(&pltype).get_name())).as_str(),
            );
            ctx.save_if_comment_doc_hover(id_range, Some(fntype.doc.clone()));
            let res = match ret.try_as_basic_value().left() {
                Some(v) => Ok((
                    {
                        let ptr = alloc(ctx, v.get_type(), "ret_alloc_tmp");
                        ctx.nodebug_builder.build_store(ptr, v);
                        Some(ptr.into())
                    },
                    Some(*fntype.ret_pltype.clone()),
                    TerminatorEnum::NONE,
                )),
                None => Ok((None, Some(*fntype.ret_pltype.clone()), TerminatorEnum::NONE)),
            };
            ctx.set_if_refs_tp(pltype.clone(), id_range);
            ctx.send_if_go_to_def(id_range, fntype.range, ctx.plmod.path.clone());
            return res;
        }
        return Err(ctx.add_err(self.range, ErrorCode::NOT_A_FUNCTION));
    }
}
#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FuncTypeNode {
    pub id: Box<VarNode>,
    pub paralist: Vec<Box<TypedIdentifierNode>>,
    pub ret: Box<TypeNodeEnum>,
    pub doc: Vec<Box<NodeEnum>>,
    pub declare: bool,
    pub generics: Option<Box<GenericDefNode>>,
}
impl FuncTypeNode {
    pub fn emit_func_type<'a, 'ctx>(
        &'a mut self,
        ctx: &mut crate::ast::ctx::Ctx<'a, 'ctx>,
    ) -> Result<(), PLDiag> {
        if let Ok(_) = ctx.get_type(&self.id.name.as_str(), self.id.range) {
            return Err(ctx.add_err(self.range, ErrorCode::REDEFINE_SYMBOL));
        }
        let mut param_pltypes = Vec::new();
        let mut param_name = Vec::new();
        let mut receiver = false;
        let mut first = true;
        for para in self.paralist.iter() {
            let paramtype = para.tp.get_type(ctx)?;
            if first && para.id.name == "self" {
                receiver = true;
            }
            first = false;
            param_pltypes.push(paramtype.clone());
            param_name.push(para.id.name.clone());
        }
        let refs = vec![];
        let ftp = FNType {
            name: self.id.name.clone(),
            ret_pltype: Box::new(self.ret.get_type(ctx)?),
            param_pltypes,
            param_name,
            range: self.id.range,
            refs: Rc::new(RefCell::new(refs)),
            doc: self.doc.clone(),
            llvmname: if self.declare {
                self.id.name.clone()
            } else {
                ctx.plmod.get_full_name(&self.id.name)
            },
            method: receiver,
        };
        if receiver {
            let a = self.paralist.first().unwrap().tp.get_type(ctx).unwrap();
            let mut b = a.borrow_mut();
            if let PLType::POINTER(s) = &mut *b {
                if let PLType::STRUCT(s) = &mut *s.borrow_mut() {
                    let mut ftp = ftp.clone();
                    ftp.param_pltypes = ftp.param_pltypes[1..].to_vec();
                    s.methods
                        .insert(self.id.name.split("::").last().unwrap().to_string(), ftp);
                }
            }
        }
        ftp.get_or_insert_fn(ctx);
        let pltype = PLType::FN(ftp);
        let pltype = Rc::new(RefCell::new(pltype));
        ctx.set_if_refs_tp(pltype.clone(), self.id.range);
        ctx.add_type(self.id.name.clone(), pltype.clone(), self.id.range)?;
        ctx.add_doc_symbols(pltype);
        Ok(())
    }
}
