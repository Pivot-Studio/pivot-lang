use super::statement::StatementsNode;
use super::types::*;
use super::*;
use super::{alloc, types::TypedIdentifierNode, Node};
use crate::ast::ctx::{FNType, PLType};
use crate::ast::node::{deal_line, tab};
use inkwell::debug_info::*;
use inkwell::values::FunctionValue;
use internal_macro::range;
use lsp_types::SemanticTokenType;
use std::cell::RefCell;
use std::fmt::format;
use std::rc::Rc;

use lazy_static::__Deref;

#[range]
pub struct FuncDefNode {
    pub typenode: FuncTypeNode,
    pub body: Option<StatementsNode>,
}

impl Node for FuncDefNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("FuncDefNode");
        tab(tabs + 1, line.clone(), end);
        println!("id: {}", self.typenode.id);
        for p in self.typenode.paralist.iter() {
            p.print(tabs + 1, false, line.clone());
        }
        if let Some(body) = &self.body {
            tab(tabs + 1, line.clone(), false);
            println!("type: {}", self.typenode.ret.id);
            body.print(tabs + 1, true, line.clone());
        } else {
            tab(tabs + 1, line, true);
            println!("type: {}", self.typenode.ret.id);
        }
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut crate::ast::ctx::Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let typenode = self.typenode.clone();
        ctx.push_semantic_token(typenode.range, SemanticTokenType::FUNCTION, 0);
        for p in typenode.paralist.iter() {
            ctx.push_semantic_token(p.id.range, SemanticTokenType::PARAMETER, 0);
            ctx.push_semantic_token(p.tp.range, SemanticTokenType::TYPE, 0);

            if p.tp.id == "void" {
                return Err(ctx.add_err(
                    p.range,
                    crate::ast::error::ErrorCode::VOID_TYPE_CANNOT_BE_PARAMETER,
                ));
            }
        }
        ctx.push_semantic_token(typenode.ret.range, SemanticTokenType::TYPE, 0);
        // build debug info
        let param_ditypes = self
            .typenode
            .paralist
            .iter()
            .map(|para| {
                let ditp = para.deref().tp.get_debug_type(ctx);
                if ditp.is_none() {
                    return ctx.get_type("i64", Default::default()).unwrap().1.unwrap();
                }
                let ditp = ditp.unwrap();
                ditp
            })
            .collect::<Vec<_>>();
        let subroutine_type = ctx.dibuilder.create_subroutine_type(
            ctx.diunit.get_file(),
            self.typenode.ret.get_debug_type(ctx),
            &param_ditypes,
            DIFlags::PUBLIC,
        );
        let subprogram = ctx.dibuilder.create_function(
            ctx.discope,
            self.typenode.id.as_str(),
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
        let para_pltype_ids: Vec<&String> =
            typenode.paralist.iter().map(|para| &para.tp.id).collect();
        // get the para's type vec & copy the para's name vec
        let mut para_names = Vec::new();
        for para in typenode.paralist.iter() {
            para_names.push(para.id.clone());
        }
        // add function
        let func;
        let (fu, _) = ctx.get_type(typenode.id.as_str(), self.range)?;
        func = match fu {
            PLType::FN(fu) => fu.fntype,
            _ => panic!("type error"), // 理论上这两个Panic不可能触发
        };
        func.set_subprogram(subprogram);
        ctx.discope = subprogram.as_debug_info_scope();
        ctx.function = Some(func);
        let mut ctx = ctx.new_child(self.range.start);

        if let Some(body) = self.body.as_mut() {
            // copy para type
            let mut para_tps = Vec::new();
            for i in 0..para_names.len() {
                let para_type = func.get_nth_param(i as u32).unwrap();
                para_tps.push(para_type);
            }
            // add block
            let allocab = ctx.context.append_basic_block(func, "alloc");
            let entry = ctx.context.append_basic_block(func, "entry");
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
                    para_pltype_ids[i].clone(),
                    typenode.paralist[i].id.range,
                )
                .unwrap();
            }
            // emit body
            _ = body.emit_child(&mut ctx);
            if ctx.block.unwrap().get_terminator().is_none() {
                return Err(ctx.add_err(
                    self.range,
                    crate::ast::error::ErrorCode::FUNCTION_MUST_HAVE_RETURN,
                ));
            }
            ctx.nodebug_builder.position_at_end(allocab);
            ctx.nodebug_builder.build_unconditional_branch(entry);
            return Ok((Value::None, None));
        }
        Ok((Value::None, None))
    }
}

#[range]
pub struct FuncCallNode {
    pub id: String,
    pub paralist: Vec<Box<dyn Node>>,
}

impl Node for FuncCallNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("FuncCallNode");
        let mut i = self.paralist.len();
        tab(tabs + 1, line.clone(), i == 0);
        println!("id: {}", self.id);
        for para in &self.paralist {
            i -= 1;
            para.print(tabs + 1, i == 0, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.range, SemanticTokenType::FUNCTION, 0);
        let mut para_values = Vec::new();
        let func = ctx.module.get_function(self.id.as_str());
        if func.is_none() {
            return Err(ctx.add_err(self.range, crate::ast::error::ErrorCode::FUNCTION_NOT_FOUND));
        }
        let func = func.unwrap();
        if func.count_params() != self.paralist.len() as u32 {
            return Err(ctx.add_err(
                self.range,
                crate::ast::error::ErrorCode::PARAMETER_LENGTH_NOT_MATCH,
            ));
        }
        for (i, para) in self.paralist.iter_mut().enumerate() {
            let pararange = para.range();
            let (v, _) = para.emit(ctx)?;
            let load = ctx.try_load2(v);
            let param = func.get_nth_param(i as u32).unwrap();
            let be = load.as_basic_value_enum_op();
            if be.is_none() {
                return Ok((load, None));
            }
            if be.unwrap().get_type() != param.get_type() {
                return Err(ctx.add_err(
                    pararange,
                    crate::ast::error::ErrorCode::PARAMETER_TYPE_NOT_MATCH,
                ));
            }
            para_values.push(load.as_basic_value_enum().into());
        }
        let ret = ctx.builder.build_call(
            func,
            &para_values,
            format(format_args!("call_{}", self.id)).as_str(),
        );
        let fntp = ctx.get_type(&self.id, self.range)?;
        if let (PLType::FN(fv), _) = fntp {
            let o = match (ret.try_as_basic_value().left(), fv.ret_pltype.as_ref()) {
                (Some(v), Some(pltype)) => Ok((Value::LoadValue(v.clone()), Some(pltype.clone()))),
                (None, Some(pltype)) => Ok((Value::None, Some(pltype.clone()))),
                _ => todo!(),
            };
            ctx.set_if_refs_tp(&fntp.0, self.range);
            ctx.send_if_go_to_def(self.range, fv.range);
            return o;
        }
        return Err(ctx.add_err(self.range, crate::ast::error::ErrorCode::NOT_A_FUNCTION));
    }
}
#[range]
#[derive(Clone)]
pub struct FuncTypeNode {
    pub id: String,
    pub paralist: Vec<Box<TypedIdentifierNode>>,
    pub ret: Box<TypeNameNode>,
}
impl FuncTypeNode {
    pub fn emit_func_type<'a, 'ctx>(
        &'a mut self,
        ctx: &mut crate::ast::ctx::Ctx<'a, 'ctx>,
    ) -> Result<FunctionValue<'ctx>, PLDiag> {
        if let Ok((func, _)) = ctx.get_type(self.id.as_str(), self.range) {
            let f = match func {
                PLType::FN(func) => func.fntype,
                _ => panic!("type error"),
            };
            return Ok(f);
        }
        let mut para_types = Vec::new();
        for para in self.paralist.iter() {
            let paramtps = para.tp.get_type(ctx)?;
            para_types.push(paramtps.0.get_basic_type().into());
        }
        let ret_type = self.ret.get_type(ctx)?.0.get_ret_type();
        let func_type = ret_type.fn_type(&para_types, false);
        let func = ctx.module.add_function(self.id.as_str(), func_type, None);
        let refs = vec![];
        let ftp = PLType::FN(FNType {
            name: self.id.clone(),
            fntype: func,
            ret_pltype: Some(self.ret.id.clone()),
            range: self.range,
            refs: Rc::new(RefCell::new(refs)),
        });
        ctx.set_if_refs_tp(&ftp, self.range);
        _ = ctx.add_type(self.id.clone(), ftp, self.range);
        Ok(func)
    }
}
