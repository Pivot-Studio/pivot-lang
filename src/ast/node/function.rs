use super::statement::StatementsNode;
use super::*;
use super::{alloc, types::TypedIdentifierNode, Node, TypeNode};
use crate::ast::ctx::{FNType, PLType};
use crate::ast::diag::ErrorCode;
use crate::ast::node::{deal_line, tab};
use inkwell::debug_info::*;
use inkwell::types::BasicType;
use inkwell::values::FunctionValue;
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
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("FuncDefNode");
        tab(tabs + 1, line.clone(), false);
        println!("id: {}", self.typenode.id);
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
        } else {
            // tab(tabs + 1, line, true);
            // if self.typenode.ret.is_ref {
            //     println!("type: &{}", self.typenode.ret.id);
            // } else {
            //     println!("type: {}", self.typenode.ret.id);
            // }
        }
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.save_if_comment_doc_hover(self.range, Some(self.typenode.doc.clone()));
        for c in self.typenode.doc.iter_mut() {
            c.emit(ctx)?;
        }
        let mut para_pltypes = Vec::new();
        let mut para_names = Vec::new();
        let mut param_ditypes = Vec::new();
        ctx.push_semantic_token(self.typenode.range, SemanticTokenType::FUNCTION, 0);
        for para in self.typenode.paralist.iter() {
            ctx.push_semantic_token(para.id.range, SemanticTokenType::PARAMETER, 0);
            ctx.push_semantic_token(para.tp.range(), SemanticTokenType::TYPE, 0);
            para_names.push(para.id.clone());
            let pltype = para.tp.get_type(ctx)?;
            match pltype {
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
            }
        }
        ctx.push_semantic_token(self.typenode.ret.range(), SemanticTokenType::TYPE, 0);
        if let Some(body) = self.body.as_mut() {
            let subroutine_type = ctx.dibuilder.create_subroutine_type(
                ctx.diunit.get_file(),
                self.typenode.ret.get_type(ctx)?.get_ditype(ctx),
                &param_ditypes,
                DIFlags::PUBLIC,
            );
            let subprogram = ctx.dibuilder.create_function(
                ctx.diunit.get_file().as_debug_info_scope(),
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
            // add function
            let func;
            let res = ctx.get_type(self.typenode.id.as_str(), self.range);
            if res.is_err() {
                let diag = res.unwrap_err();
                ctx.add_diag(diag.clone());
                return Err(diag);
            }
            let fu = res.unwrap();
            func = match fu {
                PLType::FN(fu) => fu.get_value(ctx, &ctx.plmod),
                _ => panic!("type error"), // 理论上这两个Panic不可能触发
            };
            func.set_subprogram(subprogram);
            ctx.function = Some(func);
            let mut ctx = ctx.new_child(self.range.start);
            ctx.discope = subprogram.as_debug_info_scope();
            // copy para type
            let mut para_tps = Vec::new();
            for i in 0..para_names.len() {
                let para_type = func.get_nth_param(i as u32);
                if para_type.is_none() {
                    return Err(
                        ctx.add_err(self.typenode.paralist[i].range, ErrorCode::EXPECT_TYPE)
                    );
                }
                para_tps.push(para_type.unwrap());
            }
            // add block
            let allocab = ctx.context.append_basic_block(func, "alloc");
            let entry = ctx.context.append_basic_block(func, "entry");
            let return_block = ctx.context.append_basic_block(func, "return");
            ctx.position_at_end(return_block);
            let ret_value_ptr = if func.get_type().get_return_type().is_some() {
                let pltype = self.typenode.ret.get_type(&mut ctx)?;
                let ret_type = {
                    let op = pltype.get_basic_type_op(&ctx);
                    if op.is_none() {
                        return Ok((Value::None, None, TerminatorEnum::NONE));
                    }
                    op.unwrap()
                };
                Some(alloc(&mut ctx, ret_type, "retvalue"))
            } else {
                None
            };

            ctx.return_block = Some((return_block, ret_value_ptr));
            if let Some(ptr) = ret_value_ptr {
                let value = ctx.nodebug_builder.build_load(ptr, "load_ret_tmp");
                ctx.nodebug_builder.build_return(Some(&value));
            } else {
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
                    para_pltypes[i].clone(),
                    self.typenode.paralist[i].id.range,
                    false,
                )
                .unwrap();
            }
            // emit body
            if self.typenode.id == "main" {
                ctx.nodebug_builder
                    .build_call(ctx.init_func.unwrap(), &vec![], "init_call");
            }
            let (_, _, terminator) = body.emit(&mut ctx)?;
            if !terminator.is_return() {
                return Err(ctx.add_err(self.range, ErrorCode::FUNCTION_MUST_HAVE_RETURN));
            }
            ctx.nodebug_builder.position_at_end(allocab);
            ctx.nodebug_builder.build_unconditional_branch(entry);
            return Ok((Value::None, None, TerminatorEnum::NONE));
        }
        Ok((Value::None, None, TerminatorEnum::NONE))
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FuncCallNode {
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let mut para_values = Vec::new();
        let (v, id, _) = self.id.emit(ctx)?;
        let id = id.unwrap();
        let func = match v {
            Value::FnValue(f) => Some(f),
            _ => None,
        };
        if func.is_none() {
            return Err(ctx.add_err(self.range, ErrorCode::FUNCTION_NOT_FOUND));
        }
        let func = func.unwrap();
        if func.count_params() != self.paralist.len() as u32 {
            return Err(ctx.add_err(self.range, ErrorCode::PARAMETER_LENGTH_NOT_MATCH));
        }
        for (i, para) in self.paralist.iter_mut().enumerate() {
            let pararange = para.range();
            let (value, _, _) = para.emit(ctx)?;
            let load_op = ctx.try_load2var(value).as_basic_value_enum_op();
            if load_op.is_none() {
                return Ok((Value::None, None, TerminatorEnum::NONE));
            }
            let param = func.get_nth_param(i as u32).unwrap();
            let load = load_op.unwrap();
            if load.get_type() != param.get_type() {
                return Err(ctx.add_err(pararange, ErrorCode::PARAMETER_TYPE_NOT_MATCH));
            }
            para_values.push(load.as_basic_value_enum().into());
        }
        let ret = ctx.builder.build_call(
            func,
            &para_values,
            format(format_args!("call_{}", id.get_name().unwrap())).as_str(),
        );
        if let PLType::FN(fv) = &id {
            ctx.save_if_comment_doc_hover(self.range, Some(fv.doc.clone()));
            let o = match ret.try_as_basic_value().left() {
                Some(v) => Ok((
                    Value::LoadValue(v),
                    Some(*fv.ret_pltype.clone()),
                    TerminatorEnum::NONE,
                )),
                None => Ok((
                    Value::None,
                    Some(*fv.ret_pltype.clone()),
                    TerminatorEnum::NONE,
                )),
            };
            ctx.set_if_refs_tp(&id, self.range);
            ctx.send_if_go_to_def(self.range, fv.range, ctx.plmod.path.clone());
            return o;
        }
        return Err(ctx.add_err(self.range, ErrorCode::NOT_A_FUNCTION));
    }
}
#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct FuncTypeNode {
    pub id: String,
    pub paralist: Vec<Box<TypedIdentifierNode>>,
    pub ret: Box<TypeNodeEnum>,
    pub doc: Vec<Box<NodeEnum>>,
    pub declare: bool,
}
impl FuncTypeNode {
    pub fn emit_func_type<'a, 'ctx>(
        &'a mut self,
        ctx: &mut crate::ast::ctx::Ctx<'a, 'ctx>,
    ) -> Result<FunctionValue<'ctx>, PLDiag> {
        if let Ok(_) = ctx.get_type(self.id.as_str(), self.range) {
            return Err(ctx.add_err(self.range, ErrorCode::REDEFINE_SYMBOL));
        }
        let mut para_types = Vec::new();
        let mut param_pltypes = Vec::new();
        for para in self.paralist.iter() {
            let paramtype = para.tp.get_type(ctx)?;
            param_pltypes.push(paramtype.clone());
            para_types.push(paramtype.get_basic_type(&ctx).into());
        }
        let pltype = self.ret.get_type(ctx)?;
        let func_type = if pltype.is_void() {
            // void type
            ctx.context.void_type().fn_type(&para_types, false)
        } else {
            pltype.get_basic_type(&ctx).fn_type(&para_types, false)
        };
        let mut name = self.id.clone();
        if !self.declare {
            let full = ctx.plmod.get_full_name(self.id.as_str());
            name = full;
        }
        let func = ctx.module.add_function(&name, func_type, None);
        let refs = vec![];
        let ftp = PLType::FN(FNType {
            name: self.id.clone(),
            ret_pltype: Box::new(self.ret.get_type(ctx)?),
            param_pltypes,
            range: self.range,
            refs: Rc::new(RefCell::new(refs)),
            doc: self.doc.clone(),
        });
        ctx.set_if_refs_tp(&ftp, self.range);
        ctx.add_type(self.id.clone(), ftp, self.range)?;
        Ok(func)
    }
}
