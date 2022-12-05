use super::*;
use crate::ast::ctx::Ctx;
use crate::ast::diag::{ErrorCode, WarnCode};
use inkwell::debug_info::*;
use internal_macro::{comments, format, range};
use lsp_types::SemanticTokenType;
#[range]
#[comments]
#[format]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DefNode {
    pub var: VarNode,
    pub tp: Option<Box<TypeNodeEnum>>,
    pub exp: Option<Box<NodeEnum>>,
}
impl Node for DefNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("DefNode");
        self.var.print(tabs + 1, false, line.clone());
        if let Some(tp) = &self.tp {
            tp.print(tabs + 1, true, line.clone());
        } else {
            self.exp
                .as_ref()
                .unwrap()
                .print(tabs + 1, true, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let range = self.range();
        ctx.push_semantic_token(self.var.range, SemanticTokenType::VARIABLE, 0);
        if self.exp.is_none() && self.tp.is_none() {
            return Err(ctx.add_err(self.range, ErrorCode::UNDEFINED_TYPE));
        }
        let mut pltype = None;
        let mut expv = None;
        if let Some(tp) = &self.tp {
            tp.emit_highlight(ctx);
            pltype = Some(tp.get_type(ctx)?);
        }
        if let Some(exp) = &mut self.exp {
            let (value, pltype_opt, _) = ctx.emit_with_expectation(exp, pltype.clone())?;
            // for err tolerate
            if pltype_opt.is_none() {
                return Err(ctx.add_err(self.range, ErrorCode::UNDEFINED_TYPE));
            }
            if value.is_none() {
                return Err(ctx.add_err(self.range, ErrorCode::EXPECT_VALUE));
            }
            let tp = pltype_opt.unwrap();
            if pltype.is_none() {
                ctx.push_type_hints(self.var.range, tp.clone());
                pltype = Some(tp);
            } else if pltype.clone().unwrap() != tp {
                return Err(ctx.add_err(self.range, ErrorCode::TYPE_MISMATCH));
            }
            expv = value;
        }
        let pltype = pltype.unwrap();
        let ditype = pltype.borrow().get_ditype(ctx);
        let tp = pltype.borrow().get_basic_type_op(ctx);
        if tp.is_none() || ditype.is_none() {
            return Err(ctx.add_err(range, ErrorCode::UNDEFINED_TYPE));
        }
        let base_type = tp.unwrap();
        let ptr2value = alloc(ctx, base_type, &self.var.name);
        let debug_var_info = ctx.dibuilder.create_auto_variable(
            ctx.discope,
            &self.var.name,
            ctx.diunit.get_file(),
            self.var.range.start.line as u32,
            ditype.unwrap(),
            true,
            DIFlags::PUBLIC,
            ditype.unwrap().get_align_in_bits(),
        );
        ctx.build_dbg_location(self.var.range.start);
        ctx.dibuilder.insert_declare_at_end(
            ptr2value,
            Some(debug_var_info),
            None,
            ctx.builder.get_current_debug_location().unwrap(),
            ctx.function.unwrap().get_first_basic_block().unwrap(),
        );
        ctx.add_symbol(
            self.var.name.clone(),
            ptr2value,
            pltype,
            self.var.range,
            false,
        )?;
        if let Some(exp) = expv {
            ctx.build_dbg_location(self.var.range.start);
            ctx.builder
                .build_store(ptr2value, ctx.try_load2var(range, exp)?);
        }
        return Ok((None, None, TerminatorEnum::NONE));
    }
}
#[range]
#[format]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct AssignNode {
    pub var: Box<NodeEnum>,
    pub exp: Box<NodeEnum>,
}
impl Node for AssignNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("AssignNode");
        self.var.print(tabs + 1, false, line.clone());
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let exp_range = self.exp.range();
        let (ptr, lpltype, _) = self.var.emit(ctx)?;
        let (value, rpltype, _) = self.exp.emit(ctx)?;
        if lpltype != rpltype {
            return Err(ctx.add_err(self.range, ErrorCode::ASSIGN_TYPE_MISMATCH));
        }
        if ptr.as_ref().unwrap().is_const {
            return Err(ctx.add_err(self.range, ErrorCode::ASSIGN_CONST));
        }
        let load = ctx.try_load2var(exp_range, value.unwrap())?;
        ctx.builder.build_store(
            ptr.unwrap().into_pointer_value(),
            load.as_basic_value_enum(),
        );
        return Ok((None, None, TerminatorEnum::NONE));
    }
}

#[range]
#[comments]
#[format]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct EmptyNode {}

impl Node for EmptyNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("EmptyNode");
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.emit_comment_highlight(&self.comments[0]);
        Ok((None, None, TerminatorEnum::NONE))
    }
}

#[range]
#[format]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StatementsNode {
    pub statements: Vec<Box<NodeEnum>>,
}
impl Node for StatementsNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("StatementsNode");
        let mut i = self.statements.len();
        for statement in &self.statements {
            i -= 1;
            statement.print(tabs + 1, i == 0, line.clone());
        }
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let mut terminator = TerminatorEnum::NONE;
        for m in self.statements.iter_mut() {
            if let NodeEnum::Empty(_) = **m {
                continue;
            }
            if !terminator.is_none() {
                if let NodeEnum::Comment(c) = &**m {
                    ctx.push_semantic_token(c.range, SemanticTokenType::COMMENT, 0);
                    continue;
                }
                ctx.add_warn(m.range(), WarnCode::UNREACHABLE_STATEMENT);
                continue;
            }
            let pos = m.range().start;
            ctx.build_dbg_location(pos);
            let re = m.emit(ctx);
            if re.is_err() {
                continue;
            }
            let (_, _, terminator_res) = re.unwrap();
            terminator = terminator_res;
        }
        for root in ctx.roots.clone().borrow().iter() {
            ctx.gc_rm_root(root.as_basic_value_enum())
        }
        Ok((None, None, terminator))
    }
}

impl StatementsNode {
    pub fn emit_child<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let child = &mut ctx.new_child(self.range.start);
        self.emit(child)
    }
}
