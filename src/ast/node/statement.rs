use super::primary::*;
use super::*;
use crate::ast::ctx::Ctx;
use crate::ast::error::ErrorCode;
use inkwell::debug_info::*;
use inkwell::types::AnyType;
use internal_macro::range;
use lsp_types::SemanticTokenType;

#[range]
pub struct DefNode {
    pub var: VarNode,
    pub exp: Box<dyn Node>,
}
impl Node for DefNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("DefNode");
        self.var.print(tabs + 1, false, line.clone());
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.var.range, SemanticTokenType::VARIABLE, 0);
        let (value, pltype) = self.exp.emit(ctx)?;
        let base_value = if let Value::RefValue(ref_value) = value {
            ref_value.as_basic_value_enum()
        } else {
            ctx.try_load2(value).as_basic_value_enum()
        };
        let base_type = base_value.get_type();
        let ptr2value = alloc(ctx, base_type, &self.var.name);
        // for err tolerate
        let mut tp = "i64".to_string();
        if pltype.is_some() {
            tp = pltype.unwrap();
        }
        let pltype = tp;

        if let (_, Some(ditype)) = ctx.get_type(pltype.as_str(), self.range).unwrap() {
            let var_info = ctx.dibuilder.create_auto_variable(
                ctx.discope,
                &self.var.name,
                ctx.diunit.get_file(),
                self.var.range.start.line as u32,
                *ditype,
                true,
                DIFlags::PUBLIC,
                ditype.get_align_in_bits(),
            );
            ctx.build_dbg_location(self.var.range.start);
            ctx.dibuilder.insert_declare_at_end(
                ptr2value,
                Some(var_info),
                None,
                ctx.builder.get_current_debug_location().unwrap(),
                ctx.function.unwrap().get_first_basic_block().unwrap(),
            );
            let re = ctx.add_symbol(
                self.var.name.clone(),
                ptr2value,
                pltype.clone(),
                self.var.range,
            );
            if re.is_err() {
                return Err(re.unwrap_err());
            }
            ctx.builder.build_store(ptr2value, base_value);
            return Ok((Value::None, None));
        }
        todo!()
    }
}
#[range]
pub struct AssignNode {
    pub var: Box<dyn Node>,
    pub exp: Box<dyn Node>,
}
impl Node for AssignNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("AssignNode");
        self.var.print(tabs + 1, false, line.clone());
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let vrange = self.var.range();
        let (ptr, _) = self.var.emit(ctx)?;
        let (value, _) = self.exp.emit(ctx)?;
        let ptr = ctx.try_load1(ptr);
        if let Value::VarValue(ptr) = ptr {
            let load = ctx.try_load2(value);
            if ptr.get_type().get_element_type()
                != load.as_basic_value_enum().get_type().as_any_type_enum()
            {
                return Err(ctx.add_err(self.range, ErrorCode::ASSIGN_TYPE_MISMATCH));
            }
            ctx.builder.build_store(ptr, load.as_basic_value_enum());
            return Ok((Value::None, None));
        }
        Err(ctx.add_err(vrange, ErrorCode::NOT_ASSIGNABLE))
    }
}

#[range]
pub struct NLNode {}

impl Node for NLNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("NLNode");
    }
    fn emit<'a, 'ctx>(&'a mut self, _: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        Ok((Value::None, None))
    }
}

#[range]
pub struct StatementsNode {
    pub statements: Vec<Box<dyn Node>>,
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let child = &mut ctx.new_child(self.range.start);
        for m in self.statements.iter_mut() {
            let pos = m.range().start;
            child.build_dbg_location(pos);
            _ = m.emit(child);
        }
        Ok((Value::None, None))
    }
}

impl StatementsNode {
    pub fn emit_child<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let child = ctx;
        for m in self.statements.iter_mut() {
            let pos = m.range().start;
            child.build_dbg_location(pos);
            _ = m.emit(child);
        }
        Ok((Value::None, None))
    }
}
