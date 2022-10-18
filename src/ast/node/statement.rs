use super::*;
use crate::ast::ctx::Ctx;
use crate::ast::diag::{ErrorCode, WarnCode};
use inkwell::debug_info::*;
use inkwell::types::AnyType;
use internal_macro::range;
use lsp_types::SemanticTokenType;
#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DefNode {
    pub var: VarNode,
    pub exp: Box<NodeEnum>,
}
impl Node for DefNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        return "hello".to_string();
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("DefNode");
        self.var.print(tabs + 1, false, line.clone());
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.var.range, SemanticTokenType::VARIABLE, 0);
        let (value, pltype_opt, _) = self.exp.emit(ctx)?;
        // for err tolerate
        let mut tp = "i64".to_string();
        if pltype_opt.is_some() {
            tp = pltype_opt.unwrap();
        }
        let pltype_name = tp;
        let (pltype, ditype) = ctx.get_type(pltype_name.as_str(), self.range).unwrap();
        let (base_value, debug_type) = if let Value::RefValue(ref_value) = value {
            (
                ref_value.as_basic_value_enum(),
                Some(
                    pltype
                        .clone()
                        .get_di_ref_type(ctx, ditype.clone())
                        .unwrap()
                        .as_type(),
                ),
            )
        } else {
            let ditype = ditype.clone();
            (ctx.try_load2var(value).as_basic_value_enum(), ditype)
        };
        let base_type = base_value.get_type();
        let ptr2value = alloc(ctx, base_type, &self.var.name);
        let debug_var_info = ctx.dibuilder.create_auto_variable(
            ctx.discope,
            &self.var.name,
            ctx.diunit.get_file(),
            self.var.range.start.line as u32,
            debug_type.unwrap(),
            true,
            DIFlags::PUBLIC,
            debug_type.unwrap().get_align_in_bits(),
        );
        ctx.build_dbg_location(self.var.range.start);
        ctx.dibuilder.insert_declare_at_end(
            ptr2value,
            Some(debug_var_info),
            None,
            ctx.builder.get_current_debug_location().unwrap(),
            ctx.function.unwrap().get_first_basic_block().unwrap(),
        );
        let re = ctx.add_symbol(
            self.var.name.clone(),
            ptr2value,
            pltype_name.clone(),
            self.var.range,
        );
        if re.is_err() {
            return Err(re.unwrap_err());
        }
        ctx.builder.build_store(ptr2value, base_value);
        return Ok((Value::None, None, TerminatorEnum::NONE));
    }
}
#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct AssignNode {
    pub var: Box<NodeEnum>,
    pub exp: Box<NodeEnum>,
}
impl Node for AssignNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut format_res = String::new();
        format_res.push_str(&self.var.format(tabs, prefix));
        format_res.push_str(" = ");
        format_res.push_str(&self.exp.format(tabs, prefix));
        format_res
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("AssignNode");
        self.var.print(tabs + 1, false, line.clone());
        self.exp.print(tabs + 1, true, line.clone());
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let vrange = self.var.range();
        let (ptr, _, _) = self.var.emit(ctx)?;
        let (value, _, _) = self.exp.emit(ctx)?;
        let ptr = ctx.try_load2ptr(ptr);
        if let Value::VarValue(ptr) = ptr {
            let load = ctx.try_load2var(value);
            if ptr.get_type().get_element_type()
                != load.as_basic_value_enum().get_type().as_any_type_enum()
            {
                return Err(ctx.add_err(self.range, ErrorCode::ASSIGN_TYPE_MISMATCH));
            }
            ctx.builder.build_store(ptr, load.as_basic_value_enum());
            return Ok((Value::None, None, TerminatorEnum::NONE));
        }
        Err(ctx.add_err(vrange, ErrorCode::NOT_ASSIGNABLE))
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct EmptyNode {}

impl Node for EmptyNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        return "hello".to_string();
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("EmptyNode");
    }
    fn emit<'a, 'ctx>(&'a mut self, _: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        Ok((Value::None, None, TerminatorEnum::NONE))
    }
}

#[range]
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct StatementsNode {
    pub statements: Vec<Box<NodeEnum>>,
}
impl Node for StatementsNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        let mut i = self.statements.len();
        let mut format_res = String::new();
        for statement in &self.statements {
            i -= 1;
            format_res.push_str("\n\r");
            format_res.push_str(&prefix.repeat(tabs));
            format_res.push_str(&statement.format(tabs, prefix));
        }
        return format_res;
    }

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
        Ok((Value::None, None, terminator))
    }
}

impl StatementsNode {
    pub fn emit_child<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let child = &mut ctx.new_child(self.range.start);
        self.emit(child)
    }
}
