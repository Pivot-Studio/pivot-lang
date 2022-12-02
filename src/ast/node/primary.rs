use super::*;
use crate::ast::builder::{IRBuilder, VarBuilder, NumBuilder};
use crate::ast::builder::llvmbuilder::PLLLVMBuilder;
use crate::ast::ctx::Ctx;
use crate::ast::diag::ErrorCode;
use crate::ast::pltype::{PLType, PriType};
use internal_macro::{comments, range};
use lsp_types::SemanticTokenType;

#[range]
#[comments]
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PrimaryNode {
    pub value: Box<NodeEnum>,
}

impl Node for PrimaryNode {
    fn format(&self, _tabs: usize, _prefix: &str) -> String {
        return self.value.format(_tabs, _prefix);
    }
    fn print(&self, tabs: usize, end: bool, line: Vec<bool>) {
        self.value.print(tabs, end, line);
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.emit_comment_highlight(&self.comments[0]);
        let res = self.value.emit(ctx);
        ctx.emit_comment_highlight(&self.comments[1]);
        res
    }
}

#[range]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct BoolConstNode {
    pub value: bool,
}

impl Node for BoolConstNode {
    fn format(&self, _tabs: usize, _prefix: &str) -> String {
        return self.value.to_string();
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("BoolConstNode: {}", self.value);
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.range, SemanticTokenType::KEYWORD, 0);
        Ok((
            Some(
                ctx.context
                    .i8_type()
                    .const_int(self.value as u64, true)
                    .into(),
            ),
            Some(Rc::new(RefCell::new(PLType::PRIMITIVE(PriType::BOOL)))),
            TerminatorEnum::NONE,
        ))
    }
}

#[range]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct NumNode {
    pub value: Num,
}
impl Node for NumNode {
    fn format(&self, _tabs: usize, _prefix: &str) -> String {
        if let Num::INT(x) = self.value {
            return x.to_string();
        } else if let Num::FLOAT(x) = self.value {
            return x.to_string();
        }
        panic!("not implemented")
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("NumNode: {:?}", self.value);
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.range, SemanticTokenType::NUMBER, 0);
        if let Num::INT(x) = self.value {
            let b = ctx.context.i64_type().const_int(x, true);
            return Ok((
                Some(b.into()),
                Some(Rc::new(RefCell::new(PLType::PRIMITIVE(PriType::I64)))),
                TerminatorEnum::NONE,
            ));
        } else if let Num::FLOAT(x) = self.value {
            let b = ctx.context.f64_type().const_float(x);
            return Ok((
                Some(b.into()),
                Some(Rc::new(RefCell::new(PLType::PRIMITIVE(PriType::F64)))),
                TerminatorEnum::NONE,
            ));
        }
        panic!("not implemented")
    }
}

impl <'a, 'ctx>NumBuilder<'a, 'ctx> for PLLLVMBuilder<'a, 'ctx> {
    fn build_num(&mut self, ctx: &mut Ctx<'a, 'ctx>, node: & NumNode)  -> NodeResult<'ctx>  {
        match node.value {
            Num::INT(x) => {
                let b = self.context.i64_type().const_int(x, true);
                return Ok((
                    Some(b.into()),
                    Some(Rc::new(RefCell::new(PLType::PRIMITIVE(PriType::I64)))),
                    TerminatorEnum::NONE,
                ));
            }
            Num::FLOAT(x) => {
                let b = self.context.f64_type().const_float(x);
                return Ok((
                    Some(b.into()),
                    Some(Rc::new(RefCell::new(PLType::PRIMITIVE(PriType::F64)))),
                    TerminatorEnum::NONE,
                ));
            }
        }
    }
}

#[range]
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarNode {
    pub name: String,
}
impl VarNode {
    pub fn format(&self, _tabs: usize, _prefix: &str) -> String {
        let name = &self.name;
        return name.to_string();
    }
    pub fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("VarNode: {}", self.name);
    }
    pub fn emit<'a, 'ctx>(&'a self, ctx: &Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.if_completion_no_mut(|ctx, a| {
            if a.0.is_in(self.range) {
                let completions = ctx.get_completions();
                ctx.completion_items.set(completions);
            }
        });
        let v = ctx.get_symbol(&self.name);
        if let Some((v, pltype, dst, refs, is_const)) = v {
            ctx.push_semantic_token(self.range, SemanticTokenType::VARIABLE, 0);
            let o = Ok((
                Some({
                    let mut res: PLValue = v.into();
                    res.set_const(is_const);
                    res
                }),
                Some(pltype),
                TerminatorEnum::NONE,
            ));
            ctx.send_if_go_to_def(self.range, dst, ctx.plmod.path.clone());
            ctx.set_if_refs(refs, self.range);
            return o;
        }
        if let Ok(tp) = ctx.get_type(&self.name, self.range) {
            match &*tp.borrow() {
                PLType::FN(_) => {
                    ctx.push_semantic_token(self.range, SemanticTokenType::FUNCTION, 0);
                    return Ok((None, Some(tp.clone()), TerminatorEnum::NONE));
                }
                _ => return Err(ctx.add_err(self.range, ErrorCode::VAR_NOT_FOUND)),
            }
        }
        Err(ctx.add_err(self.range, ErrorCode::VAR_NOT_FOUND))
    }
    pub fn get_type<'a, 'ctx>(&'a self, ctx: &Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.if_completion_no_mut(|ctx, a| {
            if a.0.is_in(self.range) {
                let completions = ctx.get_completions();
                ctx.completion_items.set(completions);
            }
        });

        if let Ok(tp) = ctx.get_type(&self.name, self.range) {
            match *tp.borrow() {
                PLType::STRUCT(_)
                | PLType::PRIMITIVE(_)
                | PLType::VOID
                | PLType::GENERIC(_)
                | PLType::PLACEHOLDER(_) => {
                    if let PLType::STRUCT(st) = &*tp.clone().borrow() {
                        ctx.send_if_go_to_def(self.range, st.range, ctx.plmod.path.clone());
                        ctx.set_if_refs(st.refs.clone(), self.range);
                    }
                    return Ok((None, Some(tp.clone()), TerminatorEnum::NONE));
                }
                _ => return Err(ctx.add_err(self.range, ErrorCode::UNDEFINED_TYPE)),
            }
        }
        Err(ctx.add_err(self.range, ErrorCode::UNDEFINED_TYPE))
    }
}

impl <'a, 'ctx>VarBuilder<'a, 'ctx> for PLLLVMBuilder<'a, 'ctx> {
    fn build_var(&mut self, ctx: &mut Ctx<'a, 'ctx>, node: & VarNode)  -> NodeResult<'ctx>  {
        let v = ctx.get_symbol(&node.name);
        if let Some((v, pltype, _, _, is_const)) = v {
            let o = Ok((
                Some({
                    let mut res: PLValue = v.into();
                    res.set_const(is_const);
                    res
                }),
                Some(pltype),
                TerminatorEnum::NONE,
            ));
            return o;
        }
        if let Ok(tp) = ctx.get_type(&node.name, node.range) {
            match &*tp.borrow() {
                PLType::FN(_) => {
                    return Ok((None, Some(tp.clone()), TerminatorEnum::NONE));
                }
                _ => return Err(ctx.add_err(node.range, ErrorCode::VAR_NOT_FOUND)),
            }
        }
        Err(ctx.add_err(node.range, ErrorCode::VAR_NOT_FOUND))
    }
}


#[range]
#[comments]
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ArrayElementNode {
    pub arr: Box<NodeEnum>,
    pub index: Box<NodeEnum>,
}

impl Node for ArrayElementNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        format!(
            "{}[{}]",
            &self.arr.format(tabs, prefix),
            &self.index.format(tabs, prefix)
        )
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ArrayElementNode");
        self.arr.print(tabs + 1, false, line.clone());
        self.index.print(tabs + 1, true, line);
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let (arr, pltype, _) = self.arr.emit(ctx)?;
        if let PLType::ARR(arrtp) = &*pltype.unwrap().borrow() {
            let arr = arr.unwrap();
            // TODO: check if index is out of bounds
            let index_range = self.index.range();
            let (index, index_pltype, _) = self.index.emit(ctx)?;
            let index = ctx.try_load2var(index_range, index.unwrap())?;
            if index_pltype.is_none() || !index_pltype.unwrap().borrow().is(&PriType::I64) {
                return Err(ctx.add_err(self.range, ErrorCode::ARRAY_INDEX_MUST_BE_INT));
            }
            let index_value = index.as_basic_value_enum().into_int_value();
            let arr = arr.into_pointer_value();
            let elemptr = unsafe {
                let index = &[ctx.context.i64_type().const_int(0, false), index_value];
                ctx.builder.build_in_bounds_gep(arr, index, "element_ptr")
            };
            ctx.emit_comment_highlight(&self.comments[0]);
            return Ok((
                Some(elemptr.into()),
                Some(arrtp.element_type.clone()),
                TerminatorEnum::NONE,
            ));
        }
        return Err(ctx.add_err(self.range, ErrorCode::CANNOT_INDEX_NON_ARRAY));
    }
}

#[range]
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParanthesesNode {
    pub node: Box<NodeEnum>,
}

impl Node for ParanthesesNode {
    fn format(&self, tabs: usize, prefix: &str) -> String {
        format!("({})", &self.node.format(tabs, prefix))
    }
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line.clone(), end);
        println!("ParanthesesNode");
        self.node.print(tabs + 1, true, line);
    }
    fn emit<'a, 'ctx>(&mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        self.node.emit(ctx)
    }
}
