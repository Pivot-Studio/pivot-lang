use super::*;
use crate::ast::ctx::{Ctx, PLType, PriType};
use crate::ast::diag::ErrorCode;
use internal_macro::range;
use lsp_types::SemanticTokenType;

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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.range, SemanticTokenType::KEYWORD, 0);
        Ok((
            Some(
                ctx.context
                    .i8_type()
                    .const_int(self.value as u64, true)
                    .into(),
            ),
            Some(PLType::PRIMITIVE(PriType::BOOL)),
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.range, SemanticTokenType::NUMBER, 0);
        if let Num::INT(x) = self.value {
            let b = ctx.context.i64_type().const_int(x, true);
            return Ok((
                Some(b.into()),
                Some(PLType::PRIMITIVE(PriType::I64)),
                TerminatorEnum::NONE,
            ));
        } else if let Num::FLOAT(x) = self.value {
            let b = ctx.context.f64_type().const_float(x);
            return Ok((
                Some(b.into()),
                Some(PLType::PRIMITIVE(PriType::F64)),
                TerminatorEnum::NONE,
            ));
        }
        panic!("not implemented")
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
    pub fn get_type<'a, 'ctx>(&'a self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.if_completion(|ctx, a| {
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
            match tp {
                PLType::FN(f) => {
                    ctx.push_semantic_token(self.range, SemanticTokenType::FUNCTION, 0);
                    return Ok((
                        Some(f.get_or_insert_fn(ctx).into()),
                        Some(tp.clone()),
                        TerminatorEnum::NONE,
                    ));
                }
                PLType::STRUCT(_) => {
                    ctx.push_semantic_token(self.range, SemanticTokenType::STRUCT, 0);
                    return Ok((None, Some(tp.clone()), TerminatorEnum::NONE));
                }
                PLType::PRIMITIVE(_) => {
                    ctx.push_semantic_token(self.range, SemanticTokenType::TYPE, 0);
                    return Ok((None, Some(tp.clone()), TerminatorEnum::NONE));
                }
                PLType::VOID => {
                    ctx.push_semantic_token(self.range, SemanticTokenType::TYPE, 0);
                    return Ok((None, Some(tp.clone()), TerminatorEnum::NONE));
                }
                _ => return Err(ctx.add_err(self.range, ErrorCode::VAR_NOT_FOUND)),
            }
        }
        Err(ctx.add_err(self.range, ErrorCode::VAR_NOT_FOUND))
    }
}

#[range]
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let (arr, pltype, _) = self.arr.emit(ctx)?;
        if let PLType::ARR(arrtp) = pltype.unwrap() {
            let arr = arr.unwrap();
            // TODO: check if index is out of bounds
            let index_range = self.index.range();
            let (index, index_pltype, _) = self.index.emit(ctx)?;
            let index = ctx.try_load2var(index_range, index.unwrap())?;
            if index_pltype.is_none() || !index_pltype.unwrap().is(PriType::I64) {
                return Err(ctx.add_err(self.range, ErrorCode::ARRAY_INDEX_MUST_BE_INT));
            }
            let index_value = index.as_basic_value_enum().into_int_value();
            let arr = arr.into_pointer_value();
            let elemptr = unsafe {
                let index = &[ctx.context.i64_type().const_int(0, false), index_value];
                ctx.builder.build_in_bounds_gep(arr, index, "element_ptr")
            };
            return Ok((
                Some(elemptr.into()),
                Some(*arrtp.element_type),
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
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        self.node.emit(ctx)
    }
}
