use super::*;
use crate::ast::ctx::{init_arr, Ctx, PLType, PriType};
use crate::ast::diag::ErrorCode;

use internal_macro::range;
use lsp_types::SemanticTokenType;

#[range]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct BoolConstNode {
    pub value: bool,
}

impl Node for BoolConstNode {
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        tab(tabs, line, end);
        println!("BoolConstNode: {}", self.value);
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        ctx.push_semantic_token(self.range, SemanticTokenType::KEYWORD, 0);
        Ok((
            Value::BoolValue(ctx.context.i8_type().const_int(self.value as u64, true)),
            Some(PLType::PRIMITIVE(PriType::try_from_str("bool").unwrap())),
            TerminatorEnum::NONE,
            true,
        ))
    }
}

#[range]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct NumNode {
    pub value: Num,
}
impl Node for NumNode {
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
                Value::IntValue(b),
                Some(PLType::PRIMITIVE(PriType::try_from_str("i64").unwrap())),
                TerminatorEnum::NONE,
                true,
            ));
        } else if let Num::FLOAT(x) = self.value {
            let b = ctx.context.f64_type().const_float(x);
            return Ok((
                Value::FloatValue(b),
                Some(PLType::PRIMITIVE(PriType::try_from_str("f64").unwrap())),
                TerminatorEnum::NONE,
                true,
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
                Value::VarValue(v.clone()),
                Some(pltype),
                TerminatorEnum::NONE,
                is_const,
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
                        Value::FnValue(f.get_value(ctx, &ctx.plmod)),
                        Some(tp.clone()),
                        TerminatorEnum::NONE,
                        true,
                    ));
                }
                PLType::STRUCT(s) => {
                    ctx.push_semantic_token(self.range, SemanticTokenType::STRUCT, 0);
                    return Ok((
                        Value::STValue(s.struct_type(ctx)),
                        Some(tp.clone()),
                        TerminatorEnum::NONE,
                        true,
                    ));
                }
                PLType::PRIMITIVE(_) => {
                    ctx.push_semantic_token(self.range, SemanticTokenType::TYPE, 0);
                    return Ok((Value::None, Some(tp.clone()), TerminatorEnum::NONE, true));
                }
                PLType::VOID => {
                    ctx.push_semantic_token(self.range, SemanticTokenType::TYPE, 0);
                    return Ok((Value::None, Some(tp.clone()), TerminatorEnum::NONE, true));
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
    fn print(&self, tabs: usize, end: bool, mut line: Vec<bool>) {
        deal_line(tabs, &mut line, end);
        self.arr.print(tabs + 1, false, line.clone());
        self.index.print(tabs + 1, true, line);
    }
    fn emit<'a, 'ctx>(&'a mut self, ctx: &mut Ctx<'a, 'ctx>) -> NodeResult<'ctx> {
        let (v, tp, _, is_const) = self.arr.emit(ctx)?;
        let v = ctx.try_load2ptr(v);
        let (arrtp, arr) = if let Value::VarValue(v) = v {
            if let PLType::ARR(arrtp) = tp.unwrap() {
                (arrtp, v)
            } else {
                return Err(ctx.add_err(self.range, ErrorCode::CANNOT_INDEX_NON_ARRAY));
            }
        } else if let Value::LoadValue(v) = v {
            if v.is_array_value() {
                let (index, _, _, _) = self.index.emit(ctx)?;
                let index = ctx.try_load2var(index);
                if !index.as_basic_value_enum().is_int_value() {
                    return Err(ctx.add_err(self.range, ErrorCode::ARRAY_INDEX_MUST_BE_INT));
                }
                let index_value = index.as_basic_value_enum().into_int_value();
                let ind = index_value.get_zero_extended_constant().unwrap();
                let elem = ctx
                    .builder
                    .build_extract_value(v.into_array_value(), ind as u32, "arr")
                    .unwrap();
                let elemptr = alloc(ctx, elem.get_type(), "elemptr");
                if elemptr.get_type().get_element_type().is_array_type() {
                    init_arr(elemptr, ctx);
                }
                ctx.builder.build_store(elemptr, elem);
                if let PLType::ARR(arrtp) = tp.unwrap() {
                    return Ok((
                        Value::LoadValue(elemptr.as_basic_value_enum()),
                        Some(*arrtp.get_elem_type()),
                        TerminatorEnum::NONE,
                        is_const,
                    ));
                } else {
                    return Err(ctx.add_err(self.range, ErrorCode::CANNOT_INDEX_NON_ARRAY));
                }
            } else {
                return Err(ctx.add_err(self.range, ErrorCode::CANNOT_INDEX_NON_ARRAY));
            }
        } else {
            return Err(ctx.add_err(self.range, ErrorCode::CANNOT_INDEX_NON_ARRAY));
        };
        // TODO: check if index is out of bounds
        let (index, _, _, _) = self.index.emit(ctx)?;
        let index = ctx.try_load2var(index);
        if !index.as_basic_value_enum().is_int_value() {
            return Err(ctx.add_err(self.range, ErrorCode::ARRAY_INDEX_MUST_BE_INT));
        }
        let index_value = index.as_basic_value_enum().into_int_value();
        let arr = ctx
            .try_load2ptr(Value::ArrValue(arr))
            .as_basic_value_enum()
            .into_pointer_value();
        let elemptr = unsafe {
            let index = &[ctx.context.i64_type().const_int(0, false), index_value];
            ctx.builder.build_in_bounds_gep(arr, index, "element_ptr")
        };
        return Ok((
            Value::VarValue(elemptr),
            Some(*arrtp.element_type),
            TerminatorEnum::NONE,
            is_const,
        ));
    }
}
