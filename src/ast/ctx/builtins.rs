use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::ast::{node::{function::FuncCallNode, node_result::NodeResult, string_literal::StringNode, Node, TypeNode}, builder::{BuilderEnum, ValueHandle}, range::Range};

use super::Ctx;


type EmitFn = for <'a, 'b> fn(
    &mut FuncCallNode,
    &'b mut Ctx<'a>,
    &'b BuilderEnum<'a, '_>,
) -> NodeResult;

lazy_static! {
    pub static ref BUILTIN_FN_MAP: HashMap<ValueHandle, EmitFn> = {
        let mut mp = HashMap::<ValueHandle, EmitFn>::new();
        mp.insert(usize::MAX -1, emit_name_of);
        mp
    };
    pub static ref BUILTIN_FN_NAME_MAP: HashMap<&'static str, ValueHandle> = {
        let mut mp = HashMap::new();
        mp.insert("fullnameof",usize::MAX -1);
        mp
    };
}


fn emit_name_of<'a, 'b>(
    f:&mut FuncCallNode,
    ctx:&'b mut Ctx<'a>,
    builder:&'b BuilderEnum<'a, '_>,
) -> NodeResult {
    if f.paralist.len() != 0 {
        return Err(f.range.new_err(crate::ast::diag::ErrorCode::PARAMETER_LENGTH_NOT_MATCH).add_to_ctx(ctx));
    }
    if f.generic_params.is_none() {
        return Err(f.range.new_err(crate::ast::diag::ErrorCode::GENERIC_NOT_FOUND).add_to_ctx(ctx));
    }
    let generic = f.generic_params.as_ref().unwrap();
    generic.emit_highlight(ctx);
    if generic.generics.len() != 1 {
        return Err(f.range.new_err(crate::ast::diag::ErrorCode::GENERIC_NOT_FOUND).add_to_ctx(ctx));
    }
    if let Some(g) = &generic.generics[0] {
        let tp = g.get_type(ctx, builder, false)?;
        let s = tp.borrow().get_full_elm_name();
        let mut sn = StringNode{
            content:s,
            range:Range::default(),
        };
        return sn.emit(ctx, builder);
    }else {
        return Err(generic.range.new_err(crate::ast::diag::ErrorCode::GENERIC_NOT_FOUND).add_to_ctx(ctx));
    }
}