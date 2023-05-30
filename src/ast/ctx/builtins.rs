use std::{collections::HashMap, cell::RefCell, sync::Arc};
use crate::ast::node::RangeTrait;

use lazy_static::lazy_static;

use crate::ast::{node::{function::FuncCallNode, node_result::NodeResult, string_literal::StringNode, Node, TypeNode, NodeEnum}, builder::{BuilderEnum, ValueHandle, IRBuilder}, range::Range, pltype::{PLType, get_type_deep, PriType}};

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
        mp.insert(usize::MAX -2, emit_for_fields);
        mp
    };
    pub static ref BUILTIN_FN_NAME_MAP: HashMap<&'static str, ValueHandle> = {
        let mut mp = HashMap::new();
        mp.insert("fullnameof",usize::MAX -1);
        mp.insert("forfields",usize::MAX -2);
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

fn emit_for_fields<'a, 'b>(
    f:&mut FuncCallNode,
    ctx:&'b mut Ctx<'a>,
    builder:&'b BuilderEnum<'a, '_>,
) -> NodeResult {
    if f.paralist.len() != 2 {
        return Err(f.range.new_err(crate::ast::diag::ErrorCode::PARAMETER_LENGTH_NOT_MATCH).add_to_ctx(ctx));
    }
    if !f.generic_params.is_none() {
        return Err(f.range.new_err(crate::ast::diag::ErrorCode::GENERIC_PARAM_LEN_MISMATCH).add_to_ctx(ctx));
    }
    // let generic = f.generic_params.as_ref().unwrap();
    // generic.emit_highlight(ctx);
    // if generic.generics.len() != 1 {
    //     return Err(f.range.new_err(crate::ast::diag::ErrorCode::GENERIC_NOT_FOUND).add_to_ctx(ctx));
    // }
    let st = f.paralist[0].emit(ctx, builder)?;
    let v = st.get_value();

    if let Some(g) = &v {
        let tp = g.get_ty();
        let v = g.get_value();
        let (tp,mut v) = ctx.auto_deref(tp, v, builder);
        let tp = get_type_deep(tp);
        let stp = match &*tp.borrow() {
            PLType::Struct(_) => Some(tp.clone()),
            _ => None,
        };
        if let Some(s) = stp {
            if let PLType::Struct(sttp) = &*s.borrow() {
                for (name,field) in sttp.fields.iter() {
                    let ctx = &mut ctx.new_child(f.paralist[1].range().start, builder);
                    if !builder.is_ptr(v) {
                        v = builder.alloc("temp", &s.borrow(), ctx, None);
                    }
                    let gep = builder.build_struct_gep(v, field.index, "tmp_gep").unwrap();
                    let field_tp = field.typenode.get_type(ctx, builder, true)?;
                    // let tp  = Arc::new(RefCell::new(PLType::Pointer(field_tp)));
                    // let field_v = builder.alloc("field_alloca", &tp.borrow(), ctx, None);
                    // builder.build_store(field_v, gep);
                    ctx.add_symbol_without_check("field_ptr".to_string(), gep, field_tp, Default::default(), false)?;
                    let mut sn = StringNode{
                        content:name.clone(),
                        range:Range::default(),
                    };
                    let re = sn.emit(ctx, builder)?.get_value().unwrap();
                    let sv = re.get_value();
                    ctx.add_symbol_without_check("field".to_string(), sv, re.get_ty(), Default::default(), false)?;
                    if let NodeEnum::Sts(p) = & mut *f.paralist[1] {
                        p.emit(ctx, builder)?;
                    }
                }
                return Ok(Default::default());
            }else {
                unreachable!()
            }
        }else {
            return Ok(Default::default());
        }
    }else {
        return Err(f.range.new_err(crate::ast::diag::ErrorCode::PARAMETER_TYPE_NOT_MATCH).add_to_ctx(ctx));
    }
}