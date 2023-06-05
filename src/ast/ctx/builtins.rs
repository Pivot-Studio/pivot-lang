//! # builtins
//!
//! compiler builtin methods, mostly used for static reflection
use crate::ast::{builder::no_op_builder::NoOpBuilder, node::RangeTrait, pltype::PlaceHolderType};
use std::{cell::RefCell, collections::HashMap, sync::Arc};

use lazy_static::lazy_static;

use crate::ast::{
    builder::{BuilderEnum, IRBuilder, ValueHandle},
    node::{
        function::FuncCallNode, node_result::NodeResult, string_literal::StringNode, Node,
        NodeEnum, TypeNode,
    },
    pltype::{get_type_deep, PLType},
    range::Range,
};

use super::Ctx;

type EmitFn =
    for<'a, 'b> fn(&mut FuncCallNode, &'b mut Ctx<'a>, &'b BuilderEnum<'a, '_>) -> NodeResult;

lazy_static! {
    pub static ref BUILTIN_FN_MAP: HashMap<ValueHandle, EmitFn> = {
        let mut mp = HashMap::<ValueHandle, EmitFn>::new();
        mp.insert(usize::MAX - 1, emit_full_name_of);
        mp.insert(usize::MAX - 2, emit_for_fields);
        mp.insert(usize::MAX - 3, emit_match_type);
        mp.insert(usize::MAX - 4, emit_name_of);
        mp
    };
    pub static ref BUILTIN_FN_SNIPPET_MAP: HashMap<ValueHandle, String> = {
        let mut mp = HashMap::<ValueHandle, String>::new();
        mp.insert(usize::MAX - 1, r#"fullnameof(${1:t})$0"#.to_owned());
        mp.insert(
            usize::MAX - 2,
            r#"forfields(${1:t}, { ${2:_field} })$0"#.to_owned(),
        );
        mp.insert(
            usize::MAX - 3,
            r#"match_type<${1:T}>(${2:t}, { ${3:_value} })$0"#.to_owned(),
        );
        mp.insert(usize::MAX - 4, r#"fullnameof(${1:t})$0"#.to_owned());
        mp
    };
    pub static ref BUILTIN_FN_NAME_MAP: HashMap<&'static str, ValueHandle> = {
        let mut mp = HashMap::new();
        mp.insert("fullnameof", usize::MAX - 1);
        mp.insert("forfields", usize::MAX - 2);
        mp.insert("match_type", usize::MAX - 3);
        mp.insert("nameof", usize::MAX - 4);
        mp
    };
}

fn emit_name_of<'a, 'b>(
    f: &mut FuncCallNode,
    ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, '_>,
) -> NodeResult {
    if !f.paralist.is_empty() {
        return Err(f
            .range
            .new_err(crate::ast::diag::ErrorCode::PARAMETER_LENGTH_NOT_MATCH)
            .add_to_ctx(ctx));
    }
    if f.generic_params.is_none() {
        return Err(f
            .range
            .new_err(crate::ast::diag::ErrorCode::GENERIC_NOT_FOUND)
            .add_to_ctx(ctx));
    }
    let generic = f.generic_params.as_ref().unwrap();
    generic.emit_highlight(ctx);
    if generic.generics.len() != 1 {
        return Err(f
            .range
            .new_err(crate::ast::diag::ErrorCode::GENERIC_NOT_FOUND)
            .add_to_ctx(ctx));
    }
    if let Some(g) = &generic.generics[0] {
        let tp = g.get_type(ctx, builder, false)?;
        let s = tp.borrow().get_name();
        let mut sn = StringNode {
            content: s,
            range: Range::default(),
        };
        sn.emit(ctx, builder)
    } else {
        Err(generic
            .range
            .new_err(crate::ast::diag::ErrorCode::GENERIC_NOT_FOUND)
            .add_to_ctx(ctx))
    }
}

fn emit_full_name_of<'a, 'b>(
    f: &mut FuncCallNode,
    ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, '_>,
) -> NodeResult {
    if !f.paralist.is_empty() {
        return Err(f
            .range
            .new_err(crate::ast::diag::ErrorCode::PARAMETER_LENGTH_NOT_MATCH)
            .add_to_ctx(ctx));
    }
    if f.generic_params.is_none() {
        return Err(f
            .range
            .new_err(crate::ast::diag::ErrorCode::GENERIC_NOT_FOUND)
            .add_to_ctx(ctx));
    }
    let generic = f.generic_params.as_ref().unwrap();
    generic.emit_highlight(ctx);
    if generic.generics.len() != 1 {
        return Err(f
            .range
            .new_err(crate::ast::diag::ErrorCode::GENERIC_NOT_FOUND)
            .add_to_ctx(ctx));
    }
    if let Some(g) = &generic.generics[0] {
        let tp = g.get_type(ctx, builder, false)?;
        let s = tp.borrow().get_full_elm_name();
        let mut sn = StringNode {
            content: s,
            range: Range::default(),
        };
        sn.emit(ctx, builder)
    } else {
        Err(generic
            .range
            .new_err(crate::ast::diag::ErrorCode::GENERIC_NOT_FOUND)
            .add_to_ctx(ctx))
    }
}

fn emit_for_fields<'a, 'b>(
    f: &mut FuncCallNode,
    ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, '_>,
) -> NodeResult {
    if f.paralist.len() != 2 {
        return Err(f
            .range
            .new_err(crate::ast::diag::ErrorCode::PARAMETER_LENGTH_NOT_MATCH)
            .add_to_ctx(ctx));
    }
    if f.generic_params.is_some() {
        return Err(f
            .range
            .new_err(crate::ast::diag::ErrorCode::GENERIC_PARAM_LEN_MISMATCH)
            .add_to_ctx(ctx));
    }
    let st = f.paralist[0].emit(ctx, builder)?;
    let v = st.get_value();

    if let Some(g) = &v {
        let tp = g.get_ty();
        let tp = get_type_deep(tp);
        let v = g.get_value();
        let (tp, v) = ctx.auto_deref(tp, v, builder);
        let tp = get_type_deep(tp);
        let (tp, mut v) = ctx.auto_deref(tp, v, builder);
        let mut code_gen = false;
        let stp = match &*tp.borrow() {
            PLType::Struct(s) => {
                code_gen = !s.fields.is_empty();
                Some(tp.clone())
            }
            _ => None,
        };
        let mut builder = builder;
        let noop = BuilderEnum::NoOp(NoOpBuilder::default());
        // 处理不是结构体类型或者没有有效字段的情况
        if !code_gen {
            // 这种情况不会生成对应代码，但是需要进行相应的语法分析
            builder = &noop;
            ctx.run_in_origin_mod(|ctx| {
                let field_tp = Arc::new(RefCell::new(PLType::PlaceHolder(PlaceHolderType {
                    name: "T".to_owned(),
                    range: Default::default(),
                })));
                let gep = builder.alloc("placeholder", &field_tp.borrow(), ctx, None);
                ctx.add_symbol_raw("_field".to_string(), gep, field_tp, f.range);
                let mut sn = StringNode {
                    content: "field_T".to_owned(),
                    range: Range::default(),
                };
                let re = sn.emit(ctx, builder)?.get_value().unwrap();
                let sv = re.get_value();
                ctx.add_symbol_raw("_field_name".to_string(), sv, re.get_ty(), f.range);
                if let NodeEnum::Sts(p) = &mut *f.paralist[1] {
                    p.emit(ctx, builder)?;
                }
                Ok(())
            })?;
            return Ok(Default::default());
        }

        if let Some(s) = stp {
            if let PLType::Struct(sttp) = &*s.borrow() {
                for (name, field) in sttp.fields.iter() {
                    let ctx = &mut ctx.new_child(f.paralist[1].range().start, builder);
                    if !builder.is_ptr(v) {
                        v = builder.alloc("temp", &s.borrow(), ctx, None);
                    }
                    let gep = builder.build_struct_gep(v, field.index, "tmp_gep").unwrap();
                    ctx.run_in_origin_mod(|ctx| {
                        ctx.run_in_type_mod(sttp, |ctx, _| {
                            let field_tp = field.typenode.get_type(ctx, builder, true)?;
                            ctx.add_symbol_raw("_field".to_string(), gep, field_tp, f.range);
                            let mut sn = StringNode {
                                content: name.clone(),
                                range: Range::default(),
                            };
                            let re = sn.emit(ctx, builder)?.get_value().unwrap();
                            let sv = re.get_value();
                            ctx.add_symbol_raw("_field_name".to_string(), sv, re.get_ty(), f.range);
                            Ok(())
                        })?;
                        if let NodeEnum::Sts(p) = &mut *f.paralist[1] {
                            p.emit(ctx, builder)?;
                        }
                        Ok(())
                    })?;
                }
                Ok(Default::default())
            } else {
                unreachable!()
            }
        } else {
            Ok(Default::default())
        }
    } else {
        unreachable!()
    }
}

fn emit_match_type<'a, 'b>(
    f: &mut FuncCallNode,
    ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, '_>,
) -> NodeResult {
    if f.paralist.len() != 2 {
        return Err(f
            .range
            .new_err(crate::ast::diag::ErrorCode::PARAMETER_LENGTH_NOT_MATCH)
            .add_to_ctx(ctx));
    }
    if f.generic_params.is_none() {
        return Err(f
            .range
            .new_err(crate::ast::diag::ErrorCode::GENERIC_NOT_FOUND)
            .add_to_ctx(ctx));
    }
    let generic = f.generic_params.as_ref().unwrap();
    generic.emit_highlight(ctx);
    if generic.generics.len() != 1 {
        return Err(f
            .range
            .new_err(crate::ast::diag::ErrorCode::GENERIC_NOT_FOUND)
            .add_to_ctx(ctx));
    }
    let st = f.paralist[0].emit(ctx, builder)?;
    let v = st.get_value();
    if generic.generics[0].is_none() {
        return Err(f
            .range
            .new_err(crate::ast::diag::ErrorCode::GENERIC_NOT_FOUND)
            .add_to_ctx(ctx));
    }
    let generic = generic.generics[0]
        .as_ref()
        .unwrap()
        .get_type(ctx, builder, true)?;
    let mut builder = builder;
    let noop = BuilderEnum::NoOp(NoOpBuilder::default());
    if let Some(g) = &v {
        let tp = g.get_ty();
        let v = g.get_value();
        let (tp, mut v) = ctx.auto_deref(tp, v, builder);
        let tp = get_type_deep(tp);
        if tp != generic {
            // 类型不匹配
            // 虽然不需要生成之后的代码，但是为了提供更好的语法支持，我们假装它匹配并进行分析
            // 此处将v替换为正确类型的变量
            builder = &noop;
            v = builder.alloc("placeholder", &generic.borrow(), ctx, None);
        }
        let ctx = &mut ctx.new_child(f.paralist[1].range().start, builder);

        ctx.add_symbol_raw("_value".to_string(), v, generic.clone(), f.range);
        if let NodeEnum::Sts(p) = &mut *f.paralist[1] {
            p.emit(ctx, builder)?;
        }
        Ok(Default::default())
    } else {
        Err(f
            .range
            .new_err(crate::ast::diag::ErrorCode::PARAMETER_TYPE_NOT_MATCH)
            .add_to_ctx(ctx))
    }
}