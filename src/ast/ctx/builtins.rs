//! # builtins
//!
//! compiler builtin methods, mostly used for static reflection
use crate::{
    ast::{
        builder::no_op_builder::NoOpBuilder,
        node::{node_result::NodeResultBuilder, RangeTrait},
        pltype::{ARRType, PlaceHolderType, PriType},
    },
    format_label,
};
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
        mp.insert(usize::MAX - 5, emit_unsafe_cast);
        mp.insert(usize::MAX - 6, emit_sizeof);
        mp.insert(usize::MAX - 7, emit_gc_type);
        mp.insert(usize::MAX - 8, emit_arr_len);
        mp.insert(usize::MAX - 9, emit_arr_copy);
        mp.insert(usize::MAX - 10, emit_arr_from_raw);
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
        mp.insert(
            usize::MAX - 5,
            r#"unsafe_cast<${1:T}>(${2:t})$0"#.to_owned(),
        );
        mp.insert(usize::MAX - 6, r#"sizeof<${1:T}>()$0"#.to_owned());
        mp.insert(usize::MAX - 7, r#"gc_type<${1:T}>()$0"#.to_owned());
        mp.insert(usize::MAX - 8, r#"arr_len(${1:t})$0"#.to_owned());
        mp.insert(
            usize::MAX - 9,
            r#"arr_copy(${1:from}, ${2:to}, ${3:len})$0"#.to_owned(),
        );
        mp.insert(
            usize::MAX - 10,
            r"arr_from_raw(${1:ptr}, ${2:len})$0".to_owned(),
        );
        mp
    };
    pub static ref BUILTIN_FN_NAME_MAP: HashMap<&'static str, ValueHandle> = {
        let mut mp = HashMap::new();
        mp.insert("fullnameof", usize::MAX - 1);
        mp.insert("forfields", usize::MAX - 2);
        mp.insert("match_type", usize::MAX - 3);
        mp.insert("nameof", usize::MAX - 4);
        mp.insert("unsafe_cast", usize::MAX - 5);
        mp.insert("sizeof", usize::MAX - 6);
        mp.insert("gc_type", usize::MAX - 7);
        mp.insert("arr_len", usize::MAX - 8);
        mp.insert("arr_copy", usize::MAX - 9);
        mp.insert("arr_from_raw", usize::MAX - 10);
        mp
    };
}

fn emit_sizeof<'a, 'b>(
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
    let size = builder.sizeof(&generic.borrow(), ctx);
    let b = builder.int_value(&PriType::I64, size, true);
    return b
        .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::I64))))
        .set_const()
        .to_result();
}

fn emit_gc_type<'a, 'b>(
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
    #[cfg(feature = "llvm")]
    let size = generic.borrow().get_immix_type();
    #[cfg(feature = "llvm")]
    let b = builder.int_value(&PriType::U8, size as _, true);
    #[cfg(not(feature = "llvm"))]
    let b = 0;
    return b
        .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::U8))))
        .set_const()
        .to_result();
}

fn emit_unsafe_cast<'a, 'b>(
    f: &mut FuncCallNode,
    ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, '_>,
) -> NodeResult {
    if f.paralist.len() != 1 {
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
    let v = st.get_value().unwrap();
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
    if !matches!(&*v.get_ty().borrow(), PLType::Pointer(_)) {
        return Err(f.paralist[0]
            .range()
            .new_err(crate::ast::diag::ErrorCode::NOT_A_POINTER)
            .add_to_ctx(ctx));
    }
    let ty = PLType::Pointer(generic);
    let re = builder.bitcast(ctx, v.get_value(), &ty, "unsafe_casted");
    re.new_output(Arc::new(RefCell::new(ty))).to_result()
}

fn emit_arr_from_raw<'a, 'b>(
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
    let v = st.get_value().unwrap();

    let elm = if let PLType::Pointer(p) = &*v.get_ty().borrow() {
        p.clone()
    } else {
        return Err(f.paralist[0]
            .range()
            .new_err(crate::ast::diag::ErrorCode::NOT_A_POINTER)
            .add_to_ctx(ctx));
    };

    let len = f.paralist[1].emit(ctx, builder)?;
    let v2 = len.get_value().unwrap();
    if !matches!(&*v2.get_ty().borrow(), PLType::Primitive(PriType::I64)) {
        return Err(f.paralist[1]
            .range()
            .new_err(crate::ast::diag::ErrorCode::TYPE_MISMATCH)
            .add_to_ctx(ctx));
    }

    let arr_tp = Arc::new(RefCell::new(PLType::Arr(ARRType {
        element_type: elm,
        size_handle: 0,
    })));
    let arr = builder.alloc("array_alloca", &arr_tp.borrow(), ctx, None);
    let arr_raw = builder.build_struct_gep(arr, 1, "arr_raw").unwrap();
    let loaded = ctx.try_load2var(f.paralist[0].range(), v.get_value(), builder)?;
    builder.build_store(arr_raw, loaded);
    let arr_len = builder.build_struct_gep(arr, 2, "arr_len").unwrap();
    let loaded = ctx.try_load2var(f.paralist[1].range(), v2.get_value(), builder)?;
    builder.build_store(arr_len, loaded);

    arr.new_output(arr_tp).to_result()
}
fn emit_arr_len<'a, 'b>(
    f: &mut FuncCallNode,
    ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, '_>,
) -> NodeResult {
    if f.paralist.len() != 1 {
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
    let v = st.get_value().unwrap();

    if !matches!(&*v.get_ty().borrow(), PLType::Arr(_)) {
        return Err(f.paralist[0]
            .range()
            .new_err(crate::ast::diag::ErrorCode::EXPECT_ARRAY_TYPE)
            .add_to_ctx(ctx));
    }
    let len = builder
        .build_struct_gep(v.get_value(), 2, "arr_len")
        .unwrap();
    len.new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::I64))))
        .to_result()
}

fn emit_arr_copy<'a, 'b>(
    f: &mut FuncCallNode,
    ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, '_>,
) -> NodeResult {
    if f.paralist.len() != 3 {
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
    let v = st.get_value().unwrap();

    if !matches!(&*v.get_ty().borrow(), PLType::Arr(_)) {
        return Err(f.paralist[0]
            .range()
            .new_err(crate::ast::diag::ErrorCode::EXPECT_ARRAY_TYPE)
            .add_to_ctx(ctx));
    }

    let st = f.paralist[1].emit(ctx, builder)?;
    let to = st.get_value().unwrap();

    if !matches!(&*to.get_ty().borrow(), PLType::Arr(_)) {
        return Err(f.paralist[1]
            .range()
            .new_err(crate::ast::diag::ErrorCode::EXPECT_ARRAY_TYPE)
            .add_to_ctx(ctx));
    }

    let st = f.paralist[2].emit(ctx, builder)?;
    let len = st.get_value().unwrap();

    if !matches!(&*len.get_ty().borrow(), PLType::Primitive(PriType::I64)) {
        return Err(f.paralist[2]
            .range()
            .new_err(crate::ast::diag::ErrorCode::TYPE_MISMATCH)
            .add_label(
                f.paralist[2].range(),
                ctx.get_file(),
                format_label!("expect i64, found {}", len.get_ty().borrow().get_name()),
            )
            .add_to_ctx(ctx));
    }

    let from_raw = builder
        .build_struct_gep(v.get_value(), 1, "arr_raw")
        .unwrap();
    let to_raw = builder
        .build_struct_gep(to.get_value(), 1, "arr_raw")
        .unwrap();
    let len_raw = len.get_value();
    builder.build_memcpy(from_raw, to_raw, len_raw);
    Ok(Default::default())
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
