//! # builtins
//!
//! compiler builtin methods, mostly used for static reflection
use crate::{
    ast::{
        builder::no_op_builder::NoOpBuilder,
        node::{node_result::NodeResultBuilder, RangeTrait},
        pltype::{ARRType, GenericType, PlaceHolderType, PriType},
    },
    format_label,
};
use std::{cell::RefCell, collections::HashMap, sync::Arc};

use indexmap::IndexMap;
use lazy_static::lazy_static;
use rustc_hash::FxHashSet;
use ustr::Ustr;

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
        mp.insert(usize::MAX - 11, emit_is_ptr);
        mp.insert(usize::MAX - 12, emit_if_arr);
        mp.insert(usize::MAX - 13, emit_if_union);
        mp.insert(usize::MAX - 14, emit_arr_slice);
        mp.insert(usize::MAX - 15, emit_asm_sp);
        mp.insert(usize::MAX - 16, emit_is_struct);
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
        mp.insert(usize::MAX - 11, r#"is_ptr<${1:T}>()$0"#.to_owned());
        mp.insert(
            usize::MAX - 12,
            r#"if_arr(${1:t}, { ${2:_field} })$0"#.to_owned(),
        );
        mp.insert(
            usize::MAX - 13,
            r#"if_union(${1:t}, { ${2:_field} })$0"#.to_owned(),
        );
        mp.insert(
            usize::MAX - 14,
            r#"arr_slice(${1:from}, ${2:start}, ${3:len})$0"#.to_owned(),
        );
        mp.insert(usize::MAX - 15, r#"asm_sp()$0"#.to_owned());
        mp.insert(usize::MAX - 16, r#"is_struct<${1:T}>()$0"#.to_owned());
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
        mp.insert("is_ptr", usize::MAX - 11);
        mp.insert("if_arr", usize::MAX - 12);
        mp.insert("if_union", usize::MAX - 13);
        mp.insert("arr_slice", usize::MAX - 14);
        mp.insert("asm_sp", usize::MAX - 15);
        mp.insert("is_struct", usize::MAX - 16);
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

fn emit_asm_sp<'a, 'b>(
    _f: &mut FuncCallNode,
    _ctx: &'b mut Ctx<'a>,
    builder: &'b BuilderEnum<'a, '_>,
) -> NodeResult {
    builder
        .get_sp_handle()
        .new_output(Arc::new(RefCell::new(PLType::new_i64())))
        .to_result()
}
fn emit_is_ptr<'a, 'b>(
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
    let binding = get_type_deep(generic);
    let binding = binding.borrow();
    match &*binding {
        PLType::Pointer(_) => {
            let b = builder.int_value(&PriType::BOOL, 1, false);
            b.new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                .to_result()
        }
        _ => {
            let b = builder.int_value(&PriType::BOOL, 0, false);
            b.new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                .to_result()
        }
    }
}

fn emit_is_struct<'a, 'b>(
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
    let binding = get_type_deep(generic);
    let binding = binding.borrow();
    match &*binding {
        PLType::Struct(_) => {
            let b = builder.int_value(&PriType::BOOL, 1, false);
            b.new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                .to_result()
        }
        _ => {
            let b = builder.int_value(&PriType::BOOL, 0, false);
            b.new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                .to_result()
        }
    }
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
    let ty = Arc::new(RefCell::new(ty));
    let loaded = builder.try_load2var(Default::default(), v.get_value(), &ty.borrow(), ctx)?;
    let re = builder.bitcast(ctx, loaded, &PLType::Pointer(ty.clone()), "unsafe_casted");
    let alloca = builder.alloc("tmp_cast", &ty.borrow(), ctx, None);
    builder.build_store(alloca, re);
    alloca.new_output(ty).to_result()
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
    let lenloaded = ctx.try_load2var(
        f.paralist[1].range(),
        v2.get_value(),
        builder,
        &v2.get_ty().borrow(),
    )?;

    let arr_tp = Arc::new(RefCell::new(PLType::Arr(ARRType {
        element_type: elm.clone(),
        size_handle: lenloaded,
        generic_map: IndexMap::from([(Ustr::from("T"), elm.clone())]),
    })));
    let arr = builder.alloc("array_alloca", &arr_tp.borrow(), ctx, None);
    let arr_raw = builder
        .build_struct_gep(arr, 1, "arr_raw", &arr_tp.borrow(), ctx)
        .unwrap();
    let loaded = ctx.try_load2var(
        f.paralist[0].range(),
        v.get_value(),
        builder,
        &v.get_ty().borrow(),
    )?;
    let raw_loaded = builder.build_load(arr_raw, "arr_raw_load", &PLType::new_i8_ptr(), ctx);

    builder.build_memcpy(loaded, &elm.borrow(), raw_loaded, lenloaded, ctx);
    // builder.build_store(arr_raw, loaded);
    let arr_len = builder
        .build_struct_gep(arr, 2, "arr_len", &arr_tp.borrow(), ctx)
        .unwrap();
    builder.build_store(arr_len, lenloaded);

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
        .build_struct_gep(v.get_value(), 2, "arr_len", &v.get_ty().borrow(), ctx)
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
        .build_struct_gep(v.get_value(), 1, "arr_raw", &v.get_ty().borrow(), ctx)
        .unwrap();
    let to_raw = builder
        .build_struct_gep(to.get_value(), 1, "arr_raw", &to.get_ty().borrow(), ctx)
        .unwrap();

    let len_raw = len.get_value();
    match (&*v.get_ty().borrow(), &*to.get_ty().borrow()) {
        (PLType::Arr(a1), PLType::Arr(a2)) => {
            let from_raw = builder.build_load(
                from_raw,
                "arr_load_field",
                &PLType::Pointer(a1.element_type.clone()),
                ctx,
            );
            let to_raw = builder.build_load(
                to_raw,
                "arr_load_field",
                &PLType::Pointer(a2.element_type.clone()),
                ctx,
            );
            let len_raw =
                ctx.try_load2var(Default::default(), len_raw, builder, &PLType::new_i64())?;
            builder.build_memcpy(from_raw, &a1.element_type.borrow(), to_raw, len_raw, ctx);
            Ok(Default::default())
        }
        _ => unreachable!(),
    }
}

/// # emit_arr_slice
///
/// builtin function to slice an array
fn emit_arr_slice<'a, 'b>(
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
    let startlen = st.get_value().unwrap();

    if !matches!(
        &*startlen.get_ty().borrow(),
        PLType::Primitive(PriType::I64)
    ) {
        return Err(f.paralist[1]
            .range()
            .new_err(crate::ast::diag::ErrorCode::TYPE_MISMATCH)
            .add_label(
                f.paralist[2].range(),
                ctx.get_file(),
                format_label!(
                    "expect i64, found {}",
                    startlen.get_ty().borrow().get_name()
                ),
            )
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
        .build_struct_gep(v.get_value(), 1, "arr_raw", &v.get_ty().borrow(), ctx)
        .unwrap();

    let from_raw = builder.build_load(from_raw, "arr_load_field", &PLType::new_i8_ptr(), ctx);

    if let PLType::Arr(arr) = &*v.get_ty().borrow() {
        let startlen = ctx.try_load2var(
            Default::default(),
            startlen.get_value(),
            builder,
            &PLType::new_i64(),
        )?;
        // to raw is just from raw reslice
        let to_raw = builder.build_in_bounds_gep(
            from_raw,
            &[startlen],
            "offset_p",
            &arr.element_type.borrow(),
            ctx,
        );
        let len = ctx.try_load2var(
            Default::default(),
            len.get_value(),
            builder,
            &PLType::new_i64(),
        )?;
        // store them
        let arr_tp = Arc::new(RefCell::new(PLType::Arr(ARRType {
            element_type: arr.element_type.clone(),
            size_handle: len,
            generic_map: IndexMap::from([(String::from("T").into(), arr.element_type.clone())]),
        })));
        let arr = builder.alloc("array_alloca", &arr_tp.borrow(), ctx, None);
        let arr_raw = builder
            .build_struct_gep(arr, 1, "arr_raw", &arr_tp.borrow(), ctx)
            .unwrap();
        builder.build_store(arr_raw, to_raw);
        let arr_len = builder
            .build_struct_gep(arr, 2, "arr_len", &arr_tp.borrow(), ctx)
            .unwrap();
        builder.build_store(arr_len, len);
        arr.new_output(arr_tp).to_result()
    } else {
        unreachable!()
    }
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
            content: s.to_string(),
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
            content: s.to_string(),
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
        let (tp, v) = ctx.deref_greedily(tp, v, builder);
        let tp = get_type_deep(tp);
        let (tp, v) = ctx.deref_greedily(tp, v, builder);
        let mut code_gen = false;
        let stp = match &*tp.borrow() {
            PLType::Struct(_) => {
                code_gen = true;
                Some(tp.clone())
            }
            _ => None,
        };
        let mut builder = builder;
        let noop = BuilderEnum::NoOp(NoOpBuilder::default());
        let ctx = &mut ctx.new_child(f.range().start, builder);
        // 处理不是结构体类型的情况
        if !code_gen {
            let b = builder.int_value(&PriType::BOOL, 0, false);
            // 这种情况不会生成对应代码，但是需要进行相应的语法分析
            builder = &noop;
            ctx.run_in_origin_mod(|ctx| {
                let place_holder = Arc::new(RefCell::new(PLType::PlaceHolder(PlaceHolderType {
                    name: "@field_T".into(),
                    range: Default::default(),
                    path: "".into(),
                    methods: Default::default(),
                })));
                let field_tp = Arc::new(RefCell::new(PLType::Generic(GenericType {
                    name: "@field_T".into(),
                    range: Default::default(),
                    curpltype: Some(place_holder),
                    trait_impl: None,
                    refs: Default::default(),
                })));
                let gep = builder.alloc("placeholder", &field_tp.borrow(), ctx, None);
                let mut sn = StringNode {
                    content: "field_T".to_owned(),
                    range: Range::default(),
                };
                let re = sn.emit(ctx, builder)?.get_value().unwrap();
                let sv = re.get_value();
                let ctx = &mut ctx.new_child(f.range().start, builder);
                ctx.add_symbol_raw("_field".into(), gep, field_tp, f.range);
                ctx.add_symbol_raw("_field_name".into(), sv, re.get_ty(), f.range);
                if let NodeEnum::Sts(p) = &mut *f.paralist[1] {
                    p.emit(ctx, builder)?;
                }
                Ok(())
            })?;
            return b
                .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                .to_result();
        }

        if let Some(s) = stp {
            if let PLType::Struct(sttp) = &*s.borrow() {
                for (name, field) in sttp.fields.iter() {
                    let gep = builder
                        .build_struct_gep(v, field.index, "tmp_gep", &s.borrow(), ctx)
                        .unwrap();
                    ctx.run_in_origin_mod(|ctx| {
                        ctx.run_in_type_mod(sttp, |ctx, _| {
                            let field_tp = field.typenode.get_type(ctx, builder, true)?;
                            let field_tp = Arc::new(RefCell::new(PLType::Generic(GenericType {
                                name: "@field_T".into(),
                                range: Default::default(),
                                curpltype: Some(field_tp),
                                trait_impl: None,
                                refs: Default::default(),
                            })));
                            ctx.add_symbol_raw("_field".into(), gep, field_tp, f.range);
                            let mut sn = StringNode {
                                content: name.to_string(),
                                range: Range::default(),
                            };
                            let re = sn.emit(ctx, builder)?.get_value().unwrap();
                            let sv = re.get_value();
                            ctx.add_symbol_raw("_field_name".into(), sv, re.get_ty(), f.range);
                            Ok(())
                        })?;
                        let ctx = &mut ctx.new_child(f.range().start, builder);
                        if let NodeEnum::Sts(p) = &mut *f.paralist[1] {
                            p.emit(ctx, builder)?;
                        }
                        Ok(())
                    })?;
                }
                let b = builder.int_value(&PriType::BOOL, 1, false);
                b.new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                    .to_result()
            } else {
                unreachable!()
            }
        } else {
            let b = builder.int_value(&PriType::BOOL, 0, false);
            b.new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                .to_result()
        }
    } else {
        unreachable!()
    }
}

fn emit_if_arr<'a, 'b>(
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
        let (tp, v) = ctx.deref_greedily(tp, v, builder);
        let tp = get_type_deep(tp);
        let (tp, v) = ctx.deref_greedily(tp, v, builder);
        let mut code_gen = false;
        let stp = match &*tp.borrow() {
            PLType::Arr(_) => {
                code_gen = true;
                Some(tp.clone())
            }
            _ => None,
        };
        let mut builder = builder;
        let noop = BuilderEnum::NoOp(NoOpBuilder::default());
        let ctx = &mut ctx.new_child(f.range().start, builder);
        // 处理不是结构体类型或者没有有效字段的情况
        if !code_gen {
            let b = builder.int_value(&PriType::BOOL, 0, false);
            // 这种情况不会生成对应代码，但是需要进行相应的语法分析
            builder = &noop;
            ctx.run_in_origin_mod(|ctx| {
                let t = Arc::new(RefCell::new(PLType::PlaceHolder(PlaceHolderType {
                    name: "@elm_T".into(),
                    range: Default::default(),
                    path: "".into(),
                    methods: Default::default(),
                })));
                let elm_tp = Arc::new(RefCell::new(PLType::Arr(ARRType {
                    element_type: t.clone(),
                    size_handle: 0,
                    generic_map: IndexMap::from([(Ustr::from("T"), t.clone())]),
                })));
                let gep = builder.alloc("placeholder", &elm_tp.borrow(), ctx, None);
                ctx.add_symbol_raw("_arr".into(), gep, elm_tp, f.range);
                let ctx = &mut ctx.new_child(f.range().start, builder);
                if let NodeEnum::Sts(p) = &mut *f.paralist[1] {
                    p.emit(ctx, builder)?;
                }
                Ok(())
            })?;
            return b
                .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                .to_result();
        }

        if let Some(s) = stp {
            if let PLType::Arr(_) = &*s.borrow() {
                ctx.add_symbol_raw("_arr".into(), v, s.clone(), f.range);

                let b = builder.int_value(&PriType::BOOL, 1, false);
                let ctx = &mut ctx.new_child(f.range().start, builder);
                if let NodeEnum::Sts(p) = &mut *f.paralist[1] {
                    p.emit(ctx, builder)?;
                }
                b.new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                    .to_result()
            } else {
                unreachable!()
            }
        } else {
            let b = builder.int_value(&PriType::BOOL, 0, false);
            b.new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                .to_result()
        }
    } else {
        unreachable!()
    }
}

fn emit_if_union<'a, 'b>(
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
        let (tp, v) = ctx.deref_greedily(tp, v, builder);
        let tp = get_type_deep(tp);
        let (tp, v) = ctx.deref_greedily(tp, v, builder);
        let mut code_gen = false;
        let stp = match &*tp.borrow() {
            PLType::Union(_) => {
                code_gen = true;
                Some(tp.clone())
            }
            _ => None,
        };
        let mut builder = builder;
        let noop = BuilderEnum::NoOp(NoOpBuilder::default());
        let ctx = &mut ctx.new_child(f.range().start, builder);
        // 处理不是结构体类型或者没有有效字段的情况
        if !code_gen {
            let b = builder.int_value(&PriType::BOOL, 0, false);
            // 这种情况不会生成对应代码，但是需要进行相应的语法分析
            builder = &noop;
            ctx.run_in_origin_mod(|ctx| {
                let elm_tp = Arc::new(RefCell::new(PLType::Pointer(Arc::new(RefCell::new(
                    PLType::PlaceHolder(PlaceHolderType {
                        name: "@inner_T".into(),
                        range: Default::default(),
                        path: "".into(),
                        methods: Default::default(),
                    }),
                )))));
                let gep = builder.alloc("placeholder", &elm_tp.borrow(), ctx, None);
                ctx.add_symbol_raw("_inner".into(), gep, elm_tp, f.range);
                let ctx = &mut ctx.new_child(f.range().start, builder);
                if let NodeEnum::Sts(p) = &mut *f.paralist[1] {
                    p.emit(ctx, builder)?;
                }
                Ok(())
            })?;
            return b
                .new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                .to_result();
        }

        if let Some(s) = stp {
            if let PLType::Union(u) = &*s.borrow() {
                let gep = builder
                    .build_struct_gep(v, 0, "tmp_gep", &s.borrow(), ctx)
                    .unwrap();
                let ptr = builder
                    .build_struct_gep(v, 1, "inner_ptr", &s.borrow(), ctx)
                    .unwrap();
                let ptr = builder.build_load(ptr, "inner_ptr", &PLType::Pointer(s.clone()), ctx);

                let u_tp_i =
                    builder.build_load(gep, "u_tp_i", &PLType::Primitive(PriType::I64), ctx);
                let after_bb = builder.append_basic_block(ctx.function.unwrap(), "after");

                for (i, tp) in u.get_sum_types(ctx, builder).iter().enumerate() {
                    // 生成分支，对比是否为该类型
                    let b = builder.build_int_compare(
                        crate::ast::builder::IntPredicate::EQ,
                        u_tp_i,
                        builder.int_value(&PriType::I64, i as _, true),
                        "is_this_type",
                    );
                    let new_bb = builder
                        .append_basic_block(ctx.function.unwrap(), &format!("is_this_type_{}", i));
                    let next_bb = builder.append_basic_block(
                        ctx.function.unwrap(),
                        &format!("is_not_this_type_{}", i),
                    );
                    builder.build_conditional_branch(b, new_bb, next_bb);

                    // 生成bitcast到该类型的代码
                    builder.position_at_end_block(new_bb);
                    let new_tp = tp.clone();
                    let new_v =
                        builder.bitcast(ctx, ptr, &PLType::Pointer(new_tp.clone()), "inner");

                    ctx.add_symbol_raw("_inner".into(), new_v, new_tp.clone(), f.range);
                    let ctx = &mut ctx.new_child(f.range().start, builder);
                    if let NodeEnum::Sts(p) = &mut *f.paralist[1] {
                        p.emit(ctx, builder)?;
                    }

                    builder.build_unconditional_branch(next_bb);
                    builder.position_at_end_block(next_bb);
                }

                builder.build_unconditional_branch(after_bb);
                builder.position_at_end_block(after_bb);

                let b = builder.int_value(&PriType::BOOL, 1, false);
                b.new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                    .to_result()
            } else {
                unreachable!()
            }
        } else {
            let b = builder.int_value(&PriType::BOOL, 0, false);
            b.new_output(Arc::new(RefCell::new(PLType::Primitive(PriType::BOOL))))
                .to_result()
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
        let (tp, mut v) = ctx.deref_greedily(tp, v, builder);
        let tp = get_type_deep(tp);
        if tp != generic {
            // 类型不匹配
            // 虽然不需要生成之后的代码，但是为了提供更好的语法支持，我们假装它匹配并进行分析
            // 此处将v替换为正确类型的变量
            builder = &noop;
            v = builder.alloc("placeholder", &generic.borrow(), ctx, None);
        }
        let ctx = &mut ctx.new_child(f.paralist[1].range().start, builder);

        ctx.add_symbol_raw("_value".into(), v, generic.clone(), f.range);
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

lazy_static! {
    pub static ref STUCK_FNS: FxHashSet<&'static str> = {
        let mut set: FxHashSet<&'static str> = Default::default();
        set.insert("sleep");
        set.insert("lock_mutex");
        set.insert("condvar_wait");
        set
    };
}
