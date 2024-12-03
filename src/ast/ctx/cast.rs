#![allow(clippy::too_many_arguments)]
use rustc_hash::FxHashMap;
use ustr::Ustr;

use super::super::pltype::Field;

use super::super::pltype::FNValue;

use super::super::pltype::TraitImplAble;

use super::super::pltype::ImplAble;

use super::super::pltype::PriType;

use std::cell::RefCell;

use std::sync::Arc;

use super::super::pltype::PLType;

use super::super::diag::PLDiag;
use crate::ast::diag::ErrorCode;
use crate::ast::node::TypeNodeEnum;
use crate::ast::pltype::get_type_deep;
use crate::ast::traits::CustomType;
use crate::format_label;

use super::Ctx;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::node::TypeNode;
use crate::ast::pltype::STType;
use crate::ast::pltype::TraitImplAbleWithGeneric;
use crate::ast::range::Range;
use crate::mismatch_err;

pub(crate) fn store_trait_hash_and_ptr<'a, T: TraitImplAble>(
    builder: &BuilderEnum<'a, '_>,
    ctx: &mut Ctx<'a>,
    st_value: usize,
    trait_handle: usize,
    st: &T,
    pltype: Arc<RefCell<PLType>>,
) -> Result<usize, PLDiag> {
    let st_value = builder.bitcast(
        ctx,
        st_value,
        &PLType::Pointer(pltype.clone()),
        "traitcast_tmp",
    );
    let v_ptr = builder
        .build_struct_gep(trait_handle, 1, "v_tmp", &pltype.borrow(), ctx)
        .unwrap();
    builder.build_store(v_ptr, st_value);
    let type_hash = builder
        .build_struct_gep(trait_handle, 0, "tp_hash", &pltype.borrow(), ctx)
        .unwrap();
    let hash = st.get_type_code();
    let hash = builder.int_value(&PriType::U64, hash, false);
    builder.build_store(type_hash, hash);
    Ok(trait_handle)
}

pub(crate) fn set_mthd_fields<'a, T: ImplAble>(
    t: &STType,
    st: &T,
    ctx: &mut Ctx<'a>,
    st_pltype: &Arc<RefCell<PLType>>,
    builder: &BuilderEnum<'a, '_>,
    trait_handle: usize,
) -> Result<(), PLDiag> {
    for f in t.list_trait_fields().iter() {
        let mthd = find_mthd(st, f, t).map_or_else(
            || {
                for t in &t.derives {
                    match &*t.borrow() {
                        PLType::Trait(t) => {
                            if let Some(mthd) = find_mthd(st, f, t) {
                                return Ok(mthd);
                            }
                        }
                        _ => return Err(PLDiag::default()),
                    }
                }
                Err(PLDiag::default())
            },
            Ok,
        )?;
        set_mthd_field(mthd, ctx, t, st_pltype, builder, trait_handle, f)?;
    }
    Ok(())
}

pub(crate) fn set_trait_impl_mthd_fields<'a, T: TraitImplAble>(
    t: &STType,
    st: &T,
    ctx: &mut Ctx<'a>,
    st_pltype: &Arc<RefCell<PLType>>,
    builder: &BuilderEnum<'a, '_>,
    trait_handle: usize,
) -> Result<(), PLDiag> {
    for f in t.list_trait_fields().iter() {
        let mthd = find_trait_impl_mthd(st, f.name, t).unwrap_or_else(|| {
            for t in &t.derives {
                match &*t.borrow() {
                    PLType::Trait(t) => {
                        if let Some(mthd) = find_trait_impl_mthd(st, f.name, t) {
                            return mthd;
                        }
                    }
                    _ => unreachable!(),
                }
            }
            unreachable!()
        });
        set_mthd_field(mthd, ctx, t, st_pltype, builder, trait_handle, f)?;
    }
    Ok(())
}

pub(crate) fn set_mthd_field<'a>(
    mthd: Arc<RefCell<FNValue>>,
    ctx: &mut Ctx<'a>,
    t: &STType,
    st_pltype: &Arc<RefCell<PLType>>,
    builder: &BuilderEnum<'a, '_>,
    trait_handle: usize,
    f: &Field,
) -> Result<(), PLDiag> {
    let bind = mthd.clone();
    let m = bind.borrow();
    let mut m = m.clone();
    m.fntype = m.fntype.new_pltype();
    let mthd = gen_mthd(m, ctx, st_pltype, builder, mthd)?;
    let fnhandle = builder.get_or_insert_fn_handle(&mthd.borrow(), ctx).0;
    let f_ptr = builder
        .build_struct_gep(
            trait_handle,
            f.index,
            "field_tmp",
            &PLType::Struct(t.clone()),
            ctx,
        )
        .unwrap();
    unsafe {
        builder.store_with_aoto_cast(f_ptr, fnhandle);
    }
    Ok(())
}

pub(crate) fn gen_mthd<'a>(
    mut m: FNValue,
    ctx: &mut Ctx<'a>,
    st_pltype: &Arc<RefCell<PLType>>,
    builder: &BuilderEnum<'a, '_>,
    mthd: Arc<RefCell<FNValue>>,
) -> Result<Arc<RefCell<FNValue>>, PLDiag> {
    let mthd = if m.fntype.generic {
        ctx.protect_generic_context(&m.fntype.generic_map.clone(), |ctx| {
            ctx.run_in_type_mod_mut(&mut m, |ctx, m| {
                m.fntype.param_pltypes[0].eq_or_infer(
                    ctx,
                    Arc::new(RefCell::new(PLType::Pointer(st_pltype.clone()))),
                    builder,
                )
            })?;
            if m.fntype.need_gen_code() {
                let re = ctx.run_in_type_mod_mut(&mut m, |ctx, fnvalue| {
                    // actual code gen happens here
                    fnvalue.generic_infer_pltype(ctx, builder)
                })?;
                Ok(Arc::new(RefCell::new(re)))
            } else {
                unreachable!()
            }
        })?
    } else {
        mthd
    };
    Ok(mthd)
}

pub(crate) fn find_mthd<T: ImplAble>(
    st: &T,
    f: &Field,
    t: &STType,
) -> Option<Arc<RefCell<FNValue>>> {
    st.get_method(f.name)
        .or(find_trait_impl_mthd(st, f.name, t))
}

pub(crate) fn find_trait_impl_mthd<T: TraitImplAble>(
    st: &T,
    name: Ustr,
    t: &STType,
) -> Option<Arc<RefCell<FNValue>>> {
    t.trait_methods_impl
        .borrow()
        .get(&st.get_full_name())
        .or(t
            .trait_methods_impl
            .borrow()
            .get(&st.get_full_name_except_generic()))
        .and_then(|v| v.get(&name))
        .cloned()
}

impl<'a, 'ctx> Ctx<'a> {
    pub(crate) fn cast_implable<T: TraitImplAbleWithGeneric + ImplAble>(
        &mut self,
        t: &STType,
        st: &T,
        ori_range: Range,
        target_range: Range,
        builder: &BuilderEnum<'a, 'ctx>,
        target_pltype: &Arc<RefCell<PLType>>,
        st_pltype: &Arc<RefCell<PLType>>,
        st_value: usize,
    ) -> Result<usize, PLDiag> {
        self.run_in_type_mod(t, |ctx, t| {
            ctx.protect_generic_context(&t.generic_map, |ctx| {
                if !st.implements_trait(t, ctx.get_root_ctx()) {
                    return Err(mismatch_err!(
                        ctx,
                        ori_range,
                        target_range,
                        target_pltype.borrow(),
                        st_pltype.borrow()
                    ));
                }
                let trait_handle = builder.alloc("tmp_traitv", &target_pltype.borrow(), ctx, None);
                set_mthd_fields(t, st, ctx, st_pltype, builder, trait_handle)?;

                store_trait_hash_and_ptr(
                    builder,
                    ctx,
                    st_value,
                    trait_handle,
                    st,
                    target_pltype.clone(),
                )
            })
        })
    }
    pub fn up_cast<'b>(
        &mut self,
        target_pltype: Arc<RefCell<PLType>>,
        ori_pltype: Arc<RefCell<PLType>>,
        target_range: Range,
        ori_range: Range,
        ori_value: usize,
        builder: &'b BuilderEnum<'a, 'ctx>,
    ) -> Result<usize, PLDiag> {
        if let (PLType::Closure(c), PLType::Fn(f)) =
            (&*target_pltype.borrow(), &*ori_pltype.borrow())
        {
            if f.to_closure_ty(self, builder) != *c {
                return Err(ori_range
                    .new_err(ErrorCode::FUNCTION_TYPE_NOT_MATCH)
                    .add_label(
                        target_range,
                        self.get_file(),
                        format_label!("expected type `{}`", c.get_name()),
                    )
                    .add_label(
                        ori_range,
                        self.get_file(),
                        format_label!("found type `{}`", f.to_closure_ty(self, builder).get_name()),
                    )
                    .add_to_ctx(self));
            }
            if ori_value == usize::MAX {
                return Err(ori_range
                    .new_err(ErrorCode::CANNOT_ASSIGN_INCOMPLETE_GENERICS)
                    .add_help("try add generic type explicitly to fix this error.")
                    .add_to_ctx(self));
            }
            let closure_v = builder.alloc("tmp", &target_pltype.borrow(), self, None);
            let closure_f = builder
                .build_struct_gep(closure_v, 0, "closure_f", &target_pltype.borrow(), self)
                .unwrap();
            let ori_value =
                builder.try_load2var(ori_range, ori_value, &ori_pltype.borrow(), self)?;
            builder.build_store(closure_f, builder.get_closure_trampoline(ori_value));
            return Ok(closure_v);
        }
        if let PLType::Union(u) = &*target_pltype.borrow() {
            let mut union_members = vec![];
            for tp in &u.sum_types {
                let tp = tp.get_type(self, builder, true)?;
                union_members.push(tp);
            }
            for (i, tp) in union_members.iter().enumerate() {
                if *get_type_deep(tp.to_owned()).borrow()
                    == *get_type_deep(ori_pltype.clone()).borrow()
                {
                    let union_handle =
                        builder.alloc("tmp_unionv", &target_pltype.borrow(), self, None);
                    let union_value = builder
                        .build_struct_gep(
                            union_handle,
                            1,
                            "union_value",
                            &target_pltype.borrow(),
                            self,
                        )
                        .unwrap();
                    let union_type_field = builder
                        .build_struct_gep(
                            union_handle,
                            0,
                            "union_type",
                            &target_pltype.borrow(),
                            self,
                        )
                        .unwrap();
                    let union_type = builder.int_value(&PriType::U64, i as u64, false);
                    builder.build_store(union_type_field, union_type);
                    let mut ptr = ori_value;
                    if !builder.is_ptr(ori_value) {
                        // mv to heap
                        ptr = builder.alloc("tmp", &ori_pltype.borrow(), self, None);
                        builder.build_store(ptr, ori_value);
                    }
                    let st_value = builder.bitcast(
                        self,
                        ptr,
                        &PLType::Pointer(Arc::new(RefCell::new(PLType::Primitive(PriType::I8)))),
                        "traitcast_tmp",
                    );
                    builder.build_store(union_value, st_value);

                    return Ok(union_handle);
                }
            }
        }
        let (st_pltype, st_value) = self.deref_greedily(ori_pltype, ori_value, builder);
        if let PLType::Trait(t) = &*target_pltype.borrow() {
            for f in t.list_trait_fields().iter() {
                if let TypeNodeEnum::Func(fu) = &*f.typenode {
                    if fu.generics.is_some() {
                        return Err(target_range.new_err(ErrorCode::THE_TARGET_TRAIT_CANNOT_BE_INSTANTIATED)
                        .add_label(
                            target_range,
                            self.get_file(),
                            format_label!("trait type `{}`", t.get_name()),
                        )
                        .add_label(
                            f.range,
                            t.get_path(),
                            format_label!("the method `{}` of trait `{}` has generic params, which makes it uninstantiatable",f.name, t.get_name()),
                        ).add_to_ctx(self));
                    }
                }
            }
        }
        match (&*target_pltype.borrow(), &*st_pltype.clone().borrow()) {
            (PLType::Trait(t), PLType::Struct(st)) => {
                return self.cast_implable(
                    t,
                    st,
                    ori_range,
                    target_range,
                    builder,
                    &target_pltype,
                    &st_pltype,
                    st_value,
                );
            }
            (PLType::Trait(t), PLType::Union(st)) => {
                return self.cast_implable(
                    t,
                    st,
                    ori_range,
                    target_range,
                    builder,
                    &target_pltype,
                    &st_pltype,
                    st_value,
                );
            }
            (PLType::Trait(t), PLType::Primitive(st)) => {
                return self.cast_trait_implable(
                    t,
                    st,
                    ori_range,
                    target_range,
                    builder,
                    &target_pltype,
                    &st_pltype,
                    st_value,
                );
            }
            (PLType::Trait(t), PLType::Arr(st)) => {
                return self.cast_trait_implable(
                    t,
                    st,
                    ori_range,
                    target_range,
                    builder,
                    &target_pltype,
                    &st_pltype,
                    st_value,
                );
            }
            (PLType::Trait(t), PLType::Trait(t2)) => {
                return self.run_in_type_mod(t, |ctx, t| {
                    ctx.protect_generic_context(&t.generic_map, |ctx| {
                        if !t2.trait_can_cast_to(t) {
                            return Err(mismatch_err!(
                                ctx,
                                ori_range,
                                target_range,
                                target_pltype.borrow(),
                                st_pltype.borrow()
                            ));
                        }
                        let trait_handle =
                            builder.alloc("tmp_traitv", &target_pltype.borrow(), ctx, None);
                        for f in t.list_trait_fields().iter() {
                            let fs = t2.get_all_field();
                            // convert to hash table
                            let fs: FxHashMap<Ustr, Field> =
                                fs.into_iter().map(|f| (f.name, f)).collect();

                            let field = fs.get(&f.name).unwrap();

                            let fnhandle = builder
                                .build_struct_gep(
                                    st_value,
                                    field.index,
                                    "trait_mthd",
                                    &st_pltype.borrow(),
                                    ctx,
                                )
                                .unwrap();
                            let fnhandle = builder.build_load(
                                fnhandle,
                                "trait_mthd",
                                &PLType::new_i8_ptr(),
                                ctx,
                            );
                            // let targetftp = f.typenode.get_type(ctx, builder, true).unwrap();
                            // let casted =
                            //     builder.bitcast(ctx, fnhandle, &targetftp.borrow(), "fncast_tmp");
                            let f_ptr = builder
                                .build_struct_gep(
                                    trait_handle,
                                    f.index,
                                    "field_tmp",
                                    &target_pltype.borrow(),
                                    ctx,
                                )
                                .unwrap();
                            unsafe {
                                builder.store_with_aoto_cast(f_ptr, fnhandle);
                            }
                        }
                        let st = builder
                            .build_struct_gep(st_value, 1, "src_v_tmp", &st_pltype.borrow(), ctx)
                            .unwrap();
                        let st = builder.build_load(st, "src_v", &PLType::new_i8_ptr(), ctx);
                        let v_ptr = builder
                            .build_struct_gep(
                                trait_handle,
                                1,
                                "v_tmp",
                                &target_pltype.borrow(),
                                ctx,
                            )
                            .unwrap();
                        builder.build_store(v_ptr, st);
                        let type_hash = builder
                            .build_struct_gep(
                                trait_handle,
                                0,
                                "tp_hash",
                                &target_pltype.borrow(),
                                ctx,
                            )
                            .unwrap();
                        let hash = builder
                            .build_struct_gep(st_value, 0, "src_tp_hash", &st_pltype.borrow(), ctx)
                            .unwrap();
                        let hash = builder.build_load(hash, "src_tp_hash", &PLType::new_i64(), ctx);
                        builder.build_store(type_hash, hash);
                        Ok(trait_handle)
                    })
                });
            }
            _ => (),
        }
        #[allow(clippy::needless_return)]
        return Err(mismatch_err!(
            self,
            ori_range,
            target_range,
            target_pltype.borrow(),
            st_pltype.borrow()
        ));
    }

    pub(crate) fn cast_trait_implable<T: TraitImplAble>(
        &mut self,
        t: &STType,
        st: &T,
        ori_range: Range,
        target_range: Range,
        builder: &BuilderEnum<'a, 'ctx>,
        target_pltype: &Arc<RefCell<PLType>>,
        st_pltype: &Arc<RefCell<PLType>>,
        st_value: usize,
    ) -> Result<usize, PLDiag> {
        self.run_in_type_mod(t, |ctx, t| {
            ctx.protect_generic_context(&t.generic_map, |ctx| {
                if !st.traitimplable_implements_trait(t, &ctx.get_root_ctx().plmod) {
                    return Err(mismatch_err!(
                        ctx,
                        ori_range,
                        target_range,
                        target_pltype.borrow(),
                        st_pltype.borrow()
                    ));
                }
                let trait_handle = builder.alloc("tmp_traitv", &target_pltype.borrow(), ctx, None);
                set_trait_impl_mthd_fields(t, st, ctx, st_pltype, builder, trait_handle)?;

                store_trait_hash_and_ptr(
                    builder,
                    ctx,
                    st_value,
                    trait_handle,
                    st,
                    target_pltype.clone(),
                )
            })
        })
    }
}
