#![allow(clippy::too_many_arguments)]
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

use super::Ctx;

use crate::ast::builder::BuilderEnum;
use crate::ast::builder::IRBuilder;
use crate::ast::node::TypeNode;
use crate::ast::pltype::ImplAbleWithGeneric;
use crate::ast::pltype::STType;
use crate::ast::range::Range;
use crate::mismatch_err;

pub(crate) fn store_trait_hash_and_ptr<'a, T: TraitImplAble>(
    builder: &BuilderEnum<'a, '_>,
    ctx: &mut Ctx<'a>,
    st_value: usize,
    trait_handle: usize,
    st: &T,
) -> Result<usize, PLDiag> {
    let st_value = builder.bitcast(
        ctx,
        st_value,
        &PLType::Pointer(Arc::new(RefCell::new(PLType::Primitive(PriType::I64)))),
        "traitcast_tmp",
    );
    let v_ptr = builder.build_struct_gep(trait_handle, 1, "v_tmp").unwrap();
    builder.build_store(v_ptr, st_value);
    let type_hash = builder
        .build_struct_gep(trait_handle, 0, "tp_hash")
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
        let mthd = find_mthd(st, f, t).unwrap_or_else(|| {
            for t in &t.derives {
                match &*t.borrow() {
                    PLType::Trait(t) => {
                        if let Some(mthd) = find_mthd(st, f, t) {
                            return mthd;
                        }
                    }
                    _ => unreachable!(),
                }
            }
            unreachable!()
        });
        set_mthd_field(mthd, ctx, st_pltype, builder, trait_handle, f)?;
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
        let mthd = find_trait_impl_mthd(st, &f.name, t).unwrap_or_else(|| {
            for t in &t.derives {
                match &*t.borrow() {
                    PLType::Trait(t) => {
                        if let Some(mthd) = find_trait_impl_mthd(st, &f.name, t) {
                            return mthd;
                        }
                    }
                    _ => unreachable!(),
                }
            }
            unreachable!()
        });
        set_mthd_field(mthd, ctx, st_pltype, builder, trait_handle, f)?;
    }
    Ok(())
}

pub(crate) fn set_mthd_field<'a>(
    mthd: Arc<RefCell<FNValue>>,
    ctx: &mut Ctx<'a>,
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
        .build_struct_gep(trait_handle, f.index, "field_tmp")
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
    st.get_method(&f.name)
        .or(find_trait_impl_mthd(st, &f.name, t))
}

pub(crate) fn find_trait_impl_mthd<T: TraitImplAble>(
    st: &T,
    name: &str,
    t: &STType,
) -> Option<Arc<RefCell<FNValue>>> {
    t.trait_methods_impl
        .borrow()
        .get(&st.get_full_name())
        .or(t
            .trait_methods_impl
            .borrow()
            .get(&st.get_full_name_except_generic()))
        .and_then(|v| v.get(name))
        .cloned()
}

impl<'a, 'ctx> Ctx<'a> {
    pub(crate) fn cast_implable<T: ImplAbleWithGeneric>(
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
                if !st.implements_trait(t, &ctx.get_root_ctx().plmod) {
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

                store_trait_hash_and_ptr(builder, ctx, st_value, trait_handle, st)
            })
        })
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

                store_trait_hash_and_ptr(builder, ctx, st_value, trait_handle, st)
            })
        })
    }
}
