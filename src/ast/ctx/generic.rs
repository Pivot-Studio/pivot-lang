use super::super::pltype::get_type_deep;

use super::super::builder::no_op_builder::NoOpBuilder;

use crate::ast::builder::BuilderEnum;
use crate::ast::traits::CustomType;

use indexmap::IndexMap;
use ustr::Ustr;

use super::GenericCacheMap;

use super::super::pltype::PLType;

use std::cell::RefCell;

use std::sync::Arc;

use super::super::pltype::TraitImplAble;

use super::Ctx;

impl<'a> Ctx<'a> {
    pub fn add_infer_result(
        &self,
        tp: &impl TraitImplAble,
        name: Ustr,
        pltp: Arc<RefCell<PLType>>,
    ) {
        self.generic_cache
            .borrow_mut()
            .entry(tp.get_full_name_except_generic())
            .or_default()
            .borrow_mut()
            .insert(name, pltp);
    }

    pub fn get_infer_result(
        &self,
        tp: &impl TraitImplAble,
        name: Ustr,
    ) -> Option<Arc<RefCell<PLType>>> {
        let infer_map = self.generic_cache.borrow();
        let infer_map = infer_map.get(&tp.get_full_name_except_generic())?;
        let x = infer_map.borrow().get(&name).cloned();
        x
    }

    pub(crate) fn import_all_infer_maps_from(&self, other: &GenericCacheMap) {
        for (k, v) in other.iter() {
            if !self.generic_cache.borrow().contains_key(k) {
                let map: Arc<RefCell<IndexMap<Ustr, Arc<RefCell<PLType>>>>> = Default::default();
                self.generic_cache.borrow_mut().insert(*k, map.clone());
                map.borrow_mut().extend(v.borrow().clone());
            } else {
                let infer_map = self.generic_cache.borrow();
                let infer_map = infer_map.get(k).unwrap();
                infer_map.borrow_mut().extend(v.borrow().clone());
            }
        }
    }
    pub fn import_all_infer_maps_from_sub_mods(&self) {
        for (_, sub_mod) in self.plmod.submods.iter() {
            self.import_all_infer_maps_from(&sub_mod.generic_infer.borrow());
        }
    }
    /// 左是目标类型，右是实际类型
    /// when need eq trait and sttype,the left must be trait
    pub fn eq(&mut self, l: Arc<RefCell<PLType>>, r: Arc<RefCell<PLType>>) -> EqRes {
        let noop = BuilderEnum::NoOp(NoOpBuilder::default());
        // get it's pointer
        let noop_ptr = &noop as *const BuilderEnum<'a, '_>;
        let builder = unsafe { noop_ptr.as_ref().unwrap() };
        if l == r && matches!(&*l.borrow(), PLType::Generic(_)) {
            if let PLType::Generic(l) = &mut *l.borrow_mut() {
                if l.curpltype.is_some() {
                    return self.eq(
                        l.curpltype.as_ref().unwrap().clone(),
                        l.curpltype.as_ref().unwrap().clone(),
                    );
                }
                l.set_type(Arc::new(RefCell::new(PLType::Generic(l.clone()))));
                return EqRes {
                    eq: true,
                    need_up_cast: false,
                    reason: None,
                };
            }
        }
        if let (PLType::Generic(l), PLType::Generic(r)) = (&*l.borrow(), &*r.borrow()) {
            if l == r {
                return EqRes {
                    eq: true,
                    need_up_cast: false,
                    reason: None,
                };
            }
        }
        if matches!(&*l.borrow(), PLType::Generic(_)) {
            if let PLType::Generic(lg) = &mut *l.borrow_mut() {
                if lg.curpltype.is_some() {
                    return self.eq(lg.curpltype.as_ref().unwrap().clone(), r);
                }
                // set the real type of generic type to the type of the right
                lg.set_type(r.clone());
                // in this case, we need to check if the right type
                // satisfies the trait constraints of the generic type
                // so we don't return here
            }
            if let PLType::Generic(lg) = &*l.borrow() {
                if lg.trait_impl.is_some() {
                    if let PLType::Generic(r) = &*r.borrow() {
                        // in case checking equality of two generic types,
                        // we need to check if the trait impls of them are equal
                        if let Some(reason) = self.diff_trait_impl(lg, r) {
                            return EqRes {
                                eq: false,
                                need_up_cast: false,
                                reason: Some(reason),
                            };
                        }
                    } else if lg
                        .trait_impl
                        .as_ref()
                        .unwrap()
                        .get_types(self, builder)
                        .unwrap_or_default()
                        .iter()
                        .any(|lt| !self.eq(lt.clone(), r.clone()).eq)
                    {
                        // try to pass normal type as generic type, check if it contains
                        // all the trait expected by the generic type
                        return EqRes {
                            eq: false,
                            need_up_cast: false,
                            reason: Some(
                                "cannot cast a type to a trait it never implements".into(),
                            ),
                        };
                    }
                }
            }
            if let PLType::Generic(lg) = &mut *l.borrow_mut() {
                lg.set_type(r);
            }
            return EqRes {
                eq: true,
                need_up_cast: false,
                reason: None,
            };
        }
        if l != r {
            if matches!(&*l.borrow(), PLType::Union(_)) {
                return EqRes {
                    eq: true,
                    need_up_cast: true,
                    reason: None,
                };
            }
            let trait_pltype = l;
            let st_pltype = self.auto_deref_tp(r.clone());
            if let PLType::Trait(t) = &*trait_pltype.borrow() {
                let eq = st_pltype.borrow().implements_trait(t, self.get_root_ctx());
                return EqRes {
                    eq,
                    need_up_cast: true,
                    reason: Some(
                        format!(
                            "trait `{}` is not implemented for `{}`",
                            t.get_name(),
                            st_pltype.borrow().get_name()
                        )
                        .into(),
                    ),
                };
            }
            if get_type_deep(trait_pltype) == get_type_deep(r) {
                return EqRes {
                    eq: true,
                    need_up_cast: false,
                    reason: None,
                };
            }
            return EqRes {
                eq: false,
                need_up_cast: false,
                reason: None,
            };
        }
        EqRes {
            eq: true,
            need_up_cast: false,
            reason: None,
        }
    }
}

#[derive(Debug)]
pub struct EqRes {
    pub eq: bool,
    pub need_up_cast: bool,
    pub reason: Option<Ustr>,
}

impl EqRes {
    pub fn total_eq(&self) -> bool {
        self.eq && !self.need_up_cast
    }
}
