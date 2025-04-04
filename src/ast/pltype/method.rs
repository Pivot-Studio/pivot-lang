use std::{borrow::Cow, cell::RefCell, sync::Arc};

use indexmap::IndexMap;
use lsp_types::{Command, CompletionItem, CompletionItemKind, InsertTextFormat};
use rustc_hash::FxHashMap;
use ustr::Ustr;

use crate::{
    ast::{
        builder::BuilderEnum,
        ctx::Ctx,
        diag::{ErrorCode, PLDiag},
        node::RangeTrait,
        plmod::Mod,
        tokens::TokenType,
        traits::CustomType,
    },
    format_label,
    utils::get_hash_code,
};

use super::{append_name_with_generic, impl_in_mod, ARRType, FNValue, PLType, STType, UnionType};

pub trait ImplAble: RangeTrait + CustomType + TraitImplAble {
    fn get_method_table(&self) -> Arc<RefCell<FxHashMap<Ustr, Arc<RefCell<FNValue>>>>>;
    fn get_method(&self, name: Ustr) -> Option<Arc<RefCell<FNValue>>> {
        let binding = self.get_method_table();
        let table = binding.borrow();
        table.get(&name).cloned()
    }
    fn add_method(&self, name: Ustr, value: Arc<RefCell<FNValue>>) -> Result<(), PLDiag> {
        let range = self.range();
        let binding = self.get_method_table();
        let mut table = binding.borrow_mut();
        if let Some(v) = table.get(&name) {
            return Err(range
                .new_err(ErrorCode::DUPLICATE_METHOD)
                .add_label(
                    v.borrow().range,
                    v.borrow().path,
                    format_label!("Previously defined here"),
                )
                .clone());
        }
        table.insert(name, value);
        Ok(())
    }
    fn get_mthd_completions(&self, ctx: &Ctx) -> Vec<CompletionItem> {
        let pub_only = self.get_path() != ctx.plmod.path;
        let mut completions = Vec::new();
        let mut f = |name: Ustr, v: &FNValue| {
            if pub_only && !v.is_modified_by(TokenType::PUB) {
                return;
            }
            completions.push(CompletionItem {
                kind: Some(CompletionItemKind::METHOD),
                label: name.to_string(),
                detail: Some("method".to_string()),
                insert_text: Some(v.gen_snippet().to_string()),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                command: Some(Command::new(
                    "trigger help".to_string(),
                    "editor.action.triggerParameterHints".to_string(),
                    None,
                )),
                ..Default::default()
            });
        };
        for (name, v) in self.get_method_table().borrow().iter() {
            f(*name, &v.clone().borrow());
        }
        completions
    }
}

pub trait TraitImplAble {
    fn get_full_name_except_generic(&self) -> Ustr;
    fn get_full_name(&self) -> Ustr;
    fn get_type_code(&self) -> u64 {
        let full_name = self.get_full_name();
        get_hash_code(full_name)
    }
    fn traitimplable_implements_trait(&self, tp: &STType, plmod: &Mod) -> bool {
        let name = &self.get_full_name_except_generic();
        if impl_in_mod(plmod, name, tp) {
            return true;
        } else {
            for m in plmod.submods.values() {
                if impl_in_mod(m, name, tp) {
                    return true;
                }
            }
        }
        false
    }
    fn get_mod_path(&self) -> Option<Cow<Ustr>>;
}

/// # Generic
///
/// Describe a type that has generic parameters.
pub trait Generic {
    fn get_generic_map(&self) -> &IndexMap<Ustr, Arc<RefCell<PLType>>>;
    fn get_generic_map_mut(&mut self) -> &mut IndexMap<Ustr, Arc<RefCell<PLType>>>;
    fn get_generic_infer_map(&self) -> Option<&IndexMap<Ustr, Arc<RefCell<PLType>>>>;
    /// return the size of generics that can be annotated explicitly
    /// by the user. For more details, see [get_generic_size].
    fn get_user_generic_size(&self) -> usize;
    /// This is the real generic size.
    ///
    /// **Not** all generics can be annotated.
    /// For example: in generic methods, generics belongs
    /// to the `impl` block is also counted in the generic size,
    /// but they are cannot be annotated explicitly.
    fn get_generic_size(&self) -> usize {
        self.get_generic_map().len()
    }
}

impl Generic for ARRType {
    fn get_generic_map(&self) -> &IndexMap<Ustr, Arc<RefCell<PLType>>> {
        &self.generic_map
    }
    fn get_generic_map_mut(&mut self) -> &mut IndexMap<Ustr, Arc<RefCell<PLType>>> {
        &mut self.generic_map
    }

    fn get_generic_infer_map(&self) -> Option<&IndexMap<Ustr, Arc<RefCell<PLType>>>> {
        None
    }

    fn get_user_generic_size(&self) -> usize {
        1
    }
}

impl Generic for STType {
    fn get_generic_map(&self) -> &IndexMap<Ustr, Arc<RefCell<PLType>>> {
        &self.generic_map
    }
    fn get_generic_map_mut(&mut self) -> &mut IndexMap<Ustr, Arc<RefCell<PLType>>> {
        &mut self.generic_map
    }

    fn get_generic_infer_map(&self) -> Option<&IndexMap<Ustr, Arc<RefCell<PLType>>>> {
        Some(&self.generic_infer_types)
    }

    fn get_user_generic_size(&self) -> usize {
        self.generic_map.len()
    }
}

impl Generic for FNValue {
    fn get_generic_map(&self) -> &IndexMap<Ustr, Arc<RefCell<PLType>>> {
        &self.fntype.generic_map
    }
    fn get_generic_map_mut(&mut self) -> &mut IndexMap<Ustr, Arc<RefCell<PLType>>> {
        &mut self.fntype.generic_map
    }

    fn get_generic_infer_map(&self) -> Option<&IndexMap<Ustr, Arc<RefCell<PLType>>>> {
        None
    }

    fn get_user_generic_size(&self) -> usize {
        self.fntype.generics_size
    }
}

impl Generic for UnionType {
    fn get_generic_map(&self) -> &IndexMap<Ustr, Arc<RefCell<PLType>>> {
        &self.generic_map
    }
    fn get_generic_map_mut(&mut self) -> &mut IndexMap<Ustr, Arc<RefCell<PLType>>> {
        &mut self.generic_map
    }

    fn get_generic_infer_map(&self) -> Option<&IndexMap<Ustr, Arc<RefCell<PLType>>>> {
        None
    }

    fn get_user_generic_size(&self) -> usize {
        self.generic_map.len()
    }
}

pub trait TraitImplAbleWithGeneric: Generic + TraitImplAble {
    fn implements_trait_curr_mod(&self, tp: &STType, plmod: &Mod) -> bool {
        // FIXME: strange logic
        let re = plmod
            .impls
            .borrow()
            .get(&self.get_full_name())
            .or(plmod
                .impls
                .borrow()
                .get(&self.get_full_name_except_generic()))
            .map(|v| {
                v.get(&tp.get_full_name_except_generic())
                    .map(|gm| {
                        let mut gm = gm.clone();
                        for (k, _) in &gm.clone() {
                            if let Some(vv) = self.get_generic_map().get(k) {
                                gm.insert(*k, vv.clone());
                            }
                            if let Some(vv) = self.get_generic_infer_map().and_then(|v| v.get(k)) {
                                gm.insert(*k, vv.clone());
                            }
                        }
                        tp.get_full_name()
                            == append_name_with_generic(&gm, &tp.get_full_name_except_generic())
                    })
                    .unwrap_or_default()
            })
            .unwrap_or_default();

        let re2 = plmod
            .impls
            .borrow()
            .get(&self.get_full_name())
            .or(plmod
                .impls
                .borrow()
                .get(&self.get_full_name_except_generic()))
            .map(|v| v.get(&tp.get_full_name()).is_some())
            .unwrap_or_default();
        if !re && !re2 {
            return re;
        }
        for de in &tp.derives {
            match &*de.borrow() {
                PLType::Trait(t) => {
                    let re = self.implements(t, plmod);
                    if !re {
                        return re;
                    }
                }
                _ => unreachable!(),
            }
        }
        true
    }
    fn implements(&self, tp: &STType, plmod: &Mod) -> bool {
        if plmod
            .impls
            .borrow()
            .get(&self.get_full_name())
            .and_then(|v| v.get(&tp.get_full_name()))
            .is_some()
        {
            return true;
        }
        for plmod in plmod.submods.values() {
            if plmod
                .impls
                .borrow()
                .get(&self.get_full_name())
                .and_then(|v| v.get(&tp.get_full_name()))
                .is_some()
            {
                return true;
            }
        }
        false
    }
    fn implements_trait(&self, tp: &STType, ctx: &Ctx) -> bool {
        let origin_mod = unsafe { ctx.origin_mod.as_ref().unwrap() };
        if tp.path == origin_mod.path {
            let plmod = origin_mod;
            if self.implements_trait_curr_mod(tp, plmod) {
                return true;
            }
            return false;
        }
        let plmod = &ctx.db.get_module(tp.path).unwrap();
        if self.implements_trait_curr_mod(tp, plmod) {
            return true;
        }
        let p = self.get_mod_path();
        if p.as_ref().map(|v| v.as_str()) == Some(ctx.plmod.path.as_str()) {
            let plmod = &ctx.plmod;
            if self.implements_trait_curr_mod(tp, plmod) {
                return true;
            }
            return false;
        }
        ctx.db
            .get_module(p.unwrap_or_default().into_owned())
            .map(|plmod| self.implements_trait_curr_mod(tp, &plmod))
            .unwrap_or_default()
    }
}

impl TraitImplAbleWithGeneric for STType {}
impl TraitImplAbleWithGeneric for UnionType {}
impl TraitImplAbleWithGeneric for ARRType {}

pub trait CanGenCode {
    fn gen_code<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<Arc<RefCell<PLType>>, PLDiag>;
}

impl CanGenCode for STType {
    fn gen_code<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<Arc<RefCell<PLType>>, PLDiag> {
        STType::gen_code(self, ctx, builder)
    }
}

impl CanGenCode for UnionType {
    fn gen_code<'a, 'b>(
        &self,
        ctx: &'b mut Ctx<'a>,
        builder: &'b BuilderEnum<'a, '_>,
    ) -> Result<Arc<RefCell<PLType>>, PLDiag> {
        UnionType::gen_code(self, ctx, builder).map(|v| Arc::new(RefCell::new(PLType::Union(v))))
    }
}
