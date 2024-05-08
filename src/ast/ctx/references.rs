//! # References
//!
//! This module is used to provide `find references` feature.
//!
//! ## General
//!
//! There are two types of references:
//!
//! 1. Global references: `refs_map` is used to store them.
//! 2. Local references: `local_refs` is used to store them.
//!
//! this function is used to set local references.
//!
//! Generally, local references are used to store references of symbols
//! which can only be used in the current file, such as local variables,
//! generic types, etc. Global references are used to store references
//! of symbols which can be used in other files, such as functions,
//! types, etc.
use std::{cell::RefCell, sync::Arc};

use lsp_types::*;
use ustr::Ustr;

use super::super::{
    ctx::Ctx,
    plmod::MutVec,
    pltype::{Field, PLType},
    range::Range,
};

impl<'a> Ctx<'a> {
    /// # set_if_refs_tp
    ///
    /// This function is used to set references of types.
    ///
    /// Some of types are global references, while others are local.
    ///
    /// For details, see the module documentation.
    pub fn set_if_refs_tp(&self, tp: Arc<RefCell<PLType>>, range: Range) {
        if range == Default::default() {
            return;
        }
        if let Ok(tp) = tp.try_borrow() {
            tp.if_refs(
                |tp| {
                    let name = tp.get_full_elm_name_without_generic();
                    self.set_glob_refs(name, range)
                },
                |g| self.set_local_refs(g.refs.clone(), range),
            )
        }
    }

    /// # set_field_refs
    ///
    /// This function is used to set field references, which is a special
    /// case of global references.
    ///
    /// For details, see the module documentation.
    pub fn set_field_refs(&self, pltype: Arc<RefCell<PLType>>, f: &Field, range: Range) {
        self.set_glob_refs(
            format!(
                "{}..{}",
                &pltype.borrow().get_full_elm_name_without_generic(),
                f.name
            )
            .into(),
            range,
        );
    }

    /// # set_glob_refs
    ///
    /// This function is used to set global references.
    ///
    /// For details, see the module documentation.
    pub fn set_glob_refs(&self, name: Ustr, range: Range) {
        if self.need_highlight.borrow().ne(&0) {
            return;
        }
        let root = self.get_root_ctx();
        if root.get_file() != self.get_file() {
            return;
        }
        if range.start.line == 1 && range.start.column == 12 && root.get_file().contains("iter.pi")
        {
            eprintln!(
                "{}:{}:{}",
                root.get_file(),
                range.start.line,
                range.start.column
            );
        }
        root.plmod.glob_refs.borrow_mut().insert(range, name);
        let mut rm = root.plmod.refs_map.borrow_mut();
        if let Some(refsmap) = rm.get(&name) {
            refsmap.borrow_mut().push(root.get_location(range));
        } else {
            let v = RefCell::new(vec![]);
            v.borrow_mut().push(root.get_location(range));
            rm.insert(name, Arc::new(v));
        }
    }

    pub fn set_if_sig(&self, range: Range, name: String, params: &[Ustr], n: u32) {
        self.plmod.sig_helps.borrow_mut().insert(
            range,
            SignatureHelp {
                signatures: vec![SignatureInformation {
                    label: name,
                    documentation: None,
                    parameters: Some(
                        params
                            .iter()
                            .map(|s| ParameterInformation {
                                label: ParameterLabel::Simple(s.to_string()),
                                documentation: None,
                            })
                            .collect(),
                    ),
                    active_parameter: Some(n),
                }],
                active_signature: None,
                active_parameter: None,
            },
        );
    }

    /// # set_local_refs
    ///
    /// This function is used to set local references.
    ///
    /// For details, see the module documentation.
    pub fn set_local_refs(&self, refs: Arc<MutVec<Location>>, range: Range) {
        if self.need_highlight.borrow().ne(&0) {
            return;
        }
        refs.borrow_mut().push(self.get_location(range));
        self.plmod.local_refs.borrow_mut().insert(range, refs);
    }
}
