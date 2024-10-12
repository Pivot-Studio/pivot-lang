//! # Completion
//!
//! module for auto-completion lsp feature
use super::super::plmod::CompletionItemWrapper;

use super::super::range::Range;

use super::super::plmod::GlobalType;
use super::BUILTIN_FN_NAME_MAP;
use super::BUILTIN_FN_SNIPPET_MAP;

use lsp_types::InsertTextFormat;

use lsp_types::Command;
use ustr::Ustr;

use crate::ast::tokens::TokenType;
use crate::skip_if_not_modified_by;

use super::super::pltype::PLType;

use lsp_types::CompletionItemKind;

use super::super::plmod::Mod;

use rustc_hash::FxHashMap;

use lsp_types::CompletionItem;

use super::Ctx;

impl Ctx<'_> {
    /// # completion_alternatives
    ///
    /// completion_alternatives returns all alternatives for completation in current context scope
    pub fn completion_alternatives(&self) -> Vec<CompletionItem> {
        let mut m = FxHashMap::default();
        self.get_pltp_completions(&mut m, |_| true);
        self.plmod.get_ns_completions_pri(&mut m);
        self.get_var_completions(&mut m);
        self.get_keyword_completions(&mut m);
        self.get_macro_completions(&mut m);
        self.get_root_ctx().plmod.get_glob_var_completions(&mut m);

        let cm = m.values().cloned().collect();
        cm
    }

    pub fn get_completions_in_ns(&self, ns: Ustr) -> Vec<CompletionItem> {
        let mut m = FxHashMap::default();
        self.get_glob_var_completions_in_ns(ns, &mut m);
        self.get_type_completions_in_ns(ns, &mut m);
        self.get_macro_completion_in_ns(ns, &mut m);

        let cm = m.values().cloned().collect();
        cm
    }

    pub(crate) fn with_ns(&self, ns: Ustr, f: impl FnOnce(&Mod)) {
        let ns = self.plmod.submods.get(&ns);
        if let Some(ns) = ns {
            f(ns);
        }
    }

    pub(crate) fn get_glob_var_completions_in_ns(
        &self,
        ns: Ustr,
        m: &mut FxHashMap<Ustr, CompletionItem>,
    ) {
        self.with_ns(ns, |ns| ns.get_glob_var_completions(m));
    }

    pub(crate) fn get_type_completions_in_ns(
        &self,
        ns: Ustr,
        m: &mut FxHashMap<Ustr, CompletionItem>,
    ) {
        self.with_ns(ns, |ns| {
            for (k, v) in ns.types.iter() {
                if !v.visibal_outside() {
                    continue;
                }
                let mut insert_text = None;
                let mut command = None;
                let tp = match &*v.clone().borrow() {
                    PLType::Struct(s) => {
                        skip_if_not_modified_by!(s.modifier, TokenType::PUB);
                        CompletionItemKind::STRUCT
                    }
                    PLType::Fn(fnvalue) => {
                        skip_if_not_modified_by!(fnvalue.fntype.modifier, TokenType::PUB);
                        insert_text = Some(fnvalue.gen_snippet());
                        command = Some(Command::new(
                            "trigger help".to_string(),
                            "editor.action.triggerParameterHints".to_string(),
                            None,
                        ));
                        CompletionItemKind::FUNCTION
                    }
                    _ => continue, // skip completion for primary types
                };
                if k.starts_with('|') {
                    // skip method
                    continue;
                }
                let mut item = CompletionItem {
                    label: k.to_string(),
                    kind: Some(tp),
                    insert_text: insert_text.map(|x| x.to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    command,
                    ..Default::default()
                };
                item.detail = Some(k.to_string());
                m.insert(*k, item);
            }
        });
    }

    pub(crate) fn get_macro_completion_in_ns(
        &self,
        ns: Ustr,
        m: &mut FxHashMap<Ustr, CompletionItem>,
    ) {
        self.with_ns(ns, |ns| {
            ns.get_macro_completions(m);
        });
    }

    pub fn get_type_completions(&self) -> Vec<CompletionItem> {
        let mut m = FxHashMap::default();
        self.get_pltp_completions(&mut m, |tp| !matches!(&*tp.borrow(), PLType::Fn(_)));
        self.plmod.get_ns_completions_pri(&mut m);
        m.values().cloned().collect()
    }

    pub(crate) fn get_var_completions(&self, vmap: &mut FxHashMap<Ustr, CompletionItem>) {
        if self.as_root {
            return;
        }
        for (k, _) in self.table.iter() {
            vmap.insert(
                *k,
                CompletionItem {
                    label: k.to_string(),
                    kind: Some(CompletionItemKind::VARIABLE),
                    ..Default::default()
                },
            );
        }
        if let Some(father) = self.parent {
            father.get_var_completions(vmap);
        }
    }

    pub(crate) fn get_macro_completions(&self, vmap: &mut FxHashMap<Ustr, CompletionItem>) {
        self.plmod.get_macro_completions(vmap);
        if let Some(father) = self.parent {
            father.get_macro_completions(vmap);
        }
    }

    pub(crate) fn get_builtin_completions(&self, vmap: &mut FxHashMap<Ustr, CompletionItem>) {
        for (name, handle) in BUILTIN_FN_NAME_MAP.iter() {
            vmap.insert(
                (*name).into(),
                CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    insert_text: Some(BUILTIN_FN_SNIPPET_MAP.get(handle).unwrap().to_string()),
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    ..Default::default()
                },
            );
        }
    }

    pub(crate) fn get_pltp_completions(
        &self,
        vmap: &mut FxHashMap<Ustr, CompletionItem>,
        filter: impl Fn(&GlobalType) -> bool,
    ) {
        self.plmod
            .get_pltp_completions(vmap, &filter, &self.generic_types, true);
        self.get_builtin_completions(vmap);
        if let Some(father) = self.parent {
            father.get_pltp_completions(vmap, filter);
        }
    }

    /// # should_gen
    ///
    /// it check whether a completion should be generated if it the mod is never generated before,
    /// and the editing position is located inside the range.
    pub fn should_gen(&self, range: Range) -> bool {
        if let Some(pos) = self.edit_pos {
            return pos.is_in(range) && !self.plmod.completion_gened.borrow().is_true();
        }
        false
    }

    /// # generate_completion_if
    ///
    /// generate_completion_if will call get_completions to retrieve all alternative completions
    /// when the should_gen is true for LSP completion.
    pub fn generate_completion_if(
        &self,
        should_gen: bool,
        get_completions: impl FnOnce() -> Vec<CompletionItem>,
    ) {
        if !should_gen {
            return;
        }
        let comps = get_completions();
        let comps = comps.iter().map(|x| CompletionItemWrapper(x.clone()));
        self.plmod.completions.borrow_mut().truncate(0);
        self.plmod.completions.borrow_mut().extend(comps);
        self.plmod.completion_gened.borrow_mut().set_true();
    }

    pub(crate) fn get_keyword_completions(&self, vmap: &mut FxHashMap<Ustr, CompletionItem>) {
        let keywords = vec![
            "if", "else", "while", "for", "return", "struct", "let", "true", "false", "as", "is",
            "gen", "yield", "match", "async", "await",
        ];
        let loopkeys = vec!["break", "continue"];
        let toplevel = vec![
            "fn", "struct", "const", "var", "use", "impl", "trait", "pub", "type", "gen", "where",
            "async",
        ];
        if self.parent.is_none() {
            for k in toplevel {
                vmap.insert(
                    k.into(),
                    CompletionItem {
                        label: k.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..Default::default()
                    },
                );
            }
        } else {
            for k in keywords {
                vmap.insert(
                    k.into(),
                    CompletionItem {
                        label: k.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..Default::default()
                    },
                );
            }
        }
        if self.break_block.is_some() && self.continue_block.is_some() {
            for k in loopkeys {
                vmap.insert(
                    k.into(),
                    CompletionItem {
                        label: k.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..Default::default()
                    },
                );
            }
        }
    }
}

impl Mod {
    pub(crate) fn get_glob_var_completions(&self, m: &mut FxHashMap<Ustr, CompletionItem>) {
        for (k, v) in self.global_table.iter() {
            let mut item = CompletionItem {
                label: k.to_string(),
                kind: Some(CompletionItemKind::CONSTANT),
                ..Default::default()
            };
            item.detail = Some(v.tp.borrow().get_name().to_string());
            m.insert(*k, item);
        }
    }
}
