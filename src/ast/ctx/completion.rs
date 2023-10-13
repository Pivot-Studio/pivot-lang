//! # Completion
//!
//! module for auto-completion lsp feature
use super::super::plmod::CompletionItemWrapper;

use super::super::range::Range;

use super::super::plmod::GlobType;
use super::BUILTIN_FN_NAME_MAP;
use super::BUILTIN_FN_SNIPPET_MAP;

use lsp_types::InsertTextFormat;

use lsp_types::Command;

use crate::ast::tokens::TokenType;
use crate::skip_if_not_modified_by;

use super::super::pltype::PLType;

use lsp_types::CompletionItemKind;

use super::super::plmod::Mod;

use rustc_hash::FxHashMap;

use lsp_types::CompletionItem;

use super::Ctx;

impl Ctx<'_> {
    pub fn get_completions(&self) -> Vec<CompletionItem> {
        let mut m = FxHashMap::default();
        self.get_pltp_completions(&mut m, |_| true);
        self.plmod.get_ns_completions_pri(&mut m);
        self.get_var_completions(&mut m);
        self.get_keyword_completions(&mut m);
        self.get_macro_completions(&mut m);

        let cm = m.values().cloned().collect();
        cm
    }

    pub fn get_completions_in_ns(&self, ns: &str) -> Vec<CompletionItem> {
        let mut m = FxHashMap::default();
        self.get_const_completions_in_ns(ns, &mut m);
        self.get_type_completions_in_ns(ns, &mut m);
        self.get_macro_completion_in_ns(ns, &mut m);

        let cm = m.values().cloned().collect();
        cm
    }

    pub(crate) fn with_ns(&self, ns: &str, f: impl FnOnce(&Mod)) {
        let ns = self.plmod.submods.get(ns);
        if let Some(ns) = ns {
            f(ns);
        }
    }

    pub(crate) fn get_const_completions_in_ns(
        &self,
        ns: &str,
        m: &mut FxHashMap<String, CompletionItem>,
    ) {
        self.with_ns(ns, |ns| {
            for (k, v) in ns.global_table.iter() {
                let mut item = CompletionItem {
                    label: k.to_string(),
                    kind: Some(CompletionItemKind::CONSTANT),
                    ..Default::default()
                };
                item.detail = Some(v.tp.borrow().get_name());
                m.insert(k.clone(), item);
            }
        });
    }

    pub(crate) fn get_type_completions_in_ns(
        &self,
        ns: &str,
        m: &mut FxHashMap<String, CompletionItem>,
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
                    insert_text,
                    insert_text_format: Some(InsertTextFormat::SNIPPET),
                    command,
                    ..Default::default()
                };
                item.detail = Some(k.to_string());
                m.insert(k.clone(), item);
            }
        });
    }

    pub(crate) fn get_macro_completion_in_ns(
        &self,
        ns: &str,
        m: &mut FxHashMap<String, CompletionItem>,
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

    pub(crate) fn get_var_completions(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        if self.as_root {
            return;
        }
        for (k, _) in self.table.iter() {
            vmap.insert(
                k.to_string(),
                CompletionItem {
                    label: k.to_string(),
                    kind: Some(CompletionItemKind::VARIABLE),
                    ..Default::default()
                },
            );
        }
        if let Some(father) = self.father {
            father.get_var_completions(vmap);
        }
    }

    pub(crate) fn get_macro_completions(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        self.plmod.get_macro_completions(vmap);
        if let Some(father) = self.father {
            father.get_macro_completions(vmap);
        }
    }

    pub(crate) fn get_builtin_completions(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        for (name, handle) in BUILTIN_FN_NAME_MAP.iter() {
            vmap.insert(
                name.to_string(),
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
        vmap: &mut FxHashMap<String, CompletionItem>,
        filter: impl Fn(&GlobType) -> bool,
    ) {
        self.plmod
            .get_pltp_completions(vmap, &filter, &self.generic_types, true);
        self.get_builtin_completions(vmap);
        if let Some(father) = self.father {
            father.get_pltp_completions(vmap, filter);
        }
    }

    pub fn if_completion(
        &self,
        range: Range,
        get_completions: impl FnOnce() -> Vec<CompletionItem>,
    ) {
        if let Some(pos) = self.edit_pos {
            if pos.is_in(range) && !self.plmod.completion_gened.borrow().is_true() {
                let comps = get_completions();
                let comps = comps.iter().map(|x| CompletionItemWrapper(x.clone()));
                self.plmod.completions.borrow_mut().truncate(0);
                self.plmod.completions.borrow_mut().extend(comps);
                self.plmod.completion_gened.borrow_mut().set_true();
            }
        }
    }
    pub(crate) fn get_keyword_completions(&self, vmap: &mut FxHashMap<String, CompletionItem>) {
        let keywords = vec![
            "if", "else", "while", "for", "return", "struct", "let", "true", "false", "as", "is",
            "gen", "yield",
        ];
        let loopkeys = vec!["break", "continue"];
        let toplevel = vec![
            "fn", "struct", "const", "var", "use", "impl", "trait", "pub", "type", "gen",
        ];
        if self.father.is_none() {
            for k in toplevel {
                vmap.insert(
                    k.to_string(),
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
                    k.to_string(),
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
                    k.to_string(),
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