use std::{cell::RefCell, sync::Arc};

use lsp_types::{Command, CompletionItem, CompletionItemKind, InsertTextFormat};
use rustc_hash::FxHashMap;

use crate::{
    ast::{
        ctx::Ctx,
        diag::{ErrorCode, PLDiag},
        node::RangeTrait,
        tokens::TokenType,
        traits::CustomType,
    },
    format_label,
};

use super::FNValue;

pub trait ImplAble: RangeTrait + CustomType + TraitImplAble {
    fn get_method_table(&self) -> Arc<RefCell<FxHashMap<String, Arc<RefCell<FNValue>>>>>;
    fn get_method(&self, name: &str) -> Option<Arc<RefCell<FNValue>>> {
        let binding = self.get_method_table();
        let table = binding.borrow();
        table.get(name).cloned()
    }
    fn add_method(&self, name: &str, value: Arc<RefCell<FNValue>>) -> Result<(), PLDiag> {
        let range = self.range();
        let binding = self.get_method_table();
        let mut table = binding.borrow_mut();
        if let Some(v) = table.get(name) {
            return Err(range
                .new_err(ErrorCode::DUPLICATE_METHOD)
                .add_label(
                    v.borrow().range,
                    v.borrow().path.clone(),
                    format_label!("Previously defined here"),
                )
                .clone());
        }
        table.insert(name.to_owned(), value);
        Ok(())
    }
    fn get_mthd_completions(&self, ctx: &Ctx) -> Vec<CompletionItem> {
        let pub_only = self.get_path() != ctx.plmod.path;
        let mut completions = Vec::new();
        let mut f = |name: &String, v: &FNValue| {
            if pub_only && !v.is_modified_by(TokenType::PUB) {
                return;
            }
            completions.push(CompletionItem {
                kind: Some(CompletionItemKind::METHOD),
                label: name.clone(),
                detail: Some("method".to_string()),
                insert_text: Some(v.gen_snippet()),
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
            f(name, &v.clone().borrow());
        }
        completions
    }
}

pub trait TraitImplAble {
    fn get_full_name_except_generic(&self) -> String;
    fn get_full_name(&self) -> String;
}
