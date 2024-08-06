//! # Lsp
//!
//! contains some lsp releated ctx methods.
//!
//! For complex features like `references` or `completion`, please
//! see separate files
use lsp_types::InlayHintKind;

use lsp_types::InlayHint;
use lsp_types::InlayHintLabelPart;
use lsp_types::MarkupContent;
use ustr::Ustr;

use super::super::pltype::PLType;

use std::cell::RefCell;

use std::sync::Arc;

use crate::lsp::semantic_tokens::type_index;

use lsp_types::SemanticTokenType;

use crate::utils::url_from_path;

use lsp_types::Location;

use super::super::plmod::LSPDef;

use lsp_types::Hover;

use lsp_types::HoverContents;

use lsp_types::MarkedString;

use super::super::node::NodeEnum;

use super::super::range::Range;

use super::Ctx;

impl Ctx<'_> {
    pub fn save_if_comment_doc_hover(&self, range: Range, docs: Option<Vec<Box<NodeEnum>>>) {
        if self.need_highlight.borrow().ne(&0) {
            return;
        }
        let mut content = vec![];
        let mut string = String::new();
        if let Some(docs) = docs {
            for doc in docs {
                if let NodeEnum::Comment(c) = *doc {
                    string.push_str(&c.comment);
                    string.push('\n');
                }
            }
        }
        content.push(MarkedString::String(string));
        self.save_if_hover(range, HoverContents::Array(content))
    }

    /// # save_if_var_hover
    ///
    /// save infomation to display if the variable is hovered
    pub fn save_if_var_hover(&self, range: Range, tp: &PLType) {
        if self.need_highlight.borrow().ne(&0) {
            return;
        }
        let mut content = vec![];
        let mut string = String::new();
        string.push_str(&tp.get_name());
        string.push_str("\n\n");
        string.push_str(&tp.get_docs().unwrap_or("".to_string()));
        content.push(MarkedString::String(string));
        self.save_if_hover(range, HoverContents::Array(content))
    }

    pub fn save_if_hover(&self, range: Range, value: HoverContents) {
        if self.need_highlight.borrow().ne(&0) {
            return;
        }
        self.plmod.hovers.borrow_mut().insert(
            range,
            Hover {
                range: None,
                contents: value,
            },
        );
    }

    pub fn send_if_go_to_def(&self, range: Range, destrange: Range, file: Ustr) {
        if self.need_highlight.borrow().ne(&0) {
            return;
        }
        if range == Default::default() {
            return;
        }
        if self.plmod.path != self.get_root_ctx().plmod.path {
            return;
        }
        self.get_root_ctx().plmod.defs.borrow_mut().insert(
            range,
            LSPDef::Scalar(Location {
                uri: url_from_path(&file),
                range: destrange.to_diag_range(),
            }),
        );
    }

    /// # push_semantic_token
    ///
    /// push a semantic token with range for highlight
    pub fn push_semantic_token(&self, range: Range, tp: SemanticTokenType, modifiers: u32) {
        if self.need_highlight.borrow().ne(&0) || self.in_macro {
            return;
        }
        self.get_root_ctx()
            .plmod
            .semantic_tokens_builder
            .borrow_mut()
            .push(range.to_diag_range(), type_index(tp), modifiers)
    }
    pub fn push_type_hints(&self, range: Range, pltype: Arc<RefCell<PLType>>) {
        let ori_mod = unsafe { &*self.origin_mod as &crate::ast::plmod::Mod };
        if self.need_highlight.borrow().ne(&0) || self.in_macro || ori_mod.path != self.plmod.path {
            return;
        }
        let colon = InlayHintLabelPart {
            value: ": ".to_string(),
            ..Default::default()
        };
        let type_label = InlayHintLabelPart {
            value: pltype.borrow().get_name().to_string(),
            tooltip: Some(lsp_types::InlayHintLabelPartTooltip::MarkupContent(
                MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: pltype.borrow().get_docs().unwrap_or("".to_string()),
                },
            )),
            ..Default::default()
        };

        let hint = InlayHint {
            position: range.to_diag_range().end,
            label: lsp_types::InlayHintLabel::LabelParts(vec![colon, type_label]),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        };
        self.plmod.hints.borrow_mut().push(hint);
    }
    pub fn push_param_hint(&self, range: Range, name: Ustr) {
        let ori_mod = unsafe { &*self.origin_mod as &crate::ast::plmod::Mod };
        if self.need_highlight.borrow().ne(&0) || self.in_macro || ori_mod.path != self.plmod.path {
            return;
        }
        let hint = InlayHint {
            position: range.to_diag_range().start,
            label: lsp_types::InlayHintLabel::String(name.to_string() + ":"),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: None,
            padding_left: None,
            padding_right: None,
            data: None,
        };
        self.plmod.hints.borrow_mut().push(hint);
    }
}
