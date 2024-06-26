use crossbeam_channel::Sender;
use lsp_server::{Message, RequestId};
use lsp_types::{Diagnostic, DocumentSymbol, InlayHint, SemanticTokens, SemanticTokensDelta, Url};

use crate::utils::url_from_path;

pub fn send_diagnostics(sender: &Sender<Message>, uri: String, diagnostics: Vec<Diagnostic>) {
    sender
        .send(Message::Notification(lsp_server::Notification::new(
            "textDocument/publishDiagnostics".to_string(),
            serde_json::to_value(lsp_types::PublishDiagnosticsParams {
                uri: url_from_path(&uri),
                diagnostics,
                version: None,
            })
            .unwrap(),
        )))
        .unwrap();
}

pub fn send_completions(
    sender: &Sender<Message>,
    id: RequestId,
    completions: Vec<lsp_types::CompletionItem>,
) {
    sender
        .send(Message::Response(lsp_server::Response::new_ok(
            id,
            Some(serde_json::to_value(lsp_types::CompletionResponse::Array(completions)).unwrap()),
        )))
        .unwrap();
}

pub fn send_goto_def(
    sender: &Sender<Message>,
    id: RequestId,
    goto_def: lsp_types::GotoDefinitionResponse,
) {
    sender
        .send(Message::Response(lsp_server::Response::new_ok(
            id,
            Some(serde_json::to_value(goto_def).unwrap()),
        )))
        .unwrap();
}

pub fn send_references(
    sender: &Sender<Message>,
    id: RequestId,
    references: &Vec<lsp_types::Location>,
) {
    sender
        .send(Message::Response(lsp_server::Response::new_ok(
            id,
            Some(serde_json::to_value(references).unwrap()),
        )))
        .unwrap();
}

pub fn send_rename(
    sender: &Sender<Message>,
    id: RequestId,
    texts: std::collections::HashMap<Url, Vec<lsp_types::TextEdit>>,
) {
    sender
        .send(Message::Response(lsp_server::Response::new_ok(
            id,
            Some(
                serde_json::to_value(lsp_types::WorkspaceEdit {
                    changes: Some(texts),
                    ..Default::default()
                })
                .unwrap(),
            ),
        )))
        .unwrap();
}

pub fn send_format(sender: &Sender<Message>, id: RequestId, texts: Vec<lsp_types::TextEdit>) {
    sender
        .send(Message::Response(lsp_server::Response::new_ok(
            id,
            Some(serde_json::to_value(texts).unwrap()),
        )))
        .unwrap();
}

pub fn send_hints(sender: &Sender<Message>, id: RequestId, hints: Vec<InlayHint>) {
    sender
        .send(Message::Response(lsp_server::Response::new_ok(
            id,
            Some(serde_json::to_value(hints).unwrap()),
        )))
        .unwrap();
}

pub fn send_doc_symbols(sender: &Sender<Message>, id: RequestId, doc_symbols: Vec<DocumentSymbol>) {
    sender
        .send(Message::Response(lsp_server::Response::new_ok(
            id,
            Some(serde_json::to_value(doc_symbols).unwrap()),
        )))
        .unwrap();
}

pub fn send_semantic_tokens_edit(
    sender: &Sender<Message>,
    id: RequestId,
    tokens: SemanticTokensDelta,
) {
    sender
        .send(Message::Response(lsp_server::Response::new_ok(
            id,
            Some(serde_json::to_value(tokens).unwrap()),
        )))
        .unwrap();
}

pub fn send_semantic_tokens(sender: &Sender<Message>, id: RequestId, tokens: SemanticTokens) {
    sender
        .send(Message::Response(lsp_server::Response::new_ok(
            id,
            Some(serde_json::to_value(tokens).unwrap()),
        )))
        .unwrap();
}

pub fn send_code_lens(
    sender: &Sender<Message>,
    id: RequestId,
    code_lens: Vec<lsp_types::CodeLens>,
) {
    sender
        .send(Message::Response(lsp_server::Response::new_ok(
            id,
            Some(serde_json::to_value(code_lens).unwrap()),
        )))
        .unwrap();
}

pub fn send_hover(sender: &Sender<Message>, id: RequestId, hover: lsp_types::Hover) {
    sender
        .send(Message::Response(lsp_server::Response::new_ok(
            id,
            Some(serde_json::to_value(hover).unwrap()),
        )))
        .unwrap();
}

pub fn send_signature_help(
    sender: &Sender<Message>,
    id: RequestId,
    help: lsp_types::SignatureHelp,
) {
    sender
        .send(Message::Response(lsp_server::Response::new_ok(
            id,
            Some(serde_json::to_value(help).unwrap()),
        )))
        .unwrap();
}

pub fn url_to_path(url: Url) -> String {
    #[cfg(any(unix, windows, target_os = "redox", target_os = "wasi"))]
    return crate::utils::canonicalize(url.to_file_path().unwrap().to_str().unwrap())
        .expect(&(url.to_string() + " file not exists"))
        .to_str()
        .unwrap()
        .to_string();
    #[cfg(not(any(unix, windows, target_os = "redox", target_os = "wasi")))]
    return url.to_string();
}

pub fn position_to_offset(doc: &str, pos: lsp_types::Position) -> usize {
    let le = LinesWithEndings::from(doc);
    let (line, col) = (pos.line as usize, pos.character as usize);
    let mut offset = 0;
    for (i, l) in le.enumerate() {
        if i == line {
            if i == line {
                let mut utf8_offset = 0;
                let mut utf16_offset = 0;
                for c in l.chars() {
                    if utf16_offset >= col {
                        break;
                    }
                    utf8_offset += c.len_utf8();
                    utf16_offset += c.len_utf16();
                }
                offset += utf8_offset;
                break;
            }
            break;
        }
        offset += l.len();
    }
    offset
}

/// Iterator yielding every line in a string. The line includes newline character(s).
pub struct LinesWithEndings<'a> {
    input: &'a str,
}

impl<'a> LinesWithEndings<'a> {
    pub fn from(input: &'a str) -> LinesWithEndings<'a> {
        LinesWithEndings { input }
    }
}

impl<'a> Iterator for LinesWithEndings<'a> {
    type Item = &'a str;

    #[inline]
    fn next(&mut self) -> Option<&'a str> {
        if self.input.is_empty() {
            return None;
        }
        let split = self
            .input
            .find('\n')
            .map(|i| i + 1)
            .unwrap_or(self.input.len());
        let (line, rest) = self.input.split_at(split);
        self.input = rest;
        Some(line)
    }
}
