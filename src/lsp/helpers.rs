use crossbeam_channel::Sender;
use lsp_server::{Message, RequestId};
use lsp_types::{Diagnostic, SemanticTokens};

pub fn send_diagnostics(sender: &Sender<Message>, uri: String, diagnostics: Vec<Diagnostic>) {
    sender
        .send(Message::Notification(lsp_server::Notification::new(
            "textDocument/publishDiagnostics".to_string(),
            serde_json::to_value(lsp_types::PublishDiagnosticsParams {
                uri: lsp_types::Url::from_file_path(&uri).unwrap(),
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

pub fn send_semantic_tokens(sender: &Sender<Message>, id: RequestId, tokens: SemanticTokens) {
    sender
        .send(Message::Response(lsp_server::Response::new_ok(
            id,
            Some(serde_json::to_value(tokens).unwrap()),
        )))
        .unwrap();
}

pub fn position_to_offset(doc: &String, pos: lsp_types::Position) -> usize {
    let (line, col) = (pos.line as usize, pos.character as usize);
    let mut offset = 0;
    for (i, l) in doc.lines().enumerate() {
        if i == line {
            offset += l.char_indices().nth(col).unwrap().0;
            break;
        }
        offset += l.len() + 1;
    }
    offset
}
