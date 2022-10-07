use crossbeam_channel::Sender;
use lsp_server::Message;
use lsp_types::Diagnostic;

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
