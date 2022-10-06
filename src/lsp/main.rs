use std::error::Error;

use lsp_types::{
    notification::{DidChangeTextDocument, DidOpenTextDocument, Notification, PublishDiagnostics},
    request::GotoDefinition,
    Diagnostic, GotoDefinitionResponse, InitializeParams, OneOf, Position,
    PublishDiagnosticsParams, ServerCapabilities, TextDocumentEdit, TextDocumentSyncKind,
    TextDocumentSyncOptions,
};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting generic LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        definition_provider: Some(OneOf::Left(true)),
        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                change: Some(TextDocumentSyncKind::INCREMENTAL),
                ..Default::default()
            },
        )),

        ..Default::default()
    })
    .unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    eprintln!("starting example main loop");
    for msg in &connection.receiver {
        eprintln!("got msg: {:?}", msg);
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {:?}", req);
                match cast::<GotoDefinition>(req) {
                    Ok((id, params)) => {
                        eprintln!("got gotoDefinition request #{}: {:?}", id, params);
                        let result = Some(GotoDefinitionResponse::Array(Vec::new()));
                        let result = serde_json::to_value(&result).unwrap();
                        let resp = Response {
                            id,
                            result: Some(result),
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{:?}", err),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                // ...
            }
            Message::Response(resp) => {
                eprintln!("got response: {:?}", resp);
            }
            Message::Notification(not) => {
                eprintln!("got notification: {:?}", not);

                match cast_noti::<DidChangeTextDocument>(not.clone()) {
                    Ok(params) => {
                        eprintln!("got doc edit noti: {:?}", params);
                        let result = vec![Diagnostic::new_simple(
                            lsp_types::Range {
                                start: Position {
                                    line: 1,
                                    character: 1,
                                },
                                end: Position {
                                    line: 1,
                                    character: 2,
                                },
                            },
                            "test diag".to_string(),
                        )];
                        let resp =
                            PublishDiagnosticsParams::new(params.text_document.uri, result, None);
                        let result = serde_json::to_value(&resp).unwrap();
                        connection.sender.send(
                            lsp_server::Notification::new(
                                PublishDiagnostics::METHOD.to_string(),
                                result,
                            )
                            .into(),
                        )?;
                        eprintln!("diag sent");
                        continue;
                    }
                    Err(e) => {
                        eprintln!("{:?}", e)
                    }
                }
                match cast_noti::<DidOpenTextDocument>(not) {
                    Ok(params) => {
                        eprintln!("got doc edit noti: {:?}", params);
                        let result = vec![Diagnostic::new_simple(
                            lsp_types::Range {
                                start: Position {
                                    line: 1,
                                    character: 1,
                                },
                                end: Position {
                                    line: 1,
                                    character: 2,
                                },
                            },
                            "test diag".to_string(),
                        )];
                        let resp =
                            PublishDiagnosticsParams::new(params.text_document.uri, result, None);
                        let result = serde_json::to_value(&resp).unwrap();
                        connection.sender.send(
                            lsp_server::Notification::new(
                                PublishDiagnostics::METHOD.to_string(),
                                result,
                            )
                            .into(),
                        )?;
                        eprintln!("diag sent");
                        continue;
                    }
                    Err(e) => {
                        eprintln!("{:?}", e)
                    }
                }
            }
        }
    }
    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
fn cast_noti<R>(
    req: lsp_server::Notification,
) -> Result<R::Params, ExtractError<lsp_server::Notification>>
where
    R: lsp_types::notification::Notification,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
