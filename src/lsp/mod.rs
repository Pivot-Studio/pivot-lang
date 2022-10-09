use std::error::Error;

pub mod diagnostics;
pub mod mem_docs;

use lsp_types::{
    notification::{DidChangeTextDocument, DidOpenTextDocument},
    request::{Completion, GotoDefinition, References},
    InitializeParams, OneOf, ServerCapabilities, TextDocumentSyncKind, TextDocumentSyncOptions,
};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId};

use mem_docs::MemDocs;

use crate::ast::{
    compiler::{ActionType, Compiler, Options},
    range::Pos,
};

pub fn start_lsp() -> Result<(), Box<dyn Error + Sync + Send>> {
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
                change: Some(TextDocumentSyncKind::FULL), // TODO incremental
                ..Default::default()
            },
        )),
        completion_provider: Some(lsp_types::CompletionOptions {
            trigger_characters: Some(vec![".".to_string()]),
            resolve_provider: None,
            work_done_progress_options: Default::default(),
            all_commit_characters: None,
        }),
        references_provider: Some(OneOf::Left(true)),
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
    let mut docs = MemDocs::new();
    eprintln!("starting example main loop");
    let c = Compiler::new();
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                match cast::<GotoDefinition>(req.clone()) {
                    Ok((id, params)) => {
                        let uri = params
                            .text_document_position_params
                            .text_document
                            .uri
                            .to_file_path()
                            .unwrap()
                            .to_str()
                            .unwrap()
                            .to_string();
                        let pos =
                            Pos::from_diag_pos(&params.text_document_position_params.position);
                        c.compile_dry(
                            &uri,
                            &docs,
                            Options {
                                ..Default::default()
                            },
                            &connection.sender,
                            Some((pos, id, None, ActionType::GotoDef)),
                        );
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{:?}", err),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                match cast::<References>(req.clone()) {
                    Ok((id, params)) => {
                        let uri = params
                            .text_document_position
                            .text_document
                            .uri
                            .to_file_path()
                            .unwrap()
                            .to_str()
                            .unwrap()
                            .to_string();
                        let pos = Pos::from_diag_pos(&params.text_document_position.position);
                        c.compile_dry(
                            &uri,
                            &docs,
                            Options {
                                ..Default::default()
                            },
                            &connection.sender,
                            Some((pos, id.clone(), None, ActionType::FindReferences)),
                        );
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{:?}", err),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                match cast::<Completion>(req) {
                    Ok((id, params)) => {
                        let uri = params
                            .text_document_position
                            .text_document
                            .uri
                            .to_file_path()
                            .unwrap()
                            .to_str()
                            .unwrap()
                            .to_string();
                        let pos = Pos::from_diag_pos(&params.text_document_position.position);
                        let mut trigger = None;
                        if params.context.is_some() {
                            trigger = params.context.unwrap().trigger_character;
                        }
                        c.compile_dry(
                            &uri,
                            &docs,
                            Options {
                                ..Default::default()
                            },
                            &connection.sender,
                            Some((pos, id, trigger, ActionType::Completion)),
                        );
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{:?}", err),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                // ...
            }
            Message::Response(_) => {}
            Message::Notification(not) => {
                match cast_noti::<DidChangeTextDocument>(not.clone()) {
                    Ok(params) => {
                        let f = params
                            .text_document
                            .uri
                            .to_file_path()
                            .unwrap()
                            .to_str()
                            .unwrap()
                            .to_string();
                        docs.insert(f.clone(), params.content_changes[0].text.clone());
                        c.compile_dry(
                            &f,
                            &docs,
                            Options {
                                ..Default::default()
                            },
                            &connection.sender,
                            None,
                        );
                        continue;
                    }
                    Err(e) => {
                        eprintln!("{:?}", e)
                    }
                }
                match cast_noti::<DidOpenTextDocument>(not) {
                    Ok(params) => {
                        let f = params
                            .text_document
                            .uri
                            .to_file_path()
                            .unwrap()
                            .to_str()
                            .unwrap()
                            .to_string();
                        docs.insert(f.clone(), params.text_document.text);
                        c.compile_dry(
                            &f,
                            &docs,
                            Options {
                                ..Default::default()
                            },
                            &connection.sender,
                            None,
                        );
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
