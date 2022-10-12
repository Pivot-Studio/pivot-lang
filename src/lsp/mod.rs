//! # lsp
//! pivot-lang language server entry
//! current features:
//! - diagnostics
//! - completion
//! - goto definition
//! - find references
use std::error::Error;

pub mod dispatcher;
pub mod helpers;
pub mod mem_docs;
pub mod semantic_tokens;

use lsp_types::{
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
    request::{Completion, GotoDefinition, References, SemanticTokensFullRequest},
    InitializeParams, OneOf, SemanticTokenModifier, SemanticTokenType, SemanticTokensOptions,
    ServerCapabilities, TextDocumentSyncKind, TextDocumentSyncOptions,
};

use lsp_server::{Connection, Message};

use mem_docs::MemDocs;

use crate::{
    ast::{
        compiler::{ActionType, Compiler, Options},
        range::Pos,
    },
    lsp::{dispatcher::Dispatcher, helpers::url_to_path},
};

pub fn start_lsp() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting pivot-lang LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        definition_provider: Some(OneOf::Left(true)),
        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                change: Some(TextDocumentSyncKind::INCREMENTAL), // TODO incremental
                open_close: Some(true),
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
        semantic_tokens_provider: Some(
            lsp_types::SemanticTokensServerCapabilities::SemanticTokensOptions(
                SemanticTokensOptions {
                    work_done_progress_options: Default::default(),
                    legend: lsp_types::SemanticTokensLegend {
                        token_types: vec![
                            SemanticTokenType::NAMESPACE,
                            SemanticTokenType::TYPE,
                            SemanticTokenType::CLASS,
                            SemanticTokenType::ENUM,
                            SemanticTokenType::INTERFACE,
                            SemanticTokenType::STRUCT,
                            SemanticTokenType::TYPE_PARAMETER,
                            SemanticTokenType::FUNCTION,
                            SemanticTokenType::METHOD,
                            SemanticTokenType::PROPERTY,
                            SemanticTokenType::MACRO,
                            SemanticTokenType::VARIABLE,
                            SemanticTokenType::PARAMETER,
                            SemanticTokenType::ENUM_MEMBER,
                            SemanticTokenType::STRING,
                            SemanticTokenType::NUMBER,
                            SemanticTokenType::KEYWORD,
                            SemanticTokenType::MODIFIER,
                            SemanticTokenType::COMMENT,
                            SemanticTokenType::REGEXP,
                            SemanticTokenType::OPERATOR,
                        ],
                        token_modifiers: vec![
                            SemanticTokenModifier::DECLARATION,
                            SemanticTokenModifier::DEFINITION,
                            SemanticTokenModifier::READONLY,
                            SemanticTokenModifier::STATIC,
                            SemanticTokenModifier::ABSTRACT,
                            SemanticTokenModifier::DEPRECATED,
                            SemanticTokenModifier::ASYNC,
                            SemanticTokenModifier::MODIFICATION,
                            SemanticTokenModifier::DOCUMENTATION,
                        ],
                    },
                    range: Some(true),
                    full: Some(lsp_types::SemanticTokensFullOptions::Bool(true)),
                },
            ),
        ),
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
    eprintln!("starting main loop");
    let c = Compiler::new();
    for msg in &connection.receiver {
        let di = Dispatcher::new(msg.clone());
        if let Message::Request(req) = &msg {
            if connection.handle_shutdown(req)? {
                return Ok(());
            }
        }
        di.on::<GotoDefinition, _>(|id, params| {
            let uri = url_to_path(params.text_document_position_params.text_document.uri);
            let pos = Pos::from_diag_pos(&params.text_document_position_params.position);
            c.compile_dry(
                &uri,
                &docs,
                Options {
                    ..Default::default()
                },
                &connection.sender,
                Some((pos, id, None, ActionType::GotoDef)),
            );
        })
        .on::<References, _>(|id, params| {
            let uri = url_to_path(params.text_document_position.text_document.uri);
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
        })
        .on::<Completion, _>(|id, params| {
            let uri = url_to_path(params.text_document_position.text_document.uri);
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
        })
        .on::<SemanticTokensFullRequest, _>(|id, params| {
            let uri = url_to_path(params.text_document.uri);
            c.compile_dry(
                &uri,
                &docs,
                Options {
                    ..Default::default()
                },
                &connection.sender,
                Some((
                    Pos {
                        ..Default::default()
                    },
                    id,
                    None,
                    ActionType::SemanticTokensFull,
                )),
            );
        })
        .on_noti::<DidChangeTextDocument, _>(|params| {
            let f = url_to_path(params.text_document.uri);
            for content_change in params.content_changes.iter() {
                docs.change(
                    content_change.range.unwrap().clone(),
                    f.clone(),
                    content_change.text.clone(),
                );
            }
        })
        .on_noti::<DidOpenTextDocument, _>(|params| {
            let f = url_to_path(params.text_document.uri);
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
        })
        .on_noti::<DidCloseTextDocument, _>(|params| {
            let f = url_to_path(params.text_document.uri);
            docs.remove(&f);
        });
    }
    Ok(())
}
