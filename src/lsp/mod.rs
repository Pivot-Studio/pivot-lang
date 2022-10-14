//! # lsp
//! pivot-lang language server entry
//! current features:
//! - diagnostics
//! - completion
//! - goto definition
//! - find references
use std::{cell::RefCell, error::Error, sync::Arc, thread::available_parallelism, time::Instant};

pub mod dispatcher;
pub mod helpers;
pub mod mem_docs;
pub mod semantic_tokens;

use lsp_types::{
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
    request::{
        Completion, GotoDefinition, References, SemanticTokensFullDeltaRequest,
        SemanticTokensFullRequest,
    },
    InitializeParams, OneOf, SemanticTokenModifier, SemanticTokenType, SemanticTokensDelta,
    SemanticTokensOptions, ServerCapabilities, TextDocumentSyncKind, TextDocumentSyncOptions,
};

use lsp_server::{Connection, Message};

use mem_docs::MemDocs;
use threadpool::ThreadPool;

use crate::{
    ast::{
        accumulators::{Completions, Diagnostics, GotoDef, PLReferences, PLSemanticTokens},
        compiler::{compile_dry, ActionType, Options},
        range::Pos,
    },
    db,
    lsp::{
        dispatcher::Dispatcher,
        helpers::{
            send_completions, send_diagnostics, send_goto_def, send_references,
            send_semantic_tokens, send_semantic_tokens_edit, url_to_path,
        },
        mem_docs::MemDocsInput,
        semantic_tokens::diff_tokens,
    },
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
            trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
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
                    range: Some(false),
                    full: Some(lsp_types::SemanticTokensFullOptions::Delta { delta: Some(true) }),
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
    let n_workers = available_parallelism().unwrap().get();
    let pool = ThreadPool::new(n_workers);
    let mut db = db::Database::default();
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    let docs = Arc::new(RefCell::new(MemDocs::new()));
    let docin = MemDocsInput::new(&db, docs.clone(), "".to_string());
    let mut tokens = vec![];

    eprintln!("starting main loop");
    for msg in &connection.receiver {
        let now = Instant::now();
        let di = Dispatcher::new(msg.clone());
        if let Message::Request(req) = &msg {
            if connection.handle_shutdown(req)? {
                return Ok(());
            }
        }
        di.on::<GotoDefinition, _>(|id, params| {
            let uri = url_to_path(params.text_document_position_params.text_document.uri);
            let pos = Pos::from_diag_pos(&params.text_document_position_params.position);
            docin.set_file(&mut db).to(uri);
            compile_dry(
                &db,
                docin,
                Options {
                    ..Default::default()
                },
                ActionType::GotoDef,
                Some((pos, id.clone(), None, ActionType::GotoDef)),
            );
            let defs = compile_dry::accumulated::<GotoDef>(
                &db,
                docin,
                Options {
                    ..Default::default()
                },
                ActionType::GotoDef,
                Some((pos, id.clone(), None, ActionType::GotoDef)),
            );
            let sender = connection.sender.clone();
            if !defs.is_empty() {
                pool.execute(move || {
                    send_goto_def(&sender, id, defs[0].clone());
                });
            }
        })
        .on::<References, _>(|id, params| {
            let uri = url_to_path(params.text_document_position.text_document.uri);
            let pos = Pos::from_diag_pos(&params.text_document_position.position);
            docin.set_file(&mut db).to(uri);
            compile_dry(
                &db,
                docin,
                Options {
                    ..Default::default()
                },
                ActionType::FindReferences,
                Some((pos, id.clone(), None, ActionType::FindReferences)),
            );
            let refs = compile_dry::accumulated::<PLReferences>(
                &db,
                docin,
                Options {
                    ..Default::default()
                },
                ActionType::FindReferences,
                Some((pos, id.clone(), None, ActionType::FindReferences)),
            );
            let sender = connection.sender.clone();
            pool.execute(move || {
                send_references(&sender, id, &refs);
            });
        })
        .on::<Completion, _>(|id, params| {
            let uri = url_to_path(params.text_document_position.text_document.uri);
            let pos = Pos::from_diag_pos(&params.text_document_position.position);
            let mut trigger = None;
            if params.context.is_some() {
                trigger = params.context.unwrap().trigger_character;
            }
            docin.set_file(&mut db).to(uri);
            compile_dry(
                &db,
                docin,
                Options {
                    ..Default::default()
                },
                ActionType::Completion,
                Some((pos, id.clone(), trigger.clone(), ActionType::Completion)),
            );
            let completions = compile_dry::accumulated::<Completions>(
                &db,
                docin,
                Options {
                    ..Default::default()
                },
                ActionType::Completion,
                Some((pos, id.clone(), trigger, ActionType::Completion)),
            );
            if !completions.is_empty() {
                let sender = connection.sender.clone();
                pool.execute(move || {
                    send_completions(&sender, id, completions[0].clone());
                });
            }
        })
        .on::<SemanticTokensFullRequest, _>(|id, params| {
            let uri = url_to_path(params.text_document.uri);
            docin.set_file(&mut db).to(uri);
            let newtokens = compile_dry::accumulated::<PLSemanticTokens>(
                &db,
                docin,
                Options {
                    ..Default::default()
                },
                ActionType::SemanticTokensFull,
                Some((
                    Pos {
                        ..Default::default()
                    },
                    id.clone(),
                    None,
                    ActionType::SemanticTokensFull,
                )),
            );
            tokens = newtokens[0].data.clone();
            let sender = connection.sender.clone();
            pool.execute(move || {
                send_semantic_tokens(&sender, id, newtokens[0].clone());
            });
        })
        .on::<SemanticTokensFullDeltaRequest, _>(|id, params| {
            let uri = url_to_path(params.text_document.uri);
            docin.set_file(&mut db).to(uri);
            let newtokens = compile_dry::accumulated::<PLSemanticTokens>(
                &db,
                docin,
                Options {
                    ..Default::default()
                },
                ActionType::SemanticTokensFull,
                Some((
                    Pos {
                        ..Default::default()
                    },
                    id.clone(),
                    None,
                    ActionType::SemanticTokensFull,
                )),
            );
            let delta = diff_tokens(&tokens, &newtokens[0].data);
            tokens = newtokens[0].data.clone();
            let sender = connection.sender.clone();
            pool.execute(move || {
                send_semantic_tokens_edit(
                    &sender,
                    id,
                    SemanticTokensDelta {
                        result_id: None,
                        edits: delta,
                    },
                );
            });
        })
        .on_noti::<DidChangeTextDocument, _>(|params| {
            let f = url_to_path(params.text_document.uri);
            for content_change in params.content_changes.iter() {
                docs.borrow_mut().change(
                    content_change.range.unwrap().clone(),
                    f.clone(),
                    content_change.text.clone(),
                );
                docin.set_docs(&mut db).to(docs.clone());
            }
            compile_dry(
                &db,
                docin,
                Options {
                    ..Default::default()
                },
                ActionType::Diagnostic,
                None,
            );
            let diags = compile_dry::accumulated::<Diagnostics>(
                &db,
                docin,
                Options {
                    ..Default::default()
                },
                ActionType::Diagnostic,
                None,
            );
            let sender = connection.sender.clone();
            pool.execute(move || {
                send_diagnostics(&sender, f, diags.clone());
            });
        })
        .on_noti::<DidOpenTextDocument, _>(|params| {
            let f = url_to_path(params.text_document.uri);
            docs.borrow_mut()
                .insert(f.clone(), params.text_document.text);
            docin.set_docs(&mut db).to(docs.clone());
            docin.set_file(&mut db).to(f.clone());
            compile_dry(
                &db,
                docin,
                Options {
                    ..Default::default()
                },
                ActionType::Diagnostic,
                None,
            );
            let diags = compile_dry::accumulated::<Diagnostics>(
                &db,
                docin,
                Options {
                    ..Default::default()
                },
                ActionType::Diagnostic,
                None,
            );
            let sender = connection.sender.clone();
            pool.execute(move || {
                send_diagnostics(&sender, f.clone(), diags.clone());
            });
        })
        .on_noti::<DidCloseTextDocument, _>(|params| {
            let f = url_to_path(params.text_document.uri);
            docs.borrow_mut().remove(&f);
            docin.set_docs(&mut db).to(docs.clone());
        });
        let elapsed = now.elapsed();
        eprintln!("req finished, time: {:?}", elapsed);
    }
    Ok(())
}
