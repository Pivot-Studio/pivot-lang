//! # lsp
//! pivot-lang language server entry
//! current features:
//! - diagnostics
//! - completion
//! - goto definition
//! - find references
use std::{
    cell::RefCell,
    error::Error,
    sync::{Arc, Mutex},
    thread::available_parallelism,
    time::Instant,
};

pub mod dispatcher;
pub mod helpers;
pub mod mem_docs;
pub mod semantic_tokens;
pub mod text;
use log::debug;
use lsp_types::{
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
    request::{
        Completion, DocumentSymbolRequest, Formatting, GotoDefinition, HoverRequest,
        InlayHintRequest, References, SemanticTokensFullDeltaRequest, SemanticTokensFullRequest,
        SignatureHelpRequest,
    },
    Diagnostic, Hover, HoverContents, InitializeParams, MarkedString, OneOf, SemanticTokenModifier,
    SemanticTokenType, SemanticTokens, SemanticTokensDelta, SemanticTokensOptions,
    ServerCapabilities, SignatureHelp, TextDocumentSyncKind, TextDocumentSyncOptions,
};

use lsp_server::{Connection, Message};

use mem_docs::MemDocs;
use rustc_hash::FxHashMap;
use threadpool::ThreadPool;

use crate::{
    ast::{
        accumulators::{
            Completions, Diagnostics, DocSymbols, GotoDef, Hints, PLFormat, PLHover, PLReferences,
            PLSemanticTokens, PLSignatureHelp,
        },
        compiler::{compile_dry, ActionType},
        range::Pos,
    },
    db,
    lsp::{
        dispatcher::Dispatcher,
        helpers::{
            send_completions, send_diagnostics, send_doc_symbols, send_format, send_goto_def,
            send_hints, send_hover, send_references, send_semantic_tokens,
            send_semantic_tokens_edit, send_signature_help, url_to_path,
        },
        mem_docs::MemDocsInput,
        semantic_tokens::diff_tokens,
    },
};

pub fn start_lsp() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    log::info!("starting pivot-lang LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        definition_provider: Some(OneOf::Left(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                change: Some(TextDocumentSyncKind::INCREMENTAL), // TODO incremental
                open_close: Some(true),
                ..Default::default()
            },
        )),
        inlay_hint_provider: Some(OneOf::Left(true)),
        completion_provider: Some(lsp_types::CompletionOptions {
            trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
            resolve_provider: None,
            work_done_progress_options: Default::default(),
            all_commit_characters: None,
            completion_item: None,
        }),
        document_symbol_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
        signature_help_provider: Some(lsp_types::SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: None,
            work_done_progress_options: Default::default(),
        }),
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
    log::info!("shutting down server");
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
    let docs = Arc::new(Mutex::new(RefCell::new(MemDocs::new())));
    let docin = MemDocsInput::new(
        &db,
        docs.clone(),
        "".to_string(),
        Default::default(),
        ActionType::Diagnostic,
        None,
        None,
    );
    let mut tokens = FxHashMap::default();
    let mut completions: Vec<Vec<lsp_types::CompletionItem>> = vec![];

    log::info!("starting main loop");
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
            docin.set_action(&mut db).to(ActionType::GotoDef);
            docin.set_params(&mut db).to(Some((pos, None)));
            compile_dry(&db, docin);
            let defs = compile_dry::accumulated::<GotoDef>(&db, docin);
            let sender = connection.sender.clone();
            if !defs.is_empty() {
                pool.execute(move || {
                    send_goto_def(&sender, id, defs[0].clone());
                });
            }
        })
        .on::<HoverRequest, _>(|id, params| {
            let uri = url_to_path(params.text_document_position_params.text_document.uri);
            let pos = Pos::from_diag_pos(&params.text_document_position_params.position);
            docin.set_file(&mut db).to(uri);
            docin.set_action(&mut db).to(ActionType::Hover);
            docin.set_params(&mut db).to(Some((pos, None)));
            compile_dry(&db, docin);
            let mut hover = compile_dry::accumulated::<PLHover>(&db, docin);
            let hover = hover.pop();
            let sender = connection.sender.clone();
            if hover.is_none() {
                pool.execute(move || {
                    send_hover(
                        &sender,
                        id,
                        Hover {
                            contents: HoverContents::Scalar(MarkedString::String("".to_string())),
                            range: None,
                        },
                    );
                });
                return;
            }
            pool.execute(move || {
                send_hover(&sender, id, hover.unwrap());
            });
        })
        .on::<References, _>(|id, params| {
            let uri = url_to_path(params.text_document_position.text_document.uri);
            let pos = Pos::from_diag_pos(&params.text_document_position.position);
            docin.set_file(&mut db).to(uri);
            docin.set_action(&mut db).to(ActionType::FindReferences);
            docin.set_params(&mut db).to(Some((pos, None)));
            compile_dry(&db, docin);
            let refs = compile_dry::accumulated::<PLReferences>(&db, docin);
            let sender = connection.sender.clone();
            let mut rf = vec![];
            for r in refs {
                for r in r.clone().iter() {
                    rf.push(r.clone());
                }
            }
            pool.execute(move || {
                send_references(&sender, id, &rf);
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
            docin.set_action(&mut db).to(ActionType::Completion);
            docin.set_edit_pos(&mut db).to(Some(pos));
            docin.set_params(&mut db).to(Some((pos, trigger)));
            compile_dry(&db, docin);
            if !completions.is_empty() {
                let sender = connection.sender.clone();
                let comps = completions[0].clone();
                pool.execute(move || {
                    send_completions(&sender, id, comps.clone());
                });
            }
        })
        .on::<SemanticTokensFullRequest, _>(|id, params| {
            let uri = url_to_path(params.text_document.uri);
            docin.set_file(&mut db).to(uri.clone());
            docin.set_action(&mut db).to(ActionType::SemanticTokensFull);
            docin
                .set_params(&mut db)
                .to(Some((Default::default(), None)));
            compile_dry(&db, docin);
            let mut newtokens = compile_dry::accumulated::<PLSemanticTokens>(&db, docin);
            if newtokens.is_empty() {
                newtokens.push(SemanticTokens::default());
            }
            _ = tokens.insert(uri, newtokens[0].clone());
            let sender = connection.sender.clone();
            pool.execute(move || {
                send_semantic_tokens(&sender, id, newtokens[0].clone());
            });
        })
        .on::<SemanticTokensFullDeltaRequest, _>(|id, params| {
            let uri = url_to_path(params.text_document.uri);
            docin.set_file(&mut db).to(uri.clone());
            docin.set_action(&mut db).to(ActionType::SemanticTokensFull);
            docin
                .set_params(&mut db)
                .to(Some((Default::default(), None)));
            compile_dry(&db, docin);
            let mut newtokens = compile_dry::accumulated::<PLSemanticTokens>(&db, docin);
            if newtokens.is_empty() {
                newtokens.push(SemanticTokens::default());
            }
            let old = tokens.insert(uri, newtokens[0].clone());
            let delta = diff_tokens(&old.unwrap().data, &newtokens[0].data);
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
        .on::<Formatting, _>(|id, params| {
            let uri = url_to_path(params.text_document.uri);
            docin.set_file(&mut db).to(uri);
            docin.set_action(&mut db).to(ActionType::LspFmt);
            docin
                .set_params(&mut db)
                .to(Some((Default::default(), None)));
            compile_dry(&db, docin);
            let fmt = compile_dry::accumulated::<PLFormat>(&db, docin);
            if !fmt.is_empty() {
                let sender = connection.sender.clone();
                pool.execute(move || {
                    send_format(&sender, id, fmt[0].clone());
                });
            }
        })
        .on::<SignatureHelpRequest, _>(|id, params| {
            let doc = params.text_document_position_params;
            let uri = url_to_path(doc.text_document.uri);
            let pos = Pos::from_diag_pos(&doc.position);
            docin.set_file(&mut db).to(uri);
            docin.set_action(&mut db).to(ActionType::SignatureHelp);
            docin.set_params(&mut db).to(Some((pos, None)));
            compile_dry(&db, docin);
            let sigs = compile_dry::accumulated::<PLSignatureHelp>(&db, docin);
            if !sigs.is_empty() {
                let sender = connection.sender.clone();
                pool.execute(move || {
                    send_signature_help(&sender, id, sigs[0].clone());
                });
            } else {
                let sender = connection.sender.clone();
                pool.execute(move || {
                    send_signature_help(
                        &sender,
                        id,
                        SignatureHelp {
                            signatures: vec![],
                            active_signature: None,
                            active_parameter: None,
                        },
                    );
                });
            }
        })
        .on::<InlayHintRequest, _>(|id, params| {
            let uri = url_to_path(params.text_document.uri);
            docin.set_file(&mut db).to(uri);
            docin.set_action(&mut db).to(ActionType::Hint);
            docin
                .set_params(&mut db)
                .to(Some((Default::default(), None)));
            compile_dry(&db, docin);
            let hints = compile_dry::accumulated::<Hints>(&db, docin);
            let sender = connection.sender.clone();
            if !hints.is_empty() {
                pool.execute(move || {
                    send_hints(&sender, id, hints[0].clone());
                });
            }
        })
        .on::<DocumentSymbolRequest, _>(|id, params| {
            let uri = url_to_path(params.text_document.uri);
            docin.set_file(&mut db).to(uri);
            docin.set_action(&mut db).to(ActionType::DocSymbol);
            docin
                .set_params(&mut db)
                .to(Some((Default::default(), None)));
            compile_dry(&db, docin);
            let doc_symbols = compile_dry::accumulated::<DocSymbols>(&db, docin);
            let sender = connection.sender.clone();
            if !doc_symbols.is_empty() {
                pool.execute(move || {
                    send_doc_symbols(&sender, id, doc_symbols[0].clone());
                });
            }
        })
        .on_noti::<DidChangeTextDocument, _>(|params| {
            let f = url_to_path(params.text_document.uri);
            for content_change in params.content_changes.iter() {
                docs.lock().unwrap().borrow_mut().change(
                    &mut db,
                    content_change.range.unwrap(),
                    f.clone(),
                    content_change.text.clone(),
                );
                let mut pos = Pos::from_diag_pos(&content_change.range.unwrap().end);
                pos.column += 1;
                docin.set_edit_pos(&mut db).to(Some(pos));
                docin.set_docs(&mut db).to(docs.clone());
            }
            docin.set_file(&mut db).to(f.clone());

            docin.set_action(&mut db).to(ActionType::Diagnostic);
            compile_dry(&db, docin);
            completions = compile_dry::accumulated::<Completions>(&db, docin);
            let diags = compile_dry::accumulated::<Diagnostics>(&db, docin);
            let sender = connection.sender.clone();
            pool.execute(move || {
                debug!("diags: {:#?}", diags);
                let mut m = FxHashMap::<String, Vec<Diagnostic>>::default();
                for (p, diags) in &diags {
                    diags.iter().for_each(|x| x.get_diagnostic(&p, &mut m));
                }
                for (p, _) in &diags {
                    if m.get(p).is_none() {
                        send_diagnostics(&sender, p.to_string(), vec![]);
                    }
                }
                for (f, d) in m {
                    send_diagnostics(&sender, f, d);
                }
            });
        })
        .on_noti::<DidOpenTextDocument, _>(|params| {
            let f = url_to_path(params.text_document.uri);
            docs.lock().unwrap().borrow_mut().insert(
                &mut db,
                f.clone(),
                params.text_document.text,
                f.clone(),
            );
            docin.set_docs(&mut db).to(docs.clone());
            docin.set_file(&mut db).to(f.clone());
            docin.set_action(&mut db).to(ActionType::Diagnostic);
            docin.set_params(&mut db).to(None);
            compile_dry(&db, docin);
            let diags = compile_dry::accumulated::<Diagnostics>(&db, docin);
            let sender = connection.sender.clone();
            pool.execute(move || {
                let mut m = FxHashMap::<String, Vec<Diagnostic>>::default();
                for (p, diags) in diags {
                    diags.iter().for_each(|x| x.get_diagnostic(&p, &mut m));
                }
                for (f, d) in m {
                    send_diagnostics(&sender, f, d);
                }
            });
        })
        .on_noti::<DidCloseTextDocument, _>(|_params| {
            // let f = url_to_path(params.text_document.uri);
            // docs.lock().unwrap().borrow_mut().remove(&f);
            // docin.set_docs(&mut db).to(docs.clone());
        });
        let elapsed = now.elapsed();
        log::info!("req {:?} finished, time: {:?}", docin.action(&db), elapsed);
    }
    Ok(())
}
