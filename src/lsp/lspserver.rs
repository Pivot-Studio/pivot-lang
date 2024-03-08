use std::{
    collections::HashMap,
    error::Error,
    ops::Deref,
    sync::{Arc, Mutex},
    thread::available_parallelism,
    time::Instant,
};

use log::debug;
use lsp_types::{
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
    request::{
        CodeLensRequest, Completion, DocumentSymbolRequest, Formatting, GotoDefinition,
        HoverRequest, InlayHintRequest, References, Rename, SemanticTokensFullDeltaRequest,
        SemanticTokensFullRequest, SignatureHelpRequest,
    },
    CodeLensOptions, Diagnostic, Hover, HoverContents, InitializeParams, MarkedString, OneOf,
    SemanticTokens, SemanticTokensDelta, SemanticTokensOptions, ServerCapabilities, SignatureHelp,
    TextDocumentSyncKind, TextDocumentSyncOptions,
};

use lsp_server::{Connection, Message, RequestId};

use rustc_hash::{FxHashMap, FxHashSet};
use salsa::ParallelDatabase;
#[cfg(not(target_arch = "wasm32"))]
use threadpool::ThreadPool;

#[cfg(target_arch = "wasm32")]
mod fake_thread_pool;
use crate::{
    ast::{
        accumulators::{
            Completions, Diagnostics, DocSymbols, GotoDef, Hints, PLCodeLens, PLFormat, PLHover,
            PLReferences, PLSemanticTokens, PLSignatureHelp,
        },
        compiler::{compile_dry, ActionType},
        range::Pos,
    },
    db::{self},
    lsp::{
        config::SEMANTIC_LEGEND,
        dispatcher::Dispatcher,
        helpers::{
            send_code_lens, send_completions, send_diagnostics, send_doc_symbols, send_format,
            send_goto_def, send_hints, send_hover, send_references, send_rename,
            send_semantic_tokens, send_semantic_tokens_edit, send_signature_help, url_to_path,
        },
        mem_docs::MemDocsInput,
        semantic_tokens::diff_tokens,
    },
};
#[cfg(target_arch = "wasm32")]
use fake_thread_pool::ThreadPool;

pub fn start_lsp() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    log::info!("starting pivot-lang LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(ServerCapabilities {
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
                    legend: SEMANTIC_LEGEND.clone(),
                    range: Some(false),
                    full: Some(lsp_types::SemanticTokensFullOptions::Delta { delta: Some(true) }),
                },
            ),
        ),
        rename_provider: Some(OneOf::Left(true)),
        code_lens_provider: Some(CodeLensOptions {
            resolve_provider: None,
        }),
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

type Comple = Arc<Mutex<Option<RequestId>>>;

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let n_workers = available_parallelism().unwrap().get();
    let pool = ThreadPool::new(n_workers);
    let mut db = db::Database::default();
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    let docs = Arc::new(Mutex::new(Default::default()));
    let docin = MemDocsInput::new(
        &db,
        docs.clone(),
        "".to_string(),
        Default::default(),
        ActionType::Diagnostic,
        None,
        None,
    );
    let completions: Comple = Arc::new(Mutex::new(None));
    let mut last_semantic_file = "".to_string();
    let mut last_tokens: SemanticTokens = SemanticTokens::default();

    log::info!("starting main loop");
    for msg in &connection.receiver {
        let now = Instant::now();
        let di = Dispatcher::new(msg.clone());
        if let Message::Request(req) = &msg {
            if connection.handle_shutdown(req)? {
                return Ok(());
            }
        }
        let completions = completions.clone();
        di.on::<GotoDefinition, _>(|id, params| {
            let uri = url_to_path(params.text_document_position_params.text_document.uri);
            let pos = Pos::from_diag_pos(&params.text_document_position_params.position);
            docin.set_file(&mut db).to(uri);
            docin.set_action(&mut db).to(ActionType::GotoDef);
            docin.set_params(&mut db).to(Some((pos, None)));
            docin
                .set_docs(&mut db)
                .to(Arc::new(Mutex::new(docs.lock().unwrap().clone())));
            let snapshot = db.snapshot();
            let sender = connection.sender.clone();
            pool.execute(move || {
                let _ = compile_dry(snapshot.deref(), docin);
                let defs = compile_dry::accumulated::<GotoDef>(snapshot.deref(), docin);
                if !defs.is_empty() {
                    send_goto_def(&sender, id, defs[0].clone());
                }
            });
            // compile_dry(db.snapshot().deref(), docin);
        })
        .on::<HoverRequest, _>(|id, params| {
            let uri = url_to_path(params.text_document_position_params.text_document.uri);
            let pos = Pos::from_diag_pos(&params.text_document_position_params.position);
            docin.set_file(&mut db).to(uri);
            docin.set_action(&mut db).to(ActionType::Hover);
            docin.set_params(&mut db).to(Some((pos, None)));
            docin
                .set_docs(&mut db)
                .to(Arc::new(Mutex::new(docs.lock().unwrap().clone())));
            let snapshot = db.snapshot();
            let sender = connection.sender.clone();
            pool.execute(move || {
                let _ = compile_dry(snapshot.deref(), docin);
                let mut hover = compile_dry::accumulated::<PLHover>(snapshot.deref(), docin);
                let hover = hover.pop();
                if hover.is_none() {
                    send_hover(
                        &sender,
                        id,
                        Hover {
                            contents: HoverContents::Scalar(MarkedString::String("".to_string())),
                            range: None,
                        },
                    );
                    return;
                }
                send_hover(&sender, id, hover.unwrap());
            });
        })
        .on::<References, _>(|id, params| {
            let uri = url_to_path(params.text_document_position.text_document.uri);
            let pos = Pos::from_diag_pos(&params.text_document_position.position);
            docin.set_file(&mut db).to(uri);
            docin.set_action(&mut db).to(ActionType::FindReferences);
            docin.set_params(&mut db).to(Some((pos, None)));
            docin
                .set_docs(&mut db)
                .to(Arc::new(Mutex::new(docs.lock().unwrap().clone())));
            let snapshot = db.snapshot();
            let sender = connection.sender.clone();
            pool.execute(move || {
                let _ = compile_dry(snapshot.deref(), docin);
                let refs = compile_dry::accumulated::<PLReferences>(snapshot.deref(), docin);
                let mut rf = vec![];
                for r in refs {
                    for r in r.clone().iter() {
                        rf.push(r.clone());
                    }
                }
                send_references(&sender, id, &rf);
            });
        })
        .on::<Completion, _>(|id, _| {
            let mut guard = completions.lock().unwrap();
            *guard = Some(id);
        })
        .on::<CodeLensRequest, _>(|id, params| {
            let uri = url_to_path(params.text_document.uri);
            if docin.file(&db) != &uri {
                docin.set_file(&mut db).to(uri);
            }
            let snapshot = db.snapshot();
            let sender = connection.sender.clone();
            let completions = completions.clone();
            pool.execute(move || {
                let _ = compile_dry(snapshot.deref(), docin);
                do_send_completions_and_diags(&snapshot, docin, completions, &sender);
                let codelens = compile_dry::accumulated::<PLCodeLens>(snapshot.deref(), docin);
                send_code_lens(&sender, id, codelens);
            });
        })
        .on::<SemanticTokensFullRequest, _>(|id, params| {
            let uri = url_to_path(params.text_document.uri);
            last_semantic_file = uri.clone();
            if docin.file(&db) != &uri {
                docin.set_file(&mut db).to(uri);
            }
            let snapshot = db.snapshot();

            let _ = compile_dry(snapshot.deref(), docin);
            do_send_completions_and_diags(
                &snapshot,
                docin,
                completions.clone(),
                &connection.sender,
            );
            let mut newtokens =
                compile_dry::accumulated::<PLSemanticTokens>(snapshot.deref(), docin);
            if newtokens.is_empty() {
                newtokens.push(SemanticTokens::default());
            }
            last_tokens = newtokens[0].clone();
            let sender = connection.sender.clone();
            pool.execute(move || {
                send_semantic_tokens(&sender, id, newtokens[0].clone());
            });
        })
        .on::<SemanticTokensFullDeltaRequest, _>(|id, params| {
            let uri = url_to_path(params.text_document.uri);
            if last_semantic_file != uri {
                let sender = connection.sender.clone();
                pool.execute(move || {
                    send_semantic_tokens_edit(
                        &sender,
                        id,
                        SemanticTokensDelta {
                            result_id: None,
                            edits: vec![],
                        },
                    );
                });
                return;
            }
            last_semantic_file = uri.clone();
            let snapshot = db.snapshot();
            let _ = compile_dry(snapshot.deref(), docin);
            do_send_completions_and_diags(
                &snapshot,
                docin,
                completions.clone(),
                &connection.sender,
            );
            let mut newtokens =
                compile_dry::accumulated::<PLSemanticTokens>(snapshot.deref(), docin);
            if newtokens.is_empty() {
                newtokens.push(SemanticTokens::default());
            }
            let old = last_tokens.clone();
            last_tokens = newtokens[0].clone();
            let delta = diff_tokens(&old.data, &newtokens[0].data);
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

            log::info!("send semantic tokens delta");
        })
        .on::<Formatting, _>(|id, params| {
            let uri = url_to_path(params.text_document.uri);
            docin.set_file(&mut db).to(uri);
            docin.set_action(&mut db).to(ActionType::LspFmt);
            docin
                .set_params(&mut db)
                .to(Some((Default::default(), None)));
            docin
                .set_docs(&mut db)
                .to(Arc::new(Mutex::new(docs.lock().unwrap().clone())));
            let _ = compile_dry(&db, docin);
            let fmt = compile_dry::accumulated::<PLFormat>(&db, docin);
            if !fmt.is_empty() {
                let sender = connection.sender.clone();
                pool.execute(move || {
                    send_format(&sender, id, fmt[0].clone());
                });
            }
        })
        .on::<Rename, _>(|id, params| {
            let uri = url_to_path(params.text_document_position.text_document.uri);
            let pos = Pos::from_diag_pos(&params.text_document_position.position);
            docin.set_file(&mut db).to(uri);
            docin.set_action(&mut db).to(ActionType::FindReferences);
            docin.set_params(&mut db).to(Some((pos, None)));
            docin
                .set_docs(&mut db)
                .to(Arc::new(Mutex::new(docs.lock().unwrap().clone())));
            let _ = compile_dry(&db, docin);
            let refs = compile_dry::accumulated::<PLReferences>(&db, docin);
            let sender = connection.sender.clone();
            let mut rf: HashMap<lsp_types::Url, Vec<lsp_types::TextEdit>> = Default::default();
            let mut set: FxHashMap<lsp_types::Url, FxHashSet<lsp_types::Range>> =
                Default::default();
            for r in refs {
                for r in r.clone().iter() {
                    let url = r.uri.clone();
                    let edit = lsp_types::TextEdit::new(r.range, params.new_name.clone());
                    if set.get(&url).is_some() && set.get(&url).unwrap().contains(&r.range) {
                        continue;
                    }
                    set.entry(url.clone()).or_default().insert(r.range);
                    rf.entry(url).or_default().push(edit);
                }
            }
            pool.execute(move || {
                send_rename(&sender, id, rf);
            });
        })
        .on::<SignatureHelpRequest, _>(|id, params| {
            let doc = params.text_document_position_params;
            let uri = url_to_path(doc.text_document.uri);
            let pos = Pos::from_diag_pos(&doc.position);
            docin.set_file(&mut db).to(uri);
            docin.set_action(&mut db).to(ActionType::SignatureHelp);
            docin.set_params(&mut db).to(Some((pos, None)));
            docin
                .set_docs(&mut db)
                .to(Arc::new(Mutex::new(docs.lock().unwrap().clone())));
            let _ = compile_dry(&db, docin);
            do_send_completions_and_diags(
                &db.snapshot(),
                docin,
                completions.clone(),
                &connection.sender,
            );
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
            if docin.file(&db) != &uri {
                docin.set_file(&mut db).to(uri.clone());
                docin.set_action(&mut db).to(ActionType::Hint);
                docin
                    .set_params(&mut db)
                    .to(Some((Default::default(), None)));
                docin
                    .set_docs(&mut db)
                    .to(Arc::new(Mutex::new(docs.lock().unwrap().clone())));
            }
            let sender = connection.sender.clone();
            let snapshot = db.snapshot();
            let completions = completions.clone();
            pool.execute(move || {
                if docin.file(snapshot.deref()) != &uri {
                    let _ = compile_dry(snapshot.deref(), docin);
                    do_send_completions_and_diags(&snapshot, docin, completions, &sender);
                }
                let hints = compile_dry::accumulated::<Hints>(snapshot.deref(), docin);
                if !hints.is_empty() {
                    send_hints(&sender, id, hints[0].clone());
                }
            });
        })
        .on::<DocumentSymbolRequest, _>(|id, params| {
            let uri = url_to_path(params.text_document.uri);
            if docin.file(&db) != &uri {
                docin.set_file(&mut db).to(uri.clone());
                docin.set_action(&mut db).to(ActionType::DocSymbol);
                docin
                    .set_params(&mut db)
                    .to(Some((Default::default(), None)));
            }
            docin
                .set_docs(&mut db)
                .to(Arc::new(Mutex::new(docs.lock().unwrap().clone())));

            let sender = connection.sender.clone();
            let snapshot = db.snapshot();
            let completions = completions.clone();
            pool.execute(move || {
                if docin.file(snapshot.deref()) != &uri {
                    let _ = compile_dry(snapshot.deref(), docin);
                }
                do_send_completions_and_diags(&snapshot, docin, completions, &sender);
                let doc_symbols = compile_dry::accumulated::<DocSymbols>(snapshot.deref(), docin);
                if !doc_symbols.is_empty() {
                    send_doc_symbols(&sender, id, doc_symbols[0].clone());
                }
            });
        })
        .on_noti::<DidChangeTextDocument, _>(|params| {
            let f = url_to_path(params.text_document.uri);
            for content_change in params.content_changes.iter() {
                let (doc, txt) = docs.lock().unwrap().change_txt(
                    &db,
                    content_change.range.unwrap(),
                    &f,
                    content_change.text.clone(),
                );

                doc.set_text(&mut db).to(txt);
                let mut pos = Pos::from_diag_pos(&content_change.range.unwrap().start);
                pos.column += 1;
                docin.set_edit_pos(&mut db).to(Some(pos));
            }
            docin
                .set_docs(&mut db)
                .to(Arc::new(Mutex::new(docs.lock().unwrap().clone())));
            docin.set_file(&mut db).to(f);

            docin.set_action(&mut db).to(ActionType::Diagnostic);
            let snapshot = db.snapshot();
            let sender = connection.sender.clone();
            let completions = completions.clone();
            pool.execute(move || {
                let _ = compile_dry(snapshot.deref(), docin);
                do_send_completions_and_diags(&snapshot, docin, completions, &sender);
            });
        })
        .on_noti::<DidOpenTextDocument, _>(|params| {
            let f = url_to_path(params.text_document.uri);
            docs.lock()
                .unwrap()
                .insert(&db, f.clone(), params.text_document.text, f.clone());
            docin.set_docs(&mut db).to(docs.clone());
            docin.set_file(&mut db).to(f);
            docin.set_action(&mut db).to(ActionType::Diagnostic);
            docin.set_params(&mut db).to(None);
            let _ = compile_dry(&db, docin);
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
        log::info!("main loop handle message: {:?}", elapsed);
    }
    Ok(())
}

fn do_send_completions_and_diags(
    snapshot: &salsa::Snapshot<db::Database>,
    docin: MemDocsInput,
    completions: Arc<Mutex<Option<RequestId>>>,
    sender: &crossbeam_channel::Sender<Message>,
) {
    let comps = compile_dry::accumulated::<Completions>(snapshot.deref(), docin);
    let mut guard = completions.lock().unwrap();
    if let Some(id) = &*guard {
        send_completions(
            sender,
            id.clone(),
            comps.first().cloned().unwrap_or_default(),
        );
    }
    *guard = None;
    drop(guard);
    let diags = compile_dry::accumulated::<Diagnostics>(snapshot.deref(), docin);
    debug!("diags: {:#?}", diags);
    let mut m = FxHashMap::<String, Vec<Diagnostic>>::default();
    for (p, diags) in &diags {
        diags.iter().for_each(|x| x.get_diagnostic(p, &mut m));
    }
    for (p, _) in &diags {
        if m.get(p).is_none() {
            send_diagnostics(sender, p.to_string(), vec![]);
        }
    }
    for (f, d) in m {
        send_diagnostics(sender, f, d);
    }
}
