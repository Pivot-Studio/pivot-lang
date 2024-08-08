use std::{
    cell::RefCell,
    sync::{Arc, Mutex},
};

use include_dir::{include_dir, Dir};
use lazy_static::lazy_static;
use lsp_types::{
    Diagnostic, DidChangeTextDocumentParams, Position, SemanticTokens, SemanticTokensDelta, Url,
};
use rustc_hash::FxHashMap;
use salsa::Setter;
use wasm_bindgen::prelude::wasm_bindgen;

use crate::{
    ast::{
        accumulators::{
            Completions, Diagnostics, DocSymbols, GotoDef, Hints, PLReferences, PLSemanticTokens,
        },
        compiler::{compile_dry, ActionType},
        range::Pos,
    },
    db::Database,
    lsp::semantic_tokens::diff_tokens,
};

use super::{
    config::SEMANTIC_LEGEND,
    helpers::url_to_path,
    mem_docs::{MemDocs, MemDocsInput},
};

pub struct GlobalMutWrapper<T> {
    pub inner: RefCell<T>,
}

unsafe impl<T> Sync for GlobalMutWrapper<T> {}

impl<T> GlobalMutWrapper<T> {
    pub fn new(inner: T) -> Self {
        Self {
            inner: RefCell::new(inner),
        }
    }
}

lazy_static! {
    static ref DB: GlobalMutWrapper<Database> = GlobalMutWrapper::new(Database::default());
    static ref OLD_TOKENS: GlobalMutWrapper<SemanticTokens> =
        GlobalMutWrapper::new(SemanticTokens::default());
    static ref COMPLETIONS: GlobalMutWrapper<Vec<lsp_types::CompletionItem>> =
        GlobalMutWrapper::new(vec![]);
    static ref DOCIN: MemDocsInput = {
        let b = &DB.inner;
        let db = b.borrow_mut();
        let docs = Arc::new(Mutex::new(MemDocs::default()));
        let doc = MemDocsInput::new(
            &*db,
            docs.clone(),
            "".to_string(),
            Default::default(),
            ActionType::Diagnostic,
            None,
            None,
        );
        doc
    };
}

unsafe impl Sync for Database {}

const LSP_DEMO_URI: &str = "http://www.test.com/main.pi";

const LSP_DEMO_CONF_URI: &str = "http://www.test.com/Kagari.toml";

#[wasm_bindgen]
pub unsafe fn on_change_doc(req: &str) -> String {
    log::info!("req: {}", req);
    let params: DidChangeTextDocumentParams = serde_json::from_str(req).unwrap();
    let docin = *DOCIN;
    let binding = &DB.inner;
    let db = &mut *binding.borrow_mut();
    let docs = DOCIN.docs(db);
    // let mut completions: Vec<Vec<lsp_types::CompletionItem>> = vec![];
    let f = url_to_path(params.text_document.uri);
    for content_change in params.content_changes.iter() {
        docs.lock().unwrap().change(
            db,
            content_change.range.unwrap(),
            f.clone(),
            content_change.text.clone(),
        );
        let mut pos = Pos::from_diag_pos(&content_change.range.unwrap().start);
        pos.column += 1;
        docin.set_edit_pos(db).to(Some(pos));
        docin.set_docs(db).to(docs.clone());
    }
    docin.set_file(db).to(f);

    docin.set_action(db).to(ActionType::Diagnostic);
    let _ = compile_dry(db, docin);
    // log::trace!("mod {:#?}", re.plmod(db));
    // completions = compile_dry::accumulated::<Completions>(db, docin);
    let diags = compile_dry::accumulated::<Diagnostics>(db, docin);
    let mut m = FxHashMap::<String, Vec<Diagnostic>>::default();
    for d in &diags {
        let (p, diags) = &d.0;
        diags.iter().for_each(|x| x.get_diagnostic(p, &mut m));
    }
    let comps = compile_dry::accumulated::<Completions>(db, docin);
    if comps.len() > 0 {
        COMPLETIONS.inner.replace(comps[0].0.clone());
    } else {
        COMPLETIONS.inner.replace(vec![]);
    }
    log::trace!("diags: {:#?}", diags);
    for (f, d) in m {
        if !f.contains("main") {
            continue;
        }
        return serde_json::to_value(lsp_types::PublishDiagnosticsParams {
            uri: Url::parse(LSP_DEMO_URI).unwrap(),
            diagnostics: d,
            version: None,
        })
        .unwrap()
        .to_string();
    }
    return serde_json::to_value(lsp_types::PublishDiagnosticsParams {
        uri: Url::parse(LSP_DEMO_URI).unwrap(),
        diagnostics: vec![],
        version: None,
    })
    .unwrap()
    .to_string();
}

pub static PLLIB_DIR: Dir = include_dir!("./planglib");

fn add_file(db: &mut Database, docs: Arc<Mutex<MemDocs>>, fpath: &str, content: &str) {
    // include!(real_path);
    // log::error!("add file: {}", fpath);
    docs.lock()
        .unwrap()
        .insert(db, fpath.into(), content.into(), fpath.into());
}

fn add_fill_rec(db: &mut Database, docs: Arc<Mutex<MemDocs>>, dir: &Dir) {
    dir.files().for_each(|f| {
        let path = f.path();
        f.contents_utf8().map(|x| {
            add_file(db, docs.clone(), path.to_str().unwrap(), x);
        });
    });
    dir.dirs().for_each(|d| {
        add_fill_rec(db, docs.clone(), d);
    });
}

fn add_pl_libs(db: &mut Database, docs: Arc<Mutex<MemDocs>>) {
    for entry in PLLIB_DIR.dirs() {
        let path = entry.path();
        if path.starts_with("thirdparty") {
            continue;
        }
        add_fill_rec(db, docs.clone(), entry);

        // add_file(db, docs.clone(), path.to_str().unwrap(), content);
    }
}
#[wasm_bindgen]
pub fn set_init_content(content: &str) -> String {
    console_error_panic_hook::set_once();
    wasm_logger::init(wasm_logger::Config::new(log::Level::Warn));
    let docin = *DOCIN;
    let binding = &DB.inner;
    let db = &mut *binding.borrow_mut();
    let docs = docin.docs(db);
    docs.lock().unwrap().insert(
        db,
        LSP_DEMO_URI.to_string(),
        content.to_string(),
        LSP_DEMO_URI.to_string(),
    );
    docs.lock().unwrap().insert(
        db,
        LSP_DEMO_CONF_URI.to_string(),
        r#"entry = "http://www.test.com/main.pi"
    project = "simple_test""#
            .to_string(),
        LSP_DEMO_CONF_URI.to_string(),
    );
    add_pl_libs(db, docs.clone());

    docin.set_file(db).to(LSP_DEMO_URI.to_string());

    docin.set_action(db).to(ActionType::Diagnostic);
    let _ = compile_dry(db, docin);
    // log::trace!("mod {:#?}", re.plmod(db));
    // completions = compile_dry::accumulated::<Completions>(db, docin);
    let diags = compile_dry::accumulated::<Diagnostics>(db, docin);
    let mut m = FxHashMap::<String, Vec<Diagnostic>>::default();
    for d in &diags {
        let (p, diags) = &d.0;
        diags.iter().for_each(|x| x.get_diagnostic(p, &mut m));
    }
    log::trace!("diags: {:#?}", diags);
    for (f, d) in m {
        if !f.contains("main") {
            continue;
        }
        return serde_json::to_value(lsp_types::PublishDiagnosticsParams {
            uri: Url::parse(LSP_DEMO_URI).unwrap(),
            diagnostics: d,
            version: None,
        })
        .unwrap()
        .to_string();
    }
    return serde_json::to_value(lsp_types::PublishDiagnosticsParams {
        uri: Url::parse(LSP_DEMO_URI).unwrap(),
        diagnostics: vec![],
        version: None,
    })
    .unwrap()
    .to_string();
}

#[wasm_bindgen]
pub fn get_semantic_tokens() -> String {
    let docin = *DOCIN;
    let binding = &DB.inner;
    let db = &mut *binding.borrow_mut();
    docin.set_action(db).to(ActionType::SemanticTokensFull);
    docin.set_params(db).to(Some((Default::default(), None)));
    compile_dry(db, docin);
    // let docs = DOCIN.docs(db);
    let mut newtokens = compile_dry::accumulated::<PLSemanticTokens>(db, docin)
        .drain(..)
        .map(|x| x.0)
        .collect::<Vec<_>>();
    if newtokens.is_empty() {
        newtokens.push(SemanticTokens::default());
    }
    let old = OLD_TOKENS.inner.replace(newtokens[0].clone());
    let delta = diff_tokens(&old.data, &newtokens[0].data);
    log::info!("tokens: {:#?}", delta);
    return serde_json::to_value(SemanticTokensDelta {
        result_id: None,
        edits: delta,
    })
    .unwrap()
    .to_string();
}

#[wasm_bindgen]
pub fn get_semantic_tokens_full() -> String {
    let docin = *DOCIN;
    let binding = &DB.inner;
    let db = &mut *binding.borrow_mut();
    docin.set_action(db).to(ActionType::SemanticTokensFull);
    docin.set_params(db).to(Some((Default::default(), None)));
    compile_dry(db, docin);
    // let docs = DOCIN.docs(db);
    let mut newtokens = compile_dry::accumulated::<PLSemanticTokens>(db, docin)
        .drain(..)
        .map(|x| x.0)
        .collect::<Vec<_>>();
    if newtokens.is_empty() {
        newtokens.push(SemanticTokens::default());
    }
    let _ = OLD_TOKENS.inner.replace(newtokens[0].clone());
    // let delta = diff_tokens(&old.data, &newtokens[0].data);
    log::info!("tokens: {:#?}", &newtokens[0]);
    return serde_json::to_value(&newtokens[0]).unwrap().to_string();
}

#[wasm_bindgen]
pub fn get_legend() -> String {
    return serde_json::to_value(SEMANTIC_LEGEND.clone())
        .unwrap()
        .to_string();
}

#[wasm_bindgen]
pub fn get_completions() -> String {
    log::error!("get_completions {:#?}", &*COMPLETIONS.inner.borrow());
    return serde_json::to_value(COMPLETIONS.inner.borrow().clone())
        .unwrap()
        .to_string();
}

#[wasm_bindgen]
pub fn get_inlay_hints() -> String {
    let docin = *DOCIN;
    let binding = &DB.inner;
    let db = &mut *binding.borrow_mut();
    let mut hints = compile_dry::accumulated::<Hints>(db, docin)
        .drain(..)
        .map(|x| x.0)
        .collect::<Vec<_>>();
    if hints.is_empty() {
        hints.push(vec![]);
    }
    return serde_json::to_value(hints[0].clone()).unwrap().to_string();
}

#[wasm_bindgen]
pub fn get_doc_symbol() -> String {
    let docin = *DOCIN;
    let binding = &DB.inner;
    let db = &mut *binding.borrow_mut();
    let doc_symbols = compile_dry::accumulated::<DocSymbols>(db, docin);
    let symbol = &doc_symbols[0];
    return serde_json::to_value(&symbol.0).unwrap().to_string();
}

#[wasm_bindgen]
pub fn go_to_def(req: &str) -> String {
    let docin = *DOCIN;
    let binding = &DB.inner;
    let db = &mut *binding.borrow_mut();
    let params: Position = serde_json::from_str(req).unwrap();
    // let uri = url_to_path(params.text_document_position_params.text_document.uri);
    let pos = Pos::from_diag_pos(&params);
    // docin.set_file(db).to(uri);
    docin.set_action(db).to(ActionType::GotoDef);
    docin.set_params(db).to(Some((pos, None)));
    compile_dry(db, docin);
    let defs = compile_dry::accumulated::<GotoDef>(db, docin);
    if defs.is_empty() {
        return serde_json::to_value::<Vec<i32>>(vec![])
            .unwrap()
            .to_string();
    }
    let def = &defs[0].0;
    return serde_json::to_value(def).unwrap().to_string();
}

#[wasm_bindgen]
pub fn get_refs(req: &str) -> String {
    let docin = *DOCIN;
    let binding = &DB.inner;
    let db = &mut *binding.borrow_mut();
    let params: Position = serde_json::from_str(req).unwrap();
    // let uri = url_to_path(params.text_document_position_params.text_document.uri);
    let pos = Pos::from_diag_pos(&params);
    // docin.set_file(db).to(uri);
    docin.set_action(db).to(ActionType::FindReferences);
    docin.set_params(db).to(Some((pos, None)));
    compile_dry(db, docin);
    let refs = compile_dry::accumulated::<PLReferences>(db, docin)
        .drain(..)
        .map(|x| x.0)
        .collect::<Vec<_>>();
    let mut rf = vec![];
    for r in refs {
        for r in r.clone().iter() {
            rf.push(r.clone());
        }
    }
    return serde_json::to_value(&rf).unwrap().to_string();
}
