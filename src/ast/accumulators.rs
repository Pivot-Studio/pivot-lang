use std::{cell::RefCell, path::PathBuf, rc::Rc};

use lsp_types::{
    CompletionItem, DocumentSymbol, GotoDefinitionResponse, Hover, InlayHint, Location,
    SemanticTokens, SignatureHelp, TextEdit,
};

use super::diag::PLDiag;

#[salsa::accumulator]
pub struct Diagnostics((String, Vec<PLDiag>));

#[salsa::accumulator]
pub struct PLReferences(Rc<RefCell<Vec<Location>>>);

#[salsa::accumulator]
pub struct GotoDef(GotoDefinitionResponse);

#[salsa::accumulator]
pub struct Completions(Vec<CompletionItem>);

#[salsa::accumulator]
pub struct PLSemanticTokens(SemanticTokens);

#[salsa::accumulator]
pub struct PLHover(Hover);

#[salsa::accumulator]
pub struct ModBuffer(PathBuf);

#[salsa::accumulator]
pub struct PLFormat(Vec<TextEdit>);
#[salsa::accumulator]
pub struct Hints(Vec<InlayHint>);
#[salsa::accumulator]
pub struct DocSymbols(Vec<DocumentSymbol>);

#[salsa::accumulator]
pub struct PLSignatureHelp(SignatureHelp);
