use std::path::PathBuf;

use lsp_types::{
    CodeLens, CompletionItem, DocumentSymbol, GotoDefinitionResponse, Hover, InlayHint, Location,
    SemanticTokens, SignatureHelp, TextEdit,
};

use super::diag::PLDiag;

#[salsa::accumulator]
pub struct Diagnostics(pub (String, Vec<PLDiag>));

#[salsa::accumulator]
pub struct PLReferences(pub Vec<Location>);

#[salsa::accumulator]
pub struct GotoDef(pub GotoDefinitionResponse);

#[salsa::accumulator]
pub struct Completions(pub Vec<CompletionItem>);

#[salsa::accumulator]
pub struct PLSemanticTokens(pub SemanticTokens);

#[salsa::accumulator]
pub struct PLCodeLens(pub CodeLens);

#[salsa::accumulator]
pub struct PLHover(pub Hover);

#[salsa::accumulator]
pub struct ModBuffer(pub PLModBuffer);

#[derive(Debug, Clone)]
pub struct PLModBuffer {
    pub path: PathBuf,
    pub buf: Vec<u8>,
    pub name: String,
}

#[salsa::accumulator]
pub struct PLFormat(pub Vec<TextEdit>);
#[salsa::accumulator]
pub struct Hints(pub Vec<InlayHint>);
#[salsa::accumulator]
pub struct DocSymbols(pub Vec<DocumentSymbol>);

#[salsa::accumulator]
pub struct PLSignatureHelp(pub SignatureHelp);
