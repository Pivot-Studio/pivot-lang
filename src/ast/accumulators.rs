use std::path::PathBuf;

use lsp_types::{
    CodeLens, CompletionItem, DocumentSymbol, GotoDefinitionResponse, Hover, InlayHint, Location,
    SemanticTokens, SignatureHelp, TextEdit,
};

use super::diag::PLDiag;

#[salsa::accumulator]
pub struct Diagnostics((String, Vec<PLDiag>));

#[salsa::accumulator]
pub struct PLReferences(Vec<Location>);

#[salsa::accumulator]
pub struct GotoDef(GotoDefinitionResponse);

#[salsa::accumulator]
pub struct Completions(Vec<CompletionItem>);

#[salsa::accumulator]
pub struct PLSemanticTokens(SemanticTokens);

#[salsa::accumulator]
pub struct PLCodeLens(CodeLens);

#[salsa::accumulator]
pub struct PLHover(Hover);

#[salsa::accumulator]
pub struct ModBuffer(PLModBuffer);

#[derive(Debug, Clone)]
pub struct PLModBuffer {
    pub path: PathBuf,
    pub is_main: bool,
}

#[salsa::accumulator]
pub struct PLFormat(Vec<TextEdit>);
#[salsa::accumulator]
pub struct Hints(Vec<InlayHint>);
#[salsa::accumulator]
pub struct DocSymbols(Vec<DocumentSymbol>);

#[salsa::accumulator]
pub struct PLSignatureHelp(SignatureHelp);
