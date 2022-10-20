use lsp_types::{
    CompletionItem, Diagnostic, GotoDefinitionResponse, Hover, Location, SemanticTokens,
};

#[salsa::accumulator]
pub struct Diagnostics((String, Vec<Diagnostic>));

#[salsa::accumulator]
pub struct PLReferences(Location);

#[salsa::accumulator]
pub struct GotoDef(GotoDefinitionResponse);

#[salsa::accumulator]
pub struct Completions(Vec<CompletionItem>);

#[salsa::accumulator]
pub struct PLSemanticTokens(SemanticTokens);

#[salsa::accumulator]
pub struct PLHover(Hover);
