use std::{cell::RefCell, path::PathBuf, rc::Rc};

use lsp_types::{CompletionItem, GotoDefinitionResponse, Hover, Location, SemanticTokens};

use super::ctx::PLDiag;

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
