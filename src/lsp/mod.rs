mod config;
pub mod dispatcher;
pub mod helpers;
#[cfg(not(target_arch = "wasm32"))]
mod lspserver;
pub mod mem_docs;
pub mod semantic_tokens;
pub mod text;
#[cfg(not(target_arch = "wasm32"))]
pub use lspserver::*;
#[cfg(target_arch = "wasm32")]
pub mod wasm;
