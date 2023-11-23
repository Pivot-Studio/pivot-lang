use crate::ast::compiler::HashOptimizationLevel;
use crate::ast::node::macro_nodes::MacroNode;
use crate::ast::node::NodeEnum;
use crate::ast::pltype::FNValue;
use crate::ast::range::Pos;
use crate::utils::read_config::Config;

use crate::ast::plmod::{GlobType, Mod};

use crate::lsp::mem_docs::{EmitParams, MemDocsInput};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::sync::Arc;

use super::UnsafeWrapper;

#[salsa::tracked]
pub struct ProgramEmitParam {
    pub node: ProgramNodeWrapper,
    #[return_ref]
    pub dir: String,
    #[return_ref]
    pub file: String,
    #[return_ref]
    pub fullpath: String,
    #[return_ref]
    pub params: LspParams,
    pub submods: FxHashMap<String, Arc<Mod>>,
    #[return_ref]
    pub file_content: String,
    #[return_ref]
    pub types: UnsafeWrapper<FxHashMap<String, GlobType>>,
    #[return_ref]
    pub mth_table: MthdTableWrapper,
    #[return_ref]
    pub macro_table: UnsafeWrapper<FxHashMap<String, Arc<MacroNode>>>,
    pub is_active_file: bool,
    pub opt: HashOptimizationLevel,
}

pub type MthdTableWrapper =
    UnsafeWrapper<FxHashMap<String, FxHashMap<String, Arc<RefCell<FNValue>>>>>;

#[salsa::tracked]
pub struct LspParams {
    #[return_ref]
    pub modpath: String,
    pub params: Option<Pos>,
    pub config: Config,
    pub is_compile: bool,
}

#[salsa::tracked]
pub struct ProgramNodeWrapper {
    pub node: Box<NodeEnum>,
}

#[salsa::tracked]
pub struct ModWrapper {
    pub plmod: Mod,
}

#[salsa::tracked]
pub struct Program {
    pub node: ProgramNodeWrapper,
    pub params: EmitParams,
    pub docs: MemDocsInput,
    pub config: Config,
    pub opt: HashOptimizationLevel,
}
