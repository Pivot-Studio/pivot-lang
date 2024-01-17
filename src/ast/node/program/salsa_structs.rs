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
    pub debug: bool,
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
/// # ProgramNodeWrapper
///
/// `ProgramNodeWrapper`` is a wrapper for node to enjoy the functionalities provided by salsa.
/// Because pivot-lang mixes the lsp and compiler together, it always use the wrapper after parsing.
pub struct ProgramNodeWrapper {
    pub node: Box<NodeEnum>,
}

#[salsa::tracked]
pub struct ModWrapper {
    pub plmod: Mod,
}

/// # Program
///
/// `Program` holds the parsed program entry node, and all necessary information to parse a whole AST tree,
/// for all files used by the program entry node.
#[salsa::tracked]
pub struct Program {
    /// entry_node is the entry node of a whole program, which represents a file.
    /// It's used to find all dependencies used inside it to parse the whole program.
    pub entry_node: ProgramNodeWrapper,
    pub params: EmitParams,
    pub docs: MemDocsInput,

    /// config is all neccessary information to represent a program
    pub config: Config,
    pub opt: HashOptimizationLevel,
}
