use crate::ast::compiler::HashOptimizationLevel;
use crate::ast::node::macro_nodes::MacroNode;
use crate::ast::node::NodeEnum;
use crate::ast::pltype::FNValue;
use crate::ast::range::Pos;
use crate::utils::read_config::Config;

use crate::ast::plmod::{GlobalType, Mod};

use crate::lsp::mem_docs::{EmitParams, MemDocsInput, ParentMods};
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::sync::Arc;
use ustr::Ustr;

use super::UnsafeWrapper;

/// ProgramEmitParam is the structure contains all information about the entry node and sub modules.
/// it's used to generate LLVM IR or do some LSP analysis.
#[salsa::tracked]
pub struct ProgramEmitParam<'db> {
    /// program_node is the entry AST node of a program
    pub program_node: ProgramNodeWrapper<'db>,

    #[return_ref]
    /// dir is the directory path of kagari.toml file
    pub dir: String,

    #[return_ref]
    /// file is the entry file of the program
    pub file: String,
    #[return_ref]

    /// fullpath is the absolute path of the entry file
    pub fullpath: String,
    #[return_ref]
    pub lsp_params: LspParams<'db>,

    /// sub-modules analyzed according to the use statements inside the entry node
    pub submods: FxHashMap<Ustr, Arc<Mod>>,

    /// file_content is the content of the current active file
    /// it might differ from the file above
    #[return_ref]
    pub file_content: String,

    /// types is all types in the global scope
    #[return_ref]
    pub types: UnsafeWrapper<FxHashMap<Ustr, GlobalType>>,

    // method table, which holds all available methods across all modules
    #[return_ref]
    pub mth_table: MthdTableWrapper,

    /// macro table holds all avaiable macros across all modules
    #[return_ref]
    pub macro_table: UnsafeWrapper<FxHashMap<Ustr, Arc<MacroNode>>>,

    /// whether the current [file] is active
    pub is_active_file: bool,
    pub opt: HashOptimizationLevel,
    pub debug: bool,
    pub print_escaped: bool,
}

pub type MthdTableWrapper = UnsafeWrapper<FxHashMap<Ustr, FxHashMap<Ustr, Arc<RefCell<FNValue>>>>>;

#[salsa::tracked]
pub struct LspParams<'db> {
    #[return_ref]
    pub modpath: String,

    /// last_edtting_postion is the editing position of opened files.
    /// The position of the other files are None to prevent the duplicated analysis on the unchanged files,
    /// which reduces the efficiency of LSP.
    pub editing_postion: Option<Pos>,
    pub config: Config,
    pub is_compile: bool,
}

/// # ProgramNodeWrapper
///
/// `ProgramNodeWrapper`` is a wrapper for node to enjoy the functionalities provided by salsa.
/// Because pivot-lang mixes the lsp and compiler together, it always use the wrapper after parsing.
#[salsa::tracked]
pub struct ProgramNodeWrapper<'db> {
    pub node: Box<NodeEnum>,
}

#[salsa::tracked]
pub struct ModWrapper<'db> {
    pub plmod: Mod,
}

/// # Program
///
/// `Program` holds the parsed program entry node, and all necessary information to parse a whole AST tree,
/// for all files used by the program entry node.
#[salsa::tracked]
pub struct Program<'db> {
    /// entry_node is the entry node of a whole program, which represents a file.
    /// It's used to find all dependencies used inside it to parse the whole program.
    pub entry_node: ProgramNodeWrapper<'db>,

    pub params: EmitParams<'db>,
    pub docs: MemDocsInput,

    /// config is all neccessary information to represent a program
    pub config: Config,
    pub opt: HashOptimizationLevel,
    pub parent_mods: ParentMods,
}
