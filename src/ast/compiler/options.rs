#[cfg(feature = "llvm")]
use inkwell::OptimizationLevel;

/// Options preserves additional args to do additional works during parsing AST
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Copy)]
pub struct Options {
    pub genir: bool,
    /// printast will print out the generated AST and ends up the further processes.
    pub printast: bool,
    /// flow will print out the generated AST and ends up the further processes.
    pub flow: bool,
    pub optimization: HashOptimizationLevel,
    pub fmt: bool,
    pub jit: bool,
}

#[repr(u32)]
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum HashOptimizationLevel {
    None = 0,
    Less = 1,
    Default = 2,
    Aggressive = 3,
}

pub fn convert(optimization: u64) -> HashOptimizationLevel {
    match optimization {
        0 => HashOptimizationLevel::None,
        1 => HashOptimizationLevel::Less,
        2 => HashOptimizationLevel::Default,
        3 => HashOptimizationLevel::Aggressive,
        _ => panic!("optimization level must be 0-3"),
    }
}

impl Default for HashOptimizationLevel {
    /// Returns the default value for `OptimizationLevel`, namely `OptimizationLevel::Default`.
    fn default() -> Self {
        HashOptimizationLevel::Default
    }
}

#[cfg(feature = "llvm")]
impl HashOptimizationLevel {
    pub fn to_llvm(self) -> OptimizationLevel {
        match self {
            HashOptimizationLevel::None => OptimizationLevel::None,
            HashOptimizationLevel::Less => OptimizationLevel::Less,
            HashOptimizationLevel::Default => OptimizationLevel::Default,
            HashOptimizationLevel::Aggressive => OptimizationLevel::Aggressive,
        }
    }
}

/// # ActionType
/// lsp action type
#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub enum ActionType {
    Completion,
    GotoDef,
    FindReferences,
    SemanticTokensFull,
    Diagnostic,
    Hover,
    Compile,
    PrintAst,
    Flow,
    Fmt,
    LspFmt,
    Hint,
    DocSymbol,
    SignatureHelp,
    CodeLens,
}
