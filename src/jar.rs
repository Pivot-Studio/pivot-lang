#[salsa::jar(db = crate::Db)]
pub struct Jar(
    crate::nomparser::SourceProgram,
    crate::nomparser::parse,
    crate::lsp::mem_docs::MemDocsInput,
    crate::lsp::mem_docs::MemDocsInput_get_current_file_content,
    crate::lsp::mem_docs::MemDocsInput_get_file_content,
    crate::lsp::mem_docs::MemDocsInput_get_file_params,
    crate::lsp::mem_docs::EmitParams,
    crate::lsp::mem_docs::FileCompileInput,
    crate::lsp::mem_docs::FileCompileInput_get_file_content,
    crate::lsp::mem_docs::FileCompileInput_get_emit_params,
    crate::ast::compiler::compile,
    crate::ast::compiler::compile_dry,
    crate::ast::compiler::compile_dry_file,
    crate::ast::accumulators::Diagnostics,
    crate::ast::accumulators::PLReferences,
    crate::ast::accumulators::GotoDef,
    crate::ast::accumulators::Completions,
    crate::ast::accumulators::PLSemanticTokens,
    crate::ast::accumulators::PLHover,
    crate::ast::accumulators::ModBuffer,
    crate::ast::accumulators::PLFormat,
    crate::ast::accumulators::Hints,
    crate::ast::accumulators::DocSymbols,
    crate::ast::accumulators::PLSignatureHelp,
    crate::ast::node::program::Program,
    crate::ast::node::program::Program_emit,
    crate::ast::node::program::ProgramNodeWrapper,
    crate::ast::node::program::ModWrapper,
    crate::ast::node::program::ProgramEmitParam,
    crate::ast::node::program::emit_file,
    crate::ast::node::program::LspParams,
    crate::ast::node::program::Program_is_active_file,
    crate::utils::read_config::get_config,
    crate::utils::read_config::ConfigWrapper,
    crate::utils::read_config::ConfigWrapper_resolve_dep_path,
);

pub trait Db: salsa::DbWithJar<Jar> {
    fn set_ref_str(&self, ref_str: Option<String>);
    fn get_ref_str(&self) -> Option<String>;
}
