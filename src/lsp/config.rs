use lazy_static::lazy_static;
use lsp_types::{SemanticTokenModifier, SemanticTokenType, SemanticTokensLegend};

lazy_static! {
    pub static ref SEMANTIC_LEGEND: SemanticTokensLegend = {
        lsp_types::SemanticTokensLegend {
            token_types: vec![
                SemanticTokenType::NAMESPACE,
                SemanticTokenType::TYPE,
                SemanticTokenType::CLASS,
                SemanticTokenType::ENUM,
                SemanticTokenType::INTERFACE,
                SemanticTokenType::STRUCT,
                SemanticTokenType::TYPE_PARAMETER,
                SemanticTokenType::FUNCTION,
                SemanticTokenType::METHOD,
                SemanticTokenType::PROPERTY,
                SemanticTokenType::MACRO,
                SemanticTokenType::VARIABLE,
                SemanticTokenType::PARAMETER,
                SemanticTokenType::ENUM_MEMBER,
                SemanticTokenType::STRING,
                SemanticTokenType::NUMBER,
                SemanticTokenType::KEYWORD,
                SemanticTokenType::MODIFIER,
                SemanticTokenType::COMMENT,
                SemanticTokenType::REGEXP,
                SemanticTokenType::OPERATOR,
            ],
            token_modifiers: vec![
                SemanticTokenModifier::DECLARATION,
                SemanticTokenModifier::DEFINITION,
                SemanticTokenModifier::READONLY,
                SemanticTokenModifier::STATIC,
                SemanticTokenModifier::ABSTRACT,
                SemanticTokenModifier::DEPRECATED,
                SemanticTokenModifier::ASYNC,
                SemanticTokenModifier::MODIFICATION,
                SemanticTokenModifier::DOCUMENTATION,
            ],
        }
    };
}
