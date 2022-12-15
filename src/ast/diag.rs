use internal_macro::range;
use lazy_static::lazy_static;
use std::collections::HashMap;
macro_rules! define_error {
    ($(
        $ident:ident = $string_keyword:expr
    ),*) => {
        #[derive(Debug, PartialEq, Clone, Copy, Eq, Hash, Default)]
        #[allow(non_camel_case_types, dead_code)]
        pub enum ErrorCode {
            #[default] UNKNOWN = 114513,
            $($ident),*
        }
        $(pub const $ident: &'static str = $string_keyword;)*
        lazy_static! {
            pub static ref ERR_MSG: HashMap<ErrorCode, &'static str> = {
                let mut mp = HashMap::new();
                $(mp.insert(ErrorCode::$ident, $ident);)*
                mp
            };
        }
    };
}
define_error!(
    SYNTAX_ERROR_STATEMENT = "failed to parse statement",
    SYNTAX_ERROR_TOP_STATEMENT = "failed to parse top level statement",
    WHILE_CONDITION_MUST_BE_BOOL = "while condition must be bool",
    IF_CONDITION_MUST_BE_BOOL = "if condition must be bool",
    BREAK_MUST_BE_IN_LOOP = "break statement must be in loop",
    CONTINUE_MUST_BE_IN_LOOP = "continue statement must be in loop",
    VOID_TYPE_CANNOT_BE_PARAMETER = "void type cannot be function parameter",
    PARAMETER_TYPE_NOT_MATCH = "parameter type not match",
    PARAMETER_LENGTH_NOT_MATCH = "parameter length not match",
    FUNCTION_NOT_FOUND = "function not found",
    NOT_A_FUNCTION = "not a function",
    BIN_OP_TYPE_MISMATCH = "binary operation type mismatch",
    VALUE_NOT_COMPARABLE = "value not comparable",
    LOGIC_OP_NOT_BOOL = "logic operation parameters must be bool",
    UNRECOGNIZED_BIN_OPERATOR = "unrecognized binary operator",
    UNRECOGNIZED_UNARY_OPERATOR = "unrecognized unary operator",
    INVALID_UNARY_EXPRESSION = "invalid unary expression",
    STRUCT_FIELD_NOT_FOUND = "struct field not found",
    INVALID_GET_FIELD = "cannot get field from non struct type",
    VAR_NOT_FOUND = "variable not found",
    REDECLARATION = "redeclaration of variable",
    FOR_CONDITION_MUST_BE_BOOL = "for condition must be bool",
    COMPLETION = "exp not complete",
    ASSIGN_TYPE_MISMATCH = "assign type mismatch",
    NOT_ASSIGNABLE = "left value is not assignable",
    ASSIGN_CONST = "try assigning to a const value",
    REF_CONST = "try referencing to a const value",
    INVALID_STRUCT_DEF = "invalid struct definition",
    UNDEFINED_TYPE = "undefined type",
    RETURN_VALUE_IN_VOID_FUNCTION = "return value in void function",
    RETURN_TYPE_MISMATCH = "return type mismatch",
    NO_RETURN_VALUE_IN_NON_VOID_FUNCTION = "non void function must have a return value",
    FUNCTION_MUST_HAVE_RETURN = "function must have a return value",
    REDEFINE_TYPE = "redefine type",
    STRUCT_FIELD_TYPE_NOT_MATCH = "struct field type not match",
    MISSING_SEMI = "missing semicolon",
    EXPECT_TYPE = "expect type",
    EXPECT_VALUE = "expect value",
    REDEFINE_SYMBOL = "redefine symbol",
    SYMBOL_NOT_FOUND = "symbol not found",
    UNRESOLVED_MODULE = "unresolved module",
    ARRAY_TYPE_NOT_MATCH = "array type not match",
    ARRAY_INIT_EMPTY = "array init cannot be empty",
    CANNOT_INDEX_NON_ARRAY = "cannot index non array type",
    ARRAY_INDEX_MUST_BE_INT = "array index must be int",
    ARRAY_INDEX_OUT_OF_BOUNDS = "array index out of bounds",
    NEEDED_INDEX_FOR_ARRAY_ELEMENT_ACCESS = "needed index for array element access",
    SIZE_MUST_BE_INT = "size must be int",
    TYPE_MISMATCH = "mismatch",
    ILLEGAL_GET_FIELD_OPERATION = "illegal get field operation",
    NOT_A_POINTER = "not a pointer",
    CAN_NOT_REF_CONSTANT = "can not ref constant",
    ILLEGAL_SELF_RECURSION = "illegal self recursion, please use pointer",
    GENERIC_CANNOT_BE_INFER = "generic can not be infer",
    DUPLICATE_METHOD = "duplicate method",
    GENERIC_PARAM_LEN_MISMATCH = "generic param len mismatch",
    NOT_GENERIC_TYPE = "not generic type"
);
macro_rules! define_warn {
    ($(
        $ident:ident = $string_keyword:expr
    ),*) => {
        #[derive(Debug, PartialEq, Clone, Copy, Eq, Hash, Default)]
        #[allow(non_camel_case_types, dead_code)]
        pub enum WarnCode {
            #[default] UNKNOWN = 1919810,
            $($ident),*
        }
        $(pub const $ident: &'static str = $string_keyword;)*
        lazy_static! {
            pub static ref WARN_MSG: HashMap<WarnCode, &'static str> = {
                let mut mp = HashMap::new();
                $(mp.insert(WarnCode::$ident, $ident);)*
                mp
            };
        }
    };
}
define_warn! {
    UNREACHABLE_STATEMENT= "unreachable statement"
}

#[derive(Debug, Clone, Copy)]
pub enum DiagCode {
    Err(ErrorCode),
    Warn(WarnCode),
}

impl Default for DiagCode {
    fn default() -> Self {
        DiagCode::Err(ErrorCode::UNKNOWN)
    }
}

use lsp_types::{Diagnostic, DiagnosticSeverity};

use super::range::Range;

/// # PLDiag
/// Diagnostic for pivot-lang
#[range]
#[derive(Debug, Clone, Default)]
pub struct PLDiag {
    code: DiagCode,
    help: Option<Box<String>>,
    labels: Vec<LabeledSpan>,
}

const PL_DIAG_SOURCE: &str = "plsp";

impl PLDiag {
    fn to_file_diag(&self, path: &str, source: &str) -> TerminalDiag {
        let linestart = self.range.start.offset - self.range.start.column + 1;
        let mut start = self.range.start;
        start.offset = linestart;
        start.column = 1;
        let mut me = self.clone();
        if start != self.range.end && me.labels.len() == 0 {
            me.add_label(self.range, Some("here".to_string()));
        }
        TerminalDiag {
            diag: self.get_diagnostic().clone(),
            path: path.to_string(),
            source_code: source.to_string(),
            labels: me.labels,
            help: self.help.clone(),
        }
    }
    pub fn print(&self, path: &str, doc: &str) {
        let mut r = self.get_diagnostic().range.clone();
        r.start.character = 0;
        let a = miette::Report::new(self.to_file_diag(path, doc));
        println!("{a:?}");
    }
    pub fn is_err(&self) -> bool {
        self.get_diagnostic().severity == Some(DiagnosticSeverity::ERROR)
    }
    pub fn get_diagnostic(&self) -> Diagnostic {
        match self.code {
            DiagCode::Err(code) => Diagnostic::new_with_code_number(
                self.range.to_diag_range(),
                DiagnosticSeverity::ERROR,
                code as i32,
                Some(PL_DIAG_SOURCE.to_string()),
                ERR_MSG[&code].to_string(),
            ),
            DiagCode::Warn(code) => Diagnostic::new_with_code_number(
                self.range.to_diag_range(),
                DiagnosticSeverity::WARNING,
                code as i32,
                Some(PL_DIAG_SOURCE.to_string()),
                WARN_MSG[&code].to_string(),
            ),
        }
    }
    pub fn new_error(range: Range, code: ErrorCode) -> Self {
        PLDiag {
            range,
            code: DiagCode::Err(code),
            ..Default::default()
        }
    }
    pub fn add_help(&mut self, help: &str) -> &mut Self {
        self.help = Some(Box::new(help.to_string()));
        self
    }

    pub fn add_label(&mut self, range: Range, label: Option<String>) -> &mut Self {
        let startoffset = miette::ByteOffset::from(range.start.offset);
        let mut len = miette::ByteOffset::from(range.end.offset - range.start.offset);
        if len == 0 {
            len = 1;
        }
        self.labels.push(LabeledSpan::new(label, startoffset, len));
        self
    }

    pub fn new_warn(range: Range, code: WarnCode) -> Self {
        PLDiag {
            range,
            code: DiagCode::Warn(code),
            ..Default::default()
        }
    }
}

use miette::{Diagnostic as MietteDiagnostic, LabeledSpan, Severity};

#[derive(Debug, Clone)]
pub struct TerminalDiag {
    pub path: String,
    pub diag: Diagnostic,
    pub source_code: String,
    pub labels: Vec<LabeledSpan>,
    pub help: Option<Box<String>>,
}

impl TerminalDiag {
    fn get_code(&self) -> String {
        let code = match self.diag.code.as_ref().unwrap() {
            lsp_types::NumberOrString::Number(n) => *n,
            lsp_types::NumberOrString::String(_) => todo!(),
        };
        match self.diag.severity {
            Some(DiagnosticSeverity::ERROR) => {
                format!("E{}", code)
            }
            Some(DiagnosticSeverity::WARNING) => {
                format!("W{}", code)
            }
            _ => "????".to_string(),
        }
    }
}

impl std::fmt::Display for TerminalDiag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.diag.message.clone()))
    }
}

impl std::error::Error for TerminalDiag {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn description(&self) -> &str {
        "description() is deprecated; use Display"
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }
}

impl MietteDiagnostic for TerminalDiag {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(self.get_code()))
    }

    fn severity(&self) -> Option<miette::Severity> {
        Some(match self.diag.severity {
            Some(s) => match s {
                DiagnosticSeverity::ERROR => Severity::Error,
                DiagnosticSeverity::WARNING => Severity::Warning,
                DiagnosticSeverity::INFORMATION => Severity::Advice,
                DiagnosticSeverity::HINT => Severity::Advice,
                _ => todo!(),
            },
            None => todo!(),
        })
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        match &self.help {
            Some(h) => Some(Box::new(h.clone())),
            None => None,
        }
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(format!(
            "{}:{}:{}",
            self.path,
            self.diag.range.start.line + 1,
            self.diag.range.start.character + 1
        )))
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.source_code)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        Some(Box::new(self.labels.clone().into_iter()))
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn MietteDiagnostic> + 'a>> {
        None
    }

    fn diagnostic_source(&self) -> Option<&dyn MietteDiagnostic> {
        None
    }
}
