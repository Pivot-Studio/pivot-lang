use ariadne::{Cache, ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use colored::Colorize;
use dyn_fmt::AsStrFormatExt;
use internal_macro::range;
use lazy_static::lazy_static;
use rustc_hash::FxHashMap;
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    process::exit,
};
use ustr::Ustr;

macro_rules! define_diag {
    (
        $code:expr,
        $level:ident,
        $(
            $ident:ident = $string_keyword:expr
        ),*
    ) => {
        paste::paste! {
            #[derive(Debug, PartialEq, Clone, Copy, Eq, Hash, Default, PartialOrd, Ord)]
            #[allow(non_camel_case_types, dead_code)]
            #[allow(clippy::upper_case_acronyms)]
            pub enum [<$level:camel Code>] {
                #[default] UNKNOWN = $code,
                $($ident),*
            }
            $(pub const $ident: &'static str = $string_keyword;)*
            lazy_static! {
                pub static ref [<$level:upper _MSG>]: HashMap<[<$level:camel Code>], &'static str> = {
                    let mut mp = HashMap::new();
                    $(mp.insert([<$level:camel Code>]::$ident, $ident);)*
                    mp
                };
            }
        }
    };
    (
        $code:expr,
        $level:ident,
        $(
            $ident:ident = $string_keyword:expr
        ),*,
    ) => {
        define_diag!($code, $level, $($ident = $string_keyword),*);
    };
}

define_diag!(
    114513,
    Error,
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
    VALUE_NOT_COMPARABLE = "value not comparable",
    LOGIC_OP_NOT_BOOL = "logic operation parameters must be bool",
    UNRECOGNIZED_BIN_OPERATOR = "unrecognized binary operator",
    UNRECOGNIZED_UNARY_OPERATOR = "unrecognized unary operator",
    INVALID_UNARY_EXPRESSION = "invalid unary expression",
    STRUCT_FIELD_NOT_FOUND = "struct field or method not found",
    INVALID_GET_FIELD = "cannot get field from non struct type",
    VAR_NOT_FOUND = "variable not found",
    REDECLARATION = "redeclaration of variable",
    FOR_CONDITION_MUST_BE_BOOL = "for condition must be bool",
    COMPLETION = "exp not complete",
    NOT_ASSIGNABLE = "left value is not assignable",
    ASSIGN_CONST = "try assigning to a const value",
    REF_CONST = "try referencing to a const value",
    INVALID_STRUCT_DEF = "invalid struct definition",
    UNDEFINED_TYPE = "undefined type",
    UNKNOWN_TYPE = "unknown type",
    TYPE_CANNOT_BE_FULLY_INFERRED = "type cannot be fully inferred",
    RETURN_VALUE_IN_VOID_FUNCTION = "return value in void function",
    RETURN_TYPE_MISMATCH = "return type mismatch",
    NO_RETURN_VALUE_IN_NON_VOID_FUNCTION = "non void function must have a return value",
    FUNCTION_MUST_HAVE_RETURN = "function must have a return value",
    REDEFINE_TYPE = "redefine type",
    STRUCT_FIELD_TYPE_NOT_MATCH = "struct field type not match",
    SYNTAX_ERROR_MISSING_SEMI = "expect semicolon here",
    EXPECT_TYPE = "expect type",
    EXPECT_VALUE = "expect value",
    REDEFINE_SYMBOL = "redefined symbol",
    SYMBOL_NOT_FOUND = "symbol not found",
    UNRESOLVED_MODULE = "unresolved module",
    ARRAY_TYPE_NOT_MATCH = "array type not match",
    ARRAY_INIT_EMPTY = "array init cannot be empty while no type specified",
    CANNOT_INDEX_NON_ARRAY = "cannot index non array type",
    ARRAY_INDEX_MUST_BE_INT = "array index must be int",
    ARRAY_INDEX_OUT_OF_BOUNDS = "array index out of bounds",
    NEEDED_INDEX_FOR_ARRAY_ELEMENT_ACCESS = "needed index for array element access",
    SIZE_MUST_BE_INT = "size must be int",
    TYPE_MISMATCH = "type mismatch",
    ILLEGAL_GET_FIELD_OPERATION = "illegal get field operation",
    NOT_A_POINTER = "not a pointer",
    ILLEGAL_SELF_RECURSION = "illegal self recursion, please use pointer",
    GENERIC_CANNOT_BE_INFER = "generic can not be infer",
    RECEIVER_CANNOT_BE_INFER = "receiver can not be inferred",
    DUPLICATE_METHOD = "duplicate method",
    DUPLICATE_TRAIT_BOUND = "duplicate trait bound",
    GENERIC_PARAM_LEN_MISMATCH = "generic param len mismatch",
    GENERIC_NOT_FOUND = "generic not found",
    NOT_GENERIC_TYPE = "not generic type",
    EXPECT_TRAIT_TYPE = "expect trait type",
    EXPECT_STRUCT_TYPE = "expect struct type",
    METHOD_NOT_IN_TRAIT = "method not in trait def",
    METHOD_NOT_IN_IMPL = "method required in trait not found in impl block",
    EXPECT_PUBLIC_SYMBOL = "expect public symbol",
    EXPECT_PUBLIC_FUNCTION = "expect public function",
    EXPECT_PUBLIC_STRUCT = "expect public struct",
    EXPECT_PUBLIC_TRAIT = "expect public trait",
    EXPECT_PUBLIC_FIELD = "expect public field",
    TRAIT_METHOD_SHALL_NOT_HAVE_MODIFIER = "trait method shall not have modifier",
    MACRO_NOT_FOUND = "macro not found",
    EXPECT_IDENTIFIER = "expect identifier",
    SYNTAX_ERROR_UNEXPECTED_TOKEN = "syntax error: unexpected token",
    EXPECT_STRING = "expect string",
    EXPECT_EXPRESSION = "expect expression",
    EXPECT_STATEMENT = "expect statement",
    EXPECT_STATEMENTS = "expect statements",
    NO_MACRO_LOOP_VAR = "no macro loop var used in macro loop block",
    MACRO_LOOP_VAR_USED_OUT_OF_LOOP = "macro loop var used out of loop",
    EMPTY_MACRO_LOOP = "empty macro loop",
    MACRO_VAR_NOT_FOUND = "macro var not found",
    EXPECT_PUBLIC_UNION = "expect public union",
    INVALID_UNION_CAST = "invalid union cast",
    INVALID_DIRECT_UNION_CAST = "invalid direct union cast",
    INVALID_DIRECT_TRAIT_CAST = "invalid direct trait cast",
    UNION_DOES_NOT_CONTAIN_TYPE = "union does not contain type",
    INVALID_IS_EXPR = "invalid `is` expression",
    INVALID_CAST = "invalid cast",
    METHOD_NOT_FOUND = "method not found",
    DERIVE_TRAIT_NOT_IMPL = "derive trait not impl",
    CANNOT_ASSIGN_INCOMPLETE_GENERICS = "cannot assign incomplete generic function to variable",
    FUNCTION_TYPE_NOT_MATCH = "function type not match",
    NO_RETURN_VALUE_EXPECTED_IN_VOID_FUNCTION = "no return value expected in a void function",
    CLOSURE_RET_TYPE_UNKNOWN = "cannot infer closure return type",
    CLOSURE_PARAM_TYPE_UNKNOWN = "cannot infer closure param type",
    CANNOT_IMPL_TYPE_OUT_OF_DEFINE_MOD = "cannot impl a type out of the define mod",
    TRAIT_METHOD_NOT_FOUND = "trait method not found",
    ONLY_TRAIT_CAN_BE_IMPL = "only trait can be impl",
    EXPECT_TO_BE_A_TRAIT_IMPL = "expect to be a trait impl block",
    TARGET_TYPE_NOT_IMPL_ABLE = "target type not implable",
    TUPLE_ELM_SIZE_MISS_MATCH = "tuple element size miss match",
    DEF_DECONSTRUCT_MUST_HAVE_VALUE = "def deconstruct must have value",
    STRUCT_FIELD_NOT_EXISTS = "struct field not exists",
    TRY_TO_EXPORT_NON_REEXPORT_SYMBOL = "try to export non reexport symbol",
    CYCLE_DEPENDENCY = "cycle dependency not allowed",
    YIELD_RETURN_MUST_BE_IN_GENERATOR = "yield return must be in generator function",
    GENERATOR_FN_MUST_RET_ITER = "generator function must return an Iterator",
    ASYNC_FN_MUST_RET_TASK = "async function must return a Task",
    INVALID_RET_IN_GENERATOR_FUNCTION = "invalid `return` in generator function",
    ARRAY_LEN_MUST_BE_I64 = "array len must be i64",
    EXPECT_ARRAY_TYPE = "expect array type",
    EXPECT_INT_VALUE = "expect int value",
    METHODS_MUST_HAVE_BODY = "methods must have body",
    INVALID_STRUCT_INIT = "invalid struct initialization",
    REDUNDANT_COMMA = "REDUNDANT comma",
    GENERIC_NOT_ALLOWED_IN_TRAIT_METHOD = "generic not allowed in trait method",
    THE_TARGET_TRAIT_CANNOT_BE_INSTANTIATED = "the target trait type cannot be instantiated",
    MACRO_EXPAND_DEPTH_TOO_DEEP = "macro expand depth too deep",
    ILLEGAL_GENERIC_PARAM = "illegal generic parameter",
    ILLEGAL_MATCH_VALUE =
        "illegal match value, match vale can only be of type `Union` or `Struct` or `Primitive`",
    ILLEGAL_MATCH_ARM_CONDITION =
        "illegal match arm condition: this match condition will never be met",
    SYNTAX_ERROR_REDUNDENT_SYMBOL = "syntax error: redundant symbol",
    SYNTAX_ERROR_IF_CONDITION = "syntax error: if condition",
    SYNTAX_ERROR_WHILE_CONDITION = "syntax error: while condition",
    SYNTAX_ERROR_FOR_CONDITION = "syntax error: for condition",
    ONLY_TASK_CAN_BE_AWAIT = "only Task can be awaited",
    GENERATOR_FUNCTION_CANNOT_RETURN_VOID = "generator function cannot return void",
    MACRO_EXPANSION_FAILED = "macro expansion failed",
);

define_diag! {
    1919809,
    Warn,
    UNREACHABLE_STATEMENT= "unreachable statement",
    UNUSED_VARIABLE = "unused variable",
    UNUSED_FUNCTION = "unused function",
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DiagCode {
    Err(ErrorCode),
    Warn(WarnCode),
}

impl Default for DiagCode {
    fn default() -> Self {
        DiagCode::Err(ErrorCode::UNKNOWN)
    }
}
impl Display for DiagCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            DiagCode::Err(e) => write!(f, "E{}", *e as u32),
            DiagCode::Warn(w) => write!(f, "W{}", *w as u32),
        }
    }
}

use lsp_types::{Diagnostic, DiagnosticSeverity, DiagnosticTag};

use crate::{lsp::mem_docs::MemDocsInput, utils::url_from_path, Db};

use super::{
    accumulators::Diagnostics,
    compiler::compile_dry,
    ctx::Ctx,
    range::{Pos, Range},
};

#[range]
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PLLabel {
    file: Ustr,
    txt: Option<(Ustr, Vec<Ustr>)>,
}
#[range]
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PLDiagRaw {
    code: DiagCode,
    help: Option<String>,
    pub labels: Vec<PLLabel>,
    pub source: Option<String>,
}
/// # PLDiag
/// Diagnostic for pivot-lang
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PLDiag {
    pub raw: Box<PLDiagRaw>,
}

const PL_DIAG_SOURCE: &str = "plsp";

impl PLLabel {
    pub fn new(range: Range, file: Ustr, txt: Option<(Ustr, Vec<Ustr>)>) -> Self {
        Self { file, txt, range }
    }
}

impl Pos {
    pub fn utf8_offset(&self, doc: &Source) -> usize {
        doc.line(self.line - 1).unwrap().offset() + self.column - 1
    }
}
use regex::Regex;
use std::fmt::Debug;
impl PLDiag {
    #[cfg(test)]
    pub fn rm_file(&mut self) {
        self.raw.source = None;
        self.raw.labels.iter_mut().for_each(|label| {
            label.file = "".into();
        })
    }
    pub fn print(&self, path: &str, f: impl Fn(&dyn Db, &str) -> Source + 'static, db: &dyn Db) {
        let mut colors = ColorGenerator::new();
        let mut rb = Report::build(
            self.get_report_kind(),
            path,
            self.raw.range.start.utf8_offset(&f(db, path)),
        )
        .with_code(self.raw.code)
        .with_message(self.get_msg());
        let mut labels = vec![];
        self.raw
            .labels
            .iter()
            .find(|label| {
                label.range.start.line == self.raw.range.start.line
                    && self.raw.range.end.line == label.range.end.line
                    && label.file == path
            })
            .or_else(|| {
                labels.push(PLLabel {
                    range: self.raw.range,
                    file: path.into(),
                    txt: Some(("here".into(), vec![])),
                });
                None
            });

        for label in labels.iter().chain(self.raw.labels.iter()) {
            let color = colors.next();
            let mut lab;
            if let Some((tpl, args)) = &label.txt {
                lab = Label::new((
                    label.file.as_str(),
                    label.range.start.utf8_offset(&f(db, label.file.as_str()))
                        ..label.range.end.utf8_offset(&f(db, label.file.as_str())),
                ));
                let mut msg = *tpl;
                msg = msg
                    .format(
                        &args
                            .iter()
                            .map(|s| s.fg(color).to_string())
                            .collect::<Vec<_>>(),
                    )
                    .into();
                lab = lab.with_message(msg);
            } else {
                lab = Label::new((
                    label.file.as_str(),
                    label.range.start.utf8_offset(&f(db, label.file.as_str()))
                        ..label.range.end.utf8_offset(&f(db, label.file.as_str())),
                ));
            }
            rb = rb.with_label(lab.with_color(color));
        }
        if let Some(help) = &self.raw.help {
            let re = Regex::new(r"`[a-zA-Z_$][a-zA-Z_$0-9]*`").unwrap();
            let h = re.replace_all(help, |cap: &regex::Captures<'_>| {
                let s = cap[0].to_string();
                s.fg(colors.next()).to_string()
            });
            rb = rb.with_help(h);
        }
        let r = rb.finish();
        r.eprint(PLFileCache::new(db, Box::new(f))).unwrap();
    }
    fn get_report_kind(&self) -> ReportKind {
        match self.raw.code {
            DiagCode::Err(_) => ReportKind::Error,
            DiagCode::Warn(_) => ReportKind::Warning,
        }
    }
    pub fn is_err(&self) -> bool {
        match self.raw.code {
            DiagCode::Err(_) => true,
            DiagCode::Warn(_) => false,
        }
    }
    pub fn is_warn(&self) -> bool {
        match self.raw.code {
            DiagCode::Err(_) => false,
            DiagCode::Warn(_) => true,
        }
    }
    pub fn get_msg(&self) -> String {
        match self.raw.code {
            DiagCode::Err(code) => ERROR_MSG[&code].to_string(),
            DiagCode::Warn(code) => WARN_MSG[&code].to_string(),
        }
    }
    pub fn get_diagnostic(&self, p: &str, diags: &mut FxHashMap<String, Vec<Diagnostic>>) {
        let mut d = match self.raw.code {
            DiagCode::Err(code) => Diagnostic::new_with_code_number(
                self.raw.range.to_diag_range(),
                DiagnosticSeverity::ERROR,
                code as i32,
                Some(PL_DIAG_SOURCE.to_string()),
                format!(
                    "{} {}",
                    ERROR_MSG[&code],
                    &self
                        .raw
                        .help
                        .clone()
                        .map(|h| format!("({})", h))
                        .unwrap_or_default()
                ),
            ),
            DiagCode::Warn(code) => {
                let mut warn = Diagnostic::new_with_code_number(
                    self.raw.range.to_diag_range(),
                    DiagnosticSeverity::WARNING,
                    code as i32,
                    Some(PL_DIAG_SOURCE.to_string()),
                    format!(
                        "{} {}",
                        WARN_MSG[&code],
                        &self
                            .raw
                            .help
                            .clone()
                            .map(|h| format!("({})", h))
                            .unwrap_or_default()
                    ),
                );
                if code == WarnCode::UNUSED_FUNCTION
                    || code == WarnCode::UNUSED_VARIABLE
                    || code == WarnCode::UNREACHABLE_STATEMENT
                {
                    warn.tags = Some(vec![DiagnosticTag::UNNECESSARY]);
                }
                warn
            }
        };
        let mut labels = vec![];
        self.raw.labels.iter().for_each(|label| {
            let mut lab = lsp_types::DiagnosticRelatedInformation {
                location: lsp_types::Location {
                    uri: url_from_path(&label.file),
                    range: label.range.to_diag_range(),
                },
                message: "related source here".to_string(),
            };
            if let Some((tpl, args)) = &label.txt {
                lab.message = tpl.to_string();
                lab.message = lab.message.format(args);
            }
            labels.push(lab);
        });
        d.related_information = Some(labels);
        let p = if let Some(source) = &self.raw.source {
            source.clone()
        } else {
            p.to_string()
        };
        diags.entry(p).or_default().push(d);
    }
    pub fn get_range(&self) -> Range {
        self.raw.range
    }
    pub fn get_diag_code(&self) -> DiagCode {
        self.raw.code
    }
    pub fn new_error(range: Range, code: ErrorCode) -> Self {
        PLDiag {
            raw: Box::new(PLDiagRaw {
                range,
                code: DiagCode::Err(code),
                ..Default::default()
            }),
        }
    }
    pub fn set_source(&mut self, source: &str) -> &mut Self {
        self.raw.source = Some(source.to_string());
        self
    }
    pub fn add_help(&mut self, help: &str) -> &mut Self {
        self.raw.help = Some(help.to_string());
        self
    }
    pub fn add_to_ctx(&self, ctx: &Ctx) -> PLDiag {
        let mut d = self.clone();
        if ctx.in_macro {
            d.raw.labels.push(ctx.macro_original_loc.clone().unwrap());
        }
        ctx.add_diag(d)
    }

    /// Add a label to the diagnostic
    ///
    /// # Arguments
    /// * `range` - The src range of the label
    /// * `label` - The label text and arguments, you may use `format_label` macro to build it
    pub fn add_label(
        &mut self,
        range: Range,
        file: Ustr,
        txt: Option<(Ustr, Vec<Ustr>)>,
    ) -> &mut Self {
        if range == Default::default() {
            return self;
        }
        self.raw.labels.push(PLLabel { file, txt, range });
        self
    }

    pub fn set_range(&mut self, range: Range) -> &mut Self {
        self.raw.range = range;
        self
    }

    pub fn new_warn(range: Range, code: WarnCode) -> Self {
        PLDiag {
            raw: Box::new(PLDiagRaw {
                range,
                code: DiagCode::Warn(code),
                ..Default::default()
            }),
        }
    }
}

pub struct PLFileCache<'a> {
    db: &'a dyn Db,
    f: PLDb2Src<'a>,
    cache: FxHashMap<String, Source>,
}
type PLDb2Src<'a> = Box<dyn Fn(&'a dyn Db, &str) -> Source>;
impl<'a> PLFileCache<'a> {
    pub fn new(db: &'a dyn Db, f: PLDb2Src<'a>) -> Self {
        Self {
            db,
            f,
            cache: Default::default(),
        }
    }
}

impl<'a> Cache<&str> for PLFileCache<'a> {
    fn fetch(&mut self, id: &&str) -> Result<&Source, Box<dyn std::fmt::Debug + '_>> {
        if !self.cache.contains_key(*id) {
            self.cache.insert((*id).to_string(), (self.f)(self.db, id));
        }
        Ok(self.cache.get(*id).unwrap())
    }

    fn display<'b>(&self, id: &'b &str) -> Option<Box<dyn std::fmt::Display + 'b>> {
        Some(Box::new(id))
    }
}

/// ensure_no_error validates the results after parsing from db and docs to ensure there is no error.
/// it panic directly if there is an error.
pub(crate) fn ensure_no_error(db: &dyn Db, docs: MemDocsInput) {
    let mut errs_num = 0;
    let mut warn_num = 0;
    let mut errs = compile_dry::accumulated::<Diagnostics>(db, docs);
    if !errs.is_empty() {
        print_diags(
            errs.drain(..).map(|e| e.0).collect(),
            docs,
            db,
            &mut errs_num,
            &mut warn_num,
            false,
        );
        if errs_num > 0 {
            eprintln!(
                "check failed: {} error(s), {} warning(s)",
                errs_num.to_string().bright_red(),
                warn_num.to_string().yellow()
            );
            exit(1);
        }
        eprintln!(
            "    {} error(s), {} warning(s)",
            errs_num.to_string().bright_red(),
            warn_num.to_string().yellow()
        );
    }
}

pub(crate) fn print_diags(
    errs: Vec<(String, Vec<PLDiag>)>,
    docs: MemDocsInput,
    db: &dyn Db,
    errs_num: &mut i32,
    warn_num: &mut i32,
    err_only: bool,
) {
    for e in errs.iter() {
        let mut path = e.0.clone();
        for e in e.1.iter() {
            if err_only && e.is_warn() {
                continue;
            }
            if let Some(src) = e.raw.source.clone() {
                path = src;
            }
            e.print(
                &path,
                move |db, id| {
                    Source::from(docs.get_file_content(db, id.to_string()).unwrap().text(db))
                },
                db,
            );
            if e.is_err() {
                *errs_num += 1
            } else if e.is_warn() {
                *warn_num += 1
            }
        }
    }
}
