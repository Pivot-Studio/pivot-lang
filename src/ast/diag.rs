use lazy_static::lazy_static;
use std::collections::HashMap;
macro_rules! define_error {
    ($(
        $ident:ident = $string_keyword:expr
    ),*) => {
        #[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
        #[allow(non_camel_case_types, dead_code)]
        pub enum ErrorCode {
            UNKNOWN = 114513,
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
    INVALID_STRUCT_DEF = "invalid struct definition",
    UNDEFINED_TYPE = "undefined type",
    RETURN_VALUE_IN_VOID_FUNCTION = "return value in void function",
    RETURN_TYPE_MISMATCH = "return type mismatch",
    NO_RETURN_VALUE_IN_NON_VOID_FUNCTION = "non void function must have a return value",
    FUNCTION_MUST_HAVE_RETURN = "function must have a return value",
    REDFINE_TYPE = "redefine type",
    STRUCT_FIELD_TYPE_NOT_MATCH = "struct field type not match"
);
macro_rules! define_warn {
    ($(
        $ident:ident = $string_keyword:expr
    ),*) => {
        #[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
        #[allow(non_camel_case_types, dead_code)]
        pub enum WarnCode {
            UNKNOWN = 1919810,
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
