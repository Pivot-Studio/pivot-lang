use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use lazy_static::lazy_static;
use std::collections::HashMap;
macro_rules! define_tokens {
    ($(
        $ident:ident = $string_keyword:expr
    ),*) => {
        #[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
        #[allow(non_camel_case_types)]
        #[allow(clippy::upper_case_acronyms)]
        pub enum TokenType {
            $($ident),*
        }
        $(pub const $ident: &'static str = $string_keyword;)*
        lazy_static! {
            pub static ref TOKEN_TYPE_MAP: HashMap<TokenType, &'static str> = {
                let mut mp = HashMap::new();
                $(mp.insert(TokenType::$ident, $ident);)*
                mp
            };
            pub static ref TOKEN_STR_MAP: HashMap<&'static str, TokenType> = {
                let mut mp = HashMap::new();
                $(mp.insert($ident,TokenType::$ident);)*
                mp
            };
        }
    };
    ($(
        $ident:ident = $string_keyword:expr
    ),*,) => {
        define_tokens!($($ident = $string_keyword),*);
    };
}
define_tokens!(
    PLUS = "+",
    MINUS = "-",
    MUL = "*",
    DIV = "/",
    LPAREN = "(",
    RPAREN = ")",
    ASSIGN = "=",
    NOT = "!",
    LESS = "<",
    GREATER = ">",
    LEQ = "<=",
    GEQ = ">=",
    EQ = "==",
    NE = "!=",
    AND = "&&",
    OR = "||",
    LBRACE = "{",
    RBRACE = "}",
    LET = "let",
    IF = "if",
    ELSE = "else",
    WHILE = "while",
    FOR = "for",
    BREAK = "break",
    CONTINUE = "continue",
    COLON = ":",
    STRUCT = "struct",
    FN = "fn",
    COMMA = ",",
    RETURN = "return",
    DOT = ".",
    SEMI = ";",
    CONST = "const",
    USE = "use",
    DOUBLE_COLON = "::",
    LBRACKET = "[",
    RBRACKET = "]",
    TAKE_PTR = "&",
    TAKE_VAL = "*",
    IMPL = "impl",
    GENERIC_SEP = "|",
    INGNORE = "_",
    DOUBLE_QUOTE = "\"",
    TRUE = "true",
    FALSE = "false",
    TRAIT = "trait",
    PUB = "pub",
    DOLLAR = "$",
    MACRO_TYPE_ID = "@id",
    MACRO_TYPE_STR = "@str",
    MACRO_TYPE_EXPR = "@expr",
    MACRO_TYPE_STMT = "@stmt",
    MACRO_TYPE_STMTS = "@stmts",
    MACRO = "macro",
    ARROW = "=>",
    WHERE = "where",
    TYPE = "type",
    TYPE_OR = "|",
    AS = "as",
    QUESTION = "?",
);

impl TokenType {
    pub fn get_str(&self) -> &'static str {
        TOKEN_TYPE_MAP[self]
    }
    pub fn get_op(&self) -> IntPredicate {
        match self {
            TokenType::GREATER => IntPredicate::SGT,
            TokenType::LESS => IntPredicate::SLT,
            TokenType::LEQ => IntPredicate::SLE,
            TokenType::GEQ => IntPredicate::SGE,
            TokenType::EQ => IntPredicate::EQ,
            TokenType::NE => IntPredicate::NE,
            _ => panic!("expected logic op token,found {:?}", self),
        }
    }
    pub fn get_fop(&self) -> FloatPredicate {
        match self {
            TokenType::GREATER => FloatPredicate::OGT,
            TokenType::LESS => FloatPredicate::OLT,
            TokenType::LEQ => FloatPredicate::OLE,
            TokenType::GEQ => FloatPredicate::OGE,
            TokenType::EQ => FloatPredicate::OEQ,
            TokenType::NE => FloatPredicate::ONE,
            _ => panic!("expected logic op token,found {:?}", self),
        }
    }
}
