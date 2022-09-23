use lazy_static::lazy_static;
use std::collections::HashMap;
macro_rules! define_tokens {
    ($(
        $ident:ident : $string_keyword:expr
    ),*) => {
        #[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
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
        }
    };
}
define_tokens!(
    PLUS : "+",
    MINUS : "-",  // -
    MUL : "*",    // *
    DIV : "/",    // /
    LPAREN : "(", // (
    RPAREN : ")", // )
    ASSIGN : "=", // =
    LET : "let" // let
);
impl TokenType {
    pub fn get_str(&self) -> &'static str {
        TOKEN_TYPE_MAP[self]
    }
}
