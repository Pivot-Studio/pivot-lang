use lazy_static::lazy_static;
use std::collections::HashMap;
#[derive(Debug, PartialEq)]
pub enum Token {
    Operator(Operator), // 运算符
    Keyword(Keyword),   // 关键字
    LPAREN,             // (
    RPAREN,             // )
    EOF,                //EOF
}
#[derive(Debug, PartialEq)]
pub enum Operator {
    PLUS,  // +
    MINUS, // -
    MUL,   // *
    DIV,   // /
}
macro_rules! define_keywords {
    ($(
        $ident:ident = $string_keyword:expr
    ),*) => {
        #[derive(Debug, PartialEq,Eq,Hash)]
        pub enum Keyword {
            NoKeyword,
            $($ident),*
        }
        $(pub const $ident: &'static str = $string_keyword;)*
        lazy_static! {
            static ref KEYWORDS_MAP: HashMap<&'static str,Keyword> = {
                let mut mp = HashMap::new();
                $(mp.insert($ident,Keyword::$ident);)*
                mp
            };
        }
    };
}
define_keywords!(TESTA = "testa", TESTB = "testb");
#[test]
fn test_keyword_gen() {
    println!(
        "keyword_map: {:?} {:?}",
        KEYWORDS_MAP["testa"], KEYWORDS_MAP["testb"]
    );
}
