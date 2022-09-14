use lazy_static::lazy_static;
use std::collections::HashMap;
#[derive(Debug, PartialEq)]
pub enum Token {
    Operator(Operator), // 运算符
    Keyword(Keyword),   // 关键字
    String(String),     // 字符串
    INT(String),        //整形
    FLOAT(String),      //浮点数
    LPAREN,             // (
    RPAREN,             // )
    WhiteSpace,         // ' ','\n','\r'
    EOF,                //EOF
}
#[derive(Debug, PartialEq, Eq)]
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
        #[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
        pub enum Keyword {
            $($ident),*
        }
        $(pub const $ident: &'static str = $string_keyword;)*
        lazy_static! {
            pub static ref KEYWORDS_MAP: HashMap<&'static str,Keyword> = {
                let mut mp = HashMap::new();
                $(mp.insert($ident,Keyword::$ident);)*
                mp
            };
        }
    };
}
define_keywords!(FN = "fn");
#[test]
fn test_keyword_gen() {
    println!("keyword_map: {:?}", KEYWORDS_MAP["fn"]);
}
