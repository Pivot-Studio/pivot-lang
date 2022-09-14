use lazy_static::lazy_static;
use std::collections::HashMap;
use super::pos::Range;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    Operator(Operator), // 运算符
    Keyword(Keyword),   // 关键字
    String,     // 字符串
    INT,        //整形
    FLOAT,      //浮点数
    LPAREN,             // (
    RPAREN,             // )
    WhiteSpace,         // ' ','\r'
    NewLine,            // '\n'
    EOF,                //EOF
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub range: Range,
}


#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
