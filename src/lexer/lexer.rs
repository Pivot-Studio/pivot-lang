use core::fmt;
use std::collections::HashMap;

use lazy_static::lazy_static;

use super::types::Keyword;
use super::types::Operator;
use super::types::TokenType;
use super::types::Token;
use super::types::KEYWORDS_MAP;
use super::pos::Range;
use core::iter::Peekable;
use core::str::Chars;
#[derive(Debug)]
pub struct Lexer<'a> {
    tokens: Vec<TokenType>,
    input: &'a str,
    offsset: usize, // offset char 0 based
    line: usize, // 1 based line
    column: usize, // 1 based col
}


lazy_static!(
    static ref TOKEN_MAP: HashMap<&'static char, TokenType> = {
        let mut mp = HashMap::new();
        mp.insert(&'+', TokenType::Operator(Operator::PLUS));
        mp.insert(&'-', TokenType::Operator(Operator::MINUS));
        mp.insert(&'*', TokenType::Operator(Operator::MUL));
        mp.insert(&'/', TokenType::Operator(Operator::DIV));
        mp.insert(&'(', TokenType::LPAREN);
        mp.insert(&')', TokenType::RPAREN);
        mp.insert(&'\r', TokenType::WhiteSpace);
        mp.insert(&' ', TokenType::WhiteSpace);
        mp.insert(&'\n', TokenType::NewLine);
        mp
    };
);

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut peekable = input.chars().peekable();
        let mut tokens = vec![];
        while let Ok(token) = Self::next_token(&mut peekable) {
            if token == TokenType::EOF {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        Self { input, tokens, offsset: 0, line: 1, column: 1 }
    }
    // fn eat_token(expect_type: TokenType) -> Result<(), TokenizerError> {
    // }

    fn next_token(peekable: &mut Peekable<Chars<'_>>) -> Result<TokenType, TokenizerError> {
        if let Some(&ch) = peekable.peek() {
            let tp = TOKEN_MAP.get(&ch);
            if let Some(tp) = tp {
                peekable.next();
                return Ok(*tp);
            }
            if Self::is_letter_or_underscore(ch) {
                let mut str = String::new();
                str.push(ch);
                peekable.next();
                while let Some(&ch) = peekable.peek() {
                    if !Self::is_letter_or_underscore(ch) && !Self::is_num(ch) {
                        break;
                    }
                    str.push(ch);
                    peekable.next();
                    continue;
                }
                if let Some(&keyword) = KEYWORDS_MAP.get(&str.as_str()) {
                    return Ok(TokenType::Keyword(keyword));
                }
                return Ok(TokenType::String);
            }
            if Self::is_num(ch) {
                let mut str = String::new();
                str.push(ch);
                peekable.next();
                let mut is_float = false;
                while let Some(&ch) = peekable.peek() {
                    if ch == '.' {
                        is_float = true;
                        str.push(ch);
                        peekable.next();
                        continue;
                    }
                    if !Self::is_num(ch) {
                        break;
                    }
                    str.push(ch);
                    peekable.next();
                    continue;
                }
                if is_float {
                    return Ok(TokenType::FLOAT);
                }
                return Ok(TokenType::INT);
            }
            Err(TokenizerError {
                message: "unknown ch".to_string(),
            })
        } else {
            Ok(TokenType::EOF)
        }
    }
    fn is_letter(ch: char) -> bool {
        ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')
    }
    fn is_letter_or_underscore(ch: char) -> bool {
        Self::is_letter(ch) || ch == '_'
    }
    fn is_num(ch: char) -> bool {
        '0' <= ch && ch <= '9'
    }
}
impl fmt::Display for Lexer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {:?}", self.input, self.tokens)
    }
}
#[test]
fn test_token_vec_gen() {
    let lexer = Lexer::new("+-* / fn fnabc\n34\r3.145");
    let tokens = lexer.tokens;
    let expected = vec![
        TokenType::Operator(Operator::PLUS),
        TokenType::Operator(Operator::MINUS),
        TokenType::Operator(Operator::MUL),
        TokenType::WhiteSpace,
        TokenType::Operator(Operator::DIV),
        TokenType::WhiteSpace,
        TokenType::Keyword(Keyword::FN),
        TokenType::WhiteSpace,
        TokenType::String,
        TokenType::NewLine,
        TokenType::INT,
        TokenType::WhiteSpace,
        TokenType::FLOAT,
        TokenType::EOF,
    ];
    assert_eq!(tokens, expected);
}
#[derive(Debug, PartialEq, Eq)]
pub struct TokenizerError {
    pub message: String,
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}
