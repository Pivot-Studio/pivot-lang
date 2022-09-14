use core::fmt;

use super::types::Keyword;
use super::types::Operator;
use super::types::TokenType;
use super::types::KEYWORDS_MAP;
use core::iter::Peekable;
use core::str::Chars;
#[derive(Debug)]
pub struct Lexer<'a> {
    tokens: Vec<TokenType>,
    input: &'a str,
    offsset: usize, // offset char
    line: usize, // 1 based line
    column: usize, // 1 based col
}

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
            match ch {
                '+' => Self::eat_char_and_return(peekable, TokenType::Operator(Operator::PLUS)),
                '-' => Self::eat_char_and_return(peekable, TokenType::Operator(Operator::MINUS)),
                '*' => Self::eat_char_and_return(peekable, TokenType::Operator(Operator::MUL)),
                '/' => Self::eat_char_and_return(peekable, TokenType::Operator(Operator::DIV)),
                '(' => Self::eat_char_and_return(peekable, TokenType::LPAREN),
                ')' => Self::eat_char_and_return(peekable, TokenType::RPAREN),
                '\r' | ' ' => Self::eat_char_and_return(peekable, TokenType::WhiteSpace),
                '\n' => Self::eat_char_and_return(peekable, TokenType::NewLine),
                _ => {
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
                        return Ok(TokenType::String(str));
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
                            return Ok(TokenType::FLOAT(str));
                        }
                        return Ok(TokenType::INT(str));
                    }
                    Err(TokenizerError {
                        message: "unknown ch".to_string(),
                    })
                }
            }
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
    fn eat_char_and_return(
        peekable: &mut Peekable<Chars<'_>>,
        token: TokenType,
    ) -> Result<TokenType, TokenizerError> {
        peekable.next();
        Ok(token)
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
        TokenType::String("fnabc".to_string()),
        TokenType::NewLine,
        TokenType::INT("34".to_string()),
        TokenType::WhiteSpace,
        TokenType::FLOAT("3.145".to_string()),
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
