use core::fmt;

use super::types::Keyword;
use super::types::Operator;
use super::types::Token;
use super::types::KEYWORDS_MAP;
use core::iter::Peekable;
use core::str::Chars;
#[derive(Debug)]
pub struct Lexer<'a> {
    tokens: Vec<Token>,
    input: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut peekable = input.chars().peekable();
        let mut tokens = vec![];
        while let Ok(token) = Self::next_token(&mut peekable) {
            if token == Token::EOF {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        Self { input, tokens }
    }
    fn next_token(peekable: &mut Peekable<Chars<'_>>) -> Result<Token, TokenizerError> {
        if let Some(&ch) = peekable.peek() {
            match ch {
                '+' => Self::eat_char_and_return(peekable, Token::Operator(Operator::PLUS)),
                '-' => Self::eat_char_and_return(peekable, Token::Operator(Operator::MINUS)),
                '*' => Self::eat_char_and_return(peekable, Token::Operator(Operator::MUL)),
                '/' => Self::eat_char_and_return(peekable, Token::Operator(Operator::DIV)),
                '(' => Self::eat_char_and_return(peekable, Token::LPAREN),
                ')' => Self::eat_char_and_return(peekable, Token::RPAREN),
                '\n' | '\r' | ' ' => Self::eat_char_and_return(peekable, Token::WhiteSpace),
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
                            return Ok(Token::Keyword(keyword));
                        }
                        return Ok(Token::String(str));
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
                            return Ok(Token::FLOAT(str));
                        }
                        return Ok(Token::INT(str));
                    }
                    Err(TokenizerError {
                        message: "unknown ch".to_string(),
                    })
                }
            }
        } else {
            Ok(Token::EOF)
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
        token: Token,
    ) -> Result<Token, TokenizerError> {
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
        Token::Operator(Operator::PLUS),
        Token::Operator(Operator::MINUS),
        Token::Operator(Operator::MUL),
        Token::WhiteSpace,
        Token::Operator(Operator::DIV),
        Token::WhiteSpace,
        Token::Keyword(Keyword::FN),
        Token::WhiteSpace,
        Token::String("fnabc".to_string()),
        Token::WhiteSpace,
        Token::INT("34".to_string()),
        Token::WhiteSpace,
        Token::FLOAT("3.145".to_string()),
        Token::EOF,
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
