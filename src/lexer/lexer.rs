use core::fmt;
use std::collections::HashMap;

use lazy_static::lazy_static;

use super::pos::Pos;
use super::pos::Range;
use super::types::Operator;
use super::types::Token;
use super::types::TokenType;
use super::types::KEYWORDS_MAP;
use core::iter::Peekable;
use core::str::Chars;
#[derive(Debug)]
pub struct Lexer<'a> {
    tokens: Vec<Token>,
    input: &'a str,
    offsset: usize, // offset char 0 based
    line: usize,    // 1 based line
    column: usize,  // 1 based col
}

lazy_static! {
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
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut peekable = input.chars().peekable();
        let mut tokens = vec![];
        let t = vec![];
        let mut lexer = Lexer {
            input,
            tokens: t,
            offsset: 0,
            line: 1,
            column: 1,
        };
        while let Ok(token) = lexer.next_token(&mut peekable) {
            let tp = token.token_type;
            tokens.push(token);
            if tp == TokenType::EOF {
                break;
            }
        }

        return Lexer {
            input,
            tokens,
            offsset: 0,
            line: 1,
            column: 1,
        };
    }

    pub fn eat_token_skip_whitespace(
        &mut self,
        expect_type: TokenType,
    ) -> Result<Token, TokenizerError> {
        self.skip_whitespace();
        return self.eat_token(expect_type);
    }

    pub fn peek_skip_whitespace(&mut self) -> Result<Token, TokenizerError> {
        let pos = self.curr_pos();
        self.skip_whitespace();
        let re = self.peek();
        self.go_back(pos);
        return re;
    }

    pub fn peek(&mut self) -> Result<Token, TokenizerError> {
        if self.tokens.len() > self.offsset {
            let token = self.tokens[self.offsset].clone();
            return Ok(token);
        } else {
            return Err(TokenizerError {
                message: "expect token but EOF".to_string(),
            });
        }
    }

    fn skip_whitespace(&mut self) {
        let mut re = self.eat_token(TokenType::WhiteSpace);
        while re.is_ok() {
            re = self.eat_token(TokenType::WhiteSpace);
        }
    }

    pub fn eat_token(&mut self, expect_type: TokenType) -> Result<Token, TokenizerError> {
        if self.tokens.len() > self.offsset {
            if self.tokens[self.offsset].token_type == expect_type {
                let token = self.tokens[self.offsset].clone();
                self.offsset += 1;
                return Ok(token);
            } else {
                return Err(TokenizerError {
                    message: "wrong token type".to_string(),
                });
            }
        } else {
            return Err(TokenizerError {
                message: "expect token but EOF".to_string(),
            });
        }
    }

    pub fn curr_pos(&self) -> Pos {
        return Pos {
            line: self.line,
            column: self.column,
            offset: self.offsset,
        };
    }

    pub fn go_back(&mut self, pos: Pos) {
        self.column = pos.column;
        self.line = pos.line;
        self.offsset = pos.offset;
    }

    fn build_token(&self, token_type: TokenType, start: Pos, value: String) -> Token {
        let range = self.gen_range(start);
        return Token {
            token_type,
            value,
            range,
        };
    }

    fn eat(&mut self, peekable: &mut Peekable<Chars<'_>>) {
        let ch = peekable.next().unwrap();

        self.offsset += 1;
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }

    fn gen_range(&self, start: Pos) -> Range {
        let end = self.curr_pos();
        return Range { start, end };
    }

    fn next_token(&mut self, peekable: &mut Peekable<Chars<'_>>) -> Result<Token, TokenizerError> {
        if let Some(&ch) = peekable.peek() {
            let start = self.curr_pos();
            let tp = TOKEN_MAP.get(&ch);
            if let Some(tp) = tp {
                self.eat(peekable);
                let v = ch.to_string();
                let token = self.build_token(*tp, start, v);
                return Ok(token);
            }
            if Self::is_letter_or_underscore(ch) {
                let mut str = String::new();
                str.push(ch);
                self.eat(peekable);
                while let Some(&ch) = peekable.peek() {
                    if !Self::is_letter_or_underscore(ch) && !Self::is_num(ch) {
                        break;
                    }
                    str.push(ch);
                    self.eat(peekable);
                    continue;
                }
                if let Some(&keyword) = KEYWORDS_MAP.get(&str.as_str()) {
                    let tp = TokenType::Keyword(keyword);
                    let token = self.build_token(tp, start, str);
                    return Ok(token);
                }
                let token = self.build_token(TokenType::String, start, str);
                return Ok(token);
            }
            if Self::is_num(ch) {
                let mut str = String::new();
                str.push(ch);
                self.eat(peekable);
                let mut is_float = false;
                while let Some(&ch) = peekable.peek() {
                    if ch == '.' {
                        is_float = true;
                        str.push(ch);
                        self.eat(peekable);
                        continue;
                    }
                    if !Self::is_num(ch) {
                        break;
                    }
                    str.push(ch);
                    self.eat(peekable);
                    continue;
                }
                if is_float {
                    let token = self.build_token(TokenType::FLOAT, start, str);
                    return Ok(token);
                }
                let token = self.build_token(TokenType::INT, start, str);
                return Ok(token);
            }
            Err(TokenizerError {
                message: "unknown ch".to_string(),
            })
        } else {
            let start = self.curr_pos();
            Ok(self.build_token(TokenType::EOF, start, "".to_string()))
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

#[derive(Debug, PartialEq, Eq)]
pub struct TokenizerError {
    pub message: String,
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}
