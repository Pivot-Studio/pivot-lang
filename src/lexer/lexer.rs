use core::fmt;

use super::types::Operator;
use super::types::Token;
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
            if let Token::EOF = token {
                tokens.push(token);
                break;
            }
            tokens.push(token);
            peekable.next();
        }
        Self { input, tokens }
    }
    fn next_token(peekable: &mut Peekable<Chars<'_>>) -> Result<Token, TokenizerError> {
        if let Some(&ch) = peekable.peek() {
            match ch {
                '+' => Ok(Token::Operator(Operator::PLUS)),
                '-' => Ok(Token::Operator(Operator::MINUS)),
                '*' => Ok(Token::Operator(Operator::MUL)),
                '/' => Ok(Token::Operator(Operator::DIV)),
                '(' => Ok(Token::LPAREN),
                ')' => Ok(Token::RPAREN),
                _ => Err(TokenizerError {
                    message: "unknown token".to_string(),
                }),
            }
        } else {
            Ok(Token::EOF)
        }
    }
}
impl fmt::Display for Lexer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {:?}", self.input, self.tokens)
    }
}
#[test]
fn test_token_vec_gen() {
    let lexer = Lexer::new("+-*/");
    let tokens = lexer.tokens;
    let expected = vec![
        Token::Operator(Operator::PLUS),
        Token::Operator(Operator::MINUS),
        Token::Operator(Operator::MUL),
        Token::Operator(Operator::DIV),
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
