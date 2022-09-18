use crate::ast::Node;
use crate::lexer::pos::Pos;

use super::lexer::lexer::Lexer;
use super::lexer::types::TokenType;

pub mod math;

#[derive(Debug, Clone)]
pub struct ParseError {
    pos: Pos,
    msg: String,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let lexer = Lexer::new(input);
        Parser { lexer }
    }

    pub fn parse(&mut self) -> Result<Box<dyn Node>, ParseError> {
        return self.addexp();
    }
}
