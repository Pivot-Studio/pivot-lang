use crate::lexer::pos::Pos;

use super::lexer::lexer::{Lexer};
use super::lexer::types::{Token, TokenType};

pub mod math;

pub struct  ParseError {
    pos: Pos,
    msg: String,
}

struct Parser<'a> {
    lexer: Lexer<'a>,
}


impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let lexer = Lexer::new(input);
        Parser { lexer }
    }
}

