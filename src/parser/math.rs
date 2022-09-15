use super::{Parser, ParseError};
use crate::{lexer::types::{Token, TokenType}, ast::{NumNode, Num}};

impl Parser<'_> {
    fn number(&mut self) -> Result<NumNode, ParseError> {
        let re = self.lexer
            .eat_token_skip_whitespace(TokenType::INT);
        if re.is_ok() {
            let token = re.unwrap();
            let value = Num::INT(token.value.parse::<i64>().unwrap());
            return Ok(NumNode { value });
        } else {
            return Err(ParseError {
                pos: self.lexer.curr_pos(),
                msg: "expect number".to_string(),
            });
        }
    }
}
