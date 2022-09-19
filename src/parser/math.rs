use nom::bytes::complete::take;

use super::{ParseError, Parser};
use crate::parser::TokenType::Operator;
use crate::{
    ast::{BinOpNode, Node, Num, NumNode, UnaryOpNode},
    lexer::types::TokenType,
};

impl Parser<'_> {
    pub fn number(&mut self) -> Result<Box<dyn Node>, ParseError> {
        let re = self.lexer.eat_token_skip_whitespace(TokenType::INT);
        if re.is_ok() {
            let token = re.unwrap();
            let value = Num::INT(token.value.parse::<i64>().unwrap());
            let tr = NumNode {
                value,
                range: token.range,
            };
            return Ok(Box::new(tr));
        }
        return Err(ParseError {
            pos: self.lexer.curr_pos(),
            msg: "expect number".to_string(),
        });
    }

    pub fn primayexp(&mut self) -> Result<Box<dyn Node>, ParseError> {
        let re = self.number();
        if re.is_ok() {
            return re;
        }
        let check = self.lexer.curr_pos();

        let re = self.lexer.eat_token_skip_whitespace(TokenType::LPAREN);
        if let Err(e) = re {
            return Err(ParseError {
                pos: self.lexer.curr_pos(),
                msg: e.message,
            });
        }
        let exp = self.addexp();
        if let Err(e) = re {
            self.lexer.go_back(check);
            return Err(ParseError {
                pos: self.lexer.curr_pos(),
                msg: e.message,
            });
        }
        let re = self.lexer.eat_token_skip_whitespace(TokenType::RPAREN);
        if let Err(e) = re {
            self.lexer.go_back(check);
            return Err(ParseError {
                pos: self.lexer.curr_pos(),
                msg: e.message,
            });
        }
        return exp;
    }
    pub fn addexp(&mut self) -> Result<Box<dyn Node>, ParseError> {
        let check = self.lexer.curr_pos();
        let re = self.mulexp();
        if re.is_ok() {
            let op = self.lexer.peek_skip_whitespace();
            if op.is_err() {
                return re;
            }
            let opv = op.unwrap();
            if opv.token_type == TokenType::Operator(crate::lexer::types::Operator::PLUS)
                || opv.token_type == TokenType::Operator(crate::lexer::types::Operator::MINUS)
            {
                self.lexer.eat_token_skip_whitespace(opv.token_type);
                let right = self.addexp();
                if right.is_err() {
                    self.lexer.go_back(check);
                    return Err(ParseError {
                        pos: self.lexer.curr_pos(),
                        msg: right.err().unwrap().msg,
                    });
                }
                let left = re.unwrap();
                let right = right.unwrap();
                let range = left.range().start.to(right.range().end);
                match opv.token_type {
                    Operator(x) => {
                        let tr = BinOpNode {
                            left,
                            op: x,
                            right,
                            range,
                        };
                        return Ok(Box::new(tr));
                    }
                    _ => (),
                }
            } else {
                return re;
            }
        }
        return Err(ParseError {
            pos: self.lexer.curr_pos(),
            msg: "expect number".to_string(),
        });
    }
    pub fn mulexp(&mut self) -> Result<Box<dyn Node>, ParseError> {
        let check = self.lexer.curr_pos();
        let re = self.unaryexp();
        if re.is_ok() {
            let op = self.lexer.peek_skip_whitespace();
            if op.is_err() {
                return re;
            }
            let opv = op.unwrap();
            if opv.token_type == TokenType::Operator(crate::lexer::types::Operator::MUL)
                || opv.token_type == TokenType::Operator(crate::lexer::types::Operator::DIV)
            {
                self.lexer.eat_token_skip_whitespace(opv.token_type);
                let right = self.mulexp();
                if right.is_err() {
                    self.lexer.go_back(check);
                    return Err(ParseError {
                        pos: self.lexer.curr_pos(),
                        msg: right.err().unwrap().msg,
                    });
                }
                let left = re.unwrap();
                let right = right.unwrap();
                match opv.token_type {
                    Operator(x) => {
                        let range = left.range().start.to(right.range().end);
                        let tr = BinOpNode {
                            left,
                            op: x,
                            right,
                            range,
                        };
                        return Ok(Box::new(tr));
                    }
                    _ => (),
                }
            } else {
                return re;
            }
        }
        return Err(ParseError {
            pos: self.lexer.curr_pos(),
            msg: "expect number".to_string(),
        });
    }
    pub fn unaryexp(&mut self) -> Result<Box<dyn Node>, ParseError> {
        let re = self.primayexp();
        if re.is_ok() {
            return re;
        }
        let check = self.lexer.curr_pos();

        let re = self
            .lexer
            .eat_token_skip_whitespace(TokenType::Operator(crate::lexer::types::Operator::MINUS));
        if let Err(e) = re {
            return Err(ParseError {
                pos: self.lexer.curr_pos(),
                msg: e.message,
            });
        }
        let exp = self.unaryexp();
        if let Err(e) = re {
            self.lexer.go_back(check);
            return Err(ParseError {
                pos: self.lexer.curr_pos(),
                msg: e.message,
            });
        }
        let e = exp.unwrap();
        let range = re.unwrap().range.start.to(e.range().end);
        return Ok(Box::new(UnaryOpNode {
            op: crate::lexer::types::Operator::MINUS,
            exp: e,
            range,
        }));
    }
}
