use nom::{IResult, character::complete::space0, number::{self, complete::{double, i64, be_i64}}, combinator::{opt, recognize}, branch::alt};

use crate::{ast::{Node, NumNode}, lexer::pos::{Range, Pos}};

pub struct Parser<'a> {
    input: &'a [u8],
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a [u8]) -> Self {
        Parser { input }
    }

    pub fn number(input : & [u8]) ->IResult<& [u8],Box<dyn Node>> {
        space0(input)?;
        let (re,node) = alt((
            parse_double,
            parse_int,
        ))(input)?;
        Ok((re, Box::new(node)))
    }
    
}

fn parse_double(s: &[u8])-> IResult<&[u8], NumNode>{
    let re = double(s)?;
    let p = Pos{column:0,line:0,offset:0};
    return Ok((re.0, NumNode{value: crate::ast::Num::FLOAT(re.1), range: Range{start: p,end: p}}));
}

fn parse_int(s: &[u8])-> IResult<&[u8], NumNode>{
    let re = be_i64(s)?;
    let p = Pos{column:0,line:0,offset:0};
    return Ok((re.0, NumNode{value: crate::ast::Num::INT(re.1), range: Range{start: p,end: p}}));
}
