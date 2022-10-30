use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{
    ast::node::*,
    ast::node::{
            function::FuncDefNode,
            global::GlobalNode,
            types::StructDefNode
        },
    ast::node::{
            control::*, operator::*, primary::*, program::*, statement::*
    },
    Db,
};

use self::{
    error::{alt_except, except}, 
    comment::*, pkg::*, control::*, statement::*, program::*, function::*, array::*, 
    expression::*, constval::*, helper::*, types::*, structure::*, identifier::*
};

pub mod error;
#[macro_use]
pub mod macros;
pub mod helper;
pub mod comment;
pub mod pkg;
pub mod control;
pub mod statement;
pub mod program;
pub mod function;
pub mod array;
pub mod expression;
pub mod constval;
pub mod types;
pub mod structure;
pub mod identifier;

pub enum TopLevel {
    StructDef(StructDefNode),
    FuncDef(FuncDefNode),
    GlobalDef(GlobalNode),
    Common(Box<NodeEnum>),
    Use(Box<NodeEnum>),
}

#[derive(Clone)]
pub enum ComplexOp {
    CallOp(Vec<Box<NodeEnum>>),
    IndexOp(Box<NodeEnum>),
    FieldOp(Option<Box<VarNode>>),
}

#[salsa::tracked]
pub struct SourceProgram {
    #[return_ref]
    pub text: String,
}

// ANCHOR: parse
#[salsa::tracked(lru = 32)]
pub fn parse(db: &dyn Db, source: SourceProgram) -> Result<ProgramNodeWrapper, String> {
    let text = source.text(db);
    let re = program(Span::new(text));
    if let Err(e) = re {
        return Err(format!("{:?}", e));
    }
    // eprintln!("parse");
    Ok(ProgramNodeWrapper::new(db, re.unwrap().1))
}
// ANCHOR_END: parse

