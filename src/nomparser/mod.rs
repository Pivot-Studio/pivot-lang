use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use crate::{
    ast::node::*,
    ast::node::{
        function::FuncDefNode,
        global::GlobalNode,
        types::{GenericParamNode, StructDefNode},
    },
    ast::{
        node::{
            control::*, implement::ImplNode, operator::*, primary::*, program::*, statement::*,
        },
        range::Range,
    },
    Db,
};

use self::{
    array::*,
    comment::*,
    constval::*,
    control::*,
    error::{alt_except, except},
    expression::*,
    function::*,
    helper::*,
    identifier::*,
    pkg::*,
    program::*,
    statement::*,
    structure::*,
    types::*,
};

pub mod error;
#[macro_use]
pub mod macros;
pub mod array;
pub mod comment;
pub mod constval;
pub mod control;
pub mod expression;
pub mod function;
pub mod helper;
pub mod identifier;
pub mod implement;
pub mod pkg;
pub mod program;
pub mod statement;
pub mod structure;
pub mod types;

#[derive(Debug, Clone)]
pub enum TopLevel {
    StructDef(StructDefNode),
    FuncType(FuncDefNode),
    GlobalDef(GlobalNode),
    Common(Box<NodeEnum>),
    Use(Box<NodeEnum>),
    ImplDef(ImplNode),
}

#[derive(Clone)]
pub enum ComplexOp {
    CallOp((Vec<Box<NodeEnum>>, Range, Option<Box<GenericParamNode>>)),
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
