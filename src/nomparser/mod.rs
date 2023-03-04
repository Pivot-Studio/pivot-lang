use crate::{
    ast::node::*,
    ast::node::{
        function::FuncDefNode,
        global::GlobalNode,
        interface::TraitDefNode,
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
use nom_locate::LocatedSpan;
/// extra为布尔类型，代表是否跳过struct init等可能造成if语句条件二义性的parser
/// 由于我们的if不需要括号，所以如果允许这些表达式将会出现以下情况：
/// ```pl
/// if A{}
/// ```
/// 这个时候如果不特殊处理，将无法知道`A{}`整体是个struct init，还是`A`是个变量
pub type Span<'a> = LocatedSpan<&'a str, bool>;

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
pub mod macro_parse;
pub mod pkg;
pub mod program;
pub mod statement;
pub mod string_literal;
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
    TraitDef(TraitDefNode),
}

#[derive(Clone)]
pub enum ComplexOp {
    CallOp((Vec<Box<NodeEnum>>, Range, Option<Box<GenericParamNode>>)),
    IndexOp(Box<NodeEnum>),
    FieldOp(Option<Box<VarNode>>),
}

#[salsa::input]
pub struct SourceProgram {
    #[return_ref]
    pub text: String,
    #[return_ref]
    pub path: String,
}
macro_rules! nothing {
    ($($fmt:ident,+1dasda,)* / $a:expr) => {};
}

// ANCHOR: parse
#[salsa::tracked]
pub fn parse(db: &dyn Db, source: SourceProgram) -> Result<ProgramNodeWrapper, String> {
    let text = source.text(db);
    let re = program(Span::new_extra(text, false));
    if let Err(e) = re {
        return Err(format!("{:?}", e));
    }
    nothing!(re , + 1dasda , re,+ 1dasda, / re);
    let (_, node) = re.unwrap();
    log::info!("parse {:?}", source.path(db));

    Ok(ProgramNodeWrapper::new(db, node))
}
// ANCHOR_END: parse
