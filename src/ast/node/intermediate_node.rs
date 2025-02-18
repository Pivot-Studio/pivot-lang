use crate::ast::{ctx::Ctx, range::Range};
use internal_macro::node;

use crate::ast::builder::BuilderEnum;

use super::node_result::NodeResult;
use super::{Node, PrintTrait};

#[node]
pub struct IntermediateNode {
    value: NodeResult,
}

// this node will never be shared between threads
unsafe impl Send for IntermediateNode {}
unsafe impl Sync for IntermediateNode {}

impl IntermediateNode {
    pub fn new(value: NodeResult) -> Self {
        Self {
            value,
            range: Range::default(),
        }
    }
}

impl PrintTrait for IntermediateNode {
    fn print(&self, _tabs: usize, _end: bool, _line: Vec<bool>) {
        unreachable!()
    }
}

impl Node for IntermediateNode {
    fn emit<'a, 'b>(
        &mut self,
        _ctx: &'b mut Ctx<'a>,
        _builder: &'b BuilderEnum<'a, '_>,
    ) -> NodeResult {
        self.value.clone()
    }
}
