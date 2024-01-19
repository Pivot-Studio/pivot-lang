use crate::ast::diag::PLDiag;
use crate::ast::{builder::ValueHandle, pltype::PLType};
use std::cell::RefCell;
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TerminatorEnum {
    #[default]
    None,
    Return,
    Break,
    Continue,
    YieldReturn,
}

impl TerminatorEnum {
    pub fn is_none(self) -> bool {
        self == TerminatorEnum::None || self == TerminatorEnum::YieldReturn
    }
    pub fn is_return(self) -> bool {
        self == TerminatorEnum::Return || self == TerminatorEnum::YieldReturn
    }
}

#[derive(Default, Clone)]
pub struct NodeOutput {
    value: Option<NodeValue>,
    term: TerminatorEnum,
}

pub type NodeResult = Result<NodeOutput, PLDiag>;

pub trait NodeResultBuilder {
    fn new_output(&self, ty: Arc<RefCell<PLType>>) -> NodeOutput;
}

impl NodeResultBuilder for ValueHandle {
    fn new_output(&self, typ: Arc<RefCell<PLType>>) -> NodeOutput {
        NodeOutput::new_value_simple(*self, typ)
    }
}

impl NodeOutput {
    pub fn set_const(&mut self) -> &mut Self {
        if let Some(value) = &mut self.value {
            value.set_const(true);
        }
        self
    }

    pub fn set_const_v(&mut self, v: bool) -> &mut Self {
        if let Some(value) = &mut self.value {
            value.set_const(v);
        }
        self
    }

    pub fn with_term(&mut self, term: TerminatorEnum) -> &mut Self {
        self.term = term;
        self
    }

    pub fn with_receiver(
        &mut self,
        receiver: ValueHandle,
        ty: Option<Arc<RefCell<PLType>>>,
    ) -> &mut Self {
        if let Some(value) = &mut self.value {
            value.set_receiver(receiver, ty);
        }
        self
    }

    pub fn to_result(&self) -> NodeResult {
        Ok(self.clone())
    }

    pub fn new(value: Option<NodeValue>, term: TerminatorEnum) -> Self {
        Self { value, term }
    }

    pub fn new_value(value: NodeValue) -> Self {
        Self {
            value: Some(value),
            term: TerminatorEnum::None,
        }
    }

    /// # new_value_simple
    ///
    /// constructs a new [NodeOutput] with None [TerminatorEnum]
    pub fn new_value_simple(value: ValueHandle, ty: Arc<RefCell<PLType>>) -> Self {
        Self {
            value: Some(NodeValue::new(value, ty)),
            term: TerminatorEnum::None,
        }
    }

    pub fn new_term(term: TerminatorEnum) -> Self {
        Self { value: None, term }
    }

    pub fn get_value(&self) -> Option<NodeValue> {
        self.value.clone()
    }

    pub fn get_term(&self) -> TerminatorEnum {
        self.term
    }
}

/// NodeValue is the output of the builder
#[derive(Debug, Clone)]
pub struct NodeValue {
    /// the index which maps the real value stored inside the LLVMBuilder
    value: ValueHandle,
    is_const: bool,
    /// the type of the node expression
    ty: Arc<RefCell<PLType>>,
    /// method receiver if any
    receiver: Option<(ValueHandle, Option<Arc<RefCell<PLType>>>)>,
}

impl Default for NodeValue {
    fn default() -> Self {
        Self {
            value: ValueHandle::default(),
            ty: Arc::new(RefCell::new(PLType::Unknown)),
            is_const: false,
            receiver: None,
        }
    }
}

impl NodeValue {
    pub fn new(value: ValueHandle, ty: Arc<RefCell<PLType>>) -> Self {
        Self {
            value,
            ty,
            is_const: false,
            receiver: None,
        }
    }

    pub fn new_const(value: ValueHandle, ty: Arc<RefCell<PLType>>) -> Self {
        Self {
            value,
            ty,
            is_const: true,
            receiver: None,
        }
    }

    pub fn new_receiver(
        value: ValueHandle,
        ty: Arc<RefCell<PLType>>,
        receiver: ValueHandle,
        receiver_ty: Option<Arc<RefCell<PLType>>>,
    ) -> Self {
        Self {
            value,
            ty,
            is_const: false,
            receiver: Some((receiver, receiver_ty)),
        }
    }

    pub fn get_value(&self) -> ValueHandle {
        self.value
    }

    pub fn get_ty(&self) -> Arc<RefCell<PLType>> {
        self.ty.clone()
    }

    pub fn get_receiver(&self) -> Option<(ValueHandle, Option<Arc<RefCell<PLType>>>)> {
        self.receiver.clone()
    }

    pub fn is_const(&self) -> bool {
        self.is_const
    }

    pub fn set_const(&mut self, v: bool) {
        self.is_const = v;
    }

    pub fn set_receiver(
        &mut self,
        receiver: ValueHandle,
        receiver_ty: Option<Arc<RefCell<PLType>>>,
    ) {
        self.receiver = Some((receiver, receiver_ty));
    }
}
