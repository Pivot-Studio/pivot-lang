use crate::lexer::types::Operator;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct NumNode {
    pub value: Num,
}

pub struct UnaryOpNode {
    pub op: Operator,
    pub exp: Box<dyn Node>,
}

pub struct BinOpNode {
    pub left: Box<dyn Node>,
    pub op: Operator,
    pub right: Box<dyn Node>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Num {
    INT(i64),
    FLOAT(f64),
}

pub trait Node {
    fn print(&self);
}

impl Node for NumNode {
    fn print(&self) {
        println!("{:?}", self.value)
    }
}

impl Node for BinOpNode {
    fn print(&self) {
        println!("BinOpNode");
        self.left.print();
        println!("{:?}", self.op);
        self.right.print();
    }
}

impl Node for UnaryOpNode {
    fn print(&self) {
        println!("UnaryOpNode");
        println!("{:?}", self.op);
        self.exp.print();
    }
}
