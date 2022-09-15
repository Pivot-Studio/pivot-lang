#[derive(Debug, PartialEq, Clone, Copy)]
pub struct NumNode {
    pub value: Num,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Num {
    INT(i64),
    FLOAT(f64),
}