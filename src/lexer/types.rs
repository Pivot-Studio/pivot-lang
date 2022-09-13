#[derive(Debug, PartialEq)]
pub enum Token {
    Operator(Operator), // 运算符
    LPAREN,             // (
    RPAREN,             // )
    EOF,                //EOF
}
#[derive(Debug, PartialEq)]
pub enum Operator {
    PLUS,  // +
    MINUS, // -
    MUL,   // *
    DIV,   // /
}
