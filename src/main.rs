mod ast;
mod lexer;
mod parser;
mod utils;

fn main() {
    let lexer = lexer::lexer::Lexer::new("+-*/");
    let mut parser = parser::Parser::new("-1+-2--4*(5+1)");
    let n = parser.parse();
    if let Err(e) = n {
        println!("{:?}", e);
    } else {
        n.unwrap().print();
    }
    println!("{:?}", lexer);
}
