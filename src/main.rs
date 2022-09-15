mod lexer;
mod parser;
mod ast;

fn main() {
    let lexer = lexer::lexer::Lexer::new("+-*/");
    println!("{:?}", lexer);
}
