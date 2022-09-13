mod lexer;

fn main() {
    let lexer = lexer::lexer::Lexer::new("+-*/");
    println!("{:?}", lexer);
}
