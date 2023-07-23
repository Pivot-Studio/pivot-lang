# 递归下降分析法

这章介绍我们编译的第一个步骤：递归下降分析。



递归下降法是一种用于构造LL语法分析器的技巧。在这种方法中，对于语法的每个非终止符号，我们都有一个函数来处理。

```admonish info
终结符指的是语法里不能再写出表达式的符号，比如下方语法中的`+`、`-`、`num`等。非终结符就是可以继续写出表达式的符号，比如`expr`、`add`、`sub`等。
```


下面是一个基本的递归下降解析器的实现，我们将用它进行一个简单的算术表达式解析：

```rust
/// expr = add | sub | num
/// add = num '+' expr
/// sub = num '-' expr


#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Num(i64),
}

#[derive(Debug, Clone)]
pub enum Token {
    Plus,
    Minus,
    Num(i64),
    End,
    Err,
}

#[derive(Debug)]
pub struct ParseError;

fn next_token(input: &str) -> Result<(Token, &str), ParseError> {
    let input = input.trim_start();
    let (token, rest) = match input.chars().next() {
        Some('+') => (Token::Plus, &input[1..]),
        Some('-') => (Token::Minus, &input[1..]),
        Some(ch) if ch.is_digit(10) => {
            let digits: String = input.chars().take_while(|ch| ch.is_digit(10)).collect();
            let len = digits.len();
            (Token::Num(digits.parse().unwrap()), &input[len..])
        }
        Some(_) => return Err(ParseError),
        None => (Token::End, ""),
    };
    Ok((token, rest))
}

fn add(input: &str) -> Result<(Expr, &str), ParseError> {
    let (left, rest) = num(input)?;
    let (token, rest) = next_token(rest)?;
    if let Token::Plus = token {
        let (right, rest) = expr(rest)?;
        Ok((Expr::Add(Box::new(left), Box::new(right)), rest))
    } else {
        Err(ParseError)
    }
}

fn sub(input: &str) -> Result<(Expr, &str), ParseError> {
    let (left, rest) = num(input)?;
    let (token, rest) = next_token(rest)?;
    if let Token::Minus = token {
        let (right, rest) = expr(rest)?;
        Ok((Expr::Sub(Box::new(left), Box::new(right)), rest))
    } else {
        Err(ParseError)
    }
}
/// expr = add | sub | num
fn expr(input: &str) -> Result<(Expr, &str), ParseError> {
    add(input).or_else(|_| sub(input)).or_else(|_| num(input))
}

fn num(input: &str) -> Result<(Expr, &str), ParseError> {
    let (token, rest) = next_token(input)?;
    if let Token::Num(val) = token {
        Ok((Expr::Num(val), rest))
    } else {
        Err(ParseError)
    }
}

fn main() {
    let input = "1 + 2 - 3";
    let (expr, rest) = expr(input).unwrap();
    assert_eq!(rest, "");
    assert_eq!(
        expr,
        Expr::Add(
            Box::new(Expr::Num(1)),
            Box::new(Expr::Sub(Box::new(Expr::Num(2)), Box::new(Expr::Num(3))))
        )
    );
}

```

针对这段代码，我们有四条语法规则，每条规则对应一个分析函数。这些函数的返回值是一个enum，在正确的情况下返回的是生成的语法树
以及剩余的字符串。如果解析失败，返回一个错误。

仔细观察可以看出，递归下降法和语法规则是直观对应的，例如规则`expr = add | sub | num`，expr函数的逻辑就是
顺序调用add、sub、num函数，如果其中一个成功，就立刻返回成功的结果，否则返回错误。

所以只要能明确写出对应的语法，将语法转换为递归下降分析的代码就是一件非常简单的事情了。你可以试试将上面的代码修改一下，增加对乘法和除法的支持，
并且思考递归下降中如何处理优先级的问题？




