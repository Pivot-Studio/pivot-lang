# Pivot-lang

[![codecov](https://codecov.io/gh/Pivot-Studio/pivot-lang/branch/master/graph/badge.svg?token=CA17PWK0EG)](https://codecov.io/gh/Pivot-Studio/pivot-lang) 
[![Build Status](https://drone.pivotstudio.cn/api/badges/Pivot-Studio/pivot-lang/status.svg)](https://drone.pivotstudio.cn/Pivot-Studio/pivot-lang)


![codecov](https://codecov.io/gh/Pivot-Studio/pivot-lang/branch/master/graphs/sunburst.svg?token=CA17PWK0EG)

## dependencies
- [llvm-14](https://github.com/llvm/llvm-project/releases/tag/llvmorg-14.0.6)
- [rust](https://www.rust-lang.org/)


## grammar

```ebnf
add_exp = 
    | mul_exp ("+" | "-" mul_exp)*
    ;

mul_exp = 
    | unary_exp ("*"｜"/" unary_exp)*
    ;

unary_exp =
    | primary_exp
    | ("-" | "!") primary_exp
    ;

primary_exp =
    | number
    | bool_const
    | "(" logic_exp ")"
    | identifier
    ;

bool_const =
    | "true"
    | "false"
    ;

compare_exp =
    | add_exp (("<"｜"<="｜">"｜">="｜"=="｜"!=") add_exp)*
    ;

logic_exp = 
    | compare_exp (("&&"｜"||") compare_exp)*
    ;

assignment = identifier "=" logic_exp ;

new_variable = "let" identifier "=" logic_exp ;

if_statement = "if" logic_exp statement_block ("else" if_statement | statement_block)?;

while_statement = "while" logic_exp statement_block ;

statement_block = "{" statements "}" ;

newline = "\n" | "\r\n" ;

statements = statement* ;

statement = 
    | assignment newline
    | new_variable newline
    | if_statement
    | while_statement
    | newline
    ;

program = statements ;

number = [0-9]+ | number "." number ;

identifier = [a-zA-Z_][a-zA-Z0-9_]* ;

```
