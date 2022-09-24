# Pivot-lang

[![codecov](https://codecov.io/gh/Pivot-Studio/pivot-lang/branch/master/graph/badge.svg?token=CA17PWK0EG)](https://codecov.io/gh/Pivot-Studio/pivot-lang) 
[![Build Status](https://drone.pivotstudio.cn/api/badges/Pivot-Studio/pivot-lang/status.svg)](https://drone.pivotstudio.cn/Pivot-Studio/pivot-lang)


![codecov](https://codecov.io/gh/Pivot-Studio/pivot-lang/branch/master/graphs/sunburst.svg?token=CA17PWK0EG)

## dependencies
- [llvm-14](https://github.com/llvm/llvm-project/releases/tag/llvmorg-14.0.6)
- [rust](https://www.rust-lang.org/)


## grammar

```ebnf
addexp = 
    | mulexp ("+" mulexp)*
    ;

mulexp = 
    | unaryexp ("*"｜"/" unaryexp)*
    ;

unaryexp =
    | primaryexp
    | "-" unaryexp
    ;

primaryexp =
    | number
    | boolconst
    | "(" logicexp ")"
    | identifier
    ;

boolconst =
    | "true"
    | "false"
    ;

compareexp =
    | addexp (("<"｜"<="｜">"｜">="｜"=="｜"!=") addexp)*
    ;

logicexp = 
    | compareexp (("&&"｜"||") compareexp)*
    ;

assignment = identifier "=" logicexp ;

newvariable = "let" identifier "=" logicexp ;

ifstatement = "if" logicexp "{" statement* "}" ;

whilestatement = "while" logicexp "{" statement* "}" ;

newline = "\n" | "\r\n" ;

statement = 
    | assignment newline
    | newvariable newline
    | ifstatement
    | whilestatement
    | newline
    ;

program = statement* EOF ;

number = [0-9]+ | number "." number ;

identifier = [a-zA-Z_][a-zA-Z0-9_]* ;

```
