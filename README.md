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
    | mulexp
    | mulexp "+" addexp
    | mulexp "-" addexp
    ;

mulexp = 
    | unaryexp
    | unaryexp "*" mulexp
    | unaryexp "/" mulexp
    ;

unaryexp =
    | primaryexp
    | "-" unaryexp
    ;

primaryexp =
    | float
    | number
    | "(" addexp ")"
    | identifier
    ;

float = number "." number ;

number = [0-9]+ ;

identifier = [a-zA-Z_][a-zA-Z0-9_]* ;

```
