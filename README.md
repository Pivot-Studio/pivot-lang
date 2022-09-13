# Pivot-lang


## dependencies
- [llvm-14](https://github.com/llvm/llvm-project/releases/tag/llvmorg-14.0.6)
- [rust](https://www.rust-lang.org/)


## grammar

```ebnf
addexp = 
    | mulexp
    | addexp "+" mulexp
    | addexp "-" mulexp
    ;

mulexp = 
    | unaryexp
    | mulexp "*" unaryexp
    | mulexp "/" unaryexp
    ;

unaryexp =
    | primaryexp
    | "-" unaryexp
    ;

primaryexp =
    | number
    | "(" addexp ")"
    ;

number = [0-9]+ ;

```
