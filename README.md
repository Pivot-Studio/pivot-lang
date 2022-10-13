# Pivot-lang

[![codecov](https://codecov.io/gh/Pivot-Studio/pivot-lang/branch/master/graph/badge.svg?token=CA17PWK0EG)](https://codecov.io/gh/Pivot-Studio/pivot-lang) 
[![Build Status](https://drone.pivotstudio.cn/api/badges/Pivot-Studio/pivot-lang/status.svg)](https://drone.pivotstudio.cn/Pivot-Studio/pivot-lang)


![codecov](https://codecov.io/gh/Pivot-Studio/pivot-lang/branch/master/graphs/sunburst.svg?token=CA17PWK0EG)

此项目目前处于早期开发阶段，不建议用于生产环境。  

## dependencies
- [llvm-14](https://github.com/llvm/llvm-project/releases/tag/llvmorg-14.0.6)
- [rust](https://www.rust-lang.org/)

## 特点
- 同时支持aot和jit两种模式
- 极其方便的rust互操作

## 近期开发计划
- [ ] vsc debug
  - [x] 断点
  - [x] 变量表
    - [x] 函数参数
    - [x] 普通变量
- [ ] 代码高亮
- [ ] lsp支持
  - [ ] 错误容忍
    - [x] parser错误容忍
    - [ ] ast错误容忍

## 项目结构

- [vm](vm) 包含rumtime
- [src](src) 编译器源码所在
- [internal_macro](internal_macro) 内部过程宏

## grammar

```ebnf
add_exp = 
    | mul_exp ("+" | "-" mul_exp)*
    ;

mul_exp = 
    | unary_exp ("*"｜"/" unary_exp)*
    ;

unary_exp =
    | take_exp
    | ("-" | "!" | "&") take_exp
    ;

take_exp =
    | primary_exp ("." identifier)*
    ;

primary_exp =
    | number
    | bool_const
    | "(" logic_exp ")"
    | identifier
    | struct_init
    | call_function
    ;


bool_const =
    | "true"
    | "false"
    ;

compare_exp =
    | add_exp (("<=" | "<"｜">="｜">"｜"=="｜"!=") add_exp)*
    ;

logic_exp = 
    | compare_exp (("&&"｜"||") compare_exp)*
    ;

assignee = identifier ("." identifier)*;

assignment = assignee "=" logic_exp ;

new_variable = "let" identifier "=" logic_exp ;

if_statement = "if" logic_exp statement_block ("else" if_statement | statement_block)?;

while_statement = "while" logic_exp statement_block ;

for_statement = "for" (assignment | new_variable) ";" logic_exp ";" assignment statement_block;

statement_block = "{" statements "}" ;

newline = "\n" | "\r\n" ;

statements = statement* ;

break_statement = "break" newline ;

continue_statement = "continue" newline ;

statement = 
    | assignment newline
    | new_variable newline
    | return_statement
    | if_statement
    | while_statement
    | break_statement
    | continue_statement
    | function_call
    | newline
    ;

toplevel_statement = 
    | struct_def
    | function_def
    | newline
    ;

program = toplevel_statement* ;


number = [0-9]+ | number "." number ;

identifier = [a-zA-Z_][a-zA-Z0-9_]* ;

function_def = "fn" identifier "(" (typed_identifier (","typed_identifier)*)? ")" type_name (statement_block | newline) ;

call_function = identifier "(" (logic_exp (","logic_exp)*)? ")" ;

struct_def = "struct" identifier "{" struct_field* "}" ;

type_name = ("&")? identifier ;

typed_identifier = identifier ":" type_name ;

struct_field = typed_identifier newline ;

struct_init = type_name "{" struct_init_field "}" ;

struct_init_field = identifier ":" logic_exp "," ;

return_statement = "return" logic_exp newline ;

```
