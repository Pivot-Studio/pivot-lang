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
- [x] vsc debug
  - [x] 断点
  - [x] 变量表
    - [x] 函数参数
    - [x] 普通变量
- [x] 代码高亮
- [x] lsp支持
  - [x] 错误容忍
    - [x] parser错误容忍
    - [x] ast错误容忍
  - [ ] 代码提示
    - [x] 普通变量
    - [x] 函数参数
    - [x] 函数
    - [x] 类型
    - [ ] 模块
    - [ ] 语法
  - [ ] 代码跳转
    - [x] 普通变量
    - [x] 函数参数
    - [x] 函数
    - [x] 类型
    - [ ] 模块
  - [ ] 引用查找
    - [x] 普通变量
    - [x] 函数参数
    - [x] 函数
    - [x] 类型
    - [ ] 模块
  - [x] 语法高亮

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
    | extern_identifier
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

global_variable = "const" identifier "=" logic_exp ;

if_statement = "if" logic_exp statement_block ("else" if_statement | statement_block)?;

while_statement = "while" logic_exp statement_block ;

for_statement = "for" (assignment | new_variable) ";" logic_exp ";" assignment statement_block;

statement_block = "{" statements "}" ;


statements = statement* ;

break_statement = "break" ";" ;

continue_statement = "continue" ";" ;

statement = 
    | assignment ";"
    | new_variable ";"
    | return_statement
    | if_statement
    | while_statement
    | break_statement
    | continue_statement
    | function_call
    ;

toplevel_statement = 
    | struct_def
    | function_def
    | global_variable
    | use_statement ";"
    ;

program = toplevel_statement* ;


number = [0-9]+ | number "." number ;

identifier = [a-zA-Z_][a-zA-Z0-9_]* ;

function_def = "fn" identifier "(" (typed_identifier (","typed_identifier)*)? ")" type_name (statement_block | ";") ;

call_function = extern_identifier "(" (logic_exp (","logic_exp)*)? ")" ;

struct_def = "struct" identifier "{" struct_field* "}" ;

type_name = ("&")? identifier ;

typed_identifier = identifier ":" type_name ;

struct_field = typed_identifier ";" ;

struct_init = type_name "{" struct_init_field "}" ;

struct_init_field = identifier ":" logic_exp "," ;

return_statement = "return" logic_exp ";" ;

use_statement = "use" identifier ("::" identifier)* ";" ;

extern_identifier = (identifier "::")* ;

```
