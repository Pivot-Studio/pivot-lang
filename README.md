# Pivot-lang

[![codecov](https://codecov.io/gh/Pivot-Studio/pivot-lang/branch/master/graph/badge.svg?token=CA17PWK0EG)](https://codecov.io/gh/Pivot-Studio/pivot-lang) 
[![release](https://github.com/Pivot-Studio/pivot-lang/actions/workflows/release.yml/badge.svg)](https://github.com/Pivot-Studio/pivot-lang/actions/workflows/release.yml)
[![test](https://github.com/Pivot-Studio/pivot-lang/actions/workflows/test.yml/badge.svg)](https://github.com/Pivot-Studio/pivot-lang/actions/workflows/test.yml)
[![docs](https://drone.pivotstudio.cn/api/badges/Pivot-Studio/pivot-lang/status.svg?ref=refs/heads/gh-pages)](https://drone.pivotstudio.cn/Pivot-Studio/pivot-lang)


![codecov](https://codecov.io/gh/Pivot-Studio/pivot-lang/branch/master/graphs/sunburst.svg?token=CA17PWK0EG)

此项目目前处于早期开发阶段，不建议用于生产环境。  
[项目地址](https://github.com/Pivot-Studio/pivot-lang)  

## 安装
见[此处](https://lang.pivotstudio.cn/tutorial/installation.html)

## 文档地址
https://lang.pivotstudio.cn  

## CONTRIBUTING
[CONTRIBUTING](CONTRIBUTING.md)  
中文见[此处](https://lang.pivotstudio.cn/CONTRIBUTING-CN.html)  
欢迎加入[社区群](https://jq.qq.com/?_wv=1027&k=I5vdShVl)

## dependencies
- [llvm-14](https://github.com/llvm/llvm-project/releases/tag/llvmorg-14.0.6)
- [rust](https://www.rust-lang.org/)

**重要**：如果你想参与开发，请先在项目目录`make vm install`，然后根据自己是linux还是mac运行`make devlinux`或者`make devmac`

## 特点
- 静态编译（jit模式与immix gc不兼容，因为llvm生成自定义stackmap[目前必需要静态编译](https://llvm.org/docs/GarbageCollection.html#emitting-assembly-code-gcmetadataprinter)）
- 极其方便的rust互操作
- 支持debug
- 支持lsp，自带vsc插件，能提供优秀的代码支持


## 项目结构

- [vm](vm) 包含rumtime
- [src](src) 编译器源码所在
- [internal_macro](internal_macro) 内部过程宏

## grammar

```ebnf
add_exp = 
    | mul_exp ("+" | "-" add_exp)?
    ;

mul_exp = 
    | unary_exp ("*"｜"/" mul_exp)?
    ;

unary_exp =
    | pointer_exp
    | ("-" | "!") pointer_exp
    ;


pointer_exp = ("&"|"*")* complex_exp;

complex_exp = primary_exp (take_exp_op|array_element_op|call_function_op)*;

take_exp_op = ("." identifier) ;

array_element_op = ('[' logic_exp ']') ;

call_function_op = ("(" (logic_exp (","logic_exp)*)? ")") ;

primary_exp =
    | number
    | bool_const
    | parantheses_exp
    | extern_identifier
    | struct_init_exp
    | string_literal
    ;

parantheses_exp = "(" logic_exp ")";

number = [0-9]+ ("." number)? ;

identifier = [a-zA-Z_][a-zA-Z0-9_]* ;

extern_identifier = (identifier "::")* identifier ;

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

struct_init_exp = 
    | type_name "{" (struct_init_exp_field ("," struct_init_exp_field)* )? "}" 
    ;

struct_init_exp_field = identifier ":" logic_exp ;

assignee = pointer_exp;

assignment = assignee "=" logic_exp ;

new_variable = "let" identifier "=" logic_exp ;

global_variable = "const" identifier "=" logic_exp ;

if_statement = "if" logic_exp statement_block ("else" if_statement | statement_block)?;

while_statement = "while" logic_exp statement_block ;

for_statement = "for" (assignment | new_variable) ";" logic_exp ";" assignment statement_block;

statement_block = "{" statements "}" ;

impl_block = "impl" extern_identifier "{" function_def* "}" ;

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
    | complex_exp ";"
    ;

toplevel_statement = 
    | struct_def
    | function_def
    | global_variable
    | use_statement ";"
    ;

program = toplevel_statement* ;

function_def = "fn" identifier "(" (typed_identifier (","typed_identifier)*)? ")" type_name (statement_block | ";") ;

generic_type = "<" type_name ("|" type_name)* ">" ;

generic_type_def = "<" identifier ("|" identifier)* ">" ;

struct_def = "struct" identifier generic_type_def? "{" struct_field* "}" ;

type_name = "*"* extern_identifier ;

typed_identifier = identifier ":" type_name ;

struct_field = typed_identifier ";" ;

return_statement = "return" logic_exp ";" ;

use_statement = "use" identifier ("::" identifier)* ";" ;

string_literal = "\"" [^"]* "\"" ;

trait_def = "trait" identifier generic_type_def? (":" type_add)? "{" function_def* "}" ;

type_add = type_name ("+" type_name)* ;

macro_match_exp = 
    | any_exp_except_dollar_and_parantheses
    | "$" identifier ":" tp
    | "(" macro_match_exp * ")" "*"
    | "(" macro_match_exp * ")" "+"
    | "(" macro_match_exp * ")"
    ;

macro_match_arm = "(" macro_match_exp* ")" "=>" statement_block ;
```
