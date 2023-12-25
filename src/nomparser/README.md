# nomparser

Nomparser uses nom to scan and parse the source code into an AST. It takes charges on both a lexer and a parser.
The nomparser is an LL parser, which uses the SDD(syntax-directed-definition), which combines the parser and syntax tree construction.

The syntax of pivot-language is specified in [grammar.enbf](./grammar.ebnf), and the respective parsing handler could be searched by the snake case of EBNF item. 


## 注意事项

- 记得使用`delspace`去除前后空格
- 语言的token用`tag_token`来读取，而不是`tag`
- parser理论上不该返回错误，任何语法错误情况都应该生成`ErrorNode`。如果parser返回了错误，将会导致编译panic。
