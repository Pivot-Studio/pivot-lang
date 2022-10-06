# nomparser

使用nom完成的parser，兼具parser和lexer功能  

## 注意事项

- 记得使用`delspace`去除前后空格
- 语言的token用`tag_token`来读取，而不是`tag`
- parser理论上不该返回错误，任何语法错误情况都应该生成`ErrorNode`。如果parser返回了错误，将会导致编译panic。
