# AST

抽象代码树相关代码  

## 代码结构

- [range](range.rs) 提供range trait，帮助定位源码位置
- [ctx](ctx.rs) 提供ast的上下文，包括源码位置，变量表，函数表等
- [tokens](tokens.rs) 提供token的定义，包括关键字，运算符，分隔符等
- [compiler](compiler.rs) 提供编译器，用于将ast编译成llvm bitcode，也能jit运行
- [node](node) 各种ast节点
