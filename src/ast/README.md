# AST

抽象代码树相关代码  

## 代码结构

- [range](range.rs) 提供range trait，帮助定位源码位置
- [ctx](ctx.rs) 提供ast的上下文，包括源码位置，变量表，函数表等
- [tokens](tokens.rs) 提供token的定义，包括关键字，运算符，分隔符等
- [compiler](compiler.rs) 提供编译器，用于将ast编译成llvm bitcode，也能jit运行
- [node](node) 各种ast节点

## 注意事项

- 修改`Ctx`结构体成员或者增加函数的时候，若不熟悉rust或者代码，容易出现生命周期问题。若出现此类问题，请按照以下三点依次尝试：
  - `Ctx`中所有直接或间接由`context`生成的类型（几乎都是inkwell库中的类型）的生命周期泛型应该为`'ctx`
  - 所有被borrow的字段的borrow的生命周期应该为`'a`，除了`context`字段（`context`生命周期是`'a`）
  - 如果一直解决不了，联系其他项目维护人员
