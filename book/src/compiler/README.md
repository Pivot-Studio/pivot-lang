# Compiler


本文档会总体介绍一遍Pivot-Lang编译器的整体工作流程，帮助开发人员快速上手开发。

## 编译器基本工作流程

```mermaid
graph LR
    A[Source Code] --> C[nom parser]
    C --> D[AST]
    D --> E[LLVM backend]
    E --> F[Object File]
    F --> |linker| G[Executable File]
```

## Nom parser

nom parser包含了编译器的词法分析和语法分析部分。nom parser的主要功能是使用递归下降法将pivot-lang源代码转换为ast。

### A Basic Tour of nom

nom库是纯粹的函数式思想，对于接触函数式比较少的同学可能第一次看到会一脸懵，不过其实他的用法并不困难。
第一次看到nom这种函数式的代码就能自己搞懂函数式编程的思路是很难的，但是你肯定能轻松做到看懂下方教程中的parse
样例。当你看懂了下方的例子再去看我们parser的源码，我相信你只要花一些时间就能搞懂大部分的内容

nom本质是一系列封装好的parser函数和**处理parser的函数**（被称为combinator）的集合，所有的parser函数都有类似的泛型签名，例如他们的返回值一定是：

```rust, ignore

Result<(I, O), E>

```
`parser`函数一般是一个泛型函数，拥有三个类型参数 `I`，`O` 和 `E` ，分别代表输入的类型，输出的类型和错误的类型。其中，`I` 是指输入的类型，通常在编译器中输入的数据类型是 `Span`。`E` 的类型是 `()`，因为在编译器中为了进行错误容忍我们一般不会在parse阶段抛出任何错误。`O` 的类型是我们构建的语法树的节点类型，它应该反映输入中提供的语言结构。在 `parser` 函数中，函数的输入参数类型通常是一个类型为`I`的值，并且输出的内容中的 `I` 表示自身处理后剩余的未处理内容。

举例来说，nom中的`tag`函数能产生一个接受指定值的parser：

```rust,ignore
let parser = tag("123")
let (remain, output) = parser("12345").unwrap();
assert_eq!(remain, "45");
assert_eq!(output, "123");
```

与parser相对应的是combinator，它们的输入参数一般是一个或一组parser组成，他们对自己参数中的parser进行组装并按照自己的规则产生新的parser。上方例子中的`tag`函数就是一种特殊的combinator，它的输入参数是一个字符串，它的输出是一个parser，这个parser只能parse在`tag`参数中指定的字符串。

nom中的combinator非常多，这里只介绍一些常用的combinator。

```rust,ignore

// preceded: 接受两个parser，按照顺序执行，返回第二个parser的结果，第一个parser结果被丢弃
let parser = preceded(tag("123"), tag("456"));
let (remain, output) = parser("1234567").unwrap();
assert_eq!(remain, "7");
assert_eq!(output, "456");

// map_res: 接受一个parser和一个函数，将parser的结果作为函数的输入，返回函数的输出
let parser = map_res(tag("123"), |s: &str| s.parse::<i32>());
let (remain, output) = parser("1234567").unwrap();
assert_eq!(remain, "4567");
assert_eq!(output, 123); // 输出被map_res变为了i32类型，这个combintor经常被用于生成ast node

// alt: 接受多个parser，按照顺序执行，返回第一个成功的parser的结果
let parser = alt((tag("123"), tag("456")));
let (remain, output) = parser("4567123").unwrap();
assert_eq!(remain, "7123");
assert_eq!(output, "456"); // 第一个parser失败，第二个成功，返回第二个parser的结果

```

选择合适的combinator是编写parser的关键，nom提供了一些文档：
- [Nom Recipes](https://github.com/Geal/nom/blob/main/doc/nom_recipes.md)
- [choosing a combinator](https://github.com/Geal/nom/blob/main/doc/choosing_a_combinator.md)



## AST

AST是抽象语法树的简称，是编译器的中间表示。AST是由nom parser生成的，它是一个树形结构，每个节点都是一个结构体，包含了节点的类型、子节点、行号、列号等信息。

## Builder

我们在某次重构中尝试分离了编译后端和AST相关的逻辑，虽然在别的包目前还有一些和llvm代码的耦合，但是理论上我们已经可以添加别的编译后端，
只要该后端能被封装成实现`builder` trait的类型即可。

目前我们有两个后端，一个是LLVM，一个是NoOp，LLVM是目前唯一有真正编译能力的后端，NoOp是一个空后端，它实际上不干
任何工作，它被用于lsp运行模式，因为lsp模式下不需要真的进行编译，使用该后端能增加lsp的性能。


## 需要注意的点

### 1. 差量分析

所有被salsa的proc macro标记的函数都是支持差量运行的。这些函数要求必须是纯函数，他们的输入输出应该看作是只读的，
即使里面存在例如`Arc<Refcell<xxx>>`一类的字段，你也不该在外界对其进行`borrow_mut`然后修改。
如果改了可能导致非常难查出来的bug。
