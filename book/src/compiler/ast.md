# AST

抽象语法树是目前编译器中最复杂的部分，它是编译器的中间表示，也是编译器的核心。本节将介绍AST的设计和实现。  

## AST的设计

基本上，所有源代码中的基础单位都会对应抽象语法树中的一个节点。抽象语法树有很多类型的节点，他们可能会相互引用。  

所有的节点都必须实现`Node` trait，这个trait定义了节点的基本行为。  

```rust,no_run,noplayground
{{#include ../../../src/ast/node/mod.rs:node}}
```

你可能注意到了，`Node`trait继承了`RangeTrait`，这个trait定义了节点的位置信息。  

```rust,no_run,noplayground
{{#include ../../../src/ast/node/mod.rs:range}}
```
一般来说，`RangeTrait`的实现通过`#[range]`宏来自动生成，你不需要手动实现它。  

`Node`接口中的`print`函数用于打印节点的信息，它会被用于调试。`print`打印的结果和`tree`的输出非常像，你需要用一些工具函数来
格式化输出。以`ifnode`的`print`函数为例：  

```rust
{{#include ../../../src/ast/node/control.rs:print}}
```  

`emit`函数是生成llvm代码的核心，它会调用llvm api构造自己对应的llvm ir。在编译的时候，最上层节点的`emit`会被调用，
该函数会递归的调用自己的子节点的`emit`函数，最终生成整个程序的llvm ir。  
下方是`ifnode`的`emit`函数：  

```rust,no_run,noplayground
{{#include ../../../src/ast/node/control.rs:emit}}
```

emit函数的参数是节点自身，第二个参数是编译上下文。编译上下文中会包含一些需要透传的信息，比如符号表，llvmbuilder，lsp参数等。  


## 打印AST结构

plc命令行工具有打印ast的功能，你可以使用`plc xxx.pi --printast`命令来打印ast结构。  
下方是一个ast打印结果的样例：

```ast
...
file: /Users/bobli/src/pivot-lang/test/sub/mod.pi
ProgramNode
 └─ FuncDefNode
     ├─ id: name
     ├─ TypeNameNode
     │   └─ ExternIdNode
     │       └─ VarNode: void
     └─ StatementsNode
         └─ RetNode
file: /Users/bobli/src/pivot-lang/test/mod2.pi
ProgramNode
 ├─ UseNode
 │   ├─ VarNode: sub
 │   └─ VarNode: mod
 ├─ FuncDefNode
 │   ├─ id: test_mod
 │   ├─ TypedIdentifierNode
 │   │   ├─ id: args
 │   │   └─ TypeNameNode
 │   │       └─ ExternIdNode
 │   │           └─ VarNode: i64
 │   ├─ TypeNameNode
 │   │   └─ ExternIdNode
 │   │       └─ VarNode: void
 │   └─ StatementsNode
 │       └─ RetNode
 └─ StructDefNode
     ├─ id: Mod2
     └─ TypedIdentifierNode
         ├─ id: y
         └─ TypeNameNode
             └─ ExternIdNode
                 └─ VarNode: bool
...
```
