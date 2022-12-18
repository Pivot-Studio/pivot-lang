# Language Server

Pivot Lang的Language Server（以下简称LSP）是一个用于为编译器提供语法支持的组件，它同时被用于在编译期间生成诊断信息。  

> 基本上，lsp能够为所有的现代代码编辑器提供服务，但是目前我们只为vsc提供官方支持。如果想在别的编辑器中使用lsp，可能需要自己写一个
> 简单的客户端插件。


### ⚠️注意事项
**有一些函数功能纯粹，可能被用在很多无法预料的地方，如果在这些函数中操作lsp相关功能很可能导致lsp最后工作时出现错误，请尽量避免**！  


比如`TypeNode`的`get_type`函数，一个`TypeNode`可能在代码分析的不同阶段被调用多次`get_type`函数，
如果在其中加入添加`Diagnostic`相关的代码（比如在找不到类型时往`ctx`中加入`TYPE_NOT_FOUND`类型的`Diagnostic`），
很可能最终会在用户出现该错误时产生多个重复的错误提示。  

> **面对这类问题时的正确做法**  
> 这类函数应该返回对应的分析结果，但是不应该直接将其加到分析上下文`ctx`中。分析结果应该在返回后由它的调用者决定是否加入`ctx`

#### **帮助上层函数快速将返回Diagnostic加入上下文的工具**  
为了帮助上层函数快速将返回的Diagnostic加入上下文，我们提供了`add_err_to_ctx`和`add_err_to_ctx_and_ret`宏，
它可以将Diagnostic加入到上下文中并且根据情况定义新的包含返回值的变量。其用法如下：
```rust, no_run
{{#include ../../../src/ast/node/types.rs:helper_macro2}}
```
```rust, no_run
{{#include ../../../src/ast/node/types.rs:helper_macro1}}
```