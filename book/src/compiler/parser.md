# Parser

parser源代码位置位于`src/nomparser`目录下，包含了词法分析和语法分析部分。  


## nom

[nom](https://github.com/Geal/nom)是一个用rust编写的parser combinator库，它不像lr分析器一样提供生成代码的功能，而是
提供一组函数，这些函数可以用来组合出各种parser。  

相比于lr分析器，nom的优点是它的parser combinator非常灵活，熟练后可以快速组合出各种parser，
而且可自定义性非常的强，看起来也很直观，相比很多ir生成器的语法并没有复杂多少，但是带来了更好的
语法支持（一般的ir分析生成器的语法定义文件不会有编程语言那么好的语法支持）。  

会使用nom是读懂编译器parser代码的重要前提，这里强烈推荐两个nom文档：  

- [Nom Recipes](https://github.com/Geal/nom/blob/main/doc/nom_recipes.md)
- [choosing a combinator](https://github.com/Geal/nom/blob/main/doc/choosing_a_combinator.md)

## parser结构

parser的主要功能是使用递归下降法将pivot-lang源代码转换为ast。如果你不了解递归下降法，可以先看看[这篇文章](https://ruslanspivak.com/lsbasi-part1/)。  

对于pivot lang的每一条语法规则，都会在parser里对应一个分析函数，这些分析函数可能会调用其他分析函数，最终最上层的分析函数可以将完整的源代码转换为ast。  

pivot lang的完整语法规则见[这里](../../../#grammar)  

parser最顶层的函数是`parse`，它接受一个源文件输出一个AST根节点。

```rust,no_run,noplayground
{{#include ../../../src/nomparser/mod.rs:127:136}}
```
