# Compiler

pivot-lang编译器（以下简称编译器）主要由三个部分组成：nom分析器、ast和llvm后端。  


## Nom parser

nom parser包含了编译器的词法分析和语法分析部分。nom parser的主要功能是使用递归下降法将pivot-lang源代码转换为ast。  


## AST

AST是抽象语法树的简称，是编译器的中间表示。AST是由nom parser生成的，它是一个树形结构，每个节点都是一个结构体，包含了节点的类型、子节点、行号、列号等信息。

## LLVM backend

本编译器使用llvm来生成目标代码，llvm的jit部分会被包含在我们的编译器可执行文件中，然而静态编译不行。因此静态编译相比jit会多一个llvm的依赖。
