# Flow Chart

这是一个附加功能，它将为每个函数生成流程图，以 `.dot` 文件格式输出，`.dot` 文件可通过
- [Graphviz](https://graphviz.org/)  
- [Graphviz Online](https://dreampuf.github.io/GraphvizOnline)  

查看。

## 依赖

- [petgraph](https://github.com/petgraph/petgraph)

## 实现

流程图的生成包含两个步骤：  

- 由 AST 生成 `图` 数据结构
- 根据 `图` 生成 `.dot文件`

### 图的生成  

我们以函数为单位生成 `graph` ，而 AST 根节点为 `ProgramNode`，因此我们需要遍历其 `fntypes`
 ，逐个生成 `graph` 最终得到一个 `Vec` ：

 ```rust,no_run,noplayground
{{#include ../../../src/flow/mod.rs:creategraphs}}
```

接下来实现 `from_ast()` 函数，它接收一个 `NodeEnum`（这是一个 `Statements` 节点）， 返回一个完整的 `Graph`，具体分为两步：
- 初步构建图(`build_graph()`)
- 去除不必要节点(`0入度节点`,`虚节点`,`空节点`))
 ```rust,no_run,noplayground
{{#include ../../../src/flow/mod.rs:fromast}}
```
主要介绍构建图的环节。



定义图的 `节点` 与 `边` ：

 ```rust,no_run,noplayground
{{#include ../../../src/flow/mod.rs:nodeandedge}}
```

`build_graph()`函数以 `Statement` 为单位，针对不同节点构建不同的图结构，为了方便的连接节点，我们定义了 `GraphContext` 用于存储上下文信息:

 ```rust,no_run,noplayground
{{#include ../../../src/flow/mod.rs:GraphContext}}
```

每次调用 `build_graph()` 前，我们需要为构建部分提供两个 `锚点(local_source, local_sink)` ，第一次调用时，`锚点` 即为起点和终点，
以后每次调用前，均需构建两个虚节点，作为`锚点`(虚节点之后将被去掉)。

对于不涉及分支、循环、跳转的简单语句，图结构较为简单：

 ```
 local_source -> current -> local_sink

 local_source ------------> local_sink // 注释

 local_source ---> ERR ---> local_sink // 错误
```

分支及循环语句则较为复杂(以 `IF语句` 为例)：
 ```
IF:                /--Y--> sub_source -----> [...body...] ------> sub_sink ->-\
                   /                                                            \
local_source -> cond                                                        local_sink
                   \                                                           /
                    \--N--> sub_source1 -> Option<[...els...]> -> sub_sink ->-/

```

`if.body` 及 `if.els` 部分可以通过递归调用 `build_graph()` 构建，但是需要先生成两个虚节点，并**暂时**赋给 `ctx`，构建完毕后，
`ctx` 的 `local_source/sink`需要还原  

对于语句块，对每个语句调用 `build_graph()` ，每次将 `sub_source` 更改为 `sub_sink` ，`sub_sink` 则重新创建：
 ```rust,no_run,noplayground
{{#include ../../../src/flow/mod.rs:stsloop}}
```

### .dot 文件生成

我们只需按dot语法格式生成图的点/边的信息即可。下面是一个简单的dot文件：

```
digraph _pointer_struct__name_params {
    D0 [shape=box, style=rounded, label="begin", fontname=""];
    {rank = sink; D1 [shape=box, style=rounded, label="end", fontname=""];}
    D4 [shape=box, label="return\l", fontname=""];
    D4 -> D1;
    D0 -> D4;
}
```
可以在[Graphviz Online](https://dreampuf.github.io/GraphvizOnline)查看对应流程图。