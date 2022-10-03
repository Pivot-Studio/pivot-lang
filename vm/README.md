# VM
放rust写的给pivot-lang使用的函数  
其实好像该叫他runtime  
为什么叫vm？因为一开始我们以为想调用rust必须jit。。  

所有导出的函数需要加`#[is_runtime]`

## 代码结构

- [gc](src/gc/) 一个简单gc


## JIT invalid memory access issue

在jit模式下使用runtime函数可能会出现`invalid memory access`错误，
目前只在测试情况下出现过，不确认以后会不会在非测试情况下出现。   
这个问题本质是rust编译的时候会优化掉不使用的module，导致jit时找不到对应runtime函数。所以建议每个
mod加一个叫做`reg`的函数，里边什么都不做，这样在需要jit测试的时候调用使用模块的`reg`函数，就不会被优化掉了。

