# VM
放rust写的给pivot-lang使用的函数  
其实好像该叫他runtime  
为什么叫vm？因为一开始我们以为想调用rust必须jit。。  

所有导出的函数需要加`#[is_runtime]`

## 代码结构

- [gc](src/gc/) 一个简单gc

