# Method

method就是隶属于某个结构体的函数，它们与普通函数**没有**本质区别。  
所有的method都必须在impl块里声明，且method都会隐式的有个`self`参数，该参数是`impl`类型的指针 


> 为什么pivot-lang的receiver是隐式的？因为我们有gc，所以没必要像rust那样显示的声明receiver类型，统一指针就可以解决几乎所有情况。

> 可能存在的问题：不方便约束receiver不可变的情况

## Method Example

最简单的添加method的例子：  
```pivot-lang
{{#include ../../../test/main.pi:impl}}
```
在一个包中，可以定义外部引入的包中结构体的`method` . 


调用method的时候，使用`<receiver类型>.<method>`即可 

```pivot-lang
let a = A{};
a.method();
```

