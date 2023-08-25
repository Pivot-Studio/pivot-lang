# Method

method就是隶属于某个结构体的函数，它们与普通函数**没有**本质区别。  
所有的method都必须在impl块里声明，且method都会隐式的有个`self`参数，该参数是`impl`类型的指针 


## Method Example

最简单的添加method的例子：  
```pivot-lang
{{#include ../../../test/test/method.pi:impl}}
```
在一个包中，可以定义外部引入的包中结构体的`method` . 


调用method的时候，使用`<receiver类型>.<method>`即可 

```pivot-lang
let a = A{};
a.method();
```

类似的，我们可以为一个结构体实现接口

```pivot-lang
impl A for B {
    fn method() void {
        return;
    }
}

...

B{}.method();

```

## Scope

impl的位置是有限制的。如果是单纯的impl一个结构体，
那么impl块必须在结构体所在的包中。如果是为一个外部引入的结构体impl，
则只能采用impl for的方式，且impl块必须和被impl的接口在同一个包中。  



