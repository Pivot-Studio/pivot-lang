# Method

method就是隶属于某个结构体的函数，它们与普通函数**没有**本质区别。  
所有的method都必须在impl块里声明，且method都会隐式的有个`self`参数，该参数是`impl`类型的指针

## Method Example

最简单的添加method的例子：  

```pivot-lang
{{#include ../../../test/test/method.pi:impl}}
```

在一个包中，可以定义该包中结构体的`method` .

调用method的时候，使用`<receiver类型>.<method>`即可

```pivot-lang
let a = A{};
a.method();
```

## Extension Method

实际上，如果一个结构体不在当前包中定义，我们可以仍然可以定义它实现 **当前包中** `Trait` 的 `method` 。这种方法被称为`extension method`。  

```pivot-lang
pub trait Eq<S> {
    fn eq(r:*S) bool;
}


impl <T:Eq<T>> Eq<[T]> for [T] {
    fn eq(r:*[T]) bool {
        if arr_len(*self) != arr_len(*r) {
            return false;
        }
        for let i = 0; i < arr_len(*self); i = i + 1 {
            if !(*self)[i].eq(&(*r)[i]) {
                return false;
            }
        }
        return true;
    }
}
```

当然，你也可以合法的为本包中的结构体实现子包（或本包）中的`Trait`，但是这种情况下的`method`不属于`extension method`。  

### 与普通method的区别

`extension method`与普通`method`的区别在于其作用域，普通`method`的作用域与其所在的结构体相同，只要出现了该结构体类型就一定可以使用他的
`method`（这里的出现不止指显示的引入结构体，通过函数返回值等方式间接的获取该类型的值也算）。

而`extension method`的作用域则是在`Trait`所在的包中，要使用`extension method`，必须直接或者间接的依赖了该`Trait`所在的包。
