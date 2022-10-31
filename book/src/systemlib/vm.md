# VM
放rust写的给pivot-lang使用的函数  

所有导出的函数需要加`#[is_runtime]`，所有导出结构体需要加`#[repr(C)]`


## JIT invalid memory access issue

在jit模式下使用runtime函数可能会出现`invalid memory access`错误，
这个问题本质是rust编译的时候会优化掉不使用的module，导致jit时找不到对应runtime函数。所以建议每个
mod加一个叫做`reg`的函数，里边什么都不做，这样在需要jit测试的时候调用使用模块的`reg`函数，就不会被优化掉了。


## 使用`is_runtime`导出rust函数

一个被`is_runtime`标记的rust函数在编译到静态库之后，在pivot-lang中声明对应的函数，即可像正常函数一样调用。例如：  

Rust:
```rust
#[is_runtime]
fn printi64ln(i: i64) {
    println!("{}", i);
}
```

Pivot Lang:
```pivot-lang
fn printi64ln(i: i64) void

fn main() void {
    printi64ln(1)
    return
}
```

> ！！！**注意事项**：`is_runtime`标记的函数不能有modifier（比如`pub`，`unsafe`），但是被`is_runtime`标记的`impl`块中的函数不受此限制。  
> ```rust,ignore
> struct MyStruct;
> #[is_runtime("struct")]
> impl MyStruct {
>   pub fn myfunc1() {
>       // ...
>    }
> }
> ```
> 标记impl块时，导出的函数名称会变为`{structname}__{fnname}`的形式，函数允许使用receiver。更多高级用法参见`is_runtime`的rust doc
