# Module

模块化  

## 模块的划分和使用

总体来说，pl的模块化与rust类似，但是规则比rust简单。  
任何一个pl文件都是一个模块，模块的名字就是文件名。一个pl项目的根目录一定有`Kagari.toml`配置文件，之后所有该项目
模块的路径都是从该文件所在的目录开始计算的。  

举个例子，如果我的pl项目有以下的目录结构：  

```text
test
├── Kagari.toml
├── mod1.pi
├── main.pi
└── sub
    └── mod.pi

```

如果`main.pi`想使用`mod1.pi`或者`mod.pi`中的函数，那么可以这样写：  

```pivot-lang
use mod1;
use sub::mod;

fn main() void {
    mod1::func();
    mod::func();
    return;
}
```
如果`mod.pi`想使用`mod1.pi`中的函数，那么可以这样写：  

```pivot-lang
use mod1;

fn main() void {
    mod1::func();
    return;
}

```


```admonish warning
pl的模块**不支持**循环引用

```
```admonish warning
目前pl引入的所有模块必须对应到相对的pi文件（不能对应目录），且引入同名的模块是UB。

```

## 单独引入和全引入

pl的模块引入支持单独引入某些符号或者全引入。

```pivot-lang
use mod1::func1; // 单独引入
use mod2::*; // 全引入
```
请注意，单独引入仍然会形成父子模块关系，会对[extension method](method.md#extension-method)的作用域产生影响。  








## 引用另一个pl项目
目前只支持引用本地的pl项目，引用的方式是在`Kagari.toml`中添加`[deps]` 

```toml
[deps]
sub3 = { path = "sub2" }
```

使用时，引用的项目模块会在`deps`中定义的命名空间之下：

```pivot-lang
use sub3::lib;

fn main() void {
    lib::func();
    return;
}

```
