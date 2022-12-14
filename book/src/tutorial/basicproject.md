# 基础项目
## 项目结构

一个最基础的pivot lang项目由一个配置文件和一个源文件组成。配置文件用于指定项目的一些基本信息，源文件用于编写pivot lang代码。其结构如下：  

```
.
├── Kagari.toml
└── main.pi
```

### 配置文件

一个pl项目的根目录必须有一个名为`Kagari.toml`的配置文件。示例配置文件的内容如下：
```toml
{{#include example/Kagari.toml}}
```
entry指定了该项目的入口文件，即编译器将从该文件开始编译。如果缺少该配置`plc`将无法编译该项目  

### 源文件

示例项目中的`main.pi`为源文件。其内容如下：
```pl
{{#include example/main.pi}}
```
源文件的后缀名必须为`.pi`。  
在示例中，我们调用了一个系统库重的函数`printi64ln`，该函数用于打印一个i64类型的值并换行。此源代码编译后执行会输出`666`。  
> 重要：`printi64ln`函数是目前pl runtime中的一个测试用内置函数，此函数可能会在未来移除

## 编译
如果你已经安装了`plc`，那么你可以在项目根目录下执行`plc main.pi`命令来编译该项目。此指令会生成一个名叫`out.bc`的文件，还有一些中间文件  

> 如果你配置了静态编译环境，还会生成一个叫做`out`的文件，该文件是一个可执行文件，可以直接运行
> 而如果你只有jit环境，该文件不会生成，并且编译命令会输出一个clang报错和两行warning，这是正常现象

## jit运行

编译后输入`plc run out.bc`可以jit运行该项目，其输出结果如下：  

```
666
```



