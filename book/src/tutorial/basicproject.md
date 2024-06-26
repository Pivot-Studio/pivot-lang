# 基础项目

## 两种编译模型介绍

Pivot Lang存在两种不同的编译方案：

- 静态编译：编译器会将源码编译成一个可执行文件，能给在操作系统上原生运行
- jit编译：编译器会将源码编译成一个字节码文件，然后在运行时使用编译器动态生成可执行代码

目前这两种方案使用的编译器是同一个可执行文件（plc），然而他们在依赖和功能上存在一些差别，
下方是一个简单的对比图：  

|                              | jit | 静态编译 |
| ---------------------------- | --- | -------- |
| 完整的pivot lang功能支持     | ✅   | ✅         |
| 生成可执行文件               | ❌   | ✅        |
| 启动速度                     | ❌   | ✅        |
| 运行时优化                   | ✅   | ❌        |
| 支持debug                    | ❌   | ✅        |
| stackmap支持                 | ✅    | ✅        |

## 项目结构

一个最基础的pivot lang项目由一个配置文件和一个源文件组成。

你可以使用`plc new test`来在test 目录下生成他们。

配置文件用于指定项目的一些基本信息，源文件用于编写pivot lang代码。其结构如下：  

```
test
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
在示例中，我们调用了一个系统库中的函数`print_s`，该函数用于打印一个字符串类型的值并换行。此源代码编译后执行会输出`Hello World`。  

## 编译

如果你已经安装了`plc`，那么你可以在项目根目录下执行`plc main.pi`命令来编译该项目。此指令会生成一个名叫`out`的文件，
还有一些中间文件（在target目录下）。  

```admonish tip title="如果你想尝试jit模式的话"
编译时带上`--jit`参数，如`plc main.pi --jit`，这样编译出来的文件会带有`bc`后缀，如`out.bc`。

编译后输入`plc run out.bc`可以jit运行该项目

```
