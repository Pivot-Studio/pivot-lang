# Installation

## 选择你需要的编译模型

Pivot Lang存在两种不同的编译方案：
- 静态编译：编译器会将源码编译成一个可执行文件，能给在操作系统上原生运行
- ~~jit编译：编译器会将源码编译成一个字节码文件，然后在运行时使用编译器指令进行解释执行~~

```admonish warning
目前jit模式暂时不可用，因为它与我们新的immix gc不兼容，详细情况请看[这里](../references/stackmap.md)
```

目前这两种方案使用的编译器是同一个可执行文件（plc），然而他们在依赖和功能上存在一些差别，
下方是一个简单的对比图：  



|                              | jit | 静态编译 |
| ---------------------------- | --- | -------- |
| 完整的pivot lang功能支持     | ✅   | ✅         |
| 生成可执行文件               | ❌   | ✅        |
| 启动速度                     | ❌   | ✅        |
| 依赖llvm                     | ❌   | ✅        |
| 依赖预编译的pivot lang系统库 | ❌   | ✅        |
| 运行时优化                   | ✅   | ❌        |
| 支持debug                    | ❌   | ✅        |
| stackmap支持                 | ❌   | ✅        |

可以看出，just in time模式的编译器依赖比静态编译少很多，因此如果你不需要debug功能，建议使用jit模式。如果你想要体验完整功能，建议使用静态编译。


## Windows
TODO

## Linux
目前我们对架构为amd64的Ubuntu 20.04 LTS 和 Ubuntu 22.04 LTS提供了apt包。  
首先你需要添加我们的apt源的gpg key：
```bash
apt update
apt install wget gnupg
wget -O -  https://apt.lang.pivotstudio.cn/public.key | apt-key add -
```
然后添加我们的apt源：
```bash
echo "deb [arch=amd64] https://apt.lang.pivotstudio.cn/repo focal main
# deb-src [arch=amd64] https://apt.lang.pivotstudio.cn/repo focal main
deb [arch=amd64] https://apt.lang.pivotstudio.cn/repo jammy main
# deb-src [arch=amd64] https://apt.lang.pivotstudio.cn/repo jammy main">/etc/apt/sources.list.d/pl.list
apt update
```
最后安装pivot lang编译器：
```bash
apt install pivot-lang
```
你可以运行`plc`来检查是否安装成功。  

安装完成后请按照提示设置环境变量


## MacOS

MacOS可以使用`homebrew`进行安装。  

首先你需要添加我们的homebrew tap：
```bash
brew tap pivot-studio/tap
```

然后安装pivot lang编译器：
```bash
brew install pivot-lang
```

安装完成后请按照提示设置环境变量

## Docker
TODO
