# Installation

## 选择你需要的编译模型

Pivot Lang存在两种不同的编译方案：
- 静态编译：编译器会将源码编译成一个可执行文件，能给在操作系统上原生运行
- jit编译：编译器会将源码编译成一个字节码文件，然后在运行时使用编译器指令进行解释执行

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

可以看出，just in time模式的编译器依赖比静态编译少很多，因此如果你不需要debug功能，建议使用jit模式。如果你想要体验完整功能，建议使用静态编译。


## Windows
TODO

## Linux
目前我们对Ubuntu 20.04 LTS 和 Ubuntu 22.04 LTS提供了apt包。  
首先你需要添加我们的apt源的gpg key：
```bash
wget -O -  https://apt.lang.pivotstudio.cn/public.key | sudo apt-key add -
```
然后添加我们的apt源：
```bash
sudo add-apt-repository "deb [arch=amd64] https://apt.lang.pivotstudio.cn/repo focal main"
sudo add-apt-repository "deb [arch=amd64] https://apt.lang.pivotstudio.cn/repo jammy main"
```
最后安装pivot lang编译器：
```bash
sudo apt install pivot-lang
```
你可以运行`plc`来检查是否安装成功。  

请重启当前bash或者运行`source ~/.bashrc`来使环境变量生效。如果想安装`AOT`功能，请手动下载clang-14，下载方式见[此处](https://apt.llvm.org/)



## MacOS
目前最新版macOS上的编译器可以在我们的[release页面](https://github.com/Pivot-Studio/pivot-lang/releases)进行下载，它也许能在老版本MacOS上运行，但是我们并没有测试过。  

在release中，`plc`程序是编译器可执行文件，下载后请将它添加到`PATH`环境变量中，之后即可运行`plc -h`进行验证。

TODO mac文档已过期
## Docker
TODO
