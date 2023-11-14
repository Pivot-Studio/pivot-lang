# 环境准备

## Rust

可以在项目的根目录下的`rust-toolchain`文件中查看当前项目使用的rust版本，如果你的rust版本不是这个版本，可以使用rustup安装这个版本的rust。

```bash
rustup install $(cat rust-toolchain)
```

国内如果没代理安装rust比较困难，建议使用清华源进行安装，见[此处](https://mirrors.tuna.tsinghua.edu.cn/help/rustup/)

安装完毕后，可以进行cargo换源设置，防止依赖无法下载，见[此处](https://mirrors.tuna.tsinghua.edu.cn/help/crates.io-index.git/)

## LLVM

Pivot-Lang目前使用LLVM 14作为后端，所以需要安装LLVM。如果你使用的是MacOS，可以使用brew安装LLVM。

```bash
brew install llvm@14
```

如果你使用的是ubuntu，可以使用[这里](https://github.com/Pivot-Studio/setup-llvm/blob/main/scripts/install_llvm.sh)的脚本进行安装

# CMake

我们的垃圾回收模块使用了LLVM中的StackMap功能，需要使用CMake进行编译。

## 环境变量

开发项目需要正确配置一些环境变量才能让你保证开发时正确跑通测试。

运行下方命令将会把项目需要的一些环境变量加入到你的初始化脚本中

```bash
make devlinux # linux
make devmac # mac
```
注意这些命令都只需要跑一次

## 测试是否成功配置开发环境

上方步骤完成后，可以运行下方命令进行测试

```bash
make test
```

如果上方命令都成功执行，那么恭喜你，你已经成功配置了开发环境


## 使用github codespace 进行开发

使用github codespace进行开发的环境配置较为简单，但是请注意费用问题。


点击以下链接即可创建一个包含pl开发环境的codespace

[create codespace](https://github.com/codespaces/new?machine=standardLinux32gb&repo=535925143&ref=master&devcontainer_path=.devcontainer%2Fdevcontainer.json&location=SouthEastAsia)

创建完毕后，建议使用本地vscode打开codespace中的项目进行开发

## 常见问题

### No suitable version of LLVM was found system-wide or pointed 

需要设置llvm环境变量，如果你使用的是ubuntu，可以在`~/.bashrc`中加入如下代码然后执行.

```bash
export LLVM_SYS_140_PREFIX=/usr/lib/llvm-14
source ~/.bashrc
```

如果是macOS通过`brew install llvm@14`安装，则需要设置环境变量:
```bash
export LLVM_SYS_140_PREFIX=$(brew --prefix llvm@14)
```


### Could NOT find ZLIB (missing: ZLIB_LIBRARY ZLIB_INCLUDE_DIR)

缺少zlibdev造成的，如果你是ubuntu机器，使用下方命令进行安装：

```bash
sudo apt install zlib1g-dev
```




