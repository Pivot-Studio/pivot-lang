# Installation



## Windows

windows用户可以使用[scoop](https://scoop.sh/)来安装pivot lang编译器。

**请注意，windows环境下的pivot lang编译器目前只支持x64架构，而且依赖MSVC环境。
你需要安装[Visual Studio](https://visualstudio.microsoft.com/zh-hans/)，并在
安装时勾选C++开发环境**。

```powershell
scoop bucket add pivot https://github.com/Pivot-Studio/scoop
scoop install plc
```


## Linux

目前我们对架构为amd64的Ubuntu 20.04 LTS 和 Ubuntu 22.04 LTS提供了apt包。  
首先你需要添加我们的apt源的gpg key：
```bash
sudo apt update
sudo apt install wget gnupg
wget -O -  https://pivotlang.tech/apt/public.key | sudo apt-key add -
```
然后添加我们的apt源：
```bash
sudo touch chmod +777 /etc/apt/sources.list.d/pl.list
sudo chmod +777 /etc/apt/sources.list.d/pl.list
sudo echo "deb [arch=amd64] https://pivotlang.tech/apt/repo focal main
# deb-src [arch=amd64] https://pivotlang.tech/apt/repo focal main
deb [arch=amd64] https://pivotlang.tech/apt/repo jammy main
# deb-src [arch=amd64] https://pivotlang.tech/apt/repo jammy main">/etc/apt/sources.list.d/pl.list
sudo apt update
```
最后安装pivot lang编译器：
```bash
sudo apt install pivot-lang
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


使用下方命令可以启动一个docker容器，然后在容器中使用pivot lang编译器：
```bash
docker run -it --rm registry.cn-hangzhou.aliyuncs.com/pivot_studio/pivot_lang:latest /bin/bash
```
