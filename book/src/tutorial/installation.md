# Installation



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


使用下方命令可以启动一个docker容器，然后在容器中使用pivot lang编译器：
```bash
docker run -it --rm registry.cn-hangzhou.aliyuncs.com/pivot_studio/pivot_lang:latest /bin/bash
```
