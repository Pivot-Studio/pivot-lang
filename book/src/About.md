# Pivot Lang

[![codecov](https://codecov.io/gh/Pivot-Studio/pivot-lang/branch/master/graph/badge.svg?token=CA17PWK0EG)](https://codecov.io/gh/Pivot-Studio/pivot-lang) 
[![release](https://github.com/Pivot-Studio/pivot-lang/actions/workflows/release.yml/badge.svg)](https://github.com/Pivot-Studio/pivot-lang/actions/workflows/release.yml)
[![test](https://github.com/Pivot-Studio/pivot-lang/actions/workflows/test.yml/badge.svg)](https://github.com/Pivot-Studio/pivot-lang/actions/workflows/test.yml)


![codecov](https://codecov.io/gh/Pivot-Studio/pivot-lang/branch/master/graphs/sunburst.svg?token=CA17PWK0EG)

此项目目前处于早期开发阶段，不建议用于生产环境。  
[项目地址](https://github.com/Pivot-Studio/pivot-lang)  

## 安装
见[此处](https://lang.pivotstudio.cn/docs/tutorial/installation.html)

## 官网
[https://lang.pivotstudio.cn](https://lang.pivotstudio.cn)

## CONTRIBUTING
[CONTRIBUTING](CONTRIBUTING.md)  
中文见[此处](https://lang.pivotstudio.cn/CONTRIBUTING-CN.html)  
欢迎加入[社区群](https://jq.qq.com/?_wv=1027&k=I5vdShVl)

## dependencies
- [llvm-14](https://github.com/llvm/llvm-project/releases/tag/llvmorg-14.0.6)
- [rust](https://www.rust-lang.org/)

**重要**：如果你想参与开发，请先在项目目录`make vm install`，然后根据自己是linux还是mac运行`make devlinux`或者`make devmac`

## 特点
- 静态编译 与 ~~比较残废的~~ jit支持
- 极其方便的rust互操作
- 支持debug
- 支持lsp，自带vsc插件，能提供优秀的代码支持
- 有gc，自动管理内存



