# planglib
planglib目录下的每一个文件夹都是一个系统模块，在编译的时候会自动被加入依赖中，不需要在配置文件中特殊配置。  

## planglib如何在编译期间被找到
plc编译器在编译时会试图寻找`KAGARI_LIB_ROOT`环境变量，并且将该变量视为`planglib`的根目录  

> 不设置或错误设置`KAGARI_LIB_ROOT`环境变量可能导致无法进行编译或者代码分析

如果你是plang开发者，你可以手动在`~/.bashrc`或者`~/.bash_profile`中加入以下代码：  

```bash
export KAGARI_LIB_ROOT=<pivot-lang project path>/planglib
```

