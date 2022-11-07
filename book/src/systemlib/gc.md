# GC

pivot-lang 是一门使用gc进行内存管理的语言。  
pivot-lang 的gc目前是使用rust写的，采用非常简单的 `mark and sweep` 算法。
栈变量采用`shadow stack`模式进行追踪。  

## Shadow Stack



