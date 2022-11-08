# GC

pivot-lang 是一门使用gc进行内存管理的语言。  
pivot-lang 的gc目前是使用rust写的，采用非常简单的 `mark and sweep` 算法。
栈变量采用`shadow stack`模式进行追踪。  
目前gc的源代码在vm项目中

> 目前的gc非常简单，也没做任何优化，属于能用就行。总代码量也很少，欢迎为我们改进它。

## Shadow Stack

pl的gc目前是精确gc，编译器会插入指令以让gc运行时能知道gcroot有哪些。gc记录的gcroot列表就是`shadow stack`。
在mark阶段，gc会从`shadow stack`开始扫描。  




