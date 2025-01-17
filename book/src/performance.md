# Performance

实验证明在经过我们的数次性能优化之后，Pivot Lang的性能已经在很多场景下超越Golang。优化之后的Immix GC在内存分配和回收上的性能也有了很大的提升，能够
在大部分场景下击败别的GC算法。

## GC Benchmark

我们使用知名的`BdwGC`的benchmark代码进行了测试，将Pivot Lang版本与Golang版本以及BdwGC原版进行了对比。

源代码以及结果见：[Pivot Lang GC Benchmark](https://github.com/Chronostasys/gcbench)

## RealLife application performance

我们使用Pivot Lang实现了有名的[ray tracing in one weekend](https://github.com/Pivot-Studio/rtweekend-pl)项目，
渲染速度相比于[golang实现](https://github.com/hunterloftis/oneweekend)有明显性能优势。

> 请注意golang的实现代码中的参数与原书不完全一样，测试是在调整成一样的参数之后进行的。
