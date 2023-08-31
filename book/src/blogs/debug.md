# Debug 技巧-20230831

今天又花了很长时间找出了一个很隐蔽的bug，我发现debug的时候总是需要花费很多时间，即使我对项目相当了解。
所以我决定以后bug解决的时候，尽量进行一些记录。这里就是第一篇。

## 问题描述

在运行大内存压力逻辑的时候，pivot lang会出现Segmentation Fault。该问题在gc使用release编译的时候稳定复现，debug编译的时候通常不出现。
因此简单推理，该问题和gc相关。

## 问题定位

首先分析gc代码在debug和release情况下的不同，可以发现在debug情况下，gc会在每次分配内存的时候，都会进行一次gc，而release情况下，只有在
内存不足或者自适应算法触发的时候才会进行gc。

大内存压力情况下，内存会频繁的分配，但是测试用的例子中并不是所有分配的内存都能一直存活，因此在debug情况下，gc会频繁的进行，导致
总体上程序所用内存较少。而release情况下，gc不一定及时，堆可能在某次gc之前就已经满了，从而在下一次分配的时候出发紧急回收。

理论上如果上方逻辑全部正常，不应该出现Segmentation Fault。因此合理推断紧急回收逻辑存在问题。主要考虑以下两种可能：

1. 紧急回收触发时机不对，触发的时候实际上已经分配了越界的内存，导致触发前就已经出现了越界访问。
2. 紧急回收逻辑有bug，导致触发回收的时候出现了越界访问。

### 检查代码

<https://github.com/Pivot-Studio/pivot-lang/blob/a7b52388416375897e2078e6e0262fb319d21265/immix/src/allocator/global_allocator.rs#L89-L110>

上方是分配新block的代码（如果你对block有疑问，请看[此处](https://lang.pivotstudio.cn/docs/systemlib/immix.html#admonition-%E9%83%A8%E5%88%86immix-gc%E6%9C%AF%E8%AF%AD%E4%BB%8B%E7%BB%8D)）

可以看到，当`current >= heap_end`，我们才判定为heap已满。但是这是有问题的，如果`current`在`heap_end-BLOCK_SIZE`和`heap_end`之间，那么新分配的block将会有一部分在堆外，这部分内存是没有被初始化的，访问会造成segmentation fault。

我们通过把代码修改为提前给`current`加上一个`BLOCK_SIZE`的值来解决这个问题

<https://github.com/Pivot-Studio/pivot-lang/blob/6f4f51f8101928e719161ccbe07a70d2340d2625/immix/src/allocator/global_allocator.rs#L91-L113>

### 测试

测试发现还是存在问题，但是问题发生的位置有变化。于是为了方便复现问题，我禁用了自动回收，仅启用紧急回收，接下来在紧急回收的时候打印log进行追踪。gc在没有空间，并且紧急回收结束也没有空间的时候，会返回null指针。因此我在最外层也对null进行了检查。

追踪之后发现，在返回null之前并没触发紧急回收，因为出错之前调用的是特殊分配函数`gc_malloc_no_collect`，该方法会在分配失败的时候直接返回null，而不触发紧急回收。

### 继续分析原因

分析道这里segmentation fault的原因已经呼之欲出了：在堆满的时候，mutator调用了`gc_malloc_no_collect`，该方法返回null，mutator没有检查null，直接使用了null指针，导致segmentation fault。

但是加上对null的检查也并不解决问题，因为我们不可能在null的时候触发紧急回收来重新分配内存，这么做违背了`gc_malloc_no_collect`的初衷。

`gc_malloc_no_collect`的设计是为了处理一些需要分配内存的“非安全点”。在gc中，能够安全进行回收的位置叫做安全点，反之为非安全点。安全点要求所有应用程序之后需要的内存都直接或间接的被栈所引用（或者说，从栈开始进行扫描时可达），非安全点则是有一部分以后会被用到的内存暂时从栈不可达。在非安全点触发gc将会导致应用程序使用的内存被错误的回收，从而导致崩溃。

目前的代码中，该函数主要用于函数调用返回值的分配。因为函数调用结束后，在将返回值显式的存到栈上之前，返回值是不可达的。而且函数调用返回值需要存到堆上，因此需要`gcmalloc`一块空间，这个malloc操作是一个非安全点，也就是使用`gc_malloc_no_collect`的位置。

### 解决方案

根据上方分析，实际上我们遇到了一个两难问题：

1. 非安全点不能触发gc否则会出现内存安全问题
2. 在堆满的时候使用`gc_malloc_no_collect`导致在实际上还有空间的时候出现oom

那么上方两个点至少有一个要改变。

仔细分析之后，发现函数调用返回值其实不一定需要在非安全点进行内存分配，如果我们在函数调用之前提前malloc好，然后在函数调用结束之后再将返回值存到堆上，就可以避免在非安全点进行内存分配。所以我修改了写法。

在移动malloc位置并且替换malloc函数之后，问题解决。
