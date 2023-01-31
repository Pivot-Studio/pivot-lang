# pivot-lang immix gc

本文档将会描述pl使用的immix gc的一些实现细节与对外接口

```admonish
此页面仍在编写中，内容可能有疏漏
```

## Overview

此gc是我们基于[immix gc论文](https://www.cs.utexas.edu/users/speedway/DaCapo/papers/immix-pldi-2008.pdf)实现的，
大部分的实现细节都与论文一致，对于一些论文没提到的细节我们自行进行了实现，参考了很多别的gc项目。该gc是一个支持多线程使用的、
基于shadow stack的，精确mark-region 非并发（Concurrency） 并行（Parallelism） gc。  


```admonish tip title="gc的并行（Concurrency）与并发（Parallelism）"
gc中并行和并发是两个不同的术语，并行gc指的是能够在应用不暂停的基础上进行回收的gc，
而并发gc指的是gc在回收的时候能够使用多个线程同时进行工作。一个gc可以既是并行的也是并发的，
我们的immix gc目前只具备并行能力
```
这里有一些创建该gc过程中我们主要参考的资料，列表如下：  

- [immix gc论文](https://www.cs.utexas.edu/users/speedway/DaCapo/papers/immix-pldi-2008.pdf)
- [playxe 的 immixcons（immix gc的一个rust实现，回收存在bug）-- 很多底层内存相关代码是参考该gc完成的，还有在函数头中加入自定义遍历函数的做法](https://github.com/playXE/libimmixcons)
- [给scala-native使用的一个immix gc的C实现](https://github.com/scala-native/immix)
- [康奈尔大学CS6120课程关于immix gc的博客，可以帮助快速理解论文的基本思路](https://www.cs.cornell.edu/courses/cs6120/2019fa/blog/immix/)


## General Description

本gc是为pl __定制的__，虽然理论上能被其他项目使用，但是对外部项目的支持**并不是主要目标**  

pl的组件包含一个全局的`GlobalAllocator`，然后每个`mutator`线程会包含一个独属于该线程的`Collector`，每个`Collector`中包含一个
`ThreadLocalAllocator`。在线程使用gc相关功能的时候，该线程对应的`Collector`会自动被创建，直到线程结束或者
用户手动调用


