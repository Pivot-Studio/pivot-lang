# 为Pivot Lang贡献代码

非常感谢您愿意对本项目提供帮助！

下方是对您为 [pivot-lang][1] 贡献代码的一些帮助

如果您刚开始了解pivot-lang项目，可以加入我们的社区 [qq 群](https://jq.qq.com/?_wv=1027&k=I5vdShVl) 向我们提问.

**因为本项目还处于早期阶段**: 本页面的指导很可能会在未来有更改，欢迎帮助我们改进此页面！

[1]: https://github.com/Pivot-Studio/pivot-lang

## 基础

### 开发文档

我们的基础环境配置教程在 [dev-prepare][d1] 中，开发前请务必阅读。

[d1]:./dev-prepare.md

### 开源协议

本项目使用 [MIT][l1] 协议。对本项目贡献代码即表示您同意您的更改遵守该协议。

[l1]: https://opensource.org/licenses/MIT


## 您能做的事情

### Issues

我们有很多的已有的issue，在添加新功能的时候我们也会添加相关的issue。如果您发现我们的bug或者有什么需求，欢迎新建 [issues][i2] 
来告诉我们。您也可以看一些 [open][i3] 的issue并且参与讨论或者贡献代码帮助修复它。


[i2]: https://github.com/Pivot-Studio/pivot-lang/issues
[i3]: https://github.com/Pivot-Studio/pivot-lang/issues?q=is%3Aopen+is%3Aissue

### Code

非常欢迎帮助我们实现新功能。我们的新功能实现分为几个阶段：

- 提出，讨论需求的合理性和必要性
- 讨论实现方案
- 实现
- reveiew
- 合并

一些简单的需求可以跳过第二个阶段，所有超过第一个阶段的需求都会被放在我们的[project][p]中。如果您想帮助实现
已有需求，请去此页面寻找处于new或者ready状态的项目。如果您想实现一个新的需求，请先在[issues][i2]中提出，最好加入
[qq群](http://qm.qq.com/cgi-bin/qm/qr?_wv=1027&k=vjKI1nbRHAIz1UbmDOjttLurEw93mLhA&authKey=U6cUmnIxiptTskr9trZZ9vc2p291uWht8TlzPSOEPXliihLC9vAYMaRwDI0%2FolR8&noverify=0&group_code=688301255) 和我们一起讨论方案，在讨论决定通过后，我们会在[project][p]中添加一个新的对应项目。

[p]: https://github.com/orgs/Pivot-Studio/projects/7/views/1

### Tests

强烈建议在提交修改的时候同时添加对应的测试，帮助我们将测试覆盖率保持在 **85%**, 帮助我们进一步完善测试也是相当欢迎的。

请在提交pr前确认自己的修改能通过所有的测试(通过运行 `cargo test --all`)

### Benchmark

目前我们还没有基准测试，欢迎帮助我们添加基准测试。

### 文档

参见 [文档][d1] 网站。对应源码在 [book](https://github.com/Pivot-Studio/pivot-lang/tree/master/book) 目录中，欢迎帮助我们完善文档。

[d1]: https://lang.pivotstudio.cn/

## 风格

### Issue 风格

请在提出issue时提供至少三个小自然段的说明，包括：你想干什么，遇到了什么问题，如果复现这个问题等。


如果可能的话，希望您能提供:

- 如果是在使用的时候遇到的bug，最好有一小段代码或者一个指向 [gist][is1] 的链接，其中包含能复现问题的代码。
- 完整的 backtrace, 如果是进程崩溃相关的问题。
- 一个示例项目，如果是编译相关的问题。

[is1]: https://gist.github.com

### 代码风格

rust代码风格通过使用 [rustfmt][cs1] 进行统一 
请尽量减少代码重复率，增加可读性。  

为了避免不同的rust小版本格式化的区别，请使用以下命令格式化: `cargo +stable fmt`  

[cs1]: https://github.com/rust-lang-nursery/rustfmt  

## 其他重要事项

### Immix GC对代码生成的影响

Pivot Lang使用的Immix GC是一个精确的、可重定位的垃圾回收器。重定位（evacuation）指
GC在触发的时候可能会移动堆中的对象，这主要是为了解决内存碎片化的问题。

GC的重定位正确性依赖于严谨准确的代码生成。

由于GC要保证堆对象在被移动之后，所有指向原对象位置的指针都被更新为指向新对象，所以GC需要
知道所有指向堆对象的指针。所以在Pivot中，一切堆对象的直接或间接的指针都会有个对应的栈
指针。同时，因为触发重定向的逻辑比较复杂，这种问题往往很难测试出来，具有很高的随机性，
所以目前的debug模式编译的GC在每次malloc都会触发GC，并且会强制每次回收都触发重定向，并且每一个堆里的对象都会被重定向。
