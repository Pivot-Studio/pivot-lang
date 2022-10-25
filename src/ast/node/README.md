# node

所有ast节点  

## 注意事项

- 帮助函数写[mod.rs](mod.rs)里
- alloc的时候请调用封装的`alloc`函数
- 尽量调用封装的`position_at_end`函数来改变builder指向，除非你知道你在干什么
- 在生成无法对应到源码的指令时，必须使用`ctx.nodebug_builder`生成指令，而不是`ctx.builder`
- 如果你不使用emit结果，请不要使用`ret.emit(ctx)?`这种带问号的语句，应该使用`_ = ret.emit(ctx)`，这样才能做到尽可能的分析出用户的错误

