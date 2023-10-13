
# Pivot Lang Trait 语法和功能

在 Pivot Lang 中，trait 是一种定义共享行为的方式。一个 trait 可以由多个方法组成，这些方法定义了实现该 trait 的类型应具有的行为。

## Trait 定义

一个 trait 是使用 `trait` 关键字定义的，后面跟着 trait 的名称和一个包含方法签名的代码块。例如：

```pivot
trait B {
    fn b() i64;
}
```

这定义了一个名为 `B` 的 trait，它有一个返回 `i64` 的方法 `b`。

## 复合 Traits

一个 trait 可以要求实现它的类型也实现其他的 traits。这是通过 `:` 操作符完成的。例如：

```pivot
trait C: A+B {
    fn c() void;
}
```

这定义了一个名为 `C` 的 trait，要求实现它的类型也要实现 `A` 和 `B` 这两个 traits。

## Trait 实现

一个类型通过为 trait 中的所有方法提供定义来实现一个 trait。这是通过 `impl` 关键字完成的。例如：

```pivot
impl B for test_struct {
    fn b() i64 {
        return 1000;
    }
}
```

这段代码为类型 `test_struct` 实现了 trait `B`。方法 `b` 被定义为返回 `1000`。

关于 `impl` 的更多信息，请参阅 [method](method.md)。


## Trait 使用

一个 trait 可以被用作变量和函数参数的类型。这允许代码与实现了该 trait 的任何类型一起工作。例如：

```pivot
let c: C;
```

这段代码声明了一个类型为 `C` 的变量 `c`。任何实现了 trait `C` 的类型都可以被赋值给 `c`。

## 结论

Pivot Lang 中的 traits 提供了一种定义类型间共享行为的方式。它们允许代码重用和多态，使得语言更加灵活和强大。









