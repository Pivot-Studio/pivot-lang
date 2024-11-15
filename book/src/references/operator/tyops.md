# Type Operators 类型运算符

本章节会对三种特殊的运算符进行介绍：`as`、`is` 和 `impl`。

## `as` 类型转换运算符

`as` 运算符用于将一个值转换为另一个类型。

### 基础类型转换

在转换一些基础类型的时候（比如int类型之间的转换），`as` 运算符是不会失败的，例如：

```plang
let a: i64 = 1;
let b: i32 = a as i32;
```

在这个例子中，`a` 是一个 `i64` 类型的变量，我们将其转换为 `i32` 类型的变量 `b`。这种转换永远成功，尽管可能会导致精度丢失或者损失一部分数据。

### 和类型转换

`as`也可以进行对`和类型`的转换，假设我们有如下类型：

```plang
struct ST{};
type Test<T> = T | ST;
```

我们可以将任意一个类型用`as`转换为`Option`类型：

```plang
let a: i64 = 1;
let b = a as Test<i64>;
```

将子类型转换为和类型是不可能失败的。如果尝试转化为一个不可能的类型，编译器会报错。

反之，`as`运算符也可以将和类型转换为子类型：

```plang
let a: i64 = 1;
let b = a as Test<i64>;
let c: i64 = b as i64!;
let d: Option<i64> = b as i64?;
```

但是，将和类型转换为子类型可能会失败，编译器不会允许你直接使用常规的`as`语句进行转换，你必须使用`?`或`!`来标注转换是非强制的还是强制的。

如果使用`?`标注，那么`x as T?`语句会返回一个`Option<T>`类型，如果转换失败，则该返回值是`None`。

如果使用`!`标注，那么`x as T!`语句会返回一个`T`类型，如果转换失败，则会导致运行时错误（__cast_panic）。

### 泛型类型转换

`as`运算符也可以用于泛型类型的转换：

```plang
fn test<T>(x: T) i64 {
    let y = x as i64!;
    return x;
}
```

如果泛型类型转换失败，会导致运行时错误（__cast_panic）。这种转换是编译期进行的，没有运行时开销。

泛型的转换一定是强制的，需要带上`!`标注。

#### if let ... as ... 语法

`if let ... as ...` 语法可以用于安全的对泛型类型进行转换：

```plang
fn test<T>(x: T) i64 {
    if let y = x as i64 {
        return y;
    }
    return -1;
}
```

## `is` 类型判断运算符

### 基础类型判断

`is` 运算符用于判断一个值是否是某个类型。例如：

```plang
let a: i64 = 1;
let b = a is i64;
```

在这个例子中，`b` 的值是 `true`，因为 `a` 是一个 `i64` 类型的变量。

### 和类型判断

`is` 运算符也可以用于判断和类型：

```plang
let a: i64 = 1;
let b = a as Test<i64>;
let c = b is i64;
```

在这个例子中，`c` 的值是 `true`，因为 `b` 是一个 `i64` 类型的变量。

### 泛型类型判断

特殊的，`is` 运算符也可以用于判断泛型类型：

```plang
fn test<T>(x: T) T {
    if x is i64 {
        doSth();
    }
    return x;
}
```

## `impl` 判断实现运算符

`impl` 运算符用于判断一个泛型是否实现了某个trait。例如：

```plang
trait TestTrait {
    fn test();
}

struct TestStruct{};

impl TestTrait for TestStruct {
    fn test() {
        println("test");
    }
}

fn test<T>(x: T) T {
    let y = x impl TestTrait?;
    let z = x impl TestTrait!;
    z.test();
    return x;
}

```

普通的`impl`语句必须带上`?`或`!`标注，否则编译器会报错。

对于`?`标注，如果泛型类型没有实现trait，那么语句会返回`false`，否则返回`true`。

对于`!`标注，如果泛型类型没有实现trait，那么语句会导致运行时错误（__impl_panic）。以上方例子举例，如果`x`没有实现`TestTrait`，那么`let z = x impl TestTrait!;`会导致运行时错误。反之，如果`x`实现了`TestTrait`，`z`将会是是一个特殊的`T`类型，但是他的增加了实现`TestTrait`的约束，使得下一行代码可以调用`TestTrait`trait的`test`方法。请注意，虽然`z`的类型和`x`的类型都是`T`，但是他们的约束是不同的，严格来说并不是同一类型。`z`的类型`T`，也不是上下文中的`T`类型。

### `if let ... impl ...` 语法

`if let ... impl ...` 语法可以用于安全的对泛型类型进行trait实现判断：

```plang
fn test<T>(x: T) T {
    if let y = x impl TestTrait {
        y.test();
    }
    return x;
}
```

他等同于

```plang
fn test<T>(x: T) T {
    if x impl TestTrait? {
        let y = x impl TestTrait!;
        y.test();
    }
    return x;
}

```











