
# Pivot Lang 泛型语法和功能

在 Pivot Lang 中，泛型是一种定义可以处理多种类型的代码的方式。泛型可以用于定义函数、结构体和trait。在定义时，泛型参数被指定为特定的标识符，这个标识符可以在定义中的任何地方使用。在使用泛型定义的代码时，泛型参数被具体的类型替换。

以下是一些使用泛型的例子：

## 1. 在结构体中使用泛型：

```pivot
pub struct HashTable<G:Hash+Eq<G>|V> {
    buckets:[Option<TableNode<G|V>>];
    salt:u64;
    entries:u64;
}
```

在这个定义中，`K`和`V`是泛型参数。`K`必须实现`Hash`和`Eq<K>`这两个trait，`V`没有特定的要求。`HashTable`有三个字段：`buckets`、`salt`和`entries`。`buckets`是一个数组，它的元素类型是`Option<TableNode<K,V>>`，这是一个使用泛型参数`K`和`V`的类型。

## 2. 在方法中使用泛型：

```pivot
impl <K:Hash+Eq<K>|V> HashTable<K|V>  {
    pub fn insert(k:K,v:V) void {
        // ...
    }
}
```

在这个方法中，`K`和`V`是泛型参数。这个方法接受两个参数，一个是类型为`K`的键，一个是类型为`V`的值。

## 3. 在普通函数中使用泛型：

```pivot
pub fn new_hash_table<K:Hash+Eq<K>|V>(bucket_size:u64,salt:u64) HashTable<K|V> {
  // ...
}
```

在这个函数中，`K`和`V`是泛型参数。这个函数接受两个参数，一个是桶的大小，一个是盐值。它返回一个`HashTable<K,V>`类型的值。

## 4. 使用trait约束：

在Pivot Lang中，可以使用trait约束泛型参数。例如，`K:Hash+Eq<K>`表示`K`必须实现`Hash`和`Eq<K>`这两个trait。

泛型提供了代码重用的强大工具，使得可以编写一段代码来处理多种类型，而不是为每种类型都写一段几乎相同的代码。这不仅可以减少代码量，也可以提高代码的可读性和可维护性。
