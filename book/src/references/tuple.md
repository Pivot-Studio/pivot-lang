
# Pivot Lang 元组语法和功能

在 Pivot Lang 中，元组是一种可以包含不同类型元素的数据类型。元组使用圆括号`()`包围元素，元素之间使用逗号`,`分隔。元组的元素可以通过`.`后跟索引的方式访问。

以下是一些使用元组的例子：

## 1. 定义一个空元组：

```pivot
let d = ();
```

## 2. 定义一个只有一个元素的元组：

```pivot
let a = (1,);
```

注意，只有一个元素的元组需要在元素后面加上逗号`,`，否则它会被认为是一个普通的值，而不是元组。

## 3. 定义一个包含多个元素的元组：

```pivot
let b = (1, 2);
```

## 4. 定义一个包含元组的元组：

```pivot
let c = (1, 2, (3,));
```

5. 访问元组的元素：

```pivot
let e = a.0;
let f = c.2;
```

在这些例子中，`a.0`表示元组`a`的第一个元素，`c.2`表示元组`c`的第三个元素。