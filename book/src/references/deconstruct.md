# Deconstruct

解构指的是用类似构造复杂类型的语法获取复杂类型中的部分值。一般来说，解构语法会用于结构体和元组。

## Table of Contents

<!-- toc -->



## 元组解构

```pl
let (a, b, c) = (1, 2, 3);
(a, b, c): (i32, i32, i32) = (1, 2, 3);
(a) = (1,);
```


```admonish warning
元组解构的时候目标元素个数必须与源元素个数相同，否则会报错。

```

```pl
let (a, b, c) = (1, 2); // 出错！
(a) = (1, 2); // 出错！
```

## 结构体解构

结构体解构与元组解构类似，只是解构的时候需要指定字段名。

```pl
struct Point {
    x: i32;
    y: i32;
}

let { x:xx, y:yy } = Point { x: 1, y: 2 };
```

```admonish tip
结构体解构的时候，如果字段名与变量名相同，可以省略字段名。

```

```pl
struct Point {
    x: i32;
    y: i32;
}

let { x, y } = Point { x: 1, y: 2 }; // 省略字段名
let yy:i32;
{ x, y:yy } = Point { x: 1, y: 2 };
```

对于结构体解构，你不需要像元组一样解构所有的字段。

```pl
struct Point {
    x: i32;
    y: i32;
}

let { x } = Point { x: 1, y: 2 }; // 只解构 x 字段
```

## 嵌套解构

解构语法可以嵌套使用。

```pl
struct Point {
    x: i32;
    y: i32;
}

struct Line {
    start: Point;
    end: Point;
}

let { start: { x:xx, y:yy }, end: { x:xx2, y:yy2 } } = Line { start: Point { x: 1, y: 2 }, end: Point { x: 3, y: 4 } };
```

元组和结构体也可以相互嵌套

```pl

struct Point {
    x: i32;
    y: i32;
}

struct Line {
    start: Point;
    end: Point;
}

let (a, b, { start }) = (1, 2, Line { start: Point { x: 1, y: 2 }, end: Point { x: 3, y: 4 } });

```

