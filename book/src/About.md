# Pivot Lang

[![codecov](https://codecov.io/gh/Pivot-Studio/pivot-lang/branch/master/graph/badge.svg?token=CA17PWK0EG)](https://codecov.io/gh/Pivot-Studio/pivot-lang)
[![release](https://github.com/Pivot-Studio/pivot-lang/actions/workflows/release.yml/badge.svg)](https://github.com/Pivot-Studio/pivot-lang/actions/workflows/release.yml)
[![test](https://github.com/Pivot-Studio/pivot-lang/actions/workflows/test.yml/badge.svg)](https://github.com/Pivot-Studio/pivot-lang/actions/workflows/test.yml)

![codecov](https://codecov.io/gh/Pivot-Studio/pivot-lang/branch/master/graphs/sunburst.svg?token=CA17PWK0EG)

此项目目前处于早期开发阶段，不建议用于生产环境。  
[项目地址](https://github.com/Pivot-Studio/pivot-lang)  

## 安装

见[此处](https://pivotlang.tech/docs/tutorial/installation.html)

## 官网

[https://pivotlang.tech](https://pivotlang.tech)

## CONTRIBUTING

中文见[此处](./CONTRIBUTING-CN.md)  
欢迎加入[qq群](http://qm.qq.com/cgi-bin/qm/qr?_wv=1027&k=vjKI1nbRHAIz1UbmDOjttLurEw93mLhA&authKey=U6cUmnIxiptTskr9trZZ9vc2p291uWht8TlzPSOEPXliihLC9vAYMaRwDI0%2FolR8&noverify=0&group_code=688301255)

## dependencies

- [llvm-18](https://github.com/llvm/llvm-project/releases/tag/llvmorg-18.1.2)
- [rust](https://www.rust-lang.org/)

**重要**：如果你想参与开发，请先在项目目录`make vm install`，然后根据自己是linux还是mac运行`make devlinux`或者`make devmac`

## 特点

- 支持静态编译与JIT
- REPL
- 热重载
- 极其方便的Rust互操作
- 支持debug
- 支持lsp，自带[vsc插件](https://github.com/Pivot-Studio/pivot-lang-support)，能提供优秀的代码支持
- 有GC，自动管理内存
- 强大的类型推断，支持省略大部分类型标注

## 一些ShowCase

### Hello World

```pivot
fn main() i64 {
    println!("hello world!");
    return 0;
}
```

### HashTable

（没错，我们的哈希表源码也是完全用Pivot Lang写的）

```pivot
use std::cols::hashtable;
use core::panic::assert;
use core::eq::*;

fn main() i64 {
    let table = hashtable::new_hash_table<string|string>(10 as u64, 1 as u64);
    table.insert("hello","world");
    table.insert("bye","bye");
    assert(table.get("hello") is string);
    let v = table.get("hello") as string!;
    assert("world".eq(&v));
    return 0;
}

```

### Fibonacci

```pivot
use std::io;
fn main() i64 {
    let result = fib(10);
    println!(result);
    return 0;
}

fn fib(n: i64) i64 {
    let pre = 0;
    let nxt = 0;
    let result = 1;
    for let i = 0; i < n; i = i + 1 {
        result = result + pre;
        pre = nxt;
        nxt = result;
    }
    return result;
}

```

### Y组合子

```pivot
use core::panic;
pub fn main() i64 {
    let g = |f: |i64| => i64, x: i64| => i64 {
        if x == 0 {
            return 1;
        }
        return x * f(x - 1);
    };
    let fact = Y(g);
    for let x = 0; x < 10; x = x + 1 {
        panic::assert(fact(x) == fact_recursion(x));
    }
    return 0;
}

struct Func<A|F> {
    f: |Func<A|F>, A| => F;
}

impl<A|F> Func<A|F> {
    fn call(f: Func<A|F>, x: A) F {
        return self.f(f, x);
    }

}

fn Y<A|R>(g: ||A| => R, A| => R) |A| => R {
    return |x: A| => R {
        let in = |f: Func<A|R>, x: A| => R {
            return f.call(f, x);
        };
        let field = |f: Func<A|R>, x: A| => R {
            return g(|x: A| => R {
                return f.call(f, x);
            }, x);
        };
        let f = Func{
            f: field
        };
        return in(f, x);
    };
}

fn fact_recursion(x: i64) i64 {
    if x == 0 {
        return 1;
    }
    return x * fact_recursion(x - 1);
}



```
