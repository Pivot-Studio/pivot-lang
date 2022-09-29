# internal macro

内部使用的proc macro库  


## 内容

- [add_symbol_macro](src/add_symbol_macro) 用于导出rust函数给pivot-lang使用的宏（jit或静态编译皆可）
- [range_macro](src/range_macro/) 用于生成range的宏，用于帮助ast映射源码位置
- [test_parser_macro](src/test_parser_macro/) 用于测试parser的宏
