# Lambda Calculus工具集

本项目专注于实现一套Lambda Calculus工具，这些工具主要包括：
- Lambda Calculus Beta-规约器
- 传统Lambda项转De Bruijn Lambda项工具
- Krivine Machine
- 支持扩展语法的Lambda项文本解析器

## Lambda项文本解析器

本解析器支持下列语法与特性：

### Lambda项命名

```
N_2 = \f x. f (f x);
pow2 = \x. x N_2
main = pow2 (λf x. f (f (f x)));
```

### Let-Expression

```
main = let a = b; in c a;
{-
等价于：
main = (λa. c a) b;
-}
```
### Do-Expression

```
main = do {
    a b <- c;
    d < e f;
    g
};
{-
等价于：
main = c (\a b. (e f (\d. g)));
-}
```

### 注释：

```
{-
这是一段注释，支持换行。
-}
```

由于Lambda Calculus本身具有强大的表达能力，因此添加上述扩展语法后能够更加方便地将Lambda Calculus作为一门编程语言来编写程序。

可以在lam与lam-examples目录下找到更多内容。