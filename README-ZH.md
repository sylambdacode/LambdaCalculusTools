# Lambda Calculus工具集

本项目专注于实现一套Lambda Calculus工具，这些工具主要包括：
- Lambda Calculus Beta-规约器
- 传统Lambda项转De Bruijn Lambda项工具
- 支持对简单类型Lambda演算表达式进行类型检查
- 支持对Lambda Cube表达式进行类型检查，支持自定义规则
- Krivine Machine与Lazy Krivine Machine
- 支持扩展语法的Lambda项文本解析器
- 简单的编程语言（不支持垃圾回收）

## 基本使用方法

规约Lambda表达式：

```
lambda-calculus-tools-main calculate -t type1 -f main1 lam-examples/reducation.lam
```

规约Lambda表达式（扩展模式）：

```
lambda-calculus-tools-main calculate -t type2 -f main2 lam-examples/reducation.lam
```

使用`-t type2`参数能够使用一些扩展函数。

运行Lazy Krivine Machine：

```
lambda-calculus-tools-main runKrivineMachine lam-examples/hello-world.lam lam-lib/*.lam
```

运行通过扩展Lambda演算而实现的的简单编程语言：

打印100次Hello World

```
lambda-calculus-tools-main simplelang lam-examples/helloworld100times.lam
```

猜数字小游戏

```
lambda-calculus-tools-main simplelang lam-examples/what-the-number-is.lam
```

## Lambda项文本解析器

本解析器支持下列语法与特性：

### Lambda项命名

```
N_2 = \f x. f (f x);
pow2 = \x. x N_2
main = pow2 (λf x. f (f (f x)));
```

这种命名主要用于简化Lambda表达式的编写，增强表达式可读性，是一种字面量意义上的命名，会在表达式解析完成后重新组合为单个表达式。

例如：

```
A = a;
B = b;
C = A B;
```

表达式解析完成后，`C`相当于`a b`。

（这是一个编译期特性）

### Let-Expression

```
main = let a = b; in c a;
{-
等价于：
main = (λa. c a) b;
-}
```

（这是一个编译期特性）

### CPS-Expression

```
main = cps {
    v1 v2 <- f a;
    v3 <- g v1 v2;
    v3
};
{-
等价于：
main = f a (λv1 v2. (g v1 v2 (λv3. v3)));
-}
```

（这是一个编译期特性）

### 注释：

```
{-
这是一段注释，支持换行。
-}
```

（这是一个编译期特性）

## 更多内容

由于Lambda Calculus本身具有强大的表达能力，因此添加上述扩展语法后能够更加方便地将Lambda Calculus作为一门编程语言来编写程序。

可以在lam与lam-examples目录下找到更多内容。
