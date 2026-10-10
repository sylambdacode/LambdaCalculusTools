# Lambda Calculus Tools

This project is focused on the implementation of a suite of tools for Lambda Calculus, comprising:

- A Lambda Calculus Beta-reduction tool
- A tool for converting Traditional Lambda terms to De Bruijn Lambda terms
- Support for type checking of Simple Typed Lambda Calculus expressions
- Support for type checking of Lambda Cube expressions, with support for custom rules
- Krivine Machine and Lazy Krivine Machine
- A text parser for Lambda terms supporting extended syntax
- A simple programming language (doesn't support garbage collection)

## The basic usage

Reduce Lambda terms:

```
lambda-calculus-tools-main calculate -t type1 -f main1 lam-examples/reducation.lam
```

Reduce Lambda terms(extended mode):

```
lambda-calculus-tools-main calculate -t type2 -f main2 lam-examples/reducation.lam
```

By using argument `-t type2`, some extended functions can be utilized.

Run Krivine Machine:

```
lambda-calculus-tools-main runKrivineMachine lam-examples/hello-world.lam lam-lib/*.lam
```

Run a simple programming language through the implementation of extended lambda calculus:

Looping to print Hello World:

```
lambda-calculus-tools-main simplelang lam-examples/helloworld-loop.lam
```

Guess the Number Game:

```
lambda-calculus-tools-main simplelang lam-examples/what-the-number-is.lam
```

## A text parser for Lambda terms

This parser supports the following syntax and features:

### Lambda term naming

```
N_2 = ^f x. f (f x);
pow2 = ^x. x N_2
main = pow2 (λf x. f (f (f x)));
```

This syntax is used to simplify the writing of Lambda expressions, enhancing readability.

It is a literal naming approach that will be recombined into a single expression after the expression parsing is completed.

For example：

```
A = a;
B = b;
C = A B;
```

After the expression is parsed, `C` is equivalent to `a b`.

(This is a compile-time feature)

### Let-Expression

```
main = let a = b; in c a;
{-
This is equivalent to:
main = (λa. c a) b;
-}
```

(This is a compile-time feature)

### CPS-Expression

```
main = cps {
    v1 v2 <- f a;
    v3 <- g v1 v2;
    v3
};
{-
This is equivalent to:
main = f a (λv1 v2. (g v1 v2 (λv3. v3)));
-}
```

(This is a compile-time feature)

### Comments：

```
{-
This is a comment, supporting line breaks.
-}
```

(This is a compile-time feature)

## More content:


Given the expressive power of Lambda Calculus, the addition of the extended syntax makes it more convenient to use Lambda Calculus as a programming language. 

More content can be found in the `lam-lib` and `lam-examples` directories.

The `lam-lib` directory contains some general Lambda functions.

The `lam-examples` directory includes some example code.

---

English: README.md

中文: README-ZH.md
