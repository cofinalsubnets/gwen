# 🌑 love

love is a self-reproducing software artifact composed of several related parts:

- love: a programming language and runtime environment written in C
- moon: a cross-platform C compiler/assembler/linker/libc written in love
- kore: a unix coreutils and userland including sh, vi, tar and make

love folds these components along with its own source code into a single-binary
interpreter, toolchain, and userland.

- <code>love source</code> extract the bundled source
- <code>love seed</code> bootstrap a verified identical binary

## language

the love programming language is a dynamically typed lisp superset with syntactic
extensions mimicking features of haskell and apl, allowing the majority of parens to
be omitted. features:
- three special forms: `\ : ?` lambda let cond; quote is a special case of lambda
- every value is a function and every function is curried
- every function is total unless you loop
- infix/prefix notation (desugars to lisp)
- pattern matching (implemented as a macro)


### booleans

love's rule for deciding truth value of rich data in conditionals is principled:
sum the components of the datum x into a real number n; x is then true iff n is positive.
this mostly agrees with lax rules like javascript and python that treat empty values as
false. unlike other languages, love also considers negative values false.

though any value in love can be considered boolean by using it as a test in a conditional,
comparisons and predicates (such as =) return specifically {0,1}. here are some expressions
that evaluate to 1:

```
1 = 0 5                      ; 0 is const-1, 1 is the identity
8 = 3 2                      ; n x = x ** n
262144 = 2 3 4               ; the tower 4 ** (3 ** 2)
3.0 = (1 / 2) 9              ; (1 / 2) x = sqrt x
i = 0.5 -1                   ; built in complex
[1 2 3] = sort [3 1 2]
[2 3 4] = map (+ 1) [1 2 3]
12 = +[3 4 5]
60 = *[[3 2] [2 [5]]]
1 < 2 <= 3
[1 3 6 10 15] = map (net * jot * (+ 2)) ^5
```
### hello world

```
."hello world\n"
```

### fizzbuzz

```
(100
 (\ n (: f (n % 3 = 0 ? "fizz")
         b (n % 5 = 0 ? "buzz")
       (puts $ (f || b ? (f + b) (show n)) + "\n",
        n + 1)))
 1)
```

### license

love is free 🕊

[0BSD](LICENSE)
[NOTICE](NOTICE)
