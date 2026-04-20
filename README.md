# gwen lisp

Gwen lisp is a lisp dialect and environment that can be built as a library,
an executable, or for bare metal on various platforms.

In Gwen lisp every value is a one argument function.  There are four special forms:

| gwen               |  scheme equivalent |
|--------------------|----------|
| <code>&#96;</code> | `quote`  |
| `?`                | `cond`   |
| `:`                | `let`    |
| <code>&#92;</code> | `lambda` |

## &#96; (quote)

| gwen  | scheme |
|-------|--------|
| <code>(&#96; x)</code> | `(quote x)` |


- like `quote` in other lisp
- <code>(&#96; x) = 'x</code>

## ? (cond)

| gwen            | scheme                      |
|-----------------|-----------------------------|
| `(? a b c d e)` | `(cond (a b) (c d) (#t e))` |

- like `cond` in other lisp
- `0` is the sole false value
- `(? 0 a b) = b`
- `(? x a b) = a ; x != 0`
- `(? x a) = (? x a 0)`
- `(? x) = x`

## : (let)

| gwen            | scheme                      |
|-----------------|-----------------------------|
| `(: a b c d e)` | `(let  ((a b) (c d)) e)` |

- like `let` in scheme (but see below)
- `(: a) = a`
- `(: a b) = (: a b a) ; at toplevel this is also a global definition`
- `(: (a f b) (f b)) = (: a (\ f b (f b))) ; syntact sugar for lambdas`
- `(: a 1 a (+ 1 a) a) = 2 ; variable shadowing works`
- recursive functions are supported like `letrec` in scheme
- evaluation is sequential like `let*` in scheme
- assignments are immutable (some data are mutable)
- also used for sequencing `(: _ (do this) _ (then do that) (final value))`

## &#92; (lambda)

| gwen                           | scheme                 |
|--------------------------------|------------------------|
| <code>(&#92; a b c d e)</code> | `(lambda (a b c d) e)` |

- like `lambda` in other lisp, but multiple arguments and one body expression, not one argument list and multiple body expressions
- `(\ x) = x`
- use `:` for sequencing multiple expressions in one function body

## evaluation

Gwen lisp has Scheme-like lexical scope and a single namespace for functions and values, and uses four
special forms with easy Scheme equivalents.  However, the evaluation procedure is different, more similar to Haskell,
though Gwen lisp is dynamically typed. Functions are curried, and data implicitly act as their own constant functions.
Therefore in Gwen lisp every value is a one-argument function. Lists are evaluated by left-to-right application:

- `(f x y z) = (((f x) y) z) = (foldl f id (list x y z)) ; modulo side effects`

However, this is only a conceptual description. Internally Gwen lisp may use different evaluation order for optimization
reasons. Therefore if you need specific evaluation order for function arguments you should use the sequencing form `:`

```
(: y (f 0) ; eval second argument first
   x (g 0) ; eval first argument second
 (c x y))
```

A singleton list has the value of its element, rather than representing a 0-argument function call like in other lisps.

- `(f) = f`

Nullary and variadic functions aren't distinct features in Gwen lisp, but can be replicated with unary functions.
For nullary functions, just pass an argument and ignore it. Conventionally `0` or `()` is used for this purpose in code.
Variadic functions can either be simulated with macros, or written using sentinels.

## code examples

### variadic function using a sentinel

```
(: end (sym 0)
   (li k x) (? (= end x) (k 0) (li (\ z (k (cons x z)))))
   lis (li id)
 (lis 1 2 3 4 5 end)) ; = '(1 2 3 4 5)
```

### church numerals

```
(: (add a b f x) (a f (b f x))
   (mul a b f) (a (b f))
   (zero a b) b
   one (zero zero)
   two (add one one)
   three (add one two)
   four (add two two)
   five (add two three)
   six (mul two three)
   seven (add one six)
 (assert (= 420 (mul two (mul five (mul six seven)) (+ 1) 0))))
```

