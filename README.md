# gwen lisp

Gwen lisp is a lisp dialect and environment that can be built as a library,
an executable, or for bare metal on various platforms. In Gwen lisp:

- every expression has a value (there are no runtime errors)
- every value is a function (data are their own constant functions)
- every function is unary (via currying)

There are four special forms:

| gwen               |  scheme equivalent |
|--------------------|----------|
| `?`                | `cond`   |
| `:`                | `let`    |
| <code>&#96;</code> | `quote`  |
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
- `(: (a f b) (f b)) = (: a (\ f b (f b)))`
- `(: a 1 a (+ 1 a) a) = 2`
- recursive functions are supported like `letrec` in scheme
- evaluation is sequential like `let*` in scheme
- `:` sequencing idiom: `(: _ (do 1) _ (do 2) (do 3))`

## &#92; (lambda)

| gwen                           | scheme                 |
|--------------------------------|------------------------|
| <code>(&#92; a b c d e)</code> | `(lambda (a b c d) e)` |

- like `lambda` in other lisp, but multiple arguments with one body expression, not one argument list with multiple body expressions.
- `(\ x) = x`
- use `:` for sequencing multiple expressions in one function body.

## evaluation

Gwen lisp is Scheme-like with lexical scope and a single namespace for functions and values, and it uses four
special forms with easy Scheme equivalents.  However, its evaluation procedure is different, similar to Haskell,
though Gwen lisp is dynamically typed. Functions are curried, and data implicitly act as their own constant functions.
Therefore in Gwen lisp every value is a one-argument function and lists are evaluated by left-to-right application, or
equivalently a left fold by the identity function..

- `(f) = f`
- `(f x y z) = (((f x) y) z) = (ap f (list x y z)) = (foldl f id (list x y z))`

However, this is only a conceptual description; in reality Gwen lisp may use different evaluation order for optimization
reasons. Therefore if you need specific evaluation order for function arguments you must use the sequencing form `(: a (f 0) b (g 0) (c a b))`

Nullary and variadic functions used in other languages can be replicated in Gwen lisp. For nullary functions, simply pass
an argument and ignore it. Conventionally `0` or `()` is used for this purpose in code. Variadic functions may either be
simulated with macros, or written using sentinels.


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


