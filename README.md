# gwen lisp

gwen is a like simplified scheme with less words and parentheses,
however unlike scheme and other lisp dialects, every value acts
as a one argument function. for functions this is done by currying
like in haskell. other objects are interpreted as their own constant
functions. this expands the range of evaluable expressions and
simplifies the eval function.

## 

variable scope is similar to scheme. there are five special forms
which have similar scheme counterparts.

|  scheme  | gwen |
|----------|------|
| `begin`  | `,`  |
| `cond`   | `?`  |
| `let*`   | `:`  |
| `quote`  | <code>&#96;</code> |
| `lambda` | <code>&#92;</code> |

the internal syntax of some forms is simplified compared to scheme.

|  scheme  | gwen |
|----------|------|
| `(cond (a b) (c d) (#t e))`   | `(? a b c d e)`  |
| `(let* ((a b) (c d)) e)`   | `(: a b c d e)`  |
| `(lambda (a b) c)` | <code>(&#92; a b c)</code> |

in conditionals only one value is false, `0`. `()` and `#f` are not distinct values.

## example program

```lisp
; calculating with church numerals
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
   (assert (= 420
    (mul two (mul five (mul six seven)) (+ 1) 0))))
```
