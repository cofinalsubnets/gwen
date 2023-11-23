# gwen
another list processing style programming language

## compared to other lisps
- no false or nil, only 0
- no dotted lists, the last cdr is not shown
- functions are curried, there are no variadic functions (but macros receive list arguments)
- application is left to right one argument at a time
- data are own constant functions
- singleton list is `quote`, there are no nullary functions, just pass 0 and ignore it :)
- `(, a b c d)` is like `begin`
- `(\ a b c x)` is `lambda` (`a b c` are arguments and `x` is body, use `,` for multiple expressions)
- `(? a b c d e f g)` is `cond` (g is default branch)
- `(: a x b y c z (a b c))` is like `let*` (if no expression then return last definition)

## common functions
- `cons` is `X`, `car` is `A`, `cdr` is `B`
- `.` prints a value and returns it

## code examples
### church numerals
```lisp
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
   (. (mul two (mul five (mul six seven)) (+ 1) 0)))
```

### a quine
`L` is the list macro
```lisp
((\ l (L l (L l))) '(\ l (L l (L l))))
```
