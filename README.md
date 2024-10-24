# gwen lisp

gwen is a lisp dialect. notable features include

- lexical scope (like scheme)
- all functions curried (like haskell)
- no variadic functions (can be simulated with macros)
- generalized apply procedure (interprets data as constant functions)
- nil = 0

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

