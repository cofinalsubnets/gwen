# lisa
LISt Action

## build / install
are you on linux? `make` will probably work. otherwise, see
`Makefile` for the suggested compiler flags.

## usage

lisa is alive, so unstable. usage is schemelike, but
relaxed. `#f` = `()` = `0`. special forms have short
names and use less parentheses. pairs are immutable and
eagerly cons'd, so acyclic. improper list literals are
absent.

special form examples compared to scheme:

| scheme                     | lisa        |
|----------------------------|-------------|
|`(begin a b)`               |`(, a b)`    |
|`(lambda - #f)`             |`(\)`        |
|`(lambda - a)`              |`(\ a)`      |
|`(lambda (a) b)`            |`(\ a b)`    |
|`(lambda (a b) c)`          |`(\ a b c)`  |
|`(lambda (a . b) c)`        |`(\ a b . c)`|
|`(begin (define a b) a)`    |`(: a b)`    |
|`(begin (define a b) c)`    |`(: a b c)`  |
|`(cond (a b) (#t #f))`      |`(? a b)`    |
|`(cond (a b) (#t c))`       |`(? a b c)`  |
|`(cond (a b) (c d) (#t #f))`|`(? a b c d)`|

etc.

## code examples

### an autograph
```lisp
; also called a quine
((\ - (L - (L ` -))) '(\ - (L - (L ` -))))
```

### hyperoperation sequence
```lisp
(: (hy x n y) ( ? (~ n) (+ x y) (~ y) 1
 (hy x (- n 1) (hy x n (- y 1)))))
```

### church numerals

church numerals display natural numbers as elements of an abelian
monoid `N = 0 + succ N`. hyperoperations appear as complexity
classes of self-actions of `N` via `M`, the monoid of endomorphisms on
`N` generated by `succ`. by construction, `M` is `N`'s image under
`succ`, and forms a second abelian monoid under `1 = succ 0`. the
fundamental theorem of arithmetic maps `N` onto `M` through the
factorizing homomorphism `f` whose kernel is `{0, 1}`.

this paints a dramatic picture. the polymorphism `f` floods `N`; the
receding waters reveal `M`, in privation of its former identity, yet
holding a second within its nature, as if by chance. a rainbow appears:
a bridge back to the origin made out of `M`'s infinite generators. the
bridge is endless, and the toll to pass is unlimited attention paid to
the image we wish to correct.

as an attempt to formalize this intuition, imagine a "point at infinity"
in `N ^ N` representing a quantity congruent to 0 mod all `N`. by
conjecture, each `p` is now the neutral element of a new monoid
`Np = p + Np ^ z`.  since by construction `a ^ b = b a`, this means `p`
and `z` assume the place of 0 and `succ` in the original equation for `N`.
the impression received is that primes are significant among integers
because of a property they share with 0.

church numerals reflect the identity of natural numbers as completed
degrees of repetition. this appears geometrically in euler's identity,
where they form the solutions `n` of `1 = e ^ n * i * pi`, where the value of
pi is taken to be twice that normally given. this formula implicitly
describes a function that sends `n` to its exponential with respect to
`i * pi` and is one whenever `n` is natural; this is the property used
to construct zero below.

```lisp
(:
 ; zero is the constant function at the identity function
 zero (\ (\ - -))
 ; a successor applies its argument then yields to its predecessor
 (((succ f) g) h) ((f g) (g h))

 one (succ zero) ; \ f -> f = identity
 two (succ one) ; \ f -> f . f = square
 three (succ two) ; \ f -> f . f . f = cube 

 ; binary operations follow from succ:
 ((add g) f)         ; the monoid on N
  ((f succ) g)       ; \ g f x -> f x . g x
 ((mul g) f)         ; the monoid on End(N)
  ((f (add g)) zero) ; \ g f x -> f (g x)

 ; the rest are iterations of the "up arrow" map:
 (((up op) g) f) ((f (op g)) one)
 ; these operations fail to be abelian or even associative,
 ; which makes the compatibility of the first two seem
 ; special and miraculous.
 pow (up mul) ; exponentiation ; \ f -> f = one ; this also feels significant
 tet (up pow) ; tetration, etc.

 (C n) (? n (succ (C (- n 1))) zero) ; fixnum -> N
 (N c) ((c (\ x (+ x 1))) 0))        ; N -> fixnum
```

### fizzbuzz

```lisp
(: (/p m n) (~ (% m n))
   (fizzbuzz m)
    ((\ (+ m 1)) (. (?
     (/p m 15) 'fizzbuzz
     (/p m 5)  'buzz
     (/p m 3)  'fizz
     m)))
 ((((pow ((mul ((add three) two)) two)) two) fizzbuzz) 1))
```
