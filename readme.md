# lisa
LISt Action

## build / install
are you on linux? `make` will probably work. otherwise, see
`Makefile` for the suggested compiler flags.

## syntax

lisa is highly unstable.

special forms compared to scheme:

| scheme                     |             |
|----------------------------|-------------|
|`(begin a b)`               |`(, a b)`    |
|`(lambda _ #f)`             |`(\)`        |
|`(lambda _ x)`              |`(\ x)`      |
|`(lambda (a b) c)`          |`(\ a b c)`  |
|`(begin (define a b) a)`    |`(: a b)`    |
|`(letrec ((a b)) c)`        |`(: a b c)`  |
|`(cond (a b) (#t #f))`      |`(? a b)`    |
|`(cond (a b) (#t c))`       |`(? a b c)`  |
|`(cond (a b) (c d) (#t #f))`|`(? a b c d)`|
|`'x`                        |`'x`         |

etc.

other things:

- `()` = `#f` = `0`
- numbers may take a C-style radix in `{b=2,o=8,d=10,z=12,x=16}`
- strings delimited by `"` which can be escaped with a backslash

## code examples

### a quine
```lisp
((\ - (L - (L ` -))) '(\ - (L - (L ` -))))
```

### hyperoperation sequence
```lisp
(: (hy x n y) ( ? (~ n) (+ x y) (~ y) 1
 (hy x (- n 1) (hy x n (- y 1)))))
```

### church numerals

church numerals represent natural numbers as elements of an abelian
monoid `N = zero + succ N`. hyperoperations then appear as complexity
classes of self-actions of `N` by `M`, the monoid of endomorphisms of
`N` generated by `succ`. due to the church encoding of `N`, `M` is the
image of `N` under `succ`, and forms a second abelian monoid with
identity `one = succ zero`. the fundamental theorem of arithmetic
relates `N -> M` by factorization and `M -> N` by summation: the
surjective homomorphism `f: N -> M` fails to be injective at `one`,
reflecting that `one` and `zero` both have an empty factorization;
dually `g: M -> N` is an injection that fails to be surjective at
`zero`, reflecting that `zero` has no predecessor in `N`.

this paints a dramatic picture. the polymorphism `f` floods `N`: the
water recedes to reveal `M`, minus its former identity, supplied
by nature with a ready second. a rainbow appears: a bridge back onto
`N` made of `M`'s generators. the bridge is endless, and the toll to
cross is limitless attention paid to the remnant one wishes to mend.

```lisp
(:
 ; zero is the constant function at identity
 zero (\ (\ - -))
 ; a successor applies f then yields to its predecessor
 (((succ f) g) h) ((f g) (g h))

 one (succ zero) ; \ f -> f = identity
 two (succ one) ; \ f -> f . f = square
 three (succ two) ; \ f -> f . f . f = cube 

 ; other operations follow from succ.
 ; monoid operation on N
 ((add g) f) ((f succ) g) ; \ g f x -> f x . g x
 ; monoid operation on End(N)
 ((mul g) f) ((f (add g)) zero) ; \ g f x -> f (g x)

 ; the rest are iterations of the "up arrow" map
 (((up op) g) f) ((f (op g)) one)
 pow (up mul) ; exponentiation ; = \ f -> f = one
 tet (up pow) ; tetration, etc.

 (C n) (? (= n 0) zero (succ (C (- n 1)))) ; ℕ->⛪
 (N c) ((c (\ x (+ x 1))) 0))              ; ⛪->ℕ

(: (/p m n) (= 0 (% m n))
   (fizzbuzz m)
    ((\ (+ m 1)) (. (?
     (/p m 15) 'fizzbuzz
     (/p m 5)  'buzz
     (/p m 3)  'fizz
     m)))
 ((((pow ((mul ((add three) two)) two)) two) fizzbuzz) 1))
```
