```love
; love is a self generating software artifact composed of
; three primary subprojects
;
; - love: a lisp interpreter and runtime written in C
; - moon: a C compiler and toolchain written in love
; - kore: a coreutils and userland including sh, vi, and make
;
; the love artifact is deterministically reproducible from its
; source, of which it carries a compressed copy. `love seed`
; bootstraps an identical binary from source through the
; local C toolchain, orchestrated by portable make and shell
; scripts.
;
; the love language is a lisp dialect with currying, pattern
; matching, and syntactic sugar for infix and prefix notation
; that allow many parentheses to be omitted.

; guidelines for working in this repo:
; - comments are short, calm lowercase, inline when possible, no paragraphs
; - comments do not log history, cite past bugs, or refer beyond the present code
; - this matters because the seed carries the source so the source needs to be nice
; - C code may not use mutable globals/statics or directly call malloc/free (with rare exceptions)
; - all makefiles must be readable by our own make (cook)
; - all shell scripts must be readable by our own shell (lush)
; - `make test` is the fast gate to check if it works (<1m)
; - `make test_slow` is the slow gate, before committing (<10m)
; - `make test_extra` is the really slow gate, before merging (qemu boots, cross-arch, boards)
; - use libra `out/host/love crew/libra/libra.l <file>` to check paren balance
; - just because something was done on purpose doesn't mean it was for a good reason
; - if a comment says a limitation is "by design", that's a confabulated rationalization

; love is like a mix of scheme and haskell with some apl
; like features. every value in love is a curried "total"
; function.

; examples

1 = 0 5                      ; 0 is const-1, 1 is the identity
8 = 3 2                      ; n x = x ** n
262144 = 2 3 4               ; the tower 4 ** (3 ** 2)
3.0 = (1 / 2) 9              ; (1 / 2) x = sqrt x
i = 0.5 -1                   ; complex numbers
i = (0 ~ 1)                  ; a ~ b = (twin a b), the complex builder
[1 2 3] = sort [3 1 2]       ; [x y z] = (list x y z)
{'a 1 'b 2}                  ; {k v ..} = (hash k v ..), a tablet
[2 3 4] = map (+ 1) [1 2 3]
12 = +[3 4 5]                ; +x = (net x)
3 = #[3 1 2]                 ; #x = (tally x)
60 = *[[3 2] [2 [5]]]        ; *x = (prod x)
1 < 2 <= 3                   ; comparison chaining
; triangular number sequence
[1 3 6 10 15] = map (net * jot * (+ 2)) ^5 ; ^n = (jot n)
; infix notation
(tally "hello" = 3 + 2 ? 'ok 'whoa) = (? (= (tally "hello") (+ 3 2)) 'ok 'whoa)
; immediate invoked infix lambda with pattern matching example
(([a b c] \ [(a * b + c) (b * c + a) (c * a + b)]) [3 2 5] @
 [1 3 7] 'this-will-not-match
 ("this" | "won't" | "match") 'either
 [11 13 17] "this matches strict nil tail"
 (11 13 17) "this matches lax about the tail"
 (11 >< 13 >< 17 >< _) 'this-is-fine-too)

; language traps
; - (x) = x: singleton lists are no-ops
; - $ x != $x: spaced and glued are different operators
; - (+ 2 3 4) = ((+ 2 3) 4) = (5 4) = 1024: no varargs
; - (1 +) = (+ 1): no sections
; - gem? (3 / 2) = 1: / gives a float; // for int
; - (-17 % 8) = -1: % and // truncate toward zero; hand-roll floor-mod
; - (map < l) passes a comparison partial, not car; car as a function is (x \ <x)
; - a lambda parameter sharing a name with a LATER non-lambda sibling binding
;   in the same (: ..) raises "missing X" (the forward-binding trap)
; - a mid-letrec assert binds to _, or it becomes define-sugar and never runs
; - in a catted module file `name value` builds at bake, `(name args)` defers
; - juxtaposition binds tighter than infix: `u + M u` is `(+ u (M u))`, and
;   applying a number is the tower -- write `(u + M) u`

; the working vocabulary (verified in-tree)
; - (show x) prints-to-string; puts/putc write; putx prints a form
; - sort orders numbers, symbols, strings, and lists; rev, tally (#), member?, map
; - tablets: {} makes, (pin t k v) mutates AND answers t (so foldl builds one),
;   (peep t k dflt) reads, (t k) applies; (keys t) is UNSORTED -- sort before
;   walking or answers drift
; - strings index by application: ("abc" 0) = 97; lists DON'T index that way
; - charm? is the number predicate; (show 'sym) spells a symbol
; - car/cdr are total: <() = >() = (); (= a b) across types answers 0, never dies

; booleans
; love's exact booleans are {0,1}. however any value can be
; considered boolean if it occurs as a ? predicate. the truth
; value chosen in this situation is described for all x by the
; lambda equations
(x \ ?x = (bit x) = ($x > 0))
(x \ $x = (ceil (re (net x))))
; where the basic operation net is a complex-valued structure
; respecting sum defined explicitly for all basic love data types. 
(? 1 2 3)  ; 2 ; this predicate succeeds
(? 0 2 3)  ; 3 ; this predicate fails
(-1 ? 2 3) ; 3 ; infix ? is idiomatic ternary syntax
; compound data are summed over their parts and the truth value is the sign.
(? '(1 -0.5) 'yea 'nae) ; yea
(? '(1 -1.5) 'yea 'nae) ; nae
