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
; - keep comments tight. inline when possible. no paragraphs or ⚠ YELLING
; - `make test` is the fast gate to check if it works (<1m)
; - `make test_slow` is the slow gate, before committing (<10m)
; - `make test_extra` is the really slow gate, before merging (<25m)
; - use libra `out/host/love crew/libra/libra.l <file>` to check paren balance
; - just because something was done on purpose doesn't mean it was for a good reason

; love is like a mix of scheme and haskell with some apl
; like features. every value in love is a curried "total"
; function.

; examples

1 = 0 5                      ; 0 is const-1, 1 is the identity
8 = 3 2                      ; n x = x ** n
262144 = 2 3 4               ; the tower 4 ** (3 ** 2)
3.0 = (1 / 2) 9              ; (1 / 2) x = sqrt x
i = 0.5 -1                   ; complex numbers
[1 2 3] = sort [3 1 2]       ; [x y z] = (list x y z)
[2 3 4] = map (+ 1) [1 2 3]
12 = +[3 4 5]                ; +x = (net x)
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

; language traps based on assumptions from other languages
; - (x) = x: singleton lists are no-ops
; - $ x != $x: spaced and glued are different operators
; - (+ 2 3 4) = ((+ 2 3) 4) = (5 4) = 1024: no varargs
; - (1 +) = (+ 1): no sections

; booleans: in a conditional what values are true and false?
; love's rule is simple: positive is true, zero or negative
; is false.
(? 1 2 3)  ; 2
(? 0 2 3)  ; 3
(? -1 2 3) ; 3
; compound data are summed over their parts and the truth value is the sign.
(? '(1 -0.5) 'yea 'nae) ; yea
(? '(1 -1.5) 'yea 'nae) ; nae
; this equation shows different spellings of the truth bit of x
(x \ ?x = !!x = ($x != 0) = (re +x > 0))

