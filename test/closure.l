; test/closure.l -- recursive bindings in `:` (l's letrec*).
;
; `:` is a sequential, letrec*-like scope: a binding may refer to earlier
; bindings and to itself. Recursive *functions* (a `\` literal or the
; `(name args) body` defun sugar) resolve lazily -- the name is bound before any
; call, so self/forward references resolve per call (lvm_lazyb).
;
; Recursive *VALUES* are the hard case: a binding whose eagerly-evaluated init
; is (or builds) a closure capturing the very name being defined, or a
; forward/mutual sibling. l closures capture free vars BY VALUE at creation,
; so such a closure used to snapshot the name while it was still unassigned (0)
; and the self-call would dispatch on 0. ev.l's `boxfix` pass now fixes this:
; it indirects exactly those names through a heap cell -- letrec*'s "capture by
; location" -- while leaving everything else (functions, values defined before
; their users, names captured only as lambda params) untouched. A value is boxed
; when a def at index <= its own reads it, and also when one at a LOWER index
; reaches it through the let's other names -- an earlier closure inherits a later
; sibling's captures, so it wants the slot before the slot exists.

(:
 ; recursive function: recursion via the lazily-resolved name
 (nfac n) (? (< n 1) 1 (* n (nfac (- n 1))))
 ; the eta-expanded factory idiom (a *function* binding) -- still works
 (mkconst z) (\ n (? (< n 1) z (kconst (- n 1))))
 kconst (mkconst 7)
 ; memoizing fixpoint combinator (used for the recursive-value fib below)
 (memo f) (: m (tablet 0) (\ x (: c (peep m x 0) (? c c (: y (f x) _ (pin m x y) y)))))
 (assert
  ; --- recursive functions (unchanged behaviour) ---
  (= 120 (nfac 5))                 ; named recursion
  (= 7 (kconst 0))                 ; eta-expanded factory, base case
  (= 7 (kconst 5))                 ; eta-expanded factory, re-entrant

  ; --- recursive VALUE: a factory closure captures the name being defined ---
  (= 7   (: (mk z) (\ n (? (< n 1) z (self (- n 1)))) self (mk 7) (self 0)))
  (= 7   (: (mk z) (\ n (? (< n 1) z (self (- n 1)))) self (mk 7) (self 1)))
  (= 7   (: (mk z) (\ n (? (< n 1) z (self (- n 1)))) self (mk 7) (self 5)))
  ; same shape, a captured factorial
  (= 120 (: (mk z) (\ n (? (< n 1) 1 (* n (cf (- n 1))))) cf (mk 0) (cf 5)))

  ; --- forward value reference captured by an EARLIER function binding ---
  (= 15  (: (f x) (+ v x) v 5 (f 10)))

  ; --- mutual recursion among value bindings ---
  (= 9   (: (mka z) (\ n (? (< n 1) z (kb (- n 1))))
            (mkb z) (\ n (? (< n 1) z (ka (- n 1))))
            ka (mka 7) kb (mkb 9) (ka 1)))

  ; --- memoized fib: fib is a recursive value (its init closes over fib) ---
  (= 55   (: fib (memo (\ n (? (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))) (fib 10)))
  (= 6765 (: fib (memo (\ n (? (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))) (fib 20)))

  ; --- shadowing: an inner lambda rebinds the boxed name; left untouched ---
  (= 7   (: (mk z) (\ n (? (< n 1) z (self (- n 1)))) self (mk 7) g (\ self self) (g (self 1))))

  ; --- a value defined BEFORE the function using it needs no box, stays correct ---
  (= 6   (: x 5 f (\ n (+ x n)) (f 1)))

  ; --- `_` is a binder like any other, and repeats: each pin is its own slot, nothing
  ; reads them, and a bare literal body still answers ---
  (= 0 (: _ 1 _ 2 0))
  (= 7 (: _ 1 _ 2 _ 3 7))

  ; --- box cells must not shift an EARLIER ref: `a` reads an outer binding
  ; before a later init's forward capture forces `self` to be boxed; the cell
  ; sits below `a` at runtime, so `a`'s captured slot must already count it ---
  (= 108 (: outer 100
            (: a (+ outer 1)
               (mk z) (\ n (? (< n 1) z (self (- n 1))))
               self (mk 7)
               (+ a (self 3)))))
  ; same, two cells (mutual) -- the early ref must survive BOTH box cells ---
  (= 110 (: outer 100
            (: a (+ outer 1)
               (mka z) (\ n (? (< n 1) z (kb (- n 1))))
               (mkb z) (\ n (? (< n 1) z (ka (- n 1))))
               ka (mka 7) kb (mkb 9)
               (+ a (ka 1)))))
  ; nested boxing let: an init reaches a SIBLING of an enclosing let via the
  ; data stack (g is below the inner let's box cell) -- the cell must not shift it
  (= 108 (: g 100
            q (: a (+ g 1)
                  (mk z) (\ n (? (< n 1) z (self (- n 1))))
                  self (mk 7)
                  (+ a (self 3)))
            q))
  ; doubly nested: `a` reaches g (two boxing lets out) and b (one out) -- the
  ; reference crosses two cell regions and must account for both ---
  (= 114 (: g 100
            q (: b 7
                 r (: a (+ g b)
                      (mk z) (\ n (? (< n 1) z (self (- n 1))))
                      self (mk 7)
                      (+ a (self 3)))
                 r)
            q))))

; --- tenured closures (generational): a major promotes fa/ga into the MAJOR pool, so the
;     print/introspect paths -- show, = (alpha-equivalence), and the alpha-hash a map keys
;     by -- must read the source at value[-1] THERE, not just the main pool (fn_src/ttag/hash/
;     lam_src1 via in_heap). before the gen graduation `=` said equal while the address-hash
;     split, so a map keyed on fa could not find the alpha-equal ga. (a no-op on the single-pool
;     gcg build, where nothing tenures elsewhere.)
(: (lots n) ((: (go i a) (? (< i n) (go (+ i 1) (link i a)) (tally a))) 0 ())
   (fa a b) (+ a b) (ga a b) (+ a b)
   _ (lots 200000)                                       ; force a major -> fa/ga tenured
   (assert
     (= fa ga)                                           ; alpha-equal across the major pool
     (= 9 (peep {fa 9} ga 0))                            ; map keyed on fa, found by alpha-equal ga (eq/hash consistent)
     (< 8 (tally (show fa)))))                           ; show finds the tenured source

; --- a nested loop nom shadows an enclosing letrec sibling --- a lambda (zap)
; whose body opens a nested `:` binding a loop named like a LATER sibling of the
; enclosing let (go/go), beside a sibling that carries an import (rq, a computed
; pin it reads): the walk for the inner self-reference must not escape to the
; outer sibling. it once did -- during the closures phase the inner 'lam is not
; yet pinned -- and applied the sibling's import row MID-weave, past the fixcaps
; fixpoint, so every recursive (zap x) inside zap's own body under-applied: a
; truthy PARTIAL, the body never ran, silently (rm -r walked a tree and deleted
; nothing). avw's shadow guard ('far keys every binding name) now stops the
; escape, and weave scares 'imports-grew if a frozen import row ever grows again.
(: rq (cap [1])
   (zap q)
    (? (two? q)
       (: (go m ok) (? (! (two? m)) ok
                       (: k (zap (cap m))              ; the self-site that under-applied
                          (go (cup m) (&& ok (! k))))) ; every leaf must have ANSWERED ()
          (go q 1))
       ())                                             ; the leaf: nothing (falsy) -- a partial is truthy
   (go a) (? rq (zap a) 0)
   (assert (= 1 (go [5 6 7]))))                        ; ran to every leaf, no partial anywhere

; --- capture that reaches THROUGH a later sibling -----------------------------
; the read that wants a cell need not sit in the value's own def-prefix: an earlier
; closure inherits a later sibling's captures whole, so it asks for the value's slot
; at CONSTRUCTION -- before that slot exists. boxset follows the read graph to a
; fixpoint, so the value is boxed on the reach, not only on a direct mention.
(: (tk1 _) (: (a o) (f o) c (tablet 0) f (\ o (peep c o 'no)) _ (pin c 'k 'yes) (a 'k))
   (tk2 _) (: (helper x) (other x) K (tablet 0) (other y) (peep K y 'no)
               _ (pin K 'k 'yes) (helper 'k))
   ; a binding whose nom also spells a macro is a variable here, never a macro head
   (tk3 _) (: (a o) (zz o) c (tablet 0) zz (\ o (peep c o 'no)) _ (pin c 'k 'yes) (a 'k))
   (assert (id? 'yes (tk1 ())) (id? 'yes (tk2 ())) (id? 'yes (tk3 ()))))

; --- a top-level `:` reads its own name late ----------------------------------
; the outer let is body-less, so closure-fct defglobs -- AFTER its own def is built.
; a nested lambda naming it must read the book when it RUNS; capturing a value that
; does not exist yet is what the missing condition used to report at definition.
(: closure-fct (: (h n acc) (? (n < 1) acc (h (n - 1) (acc * n))) (\ n (h n 1))))
(: (tl1 _) (: walk (: (kid k) (? (two? k) (walk (cup k)) k) (\ n (kid n))) (walk '(1 2 3)))
   (assert (= 120 (closure-fct 5)) (nil? (tl1 ()))))
