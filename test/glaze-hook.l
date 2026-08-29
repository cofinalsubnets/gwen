; --- test/glaze-hook.l: the natjit CREATION-HOOK laws ---------------------------
; the hook is love/glaze/hook.l -- ev.l's `ala` reads it via (gv 'natjit) on EVERY closure
; built, top-level and embedded, and a qualifying leaf is native-backed AT CREATION. these
; laws lived inside that source as an `_ (assert)` pair, so they ran at every egg boot and
; every bake; they are a test, and this is where a test goes.
;
; NOT in the arch-neutral corpus (mk/common.mk's $t filters it out, beside glaze-x86.l): it
; EXECUTES native machine code. `make test_hook` runs it on the arches the hook emits for
; (x86-64 + aarch64), against the WOKEN IMAGE's hook and the EGG BOOT's -- two different
; closures, the first what ships and the second what an edit to hook.l moves.
;
; the three names it drives are the ones hook.l leaks on purpose: `natjit` (the hook),
; `fires` (the probe counter it bumps) and `fired?`. every law is the same shape -- take a
; count, build a closure through the LIVE ev, and claim both the ANSWER and that the hook
; owned it. the answer alone is no evidence: bytecode computes it too.
(assert (lit? natjit) (lit? fires) (lit? fired?))            ; the const-1 trap: unbound -> ()=zero point -> every claim below reads 1

(assert
    ; --- embedded leaves get native-backed at creation, and compute correctly ---
    (: n (peep fires () 0)
       (&& (31 = (ev '(: f (\ x (+ (* x 3) 1)) (f 10))))            ; leaf in a let body
           (fired? n)))                                             ; ... the hook fired on it
    (9000000000000000000 = (ev '((\ x (* x x)) 3000000000)))        ; embedded leaf, applied inline -> deopt -> bignum
    (3 = (ev '(: g (\ x (cap x)) (g '(3 4)))))                      ; chain body -> the AMBLE lane owns it now, still right
    ; --- an embedded FLOAT-RESULT leaf: native gem box at creation, == interp, deopts on a non-float arg ---
    (: n (peep fires () 0)  fr (ev '(: f (\ x (+ (* x 1.5) 2.0)) f))   ; f built by ala -> the float lane fires
       (&& (gem? (fr 4)) (8.0 = (fr 4)) (= (fr 4) ((\ x (+ (* x 1.5) 2.0)) 4))   ; boxes a gem, == the interpreter
           (fired? n) (= (fr 'nope) ((\ x (+ (* x 1.5) 2.0)) 'nope))))   ; ... the hook fired, and a non-float arg deopts == interp
    ; --- an embedded N-VAR loop (iterative fib i,a,b): native back-edge at creation, == interp ---
    (: n (peep fires () 0)  fib (ev '(: f (\ m ((: (go i a b) (? (< i m) (go (+ i 1) b (+ a b)) a)) 0 0 1)) f))
       (&& (55 = (fib 10)) (fired? n)                                       ; the hook owned the loop; fib(10)=55
           (= (fib 20) ((\ m ((: (go i a b) (? (< i m) (go (+ i 1) b (+ a b)) a)) 0 0 1)) 20))   ; == the interpreter
           (= (fib 100) ((\ m ((: (go i a b) (? (< i m) (go (+ i 1) b (+ a b)) a)) 0 0 1)) 100))))  ; ... and past fixnum: cf-deopt's bignum matrix path == interp
    ; --- an embedded CAPTURING closure: native captured closure, transparent ---
    (: mk (ev '(\ y (\ x (+ x y))))  a5 (mk 5)                      ; inner (\ x (+ x y)) is the embedded one
       (&& (15 = (a5 10)) ('(6 7 8) = (map a5 '(1 2 3)))            ; native captured closure, composes
           (= a5 ((\ y (\ x (+ x y))) 5))))                         ; ... and = the interpreter's instance
    ; --- id? + transparency on an embedded CAPTURING leaf pulled out as a value ---
    ; (the leaf must CAPTURE (z) -- a closed leaf like (\ x (+ x 0)) hoists to a shared
    ; constant, so (id 9) twice is one object; a capturing one is allocated per call.
    (: id (ev '(\ z (\ x (+ x z))))  f (id 9)  g (id 9)            ; inner CAPTURES z -> a fresh native cell per call
       (&& (19 = (f 10)) (= f ((\ z (\ x (+ x z))) 9)) !(id? f g))) ; computes right, = the interp instance, yet distinct
    ; --- the SELF seam: a letrec name recurs natively -- a tail call re-enters as a loop,
    ; a non-tail call takes the direct self frame; a captured import re-supplies through both
    (: n (peep fires () 0)
       r (ev '(: (sum f z l) (? (two? l) (sum f (f z (cap l)) (cup l)) z) (sum (\ a x (+ a (* x x))) 0 '(1 2 3 4))))
       (&& (30 = r) (fired? n)))
    (: n (peep fires () 0)
       r (ev '(: p 7 (f k) (? (< 0 k) (+ p (f (- k 1))) 0) (f 3)))
       (&& (21 = r) (fired? n)))
    ; --- the generic = stays native and right across kinds: points inline, structure drives
    (: n (peep fires () 0)
       mem (ev '(: (mem x l) (? (two? l) (? (= x (cap l)) 1 (mem x (cup l))) 0) mem))
       (&& (1 = (mem 'b '(a b c))) (0 = (mem 'z '(a b c))) (1 = (mem "s" '("q" "s")))
           (1 = (mem 2.0 '(1 2 3))) (0 = (mem 2.5 '(1 2 3))) (fired? n)))
    ; --- TAGGED surfaces untag: a synthesized body-lambda's apx/iop forms compile
    (: n (peep fires () 0)
       r1 (ev '(: p 5 q (+ p 2) (* q q)))
       r2 (ev '(: f (\ a b (+ a b)) (f 3 4)))
       (&& (49 = r1) (7 = r2) (fired? n)))
    ; --- VALUE LETS: the destructuring idiom rides slots; order, _, and the let-bound callee
    (: n (peep fires () 0)
       dw (ev '(: (dw x l) (? (two? l) (: h (cap l) t (cup l) (? (= h x) 1 (dw x t))) 0) dw))
       (&& (fired? n) (1 = (dw 'b '(a b c))) (0 = (dw 'z '(a b c))) (0 = (dw 3 ()))
           (= (dw 2.0 '(1 2)) ((\ x l (? (two? l) (: h (cap l) t (cup l) (? (= h x) 1 (dw x t))) 0)) 2.0 '(1 2)))))
    ; --- the owned op set widens: // % & | ^ (fixnum-guarded, trunc toward zero), == interp
    (: n (peep fires () 0)
       h (ev '(: (h a b) (? (< 0 b) (+ (+ (// a b) (% a b)) (+ (& a b) (+ (| a b) (^ a b)))) 0) h))
       i (\ a b (+ (+ (// a b) (% a b)) (+ (& a b) (+ (| a b) (^ a b)))))
       (&& (fired? n) (= (h 17 5) (i 17 5)) (= (h -17 8) (i -17 8)) (0 = (h 17 0))))
    ; --- an INNER LAMBDA in a driver body: the site builds the closure natively (lz's chain
    ; over the frame's imports), fresh per evaluation, = the interpreter's instance; a
    ; closed one is the shared cell. the inner bodies here decline every lane (a show call
    ; in a one-shot body), so the fire is the driver's own.
    (: n (peep fires () 0)
       ad (ev '(: (ad f l) (? (two? l) (link (f (\ x (+ (tally (show x)) (cap l)))) (ad f (cup l))) ()) ad))
       cs (ad (\ g g) '(1 2))
       (&& (fired? n) ('(4 5 6) = (ad (\ g (g 100)) '(1 2 3)))
           !(id? (cap cs) (cap (cup cs)))
           (= (cap cs) ((\ l (\ x (+ (tally (show x)) (cap l)))) '(1 2)))))
    (: n (peep fires () 0)
       ac (ev '(: (ac f l) (? (two? l) (link (f (\ x (+ (tally (show x)) 1))) (ac f (cup l))) ()) ac))
       cs (ac (\ g g) '(1 2))
       (&& (fired? n) ('(2 2) = (ac (\ g (g 5)) '(1 2))) (id? (cap cs) (cap (cup cs)))))
    ; ... and under a named-let whose loop var shadows the frame: the site's import renames with it
    (: n (peep fires () 0)
       mk (ev '(\ l ((: (go l acc) (? (two? l) (go (cup l) (link (\ y (* y (tally (show (cap l))))) acc)) acc)) l ())))
       (&& (fired? n) ('(2 2 2) = (map (\ c (c 2)) (mk '(1 2 3)))) ('(4 2) = (map (\ c (c 2)) (mk '(1 10))))))
   )
(: _ (puts "glaze-hook: ok -- the natjit creation hook (leaf, float, loop, capture, self, =, transparency)
") 0)
