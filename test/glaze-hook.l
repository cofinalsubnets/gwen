; --- test/glaze-hook.l: the natjit creation-hook laws ---------------------------
; the hook is love/glaze/hook.l -- ev.l's `ala` reads it via (gv 'natjit) on every closure
; built, top-level and embedded, and a qualifying leaf is native-backed at creation. these
; laws lived inside that source as an `_ (assert)` pair, so they ran at every egg boot and
; every bake; they are a test, and this is where a test goes.
;
; not in the arch-neutral corpus (mk/common.mk's $t filters it out, beside glaze-x86.l): it
; executes native machine code. `make test_hook` runs it on the arches the hook emits for
; (x86-64 + aarch64), against the woken image's hook and the egg boot's -- two different
; closures, the first what ships and the second what an edit to hook.l moves.
;
; the three names it drives are the ones hook.l leaks on purpose: `natjit` (the hook),
; `fires` (the probe counter it bumps) and `fired?`. every law is the same shape -- take a
; count, build a closure through the live ev, and claim both the answer and that the hook
; owned it. the answer alone is no evidence: bytecode computes it too.
(assert (lit? natjit) (lit? fires) (lit? fired?))            ; the const-1 trap: unbound -> ()=zero point -> every claim below reads 1

(assert
    ; --- embedded leaves get native-backed at creation, and compute correctly ---
    (: n (peep fires () 0)
       (&& (31 = (ev '(: f (\ x (+ (* x 3) 1)) (f 10))))            ; leaf in a let body
           (fired? n)))                                             ; ... the hook fired on it
    (9000000000000000000 = (ev '((\ x (* x x)) 3000000000)))        ; embedded leaf, applied inline -> deopt -> bignum
    (3 = (ev '(: g (\ x (cap x)) (g '(3 4)))))                      ; chain body -> the amble lane owns it now, still right
    ; --- an embedded float-result leaf: native gem box at creation, == interp, deopts on a non-float arg ---
    (: n (peep fires () 0)  fr (ev '(: f (\ x (+ (* x 1.5) 2.0)) f))   ; f built by ala -> the float lane fires
       (&& (gem? (fr 4)) (8.0 = (fr 4)) (= (fr 4) ((\ x (+ (* x 1.5) 2.0)) 4))   ; boxes a gem, == the interpreter
           (fired? n) (= (fr 'nope) ((\ x (+ (* x 1.5) 2.0)) 'nope))))   ; ... the hook fired, and a non-float arg deopts == interp
    ; --- an embedded N-VAR loop (iterative fib i,a,b): native back-edge at creation, == interp ---
    (: n (peep fires () 0)  fib (ev '(: f (\ m ((: (go i a b) (? (< i m) (go (+ i 1) b (+ a b)) a)) 0 0 1)) f))
       (&& (55 = (fib 10)) (fired? n)                                       ; the hook owned the loop; fib(10)=55
           (= (fib 20) ((\ m ((: (go i a b) (? (< i m) (go (+ i 1) b (+ a b)) a)) 0 0 1)) 20))   ; == the interpreter
           (= (fib 100) ((\ m ((: (go i a b) (? (< i m) (go (+ i 1) b (+ a b)) a)) 0 0 1)) 100))))  ; ... and past fixnum: cf-deopt's bignum matrix path == interp
    ; --- an embedded capturing closure: native captured closure, transparent ---
    (: mk (ev '(\ y (\ x (+ x y))))  a5 (mk 5)                      ; inner (\ x (+ x y)) is the embedded one
       (&& (15 = (a5 10)) ('(6 7 8) = (map a5 '(1 2 3)))            ; native captured closure, composes
           (= a5 ((\ y (\ x (+ x y))) 5))))                         ; ... and = the interpreter's instance
    ; --- id? + transparency on an embedded capturing leaf pulled out as a value ---
    ; (the leaf must CAPTURE (z) -- a closed leaf like (\ x (+ x 0)) hoists to a shared
    ; constant, so (id 9) twice is one object; a capturing one is allocated per call.
    (: id (ev '(\ z (\ x (+ x z))))  f (id 9)  g (id 9)            ; inner captures z -> a fresh native cell per call
       (&& (19 = (f 10)) (= f ((\ z (\ x (+ x z))) 9)) !(id? f g))) ; computes right, = the interp instance, yet distinct
    ; --- the self seam: a letrec name recurs natively -- a tail call re-enters as a loop,
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
    ; --- tagged surfaces untag: a synthesized body-lambda's apx/iop forms compile
    (: n (peep fires () 0)
       r1 (ev '(: p 5 q (+ p 2) (* q q)))
       r2 (ev '(: f (\ a b (+ a b)) (f 3 4)))
       (&& (49 = r1) (7 = r2) (fired? n)))
    ; --- VALUE lets: the destructuring idiom rides slots; order, _, and the let-bound callee
    (: n (peep fires () 0)
       dw (ev '(: (dw x l) (? (two? l) (: h (cap l) t (cup l) (? (= h x) 1 (dw x t))) 0) dw))
       (&& (fired? n) (1 = (dw 'b '(a b c))) (0 = (dw 'z '(a b c))) (0 = (dw 3 ()))
           (= (dw 2.0 '(1 2)) ((\ x l (? (two? l) (: h (cap l) t (cup l) (? (= h x) 1 (dw x t))) 0)) 2.0 '(1 2)))))
    ; --- the owned op set widens: // % & | ^ (fixnum-guarded, trunc toward zero), == interp
    (: n (peep fires () 0)
       h (ev '(: (h a b) (? (< 0 b) (+ (+ (// a b) (% a b)) (+ (& a b) (+ (| a b) (^ a b)))) 0) h))
       i (\ a b (+ (+ (// a b) (% a b)) (+ (& a b) (+ (| a b) (^ a b)))))
       (&& (fired? n) (= (h 17 5) (i 17 5)) (= (h -17 8) (i -17 8)) (0 = (h 17 0))))
    ; --- an inner lambda in a driver body: the site builds the closure natively (lz's chain
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
    ; --- the negations, a cond chain, and an inlined sibling's mint-bound arg: (nil? x) is
    ; (? x 0 1), (? c a c2 b e) nests, and cprop's (: <mint> (arg) body) binds like a name --
    ; the shapes holo's layout loop is made of
    (: n (peep fires () 0)
       lay (ev '(: (pad be n) (list n n) (at l k) (? (= k 0) (cap l) (at (cup l) (- k 1)))
                   (lay p acc) (? !(two? p) (rev acc)
                                  (= (cap (cap p)) 'label) (: _ (pin (tablet 0) (at (cap p) 1) 0) (lay (cup p) acc))
                                  (= (cap (cap p)) 'align) (: r (pad 0 (at (cap p) 1)) (lay (cup p) (link (cap r) acc)))
                                  (: r (cap (cap p)) (lay (cup p) (link r acc))))
                   lay))
       (&& (fired? n) ('(add 4 mov) = (lay '((label a) (add) (align 4) (mov)) ()))
           (= (lay '((x) (align 2)) ()) '(x 2))))
    ; --- the leaf grammar is exact-arity: (+ x 1 2) is the tower ((+ x 1) 2), never x + 1
    (: f (ev '(\ x (+ x 1 2)))  g (\ x (+ x 1 2))
       (&& (16 = (f 3)) (= (f 3) (g 3)) (= (f 5) (g 5))))
    ; ... and under a named-let whose loop var shadows the frame: the site's import renames with it
    (: n (peep fires () 0)
       mk (ev '(\ l ((: (go l acc) (? (two? l) (go (cup l) (link (\ y (* y (tally (show (cap l))))) acc)) acc)) l ())))
       (&& (fired? n) ('(2 2 2) = (map (\ c (c 2)) (mk '(1 2 3)))) ('(4 2) = (map (\ c (c 2)) (mk '(1 10))))))
    ; --- the restart law: a deopt re-runs the twin from the restart point, so any call-out the
    ;     native already made is made again. both loops write a byte and then overflow a fixnum
    ;     on the very next step, so the guard fires with the byte already out -- the amble
    ;     declines them, and the answer is the interpreter's: 40 bytes, nothing written twice.
    (: f (ev '(: (mk o n k) (? (<= n 0) (slurp o) (: _ (put o (+ 97 (% k 5))) (mk o (- n 1) (+ (* k 7) 3)))) mk))
       ("ebadebadebadebadebadebadebadebadebadebad" = (f (jug 0) 40 9)))     ; a self-call loop: its restart point is the running iteration
    (: f (ev '(: (build o n) ((: (go m k) (? (<= m 0) (slurp o) (: _ (put o (+ 97 (% k 5))) (go (- m 1) (+ (* k 7) 3))))) n 9) build))
       ("ebadebadebadebadebadebadebadebadebadebad" = (f (jug 0) 40)))       ; a named-let: its vars ride reserved slots, so the restart is the whole loop
    ; --- a constant past fixnum range cannot ride as an immediate: tag(e) = 2e+1 overflows the
    ;     machine word, so the amble must decline the body rather than lay it truncated. the
    ;     answer is the whole number, never a wrapped negative -- and the sign both ways.
    (: f (ev '(\ x (: y 6000000000000000000 (+ x y))))
       (&& (6000000000000000000 = (f 0)) (= (f 0) ((\ x (: y 6000000000000000000 (+ x y))) 0))))
    (: f (ev '(\ x (: y -6000000000000000000 (+ x y))))
       (-6000000000000000000 = (f 0)))
    (: f (ev '(\ n (: (go i a) (? (< i n) (go (+ i 1) 6000000000000000000) a) (go 0 0))))
       (6000000000000000000 = (f 4)))                            ; ... and one folding left behind in a loop
    (: f (ev '(\ n ((: (go i a) (? (< i n) (go (+ i 1) (+ a 6000000000000000000)) a)) 0 0)))
       (6000000000000000000 = (f 1)))
    ; ... and the lane keeps the calls no restart can reach -- nothing deopts after this one
    (: n (peep fires () 0)
       f (ev '(: (go i m a) (? (< i m) (go (+ i 1) m (tally (show (+ a i)))) a) go))
       (&& (fired? n) (2 = (f 0 20 0))))
   )
(: _ (puts "glaze-hook: ok -- the natjit creation hook (leaf, float, loop, capture, self, =, transparency)
") 0)
