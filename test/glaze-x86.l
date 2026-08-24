; glaze-x86 -- the x86-64 native-codegen self-tests for the glaze (love/glaze/{emit,auto}.l), the
; EXECUTING counterpart to the arch-neutral test/glaze.l (which only checks the substrate). HOST-
; x86-ONLY: these INSTALL and EXECUTE native machine code, so they're run only by the x86-guarded
; `make test_glaze` (never the arch-neutral corpus -- mk/common.mk filters this file out). It runs against
; the binary's BAKED glaze (boot loads emit.l+auto.l), so jit/jitsum/jitgroupir/.../base-ev/autogroup are
; bound -- cat'd straight through, NOT re-cat'd (that double-loads the glaze under auto-ev, clobbering base-ev).
;
; Each block compiles through `base-ev` (auto.l's captured pre-rebind compiler), because the
; loader's global `ev` is now auto-ev and mis-opfixes some pathological self-test forms. The EMIT
; block also shadows `ev` -> base-ev so its `(ev src)` reference values are the pure INTERPRETER
; (as when these lived inline, pre-rebind). The AUTO block leaves `ev` = the live auto-ev, since
; those tests exist to exercise the glazing path.

; CONTEXT GUARD (top-level, runs FIRST): this file needs the binary's BAKED glaze (`make test_glaze`).
; run STANDALONE, base-ev/jitgroupir/.. are unbound -> ()=zero point -> (base-ev '(..))=const-1=1 SILENTLY
; skips every block, yet the final top-level "ok" still prints (a false pass -- the const-1 trap). assert
; the harness names are bound so a wrong invocation SCARES `;; assert (lit? base-ev)` instead.
; the assembler and the glaze live in MODULES now (the registry via holo's (leave ());
; the glaze is one too, emit.l + auto.l): this stream cat-loads post-boot, so nothing
; folded for it -- bind every internal it drives through the accessors, bare-global
; reads both (bcat stands in for the glaze's private byte-concat helper).
; ..and the boundary those accessors stand for: the glaze is a REGISTERED module and NOT ONE
; of its ~415 names is in the book. this is the assert that catches a leak coming back --
; a name on the book resolves bare, so nothing else here would ever notice. ⚠ it must run
; BEFORE the binds below: that form is a body-less top-level `:`, so ITS names land on the
; book, and half the roster here would answer its own copies.
(assert (member? 'glaze (from ()))
        (400 < (tally (keys (from 'glaze))))
        (: nm (names ()) (&& (! (member? 'autogroup nm)) (! (member? 'jitir nm)) (! (member? 'glaze nm)))))
(: assemble (from 'holo 'assemble)
   bcat (\ ps (foldl (\ a b (+ a b)) () ps))
   G (from 'glaze)
   anat-defs (G 'anat-defs)  autogroup (G 'autogroup)  autonat (G 'autonat)  autospec (G 'autospec)
   base-ev (G 'base-ev)  castbuild (G 'castbuild)  cggir (G 'cggir)  cggvir (G 'cggvir)
   consir (G 'consir)  coreir (G 'coreir)  delet (G 'delet)  deoptir (G 'deoptir)
   emit (G 'emit)  fbits (G 'fbits)  fr (G 'fr)  grok (G 'grok)
   jit (G 'jit)  jit2 (G 'jit2)  jitf (G 'jitf)  jitfgrid (G 'jitfgrid)  jitfr (G 'jitfr)
   jitgroupir (G 'jitgroupir)  jitgroupirx (G 'jitgroupirx)  jitir (G 'jitir)  jitloop-n (G 'jitloop-n)  jitn (G 'jitn)
   jitnir (G 'jitnir)  jitsum (G 'jitsum)  jitcalloutsum (G 'jitcalloutsum)  jitcalloutx (G 'jitcalloutx)  calloutir (G 'calloutir)  calloutframeir (G 'calloutframeir)  loopcode (G 'loopcode)  loopinfo (G 'loopinfo)
   loopinfo-n (G 'loopinfo-n)  mkhir (G 'mkhir)  mkouterir (G 'mkouterir)  nthl (G 'nthl)
   occurs? (G 'occurs?)  plift (G 'plift)  qualfr (G 'qualfr)  twolow (G 'twolow)
   nif (G 'nif))                                 ; the C install seam, a glaze member: every boot pulls it off the book
(assert (lit? base-ev) (lit? autogroup) (lit? jitgroupir) (lit? assemble))

(base-ev '(: ev base-ev
(: (isum n) ((: (go i a) (? (< i n) (go (+ i 1) (+ a (* i 7))) a)) 0 0)
      f (jit '(\ x (+ (* x 3) 1)))   g (jitsum '(* i 7))
    _ (assert
  (lit? f) (lit? g)                                          ; native closures are applicable values
  (31 = (f 10)) (315 = (g 10))                               ; they compute
  ((\ x (+ (* x 3) 1)) = f)                                  ; ... and are =-IDENTICAL to their source
  !(f = (\ x (+ (* x 3) 2)))
  (f = (jit '(\ x (+ (* x 3) 1))))                           ; two natives, same source
  ; fixnum INPUT guard: a non-fixnum arg DEOPTS (no sar/putfix corruption of a heap word)
  (: idn (jit '(\ x x)) (&& (5 = (idn 5)) ('(1 2) = (idn '(1 2))) ('a = (idn 'a)) (id? (:) (idn (:)))))
  (: pls (jit '(\ x (+ x 1))) (&& (6 = (pls 5)) ((+ '(1 2) 1) = (pls '(1 2)))))   ; native == interp on a non-fixnum
  (: q (jit '(\ n (+ (* n 3) 1))) (&& (31 = (q 10)) ((\ n (+ (* n 3) 1)) = q) (q = f)))  ; arity-1 leaf, non-x param (= f up to alpha)
  ('(2 4 6) = (map (jit '(\ x (+ x 1))) '(1 3 5)))           ; compose with higher-order fns
  ((isum 1000) = (g 1000))                                   ; the loop agrees with the interpreter
  (9000000000000000000 = ((jit '(\ x (* x x))) 3000000000))  ; OVERFLOW -> deopt -> correct bignum
  (1 = ((: (go i a) (? (< i 60) (go (+ i 1) (&& a (= ((jit `('\ 'x `('* 'x i))) 7) (* 7 i)))) a)) 0 1))  ; agree 0..60
  ; comparisons (-> 0/1) and conditionals
  (: lt (jit '(\ x (< x 5))) (&& (1 = (lt 3)) (1 = (lt -2)) (0 = (lt 5)) (0 = (lt 9))))
  (: ab (jit '(\ x (? (< x 0) (- 0 x) x))) (&& (7 = (ab -7)) (7 = (ab 7)) (0 = (ab 0))))   ; abs
  ((\ x (? (< x 0) (- 0 x) x)) = (jit '(\ x (? (< x 0) (- 0 x) x))))                        ; transparent
  (: cap50 (jitsum '(? (< i 50) i 0)) c50 ((: (go i a) (? (< i 1000) (go (+ i 1) (+ a (? (< i 50) i 0))) a)) 0 0)
     (= c50 (cap50 1000)))                                                                  ; loop with a conditional body
  ; CALL-OUT BRIDGE (native side): a native counted loop whose body CALLS OUT to an interp closure each
  ; iteration (the STACKLESS callout_drive; clos is car(value[1]), reached Ip-relative). native == interp.
  (: (isum clos n) ((: (go i a) (? (< i n) (go (+ i 1) (+ a (clos i))) a)) 0 0)              ; the faithful interp twin
     sq (\ i (* i i))  co (jitcalloutsum sq)
     (&& (lit? co) (0 = (co 0)) (30 = (co 5)) (285 = (co 10)) ((co 8) = (isum sq 8))         ; fixnum fast path
         ; GATE 1 (result-kind): a FLOAT-returning callee -> the untag guard deopts -> interp twin (gem sum)
         (: cf (jitcalloutsum (\ i (* i 0.5))) (&& (7.5 = (cf 6)) ((cf 20) = (isum (\ i (* i 0.5)) 20))))
         ; GATE 2 (sum-overflow): fixnum results whose SUM leaves fixnum range -> the adds guard deopts -> bignum
         (: cb (jitcalloutsum (\ i 1000000000000000000)) (&& ((cb 100) = (isum (\ i 1000000000000000000) 100))
                                                             (100000000000000000000 = (cb 100)))) ; == interp bignum
         ; * FOLD (product-over-(F i)): jitcalloutx with op '*, SA kept RAW (mulo accumulator, retag+overflow at end)
         (: iprod (\ f n ((: (go i a) (? (< i n) (go (+ i 1) (* a (f i))) a)) 0 1))                ; the faithful interp product twin
            inc (\ i (+ i 1))  pr (jitcalloutx inc '(\ n 0) '* 'i 'i 'x64)                               ; native product of (i+1) = n!
            (&& (lit? pr) (1 = (pr 0)) (120 = (pr 5)) ((pr 6) = (iprod inc 6))                     ; fast path: 5!=120, empty=1 (product identity)
                (: pf (jitcalloutx (\ i (+ i 0.5)) '(\ n 0) '* 'i 'i 'x64)                               ; GATE 1: float result -> deopt -> interp product
                   (1.875 = (pf 3)))
                (: pb (jitcalloutx inc '(\ n 0) '* 'i 'i 'x64)                                           ; GATE 2: product leaves fixnum range -> deopt -> bignum
                   (&& (51090942171709440000 = (pb 21)) ((pb 25) = (iprod inc 25))))
                ; the [2^62, 2^63) TAG-boundary window: 3*2^61 is out of fixnum yet < 2^63 -> the end-retag adds-vc gate must deopt
                (: p3 (jitcalloutx (\ i (? (= i 0) 3 2)) '(\ n 0) '* 'i 'i 'x64)
                   (6917529027641081856 = (p3 62)))))
         ; NON-(F i) ARG: the per-step term is (F ARG), ARG a computed expr over i (not the bare counter) --
         ; caarg compiles ARG natively (mirroring cgir), tags it, and an ARG that leaves fixnum range deopts
         ; (caargdeopt: restore machine sp -> codeopt -> twin, which recomputes ARG as a bignum).
         (: isumg  (\ f g n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (f (g i)))) a)) 0 0))         ; interp: sum f(g(i))
            iprodg (\ f g n ((: (go i a) (? (< i n) (go (+ i 1) (* a (f (g i)))) a)) 0 1))         ; interp: product f(g(i))
            sqx (\ x (* x x))  md7 (\ x (% x 7))
            cae  (jitcalloutx sqx '(\ n 0) '+ 'i '(+ i 1) 'x64)                                    ; sum sq(i+1)
            cap  (jitcalloutx sqx '(\ n 0) '* 'i '(+ i 1) 'x64)                                    ; product sq(i+1) = (n!)^2
            caov (jitcalloutx md7 '(\ n 0) '+ 'i '(* (* i i) 1000000000) 'x64)                     ; ARG overflows ~i=68k; bounded clos so ONLY the arg deopts
            mkq  (\ f (\ n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (f (* i i)))) a)) 0 0)))        ; captured f, computed arg (* i i) -> the FRAME lane (hook)
            cq   (mkq sqx)
            (&& (lit? cae) (55 = (cae 5)) (0 = (cae 0)) ((cae 6) = (isumg sqx (\ i (+ i 1)) 6))    ; computed arg, native fast path
                (576 = (cap 4)) ((cap 15) = (iprodg sqx (\ i (+ i 1)) 15))                         ; computed arg + * fold, overflow -> bignum twin
                (280000 = (caov 70000)) ((caov 100000) = (isumg md7 (\ i (* (* i i) 1000000000)) 100000))   ; ARG overflow -> caargdeopt -> twin
                ((cq 6) = (isumg sqx (\ i (* i i)) 6)) ((cq 50) = (isumg sqx (\ i (* i i)) 50)))))) ; captured computed arg (frame lane), fast + bignum
  ; // and %
  (: dv (jit '(\ x (// x 3))) (&& (3 = (dv 10)) (4 = (dv 13)) (-3 = (dv -10))))
  (: md (jit '(\ x (% x 7)))  (&& (1 = (md 8)) (-1 = (md -8)) (0 = (md 14))))
  ; mid-nest deopt must leave the machine stack balanced (rsp unwind on deopt)
  (16000000000000000001 = ((jit '(\ x (+ 1 (* x x)))) 4000000000))
  (16000000000000000003 = ((jit '(\ x (+ (+ 1 (* x x)) 2))) 4000000000))
  ; bitwise & | ^
  (: bt (jit '(\ x (& x 12))) (&& (8 = (bt 10)) (12 = (bt 13))))
  (: bo (jit '(\ x (| x 1)))  (&& (3 = (bo 2)) (5 = (bo 5))))
  (: bx (jit '(\ x (^ x 5)))  (&& (15 = (bx 10)) (0 = (bx 5))))
  ; arity-2 leaves: saturated + curried, transparent, deopt via interp body
  (: ad (jit2 '(\ x y (+ x y)))           (&& (7 = (ad 3 4)) (7 = ((ad 3) 4))))
  (: sb (jit2 '(\ x y (- (* x x) y)))     (&& (5 = (sb 3 4)) (16000000000000000000 = (sb 4000000000 0))))
  ((\ x y (+ x y)) = (jit2 '(\ x y (+ x y))))
  (: dd (jit2 '(\ x y (// x y)))          (&& (3 = (dd 13 4)) (-2 = (dd -7 3))))
  ; arity-3 / arity-4 leaves: param i at Sp[i], saturating at ANY split, transparent
  (: t3 (jitn '(\ x y z (+ (+ x y) (* z 2)))) (&& (13 = (t3 1 2 5)) (13 = ((t3 1 2) 5)) (13 = ((t3 1) 2 5))))
  ((\ x y z (+ (+ x y) (* z 2))) = (jitn '(\ x y z (+ (+ x y) (* z 2)))))
  (: t4 (jitn '(\ a b c d (+ (* a b) (- c d)))) (&& (7 = (t4 2 3 7 6)) (7 = ((t4 2 3) 7 6)) (7 = ((t4 2) 3 7 6))))
  (16000000000000000005 = ((jitn '(\ x y z (+ (+ (* x x) y) z))) 4000000000 0 5))   ; deopt through a middle param
  ; N-VARIABLE loop: iterative fib (3 loop vars i,a,b) -- native == interp, deopts at overflow
  (: nf (jitloop-n '(\ n ((: (go i a b) (? (< i n) (go (+ i 1) b (+ a b)) a)) 0 0 1))
                   '(i a b) '((+ i 1) b (+ a b)) '(< i n) 'a '(0 0 1))
     fi (\ n ((: (go i a b) (? (< i n) (go (+ i 1) b (+ a b)) a)) 0 0 1))
     (&& (55 = (nf 10)) (832040 = (nf 30)) (= (fi 30) (nf 30))                       ; fib(10)=55, fib(30)=832040
         (4660046610375530309 = (nf 91))))                                          ; fib(91) > fixnum -> deopt -> bignum
  ; CAPTURES = partial-apply a native multi-arg; the leading param(s) are the captures.
  (: mk (jit2 '(\ n x (+ x n)))  a5 (mk 5)  a7 (mk 7)
     (&& (15 = (a5 10)) (17 = (a7 10)) ('(6 7 8) = (map a5 '(1 2 3)))                 ; native captured closure, composes
         (8000000000000000000 = ((mk 4000000000000000000) 4000000000000000000))))    ; ... and deopts through the capture
  ; HEAP RESULT (jitfr): a float-result leaf BOXES on the heap -- a gem out, == interp, transparent, deopts
  (: fr (jitfr '(\ x (+ (* x 1.5) 2.0)) 'x64)  ifr (\ x (+ (* x 1.5) 2.0))
     (&& (gem? (fr 4)) (8.0 = (fr 4)) (= (ifr 7) (fr 7))                  ; a FLOAT box; 4*1.5+2=8.0; == interp
         (lit? fr) ((\ x (+ (* x 1.5) 2.0)) = fr)                         ; applicable + =-IDENTICAL to source
         (= (map ifr '(1 2 3)) (map fr '(1 2 3)))                         ; composes with map, == interp
         (= (ifr (:)) (fr (:)))))                                         ; non-fixnum arg -> fxv deopt -> interp
  (: frz (jitfr '(\ x (/ (- x x) (- x x))) 'x64) (&& (gem? (frz 5)) (0.0 = (frz 5))))   ; 0/0 -> NaN -> 0.0 (mk_gem's law)
  ; GC-SAFETY: a native-written box is a normal ai_gem (kind=lvm_gembox, 2 words) -> the cheney copier
  ; traces/moves it like any gem. hold one across a big alloc (forces GC), and box thousands under
  ; pressure -- if the box were malformed or the bump corrupted Sp's region, these would diverge/crash.
  (: gfr (jitfr '(\ x (+ (* x 1.5) 2.0)) 'x64)  b (gfr 4)  _ (tally (jot 200000))  (8.0 = b))   ; the box survives a forced GC
  (: gfr (jitfr '(\ x (+ (* x 1.5) 2.0)) 'x64)  ys (map gfr (jot 4000))  zs (map (\ x (+ (* x 1.5) 2.0)) (jot 4000)) (= ys zs))  ; 4000 boxes under GC pressure == interp
  ; --- added coverage: setcc <=/>/>=/=, multi-arg (cgn) %/&/|/cmp/?, *-fold loop, //-by-0 deopt ---
  ; comparison RESULT codes -- only `<` reached a setcc before, 4 of 5 opcodes were dark
  (: le (jit '(\ x (<= x 5))) (&& (1 = (le 5)) (1 = (le 3)) (0 = (le 6))))            ; setcc <=
  (: gt (jit '(\ x (>  x 5))) (&& (1 = (gt 6)) (0 = (gt 5)) (0 = (gt 2))))            ; setcc >
  (: ge (jit '(\ x (>= x 5))) (&& (1 = (ge 5)) (1 = (ge 9)) (0 = (ge 4))))            ; setcc >=
  (: eq (jit '(\ x (=  x 5))) (&& (1 = (eq 5)) (0 = (eq 4)) (0 = (eq 6))))            ; setcc =
  ; multi-arg (cgn) op dispatch -- only + - * // were exercised on the N-ary path
  (: mdn (jit2 '(\ x y (% x y))) (&& (1 = (mdn 8 7)) (-1 = (mdn -8 7)) (0 = (mdn 14 7))))   ; idivseq-mn %
  (: ban (jit2 '(\ x y (& x y))) (&& (8 = (ban 10 12)) (4 = (ban 4 5))))                    ; cgn -> bitseq
  (: bon (jit2 '(\ x y (| x y))) (&& (3 = (bon 2 1)) (7 = (bon 5 3))))
  (: ltn (jit2 '(\ x y (< x y))) (&& (1 = (ltn 3 5)) (0 = (ltn 5 3))))                      ; cgn -> cmpseq
  (: mnn (jit2 '(\ x y (? (< x y) x y))) (&& (3 = (mnn 3 5)) (4 = (mnn 9 4))))              ; cgn -> condcode (min)
  ; *-fold counted loop -- jitsum hardcodes +/<, so drive acc-mul + nonzero r11-init via loopcode
  (: prodf (: psrc `('\ 'n `(`(': `('go 'i 'a) `('? `('< 'i 'n) `('go `('+ 'i 1) `('* 'a `('+ 'i 1))) 'a)) 0 1))
              (nif (emit (loopcode '(+ i 1) 'i '* '< 'x64)) (ev psrc) psrc 1))
     pf (\ n ((: (go i a) (? (< i n) (go (+ i 1) (* a (+ i 1))) a)) 0 1))
     (&& (120 = (prodf 5)) (= (pf 8) (prodf 8))))                                           ; n! via a product fold
  ; divide-by-zero takes the guardz deopt -- only nonzero divisors were tested
  (: dz (jit '(\ x (// x 0))) (= (// 10 0) (dz 10)))                                        ; // by 0 -> deopt -> interp
  ; --- FLOAT predicate leaves (SSE): unboxed-double compares over a fixnum arg -> 0/1 ---
  (= "4616189618054758400" (show (fbits 4.0)))                                              ; fbits: 4.0 -> 0x4010000000000000
  (: flt (jitf '(\ x (< (* x 1.5) 10.0)))
     (&& (1 = (flt 6)) (0 = (flt 7)) (1 = (flt 0)) (1 = (flt -3))                           ; 1.5x < 10  <=> x < 6.67
         ((\ x (< (* x 1.5) 10.0)) = flt)                                                   ; transparent (=/show)
         (= ((\ x (< (* x 1.5) 10.0)) '(1 2)) (flt '(1 2)))))                               ; non-fixnum arg -> deopt -> interp
  (: fle (jitf '(\ x (<= (+ (* x x) 2.0) 38.0))) (&& (1 = (fle 6)) (0 = (fle 7)) (1 = (fle -6))))   ; x*x+2 <= 38 <=> |x|<=6
  (: fgt (jitf '(\ x (> (/ 100.0 x) 7.0)))       (&& (1 = (fgt 10)) (0 = (fgt 20)) (1 = (fgt 5))))  ; 100/x > 7 <=> 0<x<14.3
  (: fge (jitf '(\ x (>= (- (* x 0.5) 1.0) 2.0))) (&& (1 = (fge 6)) (0 = (fge 5))))                 ; x/2-1 >= 2 <=> x>=6
  (: feq (jitf '(\ x (= (* x 0.25) 2.0)))         (&& (1 = (feq 8)) (0 = (feq 7))))                 ; x/4 == 2 <=> x==8
  ; --- NESTED grid owner: native whole-grid mandelbrot == interpreter (checksum 87130) ---
  (: gsrc '(\ _ (: (mand cx cy zx zy it) (? (&& (< it 100) (<= (+ (* zx zx) (* zy zy)) 4.0))
                       (mand cx cy (+ (- (* zx zx) (* zy zy)) cx) (+ (* (* 2.0 zx) zy) cy) (+ it 1)) it)
                   (col px py acc) (? (< py 64) (col px (+ py 1) (+ acc (mand (- (* px 0.046875) 2.0) (- (* py 0.046875) 1.5) 0.0 0.0 0))) acc)
                   (grid px acc) (? (< px 64) (grid (+ px 1) (+ acc (col px 0 0))) acc)
                 (grid 0 0)))
     mg (jitfgrid gsrc 64 64 '(- (* px 0.046875) 2.0) '(- (* py 0.046875) 1.5)
                  '(zx zy) '((+ (- (* zx zx) (* zy zy)) cx) (+ (* (* 2.0 zx) zy) cy))
                  '(+ (* zx zx) (* zy zy)) '<= 4.0 100 'x64)
     (&& (= 87130 (mg 0)) (= ((ev gsrc) 0) (mg 0))))
  ; --- AUTO-HOOK: recognize the (mand col grid) nest from SOURCE and glaze it -> checksum 87130 ---
  (: gmand '(? (&& (< it 100) (<= (+ (* zx zx) (* zy zy)) 4.0))
               (mand cx cy (+ (- (* zx zx) (* zy zy)) cx) (+ (* (* 2.0 zx) zy) cy) (+ it 1)) it)
     gcol '(? (< py 64) (col px (+ py 1) (+ acc (mand (+ -2.0 (* px 0.046875)) (+ -1.5 (* py 0.046875)) 0.0 0.0 0))) acc)
     ggrid '(? (< px 64) (grid (+ px 1) (+ acc (col px 0 0))) acc)
     ng (grok '(\ _ 0) '(cx cy zx zy it) gmand gcol ggrid 'x64)
     (= 87130 (ng 0))))
    (say err "love/glaze/emit: native codegen ok (arith + loop + D-dim float grid, transparent, deopt)\n"))
))

; CALL-OUT RECOGNIZER (natjit calloutinfo, hook.l): a counted sum whose per-step term calls a resolvable
; GLOBAL closure glazes AUTOMATICALLY as the stackless bridge (emit.l jitcalloutx), F resolved via ala's
; gv. The targets are top-level body-less defines -> real globals, so gv finds them at the summing
; closure's creation. == interp across the fast path + both soundness gates.
(: (rc-sq i) (* i i))
(: (rc-half i) (* i 0.5))
(: (rc-big i) 1000000000000000000)
(: (rc-ssum n) ((: (go i a) (? (< i n) (go (+ i 1) (+ a (rc-sq i))) a)) 0 0))
(: (rc-fsum n) ((: (go i a) (? (< i n) (go (+ i 1) (+ a (rc-half i))) a)) 0 0))
(: (rc-bsum n) ((: (go i a) (? (< i n) (go (+ i 1) (+ a (rc-big i))) a)) 0 0))
; pure-interp references (inline sums -- not closures, so natjit never fires on them), shared globals
(: sref ((: (go i a) (? (< i 30)  (go (+ i 1) (+ a (rc-sq i)))   a)) 0 0))
(: fref ((: (go i a) (? (< i 20)  (go (+ i 1) (+ a (rc-half i))) a)) 0 0))
(: bref ((: (go i a) (? (< i 100) (go (+ i 1) (+ a (rc-big i)))  a)) 0 0))
(: _ (assert (lit? rc-ssum) (285 = (rc-ssum 10)) ((rc-ssum 30) = sref)             ; fixnum fast path == interp
        (95.0 = (rc-fsum 20)) ((rc-fsum 20) = fref)                                ; GATE 1: float call-out -> gem, == interp
        (100000000000000000000 = (rc-bsum 100)) ((rc-bsum 100) = bref)             ; GATE 2: sum overflows fixnum -> bignum, == interp
        (8999995500000500000 = (rc-ssum 3000000)))                                 ; GATE 2 BOUNDARY: sum in [2^62,2^63) overflows the fixnum TAG (not 2^63) -> bignum (tagged-accumulator gate)
   (say err "test/glaze-x86: call-out recognizer -- a global-target sum glazes (fast path + gate1 float + gate2 overflow) ok\n"))

; CAPTURED / multi-arg CALL-OUT (calloutinfo-frame + njit-calloutframe): F read from the [imps args]
; frame, not baked. rc-summer captures f -> `(\ f n ..)`; the frame native reads F/N by index, hand-rolls
; lvm_ret(n=a), deopts to value[1]=e (which captures F). == interp across the fast path + both gates.
(: (rc-summer f) (\ n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (f i))) a)) 0 0)))          ; captured: (\ f n ..)
(: (rc-hosum f n) ((: (go i a) (? (< i n) (go (+ i 1) (+ a (f i))) a)) 0 0))               ; multi-arg: f a genuine 2nd frame arg
(: cs (rc-summer rc-sq)  cf (rc-summer rc-half)  cb (rc-summer rc-big)
   _ (assert (lit? cs) (285 = (cs 10)) ((cs 30) = sref)                                    ; captured fixnum fast path == interp
        (95.0 = (cf 20)) ((cf 20) = fref)                                                  ; GATE 1 (captured float)
        (100000000000000000000 = (cb 100)) ((cb 100) = bref)                              ; GATE 2 (captured overflow -> bignum)
        (8999995500000500000 = (cs 3000000))                                             ; GATE 2 BOUNDARY (captured): sum in [2^62,2^63) -> bignum (tagged accumulator)
        (285 = (rc-hosum rc-sq 10)) ((rc-hosum rc-sq 30) = sref)                          ; multi-arg HO sum == interp
        (95.0 = (rc-hosum rc-half 20)))
   (say err "test/glaze-x86: call-out recognizer -- captured + multi-arg (frame-read F, hand-rolled ret) glaze ok\n"))

; AMBLE (njit-amble, hook.l's LAST lane): the universal boring-tier translation. shapes every
; other lane declines -- a symbolic chain walker, a consing reverse, a loop whose per-step calls
; nest (no fold shape) -- glaze at creation: tagged values, Sp-slot temporaries (GC roots), drive
; call-outs, RESTART deopt (original frame untouched). == interp across the fast path, the arith
; overflow deopt (-> bignum via the twin), the non-fixnum entry deopt, and native cons. the ()
; literal rides value[1] as the empty MINT (li 1 would be the charm 0 -- the data.l 0-fossil bug).
; calls ride the WALKABLE resume (callout_resume: the frame carries tag(bb-entry) + the raw blob
; base -- an odd charm + an out-of-pool code address, so a GC with call-outs pending walks it
; clean; the retB stack-interior pointer that stormed uu2lean is retired). FREE call heads in
; LOOP bodies resolve via gv at compile (v4, the global-callee seam -- redefinition-stale, the
; callout bet; the loop gate is the cost model: a one-shot call wrapper is net-negative ambled)
; and ride nifx extras slots; go, the frame/loop names, and ala's farq letrec-locals are NEVER
; candidates, so a non-tail (go ..) or a weave re-analysis sibling declines rather than bake a
; stale leaked namesake (the rc4 regression below; bref-summing-fref was the farq find). am-deep
; is the STORM SHAPE: deep NON-TAIL call nesting with cons churn per level -- thousands of
; pending drive frames while the GC runs.
(: (am-cnt l k a) ((: (go l a) (? (two? l) (go (cup l) (? (id? (cap l) k) (+ a 1) a)) a)) l a))
(: (am-rev l acc) ((: (go l a) (? (two? l) (go (cup l) (link (cap l) a)) a)) l acc))
(: (am-mk i acc) (? (< i 1) acc (am-mk (- i 1) (link (% i 7) acc))))
(: am-l (am-mk 200 ()))
(: am-dbl (: F (\ x (+ x x)) (\ n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (F (F i)))) a)) 0 0))))
(: (am-sq x) (+ (* x x) 1))                                                          ; the v4 global callees + the rc4 namesake
(: (am-mul2 x y) (* (+ x 1) y))
(: (am-seed k) (+ k 3))
(: (am-gref l a) ((: (go l a) (? (two? l) (go (cup l) (+ a (am-sq (cap l)))) a)) l a))
(: am-go (\ x (* x 1000000)))
(: _ (: n0 (peep fires () 0)
       wk (ev '(\ l k a ((: (go l a) (? (two? l) (go (cup l) (? (id? (cap l) k) (+ a 1) a)) a)) l a)))
       ; references are LITERALS: an in-file "interp twin" is impossible -- the hook fires at
       ; base-ev's ala too, so any reference closure ambles as well (how bref nearly lied).
       (assert (fired? n0)                                                          ; the walker ambled at creation
          (29 = (wk am-l 3 0)) (29 = (am-cnt am-l 3 0))                             ; fast path; i%7==3 for i in 1..200 -> 29
          (4611686018427387932 = (wk am-l 3 4611686018427387903))                   ; +1 past max fixnum -> restart -> bignum
          (30.5 = (wk am-l 3 1.5))                                                  ; gem acc -> arith guard deopt -> generic +
          (0 = (wk () 3 0)) (7 = (wk 42 3 7))                                      ; ()/non-chain -> two? false -> a
          ((am-rev (am-rev am-l ()) ()) = am-l)                                    ; native cons round-trips against the interp-built list (()-tailed)
          (id? () (am-rev () ()))                                                  ; the () literal is the MINT, not charm 0
          (19800 = (am-dbl 100))                                                   ; nested captured calls via the drive: sum 4i, i<100
          (: deep (base-ev '(: H (\ x ((: (go i a) (? (< i 40) (go (+ i 1) (link i a)) a)) 0 (link x ())))
                             (: (dv d) (? (< d 1) 0 (+ (two? (H d)) (dv (- d 1)))))))
             (5000 = (deep 5000)))                                                 ; the STORM SHAPE: 5000 pending frames + GC churn, == interp
          ; QUOTED-DATUM constants (v3): a nom dispatch walker -- 'r/'R ride nifx extras slots,
          ; (= a 'sym) is raw identity (the point-= law: a point equals only itself). a quoted
          ; LIST constant answers the SAME source node interp does (id?-transparent, never folded
          ; with an =-equal sibling).
          (: dsp (ev '(\ l a ((: (go l a) (? (two? l) (go (cup l) (? (id? (cap l) (\ r)) (+ a 1) (? ((cap l) = (\ R)) (+ a 10) a))) a)) l a)))
             (&& (23 = (dsp '(r R r x R r) 0)) (0 = (dsp () 0)) (5 = (dsp '(x y) 5))))
          (: qz (ev '(\ x (? (id? x (\ k)) (\ (1 2)) ())))
             (&& ('(1 2) = (qz 'k)) (id? (qz 'k) (qz 'k)) (id? () (qz 'nope))))
          (: qe (ev '(\ x (? (= x (\ q)) 1 0)))
             (&& (1 = (qe 'q)) (0 = (qe 'r)) (0 = (qe 5)) (0 = (qe "q")) (0 = (qe '(q)))))
          ; GLOBAL CALLEES (v4): a free head resolves via gv and drive-calls through its extras
          ; slot -- per-element (am-sq), multi-arg via the curry chain (am-mul2), in an INIT
          ; (am-seed), and a nif head (%). all == their literal references.
          (: gw (ev '(\ l a ((: (go l a) (? (two? l) (go (cup l) (+ a (am-sq (cap l)))) a)) l a)))
             (&& (32 = (gw '(2 3 4) 0)) ((gw am-l 0) = (am-gref am-l 0))))
          (: gm (ev '(\ n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (am-mul2 i 2))) a)) (am-seed 0) 0)))
             (&& (98 = (gm 10)) (0 = (gm 0))))                                    ; inits ran: (am-seed 0) = 3 -> i starts at 3
          (: gp (ev '(\ l a ((: (go l a) (? (two? l) (go (cup l) (+ a (% (cap l) 3))) a)) l a)))
             (4 = (gp '(10 11 12 13) 0)))                                         ; 1+2+0+1 -- a nif callee curries through two drives
          ; the rc4 REGRESSION: a GLOBAL namesake of the loop name exists (am-go, *1000000); a
          ; NON-TAIL (am-go ..) inside the body is the LOCAL named-let binding -- go is never a
          ; gn candidate, so amble declines the outer body and the value is the bytecode's
          ; (a baked global answers 11 here, not 10: rc4's miscompile, bref summing dref's loop).
          (: r4 (ev '(\ n ((: (am-go i a) (? (< i n) (am-go (+ i 1) (+ a i)) (+ a (tally (am-go n))))) 0 0)))
             (&& (10 = (r4 5))
                 ((r4 5) = ((\ n ((: (am-go i a) (? (< i n) (am-go (+ i 1) (+ a i)) (+ a (tally (am-go n))))) 0 0)) 5))))
          ; the STRING lane: a 1-arg frame-callee call carries a text fast path (kind-guarded,
          ; falls to the DRIVE -- never a deopt), so a non-negative in-range (s i) in a loop is
          ; a native byte index; the fast lane's unsigned compare wraps a negative huge, so both
          ; the from-the-end read and the out-of-range () DRIVE and answer what C answers, which
          ; is the whole point -- a glazed loop and a folded constant agree. (tally s)
          ; reads the length raw. the same call site takes closures (mx) and strings (drive vs
          ; byte path chosen at RUN time); a chain tally DRIVES (no storm); a param named tally
          ; shadows the fast lane.
          (: dg (ev '(\ s n ((: (go i a) (? (< i n) (go (+ i 1) (+ (* a 10) (- (s i) 48))) a)) 0 0)))
             (31415926 = (dg "31415926" 8)))
          (: bs (ev '(\ s ((: (go i a) (? (< i (tally s)) (go (+ i 1) (+ a (s i))) a)) 0 0)))
             (&& (294 = (bs "abc")) (0 = (bs ""))))
          (: pk (ev '(\ s n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (s (- i 2)))) a)) 0 0)))
             (390 = (pk "ab" 6)))                                                 ; 97+98+97+98: both negatives read from the END, and past-end is () (joins nothing on)
          (: mx (ev '(\ f n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (f i))) a)) 0 0)))
             (&& (90 = (mx (\ x (* x 2)) 10)) (294 = (mx "abc" 5))))              ; one site, both callee kinds; 97+98+99, the two past-end reads joining nothing on
          (: ct (ev '(\ l n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (tally l))) a)) 0 0)))
             (&& (12 = (ct '(1 2 3) 4)) (8 = (ct "ab" 4))))                       ; chain tally drives; text tally native
          (: ts (ev '(\ tally n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (tally i))) a)) 0 0)))
             (10 = (ts (\ x (+ x 1)) 4)))
          ; SIBLING CALLEES (v6): a letrec lambda sibling is a free head in the weave re-analysis
          ; surface; ala's lams hands amble the cell (entry . import-names) lz freezes into the
          ; bytecode site, so the call curries the frozen entry -- NO staleness bet. the digrun
          ; shape is moon's lexer verbatim (infix singletons + the sugared trigger named-let +
          ; the backtick's macro-spliced link VALUES as heads -- adesug owns all three).
          (: n2 (peep fires () 0)
             lx (ev '(: (dig? c) (? (< c 58) (< 47 c) ())
                        (digrun s n) (: (go k v) (? (&& (k < n) (dig? (s k))) (go (k + 1) ((* 10 v) + ((s k) - 48))) v) (go 0 0))
                        digrun))
             (&& (31415 = (lx "31415x" 6)) (0 = (lx "x" 1)) (fired? n2)))         ; the lexer class AMBLES (fires) and == interp
          (: mk (ev '(\ b (: (addb x) (+ x b)
                             (suml l) ((: (go l a) (? (two? l) (go (cup l) (+ a (addb (cap l)))) a)) l 0)
                             suml)))
             (306 = ((mk 100) '(1 2 3))))                                         ; a sibling WITH an import: entry curried through the frame
          (: eo (ev '(: (ev? n) (? (< n 1) 1 (od? (- n 1)))
                        (od? n) (? (< n 1) 0 (ev? (- n 1)))
                        (cnt l) ((: (go l a) (? (two? l) (go (cup l) (+ a (od? (cap l)))) a)) l 0)
                        cnt))
             (4 = (eo '(1 2 3 4 5 6 7))))                                         ; mutual recursion: the later sibling drives the earlier's entry
          ; the DEOPT BUDGET: a per-call restarter ((= a b) on heap values, fx2 fails every call)
          ; spends its budget and RETIRES -- every later call takes d0 straight to the twin, so a
          ; perma-deopting native heals to bytecode; correct at every point on the way.
          (: hp (ev '(\ l q ((: (go l a) (? (two? l) (go (cup l) (? (= (cap l) q) (+ a 1) a)) a)) l 0)))
             (: (chk i bad) (? (< i 40) (chk (+ i 1) (? (= 2 (hp '("ab" "cd" "ab" "x") "ab")) bad (+ bad 1))) bad)
                (0 = (chk 0 0))))))
   (say err "test/glaze-x86: amble -- the boring-tier translator (walker + cons + globals + siblings + the deopt budget) ok\n"))

; GROUP SWALLOW helpers (for the swallow test in the AUTO block below): genuine top-level globals,
; so the LOADER auto-ev's them and srcrec records their source (a body-less `:` define -- inside a
; base-ev block they'd be local + un-recorded). swq is a leaf callee; swsum has a named-let loop,
; so its swallow exercises the STRUCTURAL lift pass.
(: (swq n) (+ (* n n) 1))
(: (swsum n) (: (go i a) (? (< i 1) a (go (- i 1) (+ a i))) (go n 0)))

(base-ev '(:
 _ (assert
    ((\ x (+ (* x 2) 3)) = (ev '(\ x (+ (* x 2) 3))))          ; leaf: native, =-identical
    (23 = ((ev '(\ x (+ (* x 2) 3))) 10))
    (((base-ev '(\ x (+ (* x 9) 4))) 11) = ((ev '(\ x (+ (* x 9) 4))) 11))      ; native leaf == interp (catches njit codegen regressions)
    (: ny (ev '(\ y (+ y 1))) (&& (6 = (ny 5)) ((\ y (+ y 1)) = ny)))   ; arity-1 leaf, non-x param, auto-natived
    (3 = ((ev '(\ x (cap x))) '(3 4)))                         ; non-qualifying -> interpreter
    ((base-ev '(+ 1 2 3)) = (ev '(+ 1 2 3)))                   ; non-lambda forms unaffected (ev == base-ev; curried + nets 27, not 6)
    (9000000000000000000 = ((ev '(\ x (* x x))) 3000000000))   ; overflow deopts -> bignum
    (: ab (ev '(\ x (? (< x 0) (- 0 x) x))) (&& (7 = (ab -7)) (7 = (ab 7))))   ; conditional leaf auto-natived
    ; LOOP auto-detected -> native counted loop, =-identical + correct
    (: L1 '(\ n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (* i 7))) a)) 0 0))
       f  (ev L1)
       (&& (= f (ev L1)) (3496500 = (f 1000)) (two? (loopinfo L1))
           (id? (ev L1) (ev L1))))                                  ; CACHE: same source -> same native (compiled once, memoized)
    ; LOOP with a conditional body (sum i for i<50, capped) auto-detected
    (: L2 '(\ n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (? (< i 50) i 0))) a)) 0 0))
       h  (ev L2)  c ((: (go i a) (? (< i 1000) (go (+ i 1) (+ a (? (< i 50) i 0))) a)) 0 0)
       (&& (two? (loopinfo L2)) (= c (h 1000))))
    ; GENERALIZED loop -- a PRODUCT fold (op=*, init 1): n! = prod_{i<n}(i+1)
    (: P '(\ n ((: (go i a) (? (< i n) (go (+ i 1) (* a (+ i 1))) a)) 0 1))
       f (ev P)  fi ((: (go i a) (? (< i 10) (go (+ i 1) (* a (+ i 1))) a)) 0 1)
       (&& (two? (loopinfo P)) (3628800 = (f 10)) (= fi (f 10)) (= (ev P) (ev P))
           (15511210043330985984000000 = (f 25))))             ; overflow -> deopt -> bignum (transparent)
    ; GENERALIZED loop -- a <= comparison: sum_{i<=n} i
    (: Q '(\ n ((: (go i a) (? (<= i n) (go (+ i 1) (+ a i)) a)) 0 0))
       g (ev Q)  (&& (two? (loopinfo Q)) (55 = (g 10))))        ; 0+1+..+10 = 55
    ; N-VARIABLE loop auto-detected: iterative fib (i,a,b -- updates b and a+b)
    (: F '(\ n ((: (go i a b) (? (< i n) (go (+ i 1) b (+ a b)) a)) 0 0 1))
       nf (ev F)  (&& (two? (loopinfo-n F)) (832040 = (nf 30)) (= (ev F) (ev F))   ; fib(30)=832040, native+transparent
                      (4660046610375530309 = (nf 91))                              ; fib(91) -> deopt -> bignum
                      (354224848179261915075 = (nf 100))))                         ; fib(100) overflows -> C-FINITE matrix-power deopt, exact bignum
    ; C-FINITE deopt on a constant-scaled (* 2 b) update: pell(60) overflows int64, so the native loop
    ; deopts to the matrix-power closed form -- which must equal the interpreted loop (base-ev) exactly.
    (: PL '(\ n ((: (go i a b) (? (< i n) (go (+ i 1) b (+ (* 2 b) a)) a)) 0 0 1))
       pf (ev PL)  (&& (two? (loopinfo-n PL)) (= ((base-ev PL) 60) (pf 60)) (= 12 (pf 4))))  ; pell(60) deopt == interp; pell(4)=12 in-range
    (id? (ev '(\ x (+ x 1))) (ev '(\ x (+ x 1))))
    (: ad (ev '(\ x y (+ (* x x) y))) (&& (13 = (ad 3 4)) (13 = ((ad 3) 4)) ((\ x y (+ (* x x) y)) = ad)))  ; 2-arg leaf auto-natived
    (: a3 (ev '(\ x y z (+ (+ x y) (* z 3))))                                                  ; 3-arg leaf auto-natived, currying at any split
       (&& (13 = (a3 1 3 3)) (13 = ((a3 1 3) 3)) (13 = ((a3 1) 3 3)) ((\ x y z (+ (+ x y) (* z 3))) = a3)))
    ; captured-closure constructor auto-natived: captures transparent (= interp)
    (: adder (ev '(\ n (\ x (+ x n))))  a5 (adder 5)
       (&& (15 = (a5 10)) ('(6 7 8) = (map a5 '(1 2 3))) (= a5 ((\ n (\ x (+ x n))) 5))))
    ; FLOAT-RESULT leaf auto-natived -> a heap-boxed gem, == interp, transparent
    (: fr (ev '(\ x (+ (* x 1.5) 2.0)))
       (&& (gem? (fr 4)) (8.0 = (fr 4)) (= ((base-ev '(\ x (+ (* x 1.5) 2.0))) 7) (fr 7))
           ((\ x (+ (* x 1.5) 2.0)) = fr)))                     ; =-identical to source
    (qualfr '(\ x (/ (+ x 1.0) 2.0)))                          ; float-division leaf: param floated at first use -> SOUND
    !(qualfr '(\ x (+ (* x 3) 1.5)))                           ; param inside an INT subexpr (* x 3) -> late promotion diverges -> rejected
    !(qualfr '(\ x (/ x 2)))                                   ; int/int division: result type value-dependent -> rejected
    !(qualfr '(\ x (+ (* x 2) 3)))                             ; an all-INTEGER leaf is no float result (qual handles it)
    (: ii (ev '(\ x (+ (* x 2) 3))) (&& (7 = (ii 2)) !(gem? (ii 2)))))   ; ... and stays a fixnum (qualfr did not steal it)
 _ (assert    ; D-DIMENSIONAL float-recurrence GRIDS auto-glazed from SOURCE -> native whole-grid kernel, bit-exact (== interp)
    ; D=2 -- the mandelbrot 64x64 grid (M=2 invariants, N=2 fvars), the canonical instance -> checksum 87130
    (: g2 '(: (mand cx cy zx zy it)
               (? (&& (< it 100) (<= (+ (* zx zx) (* zy zy)) 4.0))
                  (mand cx cy (+ (- (* zx zx) (* zy zy)) cx) (+ (* (* 2.0 zx) zy) cy) (+ it 1)) it)
              (col px py acc) (? (< py 64) (col px (+ py 1) (+ acc (mand (+ -2.0 (* px 0.046875)) (+ -1.5 (* py 0.046875)) 0.0 0.0 0))) acc)
              (grid px acc) (? (< px 64) (grid (+ px 1) (+ acc (col px 0 0))) acc)
            (grid 0 0))
       (&& !(id? (autonat g2) g2) (= 87130 (ev g2)) (= (base-ev g2) (ev g2))))      ; autonat rewrote it -> native == interp
    ; D=1 -- a single row of escape counts (cy a constant 0.0) -> native == interp
    (: g1 '(: (mand cx cy zx zy it) (? (&& (< it 100) (<= (+ (* zx zx) (* zy zy)) 4.0)) (mand cx cy (+ (- (* zx zx) (* zy zy)) cx) (+ (* (* 2.0 zx) zy) cy) (+ it 1)) it)
              (row px acc) (? (< px 64) (row (+ px 1) (+ acc (mand (- (* px 0.046875) 2.0) 0.0 0.0 0.0 0))) acc)
            (row 0 0))
       (&& !(id? (autonat g1) g1) (= (base-ev g1) (ev g1))))
    ; D=3 -- an 8x8x8 nest, M=3 invariants (cx cy cz), cz=pz*0.01 shifting zy -> native == interp (exercises r12 push/pop)
    (: g3 '(: (mand cx cy cz zx zy it) (? (&& (< it 50) (<= (+ (* zx zx) (* zy zy)) 4.0)) (mand cx cy cz (+ (- (* zx zx) (* zy zy)) cx) (+ (+ (* (* 2.0 zx) zy) cy) cz) (+ it 1)) it)
              (lz px py pz acc) (? (< pz 8) (lz px py (+ pz 1) (+ acc (mand (- (* px 0.046875) 2.0) (- (* py 0.046875) 1.5) (* pz 0.01) 0.0 0.0 0))) acc)
              (ly px py acc) (? (< py 8) (ly px (+ py 1) (+ acc (lz px py 0 0))) acc)
              (lx px acc) (? (< px 8) (lx (+ px 1) (+ acc (ly px 0 0))) acc)
            (lx 0 0))
       (&& !(id? (autonat g3) g3) (= (base-ev g3) (ev g3))))
    ; TWIN(complex) grid: idiomatic twin code -- twolow lowers c/z to real-pairs, autonat glazes
    ; it, bit-exact (== the interpreted twins). The D=2 mandelbrot with z<-z*z+c written in twin gems.
    (: g2t '(: (mand c z it)
                 (? (&& (< it 100) (<= (+ (* (re z) (re z)) (* (im z) (im z))) 4.0)) (mand c (+ (* z z) c) (+ it 1)) it)
               (col px py acc) (? (< py 64) (col px (+ py 1) (+ acc (mand (twin (+ -2.0 (* px 0.046875)) (+ -1.5 (* py 0.046875))) (twin 0.0 0.0) 0))) acc)
               (grid px acc) (? (< px 64) (grid (+ px 1) (+ acc (col px 0 0))) acc)
             (grid 0 0))
       (&& !(id? (twolow g2t) g2t)                                                    ; twolow rewrote the twin grid
           !(id? (autonat (twolow g2t)) (twolow g2t))                                 ; ... and autonat then glazes the real-pair form
           (= 87130 (ev g2t)) (= (base-ev g2t) (ev g2t))))                            ; native == interp, same checksum as the plain-float g2
    ; RECURSIVE ARITHMETIC: naive fib auto-glazed to a native call tree -- native == interp, and an
    ; int64-overflowing recursion (pow2) deopts to the interp fallback -> the exact bignum (sound)
    (: rf '(: (fib n) (? (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))) (fib 25))
       (&& !(id? (autogroup rf) rf) (= 75025 (ev rf)) (= (base-ev rf) (ev rf))))
    ; SOUNDNESS REGRESSION: a BODY-LESS group `:` (the global-leaking top-level-define form -- sat.l's
    ; shape) is DECLINED (returned UNCHANGED), NOT glazed. The lift pipeline can't preserve the leak, so
    ; glazing it would silently miscompile the leaked globals to const-1 (broke sat.l's cdcl under the
    ; image). Body-LESS = even # of forms after `:`. (rf above is body-HAVING -> still glazes.)
    (: bl '(: (fib n) (? (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))  (id? (autogroup bl) bl))
    (: rp '(: (p2 n) (? (< n 1) 1 (* 2 (p2 (- n 1)))) (p2 70))
       (= (base-ev rp) (ev rp)))                               ; 2^70 overflows int64 in H -> OVF deopt -> bignum
    ; MULTI-ARG recursion: tak (3 args, nested self-calls -- args survive on the machine stack) == interp
    (: rt '(: (tak x y z) (? (< y x) (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)) z) (tak 18 12 6))
       (&& !(id? (autogroup rt) rt) (= 7 (ev rt)) (= (base-ev rt) (ev rt))))
    ; GROUP SWALLOW (always-on): a group member calling an EXTERNAL global int fn. The callee's
    ; recorded source folds in as an in-blob sibling under the group's one OVF/deopt, so native ==
    ; interp. swq (above) is a leaf callee; swsum's named-let lifts -> a STRUCTURAL swallow.
    (: rsw '(: (rsw1 n) (? (< n 1) 0 (+ (swq n) (rsw1 (- n 1)))) (rsw1 5))                    ; sum swq(1..5) = 2+5+10+17+26 = 60
       rst '(: (rst1 n) (? (< n 1) 0 (+ (swsum n) (rst1 (- n 1)))) (rst1 4))                 ; sum swsum(1..4) = 1+3+6+10 = 20
       (&& !(id? (autogroup rsw) rsw) (= 60 (ev rsw)) (= (base-ev rsw) (ev rsw))             ; leaf callee swallowed -> native == interp
           !(id? (autogroup rst) rst) (= 20 (ev rst)) (= (base-ev rst) (ev rst))))           ; named-let callee (lift) swallowed too
    ; MUTUAL recursion: evn/odn cross-call -- BOTH bindings glazed, sibling calls native inside one
    ; buffer; the `:` body drives both. The systematic win over the old per-fn autorec: a closed call
    ; GRAPH, not just self-recursion.
    (: rm '(: (evn n) (? (= n 0) 1 (odn (- n 1))) (odn n) (? (= n 0) 0 (evn (- n 1))) [(evn 100) (odn 100) (evn 7)])
       (&& !(id? (autogroup rm) rm) ('(1 0 0) = (ev rm)) (= (base-ev rm) (ev rm))))
    ; LAMBDA-LIFTING: primes' prime?/count are each a named-let LOOP -- lifted to flat group fns
    ; (prime?_go count_go), so the WHOLE trial-division graph glazes (prime? count prime?_go count_go,
    ; sibling calls native, the % trial division in the H). count(2 30) = 10 primes below 30.
    (: rpr '(: (prime? n) ((: (go d) (? (< n (* d d)) 1 (? (= 0 (% n d)) 0 (go (+ d 1))))) 2)
               (count lo hi) ((: (go n c) (? (< n hi) (go (+ n 1) (? (prime? n) (+ c 1) c)) c)) lo 0)
               (count 2 30))
       (&& !(id? (autogroup rpr) rpr) (10 = (ev rpr)) (= (base-ev rpr) (ev rpr))))
    ; STRING LANE: a recursive (peep s i) byte processor glazes from SOURCE -- s a STRING param (passed
    ; raw, no sar; the native reads the plain ai_str's bytes at s+16), (tally s) -> the len at s+8.
    (: rsl '(: data (string [72 105 90 33 65])
               (bsum s i) (? (< i 0) 0 (+ (peep s i 0) (bsum s (- i 1))))
               (bsum data 4))
       (&& !(id? (autogroup rsl) rsl) (365 = (ev rsl)) (= (base-ev rsl) (ev rsl))))
    ; FULL STRING LANE (strscan): fold-consts inlines hmod, norm-let normalizes (: n (tally s) (go ..)
    ; (go 0 0)) into a named-let, lift lifts go, interprocedural inference types s (it flows strhash ->
    ; strhash_go's peep), and the native rolling hash == interp. This is the strscan bench, glazed from source.
    (: rss '(: hmod 1000000007
               (strhash s) (: n (tally s) (go i h) (? (< i n) (go (+ i 1) (% (+ (* h 31) (peep s i 0)) hmod)) h) (go 0 0))
               data (string [72 105 90 33 65])
               (strhash data))
       (&& !(id? (autogroup rss) rss) (69709145 = (ev rss)) (= (base-ev rss) (ev rss))))
    ; CHAIN LANE: a binary-tree node-count fold glazes from SOURCE -- t a CHAIN param (raw pointer, NO
    ; entry guard, since two? is total: a non-chain reads false), with (two? t)/(cap t)/(cup t) lowered
    ; inline (cap/cup chain-guarded, deopting a non-chain) and the recursive ck a native sibling call.
    ; == interp, transparent. This is the `tree` bench's traverse half (the allocating mk stays interp).
    (: rcl '(: (mk d) (? (< d 1) 0 (link (mk (- d 1)) (mk (- d 1))))
               (ck t) (? (two? t) (+ 1 (+ (ck (cap t)) (ck (cup t)))) 0)
               (ck (mk 8)))
       (&& !(id? (autogroup rcl) rcl) (255 = (ev rcl)) (= (base-ev rcl) (ev rcl))))      ; depth 8 = 2^8-1 = 255 nodes
    ; CHAIN-LANE SOUNDNESS: a cap feeding ARITHMETIC ((+ (cap t) 0)) is a chain word in an int slot --
    ; the recognizer REJECTS it (cap is chain-valued), so it stays interpreted; cap under two? is fine.
    (: rcb '(: (g t) (+ (cap t) 0) (g (link 7 8)))
       (&& (id? (autogroup rcb) rcb) (= (base-ev rcb) (ev rcb))))                         ; NOT rewritten (rejected) -> == interp
    ; PARTIAL LIFT (plift): the hash bench's `scan` -- a READ-ONLY loop buried among the allocating
    ; ins/bump inside hash-run, sharing the mutable map h -- HOISTS to a top-level sibling `hash-run_scan`
    ; threading (n h), the two (scan 0 0) call sites rewritten. `lift` can't (3 sibling fns + a compound
    ; result); plift can. Tested via base-ev (the SOURCE->SOURCE transform preserves meaning, hash-run(N)
    ; = N^2): the hoisted map-read still needs the MAP lane (groupok?/jitgroupir) to glaze SOUNDLY -- the string
    ; peep-recognizer would mis-admit it -- so plift is NOT yet on the autogroup chain.
    (: rpl '(: (hash-run n)
                (: h (tablet 0)
                   (ins i)      (? (< i n) (: _ (pin h (+ 1 (* 97 i)) i) (ins (+ i 1))) h)
                   (scan i acc) (? (< i n) (scan (+ i 1) (+ acc (peep h (+ 1 (* 97 i)) 0))) acc)
                   (bump i)     (? (< i n) (: k (+ 1 (* 97 i)) _ (pin h k (+ 1 (peep h k 0))) (bump (+ i 1))) h)
                   _  (ins 0) a (scan 0 0) __ (bump 0) (+ a (scan 0 0)))
                (hash-run 30))
       pl (plift rpl)
       (&& !(id? pl rpl) (900 = (base-ev rpl)) (900 = (base-ev pl))                       ; fired + source->source semantic equivalence
           (member? 'hash-run_scan (map (\ pr (? (two? (cap pr)) (caap pr) (cap pr))) (cap (anat-defs (cup pl)))))))   ; scan HOISTED to a top-level sibling
    ; MAP LANE: a read-only map probe glazes from SOURCE -- `h` a MAP param (raw pointer, mapkind entry
    ; guard; the native hashes the key 0x9e37.. then linear-probes the open-addressed backing -- regs
    ; rax/r8-r11 only, NEVER rcx=Sp or rdx=Hp, both LIVE for the interp after Continue). mapfix rewrites
    ; (peep h k 0)->(mpeep h k 0) because h is peep'd but never tally'd (strscan's s is BOTH -> stays a
    ; string). == interp; a non-map arg deopts via the guard.
    (: rmap '(: (msum m i acc) (? (< i 5) (msum m (+ i 1) (+ acc (peep m (+ 1 (* 97 i)) 0))) acc)
                h (tablet 0) _ (pin h 1 10) _ (pin h 98 20) _ (pin h 195 30) _ (pin h 292 40) _ (pin h 389 50)
                (msum h 0 0))
       (&& !(id? (autogroup rmap) rmap) (150 = (ev rmap)) (= (base-ev rmap) (ev rmap))))
    ; THE HASH BENCH, glazed END TO END: plift hoists the read-only `scan` out of hash-run to a top-level
    ; hash-run_scan(n h i acc), mapfix maps its peep, the allocating ins/bump + the pins stay interpreted.
    ; == interp (hash-run(N) = N^2). The map lane owns the read; the build stays on the collector.
    (: rhash '(: (hash-run n) (: h (tablet 0)
                  (ins i)      (? (< i n) (: _ (pin h (+ 1 (* 97 i)) i) (ins (+ i 1))) h)
                  (scan i acc) (? (< i n) (scan (+ i 1) (+ acc (peep h (+ 1 (* 97 i)) 0))) acc)
                  (bump i)     (? (< i n) (: k (+ 1 (* 97 i)) _ (pin h k (+ 1 (peep h k 0))) (bump (+ i 1))) h)
                  _ (ins 0) a (scan 0 0) __ (bump 0) (+ a (scan 0 0)))
                (hash-run 50))
       (&& !(id? (autogroup rhash) rhash) (2500 = (ev rhash)) (= (base-ev rhash) (ev rhash))))   ; hash-run(50) = 2500
    ; DEFORESTATION: a map/filter/fold pipeline over (jot N) -- defoliate FUSES the producers into the
    ; foldl step (one pass, no intermediate lists) and LOWERS foldl-over-jot to the named-let LOOP `lift`
    ; already glazes, so the whole combinator pipeline compiles to a native counted loop. == interp.
    (: dq '(: (sumsq n) (foldl (+) 0 (map (\ x (* x x)) (jot n))) (sumsq 100))
       (&& !(id? (autogroup dq) dq) (328350 = (ev dq)) (= (base-ev dq) (ev dq))))      ; sum i^2 for i<100
    ; map after FILTER: the filter guards the fused step; map-after-map composes the element subs
    (: dpf '(: (f n) (foldl (+) 0 (map (+ 1) (filter (\ x (> x 2)) (jot n)))) (f 10))
       (&& !(id? (autogroup dpf) dpf) (49 = (ev dpf)) (= (base-ev dpf) (ev dpf))))      ; keep>2, +1, sum = 49
    (: dmm '(: (sm n) (foldl (+) 0 (map (* 2) (map (+ 1) (jot n)))) (sm 5))
       (&& !(id? (autogroup dmm) dmm) (30 = (ev dmm)) (= (base-ev dmm) (ev dmm))))      ; (*2).(+1) over 0..4 = 30
    ; a BARE foldl over jot (no producers) lowers straight to the loop
    (: dj '(: (s n) (foldl (+) 0 (jot n)) (s 100))
       (&& !(id? (autogroup dj) dj) (4950 = (ev dj)) (= (base-ev dj) (ev dj))))         ; sum 0..99 = 4950
    ; SOUNDNESS: a foldl over (iota N) is the ARRAY -- foldl/map (chain ops) don't iterate it, so it is
    ; degenerate (=z); fusion preserves that (both sides z) and it is NOT lowered (djot? only) -> == interp
    (: di '(: (h n) (foldl (+) 0 (map (\ x (* x x)) (iota n))) (h 100))
       (= (base-ev di) (ev di)))                                                         ; iota: degenerate to 0, transparent
    ; a foldl over a list PARAM can't lower (no jot) -> interpreted, value still correct (defoliate no-op)
    (: dl '(: (g l) (foldl (+) 0 l) (g (jot 5)))
       (= (base-ev dl) (ev dl)))                                                         ; 0+1+2+3+4 = 10
    ; foldr: a DOWN-counting reduction -- the fused down-loop lambda-lifts to a group fn and natives (the
    ; operator consumer flattens to a first-order step). == interp, value exact.
    (: dr '(: (f n) (foldr (+) 0 (map (\ x (* x x)) (jot n))) (f 100))
       (&& !(id? (autogroup dr) dr) (328350 = (ev dr)) (= (base-ev dr) (ev dr))))        ; foldr +.map sq, sum i^2 for i<100
    (: drf '(: (f n) (foldr (+) 0 (filter (\ x (= 1 (% x 2))) (jot n))) (f 20))
       (&& !(id? (autogroup drf) drf) (100 = (ev drf)) (= (base-ev drf) (ev drf))))      ; foldr +.filter odd, 1+3+..+19 = 100
    ; LOOP-CLOSER (loopclose): a polynomial fold over jot (optionally odd/even-filtered) collapses to a
    ; CLOSED FORM in the bound (no loop), == interp. Runs before defoliate, so the jot-fold cases above
    ; (dq/dj/dr/drf) now close HERE; non-period-2 filters (dpf) and map-of-map (dmm) still defoliate.
    (: lcsq '(: (f n) (foldl (+) 0 (map (\ x (* x x)) (jot n))) (f 100))
       (&& !(id? (autogroup lcsq) lcsq) (328350 = (ev lcsq)) (= (base-ev lcsq) (ev lcsq))))   ; Σ i^2 for i<100, closed
    (: lcid '(: (f n) (foldl (+) 0 (jot n)) (f 100))
       (&& !(id? (autogroup lcid) lcid) (4950 = (ev lcid)) (= (base-ev lcid) (ev lcid))))     ; Σ i for i<100 = C(100,2)
    ; the SCEV-beating `deforest` case: Σ of the odd squares, the odd-filter reparametrized k=2j+1
    (: lcodd '(: (f n) (foldl (+) 0 (map (\ x (* x x)) (filter (\ x (= 1 (% x 2))) (jot n)))) (f 20000))
       (&& !(id? (autogroup lcodd) lcodd) (1333333330000 = (ev lcodd)) (= (base-ev lcodd) (ev lcodd))))   ; Σ odd k^2 -> a literal, SCEV keeps the loop
    ; SYMBOLIC bound: the closed form is a polynomial in n, exact at MULTIPLE n (not one folded point)
    (: lcvar '(: (f n) (foldl (+) 0 (map (\ x (* x x)) (jot n))) (+ (f 7) (* 1000 (f 13))))
       (= (base-ev lcvar) (ev lcvar)))                                                        ; f(7)+1000*f(13), the closed form reused at two n
    ; DECLINE 1: a non-period-2 filter (>2) is left to defoliate; value preserved
    (: lcdec '(: (f n) (foldl (+) 0 (map (\ x (+ x 1)) (filter (\ x (> x 2)) (jot n)))) (f 10))
       (= (base-ev lcdec) (ev lcdec)))                                                        ; keep>2,+1 -> defoliate
    ; DECLINE 2: a piecewise body quals but is NOT polynomial -> the difference table never dies -> declines
    (: lcnp '(: (f n) (foldl (+) 0 (map (\ x (? (< x 5) (* x x) 0)) (jot n))) (f 12))
       (= (base-ev lcnp) (ev lcnp)))                                                          ; 0+1+4+9+16 = 30, defoliated/interpreted
    ; NAMED-LET ACCUMULATOR (loopclose, generalized 2026-06-30): a HAND-WRITTEN counting accumulator
    ; `(NM i a) (? (< i N) (NM (+ i 1) (+ a B)) a)` with B a pure polynomial in i closes to the CLOSED sum
    ; (O(1)) -- like the foldl form, but reached through a `:` binding, so a raw recursive loop collapses,
    ; not just a combinator pipeline. "did it fire" is STRUCTURAL (!(= (show ..)) -- the self-call is gone).
    (: lcacc '(: (hof i acc) (? (< i 100) (hof (+ i 1) (+ acc (+ i (+ i i)))) acc) (hof 0 0))
       (&& !(= (show (autogroup lcacc)) (show lcacc)) (14850 = (ev lcacc)) (= (base-ev lcacc) (ev lcacc))))   ; Σ 3k for k<100 = 3·4950
    ; the GUARD keeps the rewritten function faithful at a NON-ZERO start (i=50, i>=N) -- reused at two starts
    (: lcacc2 '(: (hof i acc) (? (< i 100) (hof (+ i 1) (+ acc (+ i (+ i i)))) acc) (+ (hof 0 0) (* 7 (hof 50 1000))))
       (= (base-ev lcacc2) (ev lcacc2)))                                                       ; (hof 0 0) + 7*(hof 50 1000), closed form reused
    ; DECLINE: a non-polynomial accumulator body (%) -> difference table never dies -> stays a loop, value kept
    (: lcaccd '(: (g i acc) (? (< i 50) (g (+ i 1) (+ acc (% (* i i) 7))) acc) (g 0 0))
       (= (base-ev lcaccd) (ev lcaccd)))
    ; CASK BUILDER (castbuild): a string-accumulator loop -- (: (go i s) (? COND (go .. (+ s X)) s) (go 0 "")) --
    ; rewrites to a threaded `jug` (O(1)-amortized append + one drain), == interp. "did it fire" is STRUCTURAL
    ; (!(= ..)) -- castbuild rebuilds via map, so id? never holds even on a no-op.
    (: cb1 '(: (build n) (: (go i s) (? (< i n) (go (+ i 1) (+ s (string (+ 48 (% i 10))))) s) (go 0 "")) (tally (build 100)))
       (&& !(= (castbuild cb1) cb1) (100 = (ev cb1)) (= (base-ev cb1) (ev cb1))))             ; a 100-char build -> jug, == interp
    ; the strcat bench shape: strhash glazes (string lane) WHILE strbuild fills a cask -- both in one form, hash exact
    (: cb2 '(: hmod 1000000007 (strhash s) (: n (tally s) (go i h) (? (< i n) (go (+ i 1) (% (+ (* h 31) (peep s i 0)) hmod)) h) (go 0 0))
               (strbuild n) (: (go i s) (? (< i n) (go (+ i 1) (+ s (string (+ 48 (% i 10))))) s) (go 0 "")) (strhash (strbuild 500)))
       (&& !(= (castbuild cb2) cb2) (= (base-ev cb2) (ev cb2))))                              ; build 500 chars then rolling-hash
    ; MULTI-CHAR append (the say path): a 2-char piece per step -> (say s (string [.. ..]))
    (: cb3 '(: (build n) (: (go i s) (? (< i n) (go (+ i 1) (+ s (string [(+ 65 (% i 5)) 45]))) s) (go 0 "")) (tally (build 30)))
       (&& !(= (castbuild cb3) cb3) (60 = (ev cb3)) (= (base-ev cb3) (ev cb3))))              ; 30 steps x 2 chars = 60
    ; ORIENTATION: the recursion in the ELSE branch -- (? (>= i n) s (go ..)) -- handled, branch order preserved
    (: cb4 '(: (build n) (: (go i s) (? (>= i n) s (go (+ i 1) (+ s (string 88)))) (go 0 "")) (tally (build 40)))
       (&& !(= (castbuild cb4) cb4) (40 = (ev cb4)) (= (base-ev cb4) (ev cb4))))
    ; DECLINE: s is also READ (via (tally s)) so the recur arg is not a bare (+ s X) -> NOT a pure append -> left alone
    (: cb5 '(: (build n) (: (go i s) (? (< i n) (go (+ i 1) (? (< (tally s) 3) (+ s (string 65)) s)) s) (go 0 "")) (tally (build 10)))
       (&& (= (castbuild cb5) cb5) (= (base-ev cb5) (ev cb5))))                               ; not rewritten, value still exact
    ; all/any: a short-circuit predicate -> an early-exit loop returning the truth BITS 0/1 (== interp, which
    ; now returns bits too). a clean run-off and an early short-circuit both glaze, with or without a producer.
    (: dall '(: (f n) (all (\ x (< x n)) (jot n)) (f 10))
       (&& !(id? (autogroup dall) dall) (1 = (ev dall)) (= (base-ev dall) (ev dall))))   ; every i<n -> run off -> 1
    (: dalf '(: (f n) (all (\ x (< x 5)) (jot n)) (f 10))
       (&& !(id? (autogroup dalf) dalf) (0 = (ev dalf)) (= (base-ev dalf) (ev dalf))))   ; i=5 fails -> early 0
    (: dany '(: (f n) (any (\ x (> x 7)) (jot n)) (f 10))
       (&& !(id? (autogroup dany) dany) (1 = (ev dany)) (= (base-ev dany) (ev dany))))   ; i=8 hits -> early 1
    (: danf '(: (f n) (any (\ x (> x 100)) (jot n)) (f 10))
       (&& !(id? (autogroup danf) danf) (0 = (ev danf)) (= (base-ev danf) (ev danf))))   ; none -> run off -> 0
    (: dap '(: (f n) (all (\ x (> x 0)) (map (+ 1) (jot n))) (f 10))
       (&& !(id? (autogroup dap) dap) (1 = (ev dap)) (= (base-ev dap) (ev dap))))        ; all (+1 i)>0 -> producer fuses into the guard
    ; a fn that calls OUT of the fragment (into `link`) drops the whole group -> form unchanged, interpreted
    (: ro '(: (build n) (? (< n 1) () (link n (build (- n 1)))) (build 3))
       (&& (id? (autogroup ro) ro) ('(3 2 1) = (ev ro))))   ; left interpreted; base () -> a clean ()-terminated (3 2 1)
    ; SPECIALIZATION (autospec/mix): a recursive interpreter ev1 applied to a STATIC program P (a const-
    ; bound quoted AST) + a DYNAMIC loop var specializes -- the dispatch + AST navigation fold away to P's
    ; straight-line arithmetic (the Horner residual), so the loop natives. == an interpreted ev1 loop (the
    ; 1st Futamura projection composed with the glaze; the dispatch overhead interp-spec.l clocks at ~6.5x).
    (: sp '(: (ev1 nd x) (? (id? (cap nd) 'lit) (caup nd)  (id? (cap nd) 'var) x
                            (id? (cap nd) 'add) (+ (ev1 (caup nd) x) (ev1 (cap (cuup nd)) x))
                            (id? (cap nd) 'mul) (* (ev1 (caup nd) x) (ev1 (cap (cuup nd)) x))  0)
               P '(add (mul (add (mul (add (mul (lit 2) (var)) (lit 3)) (var)) (lit 5)) (var)) (lit 7))
               (run n) ((: (go i a) (? (< i n) (go (+ i 1) (+ a (ev1 P i))) a)) 0 0)  (run 1000))
       (&& !(id? (autogroup sp) sp) (500001505000 = (ev sp)) (= (base-ev sp) (ev sp))         ; sum P(i) i<1000, native == interp
           (! (occurs? 'ev1 (nthl (autospec sp) 6)))))   ; ... and the interpreter ev1 FOLDS AWAY: the run body (elt 6) is pure Horner arithmetic, no dispatch -- guards specialization actually FIRING (the value tests above pass even if autospec degrades to a no-op)
    ; a DIFFERENT static program (a mul of two sub-exprs) specializes to its OWN residual -- == interp (soundness breadth)
    (: sp2 '(: (ev1 nd x) (? (id? (cap nd) 'lit) (caup nd)  (id? (cap nd) 'var) x
                             (id? (cap nd) 'add) (+ (ev1 (caup nd) x) (ev1 (cap (cuup nd)) x))
                             (id? (cap nd) 'mul) (* (ev1 (caup nd) x) (ev1 (cap (cuup nd)) x))  0)
                Q '(mul (add (var) (lit 1)) (add (var) (lit 2)))
                (run n) ((: (go i a) (? (< i n) (go (+ i 1) (+ a (ev1 Q i))) a)) 0 0)  (run 50))
       (&& !(id? (autogroup sp2) sp2) (= (base-ev sp2) (ev sp2))))   ; sum (i+1)(i+2) i<50, native == interp
    ; autospec is a NO-OP without BOTH a defined fn and a static-data binding -> a plain fib group is byte-unchanged
    (: nf '(: (fib n) (? (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))) (fib 10))
       (id? (autospec nf) nf))
    ; the DESCENT guard: a static-list recursion driven by a numeric counter. the comparison test
    ; folds static (< is a prim now) and the unfold shrinks its static weight every level, so the
    ; call specializes to its plain answer. the old same-SIGNATURE seen-set never caught this shape
    ; ((- n 1) never repeats a signature): it speculatively unrolled to the 100k-fuel floor and the
    ; compile hung -- a CONSTANT list bound beside a recursive local + one call was enough.
    (: sd '(: (f s n) (? (< n 1) s (f (cup s) (- n 1))) zz '(aa bb cc dd) (f zz 1))
       (&& ('(bb cc dd) = (ev sd)) (= (base-ev sd) (ev sd))))
    ; a test the specializer can't fold (%) -> the recursive arm is speculative; descent still
    ; bounds it (the list shrinks, then a non-descending re-entry residualizes) -- value exact.
    (: sr '(: (g s k) (? (= 1 (% k 2)) s (g (cup s) (+ k 1))) ww '(a b c) (g ww 0))
       (&& ('(b c) = (ev sr)) (= (base-ev sr) (ev sr))))
    ; CHURCH LOWERING (dechurch): a literal (N f x) with a FUNCTION operand is f^N(x) -- lowered to the
    ; nested application so the group lane glazes (church is otherwise opaque -> num-ap -> interp). An
    ; op-partial gives pure arithmetic; a lambda operand is beta-collapsed by dsimp. == interp, value exact.
    (: dch '(: (f i) (2 (+ i) i)                                                          ; (2 (+ i) i) = (+ i (+ i i)) = 3i
               ((: (go k a) (? (< k 100) (go (+ k 1) (+ a (f k))) a)) 0 0))
       (&& !(id? (autogroup dch) dch) (14850 = (ev dch)) (= (base-ev dch) (ev dch))))     ; Σ 3k for k<100 = 3·4950
    (: dchk '(: (f i) ((2 (+ i)) i)                                                       ; CURRIED surface form ((2 (+ i)) i) -- same value as (2 (+ i) i), must compile the same
                ((: (go k a) (? (< k 100) (go (+ k 1) (+ a (f k))) a)) 0 0))
       (&& !(id? (autogroup dchk) dchk) (14850 = (ev dchk)) (= (base-ev dchk) (ev dchk))))
    (: dchl '(: (f i) (2 (\ a (* a a)) i)                                                 ; (2 sqr i) = (sqr (sqr i)) = i^4
                ((: (go k a) (? (< k 10) (go (+ k 1) (+ a (f k))) a)) 0 0))
       (&& !(id? (autogroup dchl) dchl) (15333 = (ev dchl)) (= (base-ev dchl) (ev dchl))))  ; Σ k^4 for k<10
    ; POWER is NOT church: a NUMBER operand (tally /= 2, or a numeral) is exponentiation -- dechurch leaves
    ; it (still num-ap/interp), so the value stays exact. (2 i)=i^2, (2 3 i)=((2 3) i)=(9 i)=i^9.
    (: dpw '(: (g i) (2 i)   ((: (go k a) (? (< k 6) (go (+ k 1) (+ a (g k))) a)) 0 0))
       (&& (55 = (ev dpw)) (= (base-ev dpw) (ev dpw))))                                   ; Σ k^2 for k<6 = 55, untouched
    (: dpw2 '(: (h i) (2 3 i) ((: (go k a) (? (< k 4) (go (+ k 1) (+ a (h k))) a)) 0 0))
       (&& (20196 = (ev dpw2)) (= (base-ev dpw2) (ev dpw2))))                             ; Σ k^9 for k<4 = 20196, untouched
    ; LOCAL LET (delet): a fn body that NAMES intermediates -- a pure value-let -- inlines to one fragment
    ; expression, so the fn joins the group. == interp, value exact.
    (: lb '(: (f x) (: q (* x x) (+ q q)) (f 5))
       (&& !(id? (autogroup lb) lb) (50 = (ev lb)) (= (base-ev lb) (ev lb))))                ; q=x*x, 2q = 50
    ; a CHAIN of bindings, the first used TWICE -- but call-free (dup-cheap), so it inlines freely
    (: lc '(: (f x) (: a (+ x 1) b (* a 2) (+ a b)) (f 5))
       (&& !(id? (autogroup lc) lc) (18 = (ev lc)) (= (base-ev lc) (ev lc))))                ; a=6,b=12 -> 18
    ; a binding holding a recursive sibling CALL, used ONCE -> inlines; the fn glazes and calls native g
    (: ls '(: (g n) (? (< n 1) 0 (+ n (g (- n 1)))) (f n) (: h (g n) (* h 2)) (f 5))
       (&& !(id? (autogroup ls) ls) (30 = (ev ls)) (= (base-ev ls) (ev ls))))                ; g(5)=15, h*2 = 30
    ; a value-let WRAPPING a named-let loop: delet inlines k into the loop, lift then glazes it
    (: lw '(: (f n) (: k (* n 1) ((: (go i a) (? (< i k) (go (+ i 1) (+ a i)) a)) 0 0)) (f 10))
       (&& !(id? (autogroup lw) lw) (45 = (ev lw)) (= (base-ev lw) (ev lw))))                ; sum 0..9 = 45
    ; BAIL: a binding holding a CALL used TWICE would duplicate the recursion -> NOT inlined -> f interprets
    ; (sound, value exact; the sibling g still glazes, so the form is rewritten but f stays interpreted)
    (: lbail '(: (g n) (? (< n 1) 1 (* n (g (- n 1)))) (f n) (: a (g n) (+ a a)) (f 5))
       (= (base-ev lbail) (ev lbail)))                                                        ; g(5)=120, a+a = 240, interpreted
    ; NESTED in a sub-expression: a pure value-let in the accumulator ARG of a tail call -- the natural
    ; "bind an intermediate" idiom. delet-body recurses to surface it; without that it survives lift and
    ; the whole loop DEOPTS (slower than interp). x=(% i 13) is dup-cheap, so it inlines (used twice).
    (: ldeep '(: (f N) (: (lp i acc) (? (< i N) (lp (+ i 1) (: x (% i 13) (+ acc (* x x)))) acc) (lp 0 0)) (f 20))
       (&& !(id? (autogroup ldeep) ldeep) (741 = (ev ldeep)) (= (base-ev ldeep) (ev ldeep))))  ; sum (i%13)^2 for i<20, native == interp
    ; an EFFECTFUL nested let (a `_`-bound write used 0x) is NOT inlined -- dropping it would lose the
    ; effect; the let is kept (the fn stays interpreted, sound). The trailing pure step still glazes.
    (: leff '(: (f N) (: (lp i b) (? (< i N) (: _ (pin b 0 (+ 1 (peep b 0 0))) (lp (+ i 1) b)) (peep b 0 0)) (lp 0 (cask 1))) (f 7))
       (= (base-ev leff) (ev leff)))                                                          ; pin 7x -> 7, write preserved
    ; BOOLEANS & CLAMPS (debool): &&/||/min/max/abs lower to the ?-fragment -> the fn glazes. == interp.
    (: band '(: (f x n) (? (&& (< x n) (> x 0)) 1 0) (f 5 9))
       (&& !(id? (autogroup band) band) (1 = (ev band)) (= (base-ev band) (ev band))))        ; 5<9 && 5>0 -> 1
    (: bor '(: (f x) (? (|| (< x 0) (> x 9)) 1 0) (f 5))
       (&& !(id? (autogroup bor) bor) (0 = (ev bor)) (= (base-ev bor) (ev bor))))             ; neither -> 0
    (: babs '(: (f x) (+ (abs x) 1) (f -5))
       (&& !(id? (autogroup babs) babs) (6 = (ev babs)) (= (base-ev babs) (ev babs))))        ; |-5|+1 -> 6
    ; abs inside a hot loop: |i-50| summed for i<100 -> the running clamp natives
    (: babl '(: (f n) ((: (go i a) (? (< i n) (go (+ i 1) (+ a (abs (- i 50)))) a)) 0 0) (f 100))
       (&& !(id? (autogroup babl) babl) (2500 = (ev babl)) (= (base-ev babl) (ev babl))))      ; sum|i-50| = 2*(1+..+50) = 2550-50 = 2500
    ; nested compound: && of a cmp and an ||, guarding an abs -- all collapse to ?
    (: bnest '(: (f x) (? (&& (> x 0) (|| (< x 5) (> x 10))) (abs x) 0) (f 3))
       (&& !(id? (autogroup bnest) bnest) (3 = (ev bnest)) (= (base-ev bnest) (ev bnest))))    ; 3>0 && 3<5 -> abs 3 = 3
    ; BAIL: || whose first operand is a recursive CALL would duplicate it -> left un-lowered -> f interprets (sound)
    (: bbail '(: (g n) (? (< n 1) 0 (+ n (g (- n 1)))) (f n) (? (|| (g n) (< n 0)) 1 0) (f 5))
       (= (base-ev bbail) (ev bbail)))                                                        ; (|| g(5)=15 ..) truthy -> 1, interpreted (g still glazes)
    ; || AND abs nested in a loop accumulator -- the deep ?-nesting the perf demo exercises
    (: bdeep '(: (f n) ((: (go i a) (? (< i n) (go (+ i 1) (+ a (? (|| (< i 100) (> i 900)) (abs (- i 500)) 0))) a)) 0 0) (f 1000))
       (&& !(id? (autogroup bdeep) bdeep) (= (base-ev bdeep) (ev bdeep))))                     ; native == interp on the nested ||/abs loop
    ; a debool'd HELPER called by a loop SIBLING -- clamp (||/abs) glazes as a group fn, run_go calls it native
    (: bclamp '(: (clamp i) (? (|| (< i 100) (> i 900)) (abs (- i 500)) 0)
                  (run n) ((: (go i a) (? (< i n) (go (+ i 1) (+ a (clamp i))) a)) 0 0) (run 1000))
       (&& !(id? (autogroup bclamp) bclamp) (= (base-ev bclamp) (ev bclamp)))))                ; native == interp; clamp a glazed sibling
   (say err "love/glaze/auto: native codegen ok
")))

; --- the neutral-IR retarget, leaf lane (jitir, emit.l) -- additive alongside the byte path.
;     x86: the IR-emitted native == the interpreter across the grammar. arm64: the SAME source
;     assembles for aarch64 (DATA only -- executing it needs the qemu kernel harness), proving the
;     per-target register-role map. compiled through base-ev with ev->base-ev for interp references.
(base-ev '(: ev base-ev
   _ (assert
      ; LOUD dependency guard -- these glaze internals MUST be bound (cat'd from emit.l). an unbound name
      ; reads ()=the zero point, so (() x)=const-1=1: a SILENT miscompile that fakes a pass. assert lit?
      ; so an un-exposure (or a cat-order slip) SCARES here, `;; assert (lit? <name>)`, not downstream.
      (lit? assemble) (lit? jitir) (lit? jitnir) (lit? jitgroupir) (lit? coreir) (lit? deoptir)
      (lit? cggir) (lit? cggvir) (lit? consir) (lit? mkhir) (lit? mkouterir)
      ; x86 native == interp
      (31 = ((jitir '(\ x (+ (* x 3) 1)) 'x64) 10))
      (= ((ev '(\ x (+ (* x 3) 1))) 10) ((jitir '(\ x (+ (* x 3) 1)) 'x64) 10))
      (49 = ((jitir '(\ x (* x x)) 'x64) 7))
      (8 = ((jitir '(\ x (- x 2)) 'x64) 10)) (-8 = ((jitir '(\ x (- 2 x)) 'x64) 10))     ; - keeps a-b order
      (3 = ((jitir '(\ x (// x 3)) 'x64) 10)) (1 = ((jitir '(\ x (% x 7)) 'x64) 8))
      (8 = ((jitir '(\ x (& x 12)) 'x64) 10)) (3 = ((jitir '(\ x (| x 1)) 'x64) 2)) (15 = ((jitir '(\ x (^ x 5)) 'x64) 10))
      (1 = ((jitir '(\ x (< x 5)) 'x64) 3)) (0 = ((jitir '(\ x (< x 5)) 'x64) 9))         ; cmp -> set
      (1 = ((jitir '(\ x (<= x 5)) 'x64) 5)) (1 = ((jitir '(\ x (>= x 5)) 'x64) 9)) (1 = ((jitir '(\ x (= x 5)) 'x64) 5))
      (7 = ((jitir '(\ x (? (< x 0) (- 0 x) x)) 'x64) -7)) (0 = ((jitir '(\ x (? (< x 0) (- 0 x) x)) 'x64) 0))   ; abs (?)
      (9000000000000000000 = ((jitir '(\ x (* x x)) 'x64) 3000000000))                    ; overflow -> deopt -> bignum
      (16000000000000000001 = ((jitir '(\ x (+ 1 (* x x))) 'x64) 4000000000))             ; mid-nest overflow: sp unwind on deopt
      (= (// 10 0) ((jitir '(\ x (// x 0)) 'x64) 10))                                      ; /-by-0 guard -> deopt -> interp
      (= (+ '(1 2) 1) ((jitir '(\ x (+ x 1)) 'x64) '(1 2)))                                ; non-fixnum arg -> deopt -> interp
      ((\ x (+ (* x 3) 1)) = (jitir '(\ x (+ (* x 3) 1)) 'x64))                            ; =-IDENTICAL to source (transparent)
      (= '(2 4 6) (map (jitir '(\ x (+ x 1)) 'x64) '(1 3 5)))                              ; composes with higher-order fns
      (= 1 ((: (go i a) (? (< i 60) (go (+ i 1) (&& a (= ((jitir `('\ 'x `('* 'x i)) 'x64) 7) (* 7 i)))) a)) 0 1))   ; agree 0..60
      ; --- n-ary lane (jitnir, arity>=2): native == interp, curries to saturation, transparent, deopts
      (7 = ((jitnir '(\ x y (+ x y)) 'x64) 3 4)) (7 = (((jitnir '(\ x y (+ x y)) 'x64) 3) 4))      ; saturated + curried
      (= ((ev '(\ x y (- (* x x) y))) 5 3) ((jitnir '(\ x y (- (* x x) y)) 'x64) 5 3))
      (16000000000000000000 = ((jitnir '(\ x y (- (* x x) y)) 'x64) 4000000000 0))                 ; overflow -> deopt -> bignum
      (13 = ((jitnir '(\ x y z (+ (+ x y) (* z 2))) 'x64) 1 2 5))
      (13 = (((jitnir '(\ x y z (+ (+ x y) (* z 2))) 'x64) 1) 2 5))                                 ; arity-3, saturate at any split
      ((\ x y z (+ (+ x y) (* z 2))) = (jitnir '(\ x y z (+ (+ x y) (* z 2))) 'x64))                ; =-identical to source
      (16000000000000000005 = ((jitnir '(\ x y z (+ (+ (* x x) y) z)) 'x64) 4000000000 0 5))        ; deopt through a MIDDLE param
      (3 = ((jitnir '(\ x y (// x y)) 'x64) 13 4)) (1 = ((jitnir '(\ x y (% x y)) 'x64) 8 7))       ; n-ary // and %
      ; --- arm64: the SAME source assembles for aarch64 (data; exec needs qemu), and differs from x64
      (< 0 (tally (peep (assemble 'arm64 (coreir 'arm64 '(x) '(+ (* x 3) 1) (deoptir 'arm64 0))) 'bytes 0)))   ; leaf
      (< 0 (tally (peep (assemble 'arm64 (coreir 'arm64 '(x y) '(+ (* x x) y) (deoptir 'arm64 1))) 'bytes 0))) ; n-ary (deopt -> interp body)
      !(= (peep (assemble 'x64   (coreir 'x64   '(x) '(+ x 1) (deoptir 'x64 0))) 'bytes 0)
          (peep (assemble 'arm64 (coreir 'arm64 '(x) '(+ x 1) (deoptir 'arm64 0))) 'bytes 0))       ; per-target codegen differs
      ; --- call-out bridge: the SAME drive IR lays for aarch64 (data; exec rides the corpus), and differs from x64
      (< 0 (tally (peep (assemble 'arm64 (calloutir 'arm64 '+ 'i)) 'bytes 0)))                         ; global-F sum-over-(F i)
      (< 0 (tally (peep (assemble 'arm64 (calloutframeir 'arm64 1 0 2 '+ 'i)) 'bytes 0)))              ; frame F@1 N@0 arity 2
      !(= (peep (assemble 'x64   (calloutir 'x64 '+ 'i)) 'bytes 0)
          (peep (assemble 'arm64 (calloutir 'arm64 '+ 'i)) 'bytes 0))                                  ; per-target callout codegen differs
      !(= (peep (assemble 'x64 (calloutir 'x64 '+ 'i)) 'bytes 0)
          (peep (assemble 'x64 (calloutir 'x64 '* 'i)) 'bytes 0))                                      ; the * fold lays differently from + (mulo accumulator)
      !(= (peep (assemble 'x64 (calloutir 'x64 '+ 'i)) 'bytes 0)
          (peep (assemble 'x64 (calloutir 'x64 '+ '(* i i))) 'bytes 0))                                ; a computed arg (F (* i i)) lays differently from bare (F i)
      ; --- group lane (jitgroupir): mutually-recursive first-order integer fns, native == interp
      (55 = ((jitgroupir [(link '(fib n) '(? (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))] 'fib 'x64) 10))
      (= ((ev '(: (fib n) (? (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))) fib)) 25)
         ((jitgroupir [(link '(fib n) '(? (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))] 'fib 'x64) 25))            ; fib(25)=75025 == interp
      (1180591620717411303424 = ((jitgroupir [(link '(p2 n) '(? (< n 1) 1 (* 2 (p2 (- n 1)))))] 'p2 'x64) 70))   ; 2^70: mul overflow -> OVF -> deopt -> bignum
      ; integer-param FIXNUM GUARD (mkouterir's, distinct from the leaf coreir guard): a HEAP arg to a plain
      ; int param must DEOPT to the interp, never sar+re-tag into a garbage fixnum (pointer|1). glazed == interp.
      (: gidh ((jitgroupir [(link '(id1 x) 'x)] 'id1 'x64) '(a b)) (&& ('(a b) = gidh) !(charm? gidh)))     ; identity on a list -> the list, not a mangled fixnum
      (= (+ '(1 2) 1) ((jitgroupir [(link '(inc n) '(+ n 1))] 'inc 'x64) '(1 2)))                            ; arith on a heap arg -> deopt == interp
      ; // and % in the GROUP lane (divir), with /-by-0 deopt -- a path the jitir/jitnir cases don't reach
      (3 = ((jitgroupir [(link '(q n d) '(// n d))] 'q 'x64) 10 3)) (-3 = ((jitgroupir [(link '(q n d) '(// n d))] 'q 'x64) -10 3))
      (1 = ((jitgroupir [(link '(r n d) '(% n d))] 'r 'x64) 10 3)) (= (% 7 0) ((jitgroupir [(link '(r n d) '(% n d))] 'r 'x64) 7 0))       ; /-by-0 -> deopt -> interp
      (7 = ((jitgroupir [(link '(tak x y z) '(? (< y x) (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)) z))] 'tak 'x64) 18 12 6))     ; multi-arg, nested self-calls
      (7 = (((jitgroupir [(link '(tak x y z) '(? (< y x) (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y)) z))] 'tak 'x64) 18 12) 6))   ; ... curries at any split
      (: eo [(link '(evn n) '(? (= n 0) 1 (odn (- n 1)))) (link '(odn n) '(? (= n 0) 0 (evn (- n 1))))]
         (&& (1 = ((jitgroupir eo 'evn 'x64) 10)) (0 = ((jitgroupir eo 'evn 'x64) 7)) (1 = ((jitgroupir eo 'odn 'x64) 7))))   ; MUTUAL recursion (either entry)
      (= (jitgroupir [(link '(fib n) '(? (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))] 'fib 'x64)
         (jitgroupir [(link '(fib n) '(? (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))] 'fib 'x64))                 ; two natives, same source -> = (transparent)
      ; jitgroupirx stamps the SUPPLIED outer src (=/show) -- the hook passes `s` so a group-glazed closure
      ; is transparent against its bytecode twin; the elam entry-lambda is only the FALLBACK (jitgroupir).
      (: dfs [(link '(fib n) '(? (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))]  dopt (jitgroupir dfs 'fib 'x64)
         (&& (55 = ((jitgroupirx dfs 'fib 'x64 dopt '(\ m (fib m))) 10))                                          ; runs correct
             (= (jitgroupirx dfs 'fib 'x64 dopt '(\ m (fib m))) (jitgroupirx dfs 'fib 'x64 dopt '(\ m (fib m))))  ; same supplied src -> =
             !(= (jitgroupirx dfs 'fib 'x64 dopt '(\ m (fib m))) (jitgroupir dfs 'fib 'x64))))                     ; supplied src differs from the elam default
      ; STRING lane (peep/tally): s a string param (kind-guarded, passed RAW); peep reads byte [s+16], tally the len [s+8]
      (: data (string [72 105 90 33 65])                                                                         ; "HiZ!A": byte-sum 365, len 5
         sbn (jitgroupir [(link '(sb s i) '(? (< i 0) 0 (+ (peep s i 0) (sb s (- i 1)))))] 'sb 'x64)              ; recursive byte-sum
         tln (jitgroupir [(link '(tl s) '(tally s))] 'tl 'x64)
         pkn (jitgroupir [(link '(pk s i) '(peep s i 0))] 'pk 'x64)
         (&& (365 = (sbn data 4)) (= ((ev '(: (sb s i) (? (< i 0) 0 (+ (peep s i 0) (sb s (- i 1)))) sb)) data 4) (sbn data 4))   ; native byte-sum == interp
             (5 = (tln data)) (! (tln 5))                                                                         ; tally native; a FIXNUM arg -> guard rejects -> deopt -> interp tally of a scalar = ()
             (72 = (pkn data 0)) (0 = (pkn data 99))))                                                            ; peep in-bounds (byte 'H'=72); OOB -> the default 0 (pure-native branch, no deopt)
      ; CASK lane (pin): c a cask param (kind-guarded, raw); pin stores V's low byte at backing[i], returns c (so it threads as a call arg)
      (: c (cask 5)
         fil (jitgroupir [(link '(fill c i) '(? (< i 0) 0 (fill (pin c i (+ 65 i)) (- i 1))))] 'fill 'x64)          ; fill bytes 0..i with 65+i, recursing on (pin ..)'s returned cask
         _ (fil c 4)
         (&& (= '(65 66 67 68 69) [(peep c 0 0) (peep c 1 0) (peep c 2 0) (peep c 3 0) (peep c 4 0)])               ; each byte written natively
             (88 = (: c2 (cask 4) _ ((jitgroupir [(link '(g c) '(pin c 2 88))] 'g 'x64) c2) (peep c2 2 0)))))       ; a single native pin -> byte 2
      ; MAP lane (mpeep/mpin): m a map param (kind-guarded, raw); the open-addressed probe (mprobe-ir relabeled per site, deopt -> OVF)
      (: rn (jitgroupir [(link '(run m i) '(? (< i 0) (mpeep m 3 -1) (run (mpin m (+ 1 i) (* 10 i)) (- i 1))))] 'run 'x64)     ; fill keys 1..i+1 with 10*i (threading (mpin ..)'s map), then read key 3
         m16 (tablet 16) r16 (rn m16 8)                                                                              ; presized -> pure native
         m2 (tablet 2) r2 (rn m2 8)                                                                                  ; undersized -> mpin grow-miss -> OVF deopt -> interp grows (faithful)
         (&& (20 = r16) (40 = (peep m16 5 -1)) (0 = (peep m16 1 -1))                                                 ; native mpin wrote REAL entries (the core map accessor reads them)
             (20 = r2) (80 = (peep m2 9 -1))                                                                         ; grow-deopt faithful: same result + the map actually grew & inserted
             (-1 = ((jitgroupir [(link '(g m k) '(mpeep m k -1))] 'g 'x64) 5 3))))                                   ; a FIXNUM map arg -> guard rejects -> deopt -> interp peep on a scalar = the default -1
      ; CHAIN lane (cap/cup/two?): t a chain param (raw, NO entry guard -- two? is TOTAL; cap/cup chain-guard inline, deopting a non-chain)
      (: (mk d) (? (< d 1) 0 (link (mk (- d 1)) (mk (- d 1))))                                                       ; an interp tree builder (the allocating mk stays interp)
         ckn (jitgroupir [(link '(ck t) '(? (two? t) (+ 1 (+ (ck (cap t)) (ck (cup t)))) 0))] 'ck 'x64)              ; native node-count walk
         (&& (255 = (ckn (mk 8))) (15 = (ckn (mk 4)))                                                                ; 2^d-1 nodes, native cap/cup recursion
             (0 = (ckn 0)) (1 = (ckn (link 0 0)))))                                                                  ; two? total: a fixnum leaf -> 0; a lone chain -> 1
      ; TCO: a tail self-call loops IN PLACE (reuse the frame, O(1) stack) -- 1M deep would SIGSEGV without it
      (1000000 = ((jitgroupir [(link '(loop i) '(? (< i 1000000) (loop (+ i 1)) i))] 'loop 'x64) 0))                 ; single-arg counted loop
      (499999500000 = ((jitgroupir [(link '(sm i a) '(? (< i 1000000) (sm (+ i 1) (+ a i)) a))] 'sm 'x64) 0 0))      ; multi-arg tail accumulator (sum 0..1M-1)
      ; arm64: the group assembles (data); the H saves lr + uses 16-byte slots + bl calls (llvm-mc-verified)
      (< 0 (tally (peep (assemble 'arm64 (bcat [(mkouterir 'arm64 1 'fib 1 0 () () () () ())
                (mkhir 'arm64 '(fib) 'fib '(n) '(? (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))])) 'bytes 0))))
   (say err "love/glaze/emit-ir: neutral-IR leaf + n-ary + group lanes ok (x86 == interp; arm64 assembles)\n")))

; --- delet soundness (auto.l): a value-let inlines only dup-cheap values. Two real miscompiles
;     shipped from the same line (found by the x11 wire-codec spike, doc/misc/proto/x11.l): a `_`-bound
;     (put ..) DROPPED when a pure-integer leaf sibling made the group recognizer walk the form,
;     and two single-use effectful call-bindings INLINED into arg positions, breaking the sequence
;     law (a 32-bit read came back word-swapped). A user-fn call hides its effects from any head
;     scan, so a non-dcheap binding must always bail, never inline.
(: j (jug 0)
   (dsw v) (: _ (put j (& 255 v)) (put j (& 255 (>> v 8))))                                        ; the x11 spike's w16
   (dsleaf n) (% (- 4 (% n 4)) 4)                                                                  ; the pure leaf that triggers the walk
   _ (dsw 513)                                                                                     ; bytes 1 then 2
   t (tap [1 2 3 4])
   (dsr2 u) (: a (see t) b (see t) (| a (<< b 8)))
   (dsr4 u) (: a (dsr2 ()) b (dsr2 ()) (| a (<< b 16)))                                            ; the x11 spike's r32
   v (dsr4 ())
   _ (assert
      (0 = (dsleaf 20))
      (= (slurp j) (+ (string 1) (string 2)))                                                      ; both bytes, in order: the _-bound put kept
      (67305985 = v))                                                                              ; 0x04030201: the two dsr2 calls read in source order
   (say err "love/glaze/auto: delet keeps _-bound effects + call-binding order ok\n"))

; --- swheads reads opfixed source (auto.l): in RAW source an infix (a op b) wears its left
;     operand as a head, so a free name in operand position -- spelled like a recorded global --
;     joined the group as a stranger. the walk opfixes first: only real call heads arrive, and
;     the binder set still filters local ones.
(: swh (G 'swheads)
   h1 (swh '(? (two? l) (pm9 x (want9 = 3 ? (cup l) l)) 0) '(x l) ())
   h2 (swh '(: a (want9 1 2) (+ a x)) '(x) ())
   _ (assert
      (! (member? 'want9 h1)) (1 = (member? 'pm9 h1))                             ; a free operand is no head; the real call is
      (1 = (member? 'want9 h2))                                                   ; a called global still arrives
      (! (member? 'x h2)))                                                        ; ... and the binder set still filters
   (say err "love/glaze/auto: swheads reads opfixed source -- no operand fake heads\n"))

; --- the POINT-= grammar + the re-declared member?: the recognizer admits a point-guarded
;     key compared to a raw chain car ((= P (cap ..))), so the auto.l tail's member? is a
;     GROUP NATIVE -- the baked global rides it. Lanes: word-compare hits over any car kind
;     (a point equals only itself), the end-of-chain miss, and the entry deopts (a charm or
;     string key: only interp knows the numeric bridges (= 2 2.0) / structural string =).
(: mm (nom 0)
   ; ADMISSION (the drift gate): autogroup must REWRITE the member?-shaped group -- a
   ; declined form keeps the quoted body LIST in the binding slot; an admitted one splices
   ; the native closure (not two?). If this fires, the point-= grammar regressed and the
   ; baked member? silently fell back to interp.
   adm ((G 'autogroup) '(: (pm1 x l) (? (two? l) (? (= x (cap l)) 1 (pm1 x (cup l))) 0) (pm1 'b '(a b))))
   _ (assert
      (! (two? (caup (cup adm)))) (1 = (ev adm))                                  ; admitted: native spliced, and it answers
      (1 = (member? 'b '(a b c))) (0 = (member? 'z '(a b c)))                     ; nom key, native walk
      (1 = (member? 2 '(1 2 3))) (0 = (member? 9 '(1 2 3)))                      ; charm key -> entry deopt (bridges), interp stays right
      (1 = (member? 2 '(2.0 4))) (1 = (member? 2 '(4 2.0 2)))                    ; the numeric bridge, through the fallback
      (0 = (member? 5 '(6.0 7.0)))                                              ; deopt lane, false stays false
      (1 = (member? "ab" '(1 "ab"))) (0 = (member? "zz" '("ab")))                ; string key -> entry deopt, structural =
      (1 = (member? 'z '(1 "s" (x y) 2.5 z)))                                    ; nom key walks EVERY car kind natively
      (0 = (member? 'q '("s" 2.5 (a))))                                          ; ... and a miss over heap cars
      (1 = (member? mm (link 1 (link mm ())))) (0 = (member? mm '(a b)))         ; a mint key rides the guard
      (0 = (member? () '(1 2))) (1 = (member? () (link () ())))                  ; the zero point is a mint
      (0 = (member? 'x ())) (0 = (member? 'x 5))                                 ; no chain -> the walk never starts
      (200000 = ((: (go i a) (? (< i 200000) (go (+ i 1) (+ a (member? 'q '(a b q)))) a)) 0 0))
      (: pg (jitgroupir [(link '(pm2 x l) '(? (two? l) (? (= x (cap l)) 1 (pm2 x (cup l))) 0))] 'pm2 'x64)     ; the group lane direct: exec x64 ...
         (&& (1 = (pg 'b '(a b))) (0 = (pg 'z '(a b))) (0 = (pg 'z 7))))
      (< 0 (tally (peep (assemble 'arm64 (bcat [(mkouterir 'arm64 2 'pm3 2 0 () () () () '(0))     ; ... and the point-guarded outer assembles on arm64
                (mkhir 'arm64 '(pm3) 'pm3 '(x l) '(? (two? l) (? (= x (cap l)) 1 (pm3 x (cup l))) 0))])) 'bytes 0))))
   (say err "love/glaze/emit: the point-= grammar -- member? group-native, admission + both deopt lanes ok\n"))

(say err "test/glaze-x86: emit + auto + ir native codegen ok\n")
