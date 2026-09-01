; arm64 call-out execution -- run only under qemu-aarch64 (test_raw_arm64), never in the x86
; corpus: it builds nifs with `jitcalloutx ... 'arm64`, whose code is aarch64 bytes -- calling one on
; x86 jumps into arm64 machine code (a fault). It lives in test/arm64/ (a subfolder), so the
; non-recursive test/*.l corpus glob (make test / love0 bake) never sees it. test_raw_arm64 names it past the
; corpus, and gate_sentinel there is what proves it was read and not stopped short.
;
; Proves the stackless call-out bridge (emit.l calloutir/calloutframeir) executes correctly on aarch64:
; the near pc-relative resume address (`lar` -> ADR, translation-invariant in the word-aligned mmap blob,
; where la's page-relative adrp would mis-page) and the fold-op overflow gates -- the + fold keeps sa
; tagged (the fixnum TAG boundary at 2^62, not the 64-bit 2^63); the * fold keeps sa raw (mulo) and retags
; with an overflow gate at the end. == the interpreter across fast path, empty, the [2^62,2^63) window,
; past 2^63, and the float (gate 1) deopt -- both the direct emit and the auto-glaze (natjit hook), + and *.

; --- direct emit: build the aarch64 nif by hand and run it ---
(: G (from 'glaze)  jitcalloutx (G 'jitcalloutx)
   (ca-sq i)   (* i i)
   (ca-half i) (* i 0.5)
   (ca-inc i)  (+ i 1)
   (ca-ph i)   (+ i 0.5)                             ; 0.5,1.5,2.5,.. -> a non-trivial float product
   (ca-thr i)  (? (= i 0) 3 2)                       ; 3,2,2,.. -> product 3*2^(n-1)
   (ca-md7 x)  (% x 7)                               ; bounded 0..6 -> a computed-arg overflow deopts only on the arg
   ca-src   '(\ n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (ca-sq i)))   a)) 0 0))
   caf-src  '(\ n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (ca-half i))) a)) 0 0))
   nat  (jitcalloutx ca-sq   ca-src  '+ 'i 'i 'arm64)   ; native aarch64 call-out (+ fold, bare arg)
   natf (jitcalloutx ca-half caf-src '+ 'i 'i 'arm64)
   natp (jitcalloutx ca-inc  '(\ n 0) '* 'i 'i 'arm64)  ; * fold: product of (i+1) = n!
   natpf (jitcalloutx ca-ph  '(\ n 0) '* 'i 'i 'arm64)  ; * fold, float callee -> gate 1
   natp3 (jitcalloutx ca-thr  '(\ n 0) '* 'i 'i 'arm64) ; * fold, product lands in [2^62,2^63)
   natc  (jitcalloutx ca-sq  '(\ n 0) '+ 'i '(+ i 1) 'arm64)                       ; non-(F i): sum sq(i+1)
   natco (jitcalloutx ca-sq  '(\ n 0) '+ 'i '(* i i) 'arm64)                       ; non-(F i): sum sq(i*i) = i^4 (overflows -> bignum twin)
   natcd (jitcalloutx ca-md7 '(\ n 0) '+ 'i '(* (* i i) 1000000000) 'arm64)        ; non-(F i): arg overflows ~i=68k -> caargdeopt -> twin (bounded clos)
   r3m ((: (go i a) (? (< i 3000000) (go (+ i 1) (+ a (ca-sq i))) a)) 0 0)   ; interp reference (inline sum; a closure never glazed -> pure interp) for the boundary case
   ; NB: strict prefix `(= a b)` throughout -- opfix currently mis-factors compound=compound infix
   ; (((f x) = (g y)) -> (f (= x (g y)))), so prefix sidesteps it entirely (opfix fix is a follow-up).
   _ (assert
       (= 0 (nat 0))                                 ; empty
       (= 285 (nat 10))                              ; fixnum fast path
       (= 2666664666667000000 (nat 2000000))         ; ~2.6e15 fixnum
       (= 8999995500000500000 (nat 3000000))         ; [2^62,2^63): tag-overflow -> bignum (garbage on the untagged gate)
       (= (nat 3000000) r3m)                          ; ... == interp
       (= 333333283333335000000 (nat 10000000))      ; ~3.3e20, well past 2^63
       (= 95.0 (natf 20))                             ; gate 1: float call-out result -> deopt to interp twin, gem
       ; * fold: sa kept raw (mulo accumulator), retagged with an overflow gate at the end
       (= 1 (natp 0))                                 ; empty product = the identity 1
       (= 120 (natp 5))                               ; 5! fixnum fast path
       (= 51090942171709440000 (natp 21))            ; 21! -> gate 2 (product overflow) -> bignum
       (= 1.875 (natpf 3))                            ; gate 1: float product -> deopt -> gem
       (= 6917529027641081856 (natp3 62))            ; 3*2^61 in [2^62,2^63): the end-retag adds-vc gate must deopt
       ; non-(F i) computed args: caarg compiles arg natively; an arg leaving fixnum range deopts (caargdeopt)
       (= 55 (natc 5)) (= 0 (natc 0)) (= 91 (natc 6))    ; sum sq(i+1) native fast path
       (= 979 (natco 6)) (= 1950333330 (natco 100))      ; sum i^4: fixnum then overflow -> bignum twin
       (= 280000 (natcd 70000)) (= 399999 (natcd 100000))))   ; arg overflow -> caargdeopt -> twin

; --- auto-glaze: the natjit hook (now installed on aarch64) glazes these at closure creation, no manual
; jitcalloutx. ag-sum is a top-level global-f sum (hook's calloutinfo -> jitcalloutx arch); ag-summer
; returns an embedded captured closure (only the hook catches it -> njit-calloutframe arch, the frame-read
; F + hand-rolled lvm_ret, the genuinely-new aarch64 path). == interp across the same fast/gate cases.
(: (ag-sq i)  (* i i)
   (ag-half i) (* i 0.5)
   (ag-inc i)  (+ i 1)
   (ag-ph i)   (+ i 0.5)
   (ag-sum n)  ((: (go i a) (? (< i n) (go (+ i 1) (+ a (ag-sq i))) a)) 0 0)          ; global-F sum: hook -> jitcalloutx arch
   (ag-prod n) ((: (go i a) (? (< i n) (go (+ i 1) (* a (ag-inc i))) a)) 0 1)         ; global-F product (* fold): hook -> jitcalloutx arch
   (ag-summer f) (\ n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (f i))) a)) 0 0))      ; captured sum: hook -> njit-calloutframe arch
   (ag-multiplier f) (\ n ((: (go i a) (? (< i n) (go (+ i 1) (* a (f i))) a)) 0 1))  ; captured product (* fold): hook -> njit-calloutframe arch
   (ag-cgsum n) ((: (go i a) (? (< i n) (go (+ i 1) (+ a (ag-sq (* i i)))) a)) 0 0)   ; global-F non-(F i): sum sq(i*i) = i^4, hook -> jitcalloutx arch
   (ag-capcg f) (\ n ((: (go i a) (? (< i n) (go (+ i 1) (+ a (f (+ i 1)))) a)) 0 0)) ; captured non-(F i): sum f(i+1), hook -> njit-calloutframe arch
   cs (ag-summer ag-sq)  cf (ag-summer ag-half)  cp (ag-multiplier ag-inc)  cpf (ag-multiplier ag-ph)  ccg (ag-capcg ag-sq)
   ag30 (ag-sum 30)                                                                  ; global reference for the captured==global cross-check
   _ (assert
       (= 285 (ag-sum 10)) (= 0 (ag-sum 0))                                          ; global auto-glaze fast path + empty
       (= 8555 (ag-sum 30))                                                          ; global auto-glaze == 0+1+4+..+841
       (= 8999995500000500000 (ag-sum 3000000))                                      ; global gate-2 boundary -> bignum
       (= 285 (cs 10)) (= 0 (cs 0)) (= 8555 (cs 30))                                 ; captured auto-glaze fast path
       (= (cs 30) ag30)                                                              ; captured == global
       (= 8999995500000500000 (cs 3000000))                                          ; captured gate-2 boundary -> bignum
       (= 95.0 (cf 20))                                                              ; captured gate-1 float
       ; * fold auto-glaze (both lanes): global-F product + captured product
       (= 1 (ag-prod 0)) (= 120 (ag-prod 5)) (= 51090942171709440000 (ag-prod 21))   ; global product: empty, 5!, 21!-overflow->bignum
       (= 1 (cp 0)) (= 120 (cp 5)) (= 51090942171709440000 (cp 21))                  ; captured product: same
       (= (cp 5) (ag-prod 5))                                                        ; captured == global
       (= 1.875 (cpf 3))                                                            ; captured product gate-1 float
       ; non-(F i) auto-glaze: global-F sum sq(i*i) + captured sum sq(i+1)
       (= 979 (ag-cgsum 6)) (= 1950333330 (ag-cgsum 100))                            ; global computed arg: fixnum + overflow -> bignum
       (= 55 (ccg 5)) (= 0 (ccg 0)) (= 91 (ccg 6)))                                  ; captured computed arg (frame lane)
   (say err "test/arm64/callout: aarch64 call-out execution (direct + auto-glaze, lar/ADR + fold gates + computed args) ok\n"))
