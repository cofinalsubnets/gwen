; spec-glaze -- an INTERNAL love perf check for autospec (interpreter specialization), NOT a
; cross-language bench. Deliberately OFF the default BENCHES / the comparison table: love's glaze
; specializes the interpreter away (the 1st Futamura projection), which the other languages can't, so a
; side-by-side column is apples-to-oranges -- it tells you nothing about them, only about love's
; partial evaluator. Run it on its own: `make -C bench BENCHES=spec-glaze` (love only).
;
; A tiny tree-walking interpreter `ev1` over a fixed AST program P (the Horner form of 2x^3+3x^2+5x+7),
; summing P(i mod 97) mod 1e9+7 over i in [0,N). Under a naive interpreter every iteration walks + heads
; the AST; autospec (love/glaze/auto.l) applies ev1 to the STATIC P at compile time, folding the dispatch
; away to straight-line arithmetic that then natives -- DEFAULT behavior (the glaze is baked in; probe:
; `out/host/love this-file` glazes it, no flags). The % 1e9+7 keeps everything in int64 (no bignum deopt),
; so the number is native code vs interpretation. checksum = Σ_{i<N} P(i mod 97) mod 1000000007, N=10^6.
(: (ev1 nd x) (? (id? (cap nd) 'lit) (caup nd)  (id? (cap nd) 'var) x
                 (id? (cap nd) 'add) (+ (ev1 (caup nd) x) (ev1 (cap (cuup nd)) x))
                 (id? (cap nd) 'mul) (* (ev1 (caup nd) x) (ev1 (cap (cuup nd)) x))  0)
   P '(add (mul (add (mul (add (mul (lit 2) (var)) (lit 3)) (var)) (lit 5)) (var)) (lit 7))
   (run n) ((: (go i a) (? (< i n) (go (+ i 1) (% (+ a (ev1 P (% i 97))) 1000000007)) a)) 0 0)
   (bench "spec-glaze" (\ _ (run 1000000))))
