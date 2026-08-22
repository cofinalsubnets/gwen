; layer-3 spike: the LOOP-CLOSER (`close`) -- collapse a polynomial accumulation loop to its CLOSED
; FORM, work INDEPENDENT of the trip count. this is the sequel to deforestation (doc/misc/proto/forest.l)
; and a layer ABOVE partial evaluation (doc/misc/proto/mix.l): mix UNFOLDS a loop (an O(n) straight-line
; residual), close SOLVES it (an O(d) residual, d the body's polynomial degree, independent of n). it
; is ai's answer to LLVM's scalar-evolution -- and it reaches the one bench SCEV punts on: `deforest`
; (Σ of the odd squares, which Julia leaves as a running loop -- see the bench timings).
;
; THE ENGINE is the calculus of finite differences -- Newton-Gregory forward interpolation + the
; hockey-stick identity -- done in EXACT INTEGERS (no Bernoulli rationals, so it stays on the green
; charms and needs no rational tower):
;   * any polynomial body f of degree d is its own forward series:  f(i) = Σ_k c_k · C(i,k), where the
;     coefficients c_k = Δ^k f(0) are f's k-th forward differences at 0 (integers when f is integer-
;     valued -- the binomial basis is the integer-valued basis).
;   * the hockey-stick identity sums each basis term in CLOSED FORM:  Σ_{i<N} C(i,k) = C(N, k+1).
;   * so   Σ_{i<N} f(i)  =  Σ_k c_k · C(N, k+1)  -- a handful of binomials, NO loop over N, exact.
;
; the body is treated as a BLACK-BOX ORACLE. we never parse its arithmetic; we SAMPLE it at i=0..d and
; let the difference table collapse the samples to the c_k. so a body that ALLOCATES -- the `closure`
; bench's ((twice (adder i)) i), two heap closures an iteration -- closes exactly like a bare `i`,
; because both SAMPLE to the line 3i. that is the spike's surprise: closure-building, array math, any
; effect-free body, all reduce to their value sequence, and the value sequence is all close needs.
;
; FILTERS are met by REPARAMETRIZATION. a filter that keeps an arithmetic progression (the `deforest`
; odds: k = 1,3,5,..) is not a polynomial in the loop index -- it is 0 on the gaps -- but it IS a
; polynomial in the DENSE index j of the kept terms: k = 2j+1. close reparametrizes to j, sums the
; polynomial body f(2j+1) over the j<M kept terms, and the gaps never cost anything. SCEV cannot do
; this (it sees a data-dependent branch in the loop and gives up); close sees an affine guard and
; changes variable. that is the edge on `deforest`.
;
; run:    out/host/love doc/misc/proto/close.l                         (the demo; standalone `assert` is
;                                                                lenient -- eyeball the shown forms)
; strict: cat test/00-init.l doc/misc/proto/close.l | out/host/love    (00-init makes `assert` enforce)
;
; close composes RIGHTWARD off forest.l: deforestation lowers a map/filter/fold pipeline to the
; canonical fold `(foldl (+) z (map F SRC))`; the recognizer here takes THAT shape and, when F is
; polynomial and SRC is a jot (optionally affine-filtered), collapses it to a literal -- so the glaze
; chain reads forest -> close -> codegen, each pass handing the next a simpler form.

(:
 (nul? l) (! (two? l))                                  ; end-of-list = spine test, not the net (the house nil-trap)

 ; --- exact-integer closed-form summation by finite differences ----------------------------------
 ; fd: one forward difference of a sample list -- (y0 y1 y2 ..) |-> (y1-y0 y2-y1 ..), one shorter.
 ; lead: the LEADING DIAGONAL of the difference table -- c_0 = y0, then the head of each successive
 ; difference list. for L samples it yields exactly L coefficients (each fd shrinks the list by one,
 ; so the recursion bottoms out at the empty list); a degree-d body sampled at L>d points pads the
 ; tail with zeros, which the closed form drops. so OVER-sampling is safe, UNDER-sampling is not --
 ; `cdeg` below picks L by sampling until the differences die.
 (fd ys)   (? (nul? (cup ys)) () (link (- (caup ys) (cap ys)) (fd (cup ys))))
 (lead ys) (? (nul? ys) () (link (cap ys) (lead (fd ys))))

 ; binom: C(n,r), exact integer. falling factorial over r!, divided ONCE at the end (C(n,r) is whole,
 ; so the single `/` is exact and stays an integer -- a stepwise divide could go inexact and float).
 (binom n r) (: (go j num den) (? (< j r) (go (+ j 1) (* num (- n j)) (* den (+ j 1))) (/ num den)) (go 0 1 1))

 ; closed: Σ_k c_k · C(n, k+1) -- the hockey-stick sum of the Newton series. THIS is the closed form;
 ; its cost is O(length cks) = O(d), with NO dependence on n (n only feeds the binomials).
 (closed cks n) (: (go ks k acc) (? (nul? ks) acc (go (cup ks) (+ k 1) (+ acc (* (cap ks) (binom n (+ k 1)))))) (go cks 0 0))

 ; samples: (f 0) (f 1) .. (f d), the oracle readings the difference table eats.
 (samples f d) (: (go i) (? (<= i d) (link (f i) (go (+ i 1))) ()) (go 0))

 ; cdeg: the body's degree -- keep lengthening the sample window until the top forward difference is
 ; 0 (a degree-d polynomial has Δ^(d+1) ≡ 0). test `= 0` EXACTLY, not `nil?`: a negative leading term
 ; nets <= 0, so `(! topfd)` would falsely report the degree early. capped so a NON-polynomial body
 ; can't spin forever; a body still alive at the cap is rejected (close declines, the loop stays a
 ; loop). NB sampling can't PROVE a body polynomial -- a piecewise body (x<5 ? x^2 : 0) fits x^2 on the
 ; first samples then breaks; the glaze port (love/glaze/auto.l loopclose) restricts to PURE-polynomial
 ; bodies (+ - * only) so this is sound there; here the demo bodies are all genuine polynomials.
 (topfd ys)  (? (nul? (cup ys)) (cap ys) (topfd (fd ys)))   ; the last entry of the diagonal: Δ^(L-1) f(0)
 (cdeg f cap) (: (go d) (? (>= d cap) -1 (? (= 0 (topfd (samples f (+ d 1)))) d (go (+ d 1)))) (go 0))

 ; close: THE KERNEL. Σ_{i<n} f(i) in closed form -- find the degree, sample to it, Newton + hockey
 ; stick. returns () (declines) if f is not polynomial within the degree cap. O(d^2) to build the
 ; table, O(d) binomials -- and ZERO iterations over n. works for ANY n (literal or symbolic value).
 (close f n) (: d (cdeg f 8) (? (< d 0) () (closed (lead (samples f d)) n)))

 ; --- the naive references: what an interpreter/loop ACTUALLY runs, O(n) -------------------------
 (naive f n)  (: (go i acc) (? (< i n) (go (+ i 1) (+ acc (f i))) acc) (go 0 0))   ; Σ_{i<n} f(i), a real loop
 (oddsq n)    (: (go i acc) (? (< i n) (go (+ i 1) (+ acc (? (= 1 (% i 2)) (* i i) 0))) acc) (go 0 0))  ; the `deforest` loop

 ; --- the three bench bodies, verbatim in spirit -------------------------------------------------
 twice (\ f (\ x (f (f x))))                            ; the `closure` bench's combinators, local
 adder (\ i (\ x (+ x i)))
 cbody (\ i ((twice (adder i)) i))                      ; closure bench body: builds 2 closures, nets to 3i
 dbody (\ j (: k (+ 1 (* 2 j)) (* k k)))                ; deforest reparametrized: the j-th odd, squared

 ; --- a recognizer: the deforested fold shape -> a closed literal (the glaze tie-in) -------------
 ; forest.l hands us (foldl (+) z (map F SRC)); we close it when SRC is a jot, optionally guarded by
 ; the odd-filter. F is ev'd to its closure and SAMPLED -- the rewrite never reads F's body. the
 ; residual is the literal (z + the closed sum): O(1), exactly what SCEV emits, reached one case past it.
 (head? x h)  (&& (two? x) (id? (cap x) h))
 (plus? f)    (? (id? f '+) 1 (&& (two? f) (&& (id? (cap f) '+) (nul? (cup f)))))   ; + or (+)
 (jot? s)     (head? s 'jot)
 (oddf? s)    (&& (head? s 'filter) (head? (cap (cuup s)) 'jot))                    ; (filter _ (jot N))
 (jotn s)     (caup s)                                                              ; (jot N) -> N
 (foldmap? x) (&& (head? x 'foldl) (&& (plus? (caup x)) (head? (cap (cuup (cup x))) 'map)))
 (close-form x)
   (? (! (foldmap? x)) x
      (: z (cap (cuup x)) mp (caup (cuup x))            ; (foldl (+) z (map F SRC))
         F (ev (caup mp)) src (cap (cuup mp))
         ; close returns a number on success, the zero point () on decline -- so `constellation? r`
         ; (a number?) is the win test; `nul?` cannot serve here, it is true of any non-chain (a sum is one).
         (? (jot? src) (: r (close F (ev (jotn src)))           (? (constellation? r) (+ (ev z) r) x))
            (oddf? src) (: n (ev (jotn (cap (cuup src))))       ; (filter _ (jot N)) -> N; odds in [0,N): j-th is 2j+1, M = N//2
                           r (close (\ j (F (+ 1 (* 2 j)))) (// n 2)) (? (constellation? r) (+ (ev z) r) x))
            x)))

 ; ============================ self-checks ============================
 ; the equivalence method (as in forest.l/mix.l): the closed form computes the SAME value the loop
 ; does -- proven at the bench trip count AND at several small n, so it is genuinely the CLOSED FORM
 ; (a polynomial identity in n), not a memoized single point.
 (agree? f n) (= (naive f n) (close f n))

 ; sum bench: Σ_{i<n} i. closed = C(n,2) = n(n-1)/2.
 _ (assert (agree? (\ i i) 7)   "sum: Σi closes (small n)")
 _ (assert (agree? (\ i i) 40)  "sum: Σi closes (another n -- a real identity, not one point)")
 _ (assert (5000050000 = (close (\ i i) 100001)) "sum: bench checksum 5000050000 with NO loop")

 ; closure bench: the body BUILDS closures, yet samples to the line 3i. closed = 3·C(n,2).
 _ (assert (agree? cbody 50)  "closure: the closure-building body closes (samples to 3i)")
 _ (assert (14999850000 = (close cbody 100000)) "closure: bench checksum 14999850000 with NO loop")

 ; deforest bench: Σ of the odd squares. reparametrize odds k=2j+1; body (2j+1)^2 is degree 2 in j.
 ; this is the headline -- the O(n) filtered loop `oddsq` and the O(d) closed `close dbody` AGREE,
 ; and the closed form is the one Julia's SCEV declines (it sees the odd-guard branch and keeps the loop).
 _ (assert (agree? dbody 50)  "deforest: the reparametrized odd-square body closes")
 _ (assert ((oddsq 20000) = (close dbody 10000)) "deforest: the filtered O(n) loop == the O(d) closed form")
 _ (assert (1333333330000 = (close dbody 10000)) "deforest: bench checksum 1333333330000, SCEV-beating, NO loop")

 ; a NON-polynomial body is declined (close returns (), the loop is left alone) -- close is sound,
 ; not optimistic: it only fires when the difference table actually dies.
 _ (assert (! (constellation? (close (\ i (i 2)) 30))) "non-polynomial body 2^i (= (i 2)) is declined (no number), not mis-closed")

 ; the recognizer, end to end: the deforested fold forms collapse to a literal, value preserved.
 P '(foldl (+) 0 (map (\ x (* x x)) (jot 100)))                       ; Σ_{i<100} i^2 = 328350
 _ (assert ((ev P) = (close-form P)) "recognizer: fold/map/jot -> closed literal, value preserved")
 _ (assert (whole? (close-form P)) "recognizer: ... and the residual IS a literal (no loop survives)")
 Q '(foldl (+) 0 (map (\ x (* x x)) (filter (\ x (= 1 (% x 2))) (jot 20000))))   ; the deforest pipeline itself
 _ (assert ((ev Q) = (close-form Q)) "recognizer: the deforest odd-filter pipeline closes, value preserved")
 _ (assert (1333333330000 = (close-form Q)) "recognizer: deforest pipeline -> 1333333330000 directly from source")

 _ (. ";; doc/misc/proto/close.l: loop-closer self-checks pass -- polynomial accumulation collapses to\n")
 _ (. ";; its closed form (Newton-Gregory + hockey stick, exact integers), work independent of trip count.\n")
 _ (. ";; sum      Σ_{i<N} i        -> ") _ (. (show (close (\ i i) 100001)))  _ (. "  (= C(N,2))\n")
 _ (. ";; closure  Σ_{i<N} 3i       -> ") _ (. (show (close cbody 100000)))    _ (. "  (body builds closures; sampled to 3i)\n")
 _ (. ";; deforest Σ_{odd k<N} k^2  -> ") _ (. (show (close dbody 10000)))     _ (. "  (odds reparametrized k=2j+1; SCEV keeps the loop)\n")
 _ (. ";; recognizer: ") _ (. (show Q)) _ (. "\n;;          -> ") _ (. (show (close-form Q))) _ (. "  (one literal, no iterations)\n"))
