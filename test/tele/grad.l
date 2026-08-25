; the gradient-check gate for tele (Stage 1: add / mul / mm / sum).
;
; the oracle is SELF-VALIDATING: for every op the analytic gradient (from `back`)
; must match the central finite difference (f(x+eps) - f(x-eps)) / 2eps, cell by
; cell. no hand-written expected gradients -- the math checks itself. that IS the
; gate. wired as `test_tele` (see tools/Makefile).
;
; run from the repo root:  out/host/love -l crew/tele/tele.l test/tele/grad.l

(: eps 0.0001 tol 0.003

   ;; -- a tiny assert harness (this file is standalone, not in the corpus) --
   pass (tablet 0) fail (tablet 0)
   _ (pin pass 0 0) _ (pin fail 0 0)
   (ok nm c) (? c (pin pass 0 (+ 1 (peep pass 0 0)))
                (: _ (pin fail 0 (+ 1 (peep fail 0 0)))
                    _ (puts (+ "FAIL " nm)) (putc 10)))

   ;; -- list / constellation helpers --
   (nth l k)  (? (= k 0) <l (nth >l (- k 1)))
   (lprod l)  (? (two? l) (* <l (lprod >l)) 1)
   (lall l)   (? (two? l) (? <l (lall >l) 0) 1)
   (ncells v) (? (tray? v) (lprod (shape v)) 1)
   ;; flat row-major index k -> a coordinate list (peep takes a coord list at any rank)
   (decode sh k) (? (two? sh) (: t >sh p (lprod t) ((// k p) >< decode t (% k p))) ())
   (gcell v k)   (? (tray? v) (peep v (decode (shape v) k) 0) v)
   ;; functionally bump cell k of a constellation by d (arrays are immutable -> rebuild)
   (bump v k d)  (? (tray? v)
                    (: sh (shape v) (gem-tray sh (map (\ j (+ (gcell v j) (? (= j k) d 0)))
                                                      (jot (lprod sh)))))
                    (+ v d))
   (setnth l p x) (? (= p 0) (x >< >l) (<l >< setnth >l (- p 1) x))
   (close? a b)   (< (abs (- a b)) (* tol (+ 1 (abs b))))
   (close-c? a b) (lall (map (\ k (close? (gcell a k) (gcell b k))) (jot (ncells a))))  ; constellations

   ;; -- the grad check --
   ;; ins : a list of raw input constellations (the point to check at)
   ;; fwd : a list of leaf nodes -> the scalar loss node
   (loss-at fwd xs) (: _ (tl-reset 0) (tl-val (fwd (map glass xs))))
   (numgrad fwd ins p k)
     (/ (- (loss-at fwd (setnth ins p (bump (nth ins p) k eps)))
           (loss-at fwd (setnth ins p (bump (nth ins p) k (- 0 eps)))))
        (* 2 eps))
   (okp fwd ins ag p)                               ; every cell of input p matches
     (lall (map (\ k (close? (numgrad fwd ins p k) (gcell (nth ag p) k)))
                (jot (ncells (nth ins p)))))
   (gradcheck nm ins fwd)
     (: _ (tl-reset 0)
        leaves (map glass ins)
        loss   (fwd leaves)
        _ (back loss)
        ag (map grad leaves)                         ; analytic grad, one per input
        good (lall (map (\ p (okp fwd ins ag p)) (jot (tally ins))))
        (ok nm good))

   ;; -- fixtures --
   v3  @(1.0 2.0 3.0)
   w3  @(0.5 -1.0 2.0)
   r4  @(1.0 -2.0 0.5 -0.7)                          ; for relu: cells comfortably off the kink
   b3  @(10.0 20.0 30.0)
   t3  @(0.2 -0.4 1.1)
   m23 (gem-tray (quote (2 3)) (quote (1.0 -2.0 0.5 3.0 1.0 -1.5)))
   m32 (gem-tray (quote (3 2)) (quote (2.0 1.0 -1.0 0.5 0.0 3.0)))
   x22 (gem-tray (quote (2 2)) (quote (0.5 -1.0 2.0 0.3)))
   w23 (gem-tray (quote (2 3)) (quote (0.1 -0.2 0.3 0.4 -0.5 0.6)))

   ;; -- the cases (loss is always a scalar via sum) --
   _ (gradcheck "sum(v)"                [v3]      (\ ls (sum (nth ls 0))))
   _ (gradcheck "sum(a+b)"              [v3 w3]   (\ ls (sum (add (nth ls 0) (nth ls 1)))))
   _ (gradcheck "sum(a*b)"             [v3 w3]   (\ ls (sum (mul (nth ls 0) (nth ls 1)))))
   _ (gradcheck "sum(s*v) scalar bcast" [2.0 v3]  (\ ls (sum (mul (nth ls 0) (nth ls 1)))))
   _ (gradcheck "sum((a+b)*b) fan-out"  [v3 w3]   (\ ls (sum (mul (add (nth ls 0) (nth ls 1)) (nth ls 1)))))
   _ (gradcheck "sum(mm(A,B))"          [m23 m32] (\ ls (sum (mm (nth ls 0) (nth ls 1)))))
   _ (gradcheck "sum(mm+mm) fan-out"    [m23 m32] (\ ls (sum (add (mm (nth ls 0) (nth ls 1))
                                                                    (mm (nth ls 0) (nth ls 1))))))

   ;; -- Stage 2: activations, loss, the bias un-broadcast (colsum sumto) --
   _ (gradcheck "sum(relu(x))"          [r4]      (\ ls (sum (relu (nth ls 0)))))
   _ (gradcheck "sum(sig(x))"           [v3]      (\ ls (sum (sig (nth ls 0)))))
   _ (gradcheck "sum(tanh(x))"          [v3]      (\ ls (sum (tanh (nth ls 0)))))
   _ (gradcheck "sum(a-b)"              [v3 w3]   (\ ls (sum (sub (nth ls 0) (nth ls 1)))))
   _ (gradcheck "mean(x)"               [v3]      (\ ls (mean (nth ls 0))))
   _ (gradcheck "mse(p,t)"              [v3 t3]   (\ ls (mse (nth ls 0) (nth ls 1))))
   _ (gradcheck "bias bcast sumto"      [b3]      (\ ls (sum (add m23 (nth ls 0)))))
   _ (gradcheck "linear+sig layer"      [x22 w23 b3]
       (\ ls (sum (sig (add (mm (nth ls 0) (nth ls 1)) (nth ls 2))))))

   ;; -- Stage 3: nn modules, optim, end-to-end training --
   ;; param collection: a 2-layer seq has 4 param glasses (2 linears x {W,b})
   _ (ok "seq collects 4 params" (= 4 (tally (params (seq [(linear 2 3) relu (linear 3 1)])))))

   ;; seq forward == an explicit hand composition over the same fixed params
   _ (: W1 (glass (gem-tray (quote (2 2)) (quote (0.1 0.2 0.3 0.4)))) b1 (glass @(0.5 -0.5))
        W2 (glass (gem-tray (quote (2 1)) (quote (0.7 -0.3))))       b2 (glass 0.2)
        net (seq [(linear-wb W1 b1) relu (linear-wb W2 b2)])
        x  (gem-tray (quote (1 2)) (quote (1.0 2.0)))
        _ (tl-reset 0) got  (tl-val (call net x))
        _ (tl-reset 0) want (tl-val (add (mm (relu (add (mm x W1) b1)) W2) b2))
        (ok "seq forward == manual compose" (close-c? got want)))

   ;; sgd step: a param with a known grad moves by -lr*grad
   _ (: p (glass @(1.0 2.0 3.0)) _ (pin p (quote grad) @(0.5 0.5 0.5))
        opt (sgd [p] 0.1) _ (step opt)
        (ok "sgd step: p -= lr*grad" (close-c? (peep p (quote val) 0) @(0.95 1.95 2.95))))

   ;; zero clears the grad
   _ (: p (glass @(1.0 2.0)) _ (pin p (quote grad) @(9.0 9.0))
        opt (sgd [p] 0.1) _ (zero opt)
        (ok "zero clears grad" (= 0 (grad p))))

   ;; end-to-end: a linear layer learns a linear target; the loss collapses
   _ (: _ (rng-set (wheel 1))
        Wt (gem-tray (quote (2 1)) (quote (1.5 -2.0)))
        X  (gem-tray (quote (4 2)) (quote (1.0 0.0  0.0 1.0  1.0 1.0  2.0 -1.0)))
        Y  (inner X Wt)                                  ; the true targets, X @ Wt
        (lossof model) (: _ (tl-reset 0) (tl-val (mse (call model X) Y)))
        (trainstep model opt) (: _ (tl-reset 0) _ (zero opt)
                                  loss (mse (call model X) Y) _ (back loss) _ (step opt) (tl-val loss))
        (trainN model opt k) (? (= k 0) 0 (: _ (trainstep model opt) (trainN model opt (- k 1))))
        model (linear 2 1)
        opt (sgd (params model) 0.08)
        l0 (lossof model)
        _ (trainN model opt 300)
        lf (lossof model)
        (ok "training collapses the loss" ((< lf (* 0.001 l0)) && (< lf 0.001))))

   ;; -- summary --
   np (peep pass 0 0) nf (peep fail 0 0)
   _ (puts (+ (show np) (+ " tele tests pass" (? (< 0 nf) (+ ", " (+ (show nf) " FAIL")) ""))))
   _ (putc 10)
   (? (< 0 nf) (quit 1) 0))                          ; quit is the exit nif (was `exit`)
