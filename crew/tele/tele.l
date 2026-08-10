; tele -- a PyTorch-style reverse-mode autograd library, written in love, over the
; CONSTELLATION algebra (love's numeric tower: a `star` scalar, a `galaxy` numeric
; array). love already ships the tensor -- it calls it a constellation, with
; broadcasting +/*, reductions (net), and contractions (inner). tele adds the one
; missing ingredient: the gradient. Stage 1 -- the autograd core.
;
; the mechanism is one idea, native to love's currying: reverse-mode AD is the
; category of LENSES. a differentiable op is not a -> b, it is a pair
;     forward : a -> (b , (b-bar -> a-bar))
; the value AND the linear backward map from the output cotangent to the input
; cotangent. that second component is a closure -- and love is closures all the way
; down, so the lens falls out for free. `back` is the adjoint ray-trace: the same
; lenses, walked backward, seeded with the cotangent 1.
;
; pure love -- no host nif, so this runs on every frontend (host, kernel, wasm).
; load with `-l crew/tele/tele.l` after the prel; the gradient-check gate is test/host/tele.l.
;
; a node (a "glass") is a MUTABLE tablet { 'val 'grad 'bwd }. the galaxy VALUES it
; holds stay immutable; only the node mutates -- grad ACCUMULATES by replacing the
; node's grad slot with a fresh summed galaxy (a node consumed twice sums its
; cotangents -- the multivariate chain rule). the node itself is the identity key
; for accumulation; no mint needed -- the tablet IS the identity. leaves
; (glass/const) carry a noop bwd and stay off the tape; only OPS are taped, in
; creation order -- a define-by-run Wengert list whose reverse IS a valid
; reverse-topological order (a parent is always built before its child).

(: ;; -- list helpers (local). + adjoins () as an ELEMENT (the zero point is a
   ;; measure-zero scalar to +), so for concatenation we need a real append that
   ;; treats () as empty.
   (tl-app a b) (? (two? a) (<a >< tl-app >a b) b)
   (tl-cat ll)  (? (two? ll) (tl-app <ll (tl-cat >ll)) ())

   ;; -- the tape: a global Wengert list of op nodes, newest-first (= reverse-topo)
   tl-tape #()
   (tl-reset _) (pin tl-tape 0 ())
   (tl-push n)  (: _ (pin tl-tape 0 (n >< peep tl-tape 0 ())) n)

   ;; -- nodes --
   (tl-node? x)  (tablet? x)                          ; a tele node (a raw constellation is never a tab)
   (tl-val x)    (peep x 'val 0)
   (node x)      (? (tl-node? x) x (const x))       ; coerce a raw constellation to a const leaf
   (tl-accum n d)(pin n 'grad (peep n 'grad 0 + d))   ; cotangent += d (a fresh galaxy)
   (tl-mk v bwd) #('val v 'grad 0 'bwd bwd)
   (tl-op v bwd) (tl-push (tl-mk v bwd))            ; an op node: forward value + adjoint, taped

   ;; (glass v) wraps a constellation as a leaf that REQUIRES grad; (const v) one
   ;; that does not. mechanically identical (a leaf has a noop bwd, off the tape);
   ;; the difference is intent -- you read (grad g) of a glass, never of a const.
   (glass v) (tl-mk v (\ s 0))
   (const v) (tl-mk v (\ s 0))
   (grad g)  (peep g 'grad 0)                       ; the accumulated cotangent, after back

   ;; -- shapes & the un-broadcast (sumto) --
   ;; cshape: a constellation's shape, () for a scalar. sumto sums a cotangent back
   ;; to a target shape. Stage 1 covers a scalar target (sum all) and a matching
   ;; shape; the general numpy un-broadcast (size-1 / leading axes) is Stage 2.
   (tl-cshape v) (? (tray? v) (shape v) ())
   (tl-lsum l)   (? (two? l) (<l + tl-lsum >l) 0)
   (tl-lprod l)  (? (two? l) (<l * tl-lprod >l) 1)
   (tl-ncells v) (? (tray? v) (tl-lprod (shape v)) 1)
   ;; colsum: sum a rank-2 galaxy over axis 0 -> a length-C vector. this is the
   ;; bias un-broadcast: a (C,) added to an (N,C) output sums its cotangent over
   ;; the N rows it was stretched across.
   (tl-colsum g) (: sh (shape g) r <sh c <>sh
      (gem-tray `(c)
           (map (\ j (tl-lsum (map (\ i (peep g `(i j) 0)) (jot r)))) (jot c))))
   ;; sh is the 1-vector (C,) where C is g's last axis -- checked element-wise.
   (tl-can-colsum g sh)
      (? (tray? g)
         (? (rank g = 2)
            (? (two? sh) (? (two? >sh) 0 (<sh = <>(shape g))) 0)
            0)
         0)
   ;; sumto: un-broadcast a cotangent g back to a target shape. scalar target (sum
   ;; all), matching shape (identity), or an (N,C)->(C,) column reduction (the bias
   ;; case). higher-rank / general size-1 axes raise -- add them when a layer needs one.
   (tl-sumto g sh)
      (? !(two? sh)            (? (tray? g) (net g) g)
         (sh = tl-cshape g)  g
         (tl-can-colsum g sh)  (tl-colsum g)
         (scare 'sumto-todo sh))

   ;; -- 2D transpose (functional rebuild; arrays are immutable). mm's adjoint
   ;; needs A-transpose, and `inner` only contracts last-of-a vs first-of-b.
   (transp2 m) (: sh (shape m) r <sh c <>sh
      (gem-tray `(c r)
           (tl-cat (map (\ j (map (\ i (peep m `(i j) 0)) (jot r))) (jot c)))))

   ;; -- the device seam -- tele's heavy array ops (matmul, elementwise, reduce,
   ;; transpose) route through a SWAPPABLE backend record { gemm ew reduce transp }.
   ;; the default `tl-cpu` IS love's native constellation algebra, so tele stands
   ;; alone (test/host/tele.l loads only this file). load crew/tele/device.l for the SIM +
   ;; roofline meter -- then (tl-use that-backend) and every mm/add/mul/sub/sum/mean
   ;; below reroutes with NO change to tele's source.
   ;; `ew` takes the elementwise op as its first argument ((+)/(*)/(-)) so one
   ;; kernel slot covers all three lanes.
   tl-cpu  #('name "cpu" 'gemm inner 'ew id
             'reduce net 'transp transp2)
   tl-dev  #()
   (tl-use bk) (pin tl-dev 0 bk)
   (tl-k key)  (peep (peep tl-dev 0 tl-cpu) key 0)      ; the active kernel for `key`
   (tl-gemm a b)  ((tl-k 'gemm) a b)
   (tl-ew op a b) ((tl-k 'ew) op a b)
   (tl-reduce a)  ((tl-k 'reduce) a)
   (tl-tr m)      ((tl-k 'transp) m)

   ;; -- the lens ops -- each returns its forward value and an adjoint that
   ;; scatters cotangents into its parents (accumulating). `s` is the op node
   ;; itself; (peep s 'grad 0) is the cotangent that has landed on this op.
   ;; NB: the coerced nodes get FRESH names (na/nb/nx) -- shadowing the parameter
   ;; (`a (node a)`) would, by the one-scope law, read `a` BEFORE its pin -> the
   ;; missing condition -> the zero point, silently coercing () instead of a.
   (add a b) (: na (node a) nb (node b)
      (tl-op (tl-ew (+) (tl-val na) (tl-val nb))
             (\ s (: g (peep s 'grad 0)
                    _ (tl-accum na (tl-sumto g (tl-cshape (tl-val na))))
                      (tl-accum nb (tl-sumto g (tl-cshape (tl-val nb))))))))
   (mul a b) (: na (node a) nb (node b)             ; elementwise (Hadamard)
      (tl-op (tl-ew (*) (tl-val na) (tl-val nb))
             (\ s (: g (peep s 'grad 0)
                    _ (tl-accum na (tl-sumto (tl-ew (*) g (tl-val nb)) (tl-cshape (tl-val na))))
                      (tl-accum nb (tl-sumto (tl-ew (*) g (tl-val na)) (tl-cshape (tl-val nb))))))))
   (mm a b) (: na (node a) nb (node b)              ; matmul (rides the gemm kernel)
      (tl-op (tl-gemm (tl-val na) (tl-val nb))
             (\ s (: g (peep s 'grad 0)
                    _ (tl-accum na (tl-gemm g (tl-tr (tl-val nb))))     ; dA = G @ B^T
                      (tl-accum nb (tl-gemm (tl-tr (tl-val na)) g))))))  ; dB = A^T @ G
   (sum x) (: nx (node x)                            ; reduce-all to a scalar
      (tl-op (tl-reduce (tl-val nx))
             (\ s (tl-accum nx (tl-val nx * 0 + peep s 'grad 0)))))  ; broadcast g to x's shape

   ;; ---- Stage 2: activations + loss ----
   ;; e^z is just numeral application -- a galaxy applied to e exponentiates each
   ;; cell (the `power` nif is gone: exponentiation IS application).
   (tl-exp z) (z e)

   (sub a b) (: na (node a) nb (node b)             ; a - b (broadcasting)
      (tl-op (tl-ew (-) (tl-val na) (tl-val nb))
             (\ s (: g (peep s 'grad 0)
                    _ (tl-accum na (tl-sumto g (tl-cshape (tl-val na))))
                      (tl-accum nb (tl-sumto (-1 * g) (tl-cshape (tl-val nb))))))))

   (mean x) (: nx (node x) xv (tl-val nx) n (tl-ncells xv)   ; sum / count
      (tl-op (/ (tl-reduce xv) n)
             (\ s (tl-accum nx (xv * 0 + peep s 'grad 0 / n)))))

   ;; relu IS the color law, differentiated: it passes the positive (blue) and
   ;; clamps the rest to the blue floor at 0. the local slope is the mask (x>0) --
   ;; and the kink, where the subgradient switches, is exactly that blue floor.
   (relu x) (: nx (node x) xv (tl-val nx) m (< 0 xv)
      (tl-op (* xv m) (\ s (tl-accum nx (* (peep s 'grad 0) m)))))

   ;; sigmoid s = 1/(1+e^-x); the adjoint s'(x) = s(1-s) (read off the output).
   (sig x) (: nx (node x) xv (tl-val nx) sv (1 / (1 + tl-exp (0 - xv)))
      (tl-op sv (\ s (tl-accum nx (peep s 'grad 0 * sv * (1 - sv))))))

   ;; tanh = 2*sig(2x)-1; the adjoint t'(x) = 1 - t^2.
   (tanh x) (: nx (node x) xv (tl-val nx) tv (2 / (1 + tl-exp (-2 * xv)) - 1)
      (tl-op tv (\ s (tl-accum nx (peep s 'grad 0 * (1 - tv * tv))))))

   ;; mse = mean((p-t)^2). a COMPOSITE of the ops (no new adjoint): d is reused, so
   ;; mul d d squares it and the fan-out makes the chain rule give 2d for free.
   (mse p t) (: d (sub p t) (mean (mul d d)))

   ;; ---- Stage 3: nn modules + optim ----
   ;; a MODULE is a tablet { 'fwd 'params } -- a forward fn + its parameter glasses.
   ;; a bare function (an activation) counts as a param-less module. the parameter
   ;; glasses PERSIST across steps (updated in place); each forward builds a fresh
   ;; graph over them (define-by-run), so a held glass trains while the tape resets.
   (tl-mod fwd ps) #('fwd fwd 'params ps)
   (tl-mfwd m)    (? (tablet? m) (peep m 'fwd 0) m)        ; module-or-bare-fn -> its forward
   (tl-mparams m) (? (tablet? m) (peep m 'params 0) ())
   (call m x)     ((tl-mfwd m) x)                        ; apply a module to an input node
   (params m)     (tl-mparams m)                         ; the parameter list (for the optimizer)

   ;; weight init: small uniform in [-0.5, 0.5). seed with (rng-set (wheel n)).
   (tl-randmat r c) (gem-tray `(r c) (map (\ _ (randf 0 - 0.5)) (jot $ r * c)))
   (tl-zeros n)     (gem-tray `(n) 0)

   ;; linear: y = x@W + b. linear-wb builds from given param glasses (handy for
   ;; tests); (linear i o) inits W (i,o) random, b (o,) zero. rides mm + the bias
   ;; broadcast, whose adjoint is the colsum sumto from Stage 2.
   (linear-wb W b) (tl-mod (\ x (add (mm x W) b)) `(W b))
   (linear i o)    (linear-wb (glass (tl-randmat i o)) (glass (tl-zeros o)))

   ;; seq: compose modules/functions left-to-right; params = the concatenation.
   (seq mods) (tl-mod (\ x (foldl (\ acc m ((tl-mfwd m) acc)) x mods))
                      (tl-cat (map tl-mparams mods)))

   ;; optim: an SGD stepper over a parameter list. step: p <- p - lr*grad (in place,
   ;; swapping the node's immutable val for a fresh one); zero: clear every grad.
   (sgd ps lr) #('params ps 'lr lr)
   (step opt)  (: ps (peep opt 'params 0) lr (peep opt 'lr 0)
                  (map (\ p (pin p 'val (tl-val p - lr * grad p))) ps))
   (zero opt)  (map (\ p (pin p 'grad 0)) (peep opt 'params 0))

   ;; -- back -- seed the output cotangent with ones, then walk the tape
   ;; newest-first (reverse-topo): each op's adjoint scatters to its parents, and
   ;; by the time we reach a node every later consumer has already added in.
   (tl-seed v) (? (tray? v) (v * 0 + 1) 1)
   (tl-walk ns) (? (two? ns) (: n <ns _ ((peep n 'bwd 0) n) (tl-walk >ns)) 0)
   (back loss) (: _ (pin loss 'grad (tl-seed (tl-val loss))) _ (tl-walk (peep tl-tape 0 ())) loss))
