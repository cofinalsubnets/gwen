; the end-to-end gate for tele: a 2-layer mlp learns XOR, written in love over the pure
; constellation algebra (no host nif), so the same code runs on every frontend -- the host,
; wasm, and inle (the freestanding kernel): a neural net on bare metal. test/tele/grad.l
; grad-checks each op alone; this one asks whether they compose into a net that learns.
;
;   run:  cat test/00-init.l test/tele/xor.l test/zz-fin.l | out/host/love -l crew/tele/tele.l
;
; corpus-shaped but not in the corpus: tele is no module (no lib/tele.l, nothing registered),
; so it arrives on -l and this file cannot ride the test/*.l glob. 00-init.l brings `assert`,
; zz-fin.l the "N tests pass" line -- the one thing an assert cannot say for itself, since a
; reader that stops early leaves every assert below it unrun and exits 0.
;
; the net: 2 -> H (tanh) -> 1 (sigmoid), trained full-batch by sgd on mse. XOR is the
; classic not-linearly-separable problem -- a single linear layer cannot solve it, the
; hidden tanh layer can. seeded, so it converges the same way every run: both the rounding
; and the loss ceiling below are deterministic, not a tolerance.

(: _ (rng-set (wheel 1))
   H 8  lr 0.6  epochs 4000  ceil 0.01     ; ceil: the observed final loss is ~2e-4, 48x under

   X (gem-tray '(4 2) '(0.0 0.0  0.0 1.0  1.0 0.0  1.0 1.0))   ; the four inputs
   Y (gem-tray '(4 1) '(0.0 1.0 1.0 0.0))                       ; XOR of each

   model (seq [(linear 2 H) tanh (linear H 1) sig])
   opt   (sgd (params model) lr)

   ;; nb: these take a dummy arg -- a nullary call (f) does not fire in love, it just
   ;; returns f (the empty-form law); a real argument is what invokes it.
   (trainstep _) (: _ (tl-reset 0) _ (zero opt)
                  loss (mse (call model X) Y) _ (back loss) _ (step opt) (tl-val loss))
   (loop k lv) (? (k = 0) lv
                  (: nv (trainstep 0)
                     _ (? (0 = k % 1000) (puts $
                        "epoch " + show (epochs - k) + "   loss " + show nv + "\n"))
                     (loop (k - 1) nv)))

   _ (puts "training a 2-8-1 MLP on XOR ...") _ (putc 10)
   final (loop epochs 1.0)
   _ (putc 10)

   ;; the learned outputs (want ~0 1 1 0)
   _ (tl-reset 0)
   out (tl-val (call model X))
   (cell k) (peep out (k >< 0 >< ()) 0)
   (yes k v) (? (0.5 < cell k) (v = 1) (v = 0))             ; does row k round to v?
   _ (puts $ "00 -> " + show (cell 0) + "\n")
   _ (puts $ "01 -> " + show (cell 1) + "\n")
   _ (puts $ "10 -> " + show (cell 2) + "\n")
   _ (puts $ "11 -> " + show (cell 3) + "\n")
   _ (puts $ "final loss " + show final + " (ceiling " + show ceil + ")\n")

   ;; bound to _: a bare assert in a binding slot is define-sugar and never runs.
   ;; both halves -- the four rows round right and the loss actually fell, since rounding
   ;; alone passes a net that merely leans the right way.
   _ (assert (yes 0 0) (yes 1 1) (yes 2 1) (yes 3 0) (final < ceil))
   0)
