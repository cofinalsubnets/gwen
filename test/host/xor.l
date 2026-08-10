; the end-to-end gate for tele: a 2-layer MLP learns XOR, written in love over the pure
; constellation algebra (no host nif), so the same code runs on EVERY frontend -- the host,
; wasm, and inle (the freestanding kernel): a neural net on bare metal. test/host/tele.l
; grad-checks each op alone; this one asks whether the ops COMPOSE into a net that learns.
;
;   run:  out/host/love -l crew/tele/tele.l test/host/xor.l   (`make test_xor`)
;
; the net: 2 -> H (tanh) -> 1 (sigmoid), trained full-batch by SGD on MSE. XOR is the
; classic not-linearly-separable problem -- a single linear layer cannot solve it, the
; hidden tanh layer can. seeded, so it converges the same way every run: both the rounding
; and the loss ceiling below are deterministic, not a tolerance.

(: _ (rng-set (wheel 1))
   H 8  lr 0.6  epochs 4000  ceil 0.01     ; ceil: the observed final loss is ~3e-4, 30x under

   X (gem-tray '(4 2) '(0.0 0.0  0.0 1.0  1.0 0.0  1.0 1.0))   ; the four inputs
   Y (gem-tray '(4 1) '(0.0 1.0 1.0 0.0))                       ; XOR of each

   model (seq `((linear 2 H) tanh (linear H 1) sig))
   opt   (sgd (params model) lr)

   ;; NB: these take a dummy arg -- a nullary call (f) does NOT fire in love, it just
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
   ;; both halves: the four rows round right AND the loss actually fell. rounding alone
   ;; passes a net that merely leans the right way.
   won (yes 0 0 && yes 1 1 && yes 2 1 && yes 3 0 && final < ceil)
   _ (puts $ "00 -> " + show (cell 0) + "\n")
   _ (puts $ "01 -> " + show (cell 1) + "\n")
   _ (puts $ "10 -> " + show (cell 2) + "\n")
   _ (puts $ "11 -> " + show (cell 3) + "\n")
   _ (puts $ "final loss " + show final + " (ceiling " + show ceil + ")\n")
   _ (puts $ (? won "xor: ok\n" "FAIL xor: the net did not learn it\n"))
   (? won 0 (quit 1)))
