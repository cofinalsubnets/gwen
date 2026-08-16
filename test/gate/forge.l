; test/gate/forge.l -- lib/forge.l's gate: a nif WRITTEN IN LOVE, assembled through crew/holo
; for this cpu and installed through the `nif` seam, must agree with the twin it deopts into on
; every shape -- the lane it says natively AND the lanes it hands back.
;
; ⚠ THE DIFFERENTIAL IS THE WHOLE GATE, and here the twin is the C nif itself, so this is one
; denotation with two presentations: love.c's `aall` and 184 bytes of x86-64 that love wrote.
; A disagreement is a bug in the kernel's IR, in holo's encoder, or in the install -- and all
; three are silent otherwise, since a wrong answer from an array reduction looks like data.
;
; ⚠ THE DEOPT LANES ARE ASSERTED, not assumed. A kernel says one monomorphic lane and hands
; everything else to the twin, so the guards are most of it: a guard that falls through where
; it should jump runs the fast loop over a float payload and answers confident nonsense.
;
; ⚠ ZERO FITTED IS A FAILURE. A build with no glaze module and a cpu with no holo backend both
; decline gracefully -- that is the design -- but a gate whose subject never installed reads
; exactly like a gate that passed. Where the seam is genuinely absent the roster still has to
; answer, so the twins are exercised either way and only the native count is required.
(: _ (use 'forge)
   (die m) (: _ (say err ("forge-gate: " + m + "\n")) _ (flush err) (quit 1))
   fitted (forge-fit 0)
   _ (say out ("forge: " + show fitted + " kernel(s) fitted, "
               + show (forge-fitted? 0) + " native, arch " + love-arch + "\n"))
   _ (? (forge-why 0) (say out ("  declines: " + show (forge-why 0) + "\n")) 0)

   ; --- the samples: the Z lane the kernel owns, then every lane it must hand back ---------
   ; ⚠ the empty and the all-nonzero cases are the two the loop bounds can get wrong in
   ; opposite directions -- an off-by-one at the top answers 0 for a full array, and one at
   ; the bottom answers 1 for an array whose only zero is last.
   zs (L (iota 0) (iota 1) (iota 2) (iota 9) (iota 64)
        (+ 1 (iota 1)) (+ 1 (iota 2)) (+ 1 (iota 9)) (+ 1 (iota 64)) (+ 1 (iota 4096))
        (array '(3) -1 2 3) (array '(3) -1 0 3) (array '(3) 1 2 0) (array '(3) 0 2 3)
        (array '(2 2) 1 2 3 4) (array '(2 2) 1 2 3 0) (array '(2 3) 1 1 1 1 1 1)
        (array '(2 2 2) 1 1 1 1 1 1 1 0) (array '(4 0) ))
   ; the deopt battery: a float tray, an object tray, plain scalars, the zero point, text
   others (L (array '(2 2) 1.0 2.0 3.0 4.0) (array '(3) 1.0 0.0 2.0)
             (array '(2) 'a 'b) (array '(2) () ())
             5 0 -3 2.5 () "text" '(1 2 3))

   nat (forge-of 'aall)
   _ (? (lit? nat) 0 (die "forge-of 'aall answered nothing -- the roster did not register"))
   (chk what xs)
    (: bad (filter (\ x !(= (nat x) (aall x))) xs)
       (? (nil? bad) (say out ("  " + what + ": agrees with its twin on " + show (tally xs) + " shapes\n"))
          (die (what + " DISAGREES with its twin on " + show bad))))
   _ (chk "native lane" zs)
   _ (chk "deopt lanes" others)

   ; --- the ARITY>=2 shape, registered here rather than in the roster -------------------------
   ; ⚠ the curried cell is a DIFFERENT cell (love.c lvm_nif lays 8 words behind lvm_cur, not 6)
   ; and its deopt skips the twin's own curry preamble, so the one-argument kernel above proves
   ; none of it. A synthetic kernel is the honest way to gate an ABI the roster does not use
   ; yet: `&` over two fixnums, against the nif it deopts into. It is not a production kernel
   ; and is not registered in lib/forge.l -- when a real two-argument kernel lands, this stays
   ; as the ABI's own law.
   (fx-and-ir _)
    (forge-fixg 0 + (forge-fixg 1
    + '((ld r0 r1 0) (sar r0 1) (ld r7 r1 8) (sar r7 1) (and r0 r0 r7))))
   _ (forge-put 'fx-and 2 fx-and-ir (&))
   fitted2 (forge-fit 0)
   _ (? (= fitted2 (fitted + 1)) 0
        (die ("the roster did not grow: fit " + show fitted + " -> " + show fitted2)))
   nat2 (forge-of 'fx-and)
   pairs (L (L 0 0) (L 1 1) (L 6 3) (L 255 15) (L 1023 992) (L 7 0) (L 65535 4096)
            (L 12345678 87654321) (L 1 2.5) (L "a" 3) (L () 7) (L 3 ()))
   bad2 (filter (\ p !(= (nat2 <p <>p) (& <p <>p))) pairs)
   _ (? (nil? bad2) (say out ("  arity 2: agrees with its twin on " + show (tally pairs) + " pairs\n"))
        (die ("arity-2 kernel DISAGREES with its twin on " + show bad2)))

   ; --- the wake rung: a fit is idempotent, because a woken image must be able to re-run it --
   ; ⚠ a native cell does not survive a bake (the image codec reverts it to the twin), so
   ; `forge-fit` is what a wake calls to get native back. Called twice it must install twice
   ; and answer the same, never accumulate a second roster or hand back a stale cell.
   again (forge-fit 0)
   _ (? (= again fitted2) 0 (die ("refit fitted " + show again + ", first fit " + show fitted2)))
   _ (chk "after refit" zs)
   _ (chk "after refit (deopt)" others)

   ; --- a decline is DATA, and one bad kernel does not take the roster with it ----------------
   ; ⚠ a kernel's IR is a program and holo RAISES on an op it cannot lower, so an untrapped
   ; assemble would end the fit and every kernel after it -- and the tree would read as a build
   ; where the seam was simply absent. the bogus row must decline BY NAME, keep its twin, and
   ; leave its neighbours native.
   (bogus-ir _) '((no-such-op r0 r1))
   _ (forge-put 'bogus 1 bogus-ir aall)
   fitted3 (forge-fit 0)
   _ (? (= fitted3 fitted2) 0
        (die ("a bogus kernel changed the fitted count: " + show fitted2 + " -> " + show fitted3)))
   _ (? (forge-why 0) 0 (die "the bogus kernel declined without naming a reason"))
   _ (? (= ((forge-of 'bogus) (+ 1 (iota 4))) 1) 0 (die "the bogus kernel did not fall back to its twin"))
   _ (chk "after a decline" zs)
   _ (say out ("  decline: named " + show (forge-why 0) + ", roster intact\n"))

   ; ⚠ LAST, because everything above still had to run: the seam's absence is a legal build
   ; (no glaze module, or a cpu with no backend) and the twins carry it. What is not legal is
   ; a tree that thinks it forged and did not.
   _ (? (fitted > 0) 0 (die "zero kernels fitted -- the seam, the backend or the assemble died"))
   _ (say out "forge: 0 failed\n")
   0)
