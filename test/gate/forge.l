; test/gate/forge.l -- lib/forge.l's gate: a nif WRITTEN IN LOVE, assembled through crew/holo
; for this cpu and installed through the `nif` seam, must agree with the twin it deopts into on
; every shape -- the lane it says natively AND the lanes it hands back.
;
; ⚠ THE DIFFERENTIAL IS THE WHOLE GATE, and here the twin is the C nif itself, so this is one
; denotation with two presentations: love.c's answer and the bytes love wrote for the same job.
; A disagreement is a bug in the kernel's IR, in holo's encoder, or in the install -- and all
; three are silent otherwise, since a wrong answer from an array reduction looks like data.
;
; ⚠ AND IT HAS ALREADY CAUGHT ONE THE OTHER WAY. `outer`'s first differential red was the C
; side: love.c's lvm_outer wrote 4*M of its M*N cells (a mooncc declarator-scope bug, fixed
; 2026-08-16, test/cc/146-declscope.c). A differential does not know which side it is holding.
;
; ⚠ THE DEOPT LANES ARE ASSERTED, not assumed. A kernel says one monomorphic lane and hands
; everything else to the twin, so the guards are most of it: a guard that falls through where
; it should jump runs the fast loop over a float payload and answers confident nonsense.
;
; ⚠ ZERO FITTED IS A FAILURE UNLESS IT IS NAMED. A build with no glaze module, and a target
; whose lvm ABI the kit does not spell, both decline gracefully -- that is the design -- but a
; gate whose subject never installed reads exactly like a gate that passed. So a zero has to
; come with a reason in `forge-why`, and a silent one is still red. Where nothing installs the
; roster still has to answer, so the twins are exercised either way.
(: _ (use "lib/forge.l")
   (die m) (: _ (say err ("forge-gate: " + m + "\n")) _ (flush err) (quit 1))
   fitted (forge-fit 0)
   ; read BEFORE the roster grows a deliberately-bogus kernel below, whose own decline
   ; would else answer for a fit that died silently
   why0 (forge-why 0)
   _ (say out ("forge: " + show fitted + " kernel(s) fitted, "
               + show (forge-fitted? 0) + " native, arch " + love-arch + "\n"))
   _ (? why0 (say out ("  declines: " + show why0 + "\n")) 0)

   ; --- the shapes ---------------------------------------------------------------------------
   ; ⚠ the empty and the all-nonzero cases are the two a loop bound can get wrong in opposite
   ; directions -- an off-by-one at the top answers 0 for a full array, one at the bottom
   ; answers 1 for an array whose only zero is last.
   (seq sh) (: k (foldl (*) 1 sh) (foldl (\ g j (g (j + 1))) (array sh) (jot k)))
   zs [(iota 0) (iota 1) (iota 2) (iota 9) (iota 64)
       (+ 1 (iota 1)) (+ 1 (iota 2)) (+ 1 (iota 9)) (+ 1 (iota 64)) (+ 1 (iota 4096))
       (array '(3) -1 2 3) (array '(3) -1 0 3) (array '(3) 1 2 0) (array '(3) 0 2 3)
       (array '(2 2) 1 2 3 4) (array '(2 2) 1 2 3 0) (array '(2 3) 1 1 1 1 1 1)
       (array '(2 2 2) 1 1 1 1 1 1 1 0) (array '(4 0) )]
   ; the Z trays the two-argument kernels sweep, every rank and both orders of every shape
   ts [(seq '(1)) (seq '(3)) (seq '(4)) (seq '(6)) (seq '(2 2)) (seq '(2 3)) (seq '(3 2))
       (seq '(4 2)) (seq '(2 4)) (seq '(3 3)) (seq '(2 2 2)) (seq '(1 3)) (seq '(3 1)) (iota 0)]
   ; everything a kernel must hand back: floats, objects, scalars, the zero point, text, a list
   ds [(array '(2 2) 1.0 2.0 3.0 4.0) (array '(3) 1.0 0.0 2.0)
       (array '(2) 'a 'b) (array '(2) () ())
       5 0 -3 2.5 () "text" '(1 2 3)]

   (chk1 nm f g xs)                                  ; a one-argument kernel against its twin
    (: bad (filter (\ x !(= (f x) (g x))) xs)
       (? (nil? bad) (say out ("  " + nm + ": agrees on " + show (tally xs) + " shapes\n"))
          (die (nm + " DISAGREES with its twin on " + show bad))))
   (chk2 nm f g xs)                                  ; a two-argument kernel, every pair
    (: bad (filter (\ a (any (\ b !(= (f a b) (g a b))) xs)) xs)
       (? (nil? bad)
          (say out ("  " + nm + ": agrees on " + show (tally xs * tally xs) + " pairs\n"))
          (: _ (each xs (\ a (each xs (\ b (? (= (f a b) (g a b)) 0
                 (say out ("    " + nm + " " + show (shape a) + " x " + show (shape b)
                           + "\n      twin " + show (g a b) + "\n      holo " + show (f a b) + "\n")))))))
             (die (nm + " DISAGREES with its twin")))))
   (want nm) (: v (forge-of nm)
                (? (lit? v) v (die ("forge-of " + show nm + " answered nothing"))))

   _ (chk1 "aall  native" (want 'aall) aall zs)
   _ (chk1 "aall  deopt " (want 'aall) aall ds)
   _ (chk2 "outer native" (want 'outer) outer ts)
   _ (chk2 "outer deopt " (want 'outer) outer (ds + ts))
   _ (chk2 "inner native" (want 'inner) inner ts)
   _ (chk2 "inner deopt " (want 'inner) inner (ds + ts))

   ; --- the EXTRAS slot, which no kernel in the roster uses right now --------------------------
   ; ⚠ a kit piece nobody exercises reads exactly like a working one. `nifx` carries one love
   ; value in the cell at Ip+32 -- the door for a constant a kernel cannot spell, and the only
   ; way `sort` could reach the zero-point terminator before it was dropped. This kernel is the
   ; whole mechanism and nothing else: ignore the argument, answer what rode along. The value is
   ; a HEAP one on purpose, since the point of the slot over an `li` immediate is that the
   ; collector walks it and the image codec encodes it.
   (xt-ir _) ((forge-extras 'r0) + forge-epi-ptr)
   xtval '(the extras slot)
   _ (forge-putx 'xt 1 xt-ir (\ q xtval) xtval)

   ; --- the ARITY>=2 shape, registered here rather than in the roster -------------------------
   ; ⚠ the curried cell is a DIFFERENT cell (love.c lvm_nif lays 8 words behind lvm_cur, not 6)
   ; and its deopt skips the twin's own curry preamble. `outer` and `inner` ride it, but both
   ; guard on trays; this one is the ABI's own law over plain fixnums, where nothing else can
   ; explain a disagreement.
   (fx-and-ir _)
    (forge-fixg 0 + (forge-fixg 1
    + ('((ld r0 r1 0) (sar r0 1) (ld r7 r1 8) (sar r7 1) (and r0 r0 r7)) + forge-epi)))
   _ (forge-put 'fx-and 2 fx-and-ir (&))
   fitted2 (forge-fit 0)
   ; ⚠ conditional on the seam, like every other native-only claim here: where nothing can
   ; install, `fitted` stays 0 and the row not growing is the correct answer, not a red. The
   ; one requirement that is NOT conditional is the last line of the file.
   _ (? (|| (= fitted 0) (= fitted2 (fitted + 2))) 0
        (die ("the roster did not grow: fit " + show fitted + " -> " + show fitted2)))
   _ (? (= ((want 'xt) 0) xtval) 0
        (die ("the extras slot answered " + show ((want 'xt) 0) + ", want " + show xtval)))
   _ (? (= ((want 'xt) '(anything at all)) xtval) 0 (die "the extras kernel read its argument"))
   _ (say out ("  extras: the Ip+32 slot answers " + show xtval + "\n"))
   nat2 (want 'fx-and)
   pairs [[0 0] [1 1] [6 3] [255 15] [1023 992] [7 0] [65535 4096]
          [12345678 87654321] [1 2.5] ["a" 3] [() 7] [3 ()]]
   bad2 (filter (\ p !(= (nat2 <p <>p) (& <p <>p))) pairs)
   _ (? (nil? bad2) (say out ("  arity 2: agrees on " + show (tally pairs) + " pairs\n"))
        (die ("the arity-2 kernel DISAGREES with its twin on " + show bad2)))

   ; --- ALLOCATION: the room guard, and the law that no collection runs mid-body --------------
   ; a native body's registers are not GC roots, so a kernel proves the room BEFORE any Hp or Sp
   ; motion and deopts to the twin when it is not there. `link`, the 3-word cons, is the
   ; smallest thing that can hold the law.
   ; ⚠ GATE-ONLY, and the measurement is the reason: 300k conses run 7 ms through the C nif and
   ; 18 ms through this, because the body is smaller than the curried cell's own indirection.
   ; The lane pays where there is a LOOP in it and loses where there is not, so this one holds
   ; the ABI and stays out of the roster.
   ; ⚠ the churn is the real assertion: 2M conses through the native builder, which forces the
   ; guard to fire over and over. A kernel that let a collection run mid-body would answer a
   ; cell in the old semispace -- not a crash, a wrong list, which is why the deep build is
   ; compared against the C one rather than merely counted.
   (link-ir _)
    (forge-room 96
    + ([['li 'r8 forge-chaink] '(st r2 0 r8)
         '(ld r0 r1 0) '(st r2 8 r0)
         '(ld r0 r1 8) '(st r2 16 r0)
         '(mov r0 r2) '(add r2 r2 24)]
       + forge-epi-ptr))
   _ (forge-put 'cons2 2 link-ir link)
   fitted3 (forge-fit 0)
   _ (? (|| (= fitted 0) (= fitted3 (fitted2 + 1))) 0
        (die ("the allocating kernel did not fit: " + show fitted2 + " -> " + show fitted3)))
   natc (want 'cons2)
   cx [1 0 -3 2.5 () "s" 'nom '(1 2) (iota 3)]
   badc (filter (\ a (any (\ b !(= (natc a b) (link a b))) cx)) cx)
   _ (? (nil? badc) 0 (die ("the cons kernel DISAGREES with `link` on " + show badc)))
   deep (: (go i a) (? (i < 1) a (go (i - 1) (natc i a))) (go 5000 ()))
   deepc (: (go i a) (? (i < 1) a (go (i - 1) (link i a))) (go 5000 ()))
   _ (? (= deep deepc) 0 (die "a 5000-deep native build differs from the C one"))
   _ (: (go i) (? (i < 1) 0 (: _ (go2 500) (go (i - 1))))
        (go2 k) (: (g3 j a) (? (j < 1) (tally a) (g3 (j - 1) (natc j a))) (g3 k ()))
        (go 4000))
   _ (? (= (natc 1 '(2 3)) '(1 2 3)) 0 (die "the cons kernel is wrong after the churn"))
   _ (say out ("  allocation: " + show (tally cx * tally cx) + " pairs, a 5000-deep build and 2M conses\n"))

   ; --- the wake rung: a fit is idempotent, because a woken image must be able to re-run it ---
   ; ⚠ a native cell does not survive a bake (the image codec reverts it to the twin), so
   ; `forge-fit` is what a wake calls to get native back. Called twice it must install twice and
   ; answer the same, never accumulate a second roster or hand back a stale cell.
   again (forge-fit 0)
   _ (? (= again fitted3) 0 (die ("refit fitted " + show again + ", first fit " + show fitted3)))
   _ (? (= fitted 0) 0 (? (= (forge-fitted? 0) again) 0
        (die ("refit installed " + show again + " but " + show (forge-fitted? 0) + " are native"))))
   _ (chk1 "aall  refit" (want 'aall) aall zs)
   _ (chk2 "outer refit" (want 'outer) outer ts)

   ; --- a decline is DATA, and one bad kernel does not take the roster with it -----------------
   ; ⚠ a kernel's IR is a program and holo RAISES on an op it cannot lower, so an untrapped
   ; assemble would end the fit and every kernel after it -- and the tree would read as a build
   ; where the seam was simply absent. the bogus row must decline BY NAME, keep its twin, and
   ; leave its neighbours native.
   (bogus-ir _) '((no-such-op r0 r1))
   _ (forge-put 'bogus 1 bogus-ir aall)
   fitted4 (forge-fit 0)
   _ (? (= fitted4 fitted3) 0
        (die ("a bogus kernel changed the fitted count: " + show fitted3 + " -> " + show fitted4)))
   _ (? (forge-why 0) 0 (die "the bogus kernel declined without naming a reason"))
   _ (? (= ((forge-of 'bogus) (+ 1 (iota 4))) 1) 0 (die "the bogus kernel did not fall back to its twin"))
   _ (chk1 "aall  after a decline" (want 'aall) aall zs)
   _ (say out ("  decline: named " + show (forge-why 0) + ", roster intact\n"))

   ; ⚠ LAST, because everything above still had to run: forging nothing is a legal build
   ; (no glaze module, or a target the ABI kit does not spell) and the twins carry it. What
   ; is not legal is a tree that thinks it forged and did not.
   _ (? (fitted > 0) 0 why0 0
        (die "zero kernels fitted, and nothing named a reason -- the seam or the assemble died"))
   _ (say out "forge: 0 failed\n")
   0)
