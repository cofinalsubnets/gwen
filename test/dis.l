; test/dis.l -- `dis` (love/ev.l), the emission interface's DUAL: a compiled thread
; read back as (op-nom operand..) rows. it is the splice JIT's decoder (bench/vmsplice,
; doc/moon-regalloc.md's splice client) and the reflection door the mop leaves open --
; built pre-egg from peek + the book, like `feels`, so the words survive.
;
; ⚠ these are the GLAZE-INVARIANT laws. the corpus runs with the glaze live, and the
; glaze compiles most closures to a NATIVE cell -- which has no readable thread, so dis
; answers () there, correctly. so the corpus-honest claim is not "dis reads THIS
; closure" (which closure glazes is the glaze's business) but dis's CONTRACT: total
; (never scares), and () for an atom or a native, else a row list whose every head is an
; op nom and whose last row is a ret-family terminator. the POSITIVE proof -- dis reads a
; real thread and it round-trips through mooncc to the same value -- lives in
; bench/vmsplice under LOVE_NO_GLAZE, where no closure is glazed.
;
; ⚠ dis is a HOST reflection capability (the splice JIT runs on the host, with mooncc).
; love0 -- the bootstrap differential binary -- does not surface ev.l's late top-level
; leak, so dis is absent there. the guard skips rather than fail (the uu/glaze idiom) --
; and every `dis` reference sits INLINE under the guard, since a HELPER that closes over
; an absent global scares at load even when never called (only a direct reference defers).
(: (dis-fin? r) (: h (cap r) (? (id? h 'lvm_ret) 1 (id? h 'lvm_tap) 1 (id? h 'lvm_tapn) 1 0))
   (dis-wf? rows) (? (atom? rows) 0                      ; a non-empty row list ...
    (: (walk l) (? (atom? (cup l)) (dis-fin? (caap l))   ; ... whose last row's head is a ret-family nom
                   (&& (nom? (caap l)) (walk (cup l))))    ; ... and every earlier head is an op nom
       (&& (nom? (caap rows)) (walk rows))))
   (dis-okr rows) (? (atom? rows) 1 (dis-wf? rows))       ; takes ROWS (no `dis` ref) -- () or well-formed
   ; the shapes; whichever the glaze declines exercises the thread lane, the rest the
   ; native-answers-() lane -- either way dis-okr must hold
   (di-k1 x) (cup x)
   (di-k4 x) (cup (cap (cup (cap x))))
   (di-pr x) (two? (cup x))
   (di-seq x) (: z (cup x) (cap z))
   (di-2 x y) (cup (cap x))
   ; ⚠ assert is `_`-bound with a trailing () body, NOT the bare tail of this `:`: an
   ; assert in tail position resolves its names eagerly, and love0 (no `dis`) scares
   ; before the runtime guard can skip. as a sequence step the guard defers as intended.
   _ (assert
    ; the CONTRACT, skipped where dis is absent (love0). the `dis` calls are inline under
    ; the guard -- never named by a helper -- so love0 never resolves the absent global.
    (? (member? 'dis (names ()))
       (&& (dis-okr (dis di-k1)) (&& (dis-okr (dis di-k4)) (&& (dis-okr (dis di-pr))
       (&& (dis-okr (dis di-seq)) (&& (dis-okr (dis di-2))
       (&& (id? () (dis 5)) (id? () (dis ()))))))))     ; an atom is no thread -> ()
       1))
   ())
