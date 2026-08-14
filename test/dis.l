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
(: (dis-fin? r) (: h (cap r) (? (id? h 'lvm_ret) 1 (id? h 'lvm_ret0) 1 (id? h 'lvm_tap) 1
                                (id? h 'lvm_tapn) 1 0))
   (dis-wf? rows) (? (atom? rows) 0                      ; a non-empty row list ...
    (: (walk l) (? (atom? (cup l)) (dis-fin? (caap l))   ; ... whose last row's head is a ret-family nom
                   (&& (nom? (caap l)) (walk (cup l))))    ; ... and every earlier head is an op nom
       (&& (nom? (caap rows)) (walk rows))))
   (dis-okr rows) (? (atom? rows) 1 (dis-wf? rows))       ; takes ROWS (no `dis` ref) -- () or well-formed
   ; --- disg, the same reading as a GRAPH -----------------------------------------------
   ; ⚠ THE LAW IS CLOSURE, and it is the only one worth asserting: every branch target a
   ; row carries must be the index of a row that IS THERE. A thread's control flow is
   ; entirely internal to it (every target is a same-thread cell pointer), so a target
   ; naming no row means the walk resolved a pointer to the wrong cell -- which is what a
   ; collection moving the thread mid-scan would do, and it would read as a perfectly
   ; well-formed answer. A splicer trusting that lays down a jump into nothing.
   ; ⚠ a row is (idx nom opnd..), so its nom is cap(cup(r)) -- NOT `cuap`, which is cup∘cap
   ; and here takes the cup of the INDEX. Written that way both laws below hold vacuously:
   ; no target is ever found, so closure is never tested, and the gate reads green. It is
   ; green on the host either way (the glaze natives everything, rows are ()); love0 has no
   ; glaze, real rows reach the law, and that is the whole reason the differential is kept.
   (disg-idx? rows i) (? (atom? rows) 0 (= i (caap rows)) 1 (disg-idx? (cup rows) i))
   (disg-nom r) (cap (cup r))
   (disg-tgt r) (: h (disg-nom r) o (cup (cup r))         ; the target operand, or () -- o = the operands
                   (? (id? h 'lvm_jump) (cap o)
                      (id? h 'lvm_cond) (cap o)
                      (id? h 'lvm_argcond) (cap (cup o))
                      (id? h 'lvm_argtwocond) (cap (cup o))
                      ()))
   (disg-ok? rows) (? (atom? rows) 1                      ; () is the honest answer for a native
    (: (walk l) (? (atom? l) 1
                   (: t (disg-tgt (cap l))
                      (&& (? (whole? t) (disg-idx? rows t) 1)          ; every target names a row
                          (&& (nom? (disg-nom (cap l))) (walk (cup l))))))  ; every row is (idx nom ..)
       (walk rows)))
   (disg-sorted? rows) (? (atom? rows) 1 (atom? (cup rows)) 1          ; strictly ascending by index
    (&& (< (caap rows) (caap (cup rows))) (disg-sorted? (cup rows))))
   ; the shapes; whichever the glaze declines exercises the thread lane, the rest the
   ; native-answers-() lane -- either way dis-okr must hold
   (di-k1 x) (cup x)
   (di-k4 x) (cup (cap (cup (cap x))))
   (di-pr x) (two? (cup x))
   (di-seq x) (: z (cup x) (cap z))
   (di-2 x y) (cup (cap x))
   (di-br x) (? (two? x) (cup x) 0)                       ; one branch: the else arm is a row dis never reaches
   (di-brn x) (? (two? x) (? (two? (cup x)) 1 2) 3)       ; nested, so a target lands mid-thread
   (di-brm a b) (? (a < b) (cup a) (cap b))               ; cur + a call-free cond
   ; ⚠ the NIF lane, which no closure above reaches -- both shapes, {impl, ret0} and
   ; {cur, n, impl, ret0}. the LENGTH is the law: a run is two cells or four, so cur + impl +
   ; ret0 is every row a nif can have. neither dis-okr nor dis-wf? alone can see an overrun --
   ; it reads the NEXT run as more rows and answers a perfectly well-formed 127-row lie.
   (dis-nifs f l) (? (atom? l) 1
    (: r (f <l) (&& (dis-wf? r) (&& (>= 3 (tally r)) (dis-nifs f >l)))))
   ; ⚠ assert is `_`-bound with a trailing () body, NOT the bare tail of this `:`: an
   ; assert in tail position resolves its names eagerly, and love0 (no `dis`) scares
   ; before the runtime guard can skip. as a sequence step the guard defers as intended.
   _ (assert
    ; the CONTRACT, skipped where dis is absent (love0). the `dis` calls are inline under
    ; the guard -- never named by a helper -- so love0 never resolves the absent global.
    (? (member? 'dis (names ()))
       (&& (dis-okr (dis di-k1)) (&& (dis-okr (dis di-k4)) (&& (dis-okr (dis di-pr))
       (&& (dis-okr (dis di-seq)) (&& (dis-okr (dis di-2))
       (&& (dis-nifs dis [cap cup tally sat mint snip peep])
       (&& (id? () (dis 5)) (id? () (dis ())))))))))    ; an atom is no thread -> ()
       1))
   ; ⚠ disg's own contract, and it is the splice JIT's licence to lay out a graph: the
   ; rows a closure can REACH, index-sorted, with every branch target relativized to a
   ; cell index -- the one form safe to hand out, since a raw target is a pointer.
   ; ⚠ GLAZE-INVARIANT ONLY, like every claim above it: the corpus runs with the glaze live,
   ; so these closures are native cells here and BOTH readers answer () -- correctly. The
   ; positive claim (a branching closure's graph is strictly bigger than its linear prefix)
   ; is therefore not sayable in this gate and lives in test/gate/splice.l, which runs under
   ; LOVE_NO_GLAZE. A `(< 0 0)` written here is how that lesson arrived.
   _ (assert
      (? (member? 'disg (names ()))
         (all (\ b b) [(disg-ok? (disg di-br)) (disg-ok? (disg di-brn)) (disg-ok? (disg di-brm))
                       (disg-ok? (disg di-k4)) (disg-sorted? (disg di-brn))
                       (disg-sorted? (disg di-brm)) (id? () (disg 5)) (id? () (disg ()))])
         1))
   ())
