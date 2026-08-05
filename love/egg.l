(:
 (mop _) ((peep book 'ev 0) '(:
   ; delete internal names. keep this up to date!
   ; NB: the old repl internals (do_eval m eofsym prc + the ed* editor) USED to be
   ; mopped here -- they are now CLOSURE-PRIVATE inside love/bao.l (the shell core), so
   ; they never reach the book and need no mop entry.
   ; operators/monadics/pureset are the SENSITIVE tablets (2026-07-07): a live pin into
   ; a compile table poisons every later compile, so they go book-private with the rest --
   ; opfix/ev hold them by capture. a fresh punct operator needs no pin (default dyadic);
   ; new arities/aliases wait for the scope-layer door. opfix itself STAYS: C resolves it by nom.
   ; the 34 op-* factor helpers used to be listed here. they are CLOSURE-PRIVATE now (prel's
   ; opfix form has a body and publishes opfix + fixity by hand), which is the better shape
   ; wherever it fits: a name that never reaches the book cannot be forgotten from a list.
   ; it fits when nothing across a FILE boundary captures the names -- which is exactly why
   ; the three tablets above cannot follow it, ev.l holding them.
   ; turn/turnf were mopped while prel owned the RNG wrapper. love/rng.l ships
   ; separately now and cannot capture what birth deletes, so the two pure steps stay -- `seed`
   ; was never mopped, and minting a state you could not step was the odd shape anyway.
   ; ⚠ a name mopped here must be one nothing reads after birth, and only a boot that
   ; COMPILES FROM SOURCE shows a violation -- a woken image never does, so the fast gate
   ; cannot see it. prove an addition with a quiet `LOVE_NO_IMAGE=1 love -e ..`.
   ; cellread is boxfix's cell read (prel.l); the marker it tells absence by is
   ; closure-private there, off the book from birth on every lane.
   ; boxfix emits the VALUE, so the nom dying here costs it nothing -- and a user
   ; asking "is k here?" has peep, which answers it totally against a fresh (mint 0).
   _ (each '(cellread seal-hook books setbooks mods lib enter leave gv trim peek poke seek spin yield ci-vt to-vt macros boxfix boxfix-core boxset cellbinds dsug lambp lp ln w wl mkcs hashtx feels apx iop gone numfn nfa nfq nfnest powg power ipow tray atype asum aprod alen a-coll a-dim a-emax a-rank a-shape a-type pureset operators monadics seqx sortsplit lidx ldel packp _)
      (\ k (pull book k 0)))
   (each (keys book)
    (\ k (? (nom? k) (? (= "lvm_" (snip (string k) 0 4)) (pull book k 0) 0) 0)))))
 (sit e z a) (? (two? a) (sit e (e (cap a)) (cup a)) z)
(\ egg (:
 now (clock 0)
 ; the DOUBLE sit is load-bearing -- don't collapse to one (removed as "redundant" before).
 ; inner: c0-ev compiles the corpus; outer: that hatchling recompiles it -> ev self-hosted,
 ; not c0-compiled. the Futamura move: specialize the evaluator to the corpus, twice.
 _ (mop (pin book 'ev (sit (sit ev 0 egg) 0 egg)))
 (pin book 'born (clock now)))))   ; `book` stays reachable through the rest of the boot (the layers below still pin into it); host/main.c seals it -- with nif/nifx -- at the end of every boot
