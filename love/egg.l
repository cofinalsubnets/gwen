(:
 (mop _) ((peep book 'ev 0) '(:
   ; delete internal names. keep this up to date!
   _ (each '(cellread seal-hook books setbooks mods lib enter leave gv trim peek poke seek spin lamsrc nifnom yield ci-vt to-vt macros boxfix boxfix-core boxset cellbinds dsug lambp lp ln w wl mkcs hashtx feels apx iop gone numfn nfa nfq nfnest powg power ipow tray atype asum aprod alen a-coll a-dim a-emax a-rank a-shape a-type pureset operators seqx sortsplit lidx ldel packp dis-pair _)
      (\ k (pull book k 0)))
   (each (keys book)
    (\ k (? (nom? k) (? (= "lvm_" (snip (string k) 0 4)) (pull book k 0) 0) 0)))))
 (sit e z a) (? (link? a) (sit e (e (cap a)) (cup a)) z)
(\ egg post (:
 now (clock 0)
 ; sit twice so ev compiles itself
 _ (pin book 'ev (sit (sit ev 0 egg) 0 egg))
 ; post sits ONCE, on the hatchling -- the last seat that still sees the mopped noms
 _ (sit (peep book 'ev 0) 0 post)
 _ (pin book 'born (clock now))
 ; the mop sweeps the compiler's internal noms off the book. the debug door
 ; (LOVE_NO_MOP, host/main.c) sets `nomop` before the warm to keep them for cell
 ; introspection; every normal boot and every gate mops.
 (? (peep book 'nomop 0) () (mop ())))))
