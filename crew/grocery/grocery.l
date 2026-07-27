; --- crew/grocery/grocery.l: the grocer -- turn a recorded source tree into
;     goods you can put on a system. doc/hatch.md is the design; seed (the
;     patch-set vcs, crew/seed/) is the engine underneath.
;
; the split, and it is the whole point of this file existing: HOW GERMPLASM
; BUILDS ITSELF IS NONE OF THE VAULT'S BUSINESS. seed stores patches, names
; head states, and materializes source. what a depositor then DOES with that
; source -- compile it, install it, tear it out again -- is a different job on
; a different subject, and it lived inside seed only by accident. the tell was
; that it never touched the patch DAG: it read a config, ran an argv, hashed a
; file. the one thing it wants from the vcs is a NAME for the source state,
; and it asks for that through the registry like any other consumer.
;
; so: seed is the cold store, grocery is the field. seed's verbs act on the box
; you are standing in; grocery's act on the machine. two subjects, two tools,
; and neither one drags the other's vocabulary somewhere it does not fit.
;
; SKELETON: `build` only, moved across from seed's `hatch` with its laws intact
; (a dirty tree refuses; the same source built twice is a VIABILITY test; the
; artifact is KEPT, not merely fingerprinted). install/uninstall come next, and
; with them the goods store moves out of the box and into the machine's home --
; see the note over `mk`.
(enter 'grocery)
(:
   ; --- what grocery asks of seed. this list IS the coupling surface, and it
   ;     is meant to stay this short: three domain verbs and four file helpers.
   ;     the cat puts seed's files ahead of this one, so the module is sealed
   ;     and registered by the time we read it ---
   SD (from 'seed)
   look (SD 'look) lay (SD 'lay) form (SD 'form) fld (SD 'fld)
   dirty (SD 'dirty) tips (SD 'tips) psid (SD 'psid)

   ; --- the goods store. it lives under out/, which is one of seed's BUILT-IN
   ;     ignores -- so a build can never dirty the tree it came from, and seed
   ;     needs to know nothing about grocery for that to hold (a .grocery/ dir
   ;     would have made the vcs learn its consumer's name). box-local for now:
   ;     goods are reproducible by definition, so losing them to `make clean` is
   ;     a cache miss, not a loss. `install` moves them to the machine's home,
   ;     which is where stage -1 fetches from. ---
   (mk _) (: _ (mkdir "out" 493) _ (mkdir "out/grocery" 493)
             _ (mkdir "out/grocery/goods" 493) ())
   (keep s) (: h (sha256 s) _ (lay ("out/grocery/goods/" + h) s) h)
   (good h) (look ("out/grocery/goods/" + h))       ; (1 bytes) | () -- presence, not net
   (ledger _) (form "out/grocery/ledger")           ; ((psid arch gid time)..) newest-first | ()
   (ledger-find led pid ar)                         ; the last build of this (psid,arch) | ()
    (? (! (two? led)) ()
       (: r <led (? (&& ((<r) = pid) ((<>r) = ar)) r (ledger-find >led pid ar))))
   (arch _) (: u (run (list "uname" "-m")) ls (lines (cup u)) (? (two? ls) <ls "unknown"))

   ; --- build: (source, arch) -> goods, ledgered by the source's name.
   ;     we build the RECORDED state -- a dirty tree refuses, so what the ledger
   ;     keys by is a state that actually exists in the store. building the same
   ;     source with the same recipe twice is a VIABILITY TEST: goods that don't
   ;     match the last build are a NOT-VIABLE flag, never a silent divergence.
   ;     the recipe and its output path are OPAQUE to grocery (it runs an argv
   ;     and stores a file) the way the store is opaque to what a hunk means.
   ;     config in Grocery.l -- an ordinary VERSIONED file, not vcs metadata,
   ;     because how to build a thing is part of the thing:
   ;       (build (recipe "make") (out "out/host/love")) ---
   (grocery-build argv)
    (: _ (mk ())
       tp (tips ())
       (? (! (two? tp))
          (: _ (say err "grocery: nothing recorded here -- not a seed box, or no patches yet\n") ())
          (0 < (dirty ()))
          (: _ (say err "grocery: working tree has changes; record or revert before building\n") ())
          (: cfg (form "Grocery.l")
             rc (? (two? argv) argv (fld cfg 'recipe))
             (? (! (two? rc))
                (: _ (say err "grocery: no recipe (pass one, or set Grocery.l)\n") ())
                (: pid (psid tp) ar (arch ())
                   _ (puts ("grocery: building " + (snip pid 0 12) + " (" + ar + ") -- " + (show rc) + "\n"))
                   st (cap (run rc))
                   (? (! (0 = st))
                      (: _ (say err ("grocery: build failed (status " + (show st) + ")\n")) ())
                      (: outp (: o (fld cfg 'out) (? (two? o) <o ()))
                         ; presence, not net: a recipe that lays a 0-byte output made a
                         ; real thing with a real id, and only look's wrapper tells that
                         ; apart from an output the recipe never wrote at all
                         t (? (! outp) () (look outp))
                         gid (? (two? t) (keep <>t) ())
                         _ (? (&& outp (! (two? t)))
                              (say err ("grocery: the recipe left no " + outp + " -- nothing kept\n")) ())
                         prior (ledger-find (ledger ()) pid ar)
                         _ (? (&& (two? prior) (&& gid (<>>prior)))
                              (? ((<>>prior) = gid)
                                 (puts "grocery: viable -- the same source built the same goods\n")
                                 (say err "grocery: NOT VIABLE -- this source built differently!\n"))
                              ())
                         _ (lay "out/grocery/ledger"
                                (show (link (list pid ar gid (clock 0)) (ledger ()))))
                         _ (puts ("grocery: built " + (snip pid 0 12) + " -> "
                                  + (? gid (snip gid 0 12) "(nothing)") + "\n"))
                         pid))))))))

; --- the CLI face. lives in the LAYER (seed's shape), so the seat below reaches
;     it the way any consumer does ---
(:
   (usage _) (: _ (say err "grocery -- build a recorded source tree into goods (doc/hatch.md)
usage: grocery VERB ..
  build [CMD..]   build the recorded source (the recipe is CMD.. or Grocery.l),
                  keep the output, and ledger it by (source, arch). the same
                  source built twice is a viability test
")
                (quit 2))
   (grocery-main as)
    (? (! (two? as)) (usage ())
       (: v <as
          (? (v = "build") (: r (grocery-build >as) (quit (? r 0 1)))
             (usage ())))))

(leave ())

; the SEAT (seed's shape): fire grocery-main iff this file -- or the installed
; `grocery` binary -- sits in the program seat of the real command line (the
; positional program or a -l preload), taking the args AFTER the seat. cmdline
; is the full C argv, never rebound, so the read is the same in both load
; orders and the cat stays quiet under any other name -- which is why the gate
; can cat this file and have nothing fire. body-having: the seat leaks nothing.
(: (bn p) (: n (tally p)
             (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p r n))
             (go 0 0))
   (me? w) (: b (bn w) (|| (b = "grocery") (b = "grocery.l")))
   (seat a) (? (! (two? a)) ()
               (<a = "-l") (? (two? >a)
                              (? (me? <>a) (link 1 >>a) (seat >>a))
                              ())
               (me? <a) (link 1 >a)
               ())
   s (seat >cmdline)
   (? (two? s) ((from 'grocery 'grocery-main) >s) ()))
