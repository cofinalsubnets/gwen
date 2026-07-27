; --- crew/grocery/grocerytest.l: the grocery gate ---
; catted after seed's files and grocery.l, then run in a scratch directory.
; grocery is SEALED by the time we get here, so the test JOINS it the way a
; backend joins holo -- (enter ()) (use ..) -- and reads both modules bare.
; ⚠ both define `mk` (seed's store init, grocery's goods store), so the one
; place that means seed's is spelled out through the registry.
(enter ())
(use 'seed)
(use 'grocery)

(: root "out/host/.grocerytest"
   _ (run (list "rm" "-rf" root))
   _ (mkdir "out" 493) _ (mkdir "out/host" 493) _ (mkdir root 493)
   _ (mkdir (root + "/a") 493) _ (mkdir (root + "/b") 493)
   home (cwd 0))

; --- build: the recorded source -> goods, ledgered by (source, arch). the
;     recipe is a deterministic stub writing under out/ -- one of seed's
;     BUILT-IN ignores, so the build output can never dirty the tree it came
;     from, and seed needs to know nothing about grocery for that to hold.
;     building the same source twice is the VIABILITY test.
;     ⚠ Grocery.l is RECORDED, not written afterwards: how to build a thing is
;     part of the thing, so the config is versioned source. writing it after the
;     record leaves the tree dirty and the build (correctly) refuses.
;     ⚠ the artifact is KEPT, not merely fingerprinted: a build you can only
;     audit becomes a build you can also SHIP, which is what an install (and
;     doc/hatch.md's stage -1) needs there to be. ---
(: _ (chdir (root + "/a"))
   _ (lay "a.txt" "v1\n")
   _ (lay "Grocery.l"
       (show '(build (recipe "sh" "-c" "mkdir -p out; printf the-goods > out/thing.bin")
                     (out "out/thing.bin"))))
   _ ((from 'seed 'mk) ())
   _ (seed-record "first")
   bid (grocery-build ())
   led (ledger ())
   gid (<>><led)
   gback (good gid)                                     ; the artifact itself, back out of the store
   _ (unlink "out/thing.bin")                           ; the build output is gone; the goods are not
   gafter (good gid)
   again (grocery-build ())
   led2 (ledger ())
   ; grocery keys its ledger by SEED's name for the source state, not a private
   ; copy of the formula -- ask the vcs directly and the two must agree
   mypsid ((from 'seed 'psid) (tips ()))
   ; a dirty tree refuses, so what the ledger keys by always exists in the store.
   ; LAST, because it leaves this box dirty
   _ (lay "a.txt" "v2\n")
   dirtybuild (grocery-build (list "true"))
   _ (chdir home))

; --- a recorded box with no Grocery.l and no recipe on the command line:
;     refuse rather than guess at a build ---
(: _ (chdir (root + "/b"))
   _ (lay "b.txt" "hello\n")
   _ ((from 'seed 'mk) ())
   _ (seed-record "only")
   norec (grocery-build ())
   _ (chdir home))

(assert
 (64 = (tally bid))                                     ; the source state's name = seed's psid
 (bid = mypsid)                                         ; ...and it IS seed's, asked for directly
 (1 = (tally led))                                      ; one build ledgered...
 (bid = (<<led))                                        ; ...keyed by the source state
 ((<><led) = (arch ()))                                 ; ...and this machine's arch
 (64 = (tally gid))                                     ; ...and the goods' id, a sha256
 (two? gback)                                           ; the ARTIFACT is kept, not merely its id
 ((<>gback) = "the-goods")                              ; ...and comes back byte for byte
 (gid = (sha256 (<>gback)))                             ; ...under its own id
 (two? gafter)                                          ; ...and it OUTLIVES the build output
 (bid = again)                                          ; same source + recipe -> same name
 (2 = (tally led2))                                     ; a second, auditable build
 ((<>><led2) = gid)                                     ; ...that stayed VIABLE, bit for bit
 (! dirtybuild)                                         ; a dirty tree -> refuse
 (! norec))                                             ; no recipe -> refuse, don't guess

(: _ (puts "grocery: ok") _ (putc 10) 0)
