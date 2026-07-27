; crew/seed/seedtest.l -- the seed gate: a sandbox store driven end to end
; (record / log / diff, tips + snap + deps + the content addresses). runs under
; `make test_seed`: cat test/00-init.l $(seedfiles) THIS | out/host/love -- host
; nifs (sha256, readdir, chdir), so it stays OFF the corpus like the other
; hostnif tests. the make target clears the sandbox first; cwd is the repo
; root, and everything here happens inside out/host/.seedtest.
; seed is a sealed module, so the gate JOINS it the documented way: an anonymous
; layer with seed spliced below (the backend-joins-holo shape). every internal
; then reads bare, and the test's own helpers stay in the top layer.
(enter ())
(use 'seed)

(: _ (mkdir "out" 493) _ (mkdir "out/host" 493)
   _ (mkdir "out/host/.seedtest" 493)
   cd (chdir "out/host/.seedtest")
   _ (? (! cd) () (: _ (puts "seedtest: chdir failed\n") (quit 1)))
   ; the same-text equality for list-shaped store values
   (seq a b) ((show a) = (show b))
   ; the tree: two files, one ignored dir
   _ (mkdir "sub" 493)
   _ (lay "a.txt" "one\ntwo\nthree\n")
   _ (lay "sub/b.txt" "hello\n")
   _ (mkdir "junk" 493)
   _ (lay "junk/x" "never\n")
   _ (mk ())
   _ (lay ".seed/ignore" "junk\npr\nnest2\nmrg\n")   ; junk/ + the nests we sync with
   p1 (seed-record "first")
   s1 (snap ())
   again (seed-record "again"))

(assert
 (64 = (tally p1))                                     ; a real content address
 (seq (tips ()) `(p1))                              ; the one tip
 (2 = (tally s1))                                      ; junk/ stayed out
 ("a.txt" = <<s1)                                      ; path-sorted
 ((blob <><s1) = "one\ntwo\nthree\n")               ; the blob rides its hash
 (! again)                                             ; a clean tree records nothing
 (p1 = (sha256 <>(look (".seed/patch/" + p1)))))    ; the id IS the file's hash

; mutate: one changed, one born, one dead
(: _ (lay "a.txt" "one\ntwo point five\nthree\n")
   _ (lay "c.txt" "born\n")
   _ (unlink "sub/b.txt")
   nd (seed-diff ())
   p2 (seed-record "second")
   f2 (patch p2))

(assert
 (3 = nd)                                              ; diff saw all three
 (seq (fld f2 'deps) `(p1))                         ; the context reads p1's writes
 (3 = (tally (fld f2 'hunks)))
 (seq (tips ()) `(p2))                              ; p1 is no longer maximal
 (! (member? "sub/b.txt" (: (go m acc) (? (two? m) (go >m (link <<m acc)) acc)
                            (go (snap ()) ()))))    ; the death left the snap
 (0 = (seed-diff ())))                                 ; recorded = working again

; a patch of only births depends on nothing: a second tip (the DAG, locally)
(: _ (lay "d.txt" "free\n")
   p3 (seed-record "third")
   f3 (patch p3))

(assert
 (! (fld f3 'deps))                                 ; no context anywhere
 (2 = (tally (tips ())))                            ; two maximal patches
 (member? p2 (tips ())) (member? p3 (tips ()))
 (3 = (seed-log ())))                                  ; three patches told

; --- sync: a peer nest (built inside ./pr, ignored) shares d.txt's births and
;     adds its own, plus edits a.txt on a common base. union it in: the store
;     gains the peer's patch, tips + snap re-derive over the whole set, and the
;     working tree materializes to the merged state. content-addressing dedups
;     the shared blobs, so only the genuinely-new patch/blob copy over ---
(: _ (mkdir "pr" 493)
   _ (chdir "pr")
   _ (lay "d.txt" "free\n")                            ; same births as p3 -> dedups
   _ (lay "peer.txt" "from the other machine\n")       ; the peer's own file
   _ (mk ())
   pp (seed-record "peer work")
   _ (chdir "..")
   nbefore (tally (readdir ".seed/patch"))
   pdirty (dirty ())                                   ; the main tree is clean pre-sync
   got (seed-sync "pr")
   nafter (tally (readdir ".seed/patch")))

(assert
 (0 = pdirty)                                             ; clean, so sync will materialize
 (< nbefore nafter)                                       ; the peer's patch arrived
 (two? (patch pp))                                     ; ...as a real, addressable patch object
 (member? pp (tips ()))                                ; and it opens a fresh tip
 (3 = (tally (tips ())))                               ; p2 + p3 + pp, all maximal
 (4 = got)                                                ; p1 p2 p3 + the peer's pp
 ((<>(look "peer.txt")) = "from the other machine\n")  ; materialized into the working tree
 ((<>(look "a.txt")) = "one\ntwo point five\nthree\n") ; local edits untouched
 (0 = (seed-diff ()))                                     ; working tree == the recomputed snap
 (! (seed-record "noop")))                                ; ...so nothing is left to record

; --- an EMPTY file is content, not absence. it slurps to () -- the zero point --
;     so a scan that reads the net hashes it to () and it never reaches the snap:
;     the tree is dirty FOREVER, every record buds a junk tip, and sync/hatch
;     (which both refuse a dirty tree) never run again. it must record, land in
;     the snap under the honest empty digest, and materialize 0-byte through a
;     sync (where blob presence is likewise the wrapper, never the net) ---
(: _ (lay "e.txt" "")
   pe (seed-record "an empty file")
   esnap (: (go m) (? (! (two? m)) () ("e.txt" = <<m) <m (go >m)) (go (snap ())))
   clean (dirty ())
   _ (mkdir "nest2" 493)
   _ (chdir "nest2")
   _ (mk ())
   got2 (seed-sync "..")
   et (look "e.txt")
   _ (chdir ".."))

(assert
 (64 = (tally pe))                                        ; the empty file made a real patch
 (two? esnap)                                             ; ...and reached the snap
 ((<>esnap) = (sha256 ""))                                ; ...under the honest empty digest
 (0 = clean)                                              ; ...so the tree came back clean
 (! (seed-record "noop"))                                 ; ...and stays clean -- no junk tip
 (0 < got2)                                               ; the fresh nest took the whole store
 (two? et)                                                ; ...materializing e.txt: present...
 (0 = (tally <>et)))                                      ; ...and 0 bytes

; --- divergence on ONE path is a MERGE, not a coin flip. the incoming hunk
;     names the base it expected, so all three sides sit in the store: disjoint
;     edits to one file BOTH survive (the case that used to drop a side by hash
;     order), a true overlap lands in markers and sync answers 0, and the
;     resolution is an ordinary record -- after which the clash is settled for
;     good. two nests under an ignored parent so the outer walk never sees them ---
(: _ (mkdir "mrg" 493) _ (mkdir "mrg/a" 493)
   _ (chdir "mrg/a")
   _ (lay "f" "one\ntwo\nthree\nfour\nfive\n")
   _ (mk ())
   _ (seed-record "the base")
   _ (mkdir "../b" 493) _ (chdir "../b") _ (mk ())
   _ (seed-sync "../a")
   ; a moves the head, b moves the tail -- one file, disjoint regions
   _ (chdir "../a") _ (lay "f" "ONE\ntwo\nthree\nfour\nfive\n") _ (seed-record "a: the head")
   _ (chdir "../b") _ (lay "f" "one\ntwo\nthree\nfour\nFIVE\n") _ (seed-record "b: the tail")
   gotm (seed-sync "../a")
   mtext (<>(look "f"))
   cfm (<>(derive-snap (readdir ".seed/patch")))
   ; now both move the SAME line -- a real overlap
   _ (chdir "../a") _ (lay "f" "ONE\nA-TWO\nthree\nfour\nFIVE\n") _ (seed-record "a: two")
   _ (chdir "../b") _ (lay "f" "ONE\nB-TWO\nthree\nfour\nFIVE\n") _ (seed-record "b: two")
   gotc (seed-sync "../a")
   ctext (<>(look "f"))
   cfc (<>(derive-snap (readdir ".seed/patch")))
   ; the resolution is just a record
   _ (lay "f" "ONE\nRESOLVED\nthree\nfour\nFIVE\n")
   _ (seed-record "resolve the overlap")
   cfr (<>(derive-snap (readdir ".seed/patch")))
   ndirty (dirty ())
   _ (chdir "../.."))

(assert
 (mtext = "ONE\ntwo\nthree\nfour\nFIVE\n")               ; BOTH disjoint edits survived
 (0 < gotm)                                              ; ...and it was not a conflict
 (1 = (tally cfm)) (0 = (<><cfm))                        ; one merge, zero regions unresolved
 (0 = gotc)                                              ; the overlap DID conflict: sync answers 0
 (1 = (<><cfc))                                          ; ...one region
 (member? "=======" (lines ctext))                       ; ...and the markers are in the file
 (member? "A-TWO" (lines ctext))                         ; ...carrying BOTH sides, neither lost
 (member? "B-TWO" (lines ctext))
 (! cfr)                                                 ; the record settled it -- nothing left
 (0 = ndirty))                                           ; ...and the tree matches the snap

; --- the two sides that cannot line-merge: a delete meeting an edit keeps the
;     CONTENT (a deletion would be the one lossy answer), and flags it ---
(: _ (chdir "mrg/a")
   _ (lay "g" "keep\nme\n") _ (seed-record "g is born")
   _ (chdir "../b") _ (seed-sync "../a")
   _ (chdir "../a") _ (unlink "g") _ (seed-record "a: g dies")
   _ (chdir "../b") _ (lay "g" "keep\nme too\n") _ (seed-record "b: g edited")
   gotd (seed-sync "../a")
   gtext (look "g")
   cfd (<>(derive-snap (readdir ".seed/patch")))
   _ (chdir "../.."))

(assert
 (0 = gotd)                                              ; a delete against an edit is a conflict
 (two? gtext)                                            ; the file is STILL THERE...
 ((<>gtext) = "keep\nme too\n")                          ; ...with the edit intact, not the deletion
 (1 = (tally (filter (\ e ("g" = <e)) cfd))))            ; ...and g is named in the report

; --- sync is an EXCHANGE, not a fetch: ONE command settles BOTH nests. the
;     derive is a pure function of the patch set, so two nests over one union
;     land on the SAME snap -- after a sync the trees are identical and the far
;     end needed no attendance. a peer that is dirty takes no push at all (its
;     store is left whole, never half-fed) and sync says so ---
(: _ (mkdir "mrg/c" 493) _ (chdir "mrg/c") _ (mk ())
   _ (lay "shared" "base\n") _ (seed-record "c: the base")
   _ (mkdir "../d" 493) _ (chdir "../d") _ (mk ())
   _ (seed-sync "../c")
   _ (lay "d-only" "from d\n") _ (seed-record "d: its own file")
   _ (chdir "../c") _ (lay "c-only" "from c\n") _ (seed-record "c: its own file")
   gotx (seed-sync "../d")                               ; ONE sync, run from c
   csnap (snap ()) cdirty (dirty ())
   dhere (look "d-only")                              ; d's file walked in
   _ (chdir "../d")
   dsnap (snap ()) ddirty (dirty ())
   chere (look "c-only")                              ; ...and c's walked out, unattended
   _ (lay "scratch" "unrecorded\n")                   ; now dirty the peer
   _ (chdir "../c")
   nb4 (tally (readdir ".seed/patch"))
   _ (lay "late" "late work\n") _ (seed-record "c: late")
   goty (seed-sync "../d")
   dstore (tally (readdir "../d/.seed/patch"))
   _ (chdir "../.."))

(assert
 (0 < gotx)                                              ; the exchange went through
 (two? dhere) (two? chere)                               ; each nest took the other's file
 ((show csnap) = (show dsnap))                           ; ...and both derived the SAME snap
 (0 = cdirty) (0 = ddirty)                               ; ...both trees clean, the far end untouched by hand
 (0 = goty)                                              ; a DIRTY peer refuses the push
 (dstore = nb4))                                         ; ...and its store is left whole, not half-fed

; --- hatch: build the recorded source into a native germ, ledgered by patch
;     set + arch. the recipe is a deterministic stub writing under out/ (a
;     default ignore, so the build output never dirties the tree); a re-hatch of
;     the same head with the same recipe is a VIABILITY test -- the germ
;     must reproduce. (the real love recipe is `make`; the shape is identical.) ---
(: _ (lay ".seed/hatch"
       (show '(hatch (recipe "sh" "-c" "mkdir -p out; printf seed-germ > out/germ.bin") (out "out/germ.bin"))))
   hid (seed-hatch ())
   led (hatched ())
   again (seed-hatch ())
   led2 (hatched ()))

(assert
 (64 = (tally hid))                                    ; the patchset id = sha256 of the sorted tips
 (0 = (dirty ()))                                   ; the build output stayed under ignored out/
 (1 = (tally led))                                     ; one derivation ledgered...
 (hid = (<<led))                                       ; ...keyed by the patchset id
 ((<><led) = (arch ()))                             ; ...with this machine's arch (second field)
 (64 = (tally (<>><led)))                              ; ...and a real germ fingerprint (third)
 (hid = again)                                         ; same head + recipe -> same patchset id
 (2 = (tally led2))                                    ; a second, auditable hatch
 ((<>><led2) = (<>><led)))                             ; ...and the germ stayed VIABLE, bit for bit

(: _ (puts "seed: ok") _ (putc 10) 0)
