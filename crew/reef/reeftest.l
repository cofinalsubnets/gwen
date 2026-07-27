; crew/reef/reeftest.l -- the reef gate: a sandbox store driven end to end
; (record / log / diff, tips + snap + deps + the content addresses). runs under
; `make test_reef`: cat test/00-init.l $(reeffiles) THIS | out/host/love -- host
; nifs (sha256, readdir, chdir), so it stays OFF the corpus like the other
; hostnif tests. the make target clears the sandbox first; cwd is the repo
; root, and everything here happens inside out/host/.reeftest.
(: _ (mkdir "out" 493) _ (mkdir "out/host" 493)
   _ (mkdir "out/host/.reeftest" 493)
   cd (chdir "out/host/.reeftest")
   _ (? (! cd) () (: _ (puts "reeftest: chdir failed\n") (quit 1)))
   ; the same-text equality for list-shaped store values
   (seq a b) ((show a) = (show b))
   ; the tree: two files, one ignored dir
   _ (mkdir "sub" 493)
   _ (rf-lay "a.txt" "one\ntwo\nthree\n")
   _ (rf-lay "sub/b.txt" "hello\n")
   _ (mkdir "junk" 493)
   _ (rf-lay "junk/x" "never\n")
   _ (rf-mk ())
   _ (rf-lay ".reef/ignore" "junk\npr\nnest2\nmrg\n")   ; junk/ + the nests we sync with
   p1 (reef-record "first")
   s1 (rf-snap ())
   again (reef-record "again"))

(assert
 (64 = (tally p1))                                     ; a real content address
 (seq (rf-tips ()) `(p1))                              ; the one tip
 (2 = (tally s1))                                      ; junk/ stayed out
 ("a.txt" = <<s1)                                      ; path-sorted
 ((rf-blob <><s1) = "one\ntwo\nthree\n")               ; the blob rides its hash
 (! again)                                             ; a clean tree records nothing
 (p1 = (sha256 <>(rf-look (".reef/patch/" + p1)))))    ; the id IS the file's hash

; mutate: one changed, one born, one dead
(: _ (rf-lay "a.txt" "one\ntwo point five\nthree\n")
   _ (rf-lay "c.txt" "born\n")
   _ (unlink "sub/b.txt")
   nd (reef-diff ())
   p2 (reef-record "second")
   f2 (rf-patch p2))

(assert
 (3 = nd)                                              ; diff saw all three
 (seq (rf-fld f2 'deps) `(p1))                         ; the context reads p1's writes
 (3 = (tally (rf-fld f2 'hunks)))
 (seq (rf-tips ()) `(p2))                              ; p1 is no longer maximal
 (! (member? "sub/b.txt" (: (go m acc) (? (two? m) (go >m (link <<m acc)) acc)
                            (go (rf-snap ()) ()))))    ; the death left the snap
 (0 = (reef-diff ())))                                 ; recorded = working again

; a patch of only births depends on nothing: a second tip (the DAG, locally)
(: _ (rf-lay "d.txt" "free\n")
   p3 (reef-record "third")
   f3 (rf-patch p3))

(assert
 (! (rf-fld f3 'deps))                                 ; no context anywhere
 (2 = (tally (rf-tips ())))                            ; two maximal patches
 (member? p2 (rf-tips ())) (member? p3 (rf-tips ()))
 (3 = (reef-log ())))                                  ; three patches told

; --- sync: a peer nest (built inside ./pr, ignored) shares d.txt's births and
;     adds its own, plus edits a.txt on a common base. union it in: the store
;     gains the peer's patch, tips + snap re-derive over the whole set, and the
;     working tree materializes to the merged state. content-addressing dedups
;     the shared blobs, so only the genuinely-new patch/blob copy over ---
(: _ (mkdir "pr" 493)
   _ (chdir "pr")
   _ (rf-lay "d.txt" "free\n")                            ; same births as p3 -> dedups
   _ (rf-lay "peer.txt" "from the other machine\n")       ; the peer's own file
   _ (rf-mk ())
   pp (reef-record "peer work")
   _ (chdir "..")
   nbefore (tally (readdir ".reef/patch"))
   pdirty (rf-dirty ())                                   ; the main tree is clean pre-sync
   got (reef-sync "pr")
   nafter (tally (readdir ".reef/patch")))

(assert
 (0 = pdirty)                                             ; clean, so sync will materialize
 (< nbefore nafter)                                       ; the peer's patch arrived
 (two? (rf-patch pp))                                     ; ...as a real, addressable patch object
 (member? pp (rf-tips ()))                                ; and it opens a fresh tip
 (3 = (tally (rf-tips ())))                               ; p2 + p3 + pp, all maximal
 (4 = got)                                                ; p1 p2 p3 + the peer's pp
 ((<>(rf-look "peer.txt")) = "from the other machine\n")  ; materialized into the working tree
 ((<>(rf-look "a.txt")) = "one\ntwo point five\nthree\n") ; local edits untouched
 (0 = (reef-diff ()))                                     ; working tree == the recomputed snap
 (! (reef-record "noop")))                                ; ...so nothing is left to record

; --- an EMPTY file is content, not absence. it slurps to () -- the zero point --
;     so a scan that reads the net hashes it to () and it never reaches the snap:
;     the tree is dirty FOREVER, every record buds a junk tip, and sync/hatch
;     (which both refuse a dirty tree) never run again. it must record, land in
;     the snap under the honest empty digest, and materialize 0-byte through a
;     sync (where blob presence is likewise the wrapper, never the net) ---
(: _ (rf-lay "e.txt" "")
   pe (reef-record "an empty file")
   esnap (: (go m) (? (! (two? m)) () ("e.txt" = <<m) <m (go >m)) (go (rf-snap ())))
   clean (rf-dirty ())
   _ (mkdir "nest2" 493)
   _ (chdir "nest2")
   _ (rf-mk ())
   got2 (reef-sync "..")
   et (rf-look "e.txt")
   _ (chdir ".."))

(assert
 (64 = (tally pe))                                        ; the empty file made a real patch
 (two? esnap)                                             ; ...and reached the snap
 ((<>esnap) = (sha256 ""))                                ; ...under the honest empty digest
 (0 = clean)                                              ; ...so the tree came back clean
 (! (reef-record "noop"))                                 ; ...and stays clean -- no junk tip
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
   _ (rf-lay "f" "one\ntwo\nthree\nfour\nfive\n")
   _ (rf-mk ())
   _ (reef-record "the base")
   _ (mkdir "../b" 493) _ (chdir "../b") _ (rf-mk ())
   _ (reef-sync "../a")
   ; a moves the head, b moves the tail -- one file, disjoint regions
   _ (chdir "../a") _ (rf-lay "f" "ONE\ntwo\nthree\nfour\nfive\n") _ (reef-record "a: the head")
   _ (chdir "../b") _ (rf-lay "f" "one\ntwo\nthree\nfour\nFIVE\n") _ (reef-record "b: the tail")
   gotm (reef-sync "../a")
   mtext (<>(rf-look "f"))
   cfm (<>(rf-derive-snap (readdir ".reef/patch")))
   ; now both move the SAME line -- a real overlap
   _ (chdir "../a") _ (rf-lay "f" "ONE\nA-TWO\nthree\nfour\nFIVE\n") _ (reef-record "a: two")
   _ (chdir "../b") _ (rf-lay "f" "ONE\nB-TWO\nthree\nfour\nFIVE\n") _ (reef-record "b: two")
   gotc (reef-sync "../a")
   ctext (<>(rf-look "f"))
   cfc (<>(rf-derive-snap (readdir ".reef/patch")))
   ; the resolution is just a record
   _ (rf-lay "f" "ONE\nRESOLVED\nthree\nfour\nFIVE\n")
   _ (reef-record "resolve the overlap")
   cfr (<>(rf-derive-snap (readdir ".reef/patch")))
   ndirty (rf-dirty ())
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
   _ (rf-lay "g" "keep\nme\n") _ (reef-record "g is born")
   _ (chdir "../b") _ (reef-sync "../a")
   _ (chdir "../a") _ (unlink "g") _ (reef-record "a: g dies")
   _ (chdir "../b") _ (rf-lay "g" "keep\nme too\n") _ (reef-record "b: g edited")
   gotd (reef-sync "../a")
   gtext (rf-look "g")
   cfd (<>(rf-derive-snap (readdir ".reef/patch")))
   _ (chdir "../.."))

(assert
 (0 = gotd)                                              ; a delete against an edit is a conflict
 (two? gtext)                                            ; the file is STILL THERE...
 ((<>gtext) = "keep\nme too\n")                          ; ...with the edit intact, not the deletion
 (1 = (tally (filter (\ e ("g" = <e)) cfd))))            ; ...and g is named in the report

; --- hatch: build the recorded source into a native seed, ledgered by patch
;     set + arch. the recipe is a deterministic stub writing under out/ (a
;     default ignore, so the build output never dirties the tree); a re-hatch of
;     the same head with the same recipe is a reproducibility audit -- the seed
;     must reproduce. (the real love recipe is `make`; the shape is identical.) ---
(: _ (rf-lay ".reef/hatch"
       (show '(hatch (recipe "sh" "-c" "mkdir -p out; printf reef-seed > out/seed.bin") (out "out/seed.bin"))))
   hid (reef-hatch ())
   led (rf-hatched ())
   again (reef-hatch ())
   led2 (rf-hatched ()))

(assert
 (64 = (tally hid))                                    ; the patchset id = sha256 of the sorted tips
 (0 = (rf-dirty ()))                                   ; the build output stayed under ignored out/
 (1 = (tally led))                                     ; one derivation ledgered...
 (hid = (<<led))                                       ; ...keyed by the patchset id
 ((<><led) = (rf-arch ()))                             ; ...with this machine's arch (second field)
 (64 = (tally (<>><led)))                              ; ...and a real seed fingerprint (third)
 (hid = again)                                         ; same head + recipe -> same patchset id
 (2 = (tally led2))                                    ; a second, auditable hatch
 ((<>><led2) = (<>><led)))                             ; ...and the seed reproduced bit-for-bit

(: _ (puts "reef: ok") _ (putc 10) 0)
