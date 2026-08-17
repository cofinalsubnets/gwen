; the svalbard gate (crew/sb/): a sandbox store driven end to end
; (record / log / diff, tips + snap + deps + the content addresses). runs under
; `make test_sb`: cat test/00-init.l THIS | out/host/love -- host nifs
; (sha256, readdir, chdir), so it stays OFF the corpus like the other hostnif
; tests. the make target clears the sandbox first; cwd is the repo root, and
; everything here happens inside out/host/.sbtest.
; the use below LOADS svalbard (lib/sb.l assembles the whole stack) and splices
; it under the session layer: every internal reads bare, and the test's own
; helpers stay above in the session.
(use 'sb)

(: _ (mkdir "out" 493) _ (mkdir "out/host" 493)
   _ (mkdir "out/host/.sbtest" 493)
   cd (chdir "out/host/.sbtest")
   _ (? !cd () (: _ (puts "sbtest: chdir failed\n") (quit 1)))
   ; the same-text equality for list-shaped store values
   (seq a b) (show a = show b)
   ; the tree: two files, one ignored dir
   _ (mkdir "sub" 493)
   _ (lay "a.txt" "one\ntwo\nthree\n")
   _ (lay "sub/b.txt" "hello\n")
   _ (mkdir "junk" 493)
   _ (lay "junk/x" "never\n")
   _ (mk ())
   _ (lay ".sb/ignore" "junk\npr\nnest2\nmrg\n")   ; junk/ + the nests we sync with
   p1 (sb-record "first")
   s1 (snap ())
   again (sb-record "again"))

(assert
 (64 = tally p1)                                       ; a real content address
 (seq (tips ()) `(p1))                              ; the one tip
 (2 = tally s1)                                        ; junk/ stayed out
 ("a.txt" = <<s1)                                      ; path-sorted
 (blob <><s1 = "one\ntwo\nthree\n")                 ; the blob rides its hash
 !again                                                ; a clean tree records nothing
 (p1 = sha256 <>(look (".sb/patch/" + p1))))      ; the id IS the file's hash

; mutate: one changed, one born, one dead
(: _ (lay "a.txt" "one\ntwo point five\nthree\n")
   _ (lay "c.txt" "born\n")
   _ (unlink "sub/b.txt")
   nd (sb-diff ())
   p2 (sb-record "second")
   f2 (patch p2))

(assert
 (3 = nd)                                              ; diff saw all three
 (seq (fld f2 'deps) `(p1))                         ; the context reads p1's writes
 (3 = tally (fld f2 'hunks))
 (seq (tips ()) `(p2))                              ; p1 is no longer maximal
 !(member? "sub/b.txt" (: (go m acc) (? (two? m) (go >m (<<m >< acc)) acc)
                          (go (snap ()) ())))       ; the death left the snap
 (0 = sb-diff ()))                                   ; recorded = working again

; a patch of only births depends on nothing: a second tip (the DAG, locally)
(: _ (lay "d.txt" "free\n")
   p3 (sb-record "third")
   f3 (patch p3))

(assert
 !(fld f3 'deps)                                    ; no context anywhere
 (2 = tally (tips ()))                              ; two maximal patches
 (member? p2 (tips ())) (member? p3 (tips ()))
 (3 = sb-log ()))                                    ; three patches told

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
   pp (sb-record "peer work")
   _ (chdir "..")
   nbefore (tally (readdir ".sb/patch"))
   pdirty (dirty ())                                   ; the main tree is clean pre-sync
   got (sb-sync "pr")
   nafter (tally (readdir ".sb/patch")))

(assert
 (0 = pdirty)                                             ; clean, so sync will materialize
 (< nbefore nafter)                                       ; the peer's patch arrived
 (two? (patch pp))                                     ; ...as a real, addressable patch object
 (member? pp (tips ()))                                ; and it opens a fresh tip
 (3 = tally (tips ()))                                 ; p2 + p3 + pp, all maximal
 (4 = got)                                                ; p1 p2 p3 + the peer's pp
 ((<>(look "peer.txt")) = "from the other machine\n")  ; materialized into the working tree
 ((<>(look "a.txt")) = "one\ntwo point five\nthree\n") ; local edits untouched
 (0 = sb-diff ())                                       ; working tree == the recomputed snap
 !(sb-record "noop"))                                   ; ...so nothing is left to record

; --- an EMPTY file is content, not absence. it slurps to () -- the zero point --
;     so a scan that reads the net hashes it to () and it never reaches the snap:
;     the tree is dirty FOREVER, every record buds a junk tip, and sync
;     (which both refuse a dirty tree) never run again. it must record, land in
;     the snap under the honest empty digest, and materialize 0-byte through a
;     sync (where blob presence is likewise the wrapper, never the net) ---
(: _ (lay "e.txt" "")
   pe (sb-record "an empty file")
   esnap (: (go m) (? !(two? m) () ("e.txt" = <<m) <m (go >m)) (go (snap ())))
   clean (dirty ())
   _ (mkdir "nest2" 493)
   _ (chdir "nest2")
   _ (mk ())
   got2 (sb-sync "..")
   et (look "e.txt")
   _ (chdir ".."))

(assert
 (64 = tally pe)                                          ; the empty file made a real patch
 (two? esnap)                                             ; ...and reached the snap
 (<>esnap = sha256 "")                                  ; ...under the honest empty digest
 (0 = clean)                                              ; ...so the tree came back clean
 !(sb-record "noop")                                    ; ...and stays clean -- no junk tip
 (0 < got2)                                               ; the fresh nest took the whole store
 (two? et)                                                ; ...materializing e.txt: present...
 (0 = tally <>et))                                        ; ...and 0 bytes

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
   _ (sb-record "the base")
   _ (mkdir "../b" 493) _ (chdir "../b") _ (mk ())
   _ (sb-sync "../a")
   ; a moves the head, b moves the tail -- one file, disjoint regions
   _ (chdir "../a") _ (lay "f" "ONE\ntwo\nthree\nfour\nfive\n") _ (sb-record "a: the head")
   _ (chdir "../b") _ (lay "f" "one\ntwo\nthree\nfour\nFIVE\n") _ (sb-record "b: the tail")
   gotm (sb-sync "../a")
   mtext (<>(look "f"))
   cfm (<>(derive-snap (readdir ".sb/patch")))
   ; now both move the SAME line -- a real overlap
   _ (chdir "../a") _ (lay "f" "ONE\nA-TWO\nthree\nfour\nFIVE\n") _ (sb-record "a: two")
   _ (chdir "../b") _ (lay "f" "ONE\nB-TWO\nthree\nfour\nFIVE\n") _ (sb-record "b: two")
   gotc (sb-sync "../a")
   ctext (<>(look "f"))
   cfc (<>(derive-snap (readdir ".sb/patch")))
   ; the resolution is just a record
   _ (lay "f" "ONE\nRESOLVED\nthree\nfour\nFIVE\n")
   _ (sb-record "resolve the overlap")
   cfr (<>(derive-snap (readdir ".sb/patch")))
   ndirty (dirty ())
   _ (chdir "../.."))

(assert
 (mtext = "ONE\ntwo\nthree\nfour\nFIVE\n")               ; BOTH disjoint edits survived
 (0 < gotm)                                              ; ...and it was not a conflict
 (1 = tally cfm) (0 = <><cfm)                          ; one merge, zero regions unresolved
 (0 = gotc)                                              ; the overlap DID conflict: sync answers 0
 (1 = <><cfc)                                          ; ...one region
 (member? "=======" (lines ctext))                       ; ...and the markers are in the file
 (member? "A-TWO" (lines ctext))                         ; ...carrying BOTH sides, neither lost
 (member? "B-TWO" (lines ctext))
 !cfr                                                    ; the record settled it -- nothing left
 (0 = ndirty))                                           ; ...and the tree matches the snap

; --- the two sides that cannot line-merge: a delete meeting an edit keeps the
;     CONTENT (a deletion would be the one lossy answer), and flags it ---
(: _ (chdir "mrg/a")
   _ (lay "g" "keep\nme\n") _ (sb-record "g is born")
   _ (chdir "../b") _ (sb-sync "../a")
   _ (chdir "../a") _ (unlink "g") _ (sb-record "a: g dies")
   _ (chdir "../b") _ (lay "g" "keep\nme too\n") _ (sb-record "b: g edited")
   gotd (sb-sync "../a")
   gtext (look "g")
   cfd (<>(derive-snap (readdir ".sb/patch")))
   _ (chdir "../.."))

(assert
 (0 = gotd)                                              ; a delete against an edit is a conflict
 (two? gtext)                                            ; the file is STILL THERE...
 (<>gtext = "keep\nme too\n")                          ; ...with the edit intact, not the deletion
 (1 = tally (filter (\ e ("g" = <e)) cfd)))              ; ...and g is named in the report

; --- sync is an EXCHANGE, not a fetch: ONE command settles BOTH nests. the
;     derive is a pure function of the patch set, so two nests over one union
;     land on the SAME snap -- after a sync the trees are identical and the far
;     end needed no attendance. a peer that is dirty takes no push at all (its
;     store is left whole, never half-fed) and sync says so ---
(: _ (mkdir "mrg/c" 493) _ (chdir "mrg/c") _ (mk ())
   _ (lay "shared" "base\n") _ (sb-record "c: the base")
   _ (mkdir "../d" 493) _ (chdir "../d") _ (mk ())
   _ (sb-sync "../c")
   _ (lay "d-only" "from d\n") _ (sb-record "d: its own file")
   _ (chdir "../c") _ (lay "c-only" "from c\n") _ (sb-record "c: its own file")
   gotx (sb-sync "../d")                               ; ONE sync, run from c
   csnap (snap ()) cdirty (dirty ())
   dhere (look "d-only")                              ; d's file walked in
   _ (chdir "../d")
   dsnap (snap ()) ddirty (dirty ())
   chere (look "c-only")                              ; ...and c's walked out, unattended
   _ (lay "scratch" "unrecorded\n")                   ; now dirty the peer
   _ (chdir "../c")
   nb4 (tally (readdir ".sb/patch"))
   _ (lay "late" "late work\n") _ (sb-record "c: late")
   goty (sb-sync "../d")
   dstore (tally (readdir "../d/.sb/patch"))
   _ (chdir "../.."))

(assert
 (0 < gotx)                                              ; the exchange went through
 (two? dhere) (two? chere)                               ; each nest took the other's file
 (show csnap = show dsnap)                               ; ...and both derived the SAME snap
 (0 = cdirty) (0 = ddirty)                               ; ...both trees clean, the far end untouched by hand
 (0 = goty)                                              ; a DIRTY peer refuses the push
 (dstore = nb4))                                         ; ...and its store is left whole, not half-fed

; --- apply: realize a dep-consistent SUBSET of the store. git's checkout AND
;     its cherry-pick are ONE act here -- the tree is a pure function of a patch
;     set, so both are only "name a set", with no replay-onto-a-foreign-state to
;     conflict. a named patch drags its DEP CLOSURE along: topo silently drops a
;     patch whose context-writer is absent, so leaving one behind would quietly
;     realize less than you asked for ---
(: _ (mkdir "mrg/e" 493) _ (chdir "mrg/e") _ (mk ())
   _ (lay "a" "v1\n") pa (sb-record "a born")
   _ (lay "a" "v2\n") pb (sb-record "a edited")
   _ (lay "b" "x\n")  pc (sb-record "b born -- independent")
   n1 (sb-apply `(pa))                                 ; checkout: the tree AS OF pa
   a1 (<>(look "a")) b1 (look "b")
   n2 (sb-apply `(pa pc))                              ; cherry-pick: pc taken, pb SKIPPED
   a2 (<>(look "a")) b2 (look "b")
   n3 (sb-apply `(pb))                                 ; the closure drags pa in behind pb
   n4 (sb-apply ())                                    ; no id: the way back
   a4 (<>(look "a"))
   bad (sb-apply '("deadbeef"))                        ; a prefix that resolves to nothing
   nstore (tally (readdir ".sb/patch"))                ; read HERE -- the assert runs after the chdir back
   clean (dirty ())
   _ (chdir "../.."))

(assert
 (1 = n1) (a1 = "v1\n") !(two? b1)                       ; as of pa: a rolled back, b not yet born
 (2 = n2) (a2 = "v1\n") (two? b2)                        ; pa + pc: pb's edit skipped, pc's birth kept
 (3 = nstore)                                            ; ...and the STORE never shrank -- apply is a view
 (2 = n3)                                                ; naming pb alone realized TWO: its dep came
 (3 = n4) (a4 = "v2\n")                                  ; the way back is the whole store
 !bad                                                    ; an unresolvable prefix answers 0
 (0 = clean))                                            ; every apply leaves the tree clean

; --- undo: the INVERSE patch. removal is GROWTH -- the patch stays, its
;     dependents stay valid, the store only gets bigger, and that is what keeps
;     releases inclusion-ordered. a path that MOVED ON since is three-way merged
;     rather than clobbered: base = what the patch wrote, ours = what is there
;     now, theirs = what it replaced -- so the later edits survive and only that
;     patch's write is lifted out. a true overlap records NOTHING ---
(: _ (mkdir "mrg/u" 493) _ (chdir "mrg/u") _ (mk ())
   _ (lay "f" "one\ntwo\nthree\n") _ (sb-record "base")
   _ (lay "f" "one\nBAD\nthree\n") pbad (sb-record "the bad edit, line 2")
   _ (lay "f" "one\nBAD\nTHREE!\n") _ (sb-record "later work, line 3")
   n0 (tally (readdir ".sb/patch"))
   u1 (sb-undo `(pbad))
   ftext (<>(look "f"))
   n1 (tally (readdir ".sb/patch"))
   clean (dirty ())
   _ (lay "g" "born\n") pg (sb-record "g born")       ; a birth undone is a death
   u2 (sb-undo `(pg))
   gone (look "g")
   _ (lay "h" "a\nb\n") _ (sb-record "h base")        ; the OVERLAP: same line, later patch
   _ (lay "h" "a\nB2\n") pq (sb-record "h -> B2")
   _ (lay "h" "a\nB3\n") _ (sb-record "h -> B3")
   n2 (tally (readdir ".sb/patch"))
   u3 (sb-undo `(pq))
   n3 (tally (readdir ".sb/patch"))
   _ (lay "h" "a\nB3\n")                                ; settle the markers by hand
   _ (chdir "../.."))

(assert
 (1 = u1)                                               ; the undo landed as a patch
 (ftext = "one\ntwo\nTHREE!\n")                        ; the bad edit LIFTED, the later work KEPT
 (n1 = (n0 + 1))                                        ; ...by growing the store, never shrinking it
 (0 = clean)                                            ; ...and the tree matches the snap after
 (1 = u2) !(two? gone)                                  ; a birth undone leaves no file
 !u3                                                    ; a true overlap answers 0...
 (n3 = n2))                                             ; ...and records NOTHING -- resolve first

; --- bank: freeze the head under a NAME, immutable. ⚠ hatch.md asks for a
;     SINGLE-TIP head and that does not survive per-path deps: an independent
;     birth is never depended upon, so it stays maximal and TWO tips is what
;     ordinary parallel work looks like, not a fork to repair. a ref freezes the
;     TIP SET instead -- the head DAG state whatever its shape, and the same
;     thing psid hashes, so a release and its goods line up ---
(: _ (mkdir "mrg/k" 493) _ (chdir "mrg/k") _ (mk ())
   _ (lay "a" "1\n") _ (sb-record "a born")
   _ (lay "b" "2\n") _ (sb-record "b born -- independent, so BOTH stay tips")
   ntip (tally (tips ()))
   b1 (sb-bank '("v1"))
   b2 (sb-bank '("v1"))                                ; the same head again: idempotent
   r1 (refof "v1")
   _ (lay "c" "3\n") _ (sb-record "work after the release")
   b3 (sb-bank '("v1"))                                ; a DIFFERENT head: immutable, refuses
   b4 (sb-bank '("v2"))
   n1 (sb-apply '("v1"))                               ; realize the release BY NAME
   cgone (look "c")
   n2 (sb-apply '("v2"))
   cback (look "c")
   bad (sb-bank '(""))
   _ (lay "a" "dirty\n") bdirty (sb-bank '("v3"))      ; a dirty tree refuses too
   _ (lay "a" "1\n")
   _ (chdir "../.."))

(assert
 (2 = ntip)                                              ; two independent births, TWO tips -- normal
 (1 = b1) (1 = b2)                                       ; banked; re-banking the same head is a no-op
 (2 = tally r1)                                          ; the ref froze the TIP SET, not one id
 !b3 (1 = b4)                                            ; a banked name is IMMUTABLE
 (2 = n1) !(two? cgone)                                  ; apply BY NAME realized exactly the release
 (3 = n2) (two? cback)                                   ; ...and the later ref brings c back
 !bad !bdirty)                                           ; an empty name and a dirty tree both refuse

; --- refs travel. a release is only "the unit you propagate" if the NAME
;     reaches the other nest with the patches. a ref is a single FILE, not a
;     content-addressed one, so sync UNIONS it by name instead of gap-filling.
;     the same name at the SAME head is idempotent; at DIFFERENT heads it is a
;     human error, since a banked name is immutable and neither nest may repoint
;     the other's -- so EACH SIDE KEEPS ITS OWN and the clash is told once ---
(: _ (mkdir "mrg/r" 493) _ (mkdir "mrg/r/l" 493) _ (chdir "mrg/r/l") _ (mk ())
   _ (lay "f" "base\n") _ (sb-record "base") _ (sb-bank '("v1"))
   _ (mkdir "../d" 493) _ (chdir "../d") _ (mk ())
   got (sb-sync "../l")
   travelled (refof "v1")                                ; ...it arrived, unbanked here
   n1 (sb-apply '("v1"))                               ; ...and realizes BY NAME
   ; now bank the SAME name on each side at DIFFERENT heads
   _ (lay "g" "d\n") _ (sb-record "d work") _ (sb-bank '("rel"))
   drel (refof "rel")
   _ (chdir "../l") _ (lay "h" "l\n") _ (sb-record "l work") _ (sb-bank '("rel"))
   lrel (refof "rel")
   clash (sb-sync "../d")
   lkept (refof "rel")                                   ; l kept ITS rel...
   _ (chdir "../d") dkept (refof "rel")                  ; ...and d kept ITS
   _ (chdir "../../.."))

(assert
 (0 < got) (two? travelled)                              ; the ref crossed with the patches
 (1 = n1)                                                ; ...and named a release the nest never banked
 !(show drel = show lrel)                                ; the two `rel`s really are different heads
 !clash                                                  ; a clash makes sync answer 0
 (show lkept = show lrel)                                ; each side KEPT ITS OWN -- no silent loss
 (show dkept = show drel))

(: _ (puts "sb: ok") _ (putc 10) 0)
