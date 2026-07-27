; crew/reef/reef.l -- the reef: the patch-set vcs (doc/reef.md the interface,
; doc/hatch.md the model, test/patch.l + proof/rocq/patch.v the laws). this cut
; is the MVP's local half: `record` (working changes -> a patch in the store),
; `log`, `diff`; `sync` rides the same store next (patches + blobs are immutable
; content-addressed files, so union = copy what's missing + re-derive tips).
;
; the store (.reef/ at the working root):
;   blob/<sha256>    file content, immutable
;   patch/<sha256>   one patch, immutable -- (patch (note S) (time MS)
;                    (deps ID..) (hunks (PATH OLD NEW)..)), id = sha256 of the
;                    file's bytes; hunks path-sorted, deps sorted, so the text
;                    is deterministic and the address honest
;   tips             the maximal patches (the growth ends) -- (ID ..)
;   snap             the recorded state -- ((PATH CHASH PATCHID) ..) path-sorted
;   ignore           optional: one path prefix per line, atop (".reef" ".git" "out")
;
; a hunk is the PROVEN named-slot patch (test/patch.l's chg): slot = the path,
; context = the old content hash, value = the new -- () for absent (a birth or a
; death). patches touching disjoint paths commute (L3a), so sync's union needs
; no order; removal is a new patch whose hunk goes to () (the inverse-patch law:
; the set only grows). deps = for each touched path, the patch that wrote its
; context. finer-than-file hunks slot in later (the sequence slices are proven
; too); the store format doesn't care what a hunk's old/new mean.
;
; loads after crew/kore/text.l (lines) + crew/kore/diff.l (udiff) in the reef
; cat. body-less top-level `:` (the tool shape): the engine leaks into the cat's
; scope; the SEAT form at the tail is the only thing that fires as a program.
(:
   ; --- files ---
   ; presence is the (1 _) wrapper, content is always a string: an EMPTY file
   ; slurps to () -- the zero point -- and `+ ""` folds it back onto the shared
   ; identity, so a 0-byte file hashes honestly instead of to ().
   (rf-look p) (: q (open p "r") (? q (: s (slurp q) _ (close q) `(1 (s + ""))) ()))  ; (1 text) | ()
   (rf-lay p s) (: q (open p "w") (? q (: _ (say q s) _ (close q) 1) 0))
   (rf-form p) (: q (open p "r")                                 ; the file's one form | ()
                  (? q (: r (sound q ()) _ (close q) (? (two? r) r ())) ()))

   ; --- the store ---
   (rf-mk _) (: _ (mkdir ".reef" 493) _ (mkdir ".reef/blob" 493) _ (mkdir ".reef/patch" 493) ())
   (rf-snap _) (rf-form ".reef/snap")                            ; ((path chash pid) ..) sorted
   (rf-tips _) (rf-form ".reef/tips")                            ; (id ..)
   (rf-blob h) (: t (rf-look (".reef/blob/" + h)) (? (two? t) <>t ()))
   (rf-keep s) (: h (sha256 s) _ (rf-lay (".reef/blob/" + h) s) h)
   (rf-patch id) (rf-form (".reef/patch/" + id))
   (rf-fld f k) (: (go m) (? (! (two? m)) ()                     ; a patch field's tail
                             (&& (two? <m) (id? <<m k)) ><m
                             (go >m))
                   (go >f))

   ; --- ignores: path prefixes; a line in .reef/ignore adds to the built-ins ---
   rf-igs0 '(".reef" ".git" "out")
   (rf-igs _) (: t (rf-look ".reef/ignore")
                 (? (two? t) (rf-igs0 + (filter (\ l (0 < (tally l))) (lines <>t)))
                    rf-igs0))
   (rf-ign? p igs) (? (! (two? igs)) 0
                      (: g <igs n (tally g)
                         (? (|| (p = g) (&& ((n + 1) < (tally p)) ((snip p 0 (n + 1)) = (g + "/"))))
                            1 (rf-ign? p >igs))))

   ; --- the walk: every regular file under the root, paths relative ---
   (rf-walk dir igs acc)
    (: es (readdir (? (! dir) "." dir))
       (go m acc)
        (? (! (two? m)) acc
           (: p (? (! dir) <m (dir + "/" + <m))
              (? (rf-ign? p igs) (go >m acc)
                 (: st (stat p)
                    md (? (two? st) (& 61440 <>>st) 0)
                    (? (md = 16384) (go >m (rf-walk p igs acc))
                       (md = 32768) (go >m (link p acc))
                       (go >m acc))))))
       (go es acc))

   ; --- the scan: working tree vs snap -> the hunks, path-sorted. PURE (no
   ;     blob writes); a hunk is (path old new), old/new content hashes | () ---
   (rf-h1 p e)                                                   ; one path vs its snap entry|()
    (: t (rf-look p)
       (? (! (two? t)) (? (two? e) `(p (<>e) ()) ())             ; unreadable: dead if known
          (: h (sha256 <>t)
             (? (! (two? e)) `(p () h)                           ; born
                ((<>e) = h) ()                                   ; unchanged
                `(p (<>e) h)))))                                 ; changed
   (rf-scan ps sn acc)                                           ; ps sorted paths, sn the snap
    (? (! (two? ps))
       (? (! (two? sn)) (rev acc)
          (rf-scan ps >sn (link `((<<sn) (<><sn) ()) acc)))      ; the tail of snap: dead
       (! (two? sn))
       (: h (rf-h1 <ps ()) (rf-scan >ps sn (? (two? h) (link h acc) acc)))
       (: p <ps e <sn
          (? (p = <e) (: h (rf-h1 p e) (rf-scan >ps >sn (? (two? h) (link h acc) acc)))
             (p < <e) (: h (rf-h1 p ()) (rf-scan >ps sn (? (two? h) (link h acc) acc)))
             (rf-scan ps >sn (link `((<e) (<>e) ()) acc)))))     ; snap-only: dead

   ; --- record ---
   (reef-record note)
    (: _ (rf-mk ())
       snap (rf-snap ())
       st (tablet 0)                                             ; path -> (chash pid)
       _ (: (go m) (? (two? m) (: _ (pin st <<m ><m) (go >m)) ()) (go snap))
       hunks (rf-scan (sort (rf-walk () (rf-igs ()) ())) snap ())
       (? (! (two? hunks)) (: _ (puts "reef: nothing to record\n") ())
          (: ; the blobs: keep every hunk's new content
             _ (: (go m) (? (two? m)
                            (: h <m _ (? (! <>>h) () (rf-keep <>(rf-look <h))) (go >m))
                            ())
                  (go hunks))
             ; the deps: the patches whose writes this one's contexts read
             deps (: (go m acc)
                      (? (! (two? m)) (sort acc)
                         (: old <><m
                            (? (! old) (go >m acc)
                               (: e (peep st <<m ())
                                  pid (? (two? e) <>e ())
                                  (? (|| (! pid) (member? pid acc)) (go >m acc)
                                     (go >m (link pid acc)))))))
                      (go hunks ()))
             ptext (show (list 'patch (list 'note note) (list 'time (clock 0))
                               (link 'deps deps) (link 'hunks hunks)))
             pid (sha256 ptext)
             _ (rf-lay (".reef/patch/" + pid) ptext)
             _ (rf-lay ".reef/snap" (show (rf-snap+ snap hunks pid ())))
             tips (filter (\ t (! (member? t deps))) (rf-tips ()))
             _ (rf-lay ".reef/tips" (show (sort (link pid tips))))
             _ (puts ("reef: recorded " + (snip pid 0 12) + " (" + (show (tally hunks)) + " paths)\n"))
             pid)))

   ; --- snap under a patch's hunks (both path-sorted) ---
   (rf-snap+ sn hs pid acc)
    (? (! (two? hs)) ((rev acc) + sn)
       (! (two? sn)) (: h <hs nw <>>h
                        (rf-snap+ () >hs pid (? (! nw) acc (link `((<h) nw pid) acc))))
       (: h <hs e <sn
          (? ((<h) = <e) (: nw <>>h
                            (rf-snap+ >sn >hs pid (? (! nw) acc (link `((<h) nw pid) acc))))
             ((<h) < <e) (: nw <>>h
                            (rf-snap+ sn >hs pid (? (! nw) acc (link `((<h) nw pid) acc))))
             (rf-snap+ >sn hs pid (link <sn acc)))))

   ; --- log: the patches newest first; * marks a tip ---
   (rf-ins e l) (? (! (two? l)) `(e)                             ; insert desc by time (seat 0)
                   ((<<l) <= <e) (link e l)
                   (link <l (rf-ins e >l)))
   (reef-log _)
    (: tips (rf-tips ())
       (go m acc)
        (? (! (two? m)) acc
           (: id <m f (rf-patch id)
              (? (! (two? f)) (go >m acc)
                 (go >m (rf-ins (list (cap (rf-fld f 'time)) id
                                      (cap (rf-fld f 'note))
                                      (tally (rf-fld f 'hunks)))
                                acc)))))
       es (go (readdir ".reef/patch") ())
       (pr m)
        (? (! (two? m)) ()
           (: e <m id <>e
              _ (puts (? (member? id tips) "* " "  "))
              _ (puts ((snip id 0 12) + "  " + (show <>>>e) + " paths"))
              nt <>>e
              _ (? (! nt) () (puts ("  " + nt)))
              _ (puts "\n")
              (pr >m)))
       _ (pr es)
       (tally es))

   ; --- diff: working tree vs the recorded state, unified; answers how many differ ---
   (reef-diff _)
    (: snap (rf-snap ())
       hunks (rf-scan (sort (rf-walk () (rf-igs ()) ())) snap ())
       (pr m n)
        (? (! (two? m)) n
           (: h <m p <h
              ot (? (! <>h) "" (rf-blob <>h))
              nt (? (! <>>h) "" (: t (rf-look p) (? (two? t) <>t "")))
              d (udiff (? (! <>h) "(none)" (p + " (recorded)"))
                       (? (! <>>h) "(gone)" p)
                       ot nt)
              _ (? (! d) () (puts d))
              (pr >m (n + 1))))
       (pr hunks 0))

   ; --- sync: union a peer nest's store. blobs + patches are immutable and
   ;     content-addressed, so the union is just "copy what's missing"; then
   ;     re-derive tips + snap from the WHOLE patch set (order-free -- the DAG is
   ;     a pure function of its patches) and materialize the merged snap onto a
   ;     clean working tree. peer = a directory holding a .reef/ (a peer or a
   ;     cutting); same-path divergence warns and the topo-latest write wins ---
   (rf-dirty _) (tally (rf-scan (sort (rf-walk () (rf-igs ()) ())) (rf-snap ()) ()))
   (rf-sync-dir sub peer)                                        ; copy missing store files, verify the address
    (: pd (peer + "/.reef/" + sub + "/") ld (".reef/" + sub + "/")
       (go m n)
        (? (! (two? m)) n
           (: h <m
              (? (! (64 = (tally h))) (go >m n)                  ; only the sha256-named files
                 (rf-look (ld + h)) (go >m n)                    ; already have it
                 (: t (rf-look (pd + h))
                    (? (! (two? t)) (go >m n)
                       ((sha256 <>t) = h) (: _ (rf-lay (ld + h) <>t) (go >m (n + 1)))
                       (: _ (say err ("reef: bad address " + h + "\n")) (go >m n)))))))
       (go (readdir pd) 0))
   (rf-derive-tips ids)                                          ; the ids nobody depends on
    (: dep (tablet 0)
       _ (: (go m) (? (! (two? m)) ()
               (: f (rf-patch <m)
                  _ (: (gd d) (? (two? d) (: _ (pin dep <d 1) (gd >d)) ()) (gd (rf-fld f 'deps)))
                  (go >m)))
            (go ids))
       (sort (filter (\ i (! (peep dep i 0))) ids)))
   (rf-topo ids)                                                 ; deps before dependents, ties by sorted id
    (: have (tablet 0) idl (sort ids)
       (ready _)
        (: (go m a) (? (! (two? m)) (rev a)
             (: id <m
                (? (peep have id 0) (go >m a)
                   (: f (rf-patch id)
                      ok (: (gd d) (? (! (two? d)) 1 (? (peep have <d 0) (gd >d) 0)) (gd (rf-fld f 'deps)))
                      (go >m (? ok (link id a) a))))))
           (go idl ()))
       (pass acc)
        (: b (ready ())
           (? (! (two? b)) acc
              (: _ (: (mk m) (? (two? m) (: _ (pin have <m 1) (mk >m)) ()) (mk b))
                 (pass (acc + b)))))
       (pass ()))
   ; --- a diverged write: two patches wrote one path from a shared base. the
   ;     base is FREE -- the incoming hunk names the content hash it expected --
   ;     so the three-way merge has all three sides in the store and most
   ;     divergence resolves itself (crew/reef/merge.l). answers
   ;     (chash nconflicts note): nconflicts 0 is a clean merge, and the note is
   ;     what the report tells. a side that is ABSENT (a delete meeting an edit)
   ;     or BINARY cannot line-merge: keep the CONTENT, never the deletion, and
   ;     call it a conflict -- the loser stays addressable in the store ---
   (rf-bin? s) (: n (tally s) (go i) (? (n <= i) 0 ((s i) = 0) 1 (go (i + 1))) (go 0))
   (rf-sh h) (? h (snip h 0 12) "()")
   (rf-bl h) (? (! h) `(1 "") (rf-look (".reef/blob/" + h)))     ; (1 text) | () -- presence, not net
   (rf-merge old curh nw curpid pid)
    (: bw (rf-bl old) ow (rf-bl curh) tw (rf-bl nw)
       (? (! nw) `(curh 1 ("kept the edit -- " + (rf-sh pid) + " deletes what " + (rf-sh curpid) + " wrote"))
          (! curh) `(nw 1 ("kept the edit -- " + (rf-sh pid) + " writes what was deleted here"))
          (! (&& (two? ow) (two? tw)))
             `(curh 1 ("a blob is missing, cannot merge -- kept " + (rf-sh curh)))
          (: bt (? (two? bw) <>bw "") ot <>ow tt <>tw
             (? (|| (rf-bin? bt) (|| (rf-bin? ot) (rf-bin? tt)))
                `(curh 1 ("binary, cannot merge -- kept " + (rf-sh curh) + ", incoming " + (rf-sh nw)))
                (: r (rmerge bt ot tt ("recorded (" + (rf-sh curpid) + ")")
                                      ("incoming (" + (rf-sh pid) + ")"))
                   mh (rf-keep <r) n (<>r)
                   (? (0 = n) `(mh 0 "both sides kept")
                      `(mh n ((show n) + " region" + (? (1 = n) "" "s") + " left in markers"))))))))
   (rf-derive-snap ids)                                          ; fold the DAG -> (snap conflicts)
    (: order (rf-topo ids) snap (tablet 0) cf (tablet 0)
       _ (: (go m) (? (! (two? m)) ()
               (: pid <m f (rf-patch pid)
                  _ (: (h1 hs) (? (! (two? hs)) ()
                          (: h <hs p <h old <>h nw <>>h
                             cur (peep snap p ()) curh (? (two? cur) <cur ())
                             curpid (? (two? cur) <>cur ())
                             _ (? (|| (curh = old) (nw = curh))    ; the expected base, or convergent
                                  (: _ (pull cf p 0)               ; a later patch can settle an earlier clash
                                     (? (! nw) (pull snap p 0) (pin snap p (list nw pid))))
                                  (: m (rf-merge old curh nw curpid pid)
                                     _ (pin snap p (list <m pid))
                                     _ (pin cf p (list (<>m) (<>>m)))
                                     ()))
                             (h1 >hs)))
                       (h1 (rf-fld f 'hunks)))
                  (go >m)))
            (go order))
       (: (em ps a) (? (! (two? ps)) (rev a)
            (: p <ps e (peep snap p ()) (em >ps (link (list p <e <>e) a))))
          (cm ps a) (? (! (two? ps)) (rev a)
            (: p <ps e (peep cf p ()) (cm >ps (link (list p <e <>e) a))))
          `((em (sort (keys snap)) ()) (cm (sort (keys cf)) ()))))
   (rf-mkparents p)                                              ; mkdir every "/"-prefix of a nested path
    (: (go i) (? (< i (tally p))
                 (: _ (? ((p i) = 47) (mkdir (snip p 0 i) 493) ()) (go (i + 1)))
                 ())
       (go 0))
   (rf-mater oldsnap newsnap)                                    ; make the tree match newsnap; #files touched
    (: np (tablet 0)
       _ (: (go m) (? (two? m) (: _ (pin np <<m <><m) (go >m)) ()) (go newsnap))
       w (: (go m n) (? (! (two? m)) n
              (: e <m p <e ch <>e bl (rf-look (".reef/blob/" + ch))   ; presence, not net: an empty blob is real
                 cur (rf-look p) curh (? (two? cur) (sha256 <>cur) ())
                 (? (! (two? bl)) (: _ (say err ("reef: missing blob " + ch + "\n")) (go >m n))
                    (curh = ch) (go >m n)
                    (: _ (rf-mkparents p) _ (rf-lay p <>bl) (go >m (n + 1))))))
            (go newsnap 0))
       d (: (go m n) (? (! (two? m)) n
              (: p <<m (? (peep np p 0) (go >m n) (: _ (unlink p) (go >m (n + 1))))))
            (go oldsnap 0))
       (w + d))
   (reef-sync peer)
    (: _ (rf-mk ())
       (? (0 < (rf-dirty ()))
          (: _ (say err "reef: working tree has changes; record or revert first\n") 0)
          (: oldsnap (rf-snap ())
             nb (rf-sync-dir "blob" peer) np (rf-sync-dir "patch" peer)
             ids (readdir ".reef/patch")
             _ (rf-lay ".reef/tips" (show (rf-derive-tips ids)))
             dr (rf-derive-snap ids)
             newsnap <dr cfs <>dr
             _ (rf-lay ".reef/snap" (show newsnap))
             nc (rf-mater oldsnap newsnap)
             ; the divergence report: a clean merge is news, a marker is a stop
             nleft (: (go m n) (? (! (two? m)) n
                        (: e <m p <e k <>e
                           _ (say (? (0 = k) out err)
                                  ("reef: " + (? (0 = k) "merged " "CONFLICT in ") + p
                                   + " -- " + (<>>e) + "\n"))
                           (go >m (? (0 = k) n (n + 1)))))
                      (go cfs 0))
             _ (puts ("reef: synced -- " + (show (tally ids)) + " patches (" + (show np)
                      + " new), " + (show nb) + " blobs, " + (show nc) + " files updated"
                      + (? (0 = nleft) "" (", " + (show nleft) + " conflicted")) + "\n"))
             _ (? (0 = nleft) ()
                  (say err "reef: settle the flagged paths, then record -- the resolution is a patch\n"))
             (? (0 = nleft) (tally ids) 0))))

   ; --- hatch: (source_tree, arch) -> a native seed. the local derivation
   ;     (doc/hatch.md's fallback path: rebuild from a present binary). we hatch
   ;     the RECORDED state -- a dirty tree refuses -- run the nest's build recipe,
   ;     fingerprint the output as the SEED, and ledger it keyed by (patchset,
   ;     arch). the patchset id is the hash of the sorted tips = the head DAG
   ;     state, so hatching the same head with the same recipe twice is a
   ;     reproducibility audit: a seed that doesn't match the prior hatch is a
   ;     NON-REPRODUCIBLE flag, not a silent divergence. the recipe + its output
   ;     path are opaque to hatch (it just runs an argv and hashes a file), the
   ;     way the store is opaque to what a hunk's old/new mean; for love the recipe
   ;     is `make`, for a test a one-line stub. config in .reef/hatch:
   ;       (hatch (recipe "make" "candidate") (out "out/host/love")) ---
   (rf-hatched _) (rf-form ".reef/hatched")                     ; ((psid arch seed time)..) newest-first | ()
   (rf-hatch-find led psid arch)                                ; the last hatch of this (psid,arch) | ()
    (? (! (two? led)) ()
       (: r <led (? (&& ((<r) = psid) ((<>r) = arch)) r (rf-hatch-find >led psid arch))))
   (rf-arch _) (: u (run (list "uname" "-m")) ls (lines (cup u)) (? (two? ls) <ls "unknown"))
   (reef-hatch argv)
    (: _ (rf-mk ())
       (? (0 < (rf-dirty ()))
          (: _ (say err "reef: working tree has changes; record or revert before hatching\n") ())
          (: tips (rf-tips ())
             (? (! (two? tips)) (: _ (say err "reef: nothing recorded to hatch\n") ())
                (: cfg (rf-form ".reef/hatch")
                   rc (? (two? argv) argv (rf-fld cfg 'recipe))
                   (? (! (two? rc)) (: _ (say err "reef: no build recipe (pass one, or set .reef/hatch)\n") ())
                      (: psid (sha256 (show (sort tips))) arch (rf-arch ())
                         _ (puts ("reef: hatching " + (snip psid 0 12) + " (arch " + arch + ") -- " + (show rc) + "\n"))
                         st (cap (run rc))
                         (? (! (0 = st)) (: _ (say err ("reef: build failed (status " + (show st) + ")\n")) ())
                            (: outp (: o (rf-fld cfg 'out) (? (two? o) <o ()))
                               seed (? (! outp) () (: t (rf-look outp) (? (two? t) (sha256 <>t) ())))
                               prior (rf-hatch-find (rf-hatched ()) psid arch)
                               _ (? (&& (two? prior) (&& seed (<>>prior)))
                                    (? ((<>>prior) = seed) (puts "reef: reproducible -- the seed matches the prior hatch\n")
                                       (say err "reef: NON-REPRODUCIBLE -- this patch set hatched to a different seed!\n"))
                                    ())
                               _ (rf-lay ".reef/hatched" (show (link (list psid arch seed (clock 0)) (rf-hatched ()))))
                               _ (puts ("reef: hatched " + (snip psid 0 12) + " -> seed "
                                        + (? seed (snip seed 0 12) "(none)") + "\n"))
                               psid))))))))))

; the SEAT: fire reef-main iff reef sits in the program seat of the real command
; line -- the positional program or a -l preload -- taking the args after the
; seat. the cc.l shape; catted after the engine, quiet under any other name.
(: (bn p) (: n (tally p)
             (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p r n))
             (go 0 0))
   (me? w) (: b (bn w) (|| (b = "reef") (b = "reef.l")))
   (seat a) (? (! (two? a)) ()
               (<a = "-l") (? (two? >a)
                              (? (me? <>a) (link 1 >>a) (seat >>a))
                              ())
               (me? <a) (link 1 >a)
               ())
   (usage _) (: _ (say err "reef -- the patch-set vcs (doc/reef.md)
usage: reef VERB ..
  record [NOTE]   working changes -> a patch in the store (.reef/)
  sync PEER       union a peer nest's store (a dir holding .reef/), then
                  re-derive tips + snap and materialize onto a clean tree.
                  two patches on one path three-way merge; a true overlap
                  lands in markers and exits 1 -- resolve, then record
  hatch [CMD..]   build the recorded source into a native seed (the recipe is
                  CMD.. or .reef/hatch), fingerprint + ledger it by patch set
  log             the patches, newest first (* marks a tip)
  diff            working tree vs the recorded state (unified; exit 1 on change)
")
                (quit 2))
   (reef-main as)
    (? (! (two? as)) (usage ())
       (: v <as
          (? (v = "record") (: _ (reef-record (? (two? >as) <>as "")) (quit 0))
             (v = "sync") (? (two? >as) (: r (reef-sync <>as) (quit (? r 0 1))) (usage ()))
             (v = "hatch") (: r (reef-hatch >as) (quit (? r 0 1)))
             (v = "log") (: _ (reef-log ()) (quit 0))
             (v = "diff") (: n (reef-diff ()) (quit (? (0 < n) 1 0)))
             (usage ()))))
   s (seat >cmdline)
   (? (two? s) (reef-main >s) ()))
