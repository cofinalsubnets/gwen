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
   (look p) (: q (open p "r") (? q (: s (slurp q) _ (close q) `(1 (s + ""))) ()))  ; (1 text) | ()
   (lay p s) (: q (open p "w") (? q (: _ (say q s) _ (close q) 1) 0))
   (form p) (: q (open p "r")                                 ; the file's one form | ()
                  (? q (: r (sound q ()) _ (close q) (? (two? r) r ())) ()))

   ; --- the store ---
   (mk _) (: _ (mkdir ".reef" 493) _ (mkdir ".reef/blob" 493) _ (mkdir ".reef/patch" 493) ())
   (snap _) (form ".reef/snap")                            ; ((path chash pid) ..) sorted
   (tips _) (form ".reef/tips")                            ; (id ..)
   (blob h) (: t (look (".reef/blob/" + h)) (? (two? t) <>t ()))
   (keep s) (: h (sha256 s) _ (lay (".reef/blob/" + h) s) h)
   (patch id) (form (".reef/patch/" + id))
   (fld f k) (: (go m) (? (! (two? m)) ()                     ; a patch field's tail
                             (&& (two? <m) (id? <<m k)) ><m
                             (go >m))
                   (go >f))

   ; --- ignores: path prefixes; a line in .reef/ignore adds to the built-ins ---
   igs0 '(".reef" ".git" "out")
   (igs _) (: t (look ".reef/ignore")
                 (? (two? t) (igs0 + (filter (\ l (0 < (tally l))) (lines <>t)))
                    igs0))
   (ign? p igs) (? (! (two? igs)) 0
                      (: g <igs n (tally g)
                         (? (|| (p = g) (&& ((n + 1) < (tally p)) ((snip p 0 (n + 1)) = (g + "/"))))
                            1 (ign? p >igs))))

   ; --- the walk: every regular file under the root, paths relative ---
   (walk dir igs acc)
    (: es (readdir (? (! dir) "." dir))
       (go m acc)
        (? (! (two? m)) acc
           (: p (? (! dir) <m (dir + "/" + <m))
              (? (ign? p igs) (go >m acc)
                 (: st (stat p)
                    md (? (two? st) (& 61440 <>>st) 0)
                    (? (md = 16384) (go >m (walk p igs acc))
                       (md = 32768) (go >m (link p acc))
                       (go >m acc))))))
       (go es acc))

   ; --- the scan: working tree vs snap -> the hunks, path-sorted. PURE (no
   ;     blob writes); a hunk is (path old new), old/new content hashes | () ---
   (h1 p e)                                                   ; one path vs its snap entry|()
    (: t (look p)
       (? (! (two? t)) (? (two? e) `(p (<>e) ()) ())             ; unreadable: dead if known
          (: h (sha256 <>t)
             (? (! (two? e)) `(p () h)                           ; born
                ((<>e) = h) ()                                   ; unchanged
                `(p (<>e) h)))))                                 ; changed
   (scan ps sn acc)                                           ; ps sorted paths, sn the snap
    (? (! (two? ps))
       (? (! (two? sn)) (rev acc)
          (scan ps >sn (link `((<<sn) (<><sn) ()) acc)))      ; the tail of snap: dead
       (! (two? sn))
       (: h (h1 <ps ()) (scan >ps sn (? (two? h) (link h acc) acc)))
       (: p <ps e <sn
          (? (p = <e) (: h (h1 p e) (scan >ps >sn (? (two? h) (link h acc) acc)))
             (p < <e) (: h (h1 p ()) (scan >ps sn (? (two? h) (link h acc) acc)))
             (scan ps >sn (link `((<e) (<>e) ()) acc)))))     ; snap-only: dead

   ; --- record ---
   (reef-record note)
    (: _ (mk ())
       sn (snap ())
       st (tablet 0)                                             ; path -> (chash pid)
       _ (: (go m) (? (two? m) (: _ (pin st <<m ><m) (go >m)) ()) (go sn))
       hunks (scan (sort (walk () (igs ()) ())) sn ())
       (? (! (two? hunks)) (: _ (puts "reef: nothing to record\n") ())
          (: ; the blobs: keep every hunk's new content
             _ (: (go m) (? (two? m)
                            (: h <m _ (? (! <>>h) () (keep <>(look <h))) (go >m))
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
             _ (lay (".reef/patch/" + pid) ptext)
             _ (lay ".reef/snap" (show (snap+ sn hunks pid ())))
             tp (filter (\ t (! (member? t deps))) (tips ()))
             _ (lay ".reef/tips" (show (sort (link pid tp))))
             _ (puts ("reef: recorded " + (snip pid 0 12) + " (" + (show (tally hunks)) + " paths)\n"))
             pid)))

   ; --- snap under a patch's hunks (both path-sorted) ---
   (snap+ sn hs pid acc)
    (? (! (two? hs)) ((rev acc) + sn)
       (! (two? sn)) (: h <hs nw <>>h
                        (snap+ () >hs pid (? (! nw) acc (link `((<h) nw pid) acc))))
       (: h <hs e <sn
          (? ((<h) = <e) (: nw <>>h
                            (snap+ >sn >hs pid (? (! nw) acc (link `((<h) nw pid) acc))))
             ((<h) < <e) (: nw <>>h
                            (snap+ sn >hs pid (? (! nw) acc (link `((<h) nw pid) acc))))
             (snap+ >sn hs pid (link <sn acc)))))

   ; --- log: the patches newest first; * marks a tip ---
   (ins e l) (? (! (two? l)) `(e)                             ; insert desc by time (seat 0)
                   ((<<l) <= <e) (link e l)
                   (link <l (ins e >l)))
   (reef-log _)
    (: tp (tips ())
       (go m acc)
        (? (! (two? m)) acc
           (: id <m f (patch id)
              (? (! (two? f)) (go >m acc)
                 (go >m (ins (list (cap (fld f 'time)) id
                                      (cap (fld f 'note))
                                      (tally (fld f 'hunks)))
                                acc)))))
       es (go (readdir ".reef/patch") ())
       (pr m)
        (? (! (two? m)) ()
           (: e <m id <>e
              _ (puts (? (member? id tp) "* " "  "))
              _ (puts ((snip id 0 12) + "  " + (show <>>>e) + " paths"))
              nt <>>e
              _ (? (! nt) () (puts ("  " + nt)))
              _ (puts "\n")
              (pr >m)))
       _ (pr es)
       (tally es))

   ; --- diff: working tree vs the recorded state, unified; answers how many differ ---
   (reef-diff _)
    (: sn (snap ())
       hunks (scan (sort (walk () (igs ()) ())) sn ())
       (pr m n)
        (? (! (two? m)) n
           (: h <m p <h
              ot (? (! <>h) "" (blob <>h))
              nt (? (! <>>h) "" (: t (look p) (? (two? t) <>t "")))
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
   (dirty _) (tally (scan (sort (walk () (igs ()) ())) (snap ()) ()))
   ; one direction of the store exchange: copy the sha256-named files `to` lacks,
   ; verifying every address on the way. a nest root is "." or a peer path.
   ; answers (ncopied nfailed) -- a failed write is a peer we cannot push to.
   (copy sub from to)
    (: fd (from + "/.reef/" + sub + "/") td (to + "/.reef/" + sub + "/")
       (go m n bad)
        (? (! (two? m)) `(n bad)
           (: h <m
              (? (! (64 = (tally h))) (go >m n bad)              ; only the sha256-named files
                 (two? (look (td + h))) (go >m n bad)         ; already there
                 (: t (look (fd + h))
                    (? (! (two? t)) (go >m n bad)
                       (! ((sha256 <>t) = h))
                          (: _ (say err ("reef: bad address " + h + "\n")) (go >m n (bad + 1)))
                       (lay (td + h) <>t) (go >m (n + 1) bad)
                       (go >m n (bad + 1)))))))
       (go (readdir fd) 0 0))
   ; bring THIS nest to the state its store implies: re-derive tips + snap from
   ; the whole patch set and materialize. cwd-relative, so it settles whichever
   ; nest we stand in -- both ends of a sync run it. answers (npatches nfiles conflicts)
   (settle _)
    (: oldsnap (snap ())
       ids (readdir ".reef/patch")
       _ (lay ".reef/tips" (show (derive-tips ids)))
       dr (derive-snap ids)
       newsnap <dr
       ; the derive replays the WHOLE set every time, so a merge settled long ago
       ; recomputes and would be announced forever: a clean merge is news only
       ; when it moved the file THIS time. an unresolved conflict always is.
       ix (: (go m t) (? (two? m) (: _ (pin t <<m <><m) (go >m t)) t))
       op (ix oldsnap (tablet 0)) np (ix newsnap (tablet 0))
       cfs (filter (\ e (|| (0 < (<>e)) (! ((peep op <e ()) = (peep np <e ()))))) (<>dr))
       _ (lay ".reef/snap" (show newsnap))
       nf (mater oldsnap newsnap)
       `((tally ids) nf cfs))
   (derive-tips ids)                                          ; the ids nobody depends on
    (: dep (tablet 0)
       _ (: (go m) (? (! (two? m)) ()
               (: f (patch <m)
                  _ (: (gd d) (? (two? d) (: _ (pin dep <d 1) (gd >d)) ()) (gd (fld f 'deps)))
                  (go >m)))
            (go ids))
       (sort (filter (\ i (! (peep dep i 0))) ids)))
   (topo ids)                                                 ; deps before dependents, ties by sorted id
    (: have (tablet 0) idl (sort ids)
       (ready _)
        (: (go m a) (? (! (two? m)) (rev a)
             (: id <m
                (? (peep have id 0) (go >m a)
                   (: f (patch id)
                      ok (: (gd d) (? (! (two? d)) 1 (? (peep have <d 0) (gd >d) 0)) (gd (fld f 'deps)))
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
   (bin? s) (: n (tally s) (go i) (? (n <= i) 0 ((s i) = 0) 1 (go (i + 1))) (go 0))
   (sh h) (? h (snip h 0 12) "()")
   (bl h) (? (! h) `(1 "") (look (".reef/blob/" + h)))     ; (1 text) | () -- presence, not net
   (mend old curh nw curpid pid)
    (: bw (bl old) ow (bl curh) tw (bl nw)
       (? (! nw) `(curh 1 ("kept the edit -- " + (sh pid) + " deletes what " + (sh curpid) + " wrote"))
          (! curh) `(nw 1 ("kept the edit -- " + (sh pid) + " writes what was deleted here"))
          (! (&& (two? ow) (two? tw)))
             `(curh 1 ("a blob is missing, cannot merge -- kept " + (sh curh)))
          (: bt (? (two? bw) <>bw "") ot <>ow tt <>tw
             (? (|| (bin? bt) (|| (bin? ot) (bin? tt)))
                `(curh 1 ("binary, cannot merge -- kept " + (sh curh) + ", incoming " + (sh nw)))
                (: r (diff3 bt ot tt ("recorded (" + (sh curpid) + ")")
                                      ("incoming (" + (sh pid) + ")"))
                   mh (keep <r) n (<>r)
                   (? (0 = n) `(mh 0 "both sides kept")
                      `(mh n ((show n) + " region" + (? (1 = n) "" "s") + " left in markers"))))))))
   (derive-snap ids)                                          ; fold the DAG -> (snap conflicts)
    (: order (topo ids) snap (tablet 0) cf (tablet 0)
       _ (: (go m) (? (! (two? m)) ()
               (: pid <m f (patch pid)
                  _ (: (h1 hs) (? (! (two? hs)) ()
                          (: h <hs p <h old <>h nw <>>h
                             cur (peep snap p ()) curh (? (two? cur) <cur ())
                             curpid (? (two? cur) <>cur ())
                             _ (? (|| (curh = old) (nw = curh))    ; the expected base, or convergent
                                  (: _ (pull cf p 0)               ; a later patch can settle an earlier clash
                                     (? (! nw) (pull snap p 0) (pin snap p (list nw pid))))
                                  (: m (mend old curh nw curpid pid)
                                     _ (pin snap p (list <m pid))
                                     _ (pin cf p (list (<>m) (<>>m)))
                                     ()))
                             (h1 >hs)))
                       (h1 (fld f 'hunks)))
                  (go >m)))
            (go order))
       (: (em ps a) (? (! (two? ps)) (rev a)
            (: p <ps e (peep snap p ()) (em >ps (link (list p <e <>e) a))))
          (cm ps a) (? (! (two? ps)) (rev a)
            (: p <ps e (peep cf p ()) (cm >ps (link (list p <e <>e) a))))
          `((em (sort (keys snap)) ()) (cm (sort (keys cf)) ()))))
   (mkparents p)                                              ; mkdir every "/"-prefix of a nested path
    (: (go i) (? (< i (tally p))
                 (: _ (? ((p i) = 47) (mkdir (snip p 0 i) 493) ()) (go (i + 1)))
                 ())
       (go 0))
   (mater oldsnap newsnap)                                    ; make the tree match newsnap; #files touched
    (: np (tablet 0)
       _ (: (go m) (? (two? m) (: _ (pin np <<m <><m) (go >m)) ()) (go newsnap))
       w (: (go m n) (? (! (two? m)) n
              (: e <m p <e ch <>e bl (look (".reef/blob/" + ch))   ; presence, not net: an empty blob is real
                 cur (look p) curh (? (two? cur) (sha256 <>cur) ())
                 (? (! (two? bl)) (: _ (say err ("reef: missing blob " + ch + "\n")) (go >m n))
                    (curh = ch) (go >m n)
                    (: _ (mkparents p) _ (lay p <>bl) (go >m (n + 1))))))
            (go newsnap 0))
       d (: (go m n) (? (! (two? m)) n
              (: p <<m (? (peep np p 0) (go >m n) (: _ (unlink p) (go >m (n + 1))))))
            (go oldsnap 0))
       (w + d))
   ; sync is the EXCHANGE, and an exchange has two ends: pull what the peer has
   ; that we lack, push what we have that it lacks, then settle BOTH nests. the
   ; derive is a pure function of the patch set, so two nests over one union land
   ; on the same snap -- after a sync the trees are identical, which is the whole
   ; "distribution == cloning" claim made good. the peer's half needs its tree
   ; CLEAN and writable; when it is not we still pull (always safe) and say so,
   ; leaving the peer's store untouched rather than half-fed.
   (reef-sync peer)
    (: _ (mk ())
       (? (0 < (dirty ()))
          (: _ (say err "reef: working tree has changes; record or revert first\n") 0)
          (: home (cwd 0)
             ; chdir answers () on success and -errno on failure -- BOTH nil, and
             ; () even sorts BELOW 0, so neither `!` nor `< 0` tells them apart.
             ; the failure is a CHARM; the success is the zero point.
             pd (: c (chdir peer)
                   (? (charm? c) -1
                      (: _ (mk ()) d (dirty ()) _ (chdir home) d)))
             (? (pd < 0) (: _ (say err ("reef: cannot reach " + peer + "\n")) 0)
                (: inb (copy "blob" peer ".") inp (copy "patch" peer ".")
                   ; the push: only onto a clean peer, and only if every write lands
                   out1 (? (0 = pd) (copy "blob" "." peer) `(0 0))
                   out2 (? (0 = pd) (copy "patch" "." peer) `(0 0))
                   pushed (&& (0 = pd) (&& (0 = (<>out1)) (0 = (<>out2))))
                   st (settle ())
                   ; the divergence report: a clean merge is news, a marker is a stop
                   nleft (: (go m n) (? (! (two? m)) n
                              (: e <m p <e k <>e
                                 _ (say (? (0 = k) out err)
                                        ("reef: " + (? (0 = k) "merged " "CONFLICT in ") + p
                                         + " -- " + (<>>e) + "\n"))
                                 (go >m (? (0 = k) n (n + 1)))))
                            (go (<>>st) 0))
                   pst (? (! pushed) ()                          ; settle the far end too
                          (: c (chdir peer) r (settle ()) _ (chdir home) r))
                   _ (puts ("reef: synced -- " + (show <st) + " patches ("
                            + (show <inp) + " in, " + (show <out2) + " out), "
                            + (show (<inb + <out1)) + " blobs, " + (show (<>st)) + " files here"
                            + (? (two? pst) (", " + (show (<>pst)) + " files there") "")
                            + (? (0 = nleft) "" (", " + (show nleft) + " conflicted")) + "\n"))
                   _ (? pushed ()
                        (say err ("reef: pulled only -- the peer "
                                  + (? (0 < pd) "has unrecorded changes" "would not take the write")
                                  + "; run sync from its side to finish\n")))
                   _ (? (0 = nleft) ()
                        (say err "reef: settle the flagged paths, then record -- the resolution is a patch\n"))
                   (? (&& pushed (0 = nleft)) <st 0))))))

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
   (hatched _) (form ".reef/hatched")                     ; ((psid arch seed time)..) newest-first | ()
   (hatch-find led psid arch)                                ; the last hatch of this (psid,arch) | ()
    (? (! (two? led)) ()
       (: r <led (? (&& ((<r) = psid) ((<>r) = arch)) r (hatch-find >led psid arch))))
   (arch _) (: u (run (list "uname" "-m")) ls (lines (cup u)) (? (two? ls) <ls "unknown"))
   (reef-hatch argv)
    (: _ (mk ())
       (? (0 < (dirty ()))
          (: _ (say err "reef: working tree has changes; record or revert before hatching\n") ())
          (: tp (tips ())
             (? (! (two? tp)) (: _ (say err "reef: nothing recorded to hatch\n") ())
                (: cfg (form ".reef/hatch")
                   rc (? (two? argv) argv (fld cfg 'recipe))
                   (? (! (two? rc)) (: _ (say err "reef: no build recipe (pass one, or set .reef/hatch)\n") ())
                      (: psid (sha256 (show (sort tp))) ar (arch ())
                         _ (puts ("reef: hatching " + (snip psid 0 12) + " (arch " + ar + ") -- " + (show rc) + "\n"))
                         st (cap (run rc))
                         (? (! (0 = st)) (: _ (say err ("reef: build failed (status " + (show st) + ")\n")) ())
                            (: outp (: o (fld cfg 'out) (? (two? o) <o ()))
                               seed (? (! outp) () (: t (look outp) (? (two? t) (sha256 <>t) ())))
                               prior (hatch-find (hatched ()) psid ar)
                               _ (? (&& (two? prior) (&& seed (<>>prior)))
                                    (? ((<>>prior) = seed) (puts "reef: reproducible -- the seed matches the prior hatch\n")
                                       (say err "reef: NON-REPRODUCIBLE -- this patch set hatched to a different seed!\n"))
                                    ())
                               _ (lay ".reef/hatched" (show (link (list psid ar seed (clock 0)) (hatched ()))))
                               _ (puts ("reef: hatched " + (snip psid 0 12) + " -> seed "
                                        + (? seed (snip seed 0 12) "(none)") + "\n"))
                               psid))))))))))

; --- the CLI face: usage + the verb dispatcher. these live in the LAYER
;     (cook-main's shape), so the seat in seal.l reaches them the way any
;     consumer does -- (from 'reef 'reef-main) ---
(:
   (usage _) (: _ (say err "reef -- the patch-set vcs (doc/reef.md)
usage: reef VERB ..
  record [NOTE]   working changes -> a patch in the store (.reef/)
  sync PEER       EXCHANGE patch sets with a peer nest (a dir holding .reef/):
                  pull what it has, push what it lacks, settle BOTH trees to
                  the union. two patches on one path three-way merge; a true
                  overlap lands in markers and exits 1 -- resolve, then record
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
)

; seal the layer: every name above is now off the book, reachable only as the
; module. the seat below is ordinary top-level love again.
(leave ())

; the SEAT (cook.l's shape): fire reef-main iff this file -- or the installed
; `reef` binary -- sits in the program seat of the real command line (the
; positional program or a -l preload), taking the args AFTER the seat. cmdline
; is the full C argv, never rebound, so the read is the same in both load orders
; and the cat stays quiet under any other name -- which is why the gate can cat
; this file and have nothing fire. it reaches reef-main the way any consumer
; does, through (from 'reef 'reef-main). body-having: the seat leaks nothing.
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
   s (seat >cmdline)
   (? (two? s) ((from 'reef 'reef-main) >s) ()))
