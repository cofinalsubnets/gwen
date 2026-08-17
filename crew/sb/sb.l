; crew/sb/sb.l -- svalbard: the patch-set vcs (doc/sb.md the interface,
; test/patch.l + test/proof/rocq/patch.v the laws). this cut
; is the MVP's local half: `record` (working changes -> a patch in the store),
; `log`, `diff`; `sync` rides the same store next (patches + blobs are immutable
; content-addressed files, so union = copy what's missing + re-derive tips).
;
; the store (.sb/ at the working root):
;   blob/<sha256>    file content, immutable
;   patch/<sha256>   one patch, immutable -- (patch (note S) (time MS)
;                    (deps ID..) (hunks (PATH OLD NEW)..)), id = sha256 of the
;                    file's bytes; hunks path-sorted, deps sorted, so the text
;                    is deterministic and the address honest
;   tips             the maximal patches (the growth ends) -- (ID ..)
;   snap             the recorded state -- ((PATH CHASH PATCHID) ..) path-sorted
;   ignore           optional: one path prefix per line, atop (".sb" ".git" "out")
;
; a hunk is the PROVEN named-slot patch (test/patch.l's chg): slot = the path,
; context = the old content hash, value = the new -- () for absent (a birth or a
; death). patches touching disjoint paths commute (L3a), so sync's union needs
; no order; removal is a new patch whose hunk goes to () (the inverse-patch law:
; the set only grows). deps = for each touched path, the patch that wrote its
; context. finer-than-file hunks slot in later (the sequence slices are proven
; too); the store format doesn't care what a hunk's old/new mean.
;
; loads after crew/kore/text.l (lines) + crew/kore/diff.l (udiff) in the svalbard
; cat -- the floors ride module 'kore, read here through the scoped use. every
; lane rides module 'sb: the engine binds in the module, never the cat's scope;
; the SEAT form at the tail is the only thing that fires as a program.
(module 'sb
(use 'kore)
(:
   ; --- files ---
   ; presence is the (1 _) wrapper, content is always a string: an EMPTY file
   ; slurps to () -- the zero point -- and `+ ""` folds it back onto the shared
   ; identity, so a 0-byte file hashes honestly instead of to ().
   ; the port is MINDED, not closed at the tail: a torn read raises, and the close has
   ; to ride that way out too or the fd sits until a major compact.
   (look p) (: q (open p "r")                                            ; (1 text) | ()
                 (? q (mind (\ _ (: s (slurp q) `(1 (s + "")))) () (\ _ (close q))) ()))
   (lay p s) (: q (open p "w") (? q (mind (\ _ (: _ (say q s) 1)) () (\ _ (close q))) 0))
   (form p) (: q (open p "r")                                 ; the file's one form | ()
                  (? q (mind (\ _ (: r (sound (slurp q))
                                     d (? (two? r) <r ())
                                     (? (two? d) d ())))
                             () (\ _ (close q)))
                     ()))

   ; --- the store ---
   (mk _) (: _ (mkdir ".sb" 493) _ (mkdir ".sb/blob" 493) _ (mkdir ".sb/patch" 493) ())
   (snap _) (form ".sb/snap")                            ; ((path chash pid) ..) sorted
   (tips _) (form ".sb/tips")                            ; (id ..)
   ; the PATCH-SET ID: the name of a head DAG state. the tips are a SET -- sort
   ; before hashing, or the same head reads two ways.
   (psid tp) (sha256 (show (sort tp)))
   (blob h) (: t (look (".sb/blob/" + h)) (? (two? t) <>t ()))
   (keep s) (: h (sha256 s) _ (lay (".sb/blob/" + h) s) h)
   (patch id) (form (".sb/patch/" + id))
   (fld f k) (: (go m) (? !(two? m) ()                        ; a patch field's tail
                             (two? <m && id? <<m k) ><m
                             (go >m))
                   (go >f))

   ; --- ignores: path prefixes; a line in .sb/ignore adds to the built-ins ---
   igs0 '(".sb" ".git" "out")
   (igs _) (: t (look ".sb/ignore")
                 (? (two? t) (igs0 + filter (\ l (0 < tally l)) (lines <>t))
                    igs0))
   (ign? p igs) (? !(two? igs) 0
                      (: g <igs n (tally g)
                         (? ((p = g) || (((n + 1) < tally p) && (snip p 0 (n + 1) = (g + "/"))))
                            1 (ign? p >igs))))

   ; --- the walk: every regular file under the root, paths relative ---
   (walk dir igs acc)
    (: es (readdir (? !dir "." dir))
       (go m acc)
        (? !(two? m) acc
           (: p (? !dir <m (dir + "/" + <m))
              (? (ign? p igs) (go >m acc)
                 (: st (stat p)
                    md (? (two? st) (& 61440 <>>st) 0)
                    (? (md = 16384) (go >m (walk p igs acc))
                       (md = 32768) (go >m (p >< acc))
                       (go >m acc))))))
       (go es acc))

   ; --- the scan: working tree vs snap -> the hunks, path-sorted. PURE (no
   ;     blob writes); a hunk is (path old new), old/new content hashes | () ---
   (h1 p e)                                                   ; one path vs its snap entry|()
    (: t (look p)
       (? !(two? t) (? (two? e) `(p <>e ()) ())                ; unreadable: dead if known
          (: h (sha256 <>t)
             (? !(two? e) `(p () h)                              ; born
                (<>e = h) ()                                   ; unchanged
                `(p <>e h)))))                                 ; changed
   (scan ps sn acc)                                           ; ps sorted paths, sn the snap
    (? !(two? ps)
       (? !(two? sn) (rev acc)
          (scan ps >sn (`(<<sn <><sn ()) >< acc)))        ; the tail of snap: dead
       !(two? sn)
       (: h (h1 <ps ()) (scan >ps sn (? (two? h) (h >< acc) acc)))
       (: p <ps e <sn
          (? (p = <e) (: h (h1 p e) (scan >ps >sn (? (two? h) (h >< acc) acc)))
             (p < <e) (: h (h1 p ()) (scan >ps sn (? (two? h) (h >< acc) acc)))
             (scan ps >sn (`(<e <>e ()) >< acc)))))       ; snap-only: dead

   ; --- record ---
   (sb-record note)
    (: _ (mk ())
       sn (snap ())
       st (tablet 0)                                             ; path -> (chash pid)
       _ (: (go m) (? (two? m) (: _ (pin st <<m ><m) (go >m)) ()) (go sn))
       hunks (scan (sort (walk () (igs ()) ())) sn ())
       (? !(two? hunks) (: _ (puts "sb: nothing to record\n") ())
          (: ; the blobs: keep every hunk's new content
             _ (: (go m) (? (two? m)
                            (: h <m _ (? !<>>h () (keep <>(look <h))) (go >m))
                            ())
                  (go hunks))
             ; the deps: the patches whose writes this one's contexts read
             deps (: (go m acc)
                      (? !(two? m) (sort acc)
                         (: old <><m
                            (? !old (go >m acc)
                               (: e (peep st <<m ())
                                  pid (? (two? e) <>e ())
                                  (? (!pid || member? pid acc) (go >m acc)
                                     (go >m (pid >< acc)))))))
                      (go hunks ()))
             ptext (show (list 'patch (list 'note note) (list 'time (clock 0))
                               ('deps >< deps) ('hunks >< hunks)))
             pid (sha256 ptext)
             _ (lay (".sb/patch/" + pid) ptext)
             _ (lay ".sb/snap" (show (snap+ sn hunks pid ())))
             tp (filter (\ t !(member? t deps)) (tips ()))
             _ (lay ".sb/tips" (show (sort (pid >< tp))))
             _ (puts ("sb: recorded " + snip pid 0 12 + " (" + show (tally hunks) + " paths)\n"))
             pid)))

   ; --- snap under a patch's hunks (both path-sorted) ---
   (snap+ sn hs pid acc)
    (? !(two? hs) (rev acc + sn)
       !(two? sn) (: h <hs nw <>>h
                        (snap+ () >hs pid (? !nw acc (`(<h nw pid) >< acc))))
       (: h <hs e <sn
          (? (<h = <e) (: nw <>>h
                            (snap+ >sn >hs pid (? !nw acc (`(<h nw pid) >< acc))))
             (<h < <e) (: nw <>>h
                            (snap+ sn >hs pid (? !nw acc (`(<h nw pid) >< acc))))
             (snap+ >sn hs pid (<sn >< acc)))))

   ; --- log: the patches newest first; * marks a tip ---
   (ins e l) (? !(two? l) `(e)                                ; insert desc by time (seat 0)
                   (<<l <= <e) (e >< l)
                   (<l >< ins e >l))
   (sb-log _)
    (: tp (tips ())
       (go m acc)
        (? !(two? m) acc
           (: id <m f (patch id)
              (? !(two? f) (go >m acc)
                 (go >m (ins (list <(fld f 'time) id
                                      <(fld f 'note)
                                      (tally (fld f 'hunks)))
                                acc)))))
       es (go (readdir ".sb/patch") ())
       (pr m)
        (? !(two? m) ()
           (: e <m id <>e
              _ (puts (? (member? id tp) "* " "  "))
              _ (puts (snip id 0 12 + "  " + show <>>>e + " paths"))
              nt <>>e
              _ (? !nt () (puts ("  " + nt)))
              _ (puts "\n")
              (pr >m)))
       _ (pr es)
       _ (: (pf m) (? (two? m)
                      (: e <m t <>e
                         ; the psid: the name of this release's head DAG state.
                         ; a banked ref stores its tips sorted, so the sort is a no-op here
                         _ (puts ("  " + <e + "  " + sh (psid t) + "  "
                                  + show (tally t) + " tip" + (? (1 = tally t) "" "s") + "\n"))
                         (pf >m)) ())
            (: rs (refs ()) (? (two? rs) (: _ (puts "banked:\n") (pf rs)) ())))
       (tally es))

   ; --- diff: working tree vs the recorded state, unified; answers how many differ ---
   (sb-diff _)
    (: sn (snap ())
       hunks (scan (sort (walk () (igs ()) ())) sn ())
       (pr m n)
        (? !(two? m) n
           (: h <m p <h
              ot (? !<>h "" (blob <>h))
              nt (? !<>>h "" (: t (look p) (? (two? t) <>t "")))
              d (udiff (? !<>h "(none)" (p + " (recorded)"))
                       (? !<>>h "(gone)" p)
                       ot nt)
              _ (? !d () (puts d))
              (pr >m (n + 1))))
       (pr hunks 0))

   ; --- sync: union a peer nest's store. blobs + patches are immutable and
   ;     content-addressed, so the union is just "copy what's missing"; then
   ;     re-derive tips + snap from the WHOLE patch set (order-free -- the DAG is
   ;     a pure function of its patches) and materialize the merged snap onto a
   ;     clean working tree. peer = a directory holding a .sb/ (a peer or a
   ;     cutting); same-path divergence warns and the topo-latest write wins ---
   (dirty _) (tally (scan (sort (walk () (igs ()) ())) (snap ()) ()))
   ; one direction of the store exchange: copy the sha256-named files `to` lacks,
   ; verifying every address on the way. a nest root is "." or a peer path.
   ; answers (ncopied nfailed) -- a failed write is a peer we cannot push to.
   (copy sub from to)
    (: fd (from + "/.sb/" + sub + "/") td (to + "/.sb/" + sub + "/")
       (go m n bad)
        (? !(two? m) `(n bad)
           (: h <m
              (? !(64 = tally h) (go >m n bad)                   ; only the sha256-named files
                 (two? (look (td + h))) (go >m n bad)         ; already there
                 (: t (look (fd + h))
                    (? !(two? t) (go >m n bad)
                       !(sha256 <>t = h)
                          (: _ (say err ("sb: bad address " + h + "\n")) (go >m n (bad + 1)))
                       (lay (td + h) <>t) (go >m (n + 1) bad)
                       (go >m n (bad + 1)))))))
       (go (readdir fd) 0 0))
   ; bring THIS nest to the state a GIVEN patch set implies: re-derive tips +
   ; snap over those ids and materialize. cwd-relative, so it settles whichever
   ; nest we stand in -- both ends of a sync run it, over the whole store. the
   ; ids are a PARAMETER because the tree is a pure function of a patch SET, so
   ; realizing a subset is the same fold with a shorter list -- which is all
   ; `apply` is. answers (npatches nfiles conflicts)
   ; the divergence report a settle owes its caller: a clean merge is news, a
   ; marker is a stop. answers how many are UNRESOLVED, so the caller's exit code
   ; can say so -- sync and apply both settle, so both tell it the same way.
   (tell cfs)
    (: (go m n) (? !(two? m) n
          (: e <m p <e k <>e
             _ (say (? (0 = k) out err)
                    ("sb: " + (? (0 = k) "merged " "CONFLICT in ") + p + " -- " + <>>e + "\n"))
             (go >m (? (0 = k) n (n + 1)))))
       (go cfs 0))
   (settle ids)
    (: oldsnap (snap ())
       _ (lay ".sb/tips" (show (derive-tips ids)))
       dr (derive-snap ids)
       newsnap <dr
       ; the derive replays the WHOLE set every time, so a merge settled long ago
       ; recomputes and would be announced forever: a clean merge is news only
       ; when it moved the file THIS time. an unresolved conflict always is.
       ix (: (go m t) (? (two? m) (: _ (pin t <<m <><m) (go >m t)) t))
       op (ix oldsnap (tablet 0)) np (ix newsnap (tablet 0))
       cfs (filter (\ e ((0 < <>e) || !(peep op <e () = peep np <e ()))) <>dr)
       _ (lay ".sb/snap" (show newsnap))
       nf (mater oldsnap newsnap)
       `((tally ids) nf cfs))
   (derive-tips ids)                                          ; the ids nobody depends on
    (: dep (tablet 0)
       _ (: (go m) (? !(two? m) ()
               (: f (patch <m)
                  _ (: (gd d) (? (two? d) (: _ (pin dep <d 1) (gd >d)) ()) (gd (fld f 'deps)))
                  (go >m)))
            (go ids))
       (sort (filter (\ i !(peep dep i 0)) ids)))
   (topo ids)                                                 ; deps before dependents, ties by sorted id
    (: have (tablet 0) idl (sort ids)
       (ready _)
        (: (go m a) (? !(two? m) (rev a)
             (: id <m
                (? (peep have id 0) (go >m a)
                   (: f (patch id)
                      ok (: (gd d) (? !(two? d) 1 (? (peep have <d 0) (gd >d) 0)) (gd (fld f 'deps)))
                      (go >m (? ok (id >< a) a))))))
           (go idl ()))
       (pass acc)
        (: b (ready ())
           (? !(two? b) acc
              (: _ (: (mk m) (? (two? m) (: _ (pin have <m 1) (mk >m)) ()) (mk b))
                 (pass (acc + b)))))
       (pass ()))
   ; --- a diverged write: two patches wrote one path from a shared base. the
   ;     base is FREE -- the incoming hunk names the content hash it expected --
   ;     so the three-way merge has all three sides in the store and most
   ;     divergence resolves itself (crew/sb/merge.l). answers
   ;     (chash nconflicts note): nconflicts 0 is a clean merge, and the note is
   ;     what the report tells. a side that is ABSENT (a delete meeting an edit)
   ;     or BINARY cannot line-merge: keep the CONTENT, never the deletion, and
   ;     call it a conflict -- the loser stays addressable in the store ---
   (bin? s) (: n (tally s) (go i) (? (n <= i) 0 (s i = 0) 1 (go (i + 1))) (go 0))
   (sh h) (? h (snip h 0 12) "()")
   (bl h) (? !h '(1 "") (look (".sb/blob/" + h)))        ; (1 text) | () -- presence, not net
   (mend old curh nw curpid pid)
    (: bw (bl old) ow (bl curh) tw (bl nw)
       (? !nw `(curh 1 ("kept the edit -- " + sh pid + " deletes what " + sh curpid + " wrote"))
          !curh `(nw 1 ("kept the edit -- " + sh pid + " writes what was deleted here"))
          !(two? ow && two? tw)
             `(curh 1 ("a blob is missing, cannot merge -- kept " + sh curh))
          (: bt (? (two? bw) <>bw "") ot <>ow tt <>tw
             (? (bin? bt || bin? ot || bin? tt)
                `(curh 1 ("binary, cannot merge -- kept " + sh curh + ", incoming " + sh nw))
                (: r (diff3 bt ot tt ("recorded (" + sh curpid + ")")
                                      ("incoming (" + sh pid + ")"))
                   mh (keep <r) n <>r
                   (? (0 = n) `(mh 0 "both sides kept")
                      `(mh n (show n + " region" + (? (1 = n) "" "s") + " left in markers"))))))))
   (derive-snap ids)                                          ; fold the DAG -> (snap conflicts)
    (: order (topo ids) snap (tablet 0) cf (tablet 0)
       _ (: (go m) (? !(two? m) ()
               (: pid <m f (patch pid)
                  _ (: (h1 hs) (? !(two? hs) ()
                          (: h <hs p <h old <>h nw <>>h
                             cur (peep snap p ()) curh (? (two? cur) <cur ())
                             curpid (? (two? cur) <>cur ())
                             _ (? ((curh = old) || (nw = curh))    ; the expected base, or convergent
                                  (: _ (pull cf p 0)               ; a later patch can settle an earlier clash
                                     (? !nw (pull snap p 0) (pin snap p (list nw pid))))
                                  (: m (mend old curh nw curpid pid)
                                     _ (pin snap p (list <m pid))
                                     _ (pin cf p (list <>m <>>m))
                                     ()))
                             (h1 >hs)))
                       (h1 (fld f 'hunks)))
                  (go >m)))
            (go order))
       (: (em ps a) (? !(two? ps) (rev a)
            (: p <ps e (peep snap p ()) (em >ps (list p <e <>e >< a))))
          (cm ps a) (? !(two? ps) (rev a)
            (: p <ps e (peep cf p ()) (cm >ps (list p <e <>e >< a))))
          `((em (sort (keys snap)) ()) (cm (sort (keys cf)) ()))))
   (mkparents p)                                              ; mkdir every "/"-prefix of a nested path
    (: (go i) (? (< i (tally p))
                 (: _ (? (p i = 47) (mkdir (snip p 0 i) 493) ()) (go (i + 1)))
                 ())
       (go 0))
   (mater oldsnap newsnap)                                    ; make the tree match newsnap; #files touched
    (: np (tablet 0)
       _ (: (go m) (? (two? m) (: _ (pin np <<m <><m) (go >m)) ()) (go newsnap))
       w (: (go m n) (? !(two? m) n
              (: e <m p <e ch <>e bl (look (".sb/blob/" + ch))   ; presence, not net: an empty blob is real
                 cur (look p) curh (? (two? cur) (sha256 <>cur) ())
                 (? !(two? bl) (: _ (say err ("sb: missing blob " + ch + "\n")) (go >m n))
                    (curh = ch) (go >m n)
                    (: _ (mkparents p) _ (lay p <>bl) (go >m (n + 1))))))
            (go newsnap 0))
       d (: (go m n) (? !(two? m) n
              (: p <<m (? (peep np p 0) (go >m n) (: _ (unlink p) (go >m (n + 1))))))
            (go oldsnap 0))
       (w + d))
   ; --- the http remote: PULL ONLY. the store is content-addressed and
   ;     immutable, so a remote is a plain static file tree and every fetch is a
   ;     cacheable GET on a name we already know, sha256-verified on arrival.
   ;
   ;     there is no listing endpoint and none is needed: A PATCH IS
   ;     SELF-DESCRIBING. start at /tips, and each patch names its `deps` (the
   ;     DAG edges) and its `hunks` (the blob hashes), so the whole reachable
   ;     store is discovered by walking what we have already read. that falls out
   ;     of the format rather than being added to it, and it means the far side
   ;     runs NOTHING -- any static host is a complete remote.
   ;
   ;     push would need PUT or an endpoint, so a URL peer is read-only. that is
   ;     not a new case: sync already degrades to pull-only and says so when a
   ;     peer will not take a write ---
   (pull-http base)
    (: n (tablet 0)
       seen (tablet 0)
       (bump k) (pin n k (1 + peep n k 0))
       (getblob h)                                     ; ensure a blob is local
        (? !h 1
           (two? (look (".sb/blob/" + h))) 1
           (: b (http-fetch base ("/blob/" + h))
              (? !(two? b) (: _ (say err ("sb: cannot fetch blob " + sh h + "\n")) 0)
                 (sha256 <>b = h) (: _ (lay (".sb/blob/" + h) <>b) _ (bump 'b) 1)
                 (: _ (say err ("sb: bad address on blob " + sh h + "\n")) 0))))
       (getpatch id)                                   ; ensure a patch is local; answer its form
        (: local (look (".sb/patch/" + id))
           (? (two? local) (readform <>local)
              (: b (http-fetch base ("/patch/" + id))
                 (? !(two? b) (: _ (say err ("sb: cannot fetch patch " + sh id + "\n")) ())
                    (sha256 <>b = id) (: _ (lay (".sb/patch/" + id) <>b) _ (bump 'p)
                                              (readform <>b))
                    (: _ (say err ("sb: bad address on patch " + sh id + "\n")) ())))))
       (walk ids ok)                                   ; the transitive closure, by reading as we go
        (? !(two? ids) ok
           (: id <ids
              (? (peep seen id 0) (walk >ids ok)
                 (: _ (pin seen id 1)
                    f (getpatch id)
                    (? !(two? f) (walk >ids 0)
                       (: hs (fld f 'hunks)
                          ; ⚠ FETCH FIRST, THEN COMBINE. `&&` short-circuits, so
                          ; folding the calls into the accumulator would stop
                          ; fetching the moment one object failed -- a partial
                          ; pull that then blames every later object for missing.
                          bo (: (hb m a) (? !(two? m) a
                                   (: h <m x (getblob <>h) y (getblob <>>h)
                                      (hb >m (a && x && y))))
                                (hb hs 1))
                          (walk (fld f 'deps + >ids) (ok && bo))))))))
       tt (http-fetch base "/tips")
       (? !(two? tt) (: _ (say err ("sb: no store at " + base + " (no /tips)\n")) ())
          (: tips (readform <>tt)
             (? !(two? tips) (: _ (say err ("sb: " + base + " has no tips\n")) ())
                (: ok (walk tips 1)
                   rt (http-fetch base "/refs")             ; the refs ride along
                   theirs (? (two? rt) (readform <>rt) ())
                   ru (refadd (refs ()) theirs 1)
                   _ (? (two? <ru) (lay ".sb/refs" (show <ru)) ())
                   `(ok (peep n 'p 0) (peep n 'b 0) <>ru))))))

   (sync-url peer)
    (: _ (mk ())
       (? (0 < dirty ())
          (: _ (say err "sb: working tree has changes; record or revert first\n") 0)
          (: base (? (snip peer (tally peer - 1) (tally peer) = "/")
                     (snip peer 0 (tally peer - 1)) peer)        ; one trailing slash, ours to add
             r (pull-http base)
             (? !(two? r) 0
                (: st (settle (readdir ".sb/patch"))
                   nleft (tell <>>st)
                   nref <>>>r
                   _ (puts ("sb: pulled -- " + show <st + " patches (" + show <>r
                            + " in), " + show <>>r + " blobs, " + show <>st + " files here"
                            + (? (0 = nleft) "" (", " + show nleft + " conflicted"))
                            + (? (0 = nref) "" (", " + show nref + " ref clash")) + "\n"))
                   _ (? <r () (say err "sb: some objects did not arrive -- the pull is INCOMPLETE\n"))
                   _ (say err "sb: pulled only -- an http remote takes no push\n")
                   (? (<r && (0 = nleft) && (0 = nref)) <st 0))))))

   ; sync is the EXCHANGE, and an exchange has two ends: pull what the peer has
   ; that we lack, push what we have that it lacks, then settle BOTH nests. the
   ; derive is a pure function of the patch set, so two nests over one union land
   ; on the same snap -- after a sync the trees are identical, which is the whole
   ; "distribution == cloning" claim made good. the peer's half needs its tree
   ; CLEAN and writable; when it is not we still pull (always safe) and say so,
   ; leaving the peer's store untouched rather than half-fed.
   (sb-sync peer)
    (? (url? peer) (sync-url peer)
    (: _ (mk ())
       (? (0 < dirty ())
          (: _ (say err "sb: working tree has changes; record or revert first\n") 0)
          (: home (cwd 0)
             ; chdir answers () on success and -errno on failure -- BOTH nil, and
             ; () even sorts BELOW 0, so neither `!` nor `< 0` tells them apart.
             ; the failure is a CHARM; the success is the zero point.
             pd (: c (chdir peer)
                   (? (charm? c) -1
                      (: _ (mk ()) d (dirty ()) _ (chdir home) d)))
             (? (pd < 0) (: _ (say err ("sb: cannot reach " + peer + "\n")) 0)
                (: inb (copy "blob" peer ".") inp (copy "patch" peer ".")
                   ; the push: only onto a clean peer, and only if every write lands
                   out1 (? (0 = pd) (copy "blob" "." peer) '(0 0))
                   out2 (? (0 = pd) (copy "patch" "." peer) '(0 0))
                   pushed ((0 = pd) && (0 = <>out1) && (0 = <>out2))
                   ; the refs travel too -- a release is only "the unit you
                   ; propagate" if the NAME reaches the other nest with it
                   theirs (form (peer + "/.sb/refs"))
                   ru (refadd (refs ()) theirs 1)
                   _ (? (two? <ru) (lay ".sb/refs" (show <ru)) ())
                   _ (? !pushed ()
                        (: pu (refadd theirs (refs ()) ())
                           (? (two? <pu) (lay (peer + "/.sb/refs") (show <pu)) ())))
                   st (settle (readdir ".sb/patch"))
                   nleft (tell <>>st)
                   nref <>ru
                   pst (? !pushed ()                             ; settle the far end too
                          (: c (chdir peer) r (settle (readdir ".sb/patch")) _ (chdir home) r))
                   _ (puts ("sb: synced -- " + show <st + " patches ("
                            + show <inp + " in, " + show <out2 + " out), "
                            + show (<inb + <out1) + " blobs, " + show <>st + " files here"
                            + (? (two? pst) (", " + show <>pst + " files there") "")
                            + (? (0 = nleft) "" (", " + show nleft + " conflicted"))
                            + (? (0 = nref) "" (", " + show nref + " ref clash"
                                                + (? (1 = nref) "" "es"))) + "\n"))
                   _ (? pushed ()
                        (say err ("sb: pulled only -- the peer "
                                  + (? (0 < pd) "has unrecorded changes" "would not take the write")
                                  + "; run sync from its side to finish\n")))
                   _ (? (0 = nleft) ()
                        (say err "sb: settle the flagged paths, then record -- the resolution is a patch\n"))
                   _ (? (0 = nref) ()
                        (say err "sb: a banked name is immutable, so each nest kept its own -- rename one, then sync\n"))
                   (? (pushed && (0 = nleft) && (0 = nref)) <st 0)))))))

   ; --- apply: realize a dep-consistent SUBSET of the store into the working
   ;     tree. this is git's checkout AND its cherry-pick, which are one act here:
   ;     the tree is a pure function of a patch SET, so "the state as of P" and
   ;     "what I have plus P" differ only in the set you name -- there is no
   ;     replay-a-diff-onto-a-foreign-state step to conflict. a patch may not
   ;     travel without the patches that wrote its context, so the DEP CLOSURE
   ;     comes along automatically: naming a subset that leaves a dep behind would
   ;     otherwise DROP it silently (topo only readies a patch whose deps are all
   ;     present). apply is a VIEW -- the store never shrinks, and the next sync
   ;     re-derives the whole union. to remove a patch for good you `undo` it,
   ;     which grows an inverse rather than forgetting ---
   (idpfx pfx)                                                   ; one patch from a prefix | ()
    (: n (tally pfx)
       hits (filter (\ i ((tally i = 64) && (snip i 0 n = pfx))) (readdir ".sb/patch"))
       (? (1 = tally hits) <hits
          !(two? hits) (: _ (say err ("sb: no ref or patch matches " + pfx + "\n")) ())
          (: _ (say err ("sb: " + show (tally hits) + " patches match " + pfx + "\n")) ())))
   (idsof a)                                                     ; a REF -> its whole tip set;
    (: r (refof a)                                               ; else one patch from a prefix
       (? (two? r) r (: i (idpfx a) (? i `(i) ()))))
   (closure ids)                                                 ; ids + every dep, transitively
    (: seen (tablet 0)
       (go m) (? !(two? m) ()
                 (: id <m
                    (? (peep seen id 0) (go >m)
                       (: _ (pin seen id 1)
                          _ (go (fld (patch id) 'deps))
                          (go >m)))))
       _ (go ids)
       (sort (keys seen)))
   (sb-apply args)
    (: _ (mk ())
       (? (0 < dirty ())
          (: _ (say err "sb: working tree has changes; record or revert first\n") 0)
          (: all (readdir ".sb/patch")
             want (: (go m acc) (? !(two? m) acc
                       (: l (idsof <m) (? (two? l) (go >m (acc + l)) ())))
                     (go args ()))
             (? (two? args && !(two? want)) 0                 ; a prefix failed to resolve; it said so
                (: ids (? (two? args) (closure want) all)
                   st (settle ids)
                   nleft (tell <>>st)
                   _ (puts ("sb: applied " + show <st + " of " + show (tally all)
                            + " patches, " + show <>st + " files updated"
                            + (? (tally ids = tally all) " (the whole store)" "")
                            + (? (0 = nleft) "" (", " + show nleft + " conflicted")) + "\n"))
                   _ (? (0 = nleft) ()
                        (say err "sb: settle the flagged paths, then record -- the resolution is a patch\n"))
                   (? (0 = nleft) (tally ids) 0))))))

   ; --- bank: freeze the current head under a NAME -- an immutable release, the
   ;     unit you propagate. git's tag.
   ;
   ;     ⚠ hatch.md asks for a SINGLE-TIP head here ("reconcile tips before
   ;     cutting a release") and that is git thinking that does not survive this
   ;     model. deps are PER PATH, so a patch only depends on what it TOUCHED --
   ;     an independent birth is never depended upon and stays maximal forever.
   ;     three tips is what ordinary parallel work looks like, not a fork to be
   ;     repaired, and the only way to collapse them is to write a patch touching
   ;     every path every tip touched, i.e. to edit files to appease the check.
   ;     so a ref freezes the TIP SET, which is the head DAG state whatever its
   ;     shape -- exactly what psid hashes, and it never
   ;     demanded one tip either.
   ;
   ;     a banked name is IMMUTABLE: re-banking at the same head is a no-op, at a
   ;     different head an error. the dep closure derives the patch set from the
   ;     tips, so `apply NAME` realizes the release and needs nothing stored but
   ;     the tips. refs TRAVEL: sync unions them by name, which is what makes a
   ;     release the unit you propagate rather than a local bookmark ---
   (refs _) (form ".sb/refs")                                  ; ((name (id..) time)..) | ()
   (refof nm) (refof-in (refs ()) nm)
   ; --- the ref exchange. a ref is a single FILE, not a content-addressed one,
   ;     so sync cannot gap-fill it the way it does blobs and patches: it has to
   ;     UNION by name. a name both nests banked to the SAME head is one entry --
   ;     idempotent, the ordinary case. a name banked to DIFFERENT heads is a
   ;     genuine human error, since a banked name is immutable and neither nest
   ;     may unilaterally repoint the other's: so each side KEEPS ITS OWN and the
   ;     clash is reported. the union is therefore per-side, not one merged set
   ;     written to both -- writing one set would silently drop a real release.
   ;     answers (merged nclash) ---
   (refadd base extra loud)                                     ; loud: only ONE side tells it
    (: (go m acc n)
        (? !(two? m) `((sort acc) n)
           (: e <m mine (refof-in base <e)
              (? !(two? mine) (go >m (e >< acc) n)               ; theirs alone: take it
                 (show mine = show <>e) (go >m acc n)          ; the same head: already ours
                 (: _ (? loud (say err ("sb: REF CLASH on " + <e
                                        + " -- banked to different heads\n")) ())
                    (go >m acc (n + 1))))))
       (go extra base 0))
   (refof-in rs nm) (: (go m) (? !(two? m) () (<<m = nm) <><m (go >m)) (go rs))

   (sb-bank args)
    (: _ (mk ())
       nm (? (two? args) <args "")
       (? !(0 < tally nm)
          (: _ (say err "sb: bank needs a name\n") 0)
          (0 < dirty ())
          (: _ (say err "sb: working tree has changes; record or revert first\n") 0)
          (: tp (sort (tips ()))
             (? !(two? tp) (: _ (say err "sb: nothing recorded to bank\n") 0)
                (: old (refof nm)
                   (? (two? old && !(show old = show tp))
                      (: _ (say err ("sb: " + nm + " is already banked at a different head"
                                     + " -- a banked name is immutable\n")) 0)
                      (two? old) (: _ (puts ("sb: " + nm + " is already banked\n")) 1)
                      (: _ (lay ".sb/refs" (show (sort (list nm tp (clock 0) >< refs ()))))
                         _ (puts ("sb: banked " + nm + " at " + show (tally tp) + " tip"
                                  + (? (1 = tally tp) "" "s") + " -- " + sh <tp + "\n"))
                         1)))))))

   ; --- undo: add the INVERSE patch. removal is growth here, never deletion --
   ;     the patch stays, its dependents stay valid, and the store only gets
   ;     bigger, which is what keeps releases inclusion-ordered (R0 <= R1 <= R2)
   ;     and "upgrade = move to a superset" well-defined. so undo is git's
   ;     `revert`, never its `reset`.
   ;
   ;     it works through the WORKING TREE and hands off to `record`: put every
   ;     path P touched back the way P found it, then record that. record then
   ;     computes deps, blobs and tips against reality, so the inverse is a patch
   ;     like any other and needs no special case anywhere downstream.
   ;
   ;     the interesting case is a path that MOVED ON since P wrote it. undoing
   ;     wholesale would silently discard the later work (a hunk is file-grain,
   ;     so P's hunk IS the whole file). instead it is a three-way merge, and the
   ;     three sides are already named: base = what P WROTE, ours = what the path
   ;     holds NOW, theirs = what P REPLACED. that keeps the later edits and lifts
   ;     out P's, which is what a revert means; a true overlap lands in markers ---
   (sb-undo args)
    (: _ (mk ())
       (? (0 < dirty ())
          (: _ (say err "sb: working tree has changes; record or revert first\n") 0)
          (: id (idpfx (? (two? args) <args ""))
             (? !id 0                                          ; idpfx already said why
                (: hs (fld (patch id) 'hunks)
                   ix (: (go m t) (? (two? m) (: _ (pin t <<m <><m) (go >m t)) t))
                   sn (ix (snap ()) (tablet 0))
                   nc (: (go m n)
                          (? !(two? m) n
                             (: h <m p <h pold <>h pnew <>>h
                                cur (peep sn p ())             ; the index maps path -> chash DIRECTLY
                                (? (cur = pnew)                ; P still owns the path: a clean lift
                                   (: _ (? !pold (unlink p)
                                           (: _ (mkparents p) (lay p (blob pold))))
                                      (go >m n))
                                   ; it moved on -- merge P's write OUT of what is there now
                                   (: bw (bl pnew) ow (bl cur) tw (bl pold)
                                      (? !(two? bw && two? ow && two? tw)
                                         (: _ (say err ("sb: CONFLICT on " + p
                                                        + " -- a birth or death moved on; kept what is there\n"))
                                            (go >m (n + 1)))
                                         (bin? <>bw || bin? <>ow || bin? <>tw)
                                         (: _ (say err ("sb: CONFLICT on " + p
                                                        + " -- binary, cannot merge; kept what is there\n"))
                                            (go >m (n + 1)))
                                         (: r (diff3 <>bw <>ow <>tw "here now" ("undone " + sh id))
                                            _ (lay p <r)
                                            k <>r
                                            _ (? (0 = k) (puts ("sb: merged " + p + " -- the later edits kept\n"))
                                                 (say err ("sb: CONFLICT in " + p + " -- " + show k
                                                           + " region" + (? (1 = k) "" "s") + " left in markers\n")))
                                            (go >m (+ n k))))))))
                        (go hs 0))
                   _ (? (0 = nc) ()
                        (say err "sb: settle the flagged paths before recording the undo\n"))
                   (? (0 < nc) 0
                      (: r (sb-record ((? (two? >args) <>args "undo ") + sh id))
                         (? r 1 (: _ (puts "sb: nothing to undo -- that patch is already lifted\n") 0))))))))))

; --- the CLI face: usage + the verb dispatcher (cook-main's shape) ---
(:
   (usage _) (: _ (say err "sb -- svalbard, the patch-set vcs (doc/sb.md)
usage: sb VERB ..
  record [NOTE]   working changes -> a patch in the store (.sb/)
  sync PEER       EXCHANGE patch sets with a peer nest (a dir holding .sb/),
                  or PULL from an http:// remote -- any static file tree serving
                  a .sb/ is a complete one, and it takes no push.
                  pull what it has, push what it lacks, settle BOTH trees to
                  the union. two patches on one path three-way merge; a true
                  overlap lands in markers and exits 1 -- resolve, then record
  apply [ID..]    realize a dep-consistent SUBSET into the working tree (an ID
                  may be a prefix; its deps come along). checkout and cherry-pick
                  are one act here. no ID realizes the whole store -- the way back
  bank NAME       freeze the current head -- its whole TIP SET -- as an immutable
                  release. deps are per PATH, so a multi-tip head is ordinary
                  parallel work, not a fork to repair. `apply NAME` realizes it
  undo ID [NOTE]  add the INVERSE of a patch -- revert as growth, never deletion.
                  a path that moved on since three-way merges instead of being
                  clobbered: P's write is lifted OUT, later edits stay
  log             the patches, newest first (* marks a tip)
  diff            working tree vs the recorded state (unified; exit 1 on change)
")
                (quit 2))
   (sb-main as)
    (? !(two? as) (usage ())
       (: v <as
          (? (v = "record") (: _ (sb-record (? (two? >as) <>as "")) (quit 0))
             (v = "sync") (? (two? >as) (: r (sb-sync <>as) (quit (? r 0 1))) (usage ()))
             (v = "apply") (: r (sb-apply >as) (quit (? r 0 1)))
             (v = "bank") (? (two? >as) (: r (sb-bank >as) (quit (? r 0 1))) (usage ()))
             (v = "undo") (? (two? >as) (: r (sb-undo >as) (quit (? r 0 1))) (usage ()))
             (v = "log") (: _ (sb-log ()) (quit 0))
             (v = "diff") (: n (sb-diff ()) (quit (? (0 < n) 1 0)))
             (usage ()))))
)

; the SEAT (cook.l's shape): fire sb-main iff this file -- or the installed
; `sb` binary -- sits in the program seat of the real command line (the
; positional program or a -l preload), taking the args AFTER the seat. cmdline
; is the full C argv, never rebound, so the read is the same in both load orders
; and the cat stays quiet under any other name -- which is why the gate can cat
; this file and have nothing fire. sb-main reads BARE: the module's layer holds
; this file's binds and is still the head when this form compiles. body-having:
; the seat leaks nothing.
(: _ (pins (from 'verbs 'tab) 'sb sb-main)
   s ((from 'verbs 'fire) ["sb" "sb.l"])
   (? (two? s) (sb-main >s) ()))

) ; module 'sb
