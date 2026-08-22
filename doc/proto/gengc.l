; layer-2 spike: a GENERATIONAL GC (`gengc`) modelled in ai -- a runnable, value-level model of a
; nursery + old-space copying collector over ai's current single-pool Cheney two-space core (love.c
; gcg/gcp/evac_*). it is the design-by-prototype step the measurement justified: tools/gcstat.l on a
; -DAI_STAT host showed ai's interpreted churn is 85-94% INFANT MORTALITY and the persistent live set
; is RECOPIED ~150x over a run (every full collection drags the same long-lived objects through to-
; space). a nursery removes that rescan tax: a MINOR collection copies only the young survivors, never
; touching old space; a MAJOR collection is today's gcg. glaze/deforestation (doc/proto/forest.l) is the
; ORTHOGONAL attack -- it AVOIDS allocating the intermediates; this REAPS cheaply what's still allocated.
;
; THE MODEL. a heap object is (addr . out-edges) -- its identity and the addresses it points at. a
; REGION (nursery `nur`, old `old`) is a list of such objects; a WORLD is (nur old rem roots next):
;   nur   young objects (this collection's allocations)
;   old   tenured objects (survived a minor, or born old)
;   rem   the REMEMBERED SET: old addrs that hold a pointer INTO the nursery (recorded at the barrier)
;   roots the stack + C live + core vars (addrs the mutator can reach directly)
;   next  the fresh-address stream (a model of bump allocation)
; THE CORRESPONDENCE TO REAL ai (annotated per op below):
;   region membership  <->  a pointer ADDRESS-RANGE test (ai carries NO spare age bit -- the recent
;                           "drop jitba's GC tag word" commit confirms objects are header-minimal, so
;                           generation MUST be read from the address, never a per-object field).
;   point + rem        <->  the WRITE BARRIER on ai_mapput (love.c:~3909/3914) -- the ONLY in-place
;                           pointer mutation that can mint an old->young edge (chains/arrays immutable,
;                           casks hold bytes, num/str are GC leaves). a tiny barrier surface: one hot path.
;   alloc-old          <->  TENURE-ON-BIRTH: interned noms (sidesteps the weak-table/minor-GC race),
;                           casks/ports/toasts (sidesteps minor-GC finalizers), maps/boxes -> born old.
;   minor              <->  a Cheney scavenge of the NURSERY, roots = stack + C roots + core vars + rem,
;                           survivors promoted into old; old is NOT scanned (that is the whole win).
;   major              <->  today's gcg: a full copy of old from the roots.
;
; run:    out/host/love doc/proto/gengc.l                       (the demo; standalone `assert` is lenient)
; strict: cat test/00-init.l doc/proto/gengc.l | out/host/love  (00-init makes `assert` enforce -- the gate)
;
; the load-bearing assert is `barrier is necessary`: an old->young edge recorded WITHOUT the rem entry
; lets a minor wrongly collect a live young object -- the model reproduces the exact bug the barrier
; prevents, so the barrier earns its keep by construction. counters show a minor scans young+rem, not
; all of old. gengc functions share one body-less `:` letrec; boxfix resolves the forward/mutual edges.

(:
 ; --- set-as-list helpers (a fresh gensym would be overkill; addrs are charms, compared by =) ---
 (amem? x l) (? (atom? l) 0 (= x (cap l)) 1 (amem? x (cup l)))   ; spine test, never (! l): ! reads the NET
 (aadd x l)  (? (amem? x l) l (link x l))                        ; set insert
 (aunion a b) (foldl (\ s x (aadd x s)) b a)                     ; foldl is (step acc elem) -- house order

 ; --- a region is a list of objects (addr . edges); accessors over it ---
 (nth l i)   (? (= i 0) (cap l) (nth (cup l) (- i 1)))
 (rget a r)  (? (atom? r) () (= a (cap (cap r))) (cap r) (rget a (cup r)))   ; the object at addr a, or ()
 (rhas? a r) (two? (rget a r))                                   ; present? (an object is a chain, () is not)
 (redges a r)(cup (rget a r))                                    ; its out-edges ((), absent or leaf)
 (rdel a r)  (filter (\ o (! (= (cap o) a))) r)                  ; region minus object a
 (rupd a f r)(map (\ o (? (= (cap o) a) (link a (f (cup o))) o)) r)  ; map f over object a's edges

 ; --- the world (nur old rem roots next) and its accessors ---
 (world n o m r x) (L n o m r x)
 (w-nur w) (nth w 0)  (w-old w) (nth w 1)  (w-rem w) (nth w 2)  (w-roots w) (nth w 3)  (w-next w) (nth w 4)
 (w-last w) (- (w-next w) 1)                                     ; the addr the last alloc handed out
 fresh (world () () () () 0)

 ; --- allocation: bump a fresh addr. young by default; alloc-old is tenure-on-birth. ---
 (alloc w edges)
   (world (link (link (w-next w) edges) (w-nur w)) (w-old w) (w-rem w) (w-roots w) (+ 1 (w-next w)))
 (alloc-old w edges)                                             ; born old: noms/casks/ports/maps
   (world (w-nur w) (link (link (w-next w) edges) (w-old w)) (w-rem w) (w-roots w) (+ 1 (w-next w)))
 (addroot w a) (world (w-nur w) (w-old w) (w-rem w) (aadd a (w-roots w)) (w-next w))

 ; --- the WRITE BARRIER. (point w oa ya): make old object oa reference young ya AND remember oa.
 ;     point-raw is the SAME edge WITHOUT remembering -- the bug a missing barrier would be. ---
 (point w oa ya)
   (world (w-nur w) (rupd oa (\ es (aadd ya es)) (w-old w)) (aadd oa (w-rem w)) (w-roots w) (w-next w))
 (point-raw w oa ya)
   (world (w-nur w) (rupd oa (\ es (aadd ya es)) (w-old w)) (w-rem w) (w-roots w) (w-next w))

 ; --- reachability WITHIN a region: DFS that follows edges only into objects present in r (an edge
 ;     leaving the region -- e.g. young -> old in a minor -- is a boundary the scavenge does not cross). ---
 (reach1 r a seen) (? (|| (amem? a seen) (! (rhas? a r))) seen
                      (: s2 (aadd a seen) (foldl (\ s e (reach1 r e s)) s2 (redges a r))))
 (reach r seeds)   (foldl (\ s a (reach1 r a s)) () seeds)

 ; --- the MINOR collection: scavenge the nursery, promote survivors to old, leave old untouched.
 ;     seeds = young addrs reachable DIRECTLY (roots that are young) PLUS young addrs held by a
 ;     remembered old object (the old->young edges). everything young not so reached is dead. ---
 (young-roots w)
   (aunion (filter (\ a (rhas? a (w-nur w))) (w-roots w))                       ; roots into the nursery
           (foldl (\ s oa (aunion (filter (\ e (rhas? e (w-nur w))) (redges oa (w-old w))) s))
                  () (w-rem w)))                                                ; rem old-objs' young edges
 (minor w)
   (: live (reach (w-nur w) (young-roots w))                                    ; the young survivors
      old2 (foldl (\ o a (link (rget a (w-nur w)) o)) (w-old w) live)           ; promote them into old
    (world () old2 () (w-roots w) (w-next w)))                                  ; nursery cleared; rem reset
 (minor-scanned w) (+ (tally (reach (w-nur w) (young-roots w))) (tally (w-rem w)))  ; minor's work
 (major-scanned w) (tally (w-old w))                                            ; a full gcg's work

 ; --- the MAJOR collection: today's gcg -- a full copy of old from the roots (nursery already empty). ---
 (major w)
   (: live (reach (w-old w) (w-roots w))
    (world (w-nur w) (foldl (\ o a (link (rget a (w-old w)) o)) () live) () (w-roots w) (w-next w)))

 ; ================================ self-checks ================================
 ; (1) infant mortality made cheap: a young object referenced by NOTHING dies at the next minor.
 _ (assert "(1) dead young is reclaimed by a minor (and not resurrected into old)"
     (: w (alloc fresh ())                                       ; one young object, no root, no referrer
        a (w-last w)
        w (minor w)
      (! (rhas? a (w-nur w))))                                   ; gone from the nursery
     (: w (minor (alloc fresh ())) (! (two? (w-old w)))) )       ; ... and old stays empty

 ; (2) a rooted young object is PROMOTED to old (survives, leaves the nursery).
 _ (assert "(2) a rooted young object promotes into old"
     (: w (alloc fresh ()) a (w-last w)
        w (addroot w a)
        w (minor w)
      (&& (rhas? a (w-old w)) (! (rhas? a (w-nur w))))) )

 ; (3) THE LOAD-BEARING ONE: an old->young edge keeps the young object alive ONLY because the barrier
 ;     remembered it. with the barrier: Y survives. without (point-raw): the minor wrongly reaps Y --
 ;     the model reproduces the precise bug a missing write barrier is, so the barrier earns its keep.
 _ (assert "(3a) barrier: old->young edge keeps young alive (Y promoted)"
     (: w (alloc-old fresh ()) o (w-last w)                      ; an OLD object o (not a root)
        w (addroot w o)                                          ; ... reachable, so it is live
        w (alloc w ()) y (w-last w)                              ; a YOUNG object y, no root of its own
        w (point w o y)                                          ; barrier: o -> y, remembered
        w (minor w)
      (rhas? y (w-old w))) )                                     ; y survived via the rem seed
 _ (assert "(3b) WITHOUT the barrier the same y is wrongly collected (so the barrier is necessary)"
     (: w (alloc-old fresh ()) o (w-last w)
        w (addroot w o)
        w (alloc w ()) y (w-last w)
        w (point-raw w o y)                                      ; edge added but NOT remembered
        w (minor w)
      (! (rhas? y (w-old w)))) )                                 ; y is gone -- the bug the barrier prevents

 ; (4) tenure-on-birth: an OLD object survives a minor without being a root and without any rem entry,
 ;     because a minor never scans old. (noms/casks/ports/maps ride this lane.)
 _ (assert "(4) a born-old object is untouched by a minor (no root, no rem)"
     (: w (alloc-old fresh ()) a (w-last w)
        w (minor w)
      (rhas? a (w-old w))) )

 ; (5) the nursery is empty after every minor (a clean semispace flip of young space).
 _ (assert "(5) the nursery is empty after a minor"
     (: w (alloc (alloc (alloc fresh ()) ()) ()) (! (two? (w-nur (minor w)))) ) )

 ; (6) a chain of young objects reachable from one root all promote together (transitive reach).
 _ (assert "(6) a transitively-reachable young chain promotes whole"
     (: w (alloc fresh ()) a (w-last w)                          ; a
        w (alloc w (L a)) b (w-last w)                           ; b -> a
        w (alloc w (L b)) c (w-last w)                           ; c -> b -> a
        w (addroot w c)
        w (minor w)
      (&& (rhas? a (w-old w)) (rhas? b (w-old w)) (rhas? c (w-old w)))) )

 ; (7) the COST claim: a minor scans only (young survivors + rem), NOT all of old -- the rescan tax
 ;     gone. build a big rooted-old set and a tiny young set; the minor's work is far below old's size.
 bigold (: (grow w k) (? (= k 0) w (grow (addroot (alloc-old w ()) (w-last (alloc-old w ()))) (- k 1)))
           (grow fresh 50))                                      ; 50 live old objects (a heavy gcg)
 _ (assert "(7) minor scans young+rem, not all of old (the rescan tax a full gcg pays)"
     (: w (alloc bigold ()) w (addroot w (w-last w))             ; + one young object
      (&& (< (minor-scanned w) (major-scanned w))                ; minor work << major work
          (= 1 (minor-scanned w)))) )                            ; exactly the 1 young survivor scanned

 ; (8) a major (today's gcg) still reclaims dead OLD objects -- generational ADDS a cheap minor, it
 ;     does not replace the full collection that recovers tenured garbage.
 _ (assert "(8) a major reclaims unreachable old objects"
     (: w (alloc-old fresh ()) a (w-last w)                      ; an unrooted old object
        w (alloc-old w ()) b (w-last w)
        w (addroot w b)                                          ; only b is rooted
        w (major w)
      (&& (rhas? b (w-old w)) (! (rhas? a (w-old w))))) )

 _ (. ";; doc/proto/gengc.l: generational-GC model self-checks pass -- ")
 _ (. "a nursery+old collector with a write barrier; the barrier is shown NECESSARY (3b) and a minor\n")
 _ (. ";; scans young+rem, not all of old (7) -- the ~150x recopy tools/gcstat.l measured, removed.\n")
 _ (. ";; minor vs major work on 50-old + 1-young: ")
 _ (: w (alloc bigold ()) w (addroot w (w-last w))
      (do (. (minor-scanned w)) (. " scanned by a minor vs ") (. (major-scanned w)) (. " by a major\n"))))
