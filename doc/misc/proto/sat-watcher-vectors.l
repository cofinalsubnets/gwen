; sat-watcher-vectors.l -- the minisat-style WATCHER-VECTOR + BLOCKING-LITERAL + binary-lane
; experiment for crew/sat/flat.l, built and MEASURED 2026-07-02, then KEPT OUT: on this solver the
; committed intrusive-NODE watch lists win everywhere we measured. warm kernels, same battery:
;   php6/7/8:            nodes 4/42/980 ms      vectors+blockers 6/57/981 ms
;   random 3-SAT n=1000: nodes 12-13 ms         vectors+blockers 22 ms
; WHY vectors lose here: CDCL slide churn is constant, and an O(1) node relink beats the
; vector's swap-remove + amortized segment growth (grow-by-abort lisp round trips, ~1k/solve
; on php7) + the reduce-time exact rebuild; the blocker only pays when a touched clause is
; ALREADY satisfied, which conflict-storm propagation rarely gives it. the binary-clause
; watcher lane (crew/sat/unit/conflict without touching the arena) is real but rode the same
; vector costs. the design is FAITHFUL minisat structure (blockers, in-watcher binaries,
; backward newest-first walk) and gates green -- differentially fuzzed vs DPLL, kernels ==
; twins -- so it is a sound base if a workload with high blocker hit rates ever shows up.
; run: cat crew/sat/sat.l doc/misc/proto/sat-watcher-vectors.l | ai   (replaces flat.l wholesale)
;
; crew/sat/flat -- the CDCL solver over FLAT state, with THREE DOMAIN-SPECIFIC NATIVE
; KERNELS: at solver-build time the hot loops are assembled through crew/holo/ (neutral IR,
; section offsets baked as immediates -- each kernel is SPECIALIZED to the instance's
; nvars, cached per nv) and installed via the `nif` seam:
;   bcp  (fx ar wn)        two-watched-literal propagation      -> conflict cid | -1
;   conf (fx ar wn confl)  the WHOLE conflict: 1-UIP absorb (+ inline VSIDS bumps),
;                          self-subsumption minimization (in-place compaction), glue,
;                          backjump, learnt-clause commit + watches, asserting assign
;                                                              -> (cid<<6)|glue | -1 at level 0
;   dec  (fx)              highest-activity free var, phase-saved polarity, assign
;                                                              -> the var | 0 = SAT
; the interpreted twins (fbcp3/fconf3/fdec3) over the SAME memory are the deopt
; fallbacks, the arm64/no-`nif` engine (the glazed image mops nif; make exports
; LOVE_NO_IMAGE=1 so every gate run has it), and the differential oracle: the `fknob`
; box forces the twins and `fboth` gates verdict equality. lisp keeps only the cold
; path: restarts, learnt-DB reduction, activity decay/rescale.
; Run AFTER sat.l (it reuses the leaked oracle + instances):
;   cat crew/sat/sat.l crew/sat/flat.l | ai
;
; a literal is ENCODED: +v -> 2v, -v -> 2v+1 (neg = xor 1, var = >>1). val[v] holds
; the TRUE encoded literal itself (0 = free) so lit-val is a load + a compare. VSIDS
; activities are INTEGERS (vinc starts 256, grows *21//20 per conflict; when it tops
; 2^24 every activity shifts right 8 -- decay-by-shift, ratios kept, ancient bumps
; fade to 0, which is the point). words are 8-byte slots whose LOW 4 bytes carry the
; value (getw/putw touch only those; the high bytes stay zero from the zeroed cask,
; so the kernels' 64-bit ld/st and the byte accessors agree exactly).
; fx layout (word offsets; nv = nvars):
;   w0 top  w1 qhead  w2 level  w3 nv  w4 aru  w5 wvu  w6 vinc  w7 kernel scratch
;   TRO=8 trail | LVO lvl | RSO rsn (0 none, else cid+1) | WVO + WVS the WATCHER
;   VECTOR spine (2nv+2 each, by encoded lit: segment offset into wv / live count)
;   | VAO val | ACO activity | PHO saved phase (0 never, 1 pos, 2 neg) | SEO seen
;   scratch (conf clears it on entry; also reused as a level-mark set)
; a clause at arena offset cid is [size, lit0..], watches ALWAYS slots cid+1/cid+2.
; WATCHERS are minisat-style contiguous per-literal vectors in the wv cask: a segment
; is [cap, (blocker, 2*cid+bin)..]; offset 0 = "empty, cap 0" (wv word 0 stays 0).
; the BLOCKER is a literal of the clause (kept fresh = the other watch): if it reads
; true the clause is satisfied and BCP never touches the arena. a BINARY clause
; (bin bit set) stores the other literal AS the blocker, so crew/sat/unit/conflict all
; resolve in the watcher alone -- the arena is only read by analyze. propagation
; compacts each vector in place (read i / write j). the kernels never grow a cask:
; a slide into a FULL vector aborts cleanly (compact the rest back, qhead-- so the
; literal re-propagates, return -2-lit) and lisp doubles that segment and re-calls;
; conf's two learnt watch appends set PEND bITS in its return instead and lisp does
; the rare append before anything propagates. the conf caller pre-ensures arena room
; (aru+nv+3 words).
(:
   ; --- 8-byte word slots, low 4 bytes live ---------------------------------------
   (getw c i) (: b (* 8 i) (| (peep c b 0)
                              (| (<< (peep c (+ b 1) 0) 8)
                                 (| (<< (peep c (+ b 2) 0) 16) (<< (peep c (+ b 3) 0) 24)))))
   (putw c i v) (: b (* 8 i)
                   _ (pin c b (& v 255)) _ (pin c (+ b 1) (& (>> v 8) 255))
                   _ (pin c (+ b 2) (& (>> v 16) 255)) (pin c (+ b 3) (& (>> v 24) 255)))
   (enc l) (? (< l 0) (+ 1 (* 2 (- 0 l))) (* 2 l))          ; signed literal -> encoded
   ; --- the layout, from nv (twins recompute these from fx w3) --------------------
   (lvo-of nv) (+ 8 (+ nv 1))
   (rso-of nv) (+ (lvo-of nv) (+ nv 1))
   (wvo-of nv) (+ (rso-of nv) (+ nv 1))
   (wvs-of nv) (+ (wvo-of nv) (+ 2 (* 2 nv)))
   (vao-of nv) (+ (wvs-of nv) (+ 2 (* 2 nv)))
   (aco-of nv) (+ (vao-of nv) (+ nv 1))
   (pho-of nv) (+ (aco-of nv) (+ nv 1))
   (seo-of nv) (+ (pho-of nv) (+ nv 1))
   ; --- the twin assign, self-contained (offsets from fx w3) ----------------------
   (fasg fx el rv)                                          ; set el TRUE at the current level, reason rv (0 = decision)
    (: nv (getw fx 3) v (>> el 1) t (getw fx 0)
       _ (putw fx (+ (vao-of nv) v) el)
       _ (putw fx (+ (lvo-of nv) v) (getw fx 2))
       _ (putw fx (+ (rso-of nv) v) rv)
       _ (putw fx (+ 8 t) el)
       (putw fx 0 (+ t 1)))
   ; --- state access for the lisp driver (offsets cached in s) --------------------
   (flv s el) (: m (getw (peep s 'fx ()) (+ (peep s 'vao 0) (>> el 1)))
                 (? (= m 0) 0 (= m el) 1 -1))               ; 1 sat / -1 false / 0 free
   (flvl s el) (getw (peep s 'fx ()) (+ (peep s 'lvo 0) (>> el 1)))
   (fens s k need)                                          ; ensure cask s[k] holds >= need bytes (grow by doubling)
    (: c (peep s k ())
       (? (<= need (tally c)) 0
          (: n2 ((: (go m) (? (>= m need) m (go (* 2 m)))) (* 2 (tally c)))
             nu (cask n2) _ (pour nu 0 c 0 (tally c)) _ (pin s k nu) 0)))
   (fassign s el rv) (fasg (peep s 'fx ()) el rv)
   ; --- clause DB (lisp side: initial load + reduction; aru/wnu live in fx) --------
   (fadd s lits)                                            ; append [size, lits..] to the arena; -> cid (word offset)
    (: n (tally lits) fx (peep s 'fx ()) aru (getw fx 4)
       _ (fens s 'ar (* 8 (+ aru (+ n 1))))
       ar (peep s 'ar ())
       _ (putw ar aru n)
       _ (foldl (\ j el (: _ (putw ar j el) (+ j 1))) (+ aru 1) lits)
       _ (putw fx 4 (+ aru (+ n 1))) aru)
   (fclits s cid)                                           ; the clause's encoded literals, as a list
    (: ar (peep s 'ar ()) n (getw ar cid)
       ((: (go j acc) (? (< j (+ cid 1)) acc (go (- j 1) (link (getw ar j) acc)))) (+ cid n) ()))
   (fseg-grow s el)                                         ; give el's vector a doubled segment; -> its offset
    (: fx (peep s 'fx ()) wvo (peep s 'wvo 0) wvs (peep s 'wvs 0)
       seg (getw fx (+ wvo el))
       cap (? (= seg 0) 0 (getw (peep s 'wv ()) seg))
       ncap (? (< cap 4) 4 (* 2 cap))
       wvu (getw fx 5)
       _ (fens s 'wv (* 8 (+ wvu (+ 1 (* 2 ncap)))))
       wv (peep s 'wv ())
       sz (getw fx (+ wvs el))
       _ (putw wv wvu ncap)
       _ (? (= seg 0) 0 (pour wv (* 8 (+ wvu 1)) wv (* 8 (+ seg 1)) (* 8 (* 2 sz))))
       _ (putw fx (+ wvo el) wvu)
       _ (putw fx 5 (+ wvu (+ 1 (* 2 ncap))))
       wvu)
   (fwatch s cid el blk)                                    ; append the watcher (blk, 2cid+bin) to el's vector
    (: fx (peep s 'fx ()) wvo (peep s 'wvo 0) wvs (peep s 'wvs 0)
       sz (getw fx (+ wvs el))
       seg (: sg (getw fx (+ wvo el))
              cap (? (= sg 0) 0 (getw (peep s 'wv ()) sg))
              (? (< sz cap) sg (fseg-grow s el)))
       wv (peep s 'wv ())
       c2 (+ (* 2 cid) (? (= 2 (getw (peep s 'ar ()) cid)) 1 0))
       _ (putw wv (+ seg (+ 1 (* 2 sz))) blk)
       _ (putw wv (+ seg (+ 2 (* 2 sz))) c2)
       (putw fx (+ wvs el) (+ sz 1)))
   (fenq s el rv)                                           ; level-0 fact: assign if free, flag bad if already false
    (: m (flv s el) (? (= m 1) 0 (= m -1) (pin s 'bad 1) (: _ (fassign s el rv) 0)))
   (fload s c0)                                             ; a source clause (signed lits) into the DB + watches
    (? (= 1 (taut? c0)) 0
       (: c (dedup-lits c0)
          (? (atom? c) (pin s 'bad 1)
             (atom? (cup c)) (: cid (fadd s (link (enc (cap c)) ())) (fenq s (enc (cap c)) (+ cid 1)))
             (: cid (fadd s (map enc c)) a (enc (cap c)) b (enc (caup c))
                _ (fwatch s cid a b) (fwatch s cid b a)))))
   ; --- the interpreted BCP twin: (fbcp3 fx ar wv) --------------------------------
   ; -> conflict cid | -1 done | -2-el (el's vector is full: grow it and re-call).
   ; walks each false literal's watcher vector with read i / write j compaction. the
   ; BLOCKER fast path (val[blk] == blk -> keep, no arena touch) handles satisfied
   ; clauses; a BINARY watcher (bin bit) resolves crew/sat/unit/conflict in the watcher
   ; alone. bail commits the compaction (copy [i..n) down to j) so the vector stays
   ; exact on every exit; a grow-bail also rewinds qhead so f re-propagates (kept
   ; watchers re-check as satisfied -- idempotent).
   (fbcp3 fx ar wv)
    (: nv (getw fx 3) wvo (wvo-of nv) wvs (wvs-of nv) vao (vao-of nv)
       val (\ el (getw fx (+ vao (>> el 1))))               ; the var's true literal, 0 = free
       asg (\ el rv (fasg fx el rv))
       prop1 (\ f
        (: seg (getw fx (+ wvo f)) b (+ seg 1)
           ((: (go i sz)                                      ; BACKWARD walk: newest watchers first
               (? (< i 0) (: _ (putw fx (+ wvs f) sz) -1)
                  (: blk (getw wv (+ b (* 2 i))) c2 (getw wv (+ b (+ 1 (* 2 i))))
                     fresh (\ bl (: _ (putw wv (+ b (* 2 i)) bl) (go (- i 1) sz)))
                     (? (= (val blk) blk) (go (- i 1) sz)             ; blocker satisfied: just move on
                        (! (= 0 (& c2 1)))                            ; BINARY: blk IS the other literal
                          (? (= 0 (val blk)) (: _ (asg blk (+ (>> c2 1) 1)) (go (- i 1) sz))   ; unit
                             (: _ (putw fx (+ wvs f) sz) (>> c2 1)))  ; false: conflict
                        (: cid (>> c2 1)
                           _ (? (= (getw ar (+ cid 1)) f)             ; normalize: f at slot cid+2
                                (: t (getw ar (+ cid 2)) _ (putw ar (+ cid 2) f) (putw ar (+ cid 1) t)) 0)
                           oth (getw ar (+ cid 1))
                           (? (= (val oth) oth) (fresh oth)           ; sat: blocker freshened
                              (: k ((: (sc j2) (? (>= j2 (+ cid (+ 1 (getw ar cid)))) 0
                                            (: e (getw ar j2)
                                               (? (! (= (val e) (^ e 1))) j2 (sc (+ j2 1))))))
                                    (+ cid 3))
                                 (? (! (= k 0))
                                    (: e (getw ar k)                  ; slide f -> e: append FIRST (abort-safe)
                                       sg2 (getw fx (+ wvo e)) sz2 (getw fx (+ wvs e))
                                       cap2 (? (= sg2 0) 0 (getw wv sg2))
                                       (? (>= sz2 cap2)
                                          (: _ (putw fx (+ wvs f) sz)                    ; grow request:
                                             _ (putw fx 1 (- (getw fx 1) 1)) (- -2 e))   ; commit, qhead--
                                          (: _ (putw wv (+ sg2 (+ 1 (* 2 sz2))) oth)
                                             _ (putw wv (+ sg2 (+ 2 (* 2 sz2))) c2)
                                             _ (putw fx (+ wvs e) (+ sz2 1))
                                             _ (putw ar k f) _ (putw ar (+ cid 2) e)
                                             ns (- sz 1)                                 ; swap-remove: last -> i
                                             _ (putw wv (+ b (* 2 i)) (getw wv (+ b (* 2 ns))))
                                             _ (putw wv (+ b (+ 1 (* 2 i))) (getw wv (+ b (+ 1 (* 2 ns)))))
                                             (go (- i 1) ns))))                          ; last was already visited
                                    (= 0 (val oth)) (: _ (asg oth (+ cid 1)) (fresh oth))   ; unit
                                    (: _ (putw fx (+ wvs f) sz) cid)))))))))             ; conflict
            (- (getw fx (+ wvs f)) 1) (getw fx (+ wvs f)))))
       ((: (drain _)
           (? (>= (getw fx 1) (getw fx 0)) -1
              (: p (getw fx (+ 8 (getw fx 1))) _ (putw fx 1 (+ 1 (getw fx 1)))
                 r (prop1 (^ p 1))
                 (? (= r -1) (drain ()) r)))) ()))
   ; --- the interpreted CONFLICT twin: (fconf3 fx ar wv confl) --------------------
   ; the whole conflict handler, phase for phase what the conf kernel does:
   ;   0 level 0 -> -1        1 clear seen           2 1-UIP (absorb + trail walk),
   ;   asserting lit to ar[cid0+1], tail collected at ar[cid0+2..]
   ;   3 minimize (self-subsumption; dropped lits STAY seen), compact in place
   ;   4 clear seen; glue (level-mark set) + backjump level in one pass
   ;   5 backjump (save phase, unassign)   6 commit size/aru, reorder watch-2, watches
   ;   7 assert learnt[0] with reason cid0+1   -> (cid0<<6)|min(glue,63)
   (fconf3 fx ar wv confl)
    (: nv (getw fx 3) lvo (lvo-of nv) rso (rso-of nv) wvo (wvo-of nv) wvs (wvs-of nv)
       vao (vao-of nv) aco (aco-of nv) pho (pho-of nv) seo (seo-of nv)
       dl (getw fx 2)
       (? (= dl 0) -1
        (: cid0 (getw fx 4)
           czero (\ _ ((: (go v) (? (> v nv) 0 (: _ (putw fx (+ seo v) 0) (go (+ v 1))))) 0))
           _ (czero ())
           abs1 (\ cs skipv counter widx                    ; absorb clause at cs -> (counter . widx)
            ((: (go j c w)
                (? (> j (+ cs (getw ar cs))) (link c w)
                   (: el (getw ar j) v (>> el 1)
                      (? (= v skipv) (go (+ j 1) c w)
                         (! (= 0 (getw fx (+ seo v)))) (go (+ j 1) c w)
                         (= 0 (getw fx (+ lvo v))) (go (+ j 1) c w)
                         (: _ (putw fx (+ seo v) 1)
                            _ (putw fx (+ aco v) (+ (getw fx (+ aco v)) (getw fx 6)))   ; bump
                            (? (= (getw fx (+ lvo v)) dl) (go (+ j 1) (+ c 1) w)
                               (: _ (putw ar w el) (go (+ j 1) c (+ w 1)))))))))
             (+ cs 1) counter widx))
           a0 (abs1 confl 0 0 (+ cid0 2))
           r ((: (walk counter widx t)
                 (: np ((: (fs i) (? (= 0 (getw fx (+ seo (>> (getw fx (+ 8 i)) 1)))) (fs (- i 1)) i)) t)
                    el (getw fx (+ 8 np)) v (>> el 1)
                    _ (putw fx (+ seo v) 0) c (- counter 1)
                    (? (= c 0) (link el widx)
                       (: a (abs1 (- (getw fx (+ rso v)) 1) v c widx)
                          (walk (cap a) (cup a) (- np 1))))))
              (cap a0) (cup a0) (- (getw fx 0) 1))
           uip (cap r) wend (cup r)
           _ (putw ar (+ cid0 1) (^ uip 1))
           wr ((: (mn rd wr)                                ; minimize, compacting kept lits down
                  (? (>= rd wend) wr
                     (: el (getw ar rd) v (>> el 1) rv (getw fx (+ rso v))
                        red (? (= rv 0) 0
                               ((: (ck j) (? (> j (+ (- rv 1) (getw ar (- rv 1)))) 1
                                             (: v2 (>> (getw ar j) 1)
                                                (? (= v2 v) (ck (+ j 1))
                                                   (= 0 (getw fx (+ lvo v2))) (ck (+ j 1))
                                                   (! (= 0 (getw fx (+ seo v2)))) (ck (+ j 1))
                                                   0)))) (+ rv 0)))     ; j starts at (rv-1)+1 = rv
                        (? (= red 1) (mn (+ rd 1) wr)
                           (: _ (putw ar wr el) (mn (+ rd 1) (+ wr 1)))))))
               (+ cid0 2) (+ cid0 2))
           _ (czero ())
           gb ((: (go j glue bj)                            ; glue + backjump level over the kept tail
                  (? (>= j wr) (link glue bj)
                     (: L (getw fx (+ lvo (>> (getw ar j) 1)))
                        bj2 (? (> L bj) L bj)
                        (? (= 0 (getw fx (+ seo L)))
                           (: _ (putw fx (+ seo L) 1) (go (+ j 1) (+ glue 1) bj2))
                           (go (+ j 1) glue bj2)))))
               (+ cid0 2) (: _ (putw fx (+ seo dl) 1) 1) 0)
           glue (cap gb) bj (cup gb)
           nt ((: (go t)                                    ; backjump: pop above bj, save phase
                  (? (= t 0) 0
                     (: el (getw fx (+ 8 (- t 1))) v (>> el 1)
                        (? (> (getw fx (+ lvo v)) bj)
                           (: _ (putw fx (+ pho v) (+ 1 (& el 1)))
                              _ (putw fx (+ vao v) 0) _ (putw fx (+ lvo v) 0) _ (putw fx (+ rso v) 0)
                              (go (- t 1)))
                           t))))
               (getw fx 0))
           _ (putw fx 0 nt) _ (putw fx 1 nt) _ (putw fx 2 bj)
           size (- wr (+ cid0 1))
           _ (putw ar cid0 size) _ (putw fx 4 wr)
           pend (? (< size 2) 0
                (: mx ((: (go j b) (? (>= j wr) b                       ; reorder: max-level tail lit to slot 2
                              (go (+ j 1) (? (> (getw fx (+ lvo (>> (getw ar j) 1)))
                                               (getw fx (+ lvo (>> (getw ar b) 1)))) j b))))
                       (+ cid0 3) (+ cid0 2))
                   a (getw ar (+ cid0 2)) b (getw ar mx)
                   _ (putw ar (+ cid0 2) b) _ (putw ar mx a)
                   l0 (getw ar (+ cid0 1)) l1 (getw ar (+ cid0 2))
                   c2 (+ (* 2 cid0) (? (= size 2) 1 0))
                   try (\ el blk bit                                    ; append the watcher, or PEND (lisp appends)
                        (: sg (getw fx (+ wvo el)) sz (getw fx (+ wvs el))
                           cap (? (= sg 0) 0 (getw wv sg))
                           (? (>= sz cap) bit
                              (: _ (putw wv (+ sg (+ 1 (* 2 sz))) blk)
                                 _ (putw wv (+ sg (+ 2 (* 2 sz))) c2)
                                 _ (putw fx (+ wvs el) (+ sz 1)) 0))))
                   (+ (try l0 l1 1) (try l1 l0 2))))
           _ (fasg fx (getw ar (+ cid0 1)) (+ cid0 1))
           (| (<< cid0 8) (| (<< pend 6) (? (> glue 63) 63 glue))))))
   ; --- the interpreted DECIDE twin: (fdec3 fx) -> the decided var, 0 = SAT -------
   (fdec3 fx)
    (: nv (getw fx 3) vao (vao-of nv) aco (aco-of nv) pho (pho-of nv)
       v ((: (go v best ba)
             (? (> v nv) best
                (: a (getw fx (+ aco v))
                   (? (&& (= 0 (getw fx (+ vao v))) (|| (= best 0) (> a ba)))
                      (go (+ v 1) v a) (go (+ v 1) best ba)))))
          1 0 -1)
       (? (= v 0) 0
          (: ph (getw fx (+ pho v))
             _ (putw fx 2 (+ 1 (getw fx 2)))
             _ (fasg fx (? (= ph 1) (* 2 v) (+ 1 (* 2 v))) 0) v)))
   ; ===============================================================================
   ; the NATIVE kernels. lvm ABI: g=rdi(r6) Ip=rsi(r5) Hp=rdx(r2) Sp=rcx(r1); params
   ; TAGGED on Sp[i]. cask guard (deopts BEFORE any push -> machine sp balanced on
   ; every path): fixnum test + ap word vs (apof (cask 1)); byte base = [[v+8]+16].
   ; epilogue: tag acc, st Sp[0], Ip+16 -> lvm_ret. deopt (arity>=2): Ip <- interp
   ; body (args still on Sp). callee-saved r3/r11-r14 pushed after the guards.
   ; ===============================================================================
   (guard-ir i bufk)                                        ; Sp[i] must be a cask
    (L (L 'ld 'r0 'r1 (* 8 i)) '(test r0 1) '(br ne deopt)
       '(ld r7 r0 0) (L 'li 'r8 bufk) '(cmp r7 r8) '(br ne deopt))
   (fixg-ir i)                                              ; Sp[i] must be a fixnum
    (L (L 'ld 'r0 'r1 (* 8 i)) '(test r0 1) '(br eq deopt))
   epi-ir '((shl r0 1) (or r0 r0 1) (st r1 0 r0) (add r5 r5 16) (ld r7 r5 0) (jmpr r7))
   deopt2-ir '((label deopt) (ld r7 r5 8) (lea r5 r7 16) (ld r7 r5 0) (jmpr r7))   ; arity>=2
   deopt1-ir '((label deopt) (ld r7 r5 8) (mov r5 r7) (ld r7 r5 0) (jmpr r7))      ; arity 1
   ; --- kernel 1: BCP -- (bcp fx ar wv). registers: r11=fx r12=ar r13=wv r14=f
   ; (callee-saved), r3 = walk base (&wv[seg+1]), r9=i read, r10=j write, r0/r7/r8
   ; scratch; fx w7 stashes oth across the scan; machine stack holds n during the
   ; walk (+ lim during the scan, + j2/j/cid during a slide append). bail-copy (the
   ; shared exit) compacts [i..n) down to j and commits the size, so the vector is
   ; exact on every path; the grow exit also rewinds qhead. ---------------------------
   (fbcp-ir nv)
    (: tro8 64 wvo8 (* 8 (wvo-of nv)) wvs8 (* 8 (wvs-of nv)) vao8 (* 8 (vao-of nv))
       lvo8 (* 8 (lvo-of nv)) rso8 (* 8 (rso-of nv)) bufk (apof (cask 1))
       (cat (guard-ir 0 bufk) (cat (guard-ir 1 bufk) (cat (guard-ir 2 bufk)
       (cat '((push r3) (push r11) (push r12) (push r13) (push r14)
              (ld r0 r1 0)  (ld r0 r0 8) (lea r11 r0 16)
              (ld r0 r1 8)  (ld r0 r0 8) (lea r12 r0 16)
              (ld r0 r1 16) (ld r0 r0 8) (lea r13 r0 16))
       (cat (L '(label drain)
               '(ld r0 r11 8) '(ld r7 r11 0) '(cmp r0 r7) '(br ge noconf)
               (L 'ldx 'r14 'r11 'r0 8 tro8)
               '(xor r14 r14 1)
               '(add r0 r0 1) '(st r11 8 r0)
               (L 'ldx 'r3 'r11 'r14 8 wvo8)                      ; seg
               '(leax r3 r13 r3 8 8)                              ; walk base = &wv[seg+1]
               (L 'ldx 'r10 'r11 'r14 8 wvs8)                     ; sz
               '(mov r9 r10) '(sub r9 r9 1)                       ; i = sz-1: BACKWARD (newest first)
               '(label wloop)
               '(cmp r9 0) '(br lt wend)
               '(mov r0 r9) '(add r0 r0 r0)
               '(ldx r8 r3 r0 8 0)                                ; blk
               '(ldx r7 r3 r0 8 8)                                ; c2
               '(mov r0 r8) '(sar r0 1) (L 'ldx 'r0 'r11 'r0 8 vao8)
               '(cmp r0 r8) '(br eq inext)                        ; blocker satisfied: i++, ZERO stores
               '(test r7 1) '(br eq longc)
               '(cmp r0 0) '(br ne bconfl)                        ; BINARY, not sat: free -> unit, else conflict
               '(sar r7 1) '(add r7 r7 1)                         ; unit: assign(blk, cid+1)
               '(mov r0 r8) '(sar r0 1)
               (L 'stx 'r11 'r0 8 vao8 'r8)
               (L 'stx 'r11 'r0 8 rso8 'r7)
               '(ld r7 r11 16) (L 'stx 'r11 'r0 8 lvo8 'r7)
               '(ld r7 r11 0) (L 'stx 'r11 'r7 8 tro8 'r8) '(add r7 r7 1) '(st r11 0 r7)
               '(jmp inext)
               '(label bconfl)
               '(sar r7 1) '(mov r8 r7) '(jmp confl)
               '(label longc)
               '(sar r7 1)                                        ; cid
               '(ldx r0 r12 r7 8 8) '(cmp r0 r14) '(br ne nsw)    ; normalize: f at slot cid+2
               '(ldx r8 r12 r7 8 16) '(stx r12 r7 8 8 r8) '(stx r12 r7 8 16 r0)
               '(label nsw)
               '(ldx r8 r12 r7 8 8)                               ; oth
               '(mov r0 r8) '(sar r0 1) (L 'ldx 'r0 'r11 'r0 8 vao8)
               '(cmp r0 r8) '(br eq fresh)                        ; sat by oth: freshen blocker, i++
               '(st r11 56 r8)                                    ; w7 = oth
               '(ldx r0 r12 r7 8 0) '(add r0 r0 r7) '(add r0 r0 1) '(push r0)   ; [lim]
               '(add r0 r7 3)                                     ; j2
               '(label scan)
               '(ld r8 sp 0) '(cmp r0 r8) '(br ge scfail)
               '(ldx r8 r12 r0 8 0)                               ; e
               '(mov r7 r8) '(sar r7 1) (L 'ldx 'r7 'r11 'r7 8 vao8)
               '(xor r8 r8 1)
               '(cmp r7 r8) '(br ne found)                        ; not falsified
               '(add r0 r0 1) '(jmp scan)
               '(label found)
               '(add sp sp 8)
               '(xor r8 r8 1)                                     ; e (r8); j2 (r0)
               '(push r0)                                         ; [j2]
               (L 'ldx 'r0 'r11 'r8 8 wvs8)                       ; sz2
               (L 'ldx 'r7 'r11 'r8 8 wvo8)                       ; sg2
               '(cmp r7 0) '(br eq growx)
               '(push r7)                                         ; [sg2 j2]
               '(ldx r7 r13 r7 8 0)                               ; cap
               '(cmp r0 r7) '(br ge growy)
               '(pop r7)                                          ; sg2
               '(add r7 r7 1) '(add r7 r7 r0) '(add r7 r7 r0)     ; addr = sg2+1+2sz2
               '(add r0 r0 1) (L 'stx 'r11 'r8 8 wvs8 'r0)        ; commit sz2+1
               '(ld r0 r11 56) '(stx r13 r7 8 0 r0)               ; blocker = oth
               '(mov r0 r9) '(add r0 r0 r0) '(ldx r0 r3 r0 8 8) '(stx r13 r7 8 8 r0)   ; c2
               '(pop r0)                                          ; j2
               '(stx r12 r0 8 0 r14)                              ; ar[j2] = f
               '(mov r7 r9) '(add r7 r7 r7) '(ldx r7 r3 r7 8 8) '(sar r7 1)   ; cid reload
               '(stx r12 r7 8 16 r8)                              ; ar[cid+2] = e
               '(sub r10 r10 1)                                   ; swap-remove: last -> slot i
               '(mov r0 r10) '(add r0 r0 r0)
               '(ldx r7 r3 r0 8 0) '(ldx r8 r3 r0 8 8)
               '(mov r0 r9) '(add r0 r0 r0)
               '(stx r3 r0 8 0 r7) '(stx r3 r0 8 8 r8)
               '(jmp inext)                                       ; last was already visited: i--
               '(label growy)
               '(pop r7)
               '(label growx)
               '(pop r0)                                          ; e (r8) survives
               (L 'stx 'r11 'r14 8 wvs8 'r10)                     ; commit sz
               '(ld r0 r11 8) '(sub r0 r0 1) '(st r11 8 r0)       ; qhead--: f re-propagates
               '(mov r0 r8) '(neg r0) '(sub r0 r0 2) '(jmp ret)   ; -2-e: the grow request
               '(label scfail)
               '(add sp sp 8)
               '(ld r8 r11 56)                                    ; oth
               '(mov r0 r8) '(sar r0 1) (L 'ldx 'r7 'r11 'r0 8 vao8)
               '(cmp r7 0) '(br ne lconfl)
               '(mov r7 r9) '(add r7 r7 r7) '(ldx r7 r3 r7 8 8) '(sar r7 1) '(add r7 r7 1)   ; cid+1
               '(mov r0 r8) '(sar r0 1)
               (L 'stx 'r11 'r0 8 vao8 'r8)                       ; unit: assign(oth, cid+1)
               (L 'stx 'r11 'r0 8 rso8 'r7)
               '(ld r7 r11 16) (L 'stx 'r11 'r0 8 lvo8 'r7)
               '(ld r7 r11 0) (L 'stx 'r11 'r7 8 tro8 'r8) '(add r7 r7 1) '(st r11 0 r7)
               '(jmp fresh)                                       ; freshen blocker to oth
               '(label lconfl)
               '(mov r8 r9) '(add r8 r8 r8) '(ldx r8 r3 r8 8 8) '(sar r8 1)   ; cid
               '(label confl)
               (L 'stx 'r11 'r14 8 wvs8 'r10)                     ; commit sz
               '(mov r0 r8) '(jmp ret)
               '(label fresh)                                     ; store r8 as the blocker at slot i
               '(mov r0 r9) '(add r0 r0 r0) '(stx r3 r0 8 0 r8)
               '(label inext)
               '(sub r9 r9 1) '(jmp wloop)
               '(label wend)
               (L 'stx 'r11 'r14 8 wvs8 'r10)
               '(jmp drain)
               '(label noconf)
               '(li r0 -1)
               '(label ret)
               '(pop r14) '(pop r13) '(pop r12) '(pop r11) '(pop r3))
            (cat epi-ir deopt2-ir)))))))
   ; --- kernel 2: the CONFLICT handler -- (conf fx ar wv confl). ---------------------
   ; registers: r11=fx r12=ar r13=wv (pushed), r3 scratch (pushed), r14 counter/glue
   ; (pushed); r9 write index, r10 trail/read index, r0/r7/r8 scratch; fx w7 (byte 56)
   ; holds the current skip-var (later: the learnt's c2). the twin above is the spec.
   (cls-ir nv seo8)                                         ; clear the seen section (fresh labels)
    (: L1 (mint 0) L2 (mint 0)
       (L '(li r0 0) '(li r8 0) (L 'label L1) (L 'cmp 'r0 nv) (L 'br 'gt L2)
          (L 'stx 'r11 'r0 8 seo8 'r8) '(add r0 r0 1) (L 'jmp L1) (L 'label L2)))
   (abs-ir lvo8 seo8 aco8)                                  ; absorb the clause at r7 (fresh labels);
    (: Ll (mint 0) Lt (mint 0) Ln (mint 0) Le (mint 0)      ; counter r14, write r9, skipv fx w7
       (L '(ldx r0 r12 r7 8 0) '(add r0 r0 r7) '(add r0 r0 1) '(push r0)
          '(add r7 r7 1)
          (L 'label Ll) '(ld r0 sp 0) '(cmp r7 r0) (L 'br 'ge Le)
          '(ldx r3 r12 r7 8 0)
          '(mov r0 r3) '(sar r0 1)
          '(ld r8 r11 56) '(cmp r0 r8) (L 'br 'eq Ln)
          (L 'ldx 'r8 'r11 'r0 8 seo8) '(cmp r8 0) (L 'br 'ne Ln)
          (L 'ldx 'r8 'r11 'r0 8 lvo8) '(cmp r8 0) (L 'br 'eq Ln)
          '(li r8 1) (L 'stx 'r11 'r0 8 seo8 'r8)
          (L 'ldx 'r8 'r11 'r0 8 aco8) '(ld r3 r11 48) '(add r8 r8 r3) (L 'stx 'r11 'r0 8 aco8 'r8)
          (L 'ldx 'r8 'r11 'r0 8 lvo8) '(ld r3 r11 16) '(cmp r8 r3) (L 'br 'ne Lt)
          '(add r14 r14 1) (L 'jmp Ln)
          (L 'label Lt) '(ldx r3 r12 r7 8 0) '(stx r12 r9 8 0 r3) '(add r9 r9 1)
          (L 'label Ln) '(add r7 r7 1) (L 'jmp Ll)
          (L 'label Le) '(add sp sp 8)))
   (try-ir wvo8 wvs8 bit)                                   ; append the learnt watcher (el r7, blk r8, c2
    (: Lp (mint 0) Lpp (mint 0) Ld (mint 0)                 ; in w7) or set the pend bit in r9 (fresh labels)
       (L (L 'ldx 'r0 'r11 'r7 8 wvo8)
          '(cmp r0 0) (L 'br 'eq Lp)
          '(ldx r10 r13 r0 8 0)                             ; cap
          '(push r8)
          (L 'ldx 'r8 'r11 'r7 8 wvs8)                      ; sz
          '(cmp r8 r10) (L 'br 'ge Lpp)
          '(mov r10 r8) '(add r10 r10 r10) '(add r10 r10 r0) '(add r10 r10 1)   ; addr
          '(add r8 r8 1) (L 'stx 'r11 'r7 8 wvs8 'r8)
          '(pop r0) '(stx r13 r10 8 0 r0)                   ; blocker
          '(ld r0 r11 56) '(stx r13 r10 8 8 r0)             ; c2
          (L 'jmp Ld)
          (L 'label Lpp) '(add sp sp 8)
          (L 'label Lp) (L 'or 'r9 'r9 bit)
          (L 'label Ld)))
   (fconf-ir nv)
    (: tro8 64 lvo8 (* 8 (lvo-of nv)) rso8 (* 8 (rso-of nv)) wvo8 (* 8 (wvo-of nv)) wvs8 (* 8 (wvs-of nv))
       vao8 (* 8 (vao-of nv)) aco8 (* 8 (aco-of nv)) pho8 (* 8 (pho-of nv)) seo8 (* 8 (seo-of nv))
       bufk (apof (cask 1))
       (cat (guard-ir 0 bufk) (cat (guard-ir 1 bufk) (cat (guard-ir 2 bufk) (cat (fixg-ir 3)
       (cat '((push r3) (push r11) (push r12) (push r13) (push r14)
              (ld r0 r1 0)  (ld r0 r0 8) (lea r11 r0 16)
              (ld r0 r1 8)  (ld r0 r0 8) (lea r12 r0 16)
              (ld r0 r1 16) (ld r0 r0 8) (lea r13 r0 16)
              (ld r7 r1 24) (sar r7 1)                            ; confl -> r7 (first absorb's clause)
              (ld r0 r11 16) (cmp r0 0) (br eq retm1))            ; level 0 -> -1
       (cat (cls-ir nv seo8)                                      ; phase 1: clear seen
       (cat '((li r14 0)                                          ; counter
              (ld r9 r11 32) (add r9 r9 2)                        ; widx = cid0+2
              (li r0 0) (st r11 56 r0))                           ; skipv = 0
       (cat (abs-ir lvo8 seo8 aco8)                               ; phase 2: absorb the conflict clause
       (cat (L '(ld r10 r11 0) '(sub r10 r10 1)                   ; t = top-1
               '(label walk)
               (L 'ldx 'r0 'r11 'r10 8 tro8)                      ; np = trail[t]
               '(mov r3 r0) '(sar r3 1)
               (L 'ldx 'r8 'r11 'r3 8 seo8) '(cmp r8 0) '(br ne got)
               '(sub r10 r10 1) '(jmp walk)
               '(label got)
               '(li r8 0) (L 'stx 'r11 'r3 8 seo8 'r8)            ; seen[v] = 0
               '(sub r14 r14 1) '(cmp r14 0) '(br eq uip)
               (L 'ldx 'r7 'r11 'r3 8 rso8) '(sub r7 r7 1)        ; cs = rsn[v]-1
               '(st r11 56 r3)                                    ; skipv = v
               '(push r10))
       (cat (abs-ir lvo8 seo8 aco8)                               ; absorb the reason clause
       (cat '((pop r10) (sub r10 r10 1) (jmp walk)
              (label uip)
              (xor r0 r0 1)                                       ; asserting literal
              (ld r7 r11 32) (stx r12 r7 8 8 r0)                  ; ar[cid0+1] = it
              ; phase 3: minimize in place. rd=r10, wr=r9, wend on the stack.
              (push r9)
              (ld r10 r11 32) (add r10 r10 2) (mov r9 r10)
              (label mn) (ld r0 sp 0) (cmp r10 r0) (br ge mnend)
              (ldx r0 r12 r10 8 0)
              (mov r3 r0) (sar r3 1)
              (st r11 56 r3))                                     ; w7 = v
       (cat (L (L 'ldx 'r7 'r11 'r3 8 rso8)
               '(cmp r7 0) '(br eq mkeep)
               '(sub r7 r7 1)
               '(ldx r0 r12 r7 8 0) '(add r0 r0 r7) '(push r0)    ; inner lim (inclusive)
               '(add r7 r7 1)
               '(label mck) '(ld r0 sp 0) '(cmp r7 r0) '(br gt mred)
               '(ldx r0 r12 r7 8 0) '(sar r0 1)
               '(ld r8 r11 56) '(cmp r0 r8) '(br eq mcknx)
               (L 'ldx 'r8 'r11 'r0 8 lvo8) '(cmp r8 0) '(br eq mcknx)
               (L 'ldx 'r8 'r11 'r0 8 seo8) '(cmp r8 0) '(br ne mcknx)
               '(add sp sp 8) '(jmp mkeep)                        ; a fresh lit -> NOT redundant
               '(label mcknx) '(add r7 r7 1) '(jmp mck)
               '(label mred) '(add sp sp 8) '(jmp mnnext)
               '(label mkeep) '(ldx r0 r12 r10 8 0) '(stx r12 r9 8 0 r0) '(add r9 r9 1)
               '(label mnnext) '(add r10 r10 1) '(jmp mn)
               '(label mnend) '(add sp sp 8))                     ; r9 = wend after compaction
       (cat (cls-ir nv seo8)                                      ; phase 4: clear seen (-> level marks)
       (cat (L '(ld r0 r11 16)                                    ; mark dl
               '(li r8 1) (L 'stx 'r11 'r0 8 seo8 'r8)
               '(li r14 1) '(li r10 0)                            ; glue=1, bj=0
               '(ld r7 r11 32) '(add r7 r7 2)
               '(label gb) '(cmp r7 r9) '(br ge gbe)
               '(ldx r0 r12 r7 8 0) '(sar r0 1)
               (L 'ldx 'r0 'r11 'r0 8 lvo8)                       ; L
               '(cmp r10 r0) '(br ge gb1) '(mov r10 r0)
               '(label gb1)
               (L 'ldx 'r8 'r11 'r0 8 seo8) '(cmp r8 0) '(br ne gb2)
               '(li r8 1) (L 'stx 'r11 'r0 8 seo8 'r8) '(add r14 r14 1)
               '(label gb2) '(add r7 r7 1) '(jmp gb)
               '(label gbe)
               ; phase 5: backjump to bj (r10)
               '(ld r7 r11 0)
               '(label bjl) '(cmp r7 0) '(br eq bje)
               '(mov r8 r7) '(sub r8 r8 1)
               (L 'ldx 'r0 'r11 'r8 8 tro8)                       ; el = trail[t-1]
               '(mov r3 r0) '(sar r3 1)
               (L 'ldx 'r8 'r11 'r3 8 lvo8) '(cmp r8 r10) '(br le bje)
               '(and r0 r0 1) '(add r0 r0 1) (L 'stx 'r11 'r3 8 pho8 'r0)
               '(li r0 0) (L 'stx 'r11 'r3 8 vao8 'r0) (L 'stx 'r11 'r3 8 lvo8 'r0) (L 'stx 'r11 'r3 8 rso8 'r0)
               '(sub r7 r7 1) '(jmp bjl)
               '(label bje) '(st r11 0 r7) '(st r11 8 r7) '(st r11 16 r10)
               ; phase 6: size, aru, reorder, watches
               '(ld r3 r11 32)                                    ; cid0
               '(mov r0 r9) '(sub r0 r0 r3) '(sub r0 r0 1)        ; size
               '(stx r12 r3 8 0 r0) '(st r11 32 r9)               ; ar[cid0]=size; aru=wend
               '(cmp r0 2) '(br lt small)
               '(mov r10 r3) '(add r10 r10 2)                     ; reorder: argmax level
               '(ldx r0 r12 r10 8 0) '(sar r0 1) (L 'ldx 'r8 'r11 'r0 8 lvo8)
               '(mov r7 r10) '(add r7 r7 1)
               '(label ro) '(cmp r7 r9) '(br ge roe)
               '(ldx r0 r12 r7 8 0) '(sar r0 1) (L 'ldx 'r0 'r11 'r0 8 lvo8)
               '(cmp r0 r8) '(br le ro2) '(mov r8 r0) '(mov r10 r7)
               '(label ro2) '(add r7 r7 1) '(jmp ro)
               '(label roe)
               '(ldx r0 r12 r3 8 16) '(ldx r7 r12 r10 8 0) '(stx r12 r3 8 16 r7) '(stx r12 r10 8 0 r0)
               '(li r9 0)                                         ; pend bits
               '(mov r7 r3) '(add r7 r7 r7)                       ; c2 = 2*cid0 (+1 if binary)
               '(ldx r0 r12 r3 8 0) '(cmp r0 2) '(br ne c2n) '(add r7 r7 1)
               '(label c2n) '(st r11 56 r7)                       ; w7 = c2
               '(ldx r7 r12 r3 8 8) '(ldx r8 r12 r3 8 16))        ; el=l0, blk=l1
       (cat (try-ir wvo8 wvs8 1)
       (cat (L '(ldx r7 r12 r3 8 16) '(ldx r8 r12 r3 8 8))        ; el=l1, blk=l0
       (cat (try-ir wvo8 wvs8 2)
       (cat (L '(jmp assert)
               '(label small) '(li r9 0)
               '(label assert)                                    ; phase 7: assign learnt[0], reason cid0+1
               '(ldx r8 r12 r3 8 8) '(mov r0 r8) '(sar r0 1)
               (L 'stx 'r11 'r0 8 vao8 'r8)
               '(ld r7 r11 16) (L 'stx 'r11 'r0 8 lvo8 'r7)
               '(mov r7 r3) '(add r7 r7 1) (L 'stx 'r11 'r0 8 rso8 'r7)
               '(ld r7 r11 0) (L 'stx 'r11 'r7 8 tro8 'r8) '(add r7 r7 1) '(st r11 0 r7)
               '(mov r0 r3) '(shl r0 8)                           ; return (cid0<<8)|(pend<<6)|min(glue,63)
               '(cmp r14 63) '(br le pk) '(li r14 63)
               '(label pk) '(or r0 r0 r14) '(shl r9 6) '(or r0 r0 r9) '(jmp fin)
               '(label retm1) '(li r0 -1)
               '(label fin)
               '(pop r14) '(pop r13) '(pop r12) '(pop r11) '(pop r3))
            (cat epi-ir deopt2-ir))))))))))))))))))))
   ; --- kernel 3: DECIDE -- (dec fx). caller-saved regs only, no pushes. -------------
   (fdec-ir nv)
    (: tro8 64 lvo8 (* 8 (lvo-of nv)) rso8 (* 8 (rso-of nv)) vao8 (* 8 (vao-of nv))
       aco8 (* 8 (aco-of nv)) pho8 (* 8 (pho-of nv)) bufk (apof (cask 1))
       (cat (guard-ir 0 bufk)
       (cat '((ld r9 r1 0) (ld r9 r9 8) (lea r9 r9 16)
              (li r7 0) (li r8 -1) (li r0 1))
       (cat (L '(label dl) (L 'cmp 'r0 nv) '(br gt dle)
               (L 'ldx 'r3 'r9 'r0 8 vao8) '(cmp r3 0) '(br ne dn)
               (L 'ldx 'r3 'r9 'r0 8 aco8)
               '(cmp r7 0) '(br eq take)
               '(cmp r3 r8) '(br gt take) '(jmp dn)
               '(label take) '(mov r7 r0) '(mov r8 r3)
               '(label dn) '(add r0 r0 1) '(jmp dl)
               '(label dle) '(cmp r7 0) '(br eq ret0)
               '(ld r0 r9 16) '(add r0 r0 1) '(st r9 16 r0)       ; level++
               (L 'ldx 'r3 'r9 'r7 8 pho8)
               '(mov r0 r7) '(add r0 r0 r0)                       ; el = 2v (+1 unless phase 1)
               '(cmp r3 1) '(br eq pos) '(add r0 r0 1)
               '(label pos)
               (L 'stx 'r9 'r7 8 vao8 'r0)                        ; assign(el, 0)
               '(ld r3 r9 16) (L 'stx 'r9 'r7 8 lvo8 'r3)
               '(li r3 0) (L 'stx 'r9 'r7 8 rso8 'r3)
               '(ld r3 r9 0) (L 'stx 'r9 'r3 8 tro8 'r0) '(add r3 r3 1) '(st r9 0 r3)
               '(mov r0 r7) '(jmp dfin)
               '(label ret0) '(li r0 0)
               '(label dfin))
            (cat epi-ir deopt1-ir)))))
   fkers (tablet 0)                                         ; nv -> (bcp conf dec), one assemble per nvars
   fknob #0                                                 ; pin 1 to force the interpreted twins (the differential switch)
   fmaxl0 #3000                                             ; the initial learnt-DB cap (the gate lowers it to force reductions)
   (fnif ir fb ar)                                          ; assemble + install one kernel (fb = the honest twin)
    (: bs (peep (assemble 'x64 ir) 'bytes 0)
       c (cask (tally bs))
       _ (foldl (\ i b (: _ (pin c i b) (+ i 1))) 0 bs)
       (nif c fb '(\ q (q)) ar))
   (fbcpk nv)                                               ; -> (bcp conf dec), or () when they can't build
    (? (! (&& (lit? nif) (id? (intern love-arch) 'x64))) ()
       (: hit (peep fkers nv ())
          (? (two? hit) hit
             (: ks (L (fnif (fbcp-ir nv) fbcp3 3)
                      (fnif (fconf-ir nv) fconf3 4)
                      (fnif (fdec-ir nv) fdec3 1))
                _ (pin fkers nv ks) ks))))
   (fmk formula nvars)                                      ; build the flat solver state
    (: s (tablet 0) nv nvars
       fx (cask (* 8 (+ (seo-of nv) (+ nv 1))))
       _ (putw fx 3 nv) _ (putw fx 4 2) _ (putw fx 5 1) _ (putw fx 6 256)   ; arena word 0-1 / wv word 0 reserved
       _ (pin s 'nvars nv) _ (pin s 'fx fx)
       _ (pin s 'lvo (lvo-of nv)) _ (pin s 'rso (rso-of nv)) _ (pin s 'vao (vao-of nv))
       _ (pin s 'wvo (wvo-of nv)) _ (pin s 'wvs (wvs-of nv))
       _ (pin s 'ar (cask 4096)) _ (pin s 'wv (cask 4096))
       _ (pin s 'bad 0)
       _ (pin s 'nconf 0) _ (pin s 'rnum 1) _ (pin s 'climit 100)
       _ (pin s 'learnts ()) _ (pin s 'maxl (peep fmaxl0 0 2000))
       ks (fbcpk nv)
       use (&& (two? ks) (= 0 (peep fknob 0 0)))
       _ (pin s 'bcp  (? use (cap ks) fbcp3))
       _ (pin s 'conf (? use (caup ks) fconf3))
       _ (pin s 'dec  (? use (caup (cup ks)) fdec3))
       _ (foldl (\ _ c (fload s c)) 0 formula) s)
   ; --- the cold path: restarts, decay, learnt-DB reduction (lisp side) -----------
   (fbackjump s lvl)                                        ; pop + unassign above lvl (restarts: lvl = 0)
    (: fx (peep s 'fx ()) nv (peep s 'nvars 0) lvo (peep s 'lvo 0) rso (peep s 'rso 0)
       vao (peep s 'vao 0) pho (pho-of nv)
       nt ((: (go t)
              (? (= t 0) 0
                 (: el (getw fx (+ 8 (- t 1))) v (>> el 1)
                    (? (> (getw fx (+ lvo v)) lvl)
                       (: _ (putw fx (+ pho v) (+ 1 (& el 1)))
                          _ (putw fx (+ vao v) 0) _ (putw fx (+ lvo v) 0) _ (putw fx (+ rso v) 0)
                          (go (- t 1)))
                       t))))
           (getw fx 0))
       _ (putw fx 0 nt) _ (putw fx 1 nt) (putw fx 2 lvl))
   (fdecay s)                                               ; integer VSIDS: vinc *= 21/20; rescale by >>8
    (: fx (peep s 'fx ()) vi (// (* (getw fx 6) 21) 20)
       (? (> vi 16777216)
          (: nv (peep s 'nvars 0) aco (aco-of nv)
             _ ((: (go v) (? (> v nv) 0 (: _ (putw fx (+ aco v) (>> (getw fx (+ aco v)) 8)) (go (+ v 1))))) 1)
             (putw fx 6 (>> vi 8)))
          (putw fx 6 vi)))
   (fnext-restart s) (: rn (+ 1 (peep s 'rnum 1)) _ (pin s 'rnum rn) _ (pin s 'nconf 0) _ (pin s 'climit (* (luby rn) 100)) 0)
   ; learnt-DB REDUCTION at a level-0 restart (no reason cid is live: absorb never
   ; reads a level-0 var's reason, and those are zeroed here anyway). the worst-glue
   ; half dies (glue <= 3 immortal), survivors compact by pour, watches re-established
   ; vs the level-0 assignment; a level-0-satisfied clause drops, a single-nonfalse
   ; one enqueues as a permanent fact; all-false cannot arise (propagation was drained).
   (fswap c i j) (? (= i j) 0 (: a (getw c i) b (getw c j) _ (putw c i b) (putw c j a)))
   (freduce s)
    (: fx (peep s 'fx ()) nv (peep s 'nvars 0) wvo (peep s 'wvo 0) wvs (peep s 'wvs 0)
       rso (peep s 'rso 0) ar (peep s 'ar ())
       ls (peep s 'learnts ())
       srt (sortby (\ a b (<= (cup a) (cup b))) (filter (\ p (> (cup p) 3)) ls))
       nk (// (tally srt) 2)
       dead (tablet 0)
       _ (foldl (\ i p (: _ (? (>= i nk) (pin dead (cap p) 1) 0) (+ i 1))) 0 srt)
       lt (tablet 0) _ (foldl (\ _ p (pin lt (cap p) (cup p))) 0 ls)
       _ ((: (zw i) (? (> i (+ 1 (* 2 nv))) 0                       ; drop every vector (wv compacts free)
              (: _ (putw fx (+ wvo i) 0) _ (putw fx (+ wvs i) 0) (zw (+ i 1))))) 0)
       _ (putw fx 5 1)
       _ ((: (zr t) (? (>= t (getw fx 0)) 0
              (: v (>> (getw fx (+ 8 t)) 1) _ (putw fx (+ rso v) 0) (zr (+ t 1))))) 0)
       na (cask (tally ar)) _ (pin s 'ar na)                ; survivors compact into na (fwatch sees it)
       end (getw fx 4)
       res ((: (walk cid nu nls)
               (? (>= cid end) (link nu nls)
                  (: n (getw ar cid) nxt (+ cid (+ n 1))
                     lits ((: (go j acc) (? (< j (+ cid 1)) acc (go (- j 1) (link (getw ar j) acc)))) (+ cid n) ())
                     (? (! (= 0 (peep dead cid 0))) (walk nxt nu nls)
                        (< n 2) (walk nxt nu nls)
                        (any (\ el (= 1 (flv s el))) lits) (walk nxt nu nls)
                        (: nf (filter (\ el (! (= -1 (flv s el)))) lits)
                           (? (atom? nf) (scare 'freduce-allfalse cid)
                              (atom? (cup nf)) (: _ (fenq s (cap nf) 0) (walk nxt nu nls))
                              (: _ (pour na (* 8 nu) ar (* 8 cid) (* 8 (+ n 1)))
                                 l1 (cap nf) l2 (caup nf)
                                 p1 ((: (fnd j) (? (= (getw na j) l1) j (fnd (+ j 1)))) (+ nu 1))
                                 _ (fswap na (+ nu 1) p1)
                                 p2 ((: (fnd j) (? (= (getw na j) l2) j (fnd (+ j 1)))) (+ nu 2))
                                 _ (fswap na (+ nu 2) p2)
                                 g (peep lt cid ())
                                 (walk nxt (+ nu (+ n 1))
                                       (? (whole? g) (link (link nu g) nls) nls)))))))))
            2 2 ())
       nu (cap res)
       ; rebuild the watcher vectors EXACTLY (no doubling chains, no post-reduce grow
       ; storms): count per literal into the zeroed WVS spine, size wv in one fens,
       ; hand out exact segments (+2 headroom), then fill -- counters become sizes.
       _ ((: (cnt cid) (? (>= cid nu) 0
              (: n (getw na cid) l1 (getw na (+ cid 1)) l2 (getw na (+ cid 2))
                 _ (putw fx (+ wvs l1) (+ 1 (getw fx (+ wvs l1))))
                 _ (putw fx (+ wvs l2) (+ 1 (getw fx (+ wvs l2))))
                 (cnt (+ cid (+ n 1)))))) 2)
       tot ((: (go el t) (? (> el (+ 1 (* 2 nv))) t
                 (: c (getw fx (+ wvs el)) (go (+ el 1) (? (= c 0) t (+ t (+ 5 (* 2 c)))))))) 0 1)
       _ (fens s 'wv (* 8 (+ tot 8)))
       wv (peep s 'wv ())
       _ ((: (go el u) (? (> el (+ 1 (* 2 nv))) (putw fx 5 u)
              (: c (getw fx (+ wvs el))
                 (? (= c 0) (: _ (putw fx (+ wvo el) 0) (go (+ el 1) u))
                    (: _ (putw wv u (+ c 2)) _ (putw fx (+ wvo el) u)
                       _ (putw fx (+ wvs el) 0)
                       (go (+ el 1) (+ u (+ 5 (* 2 c))))))))) 0 1)
       _ ((: (fill cid) (? (>= cid nu) 0
              (: n (getw na cid) l1 (getw na (+ cid 1)) l2 (getw na (+ cid 2))
                 c2 (+ (* 2 cid) (? (= n 2) 1 0))
                 put (\ el blk (: sg (getw fx (+ wvo el)) sz (getw fx (+ wvs el))
                         _ (putw wv (+ sg (+ 1 (* 2 sz))) blk)
                         _ (putw wv (+ sg (+ 2 (* 2 sz))) c2)
                         (putw fx (+ wvs el) (+ sz 1))))
                 _ (put l1 l2) _ (put l2 l1)
                 (fill (+ cid (+ n 1)))))) 2)
       _ (putw fx 4 nu) _ (pin s 'learnts (cup res))
       (pin s 'maxl (// (* (peep s 'maxl 2000) 3) 2)))
   ; --- the search loop: three kernel calls + lisp bookkeeping --------------------
   (fsearch s)                                              ; -> 'sat or 'unsat (s holds the model on SAT)
    (: fx (peep s 'fx ()) nv (peep s 'nvars 0)
       ((: (loop _)
           (: confl ((peep s 'bcp ()) fx (peep s 'ar ()) (peep s 'wv ()))
              (? (> confl -1)
                 (: _ (fens s 'ar (* 8 (+ (getw fx 4) (+ nv 3))))   ; room for the learnt clause
                    r ((peep s 'conf ()) fx (peep s 'ar ()) (peep s 'wv ()) confl)
                    (? (= r -1) 'unsat
                       (: cid (>> r 8) glue (& r 63) pend (& (>> r 6) 3)
                          _ (? (= 0 pend) 0                         ; a full vector: lisp appends (rare)
                               (: arr (peep s 'ar ()) l0 (getw arr (+ cid 1)) l1 (getw arr (+ cid 2))
                                  _ (? (= 0 (& pend 1)) 0 (fwatch s cid l0 l1))
                                  (? (= 0 (& pend 2)) 0 (fwatch s cid l1 l0))))
                          _ (? (< 1 (getw (peep s 'ar ()) cid))     ; size >= 2: track for reduction
                               (pin s 'learnts (link (link cid glue) (peep s 'learnts ()))) 0)
                          _ (fdecay s) _ (pin s 'nconf (+ 1 (peep s 'nconf 0)))
                          (loop ()))))
                 (< confl -1)                                       ; a vector filled mid-slide: grow, re-propagate
                    (: _ (fseg-grow s (- 0 (+ confl 2))) (loop ()))
                 (&& (> (getw fx 2) 0) (>= (peep s 'nconf 0) (peep s 'climit 0)))
                    (: _ (fbackjump s 0) _ (fnext-restart s)
                       _ (? (> (tally (peep s 'learnts ())) (peep s 'maxl 2000)) (freduce s) 0)
                       (loop ()))
                 (: v ((peep s 'dec ()) fx)
                    (? (= v 0) 'sat (loop ())))))) ()))
   (fextract s nvars) (: fx (peep s 'fx ()) vao (peep s 'vao 0)
                         (map (\ v (link v (? (= 0 (& (getw fx (+ vao v)) 1)) 1 -1))) (rng 1 (+ nvars 1))))
   (fcdcl formula nvars)
    (: s (fmk formula nvars)
       (? (= 1 (peep s 'bad 0)) 'unsat
          (: r (fsearch s) (? (id? r 'unsat) 'unsat (fextract s nvars)))))
   (count-fcdcl formula nvars)                              ; #SAT by blocking clauses, on the flat engine
    ((: (go fm c) (: r (fcdcl fm nvars) (? (id? r 'unsat) c (go (link (block r) fm) (+ c 1))))) formula 0)
   (fdiff seed n m)                                         ; flat CDCL == the DPLL oracle (and models check)
    (: f (gen-formula seed n m) d (solve f n) c (fcdcl f n)
       dsat (! (id? d 'unsat)) csat (! (id? c 'unsat))
       (? (! (= dsat csat)) 0  (! csat) 1  (sat? c f)))
   (ffuzz k n m) (all (\ i (= 1 (fdiff (+ 1009 (* i 7919)) n m))) (jot k))
   (fboth f n)                                              ; kernels and twins agree, verdict for verdict
    (: _ (pin fknob 0 1) a (fcdcl f n) _ (pin fknob 0 0) b (fcdcl f n)
       (? (id? a 'unsat) (id? b 'unsat) (&& (! (id? b 'unsat)) (sat? b f))))

 ; --- the gate: the flat engine answers exactly like the oracle, kernels == twins
 _ (assert
    ; tiny SAT / UNSAT
    (: f '((1 2) (-1 3) (-2 -3)) m (fcdcl f 3) (&& (! (id? m 'unsat)) (sat? m f)))
    (id? (fcdcl '((1) (-1)) 1) 'unsat)
    (id? (fcdcl '((1 2) (1 -2) (-1 2) (-1 -2)) 2) 'unsat)
    (id? (fcdcl '((1 2 3) (1 2 -3) (1 -2 3) (1 -2 -3) (-1)) 3) 'unsat)
    ; cyclic: SAT, the model checks, and #SAT == Lucas (solver-independent)
    (: f (cyclic 12) m (fcdcl f 12) (&& (! (id? m 'unsat)) (sat? m f)))
    (= (count-fcdcl (cyclic 8) 8) (lucas 8))
    (= (count-fcdcl (cyclic 10) 10) (lucas 10))
    ; differential fuzz vs the DPLL oracle, mixed SAT/UNSAT near the phase transition
    (ffuzz 60 8 30)
    (ffuzz 40 10 30)
    (ffuzz 30 12 50)
    (ffuzz 20 14 58)
    ; native kernels == interpreted twins, verdict for verdict (a no-op switch where they can't build)
    (fboth (cyclic 14) 14)
    (fboth (php 4) (php-vars 4))
    (fboth (php 5) (php-vars 5))
    ; pigeonhole PHP(5): UNSAT + resolution-hard (learning, restarts, arena growth end to end)
    (id? (fcdcl (php 5) (php-vars 5)) 'unsat)
    ; force the learnt-DB REDUCTION path hard (a tiny cap -> compaction every few restarts)
    ; and re-run the differential battery through it, both engines
    (: _ (pin fmaxl0 0 24)
       ok (&& (id? (fcdcl (php 5) (php-vars 5)) 'unsat)
              (&& (ffuzz 30 12 50) (fboth (php 4) (php-vars 4))))
       _ (pin fmaxl0 0 2000) ok))
 _ (say err (+ "crew/sat/flat: ok -- flat-arena CDCL == DPLL on 150 fuzzed + PHP(5) + #SAT==Lucas"
               (? (two? (fbcpk 8)) " (native bcp+conf+dec kernels live)\n" " (interpreted twins)\n"))))
