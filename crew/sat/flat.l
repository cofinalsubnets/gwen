; crew/sat/flat -- the CDCL solver over FLAT state, with FOUR DOMAIN-SPECIFIC NATIVE
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
;   fbvag (bx ba bo bp)    one BVA grow step over the flat intake state (its own
;                          arena family; the layout comment sits above fbvag3)
;                                                              -> best key | count = M_cls
; the interpreted twins (fbcp3/fconf3/fdec3/fbvag3) over the SAME memory are the deopt
; fallbacks, the arm64/no-`nif` engine (the seam rides `(from 'glaze 'nif)` -- main.c's
; seal pulls nif off the book, and the glaze module is the door it keeps), and the
; differential oracle: the `fknob` box forces the twins and `fboth` gates verdict
; equality. lisp keeps only the cold path: restarts, learnt-DB reduction, decay/rescale.
; Run AFTER sat.l (it reuses the leaked oracle + instances):
;   cat crew/sat/sat.l crew/sat/flat.l | love
; the story, the kernel contract, and the measured results:.
;
; a literal is ENCODED: +v -> 2v, -v -> 2v+1 (neg = xor 1, var = >>1). val[v] holds
; the TRUE encoded literal itself (0 = free) so lit-val is a load + a compare. VSIDS
; activities are INTEGERS (vinc starts 256, grows *21//20 per conflict; when it tops
; 2^24 every activity shifts right 8 -- decay-by-shift, ratios kept, ancient bumps
; fade to 0, which is the point). words are 8-byte slots whose LOW 4 bytes carry the
; value (getw/putw touch only those; the high bytes stay zero from the zeroed cask,
; so the kernels' 64-bit ld/st and the byte accessors agree exactly).
; fx layout (word offsets; nv = nvars):
;   w0 top  w1 qhead  w2 level  w3 nv  w4 aru  w5 wnu  w6 vinc  w7 kernel scratch
;   TRO=8 trail | LVO lvl | RSO rsn (0 none, else cid+1) | WHO watch heads (2nv+2,
;   by encoded lit) | VAO val | ACO activity | PHO saved phase (0 never, 1 pos, 2 neg)
;   | SEO seen scratch (conf clears it on entry; also reused as a level-mark set)
; a clause at arena offset cid is [size, lit0..], watches ALWAYS slots cid+1/cid+2;
; a watch node at offset p is [cid, next] (0 = nil; words 0-1 of both arenas reserved).
; the conf caller pre-ensures arena room (aru+nv+3 words) and node room (wnu+4): the
; kernels never grow a cask.
(:
   ; --- 8-byte word slots, low 4 bytes live: the host's word nifs (src/mem.c,
   ; module 'mem -- (from 'mem) answers () off a host, so presence is an ordinary
   ; read), the byte path as the portable twin -- picked once at bind time --------
   memw (from 'mem)
   getw (? (lit? memw) (memw 'peepw)
           (\ c i (: b (8 * i) (peep c b 0 | ((peep c (b + 1) 0 << 8) | ((peep c (b + 2) 0 << 16) | (peep c (b + 3) 0 << 24)))))))
   putw (? (lit? memw) (memw 'pinw)
           (\ c i v (: b (8 * i)
                       _ (pin c b (v & 255)) _ (pin c (b + 1) ((v >> 8) & 255))
                       _ (pin c (b + 2) ((v >> 16) & 255)) (pin c (b + 3) ((v >> 24) & 255)))))
   (enc l) (? (l < 0) (1 + (2 * (0 - l))) (2 * l))          ; signed literal -> encoded
   ; --- the layout, from nv (twins recompute these from fx w3) --------------------
   (lvo-of nv) (8 + (nv + 1))
   (rso-of nv) (lvo-of nv + (nv + 1))
   (who-of nv) (rso-of nv + (nv + 1))
   (vao-of nv) (who-of nv + (2 + (2 * nv)))
   (aco-of nv) (vao-of nv + (nv + 1))
   (pho-of nv) (aco-of nv + (nv + 1))
   (seo-of nv) (pho-of nv + (nv + 1))
   ; --- the twin assign, self-contained (offsets from fx w3) ----------------------
   (fasg fx el rv)                                          ; set el TRUE at the current level, reason rv (0 = decision)
    (: nv (getw fx 3) v (el >> 1) t (getw fx 0)
       _ (putw fx (vao-of nv + v) el)
       _ (putw fx (lvo-of nv + v) (getw fx 2))
       _ (putw fx (rso-of nv + v) rv)
       _ (putw fx (8 + t) el)
       (putw fx 0 (t + 1)))
   ; --- state access for the lisp driver (offsets cached in s) --------------------
   (flv s el) (: m (getw (peep s 'fx ()) (peep s 'vao 0 + (el >> 1)))
                 (? (m = 0) 0 (m = el) 1 -1))               ; 1 sat / -1 false / 0 free
   (flvl s el) (getw (peep s 'fx ()) (peep s 'lvo 0 + (el >> 1)))
   (fens s k need)                                          ; ensure cask s[k] holds >= need bytes (grow by doubling)
    (: c (peep s k ())
       (? (need <= tally c) 0
          (: n2 ((: (go m) (? (m >= need) m (go (2 * m)))) (2 * tally c))
             nu (cask n2) _ (pour nu 0 c 0 (tally c)) _ (pin s k nu) 0)))
   (fassign s el rv) (fasg (peep s 'fx ()) el rv)
   ; --- clause DB (lisp side: initial load + reduction; aru/wnu live in fx) --------
   (fadd s lits)                                            ; append [size, lits..] to the arena; -> cid (word offset)
    (: n #lits fx (s 'fx) aru (getw fx 4)
       _ (fens s 'ar (8 * (aru + (n + 1))))
       ar (s 'ar)
       _ (putw ar aru n)
       _ (foldl (\ j el (: _ (putw ar j el) (j + 1))) (aru + 1) lits)
       _ (putw fx 4 (aru + (n + 1))) aru)
   (fclits s cid)                                           ; the clause's encoded literals, as a list
    (: ar (s 'ar) n (getw ar cid)
       ((: (go j acc) (j < cid + 1 ? acc (go (j - 1) (getw ar j >< acc)))) (cid + n) ()))
   (fwatch s cid el)                                        ; push a watch node [cid -> wh[el] list]
    (: fx (s 'fx) wnu (getw fx 5) _ (fens s 'wn (8 * (wnu + 2)))
       wn (s 'wn) who (peep s 'who 0)
       _ (putw wn wnu cid) _ (putw wn (wnu + 1) (getw fx (who + el)))
       _ (putw fx (who + el) wnu) (putw fx 5 (wnu + 2)))
   (fenq s el rv)                                           ; level-0 fact: assign if free, flag bad if already false
    (: m (flv s el) (? (m = 1) 0 (m = -1) (pin s 'bad 1) (fassign s el rv, 0)))
   (fload s c0)                                             ; a source clause (signed lits) into the DB + watches
    (? (1 = taut? c0) 0
       (: c (dedup-lits c0)
          (? (atom? c) (pin s 'bad 1)
             (atom? >c) (: cid (fadd s (enc <c >< ())) (fenq s (enc <c) (cid + 1)))
             (: cid (fadd s (map enc c))
                _ (fwatch s cid (enc <c)) (fwatch s cid (enc <>c))))))
   ; --- the interpreted BCP twin: (fbcp3 fx ar wn) -> conflict cid or -1 ----------
   (fbcp3 fx ar wn)
    (: nv (getw fx 3) who (who-of nv) vao (vao-of nv)
       lv (\ el (: m (getw fx (vao + (el >> 1))) (? (m = 0) 0 (m = el) 1 -1)))
       prop1 (\ f                                           ; f just became FALSE: re-examine wh[f]
        ((: (go p prev)
            (? (p = 0) -1
               (: cid (getw wn p)
                  (? (0 = getw ar cid)                                   ; TOMBSTONE (freduce): unlink, walk on
                     (: nx (getw wn (p + 1))
                        _ (? (prev = 0) (putw fx (who + f) nx) (putw wn (prev + 1) nx))
                        (go nx prev))
                  (: _ (? (getw ar (cid + 1) = f)                        ; normalize: f sits at slot cid+2
                       (: t (getw ar (cid + 2)) _ (putw ar (cid + 2) f) (putw ar (cid + 1) t)) 0)
                  oth (getw ar (cid + 1))
                  (? (1 = lv oth) (go (getw wn (p + 1)) p)               ; satisfied by the other watch: keep
                     (: k ((: (sc j)                                     ; scan the tail for a non-false literal
                              (? (j >= (cid + (1 + getw ar cid))) 0
                                 (: e (getw ar j) (? !(-1 = lv e) j (sc (j + 1))))))
                           (cid + 3))
                        (? !(k = 0)                                   ; slide the watch f -> ar[k]
                           (: e (getw ar k) nx (getw wn (p + 1))
                              _ (putw ar k f) _ (putw ar (cid + 2) e)
                              _ (? (prev = 0) (putw fx (who + f) nx) (putw wn (prev + 1) nx))
                              _ (putw wn (p + 1) (getw fx (who + e))) _ (putw fx (who + e) p)
                              (go nx prev))
                           (0 = lv oth) (: _ (fasg fx oth (cid + 1)) (go (getw wn (p + 1)) p))     ; unit: force oth
                           cid))))))))                                   ; oth false too: CONFLICT
         (getw fx (who + f)) 0))
       ((: (drain _)
           (? (getw fx 1 >= getw fx 0) -1
              (: p (getw fx (8 + getw fx 1)) _ (putw fx 1 (1 + getw fx 1))
                 confl (prop1 (p ^ 1))
                 (? (confl > -1) confl (drain ()))))) ()))
   ; --- the interpreted CONFLICT twin: (fconf3 fx ar wn confl) --------------------
   ; the whole conflict handler, phase for phase what the conf kernel does:
   ;   0 level 0 -> -1        1 clear seen           2 1-UIP (absorb + trail walk),
   ;   asserting lit to ar[cid0+1], tail collected at ar[cid0+2..]
   ;   3 minimize (self-subsumption; dropped lits STAY seen), compact in place
   ;   3b reason-side bump: unseen vars in the learnt lits' reasons get act += vinc
   ;   4 clear seen; glue (level-mark set) + backjump level in one pass
   ;   5 backjump (save phase, unassign)   6 commit size/aru, reorder watch-2, watches
   ;   7 assert learnt[0] with reason cid0+1   -> (cid0<<6)|min(glue,63)
   (fconf3 fx ar wn confl)
    (: nv (getw fx 3) lvo (lvo-of nv) rso (rso-of nv) who (who-of nv)
       vao (vao-of nv) aco (aco-of nv) pho (pho-of nv) seo (seo-of nv)
       dl (getw fx 2)
       (? (dl = 0) -1
        (: cid0 (getw fx 4)
           czero (\ _ ((: (go v) (? (v > nv) 0 (: _ (putw fx (seo + v) 0) (go (v + 1))))) 0))
           _ (czero ())
           abs1 (\ cs skipv counter widx                    ; absorb clause at cs -> (counter . widx)
            ((: (go j c w)
                (? (j > (cs + getw ar cs)) (c >< w)
                   (: el (getw ar j) v (el >> 1)
                      (? (v = skipv) (go (j + 1) c w)
                         !(0 = getw fx (seo + v)) (go (j + 1) c w)
                         (0 = getw fx (lvo + v)) (go (j + 1) c w)
                         (: _ (putw fx (seo + v) 1)
                            _ (putw fx (aco + v) (getw fx (aco + v) + getw fx 6))       ; bump
                            (? (getw fx (lvo + v) = dl) (go (j + 1) (c + 1) w)
                               (: _ (putw ar w el) (go (j + 1) c (w + 1)))))))))
             (cs + 1) counter widx))
           a0 (abs1 confl 0 0 (cid0 + 2))
           r ((: (walk counter widx t)
                 (: np ((: (fs i) (? (0 = getw fx (seo + (getw fx (8 + i) >> 1))) (fs (i - 1)) i)) t)
                    el (getw fx (8 + np)) v (el >> 1)
                    _ (putw fx (seo + v) 0) c (counter - 1)
                    (? (c = 0) (el >< widx)
                       (: a (abs1 (getw fx (rso + v) - 1) v c widx)
                          (walk <a >a (np - 1))))))
              <a0 >a0 (getw fx 0 - 1))
           uip <r wend >r
           _ (putw ar (cid0 + 1) (uip ^ 1))
           wr ((: (mn rd wr)                                ; minimize, compacting kept lits down
                  (? (rd >= wend) wr
                     (: el (getw ar rd) v (el >> 1) rv (getw fx (rso + v))
                        red (? (rv = 0) 0
                               ((: (ck j) (? (j > ((rv - 1) + getw ar (rv - 1))) 1
                                             (: v2 (getw ar j >> 1)
                                                (? (v2 = v) (ck (j + 1))
                                                   (0 = getw fx (lvo + v2)) (ck (j + 1))
                                                   !(0 = getw fx (seo + v2)) (ck (j + 1))
                                                   0)))) (rv + 0)))     ; j starts at (rv-1)+1 = rv
                        (? (red = 1) (mn (rd + 1) wr)
                           (: _ (putw ar wr el) (mn (rd + 1) (wr + 1)))))))
               (cid0 + 2) (cid0 + 2))
           ; reason-side bump (cadical's bumpreason): every UNSEEN var in a learnt
           ; literal's reason gets an activity bump -- focuses VSIDS one step past the
           ; learnt, the decisive lever on factored/tree instances (php8 ~4000 -> ~1500
           ; conflicts). seen still holds the analysis marks here (czero follows).
           _ ((: (rb j) (? (j >= wr) 0
                  (: v (getw ar j >> 1) rv (getw fx (rso + v))
                     _ (? (rv = 0) 0
                          ((: (go2 k) (? (k > ((rv - 1) + getw ar (rv - 1))) 0
                               (: v2 (getw ar k >> 1)
                                  _ (? (0 = getw fx (seo + v2))
                                       (putw fx (aco + v2) (getw fx (aco + v2) + getw fx 6)) 0)
                                  (go2 (k + 1))))) rv))
                     (rb (j + 1))))) (cid0 + 1))
           _ (czero ())
           gb ((: (go j glue bj)                            ; glue + backjump level over the kept tail
                  (? (j >= wr) (glue >< bj)
                     (: L (getw fx (lvo + (getw ar j >> 1)))
                        bj2 (? (L > bj) L bj)
                        (? (0 = getw fx (seo + L))
                           (: _ (putw fx (seo + L) 1) (go (j + 1) (glue + 1) bj2))
                           (go (j + 1) glue bj2)))))
               (cid0 + 2) (: _ (putw fx (seo + dl) 1) 1) 0)
           glue <gb bj >gb
           nt ((: (go t)                                    ; backjump: pop above bj, save phase
                  (? (t = 0) 0
                     (: el (getw fx (8 + (t - 1))) v (el >> 1)
                        (? (getw fx (lvo + v) > bj)
                           (: _ (putw fx (pho + v) (1 + (el & 1)))
                              _ (putw fx (vao + v) 0) _ (putw fx (lvo + v) 0) _ (putw fx (rso + v) 0)
                              (go (t - 1)))
                           t))))
               (getw fx 0))
           _ (putw fx 0 nt) _ (putw fx 1 nt) _ (putw fx 2 bj)
           size (wr - (cid0 + 1))
           _ (putw ar cid0 size) _ (putw fx 4 wr)
           _ (? (size < 2) 0
                (: mx ((: (go j b) (? (j >= wr) b                       ; reorder: max-level tail lit to slot 2
                              (go (j + 1) (? (getw fx (lvo + (getw ar j >> 1)) > getw fx (lvo + (getw ar b >> 1))) j b))))
                       (cid0 + 3) (cid0 + 2))
                   a (getw ar (cid0 + 2)) b (getw ar mx)
                   _ (putw ar (cid0 + 2) b) _ (putw ar mx a)
                   l0 (getw ar (cid0 + 1)) l1 (getw ar (cid0 + 2)) wnu (getw fx 5)
                   _ (putw wn wnu cid0) _ (putw wn (wnu + 1) (getw fx (who + l0))) _ (putw fx (who + l0) wnu)
                   _ (putw wn (wnu + 2) cid0) _ (putw wn (wnu + 3) (getw fx (who + l1))) _ (putw fx (who + l1) (wnu + 2))
                   (putw fx 5 (wnu + 4))))
           _ (fasg fx (getw ar (cid0 + 1)) (cid0 + 1))
           ((cid0 << 6) | (? (glue > 63) 63 glue)))))
   ; --- the interpreted DECIDE twin: (fdec3 fx) -> the decided var, 0 = SAT -------
   (fdec3 fx)
    (: nv (getw fx 3) vao (vao-of nv) aco (aco-of nv) pho (pho-of nv)
       v ((: (go v best ba)
             (? (v > nv) best
                (: a (getw fx (aco + v))
                   (? ((0 = getw fx (vao + v)) && ((best = 0) || a > ba))
                      (go (v + 1) v a) (go (v + 1) best ba)))))
          1 0 -1)
       (? (v = 0) 0
          (: ph (getw fx (pho + v))
             _ (putw fx 2 (1 + getw fx 2))
             _ (fasg fx (? (ph = 1) (2 * v) (1 + (2 * v))) 0) v)))
   ; ===============================================================================
   ; the NATIVE kernels. lvm ABI: g=rdi(r6) Ip=rsi(r5) Hp=rdx(r2) Sp=rcx(r1); params
   ; TAGGED on Sp[i]. cask guard (deopts BEFORE any push -> machine sp balanced on
   ; every path): fixnum test + ap word vs (apof (cask 1)); byte base = [[v+8]+16].
   ; epilogue: tag acc, st Sp[0], Ip+16 -> lvm_ret. deopt (arity>=2): Ip <- interp
   ; body (args still on Sp). callee-saved r3/r11-r14 pushed after the guards.
   ; ===============================================================================
   (guard-ir i bufk)                                        ; Sp[i] must be a cask
    [['ld 'r0 'r1 (8 * i)] '(test r0 1) '(br ne deopt)
      '(ld r7 r0 0) ['li 'r8 bufk] '(cmp r7 r8) '(br ne deopt)]
   (fixg-ir i)                                              ; Sp[i] must be a fixnum
    [['ld 'r0 'r1 (8 * i)] '(test r0 1) '(br eq deopt)]
   epi-ir '((shl r0 1) (or r0 r0 1) (st r1 0 r0) (add r5 r5 16) (ld r7 r5 0) (jmpr r7))
   deopt2-ir '((label deopt) (ld r7 r5 8) (lea r5 r7 16) (ld r7 r5 0) (jmpr r7))   ; arity>=2
   deopt1-ir '((label deopt) (ld r7 r5 8) (mov r5 r7) (ld r7 r5 0) (jmpr r7))      ; arity 1
   ; --- kernel 1: BCP -- (bcp fx ar wn). registers: r11=fx r12=ar r13=wn r14=f
   ; (callee-saved) r3=cid, r9=p r10=prev r0/r7/r8 scratch. ---------------------------
   (fbcp-ir nv)
    (: tro8 64 who8 (8 * who-of nv) vao8 (8 * vao-of nv)
       lvo8 (8 * lvo-of nv) rso8 (8 * rso-of nv) bufk (apof (cask 1))
       (guard-ir 0 bufk + guard-ir 1 bufk + guard-ir 2 bufk
       + '((push r3) (push r11) (push r12) (push r13) (push r14)
              (ld r0 r1 0)  (ld r0 r0 8) (lea r11 r0 16)
              (ld r0 r1 8)  (ld r0 r0 8) (lea r12 r0 16)
              (ld r0 r1 16) (ld r0 r0 8) (lea r13 r0 16))
       + ['(label drain)
              '(ld r0 r11 8) '(ld r7 r11 0) '(cmp r0 r7) '(br ge noconf)
              ['ldx 'r14 'r11 'r0 8 tro8]
              '(xor r14 r14 1)
              '(add r0 r0 1) '(st r11 8 r0)
              ['ldx 'r9 'r11 'r14 8 who8]
              '(li r10 0)
              '(label node)
              '(cmp r9 0) '(br eq drain)
              '(ldx r3 r13 r9 8 0)
              '(ldx r0 r12 r3 8 0) '(cmp r0 0) '(br ne live)    ; size 0 = TOMBSTONE (freduce):
              '(ldx r0 r13 r9 8 8)                              ; unlink the dead node, walk on
              '(cmp r10 0) '(br ne dprev)
              ['stx 'r11 'r14 8 who8 'r0] '(jmp dnext)
              '(label dprev)
              '(stx r13 r10 8 8 r0)
              '(label dnext)
              '(mov r9 r0) '(jmp node)
              '(label live)
              '(ldx r0 r12 r3 8 8) '(cmp r0 r14) '(br ne noswap)
              '(ldx r7 r12 r3 8 16) '(stx r12 r3 8 8 r7) '(stx r12 r3 8 16 r0)
              '(label noswap)
              '(ldx r8 r12 r3 8 8)
              '(mov r0 r8) '(sar r0 1)
              ['ldx 'r0 'r11 'r0 8 vao8]
              '(cmp r0 r8) '(br eq keep)
              '(ldx r7 r12 r3 8 0) '(add r7 r7 r3) '(add r7 r7 1) '(push r7)
              '(add r0 r3 3)
              '(label scan)
              '(ld r7 sp 0) '(cmp r0 r7) '(br ge scanfail)
              '(ldx r7 r12 r0 8 0)
              '(mov r8 r7) '(sar r8 1)
              ['ldx 'r8 'r11 'r8 8 vao8]
              '(xor r7 r7 1)
              '(cmp r8 r7) '(br ne found)
              '(add r0 r0 1) '(jmp scan)
              '(label found)
              '(xor r7 r7 1)
              '(stx r12 r0 8 0 r14)
              '(stx r12 r3 8 16 r7)
              '(ldx r0 r13 r9 8 8)
              '(cmp r10 0) '(br ne fprev)
              ['stx 'r11 'r14 8 who8 'r0] '(jmp flink)
              '(label fprev)
              '(stx r13 r10 8 8 r0)
              '(label flink)
              ['ldx 'r8 'r11 'r7 8 who8]
              '(stx r13 r9 8 8 r8)
              ['stx 'r11 'r7 8 who8 'r9]
              '(mov r9 r0)
              '(add sp sp 8) '(jmp node)
              '(label scanfail)
              '(add sp sp 8)
              '(ldx r8 r12 r3 8 8)
              '(mov r0 r8) '(sar r0 1)
              ['ldx 'r7 'r11 'r0 8 vao8]
              '(cmp r7 0) '(br ne conflict)
              ['stx 'r11 'r0 8 vao8 'r8]                         ; unit: val[v] = oth
              '(ld r7 r11 16) ['stx 'r11 'r0 8 lvo8 'r7]
              '(add r7 r3 1)  ['stx 'r11 'r0 8 rso8 'r7]
              '(ld r7 r11 0)  ['stx 'r11 'r7 8 tro8 'r8]
              '(add r7 r7 1) '(st r11 0 r7)
              '(label keep)
              '(ldx r7 r13 r9 8 8) '(mov r10 r9) '(mov r9 r7) '(jmp node)
              '(label conflict)
              '(mov r0 r3) '(jmp ret)
              '(label noconf)
              '(li r0 -1)
              '(label ret)
              '(pop r14) '(pop r13) '(pop r12) '(pop r11) '(pop r3)]
            + epi-ir + deopt2-ir))
   ; --- kernel 2: the CONFLICT handler -- (conf fx ar wn confl). ---------------------
   ; registers: r11=fx r12=ar r13=wn (pushed), r3 scratch (pushed), r14 counter/glue
   ; (pushed); r9 write index, r10 trail/read index, r0/r7/r8 scratch; fx w7 (byte 56)
   ; holds the current skip-var. the twin above is the spec, phase for phase.
   (cls-ir nv seo8)                                         ; clear the seen section (fresh labels)
    (: L1 (mint 0) L2 (mint 0)
       ['(li r0 0) '(li r8 0) ['label L1] ['cmp 'r0 nv] ['br 'gt L2]
         ['stx 'r11 'r0 8 seo8 'r8] '(add r0 r0 1) ['jmp L1] ['label L2]])
   (abs-ir lvo8 seo8 aco8)                                  ; absorb the clause at r7 (fresh labels);
    (: Ll (mint 0) Lt (mint 0) Ln (mint 0) Le (mint 0)      ; counter r14, write r9, skipv fx w7
       ['(ldx r0 r12 r7 8 0) '(add r0 r0 r7) '(add r0 r0 1) '(push r0)
         '(add r7 r7 1)
         ['label Ll] '(ld r0 sp 0) '(cmp r7 r0) ['br 'ge Le]
         '(ldx r3 r12 r7 8 0)
         '(mov r0 r3) '(sar r0 1)
         '(ld r8 r11 56) '(cmp r0 r8) ['br 'eq Ln]
         ['ldx 'r8 'r11 'r0 8 seo8] '(cmp r8 0) ['br 'ne Ln]
         ['ldx 'r8 'r11 'r0 8 lvo8] '(cmp r8 0) ['br 'eq Ln]
         '(li r8 1) ['stx 'r11 'r0 8 seo8 'r8]
         ['ldx 'r8 'r11 'r0 8 aco8] '(ld r3 r11 48) '(add r8 r8 r3) ['stx 'r11 'r0 8 aco8 'r8]
         ['ldx 'r8 'r11 'r0 8 lvo8] '(ld r3 r11 16) '(cmp r8 r3) ['br 'ne Lt]
         '(add r14 r14 1) ['jmp Ln]
         ['label Lt] '(ldx r3 r12 r7 8 0) '(stx r12 r9 8 0 r3) '(add r9 r9 1)
         ['label Ln] '(add r7 r7 1) ['jmp Ll]
         ['label Le] '(add sp sp 8)])
   (fconf-ir nv)
    (: tro8 64 lvo8 (8 * lvo-of nv) rso8 (8 * rso-of nv) who8 (8 * who-of nv)
       vao8 (8 * vao-of nv) aco8 (8 * aco-of nv) pho8 (8 * pho-of nv) seo8 (8 * seo-of nv)
       bufk (apof (cask 1))
       (guard-ir 0 bufk + guard-ir 1 bufk + guard-ir 2 bufk + fixg-ir 3
       + '((push r3) (push r11) (push r12) (push r13) (push r14)
              (ld r0 r1 0)  (ld r0 r0 8) (lea r11 r0 16)
              (ld r0 r1 8)  (ld r0 r0 8) (lea r12 r0 16)
              (ld r0 r1 16) (ld r0 r0 8) (lea r13 r0 16)
              (ld r7 r1 24) (sar r7 1)                            ; confl -> r7 (first absorb's clause)
              (ld r0 r11 16) (cmp r0 0) (br eq retm1))            ; level 0 -> -1
       + cls-ir nv seo8                                      ; phase 1: clear seen
       + '((li r14 0)                                          ; counter
              (ld r9 r11 32) (add r9 r9 2)                        ; widx = cid0+2
              (li r0 0) (st r11 56 r0))                           ; skipv = 0
       + abs-ir lvo8 seo8 aco8                               ; phase 2: absorb the conflict clause
       + ['(ld r10 r11 0) '(sub r10 r10 1)                    ; t = top-1
              '(label walk)
              ['ldx 'r0 'r11 'r10 8 tro8]                        ; np = trail[t]
              '(mov r3 r0) '(sar r3 1)
              ['ldx 'r8 'r11 'r3 8 seo8] '(cmp r8 0) '(br ne got)
              '(sub r10 r10 1) '(jmp walk)
              '(label got)
              '(li r8 0) ['stx 'r11 'r3 8 seo8 'r8]              ; seen[v] = 0
              '(sub r14 r14 1) '(cmp r14 0) '(br eq uip)
              ['ldx 'r7 'r11 'r3 8 rso8] '(sub r7 r7 1)          ; cs = rsn[v]-1
              '(st r11 56 r3)                                     ; skipv = v
              '(push r10)]
       + abs-ir lvo8 seo8 aco8                               ; absorb the reason clause
       + '((pop r10) (sub r10 r10 1) (jmp walk)
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
       + [['ldx 'r7 'r11 'r3 8 rso8]
              '(cmp r7 0) '(br eq mkeep)
              '(sub r7 r7 1)
              '(ldx r0 r12 r7 8 0) '(add r0 r0 r7) '(push r0)     ; inner lim (inclusive)
              '(add r7 r7 1)
              '(label mck) '(ld r0 sp 0) '(cmp r7 r0) '(br gt mred)
              '(ldx r0 r12 r7 8 0) '(sar r0 1)
              '(ld r8 r11 56) '(cmp r0 r8) '(br eq mcknx)
              ['ldx 'r8 'r11 'r0 8 lvo8] '(cmp r8 0) '(br eq mcknx)
              ['ldx 'r8 'r11 'r0 8 seo8] '(cmp r8 0) '(br ne mcknx)
              '(add sp sp 8) '(jmp mkeep)                         ; a fresh lit -> NOT redundant
              '(label mcknx) '(add r7 r7 1) '(jmp mck)
              '(label mred) '(add sp sp 8) '(jmp mnnext)
              '(label mkeep) '(ldx r0 r12 r10 8 0) '(stx r12 r9 8 0 r0) '(add r9 r9 1)
              '(label mnnext) '(add r10 r10 1) '(jmp mn)
              '(label mnend) '(add sp sp 8)]                      ; r9 = wend after compaction
       ; phase 3b: reason-side bump -- every UNSEEN var in a learnt literal's reason
       ; gets act += vinc (seen still holds the analysis marks; w7 free as scratch).
       + ['(ld r10 r11 32) '(add r10 r10 1)                   ; j = cid0+1
              '(label rb) '(cmp r10 r9) '(br ge rbe)
              '(ldx r0 r12 r10 8 0) '(sar r0 1)                   ; v
              ['ldx 'r7 'r11 'r0 8 rso8]
              '(cmp r7 0) '(br eq rbn)
              '(sub r7 r7 1)
              '(ldx r0 r12 r7 8 0) '(add r0 r0 r7) '(st r11 56 r0)    ; w7 = last lit index
              '(add r7 r7 1)
              '(label rk) '(ld r0 r11 56) '(cmp r7 r0) '(br gt rbn)
              '(ldx r0 r12 r7 8 0) '(sar r0 1)                    ; v2
              ['ldx 'r8 'r11 'r0 8 seo8] '(cmp r8 0) '(br ne rknx)
              ['ldx 'r8 'r11 'r0 8 aco8] '(ld r3 r11 48) '(add r8 r8 r3)
              ['stx 'r11 'r0 8 aco8 'r8]
              '(label rknx) '(add r7 r7 1) '(jmp rk)
              '(label rbn) '(add r10 r10 1) '(jmp rb)
              '(label rbe)]
       + cls-ir nv seo8                                      ; phase 4: clear seen (-> level marks)
       + ['(ld r0 r11 16)                                     ; mark dl
              '(li r8 1) ['stx 'r11 'r0 8 seo8 'r8]
              '(li r14 1) '(li r10 0)                             ; glue=1, bj=0
              '(ld r7 r11 32) '(add r7 r7 2)
              '(label gb) '(cmp r7 r9) '(br ge gbe)
              '(ldx r0 r12 r7 8 0) '(sar r0 1)
              ['ldx 'r0 'r11 'r0 8 lvo8]                         ; L
              '(cmp r10 r0) '(br ge gb1) '(mov r10 r0)
              '(label gb1)
              ['ldx 'r8 'r11 'r0 8 seo8] '(cmp r8 0) '(br ne gb2)
              '(li r8 1) ['stx 'r11 'r0 8 seo8 'r8] '(add r14 r14 1)
              '(label gb2) '(add r7 r7 1) '(jmp gb)
              '(label gbe)
               ; phase 5: backjump to bj (r10)
              '(ld r7 r11 0)
              '(label bjl) '(cmp r7 0) '(br eq bje)
              '(mov r8 r7) '(sub r8 r8 1)
              ['ldx 'r0 'r11 'r8 8 tro8]                         ; el = trail[t-1]
              '(mov r3 r0) '(sar r3 1)
              ['ldx 'r8 'r11 'r3 8 lvo8] '(cmp r8 r10) '(br le bje)
              '(and r0 r0 1) '(add r0 r0 1) ['stx 'r11 'r3 8 pho8 'r0]
              '(li r0 0) ['stx 'r11 'r3 8 vao8 'r0] ['stx 'r11 'r3 8 lvo8 'r0] ['stx 'r11 'r3 8 rso8 'r0]
              '(sub r7 r7 1) '(jmp bjl)
              '(label bje) '(st r11 0 r7) '(st r11 8 r7) '(st r11 16 r10)
               ; phase 6: size, aru, reorder, watches
              '(ld r3 r11 32)                                     ; cid0
              '(mov r0 r9) '(sub r0 r0 r3) '(sub r0 r0 1)         ; size
              '(stx r12 r3 8 0 r0) '(st r11 32 r9)                ; ar[cid0]=size; aru=wend
              '(cmp r0 2) '(br lt assert)
              '(mov r10 r3) '(add r10 r10 2)                      ; reorder: argmax level
              '(ldx r0 r12 r10 8 0) '(sar r0 1) ['ldx 'r8 'r11 'r0 8 lvo8]
              '(mov r7 r10) '(add r7 r7 1)
              '(label ro) '(cmp r7 r9) '(br ge roe)
              '(ldx r0 r12 r7 8 0) '(sar r0 1) ['ldx 'r0 'r11 'r0 8 lvo8]
              '(cmp r0 r8) '(br le ro2) '(mov r8 r0) '(mov r10 r7)
              '(label ro2) '(add r7 r7 1) '(jmp ro)
              '(label roe)
              '(ldx r0 r12 r3 8 16) '(ldx r7 r12 r10 8 0) '(stx r12 r3 8 16 r7) '(stx r12 r10 8 0 r0)
              '(ldx r7 r12 r3 8 8) '(ldx r8 r12 r3 8 16)          ; l0, l1
              '(ld r10 r11 40)                                    ; wnu
              '(stx r13 r10 8 0 r3)
              ['ldx 'r0 'r11 'r7 8 who8] '(stx r13 r10 8 8 r0) ['stx 'r11 'r7 8 who8 'r10]
              '(add r10 r10 2)
              '(stx r13 r10 8 0 r3)
              ['ldx 'r0 'r11 'r8 8 who8] '(stx r13 r10 8 8 r0) ['stx 'r11 'r8 8 who8 'r10]
              '(add r10 r10 2) '(st r11 40 r10)
              '(label assert)                                     ; phase 7: assign learnt[0], reason cid0+1
              '(ldx r8 r12 r3 8 8) '(mov r0 r8) '(sar r0 1)
              ['stx 'r11 'r0 8 vao8 'r8]
              '(ld r7 r11 16) ['stx 'r11 'r0 8 lvo8 'r7]
              '(mov r7 r3) '(add r7 r7 1) ['stx 'r11 'r0 8 rso8 'r7]
              '(ld r7 r11 0) ['stx 'r11 'r7 8 tro8 'r8] '(add r7 r7 1) '(st r11 0 r7)
              '(mov r0 r3) '(shl r0 6)                            ; return (cid0<<6)|min(glue,63)
              '(cmp r14 63) '(br le pk) '(li r14 63)
              '(label pk) '(or r0 r0 r14) '(jmp fin)
              '(label retm1) '(li r0 -1)
              '(label fin)
              '(pop r14) '(pop r13) '(pop r12) '(pop r11) '(pop r3)]
            + epi-ir + deopt2-ir))
   ; --- kernel 3: DECIDE -- (dec fx). caller-saved regs only, no pushes. -------------
   (fdec-ir nv)
    (: tro8 64 lvo8 (8 * lvo-of nv) rso8 (8 * rso-of nv) vao8 (8 * vao-of nv)
       aco8 (8 * aco-of nv) pho8 (8 * pho-of nv) bufk (apof (cask 1))
       (guard-ir 0 bufk
       + '((ld r9 r1 0) (ld r9 r9 8) (lea r9 r9 16)
              (li r7 0) (li r8 -1) (li r0 1))
       + ['(label dl) ['cmp 'r0 nv] '(br gt dle)
              ['ldx 'r3 'r9 'r0 8 vao8] '(cmp r3 0) '(br ne dn)
              ['ldx 'r3 'r9 'r0 8 aco8]
              '(cmp r7 0) '(br eq take)
              '(cmp r3 r8) '(br gt take) '(jmp dn)
              '(label take) '(mov r7 r0) '(mov r8 r3)
              '(label dn) '(add r0 r0 1) '(jmp dl)
              '(label dle) '(cmp r7 0) '(br eq ret0)
              '(ld r0 r9 16) '(add r0 r0 1) '(st r9 16 r0)        ; level++
              ['ldx 'r3 'r9 'r7 8 pho8]
              '(mov r0 r7) '(add r0 r0 r0)                        ; el = 2v (+1 unless phase 1)
              '(cmp r3 1) '(br eq pos) '(add r0 r0 1)
              '(label pos)
              ['stx 'r9 'r7 8 vao8 'r0]                          ; assign(el, 0)
              '(ld r3 r9 16) ['stx 'r9 'r7 8 lvo8 'r3]
              '(li r3 0) ['stx 'r9 'r7 8 rso8 'r3]
              '(ld r3 r9 0) ['stx 'r9 'r3 8 tro8 'r0] '(add r3 r3 1) '(st r9 0 r3)
              '(mov r0 r7) '(jmp dfin)
              '(label ret0) '(li r0 0)
              '(label dfin)]
            + epi-ir + deopt1-ir))
   ; --- kernel 4: the BVA grow step -- (fbvag bx ba bo bp); fbvag3 below is the
   ; twin, phase for phase. registers: r11=bx r12=ba r13=bo r14=bp (callee-saved),
   ; r10=l, r9 the chain cursor, r0/r3/r7/r8 scratch; bx header words 14+ are the
   ; spill frame (cid/n/limi/limj/x/pnext/tail), so the walk fits the register
   ; file. byte offsets: w3=24 pnu, w5=40 l, w6=48 chain-in, w7=56 mode, w8=64 LO,
   ; w9=72 HI, w11=88 best, w12=96 count, w13=104 chain-out, w14..w20=112..160.
   (bva-p1-ir pco8)                                         ; pass-1 match block (m=r0): count + running best
    (: An (mint 0) Ax (mint 0)
       [['ldx 'r3 'r11 'r0 8 pco8] '(add r3 r3 1) ['stx 'r11 'r0 8 pco8 'r3]
         '(ld r7 r11 96)
         '(cmp r3 r7) ['br 'gt An]
         '(cmp r3 r7) ['br 'ne Ax]
         '(ld r7 r11 88) '(cmp r0 r7) ['br 'ge Ax]          ; ties -> LOWEST key
         ['label An] '(st r11 88 r0) '(st r11 96 r3)
         ['label Ax]])
   (bva-p2-ir pco8)                                         ; pass-2 match block: reset on sight + collect best's cids
    (: Ax (mint 0)
       ['(li r3 0) ['stx 'r11 'r0 8 pco8 'r3]
         '(ld r3 r11 88) '(cmp r0 r3) ['br 'ne Ax]
         '(ld r3 r11 24) '(ld r7 r11 112)
         '(stx r14 r3 8 0 r7)                               ; node = [cid, chain]
         '(ld r7 r11 104) '(stx r14 r3 8 8 r7)
         '(st r11 104 r3)
         '(add r3 r3 2) '(st r11 24 r3)
         ['label Ax]])
   (bva-walk-ir oco8 hho8 mlo8 act)                         ; the shared two-pass walk (fresh labels)
    (: Lo (mint 0) Lod (mint 0) Lk (mint 0) Lkn (mint 0) Lke (mint 0)
       Lq (mint 0) Lqd (mint 0) Lm (mint 0) Lms (mint 0) Lmb (mint 0) Lmx (mint 0)
       Lme (mint 0) Lm2 (mint 0) Lmm (mint 0)
       (['(ld r0 r11 48)                                    ; p = chain-in
         ['label Lo] '(cmp r0 0) ['br 'eq Lod]
         '(ldx r3 r14 r0 8 0) '(st r11 112 r3)              ; cid
         '(ldx r7 r14 r0 8 8) '(st r11 152 r7)              ; pnext
         '(ldx r7 r12 r3 8 0) '(st r11 120 r7)              ; n
         '(add r7 r7 r3) '(st r11 128 r7)                   ; limi = cid + n
         '(ld r0 r11 112) '(add r0 r0 1)                    ; k = fewest-occurrence lit, l aside
         '(li r8 2147483647) '(li r9 0)
         ['label Lk] '(ld r7 r11 128) '(cmp r0 r7) ['br 'gt Lke]
         '(ldx r3 r12 r0 8 0)
         '(cmp r3 r10) ['br 'eq Lkn]
         ['ldx 'r7 'r11 'r3 8 oco8]
         '(cmp r7 r8) ['br 'ge Lkn]
         '(mov r8 r7) '(mov r9 r3)
         ['label Lkn] '(add r0 r0 1) ['jmp Lk]
         ['label Lke]
         ['ldx 'r9 'r11 'r9 8 hho8]                         ; q = heads[k]
         ['label Lq] '(cmp r9 0) ['br 'eq Lqd]
         '(ldx r0 r13 r9 8 0)                               ; did
         '(ldx r7 r13 r9 8 8) '(mov r9 r7)                  ; advance early: fails jump straight to Lq
         '(ldx r7 r12 r0 8 0) '(ld r3 r11 120)
         '(cmp r7 r3) ['br 'ne Lq]                          ; size != n (covers tombstones)
         '(ld r3 r11 112) '(cmp r0 r3) ['br 'eq Lq]         ; did == cid
         '(ld r3 r11 120) '(add r3 r0 r3) '(st r11 136 r3)  ; limj = did + n
         '(li r3 0) '(st r11 144 r3)                        ; x = 0
         '(ld r7 r11 112) '(add r7 r7 1)                    ; i = cid+1
         '(add r8 r0 1)                                     ; j = did+1
         ['label Lm] '(ld r0 r11 128) '(cmp r7 r0) ['br 'gt Lme]
         '(ldx r0 r12 r7 8 0)                               ; ci
         '(cmp r0 r10) ['br 'eq Lms]                        ; ci == l -> i++
         '(ld r3 r11 136) '(cmp r8 r3) ['br 'gt Lq]         ; j exhausted with r left -> fail
         '(ldx r3 r12 r8 8 0)                               ; cj
         '(cmp r0 r3) ['br 'eq Lmb]
         '(cmp r3 r0) ['br 'lt Lmx]                         ; cj < ci -> the extra
         ['jmp Lq]                                          ; ci < cj -> r not a subset
         ['label Lmx] '(ld r0 r11 144) '(cmp r0 0) ['br 'ne Lq]      ; a second extra -> fail
         '(st r11 144 r3) '(add r8 r8 1) ['jmp Lm]
         ['label Lms] '(add r7 r7 1) ['jmp Lm]
         ['label Lmb] '(add r7 r7 1) '(add r8 r8 1) ['jmp Lm]
         ['label Lme]                                       ; i exhausted
         '(ld r0 r11 144) '(cmp r0 0) ['br 'ne Lm2]
         '(ld r3 r11 136) '(cmp r8 r3) ['br 'ne Lq]         ; x=0: exactly the last slot remains
         '(ldx r0 r12 r8 8 0) ['jmp Lmm]
         ['label Lm2] '(ld r3 r11 136) '(cmp r8 r3) ['br 'le Lq]    ; x>0: j must be past limj
         '(ld r0 r11 144)
         ['label Lmm]                                       ; m = r0: band + M_lit mark
         '(ld r3 r11 64) '(cmp r0 r3) ['br 'lt Lq]
         '(ld r3 r11 72) '(cmp r0 r3) ['br 'gt Lq]
         ['ldx 'r3 'r11 'r0 8 mlo8] '(cmp r3 0) ['br 'ne Lq]]
        + act
        + [['jmp Lq]
           ['label Lqd]
           '(ld r0 r11 152) ['jmp Lo]
           ['label Lod]]))
   (fbvag-ir NB2)
    (: s2 ((2 * NB2) + 2)
       hho8 192 oco8 (8 * (24 + s2)) pco8 (8 * (24 + (3 * s2))) mlo8 (8 * (24 + (4 * s2)))
       bufk (apof (cask 1))
       (guard-ir 0 bufk + guard-ir 1 bufk + guard-ir 2 bufk + guard-ir 3 bufk
       + '((push r3) (push r11) (push r12) (push r13) (push r14)
              (ld r0 r1 0)  (ld r0 r0 8) (lea r11 r0 16)
              (ld r0 r1 8)  (ld r0 r0 8) (lea r12 r0 16)
              (ld r0 r1 16) (ld r0 r0 8) (lea r13 r0 16)
              (ld r0 r1 24) (ld r0 r0 8) (lea r14 r0 16)
              (li r0 0) (st r11 88 r0) (st r11 96 r0) (st r11 104 r0) (st r11 160 r0)
              (ld r0 r11 56) (cmp r0 0) (br eq grow))
       ; build mode: chain occ[l]'s live in-band clauses, walk order (tail-append)
       + ['(ld r10 r11 40)
             ['ldx 'r9 'r11 'r10 8 hho8]                      ; q = heads[l]
             '(ld r8 r11 64) '(ld r10 r11 72)                ; LO / HI
             '(label bq) '(cmp r9 0) '(br eq bend)
             '(ldx r0 r13 r9 8 0) '(ldx r7 r13 r9 8 8) '(mov r9 r7)
             '(ldx r3 r12 r0 8 0)
             '(cmp r3 2) '(br lt bq)                         ; unit / tombstone
             '(st r11 112 r0)                                ; did
             '(add r3 r0 r3) '(add r7 r0 1)                  ; limj / j
             '(label bo) '(cmp r7 r3) '(br gt badd)
             '(ldx r0 r12 r7 8 0)
             '(cmp r0 r8) '(br lt bq)                        ; out of band -> skip the clause
             '(cmp r0 r10) '(br gt bq)
             '(add r7 r7 1) '(jmp bo)
             '(label badd)
             '(ld r0 r11 24) '(ld r7 r11 112)
             '(stx r14 r0 8 0 r7)
             '(li r3 0) '(stx r14 r0 8 8 r3)
             '(ld r7 r11 160) '(cmp r7 0) '(br ne btl)
             '(st r11 104 r0) '(jmp btl2)
             '(label btl) '(stx r14 r7 8 8 r0)
             '(label btl2) '(st r11 160 r0)
             '(add r0 r0 2) '(st r11 24 r0)
             '(ld r0 r11 96) '(add r0 r0 1) '(st r11 96 r0)
             '(jmp bq)
             '(label bend) '(ld r0 r11 96) '(jmp gret)
             '(label grow)
             '(ld r10 r11 40)]
       + bva-walk-ir oco8 hho8 mlo8 (bva-p1-ir pco8)
       + '((ld r0 r11 88) (cmp r0 0) (br eq gret))
       + bva-walk-ir oco8 hho8 mlo8 (bva-p2-ir pco8)
       + '((ld r0 r11 88)
              (label gret)
              (pop r14) (pop r13) (pop r12) (pop r11) (pop r3))
       + epi-ir + deopt2-ir))
   fkers (tablet 0)                                         ; nv -> (bcp conf dec), one assemble per nvars
   fknob {0}                                                ; pin 1 to force the interpreted twins (the differential switch)
   fmaxl0 {2000}                                            ; the initial learnt-DB cap (the gate lowers it to force reductions)
   frp0 {4}                                                 ; rephase-wheel cadence in restarts (pin 0 = off)
   fcmp0 {1048576}                                          ; arena-words threshold for the real compaction
   fru0 {400}                                               ; the luby restart unit (conflicts per luby-1 block)
   fst0 {10000}                                             ; the stable/focused mode budget in conflicts (x3/2 per switch)
   ; --- DRAT proof emission: pin a JUG into fdrat0 and every UNSAT solve leaves a
   ; checkable refutation in it -- each learnt commit a RUP line, each tombstoned
   ; learnt a `d` line, each BVA application its extended-resolution lines (RAT on
   ; the fresh variable), and fcdcl caps an UNSAT verdict with the empty clause.
   ; the proof checks against the ORIGINAL formula: in-gate by fd-check (below),
   ; externally by drat-trim (crew/sat/dratcheck.sh).
   fdrat0 {0}                                               ; 0 off; a jug records the proof
   (fdlit el) (: v (el >> 1) (? (1 = (el & 1)) (0 - v) v))  ; encoded -> signed DIMACS literal
   (fdsay j pre c)                                          ; one proof line: prefix, lits, the closing 0
    (: _ (say j pre)
       _ (foldl (\ _ l (: _ (say j (show l)) (say j " "))) 0 c)
       (say j "0\n"))
   ; the seam itself, bound ONCE, through the MODULE: main.c's seal pulls `nif` off the book on
   ; every boot -- the glaze folded it into its closures and keeps it as a member, so this is the
   ; one door left. no glaze answers () and (() 'nif) is 1, which `lit?` below declines: the guard
   ; is the presence test. a local, so those guards read a binding and never a name.
   nifseam (from 'glaze 'nif)
   (fnif ir fb ar)                                          ; assemble + install one kernel (fb = the honest twin)
    (: bs (peep ((from 'holo 'assemble) 'amd64 ir) 'bytes 0)   ; through the registry accessor -- holo's whole public surface
       c (cask bs)                                           ; the charlist lane fills in C
       (nifseam c fb '(\ q (q)) ar))
   (fbcpk nv)                                               ; -> (bcp conf dec), or () when they can't build
    (? !(lit? nifseam && id? love-arch 'amd64) ()
       (: hit (peep fkers nv ())
          (? (two? hit) hit
             (: ks [(fnif (fbcp-ir nv) fbcp3 3)
                     (fnif (fconf-ir nv) fconf3 4)
                     (fnif (fdec-ir nv) fdec3 1)]
                _ (pin fkers nv ks) ks))))
   bvkers (tablet 0)                                        ; NB -> (fbvag), the grow-step kernel, one assemble per bucket
   (fbvagk NB2)
    (? !(lit? nifseam && id? love-arch 'amd64) ()
       (: hit (peep bvkers NB2 ())
          (? (two? hit) hit
             (: ks (fnif (fbvag-ir NB2) fbvag3 4 >< ())
                _ (pin bvkers NB2 ks) ks))))
   (fmk formula nvars)                                      ; build the flat solver state
    ; the LAYOUT nv is nvars rounded up to a power-of-2 bucket, so the kernel cache
    ; (keyed per layout) survives BVA's varying variable counts. phantom vars above
    ; the real count are sound: never watched, activity 0 (decided dead last, after
    ; every real var -- the ascending scan keeps the FIRST max), popped clean.
    (: s (tablet 0) nv ((: (go k) (? (k >= nvars) k (go (2 * k)))) 16)
       fx (cask (8 * (seo-of nv + (nv + 1))))
       _ (putw fx 3 nv, putw fx 4 2, putw fx 5 2, putw fx 6 256)
       _ (pins s 'nvars nv 'fx fx
          'lvo (lvo-of nv) 'rso (rso-of nv) 'who (who-of nv) 'vao (vao-of nv)
          'ar (cask 4096) 'wn (cask 4096)  ; words 0-1 of each reserved: 0 stays "none"
          'bad 0 'nconf 0 'rnum 1 'climit (peep fru0 () 400) 'rew 0
          'mode 0 'mconf 0 'mbudget (peep fst0 () 10000) 'sclimit 2000
          'btop 0 'bph (cask (8 * (nv + 1)))
          'learnts () 'nl 0 'maxl (peep fmaxl0 () 2000))
       ks (fbcpk nv)
       use (two? ks && (0 = peep fknob () 0))
       _ (pins s 'bcp  (use ? <ks fbcp3)
                 'conf (use ? <>ks fconf3)
                 'dec  (use ? <>>ks fdec3))
       _ (foldl (\ _ c (fload s c)) 0 formula) s)
   ; --- the cold path: restarts, decay, learnt-DB reduction (lisp side) -----------
   (fbackjump s lvl)                                        ; pop + unassign above lvl (restarts: lvl = 0)
    (: fx (peep s 'fx ()) nv (peep s 'nvars 0) lvo (peep s 'lvo 0) rso (peep s 'rso 0)
       vao (peep s 'vao 0) pho (pho-of nv)
       nt ((: (go t)
              (? (t = 0) 0
                 (: el (getw fx (8 + t - 1)) v (el >> 1)
                    (? (getw fx (lvo + v) > lvl)
                       (putw fx (pho + v) (1 + (el & 1)),
                        putw fx (vao + v) 0, putw fx (lvo + v) 0, putw fx (rso + v) 0,
                          (go (t - 1)))
                       t))))
           (getw fx 0))
       _ (putw fx 0 nt) _ (putw fx 1 nt) (putw fx 2 lvl))
   (fdecay s)                                               ; integer VSIDS: vinc *= 21/20; rescale by >>8
    (: fx (peep s 'fx ()) vi ((getw fx 6 * 21) // 20)
       (? (vi > 16777216)
          (: nv (peep s 'nvars 0) aco (aco-of nv)
             _ ((: (go v) (? (v > nv) 0 (: _ (putw fx (aco + v) (getw fx (aco + v) >> 8)) (go (v + 1))))) 1)
             (putw fx 6 (vi >> 8)))
          (putw fx 6 vi)))
   (fnext-restart s) (: rn (1 + peep s 'rnum 1) _ (pin s 'rnum rn) _ (pin s 'nconf 0) _ (pin s 'climit (luby rn * peep fru0 () 400)) 0)
   (frephase s st)                                          ; randomize every saved phase; -> st'
    (: fx (peep s 'fx ()) nv (peep s 'nvars 0) pho (pho-of nv)
       ((: (go v st) (? (v > nv) st
             (: p (random st 2) _ (putw fx (pho + v) (1 + <p)) (go (v + 1) >p))))
        1 st))
   (fbest s)                                                ; restore the BEST-TRAIL phases (the deepest basin seen)
    (: fx (peep s 'fx ()) nv (peep s 'nvars 0) pho (pho-of nv) b (peep s 'bph ())
       ((: (go v) (? (v > nv) 0
             (: p (getw b v) _ (? (p = 0) 0 (putw fx (pho + v) p)) (go (v + 1))))) 1))
   (fsnap s)                                                ; snapshot the ASSIGNMENT's polarities at a new deepest trail
    (: fx (peep s 'fx ()) nv (peep s 'nvars 0) vao (vao-of nv) b (peep s 'bph ())
       ((: (go v) (? (v > nv) 0
             (: el (getw fx (vao + v))
                _ (putw b v (? (el = 0) 0 (0 = (el & 1)) 1 2))
                (go (v + 1))))) 1))
   (finvert s)                                              ; flip every saved phase
    (: fx (peep s 'fx ()) nv (peep s 'nvars 0) pho (pho-of nv)
       ((: (go v) (? (v > nv) 0
             (: p (getw fx (pho + v)) _ (putw fx (pho + v) (? (p = 1) 2 1)) (go (v + 1))))) 1))
   ; learnt-DB REDUCTION at a level-0 restart, by TOMBSTONE: the worst-glue half
   ; (glue <= 3 immortal) gets its arena SIZE word zeroed, and both kernels' bcp walk
   ; unlinks a dead clause's watch nodes lazily on the next visit -- no arena
   ; compaction, no watch rebuild, so a reduction is O(learnts) lisp. a dead clause's
   ; words stay intact (mark, not corrupt), but the size-0 check runs FIRST in the
   ; walk, so its stale watch slots are never read as a clause. level-0 reasons are
   ; zeroed (absorb skips level-0 vars anyway; belt and braces). the arena only
   ; grows -- dead slabs leak until the solve ends, the price of O(1)-amortized
   ; reduction (fens doubles, so the growth stays geometric).
   (freduce s)
    (: fx (s 'fx) rso (peep s 'rso 0) ar (s 'ar)
       ls (s 'learnts)
       srt (sortby (a \ b \ >a <= >b) (filter (p \ >p > 3) ls))
       nk (#srt // 2)
       dead (tablet 0)
       _ (foldl (\ i p (i >= nk ? (pin dead <p 1), i + 1)) 0 srt)
       _ ((: (zr t) (? (t >= getw fx 0) 0
              (: v (getw fx (8 + t) >> 1) _ (putw fx (rso + v) 0) (zr (t + 1))))) 0)
       ; the tombstone stashes the slab's true length at cid+1 (nothing reads a dead
       ; clause's lits -- the size-0 check precedes every access), so fcompact can walk
       ; the arena without it.
       j (peep fdrat0 () 0)
       _ (foldl (\ _ cid (: n (getw ar cid)
                            _ (? (hot? j) (fdsay j "d " (map fdlit (fclits s cid))) 0)
                            _ (putw ar cid 0) _ (putw ar (cid + 1) n) 0)) 0 (keys dead))
       nl (filter (\ p (0 = peep dead <p 0)) ls)
       _ (pin s 'learnts nl) _ (pin s 'nl #nl)
       ; the cap grows LINEARLY: geometric (x3/2 per reduce) compounds to an unbounded
       ; DB on 100k-conflict runs -- watch lists balloon and bcp relives the pre-tombstone
       ; disease (uuf250: 101s geometric vs 31s linear).
       (pin s 'maxl (peep s 'maxl 2000 + 500)))
   ; RARE REAL COMPACTION (fcompact): the tombstone reducer leaks dead slabs -- on a
   ; 100k+-conflict run the arena spreads over tens of MB and bcp's working set dies.
   ; when the arena top passes fcmp0 words (at a level-0 restart, after freduce), the
   ; LIVE clauses compact into a fresh cask, watches rebuild from scratch (wn resets:
   ; its dead nodes leak too), learnt cids remap. same level-0 discipline as freduce:
   ; level-0 reasons zeroed, a level-0-satisfied clause drops, a single-nonfalse one
   ; enqueues as a fact, all-false cannot arise (propagation was drained).
   (fswap c i j) (? (i = j) 0 (: a (getw c i) b (getw c j) _ (putw c i b) (putw c j a)))
   (fcompact s)
    (: fx (peep s 'fx ()) nv (peep s 'nvars 0) who (peep s 'who 0) rso (peep s 'rso 0) ar (peep s 'ar ())
       ls (peep s 'learnts ())
       lt (tablet 0) _ (foldl (\ _ p (pin lt <p >p)) 0 ls)
       _ ((: (zw i) (? (i > (1 + (2 * nv))) 0 (: _ (putw fx (who + i) 0) (zw (i + 1))))) 0)
       _ (putw fx 5 2)
       ; ZERO the level-0 reasons: every cid MOVES here, so a stale reason would
       ; dereference garbage in the new arena (absorb skips level-0 vars, but the
       ; discipline is load-bearing -- omitting this produced a bogus SAT on uuf250).
       _ ((: (zr t) (? (t >= getw fx 0) 0
              (: v (getw fx (8 + t) >> 1) _ (putw fx (rso + v) 0) (zr (t + 1))))) 0)
       end (getw fx 4)
       lw ((: (cw cid acc)                                  ; live words (dead slabs skip via the stash)
              (? (cid >= end) acc
                 (: n (getw ar cid)
                    (? (n = 0) (cw (cid + (getw ar (cid + 1) + 1)) acc)
                       (cw (cid + (n + 1)) (acc + (n + 1))))))) 2 2)
       na (cask (8 * (lw + (nv + 8)))) _ (pin s 'ar na)
       res ((: (walk cid nu nls)
               (? (cid >= end) (nu >< nls)
                  (: n (getw ar cid)
                     (? (n = 0) (walk (cid + (getw ar (cid + 1) + 1)) nu nls)
                        (: nxt (cid + (n + 1))
                           lits ((: (go j acc) (? (j < (cid + 1)) acc (go (j - 1) (getw ar j >< acc)))) (cid + n) ())
                           (? (n < 2) (walk nxt nu nls)
                              (any (\ el (1 = flv s el)) lits) (walk nxt nu nls)
                              (: nf (filter (\ el !(-1 = flv s el)) lits)
                                 ; all-false CAN arise here: a fact fenq'd earlier in
                                 ; THIS walk is still queued, so a later clause may be
                                 ; falsified before bcp ever sees it -- that is a
                                 ; level-0 conflict: the formula is UNSAT (s 'bad, read
                                 ; by the driver after the compaction).
                                 (? (atom? nf) (: _ (pin s 'bad 1) (walk nxt nu nls))
                                    (atom? >nf) (: _ (fenq s <nf 0) (walk nxt nu nls))
                                    (: _ (pour na (8 * nu) ar (8 * cid) (8 * (n + 1)))
                                       l1 <nf l2 <>nf
                                       p1 ((: (fnd j) (? (getw na j = l1) j (fnd (j + 1)))) (nu + 1))
                                       _ (fswap na (nu + 1) p1)
                                       p2 ((: (fnd j) (? (getw na j = l2) j (fnd (j + 1)))) (nu + 2))
                                       _ (fswap na (nu + 2) p2)
                                       _ (fwatch s nu (getw na (nu + 1))) _ (fwatch s nu (getw na (nu + 2)))
                                       g (peep lt cid ())
                                       (walk nxt (nu + (n + 1))
                                             (? (whole? g) ((nu >< g) >< nls) nls)))))))))))
            2 2 ())
       _ (putw fx 4 <res) _ (pin s 'learnts >res) (pin s 'nl (tally >res)))
   ; --- the search loop: three kernel calls + lisp bookkeeping --------------------
   (fsearch s)                                              ; -> 'sat or 'unsat (s holds the model on SAT)
    (: fx (peep s 'fx ()) nv (peep s 'nvars 0)
       ((: (loop st)                                                ; st threads the rephase RNG (seeded: reproducible)
           (: confl ((peep s 'bcp ()) fx (peep s 'ar ()) (peep s 'wn ()))
              (? (confl > -1)
                 (: _ (? (getw fx 0 > peep s 'btop 0)               ; a new deepest trail: snapshot its
                         (: _ (pin s 'btop (getw fx 0)) (fsnap s))  ; polarities (the wheel's best spoke)
                         0)
                    _ (fens s 'ar (8 * (getw fx 4 + (nv + 3))))     ; room for the learnt clause
                    _ (fens s 'wn (8 * (getw fx 5 + 4)))            ; ... and its two watch nodes
                    r ((peep s 'conf ()) fx (peep s 'ar ()) (peep s 'wn ()) confl)
                    (? (r = -1) 'unsat
                       (: cid (r >> 6) glue (r & 63)
                          _ (: j (peep fdrat0 () 0)              ; the learnt is RUP: log it
                               (? (hot? j) (fdsay j "" (map fdlit (fclits s cid))) 0))
                          _ (? (1 < getw (peep s 'ar ()) cid)       ; size >= 2: track for reduction
                               (: _ (pin s 'learnts ((cid >< glue) >< peep s 'learnts ()))
                                  (pin s 'nl (1 + peep s 'nl 0))) 0)
                          _ (fdecay s) _ (pin s 'nconf (1 + peep s 'nconf 0))
                          _ (pin s 'mconf (1 + peep s 'mconf 0))
                          (loop st))))
                 ((getw fx 2 > 0) &&                                ; a full DB also forces the restart:
                  ((peep s 'nconf 0 >= peep s 'climit 0) ||         ; reduction is only sound at level 0
                   (peep s 'nl 0 > peep s 'maxl 2000)))
                    (: _ (fbackjump s 0) _ (fnext-restart s)
                       _ (? (peep s 'nl 0 > peep s 'maxl 2000) (freduce s) 0)
                       _ (? (getw fx 4 > peep fcmp0 () 1048576) (fcompact s) 0)
                       ; STABLE/FOCUSED alternation on a growing conflict budget (fst0,
                       ; x3/2 per switch): focused = the luby schedule; stable = rare
                       ; GEOMETRIC restarts (climit 2000 x6/5 within the phase) with
                       ; sticky phases. the REPHASE WHEEL (random -> invert -> keep)
                       ; fires only on ENTERING stable: early enough to spring a SAT
                       ; instance from a bad polarity basin (uf250 keeps its ~35% win),
                       ; rare enough that a long UNSAT refutation keeps its phase-saving
                       ; locality (uuf250 panel 65s -> 42s; the old every-4-restarts
                       ; wheel fired ~440 times over 700k conflicts and DOUBLED the
                       ; conflict count). frp0 pins the wheel off.
                       st2 (? (peep s 'mconf 0 > peep s 'mbudget 0)
                              (: m (peep s 'mode 0) _ (pin s 'mode (1 - m)) _ (pin s 'mconf 0)
                                 _ (pin s 'mbudget ((peep s 'mbudget 0 * 3) // 2))
                                 (? ((m = 0) && (peep frp0 () 0 > 0))
                                    (: _ (pin s 'sclimit 2000)
                                       w (peep s 'rew 0) _ (pin s 'rew (w + 1))
                                       ; random FIRST: at the first entry bph ~= the current
                                       ; phases, so leading with best would waste the one
                                       ; early escape that wins the SAT instances. over a
                                       ; 14-instance panel the orderings tie on net (85s vs
                                       ; 88s) with opposite trades (best-first -21% UNSAT,
                                       ; random-first -5x SAT tail risk); random-first has
                                       ; the lower variance and protects the raced rows.
                                       (? (0 = (w % 4)) (frephase s st)
                                          (1 = (w % 4)) (: _ (fbest s) st)
                                          (2 = (w % 4)) (: _ (finvert s) st)
                                          (: _ (fbest s) st)))
                                    st))
                              st)
                       _ (? (1 = peep s 'mode 0)
                            (: sc (peep s 'sclimit 2000)
                               _ (pin s 'sclimit ((sc * 6) // 5))
                               (pin s 'climit sc))
                            0)
                       (? (0 = peep s 'bad 0) (loop st2) 'unsat))     ; a compaction can prove UNSAT (all-false at level 0)
                 (: v (s 'dec  fx)
                    (v = 0 ? 'sat (loop st)))))) (wheel 42)))
   (fextract s nvars) (: fx (peep s 'fx ()) vao (peep s 'vao 0)
                         (map (\ v (v >< (? (0 = (getw fx (vao + v) & 1)) 1 -1))) (rng 1 (nvars + 1))))
   ; --- BVA: bounded variable ADDITION (clause factoring), the preprocessing pass ---
   ; the ablation-found pigeonhole killer: cadical minus factoring
   ; collapses to minisat-class. for a literal l, GROW a set of literals M_lit and a set
   ; of clauses M_cls (all containing l) such that (m | C\{l}) is in the formula for
   ; every pairing; then a fresh variable x replaces the |M_lit|*|M_cls| clauses with
   ; |M_cls| clauses (x | C\{l}) + |M_lit| definitions (-x | m). resolving on x recovers
   ; every original (UNSAT preserved); a model extends by x := AND of M_lit (SAT
   ; preserved); models of the new formula project by DROPPING vars > nvars. adding
   ; definitional variables is the extended-resolution move -- the proof system where
   ; pigeonhole has POLYNOMIAL refutations (Cook 1976), which plain resolution (and so
   ; ANY amount of clause learning) provably lacks (Haken 1985).
   fbva0 {1}                                                ; pin 0 to solve raw (the A/B switch)
   fbvar0 {8}                                               ; BVA round cap
   fbvac0 {1}                                               ; 1 = factor ORIGINAL structure only (no aux cascade)
   ; the pass runs over FLAT state, and its hot loop -- the occ-probe / match-count
   ; machinery inside `try` -- is the FOURTH native kernel (fbvag; fbvag3 the honest
   ; twin, phase for phase). a literal is an OFFSET KEY: signed s -> s + NB, so key
   ; order IS signed order and the hot walks compare words directly. nvars buckets
   ; to a power of 2 (the kernel cache survives the ladder's varying counts) and NB
   ; = 12*bucket+256 bounds every var a call can mint: a round starts under
   ; 4*nvars+64, tries at most one lit per candidate, each adding at most one var.
   ; bx header (24 words): w0 aru (clause-arena top) w1 onu (occ-node top) w2 nv
   ; w3 pnu (match-node top) w4 mt (empty clause seen); the kernel seam: w5 l
   ; w6 chain-in w7 mode (1 = build M_cls from occ[l], 0 = one grow level) w8/w9
   ; the ORIGINAL-variable key band (sentinels 0/4NB pass everything when the aux
   ; cascade is on) w10 NB; outs w11 best-key w12 count w13 chain-out; w14+ kernel
   ; scratch. then five sections indexed by key, 2*NB+2 words each: HHO occ heads
   ; | OCO raw occurrence counts (adds only -- the min-occ heuristic wants the
   ; UNPRUNED tally) | LCO live counts (adds bump, deletes drop: the round sweep
   ; reads a counter) | PCO the per-level match counts (pass 2 re-walks and zeroes:
   ; state-clean without a touched list) | MLO the M_lit marks. a clause lays
   ; [size, key0..] in ba, sorted; a tombstone zeroes the size and stashes the true
   ; length at cid+1 (the freduce discipline -- occ chains are never pruned, every
   ; walk checks the size first). occ / match nodes are [cid, next] in bo / bp
   ; (0 = nil; words 0-1 reserved). the kernel never grows a cask: the driver
   ; pre-ensures bp room (build: 2*OCO[l]; grow: 2*level-length) before each call.
   ; --- the grow-step twin -- (fbvag3 bx ba bo bp), the kernel's spec. build mode
   ; chains occ[l]'s live in-band clauses in WALK order (tail-append) -> count.
   ; grow mode, pass 1: per clause, k = fewest-occurrence lit beside l; per occ[k]
   ; partner of equal size, the ONE key beyond C\{l} (a sorted two-pointer merge);
   ; in-band, unmarked keys count into PCO under a RUNNING best (count, ties ->
   ; lowest key -- increments reach the final max, so the running max IS the
   ; argmax). pass 2 (only when a best exists) re-walks, zeroing PCO on sight and
   ; chaining the best key's clauses (front-push = the walk's own order reversed,
   ; the pl discipline) -> best key, count + chain in w11-w13.
   (fbvag3 bx2 ba2 bo2 bp2)
    (: NB2 (getw bx2 10) s2 ((2 * NB2) + 2)
       hho 24 oco (hho + s2) pco (hho + (3 * s2)) mlo (hho + (4 * s2))
       l (getw bx2 5) lo (getw bx2 8) hi (getw bx2 9)
       _ (putw bx2 11 0) _ (putw bx2 12 0) _ (putw bx2 13 0)
       (? (1 = getw bx2 7)
          ((: (bld q cnt tail)                              ; build M_cls: live, in-band, walk order
              (q = 0 ? (putw bx2 12 cnt, cnt)
                 (: did (getw bo2 q) qn (getw bo2 (q + 1))
                    n2 (getw ba2 did)
                    ok (n2 >= 2 &&
                        (: (ofr j) (j > did + n2 || lo <= getw ba2 j <= hi && ofr (j + 1)))
                        (did + 1))
                    (!ok ? (bld qn cnt tail)
                       (: p (getw bx2 3) _ (putw bx2 3 (p + 2))
                        (putw bp2 p did, putw bp2 (p + 1) 0,
                         tail = 0 ? (putw bx2 13 p) (putw bp2 (tail + 1) p),
                         bld qn (cnt + 1) p))))))
           (getw bx2 (hho + l)) 0 0)
          (: (mrg cid did n)                                ; the ONE key of d beyond c\{l}, -1 if none
              ((: (go i j x)
                  (? (i > cid + n)
                     (? (x > 0)         (j > did + n ? x -1)
                        (j = did + n) (getw ba2 j)
                        -1)
                     (: ci (getw ba2 i)
                        (? (ci = l) (go (i + 1) j x)
                           (j > did + n) -1
                           (: cj (getw ba2 j)
                              (? (ci = cj) (go (i + 1) (j + 1) x)
                                 (cj < ci) (x > 0 ? -1 (go i (j + 1) cj))
                                 -1))))))
               (cid + 1) (did + 1) 0)
             (bmk2 cid n)                                   ; fewest-occurrence lit of the clause, l aside
              ((: (go j k kc)
                  (? (j > cid + n) k
                     (: e2 (getw ba2 j)
                        (e2 = l ? (go (j + 1) k kc)
                           (: ec (getw bx2 (oco + e2))
                              (? (ec < kc) (go (j + 1) e2 ec) (go (j + 1) k kc)))))))
               (cid + 1) 0 2147483647)
             (walk p act)                                   ; the shared two-pass skeleton
              (? (p = 0) 0
                 (: cid (getw bp2 p) pn (getw bp2 (p + 1))
                    n (getw ba2 cid) k (bmk2 cid n)
                    _ ((: (ow q)
                          (? (q = 0) 0
                             (: did (getw bo2 q) qn (getw bo2 (q + 1))
                                _ (? !(n = getw ba2 did) 0     ; (covers tombstones: size 0)
                                     (did = cid) 0
                                     (: m (mrg cid did n)
                                        (? (m < 0) 0
                                           (m < lo || m > hi) 0
                                           (1 = getw bx2 (mlo + m)) 0
                                           (act cid m))))
                                (ow qn))))
                       (getw bx2 (hho + k)))
                    (walk pn act)))
             _ (walk (getw bx2 6)
                 (\ cid m (: c2 (1 + getw bx2 (pco + m))
                             _ (putw bx2 (pco + m) c2)
                             bb (getw bx2 12)
                             (? (c2 > bb || ((c2 = bb) && (m < getw bx2 11)))
                                (: _ (putw bx2 11 m) (putw bx2 12 c2)) 0))))
             bk (getw bx2 11)
             (? (bk = 0) 0
                (: _ (walk (getw bx2 6)
                       (\ cid m (: _ (putw bx2 (pco + m) 0)
                                   (? !(m = bk) 0
                                      (: p (getw bx2 3) _ (putw bx2 3 (p + 2))
                                         _ (putw bp2 p cid) _ (putw bp2 (p + 1) (getw bx2 13))
                                         (putw bx2 13 p))))))
                   bk)))))
   (fbva formula nvars)                                     ; -> (formula2 . nvars2), equisatisfiable
    (: NB (12 * (: (bkt k) (k >= nvars ? k (bkt (2 * k)))) 16 + 256)
       s2 (2 * NB + 2)
       HHO 24  OCO (HHO + s2)  LCO (OCO + s2)  PCO (LCO + s2)  MLO (PCO + s2)
       fo (peep fbvac0 () 0)
       LO (1 = fo ? (NB - nvars) 0)                       ; the ORIGINAL-variable key band
       HI (1 = fo ? (NB + nvars) (4 * NB))                ; (sentinels: everything passes)
       bx (cask (8 * (MLO + s2)))
       b {'ba (cask 8192) 'bo (cask 8192) 'bp (cask 4096)}
       _ (putw bx 0 2, putw bx 1 2, putw bx 2 nvars, putw bx 3 2,
          putw bx 8 LO, putw bx 9 HI, putw bx 10 NB)
       ks (fbvagk NB)
       gstep (? (two? ks && (0 = peep fknob () 0)) <ks fbvag3)
       (bcl ar cid)                                         ; the clause back out, sorted signed lits
        (: n (getw ar cid)
           ((: (go j acc) (? (j = cid) acc (go (j - 1) ((getw ar j - NB) >< acc)))) (cid + n) ()))
       (feq e n)                                            ; the live cid holding exactly keys e (size n), or ()
        (: ar (peep b 'ba ()) bo (peep b 'bo ())
           k (foldl (\ a el (? (getw bx (OCO + el) < getw bx (OCO + a)) el a)) <e >e)
           (eqr j e2) (? (atom? e2) 1 !(getw ar j = <e2) 0 (eqr (j + 1) >e2))
           ((: (go p) (? (p = 0) ()
                         (: cid (getw bo p)
                            (? ((n = getw ar cid) && (1 = eqr (cid + 1) e)) cid
                               (go (getw bo (p + 1)))))))
            (getw bx (HHO + k))))
       (addc c0)                                            ; canon + dedupe + lay + index; drops tautologies
        (: c (sort (dedup-lits c0))
           (? (1 = taut? c) 0
              (atom? c) (? (0 = getw bx 4) (: _ (putw bx 4 1) 1) 0)     ; the empty clause rides a flag
              (: e (map (\ s (s + NB)) c) n (tally e)
                 (? (whole? (feq e n)) 0
                    (: _ (fens b 'ba (8 * (getw bx 0 + (n + 1))))
                       ar (b 'ba ) cid (getw bx 0)
                       _ (putw ar cid n)
                       _ (foldl (\ j el (putw ar j el, j + 1)) (cid + 1) e)
                       _ (putw bx 0 (cid + (n + 1)))
                       _ (each e (\ el
                          (fens b 'bo (8 * (getw bx 1 + 2)),
                           : bo (b 'bo) p (getw bx 1)
                           (putw bo p cid, putw bo (p + 1) (getw bx (HHO + el)),
                            putw bx (HHO + el) p,    putw bx 1 (p + 2),
                            putw bx (OCO + el) (1 + getw bx (OCO + el))))))
                       _ (n >= 2 ? (each e (\ el (putw bx (LCO + el) (1 + getw bx (LCO + el))))))
                       1)))))
       (delc cid)
        (: ar (peep b 'ba ()) n (getw ar cid)
           (? (n = 0) 0
              (: _ (? (n < 2) 0                             ; units were never counted
                      ((: (dr j) (? (j > cid + n) 0
                            (: e2 (getw ar j)
                               _ (putw bx (LCO + e2) (getw bx (LCO + e2) - 1)) (dr (j + 1)))))
                       (cid + 1)))
                 _ (putw ar cid 0) _ (putw ar (cid + 1) n) 1)))
       (lcnt el) (getw bx (LCO + el))                       ; live (size >= 2) clauses holding el
       (try l)                                              ; one greedy factoring attempt at key l -> 1 if applied
        (: pnu0 (getw bx 3)                                 ; bp is per-try scratch: top restored on every exit
           _ (putw bx 5 l)
           _ (fens b 'bp (8 * (pnu0 + (2 * getw bx (OCO + l)))))
           _ (putw bx 7 1)
           n0 (? ((l < LO) || l > HI) 0                     ; an out-of-band l factors nothing
                 (gstep bx (peep b 'ba ()) (peep b 'bo ()) (peep b 'bp ())))
           (? (n0 < 4) (: _ (putw bx 3 pnu0) 0)
              (: _ (putw bx (MLO + l) 1)
                 g ((: (grow mlit len chain red)            ; extend M_lit while the reduction strictly grows
                       (: _ (putw bx 6 chain) _ (putw bx 7 0)
                          _ (fens b 'bp (8 * (getw bx 3 + (2 * len))))
                          bk (gstep bx (peep b 'ba ()) (peep b 'bo ()) (peep b 'bp ()))
                          (? (bk = 0) (mlit >< chain)
                             (: nc (getw bx 12) nl (1 + tally mlit)
                                red2 ((nl * nc) - (nl + nc))
                                (? (red2 > red)                   ; max count; ties -> LOWEST literal
                                   (: _ (putw bx (MLO + bk) 1)    ; (symmetric instances: groups stay contiguous)
                                      (grow (bk >< mlit) nc (getw bx 13) red2))
                                   (mlit >< chain))))))
                    (l >< ()) n0 (getw bx 13) -1)
                 mlit <g
                 bp2 (peep b 'bp ())
                 mcls ((: (go p) (? (p = 0) () (getw bp2 p >< go (getw bp2 (p + 1))))) >g)
                 _ (foldl (\ _ m (: _ (putw bx (MLO + m) 0) 0)) 0 mlit)
                 _ (putw bx 3 pnu0)
                 ar (peep b 'ba ())
                 nl (tally mlit) ncl (tally mcls)
                 (? (((nl * ncl) - (nl + ncl)) < 1) 0       ; apply only when it strictly shrinks the formula
                    (: x (1 + getw bx 2) _ (putw bx 2 x)
                       ml (map (\ m (m - NB)) mlit)         ; signed, most-recent first (l last)
                       rs (map (\ cid (rem-lit (l - NB) (bcl ar cid))) mcls)
                       ; the proof lines, in RAT-checkable order: the definitions
                       ; (-x m) while NO clause carries x (blocked on -x), then the
                       ; x-clauses (RAT on x: every resolvent is an original (m r),
                       ; still present, or a tautology), then the originals deleted
                       _ (: j (peep fdrat0 () 0)
                            (? (hot? j)
                               (: _ (foldl (\ _ m (: _ (fdsay j "" [(0 - x) m]) 0)) 0 ml)
                                  _ (fdsay j "" (x >< map (\ m (0 - m)) ml))
                                  _ (foldl (\ _ r (: _ (fdsay j "" (x >< r)) 0)) 0 rs)
                                  (foldl (\ _ m (foldl (\ _ r (: _ (fdsay j "d " (m >< r)) 0)) 0 rs)) 0 ml))
                               0))
                       _ (foldl (\ _ m (: _ (foldl (\ _ r
                              (: k (sort (m >< r)) e2 (map (\ s (s + NB)) k)
                                 did (feq e2 (tally e2))
                                 (? (whole? did) (delc did) (scare 'bva-miss k))))
                            0 rs) 0)) 0 ml)
                       _ (foldl (\ _ r (: _ (addc (x >< r)) 0)) 0 rs)
                       _ (foldl (\ _ m (: _ (addc ((0 - x) >< (m >< ()))) 0)) 0 ml)
                       _ (addc (x >< map (\ m (0 - m)) ml))       ; the OTHER direction: x <- AND(m) --
                       1)))))                                      ; a COMPLETE definition (pure extended
                                                                   ; resolution), so x propagates instead of
                                                                   ; wandering: without it CDCL decides x freely
       _ (foldl (\ _ c (: _ (addc c) 0)) 0 formula)
       _ ((: (round r)                                      ; sweep most-occurring literals first; stop on a quiet
             (: nv2 (getw bx 2)                             ; round OR when the cumulative find is a handful (a sweep
                ls (sortby (\ a2 b2 (? (<a2 = <b2) (>a2 <= >b2) (<a2 > <b2)))   ; costs a full pass; an
                           ((: (go el acc)                                       ; incidental 6-of-300 is
                               (? (el > (NB + nv2)) acc                          ; not worth another)
                                  (: c (lcnt el)
                                     (go (el + 1) (? (c >= 4) ((c >< (el - NB)) >< acc) acc)))))
                            (NB - nv2) ()))
                ch (foldl (\ a2 p (a2 + try (>p + NB))) 0 ls)
                (? (ch > 0 &&
                    ((16 * (getw bx 2 - nvars)) > nvars) &&
                    (r < peep fbvar0 () 8) && (getw bx 2 < ((4 * nvars) + 64)))
                   (round (r + 1)) 0)))
           0)
       ar (peep b 'ba ()) end (getw bx 0)
       out ((: (walk cid acc)
               (? (cid >= end) (rev acc)
                  (: n (getw ar cid)
                     (? (n = 0) (walk (cid + (getw ar (cid + 1) + 1)) acc)
                        (walk (cid + (n + 1)) (bcl ar cid >< acc))))))
            2 ())
       ((? (1 = getw bx 4) (() >< out) out) >< getw bx 2))
   (fcdcl formula nvars)
    ; the fbva LADDER: re-run the pass on its own output with the baseline lifted (level-1
    ; aux become "original" for level 2) while it keeps factoring SUBSTANTIALLY (> nvars/16
    ; fresh vars -- an incidental handful, e.g. flat graph coloring's 6-of-300, is not a
    ; commander hierarchy and another pass is pure intake cost) -- level 2 deepens the
    ; commander tree (php8 ~1550 -> ~1400 conflicts under the reason-side bump; WITHOUT
    ; that bump a deeper tree actively hurts, so the ladder rides the bump). an instance
    ; with nothing factorable exits after one pass (randoms: no ladder cost). models
    ; still project by the ORIGINAL nvars.
    (: p (? (0 = peep fbva0 () 1) (formula >< nvars)
            ((: (go p k) (: p2 (fbva <p >p)
                            (? (((16 * (>p2 - >p)) <= >p) || (k >= 3)) p2 (go p2 (k + 1)))))
             (formula >< nvars) 1))
       s (fmk <p >p)
       r (? (1 = peep s 'bad 0) 'unsat
            (: r (fsearch s) (? (id? r 'unsat) 'unsat (fextract s nvars))))
       j (peep fdrat0 () 0)                                  ; cap an UNSAT proof with the empty clause
       _ (? (hot? j && id? r 'unsat) (fdsay j "" ()) 0)
       r)
   (count-fcdcl formula nvars)                              ; #SAT by blocking clauses, on the flat engine
    ((: (go fm c) (: r (fcdcl fm nvars) (? (id? r 'unsat) c (go (block r >< fm) (c + 1))))) formula 0)
   (fdiff seed n m)                                         ; flat CDCL == the DPLL oracle (and models check)
    (: f (gen-formula seed n m) d (solve f n) c (fcdcl f n)
       dsat !(id? d 'unsat) csat !(id? c 'unsat)
       (? !(dsat = csat) 0  !csat 1  (sat? c f)))
   (ffuzz k n m) (all (\ i (1 = fdiff (1009 + (i * 7919)) n m)) (jot k))
   (fboth f n)                                              ; kernels and twins agree, verdict for verdict
    (: _ (pin fknob () 1) a (fcdcl f n) _ (pin fknob () 0) b (fcdcl f n)
       (? (id? a 'unsat) (id? b 'unsat) (!(id? b 'unsat) && sat? b f)))

   (fbvaboth f n)                                           ; the BVA PASS itself, kernel vs twin.
    (: _ (pin fknob () 1) a (fbva f n)                      ; fboth compares VERDICTS, and a verdict
       _ (pin fknob () 0) b (fbva f n) (a = b))              ; survives a wrong transform -- an
                                                             ; equisatisfiable formula still solves.
                                                             ; this compares the transform itself.

   ; --- the in-gate DRAT checker: RUP + RAT on the first literal, a from-scratch
   ; reimplementation of the CHECKING side (shares nothing with the emitter but
   ; fd-parse's inverse relation to fdsay). naive O(lines * db) unit propagation --
   ; the gate runs it on small refutations; drat-trim (crew/sat/dratcheck.sh) is the
   ; independent external checker for the bench-scale ones.
   (fd-parse txt)                                           ; DRAT text -> steps (tag . lits), tag 1 = delete
    (: n (tally txt)
       ((: (go i tag num sgn lits steps)
           (? (i >= n) (rev steps)
              (: ch (txt i)
                 (? (ch = 100) (go (i + 1) 1 0 0 lits steps)
                    (ch = 45) (go (i + 1) tag 0 -1 lits steps)
                    (48 <= ch <= 57)
                     (go (i + 1) tag (10 * num + ch - 48) (sgn = 0 ? 1 sgn) lits steps)
                    (sgn = 0) (go (i + 1) tag 0 0 lits steps)
                    (: v (num * sgn)
                       (? (v = 0) (go (i + 1) 0 0 0 () ((tag >< rev lits) >< steps))
                          (go (i + 1) tag 0 0 (v >< lits) steps)))))))
        0 0 0 0 () ()))
   (fd-up db asg)                                           ; propagate units over asg; -> 1 conflict / 0 fixpoint
    ((: (pass rest ch)
        (? (atom? rest) (ch @ 1 (pass db 0) 0)
           (: r ((: (scan c sat un)
                    (? (atom? c) (? (sat = 1) 'sat (? (atom? un) 'conf un))
                       (: l <c
                          (? (1 = peep asg l 0) (scan >c 1 un)
                             (1 = peep asg (neg l) 0) (scan >c sat un)
                             (scan >c sat (l >< un))))))
                 <rest 0 ())
              (? (id? r 'conf) 1
                 (id? r 'sat) (pass >rest ch)
                 (atom? >r) (pin asg <r 1, pass >rest 1)
                 (pass >rest ch)))))
     db 0)
   (fd-rup? db c)                                           ; does the negation propagate to a conflict?
    (fd-up db (foldl (a \ l \ pin a (neg l) 1) {} c))
   (fd-rat? db c)                                           ; RAT on <c: every resolvent taut or RUP
    (? (atom? c) 0
       (: p <c
          (all (\ d (? (member? (neg p) d)
                       (: rv (>c + rem-lit (neg p) d)
                          (? (1 = taut? rv) 1 (fd-rup? db rv)))
                       1))
               db)))
   (fd-del db c)                                            ; drop ONE clause equal as a set
    (: k (sort c)
       ((: (go d) (? (atom? d) ()
                     (sort <d = k) >d
                     (<d >< go >d))) db))
   (fd-check f proof)                                       ; 1 iff every step checks and one derives ()
    ((: (go db ps)
        (? (atom? ps) 0
           (: st <ps c >st
              (? (1 = <st) (go (fd-del db c) >ps)
                 (: ok (1 = fd-rup? db c || fd-rat? db c)
                    (? !ok 0
                       (atom? c) 1
                       (go (c >< db) >ps)))))))
     (map dedup-lits f) proof)

 ; --- the gate: the flat engine answers exactly like the oracle, kernels == twins
 _ (assert
    ; tiny SAT / UNSAT
    (: f '((1 2) (-1 3) (-2 -3)) m (fcdcl f 3) (!(id? m 'unsat) && sat? m f))
    (id? (fcdcl '((1) (-1)) 1) 'unsat)
    (id? (fcdcl '((1 2) (1 -2) (-1 2) (-1 -2)) 2) 'unsat)
    (id? (fcdcl '((1 2 3) (1 2 -3) (1 -2 3) (1 -2 -3) (-1)) 3) 'unsat)
    ; cyclic: SAT, the model checks, and #SAT == Lucas (solver-independent)
    (: f (cyclic 12) m (fcdcl f 12) (!(id? m 'unsat) && sat? m f))
    (count-fcdcl (cyclic 8) 8 = lucas 8)
    (count-fcdcl (cyclic 10) 10 = lucas 10)
    ; differential fuzz vs the DPLL oracle, mixed SAT/UNSAT near the phase transition
    (ffuzz 60 8 30)
    (ffuzz 40 10 30)
    (ffuzz 30 12 50)
    (ffuzz 20 14 58)
    ; native kernels == interpreted twins, verdict for verdict (a no-op switch where they can't build)
    (fboth (cyclic 14) 14)
    (fboth (php 4) (php-vars 4))
    (fboth (php 5) (php-vars 5))
    ; ..and the BVA TRANSFORM agrees, not just the verdict it leads to (php reaches
    ; fbvag3's build mode ~50-100 times; cyclic exercises the no-factoring exit).
    (fbvaboth (php 4) (php-vars 4))
    (fbvaboth (php 5) (php-vars 5))
    (fbvaboth (cyclic 14) 14)
    ; pigeonhole PHP(5): UNSAT + resolution-hard (learning, restarts, arena growth end to end)
    (id? (fcdcl (php 5) (php-vars 5)) 'unsat)
    ; DRAT: an UNSAT solve's emitted proof re-checks against the ORIGINAL formula --
    ; pure RUP on the tiny ones, BVA's RAT lines on php(4), `d` lines under a tiny
    ; learnt cap. fd-check is the in-gate eye; crew/sat/dratcheck.sh aims drat-trim at php.
    (: f '((1) (-1)) j (jug 0) _ (pin fdrat0 () j) r (fcdcl f 1) _ (pin fdrat0 () 0)
       (id? r 'unsat && (1 = fd-check f (fd-parse (slurp j)))))
    (: f '((1 2) (1 -2) (-1 2) (-1 -2)) j (jug 0) _ (pin fdrat0 () j) r (fcdcl f 2) _ (pin fdrat0 () 0)
       (id? r 'unsat && (1 = fd-check f (fd-parse (slurp j)))))
    (: f (php 4) j (jug 0) _ (pin fdrat0 () j) r (fcdcl f (php-vars 4)) _ (pin fdrat0 () 0)
       ps (fd-parse (slurp j))
       bad (map (\ st (? ((0 = <st) && (2 = tally >st))      ; negate a literal in every
                         (0 >< ((0 - <>st) >< >>st)) st))      ; 2-lit add line:
            ps)                                                        ; the checker must refuse
       (id? r 'unsat && (1 = fd-check f ps) && (0 = fd-check f bad)))
    (: keep (peep fmaxl0 () 0) _ (pin fmaxl0 () 24)
       f (php 4) j (jug 0) _ (pin fdrat0 () j) r (fcdcl f (php-vars 4))
       _ (pin fdrat0 () 0) _ (pin fmaxl0 () keep)
       (id? r 'unsat && (1 = fd-check f (fd-parse (slurp j)))))
    ; force the learnt-DB REDUCTION path hard (a tiny cap -> reduction every few restarts)
    ; and re-run the differential battery through it, both engines
    (: keep (peep fmaxl0 () 0) _ (pin fmaxl0 () 24)
       ok (id? (fcdcl (php 5) (php-vars 5)) 'unsat &&
           ffuzz 30 12 50 && fboth (php 4) (php-vars 4))
       _ (pin fmaxl0 () keep) ok)
    ; force the REAL COMPACTION path the same way (a tiny arena threshold -> fcompact at
    ; every level-0 restart; its all-false-at-level-0 UNSAT verdict and cid remapping are
    ; only reachable here -- an untested fcompact once shipped a bogus SAT on uuf250)
    (: keep (peep fcmp0 () 0) _ (pin fcmp0 () 64)
       ok (id? (fcdcl (php 5) (php-vars 5)) 'unsat &&
           ffuzz 30 12 50 && (count-fcdcl (cyclic 10) 10 = lucas 10))
       _ (pin fcmp0 () keep) ok))
 _ (say err ("crew/sat/flat: ok -- flat-arena CDCL == DPLL on 150 fuzzed + PHP(5) + #SAT==Lucas"
             + (? (two? (fbcpk 8)) " (native bcp+conf+dec+bvag kernels live)\n" " (interpreted twins)\n"))))
