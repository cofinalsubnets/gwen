; test/gate/splice.l -- the splice JIT's gate: `(jit f)` reads a live closure's op rows, takes
; each op's own IR out of THIS BINARY's .rodata (where `mooncc -fir=lvm_` wrote it), splices
; them into one body, assembles it with holo, binds the one external reference to a live
; address and nifs it -- and the result must agree with the closure it came from on every
; input. ⚠ a PLAIN love runs this: no compiler, no source tree, no object file. LOVE_NO_GLAZE=1
; because a glazed closure is a native cell `dis` reads as a husk.
;
; ⚠ THE DIFFERENTIAL IS THE WHOLE GATE. A composed body is never allowed to be wrong, only
; faster: the twin is the SAME closure, so this is `sl-cross` one level down -- one
; denotation, two presentations. A disagreement is a codegen bug in mooncc, in the op-body
; extraction, or in the bind, and all three are silent otherwise.
;
; ⚠ A DECLINED SAMPLE IS NOT A FAILURE and is named rather than skipped: coverage is a
; measured gap (an op that jumps, applies or allocates ends the straight line). What fails
; is a disagreement, a bind that dies, or ZERO samples composed -- the last one because a
; gate whose subject never ran reads exactly like a gate that passed.
(: _ (use 'splice)
   ; the inputs: a charm, the zero point, chains of four shapes, and text -- every sample
   ; here walks its argument, so a shape that ends the walk early is the interesting one
   ins (L 7 0 () '(1) '(1 2 3) '(1 2 3 4 5) '(1 2 3 4 5 6 7 8 9 10) '((1 2) (3 4) 5) "text")
   (sm-cup4 x) (cup (cup (cup (cup x))))
   (sm-pred x) (two? (cup (cup x)))
   (sm-caps x) (cap (cup (cap (cup x))))
   (sm-deep x) (cup (cup (cup (cup (cup (cup (cup (cup x))))))))
   (sm-mix x) (cap (cup (cup (cap x))))
   ; ⚠ the MULTI-ARG rows, and they are not decoration: arity was 90% of the corpus census,
   ; and the frame they index is the one thing an arity-1 sample can never exercise -- a
   ; `cur` row misread would land the whole body one slot off and still answer for sm-cup4.
   ; ⚠ the second reads its SECOND parameter on purpose: an off-by-one frame reads the first.
   (sm-two a b) (cup (cap a))
   (sm-2nd a b) (cup (cup b))
   (sm-thr a b c) (cap (cup b))
   samples [['sm-cup4 1 sm-cup4] ['sm-pred 1 sm-pred] ['sm-caps 1 sm-caps]
            ['sm-deep 1 sm-deep] ['sm-mix 1 sm-mix]
            ['sm-two 2 sm-two] ['sm-2nd 2 sm-2nd] ['sm-thr 3 sm-thr]]
   (sat-ap f n x) (? (n = 1) (f x) (n = 2) (f x '(8 9)) (f x '(8 9) 5))   ; saturate at the row's arity
   (die m) (: _ (say err ("splice-gate: " + m + "\n")) _ (flush err) (quit 1))
   (one s a)                                     ; a is (composed declined)
    (: nm <s ar <>s f <>>s j (jit f)
       (? (nil? j) (: _ (say out ("  " + string nm + ": declined (a row the table cannot say)\n"))
                      ((cap a) >< ((cuap a) + 1)))
          (: bad (filter (\ x !(= (sat-ap j ar x) (sat-ap f ar x))) ins)
             _ (? (nil? bad) 0 (die (string nm + " DISAGREES with its twin on " + show bad)))
             _ (say out ("  " + string nm + ": agrees with its twin on " + show (tally ins) + " inputs\n"))
             (((cap a) + 1) >< (cuap a)))))
   ; the IR the compiler wrote down: `mooncc -fir=lvm_` lays every op handler under the splice
   ; cap into .rodata, and this love reads its OWN. ⚠ the count is not asserted (it moves with
   ; love.c); what is asserted is that the table exists, sounds back, and carries a known op --
   ; a table that read as empty would look exactly like a compiler that was never asked.
   ; ⚠ THE REACH LAW, and this is the only gate that can say it: nothing is glazed here, so a
   ; closure has a readable thread. A thread's control flow is entirely internal to it, so
   ; `disg` sees rows the linear `dis` cannot -- an ELSE arm sits past the ret that stops the
   ; linear walk. Equal counts would mean the arms went unread, and every well-formedness law
   ; in test/dis.l would still hold. The other half is CLOSURE: a target names a row that is
   ; there, which is what a collection moving the thread mid-scan would silently break.
   (sm-br x) (? (two? x) (cup x) 0)
   (sm-brn x) (? (two? x) (? (two? (cup x)) 1 2) 3)
   ; ⚠ a row is (idx nom opnd..): the nom is cap(cup(r)). `cuap` is cup∘cap and would take
   ; the cup of the INDEX -- a target then never matches and the closure law holds vacuously.
   (gtgt r) (: h (cap (cup r)) o (cup (cup r))
               (? (id? h 'lvm_argtwocond) (cap (cup o))  (id? h 'lvm_cond) (cap o)  ()))
   (gidx? rows i) (? (atom? rows) 0 (= i (caap rows)) 1 (gidx? (cup rows) i))
   (greach f) (: l (tally (dis f)) g (tally (disg f)) (? (l < g) 1 0))
   (gclosed? f) (: rows (disg f)
                   (go l) (? (atom? l) 1
                             (: t (gtgt (cap l)) (? (whole? t) (? (gidx? rows t) (go (cup l)) 0)
                                                     (go (cup l)))))
                   (go rows))
   _ (? (member? 'disg (names ()))
        (? (all (\ b b) [(greach sm-br) (greach sm-brn) (gclosed? sm-br) (gclosed? sm-brn)])
           (say out ("  disg: the graph outreaches the linear prefix, every target closed ("
                     + show (tally (dis sm-brn)) + " rows -> " + show (tally (disg sm-brn)) + ")\n"))
           (die "disg does not outreach dis, or a branch target names no row"))
        (say out "  disg: absent on this build\n"))
   ; ⚠ THE RECORD MUST BE ONE FUNCTION, and this is a SECOND READING of the tail-jump law --
   ; `make vmret` reads it off a disassembly (objdump, per-arch return mnemonics, its own header
   ; calling it a heuristic, and a silent skip when no disassembler is installed); this reads it
   ; off what the compiler WROTE DOWN, needing no tool and no arch knowledge. Two presentations,
   ; one denotation. ⚠ they disagreed once, and the record was wrong: irblob broke its slice
   ; only on an EXPORTED name, so a file-scope static's body rode into the previous row --
   ; lvm_scare carried missing_tag, lvm_eval carried ap_next, each ending in a stranger's `ret`.
   ; It cost 14 handlers too, inflated past the form cap by a neighbour they had swallowed. And
   ; the splicer survived it only by luck: its `ends in the dispatch tail` check happened to
   ; reject both, where a swallowed neighbour ending the right way would have spliced in whole.
   (ir-foreign fs) (: (go l acc) (? (atom? l) acc
                                    (: f (cap l)
                                       (go (cup l) (? (&& (id? (cap f) 'label) (! (jit-lbl? (caup f))))
                                                      (link (caup f) acc) acc))))
                      (go fs ()))
   (ir-ret? fs) (: (go l) (? (atom? l) 0 (id? (cap (cap l)) 'ret) 1 (go (cup l))) (go fs))
   ; ⚠ THE TAIL-JUMP LAW IS THE VM'S, NOT EVERY FUNCTION'S, and the record now holds every
   ; function -- `-fir` bare, so the binary can be read without a disassembler. An ordinary C
   ; function ENDS in a ret and must; only an lvm_ handler owes the jump. So the ret law reads
   ; the lvm_ family and the one-function law reads them all, which is what each ever meant.
   (lvm? k) (: s (string k) ?((5 <= tally s) && ("lvm_" = snip s 0 4)))
   _ (: t (jit-irtab ())
        tb (? (two? t) (caup t) ())
        ks (? (two? t) (keys tb) ())
        vs (filter lvm? ks)
        fo (filter (\ k (two? (ir-foreign (peep tb k ())))) ks)
        rt (filter (\ k (ir-ret? (peep tb k ()))) vs)
        _ (? (two? fo) (die ("an IR record carries a foreign function label: " + show fo)) 0)
        _ (? (two? rt) (die ("an lvm_ record contains a ret -- the tail-jump law: " + show rt)) 0)
        _ (? (two? vs) 0 (die "the record names no lvm_ handler at all"))
        (say out ("  record: " + show (tally ks) + " functions, each ONE function; "
                  + show (tally vs) + " lvm_ handlers, all ret-free (vmret's law, off the record)\n")))
   it (jit-irtab ())
   _ (? (two? it) 0 (die "no IR record in this binary -- was it built with -fir?"))
   _ (: cf (jit-ir 'lvm_cup)
        (? (two? cf) (say out ("  record: " + show (caup (cup it)) + " splice-able bodies, lvm_cup at "
                               + show (tally cf) + " forms\n"))
           (die "the record carries no lvm_cup")))
   r (foldl (\ a s (one s a)) (0 >< 0) samples)
   n (cap r) d (cuup r)
   _ (say out ("splice: " + show n + " of " + show (tally samples) + " samples jitted and agree"
               + (? (0 < d) (", " + show d + " declined") "") + "\n"))
   _ (? (0 < n) 0 (die "no sample composed -- the pipeline did not run"))
   _ (flush out)
   (quit 0))
