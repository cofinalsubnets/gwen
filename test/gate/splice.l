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
   samples (L ('sm-cup4 >< sm-cup4) ('sm-pred >< sm-pred) ('sm-caps >< sm-caps)
              ('sm-deep >< sm-deep) ('sm-mix >< sm-mix))
   (die m) (: _ (say err ("splice-gate: " + m + "\n")) _ (flush err) (quit 1))
   (one s a)                                     ; a is (composed declined)
    (: nm (cap s) f (cup s) j (jit f)
       (? (nil? j) (: _ (say out ("  " + string nm + ": declined (a row the table cannot say)\n"))
                      ((cap a) >< ((cuap a) + 1)))
          (: bad (filter (\ x !(= (j x) (f x))) ins)
             _ (? (nil? bad) 0 (die (string nm + " DISAGREES with its twin on " + show bad)))
             _ (say out ("  " + string nm + ": agrees with its twin on " + show (tally ins) + " inputs\n"))
             (((cap a) + 1) >< (cuap a)))))
   ; the IR the compiler wrote down: `mooncc -fir=lvm_` lays every op handler under the splice
   ; cap into .rodata, and this love reads its OWN. ⚠ the count is not asserted (it moves with
   ; love.c); what is asserted is that the table exists, sounds back, and carries a known op --
   ; a table that read as empty would look exactly like a compiler that was never asked.
   it (jit-irtab ())
   _ (? (two? it) 0 (die "no ai_lvm_ir in this binary -- was love.o built with -fir=lvm_?"))
   _ (: cf (jit-ir 'lvm_cup)
        (? (two? cf) (say out ("  ai_lvm_ir: " + show (caup (cup it)) + " handlers, lvm_cup at "
                               + show (tally cf) + " forms\n"))
           (die "ai_lvm_ir carries no lvm_cup")))
   r (foldl (\ a s (one s a)) (0 >< 0) samples)
   n (cap r) d (cuup r)
   _ (say out ("splice: " + show n + " of " + show (tally samples) + " samples jitted and agree"
               + (? (0 < d) (", " + show d + " declined") "") + "\n"))
   _ (? (0 < n) 0 (die "no sample composed -- the pipeline did not run"))
   _ (flush out)
   (quit 0))
