; tools/dest2uu.l -- emit test/uuhomgen.l: doc/misc/proto/dest.l's two generators (cv
; the r0 protocol, ck the destination die) run on dest.l's own law-site samples,
; the emissions lifted to uu nlist terms so test/uuhomlaw.l can prove the laws
; of them (substrate: test/uuhom.l). Run on the host binary:
;
;   love tools/dest2uu.l > test/uuhomgen.l        (or: make uuhomgen)
;
; the lift: 4 nats per instruction (op d a b, 0-padded); operands parity-tagged
; (reg r = 2r, imm k = 2k+1); the sp shuttle idioms fold to push/pop. dest.l's
; `:` forms are eval'd (its samples s1..e5 leak from their body-less binders, so
; the roster below is dest.l's), its law asserts run as a gate; only the e5 bin
; expression is copied, dest.l holding it inline. the lift is a translator --
; untrusted like wm2uu: a wrong lift can miss a law, never fake a theorem the
; laws file doesn't ask, and the artifact prints each source form beside its
; term for the reading.
; load dest.l first, in its own top-level form: the next form's bare cv/cvs/ck/cks
; references resolve at its compile, which must follow this run. reads carries the
; top-level leak (uu2coq's door); out rides a jug meanwhile -- dest.l's own asserts
; gate the run, its "dest: ok" stays out of the artifact.
(: j (jug 0) _ (wear [in j err])
   _ (reads (open "doc/misc/proto/dest.l" "r"))
   _ (wear ()) ())

(: regs '((r0 0) (r1 1) (r2 2) (r3 3) (r4 4) (r5 5) (r6 6) (r7 7)
          (r8 8) (r9 9) (r10 10) (r11 11) (sp 12))
   rels '((lt 0) (ge 1) (le 2) (gt 3) (eq 4) (ne 5))
   (look t k) (: e (assoc k t) (? (two? e) (cap (cup e)) (scare 'dest2uu-look k)))
   ; labels and frame offsets take codes as met, in emission order -- presence is
   ; the (1 x) wrapper, a code may be 0
   LAB (tablet 0) OFF (tablet 0)
   (dyn t x) (: e (peep t x ())
                (? (two? e) <e
                   (: k (peep t 'n 0) _ (pin t 'n (k + 1)) _ (pin t x [k]) k)))
   (tr r) (2 * (look regs r))
   (ti k) (1 + 2 * k)
   (toff o) (ti (dyn OFF o))
   (tlab l) (ti (dyn LAB l))
   (tx x) (? (charm? x) (ti x) (tr x))
   (each f l) (? (two? l) (: _ (f <l) (each f >l)) ())

   ; the shuttle idioms, folded before the lift: shov -> push X, shpo -> pop X
   (plift fs)
    (? !(two? fs) ()
       (? (&& (id? (<fs 0) 'sub) (id? (<fs 1) 'sp) (two? >fs)
              (id? ((cap >fs) 0) 'st) (id? ((cap >fs) 1) 'sp))
          (link ['push ((cap >fs) 3)] (plift (cup >fs)))
          (&& (id? (<fs 0) 'ld) (id? (<fs 2) 'sp) (two? >fs)
              (id? ((cap >fs) 0) 'add) (id? ((cap >fs) 1) 'sp))
          (link ['pop (<fs 1)] (plift (cup >fs)))
          (link <fs (plift >fs))))
   (lift1 f)
    (: h (f 0)
       (? (id? h 'li)    [0 (tr (f 1)) (tx (f 2)) 0]
          (id? h 'mov)   [1 (tr (f 1)) (tr (f 2)) 0]
          (id? h 'ld)    [2 (tr (f 1)) (tr (f 2)) (toff (f 3))]
          (id? h 'st)    [3 (tr (f 1)) (toff (f 2)) (tr (f 3))]
          (id? h 'si)    [4 (tr (f 1)) (toff (f 2)) (ti (f 3))]
          (id? h 'add)   [5 (tr (f 1)) (tx (f 2)) (tx (f 3))]
          (id? h 'sub)   [6 (tr (f 1)) (tx (f 2)) (tx (f 3))]
          (id? h 'cmp)   [7 (tx (f 1)) (tx (f 2)) 0]
          (id? h 'set)   [8 (ti (look rels (f 1))) (tr (f 2)) 0]
          (id? h 'br)    [9 (ti (look rels (f 1))) (tlab (f 2)) 0]
          (id? h 'jmp)   [10 (tlab (f 1)) 0 0]
          (id? h 'label) [11 (tlab (f 1)) 0 0]
          (id? h 'call)  [12 (tlab (f 1)) 0 0]
          (id? h 'ret)   [13 0 0 0]
          (id? h 'push)  [14 (tr (f 1)) 0 0]
          (id? h 'pop)   [15 (tr (f 1)) 0 0]
          (scare 'dest2uu-op h)))

   (pre fs) (? !(two? fs) ()
               (? (member? ((cap fs) 0) '(call jmp ret)) () (link <fs (pre >fs))))
   (emit1 g)
    (: nm (g 0) fs (plift (g 1))
       _ (each (\ f (puts (+ (+ "\n ; " (string nm)) (+ "  " (show f))))) fs)
       _ (puts (+ "\n (defn " (+ (string nm) " nlist")))
       _ (each (\ f (: q (lift1 f)
                      (puts (+ "\n   " " "
                             (+ "(ncons " (+ (show (q 0))
                              (+ " (ncons " (+ (show (q 1))
                               (+ " (ncons " (+ (show (q 2))
                                (+ " (ncons " (show (q 3)))))))))))))
             fs)
       _ (puts (+ "\n    nnil" (+ (")" * (4 * (tally fs))) ")")))
       ())

   eb '(bin add (v y) (k 1))                    ; dest.l's e5 bin, held inline there
   _ (relab 0)  a3 (cvs s3 e3)
   _ (relab 0)  b3 (cks s3 e3 P)
   a4 (cvs s4 e4)  b4 (cks s4 e4 P)
   progs [['ds1cv (cvs s1 e1)]    ['ds1ck (cks s1 e1 P)]
          ['ds2cv (cvs s2 e2)]    ['ds2ck (cks s2 e2 P)]
          ['ds3cv a3]             ['ds3ck b3]
          ['ds4cv a4]             ['ds4ck b4]
          ['ds4cvargs (pre a4)]   ['ds4ckargs (pre b4)]
          ['de5kcv (cv '(k 7) e5)]   ['de5kck (ck '(k 7) e5 P ())]
          ['de5vcv (cv '(v z) e5)]   ['de5vck (ck '(v z) e5 P ())]
          ['de5bcv (cv eb e5)]       ['de5bck (ck eb e5 P ())]]

   _ (puts "; test/uuhomgen.l -- GENERATED from doc/misc/proto/dest.l by tools/dest2uu.l. Do not")
   _ (puts "\n; edit; refresh with `make uuhomgen`. The law-site programs, cv (r0 protocol)")
   _ (puts "\n; and ck (destination die), lifted to uu nlist terms -- 4 nats per instruction")
   _ (puts "\n; (op d a b), operands parity-tagged (reg r = 2r, imm k = 2k+1), frame offsets")
   _ (puts "\n; and labels coded as met, opcodes li0 mov1 ld2 st3 si4 add5 sub6 cmp7 set8")
   _ (puts "\n; br9 jmp10 label11 call12 ret13 push14 pop15 (the sp shuttle rides push/pop).")
   _ (puts "\n; Each source form is printed over its term. Laws: test/uuhomlaw.l; substrate:")
   _ (puts "\n; test/uuhom.l over test/uu.l (the kernel).")
   _ (puts "\n(assert")
   _ (each emit1 progs)
   _ (puts ")\n")
   0)
