; tools/spl2uu.l -- EMIT test/uusplgen.l: doc/misc/proto/spl.l's three call-site
; compilers (ecall the convention, ebind the binding splice, esubst the
; substitution splice) run on spl.l's own samples, the threads lifted to uu
; nlist terms so test/uuspllaw.l can prove the splice license OF THE EMISSIONS
; (substrate: test/uuhom.l's stack machine). Run on the host binary:
;
;   love tools/spl2uu.l > test/uusplgen.l        (or: make uusplgen)
;
; the lift: 2 nats per op (op x), operands RAW (each op knows its operand's
; kind); opcodes pushk0 pusha1 pushl2 pushs3 setl4 sets5 add6 emit7. spl.l's
; samples leak from its body-less binder, so the roster below IS the model's;
; its own asserts gate the load. untrusted like every translator here: a wrong
; lift can miss a law, never fake one, and each source op prints over its term.
(: j (jug 0) _ (wear [in j err])
   _ (reads (open "doc/misc/proto/spl.l" "r"))
   _ (wear ()) ())

(: ops '((pushk 0) (pusha 1) (pushl 2) (pushs 3) (setl 4) (sets 5) (add 6) (emit 7))
   (opc o) (: e (assoc o ops) (? (two? e) (cap (cup e)) (scare 'spl2uu-op o)))
   (each f l) (? (two? l) (: _ (f <l) (each f >l)) ())
   (emit1 g)
    (: nm (g 0) th ((g 1) ())
       _ (each (\ f (puts (+ (+ "\n ; " (string nm)) (+ "  " (show f))))) th)
       _ (puts (+ "\n (defn " (+ (string nm) " nlist")))
       _ (each (\ f (puts (+ "\n   " " "
                           (+ "(ncons " (+ (show (opc (f 0)))
                            (+ " (ncons " (show (f 1))))))))
               th)
       _ (puts (+ "\n    nnil" (+ (")" * (2 * (tally th))) ")")))
       ())
   progs [['sp0call (ecall (e0 0) (e0 1))]  ['sp0bind (ebind (e0 0) (e0 1))]
          ['sp0subst (esubst (e0 0) (e0 1))]
          ['sp1call (ecall (e1 0) (e1 1))]  ['sp1bind (ebind (e1 0) (e1 1))]
          ['sp1subst (esubst (e1 0) (e1 1))]
          ['sp2call (ecall (e2 0) (e2 1))]  ['sp2bind (ebind (e2 0) (e2 1))]
          ['sp2subst (esubst (e2 0) (e2 1))]
          ['sp3call (ecall (e3 0) (e3 1))]  ['sp3bind (ebind (e3 0) (e3 1))]]

   _ (puts "; test/uusplgen.l -- GENERATED from doc/misc/proto/spl.l by tools/spl2uu.l. Do not")
   _ (puts "\n; edit; refresh with `make uusplgen`. The call-site samples under the three")
   _ (puts "\n; compilers -- ecall (args into seats), ebind (the binding splice), esubst")
   _ (puts "\n; (the substitution splice) -- lifted to uu nlist terms, 2 nats per op (op x),")
   _ (puts "\n; opcodes pushk0 pusha1 pushl2 pushs3 setl4 sets5 add6 emit7, operands raw.")
   _ (puts "\n; Each source op prints over its term. Laws: test/uuspllaw.l; substrate:")
   _ (puts "\n; test/uuhom.l's stack machine over test/uu.l (the kernel).")
   _ (puts "\n(assert")
   _ (each emit1 progs)
   _ (puts ")\n")
   0)
