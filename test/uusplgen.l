; test/uusplgen.l -- GENERATED from doc/proto/spl.l by tools/spl2uu.l. Do not
; edit; refresh with `make uusplgen`. The call-site samples under the three
; compilers -- ecall (args into seats), ebind (the binding splice), esubst
; (the substitution splice) -- lifted to uu nlist terms, 2 nats per op (op x),
; opcodes pushk0 pusha1 pushl2 pushs3 setl4 sets5 add6 emit7, operands raw.
; Each source op prints over its term. Laws: test/uuspllaw.l; substrate:
; test/uuhom.l's stack machine over test/uu.l (the kernel).
(assert
 ; sp0call  (pusha 0)
 ; sp0call  (sets 0)
 ; sp0call  (pushs 0)
 ; sp0call  (pushk 1)
 ; sp0call  (add 0)
 (defn 'sp0call 'nlist
   '(ncons 1 (ncons 0
    (ncons 5 (ncons 0
    (ncons 3 (ncons 0
    (ncons 0 (ncons 1
    (ncons 6 (ncons 0
    nnil)))))))))))
 ; sp0bind  (pusha 0)
 ; sp0bind  (setl 0)
 ; sp0bind  (pushl 0)
 ; sp0bind  (pushk 1)
 ; sp0bind  (add 0)
 (defn 'sp0bind 'nlist
   '(ncons 1 (ncons 0
    (ncons 4 (ncons 0
    (ncons 2 (ncons 0
    (ncons 0 (ncons 1
    (ncons 6 (ncons 0
    nnil)))))))))))
 ; sp1call  (pusha 0)
 ; sp1call  (emit 0)
 ; sp1call  (sets 0)
 ; sp1call  (pushs 0)
 ; sp1call  (pushk 1)
 ; sp1call  (add 0)
 (defn 'sp1call 'nlist
   '(ncons 1 (ncons 0
    (ncons 7 (ncons 0
    (ncons 5 (ncons 0
    (ncons 3 (ncons 0
    (ncons 0 (ncons 1
    (ncons 6 (ncons 0
    nnil)))))))))))))
 ; sp1bind  (pusha 0)
 ; sp1bind  (emit 0)
 ; sp1bind  (setl 0)
 ; sp1bind  (pushl 0)
 ; sp1bind  (pushk 1)
 ; sp1bind  (add 0)
 (defn 'sp1bind 'nlist
   '(ncons 1 (ncons 0
    (ncons 7 (ncons 0
    (ncons 4 (ncons 0
    (ncons 2 (ncons 0
    (ncons 0 (ncons 1
    (ncons 6 (ncons 0
    nnil)))))))))))))
 ; sp2call  (pusha 0)
 ; sp2call  (emit 0)
 ; sp2call  (sets 0)
 ; sp2call  (pushs 0)
 ; sp2call  (pushs 0)
 ; sp2call  (add 0)
 (defn 'sp2call 'nlist
   '(ncons 1 (ncons 0
    (ncons 7 (ncons 0
    (ncons 5 (ncons 0
    (ncons 3 (ncons 0
    (ncons 3 (ncons 0
    (ncons 6 (ncons 0
    nnil)))))))))))))
 ; sp2bind  (pusha 0)
 ; sp2bind  (emit 0)
 ; sp2bind  (setl 0)
 ; sp2bind  (pushl 0)
 ; sp2bind  (pushl 0)
 ; sp2bind  (add 0)
 (defn 'sp2bind 'nlist
   '(ncons 1 (ncons 0
    (ncons 7 (ncons 0
    (ncons 4 (ncons 0
    (ncons 2 (ncons 0
    (ncons 2 (ncons 0
    (ncons 6 (ncons 0
    nnil)))))))))))))
 ; sp2subst  (pusha 0)
 ; sp2subst  (emit 0)
 ; sp2subst  (pusha 0)
 ; sp2subst  (emit 0)
 ; sp2subst  (add 0)
 (defn 'sp2subst 'nlist
   '(ncons 1 (ncons 0
    (ncons 7 (ncons 0
    (ncons 1 (ncons 0
    (ncons 7 (ncons 0
    (ncons 6 (ncons 0
    nnil)))))))))))
 ; sp3call  (pusha 0)
 ; sp3call  (emit 0)
 ; sp3call  (sets 0)
 ; sp3call  (pushk 5)
 ; sp3call  (emit 0)
 (defn 'sp3call 'nlist
   '(ncons 1 (ncons 0
    (ncons 7 (ncons 0
    (ncons 5 (ncons 0
    (ncons 0 (ncons 5
    (ncons 7 (ncons 0
    nnil)))))))))))
 ; sp3bind  (pusha 0)
 ; sp3bind  (emit 0)
 ; sp3bind  (setl 0)
 ; sp3bind  (pushk 5)
 ; sp3bind  (emit 0)
 (defn 'sp3bind 'nlist
   '(ncons 1 (ncons 0
    (ncons 7 (ncons 0
    (ncons 4 (ncons 0
    (ncons 0 (ncons 5
    (ncons 7 (ncons 0
    nnil))))))))))))
