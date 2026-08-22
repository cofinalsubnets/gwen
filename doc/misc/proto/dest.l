; --- doc/misc/proto/dest.l: the DESTINATION vocabulary, modeled -- a gen.l design spike ---
; a runnable companion to (the catalog; the gap it chases is
; scored in): TWO code generators over one C-shaped AST, emitting
; one holo-flavored IR. `cv` is the r0 protocol as gen.l has it today --
; every expression lands its value in r0 and the consumer relocates it. `ck` passes the
; CONSUMER down instead, a destination die:
;
;   ('reg r)    deliver into r -- a home, a cs seat, an argument seat
;   ('mem b o)  deliver through a store -- the member-store fold becomes a non-event
;   ('br lt lf) a control destination: compares branch, && threads labels, no 0/1
;               reify; either label may be 'fall (the caller lays that arm next)
;   'tail       the return object itself -- a call here is a sibcall by construction
;   ()          the degenerate die: r0, so an unmigrated consumer keeps reading r0
;
; this is ev.l's emitter shape -- (\ k n ..), the continuation in hand at emission,
; the thread built toward Answer -- worn by a register machine: fix R = "a returned
; value" and work in Hom(-, R); passing the die down is the hom functor's contravariant
; leg, defunctionalized (a C expression's continuations form this small closed set).
; the literature calls it destination-driven code generation (Dybvig & Burger).
; the design said once, both machines:.
;
; run: cat test/00-init.l doc/misc/proto/dest.l | out/host/love    ; dots, then `dest: ok`
;
; the laws, gen.l's machinery named in brackets:
;   * p->f = 7 is ONE si form            [the addressing/store folds, by construction]
;   * y = y + 1 into y's home is ONE alu [the want hint promoted to the contract]
;   * if (x<y && y<3) has ZERO set forms [cbinl/tbr/fcb collapse into the ('br) lane]
;   * return f(x,3) ends in jmp          [sibcall/musttail by construction]
;   * (ck e ()) == (cv e) on leaves      [migration: () is today's protocol -- lane by lane]
; NOT modeled: residency across statements/calls (the seats/vmap/cs question is
; orthogonal -- a destination can BE a home, which is why the two compose), pool spill,
; and the arg-seat ordering law (a later arg READING an earlier seat must park -- the
; argseal's shape; these laws' args are non-interfering on purpose).

(: G (tablet 0)                                    ; the label mint, gen.l's nlab shape
   (relab _) (pin G 'n 0)
   (nlab _) (: k (peep G 'n 0) _ (pin G 'n (k + 1)) (intern ("l" + show k)))
   (site n env) (assoc n env)                      ; env entry: (name slot off) | (name home r)
   (heads fs) (map cap fs)

   ; --- cv: the r0 protocol (gen.l today). value lands in r0; compounds shuttle the
   ; left through an sp cell (spush2's shape); a relation reifies 0/1 via set. ---
   shov '((sub sp sp 8) (st sp 0 r0))
   shpo '((ld r1 sp 0) (add sp sp 8))
   (cv e env)
    (? (id? <e 'k) [['li 'r0 <>e]]
       (id? <e 'v) (: s (site <>e env)
                      (? (id? <>s 'home) [['mov 'r0 <>>s]] [['ld 'r0 'r4 <>>s]]))
       (id? <e 'bin) ((cv <>>e env) + shov + (cv <>>>e env) + shpo + [[<>e 'r0 'r1 'r0]])
       (id? <e 'rel) ((cv <>>e env) + shov + (cv <>>>e env) + shpo
                      + [['cmp 'r1 'r0] ['set <>e 'r0]])
       (id? <e 'and) (: lf (nlab 0) l2 (nlab 0)
                        ((cv <>e env) + [['cmp 'r0 0] ['br 'eq lf]] + (cv <>>e env)
                         + [['cmp 'r0 0] ['br 'eq lf] ['li 'r0 1] ['jmp l2]
                             ['label lf] ['li 'r0 0] ['label l2]]))
       (id? <e 'call) ((argcv <>>e '(r6 r5) env) + [['call <>e]])
       [['bad]])
   (argcv as ss env) (? !(two? as) ()
                        ((cv <as env) + [['mov <ss 'r0]] + (argcv >as >ss env)))
   (cvs s env)                                     ; statements: asn / if / ret
    (? (id? <s 'asn)
       (: lv <>s
          (? (id? <lv 'loc)
             (: e9 (site <>lv env)
                ((cv <>>s env)
                 + (? (id? <>e9 'home) [['mov <>>e9 'r0]] [['st 'r4 <>>e9 'r0]])))
             ((cv <>lv env) + shov + (cv <>>s env) + shpo + [['st 'r1 <>>lv 'r0]])))
       (id? <s 'if)
       (: cf (cv <>s env) le (nlab 0) ld (nlab 0)
          (cf + [['cmp 'r0 0] ['br 'eq le]] + (cvs <>>s env)
              + [['jmp ld] ['label le]] + (cvs <>>>s env) + [['label ld]]))
       (id? <s 'ret) ((cv <>s env) + [['ret]])
       [['bad]])

   ; --- ck: the destination generator. the die is ev's k defunctionalized; rd9 is
   ; karg/kim fusion worn by a register machine (an operand that already sits
   ; somewhere costs zero forms). a subexpression only ever writes its own die and
   ; scratches strictly below it in the pool -- the alsafe? discipline's shape. ---
   (negrel r) (? (id? r 'lt) 'ge (id? r 'ge) 'lt (id? r 'le) 'gt
                 (id? r 'gt) 'le (id? r 'eq) 'ne 'eq)
   (brto rel lt lf)
    (? (id? lf 'fall) [['br rel lt]]
       (id? lt 'fall) [['br (negrel rel) lf]]
       [['br rel lt] ['jmp lf]])
   (rd9 e env pool)                                ; operand read -> (x forms)
    (? (id? <e 'k) [<>e ()]
       (id? <e 'v) (: s9 (site <>e env)
                      (? (id? <>s9 'home) [<>>s9 ()]
                         (: s <pool [s [['ld s 'r4 <>>s9]]])))
       (: s <pool [s (ck e env >pool ['reg s])]))
   (ck e env pool w)
    (? (id? w 'tail)
       (? (id? <e 'call) ((argck <>>e '(r6 r5) env pool) + [['jmp <>e]])
          ((ck e env pool ['reg 'r0]) + [['ret]]))
       (: w9 (? (two? w) w ['reg 'r0])
          (? (id? <w9 'reg)
             (: r <>w9
                (? (id? <e 'k) [['li r <>e]]
                   (id? <e 'v) (: s9 (site <>e env)
                                  (? (id? <>s9 'home)
                                     (? (id? <>>s9 r) () [['mov r <>>s9]])
                                     [['ld r 'r4 <>>s9]]))
                   (id? <e 'bin) (: pb (rd9 <>>>e env pool)
                                    ((ck <>>e env pool ['reg r]) + <>pb + [[<>e r r <pb]]))
                   (id? <e 'rel) (: pa (rd9 <>>e env pool) pb (rd9 <>>>e env >pool)
                                    (<>pa + <>pb + [['cmp <pa <pb] ['set <>e r]]))
                   (id? <e 'call) ((argck <>>e '(r6 r5) env pool) + [['call <>e]]
                                   + (? (id? r 'r0) () [['mov r 'r0]]))
                   [['bad]]))
             (id? <w9 'mem)
             (: b <>w9 o <>>w9
                (? (id? <e 'k) [['si b o <>e]]
                   (id? <e 'v) (: s9 (site <>e env)
                                  (? (id? <>s9 'home) [['st b o <>>s9]]
                                     (: s <pool
                                        ((ck e env >pool ['reg s]) + [['st b o s]]))))
                   (: s <pool ((ck e env >pool ['reg s]) + [['st b o s]]))))
             (id? <w9 'br)
             (: lt <>w9 lf <>>w9
                (? (id? <e 'rel) (: pa (rd9 <>>e env pool) pb (rd9 <>>>e env >pool)
                                    (<>pa + <>pb + [['cmp <pa <pb]] + (brto <>e lt lf)))
                   (id? <e 'and) ((ck <>e env pool ['br 'fall lf]) + (ck <>>e env pool w9))
                   (: pa (rd9 e env pool)
                      (<>pa + [['cmp <pa 0]] + (brto 'ne lt lf)))))
             [['bad]])))
   (argck as ss env pool) (? !(two? as) ()
                             ((ck <as env pool ['reg <ss]) + (argck >as >ss env pool)))
   (base9 lv env pool)                             ; a store's base: a homed obj ALIASES
    (: obj <>lv                                    ; its home -- zero forms
       (? (id? <obj 'v) (: s8 (site <>obj env)
                           (? (id? <>s8 'home) [<>>s8 ()]
                              (: s <pool [s [['ld s 'r4 <>>s8]]])))
          (: s <pool [s (ck obj env >pool ['reg s])])))
   (cks s env pool)
    (? (id? <s 'asn)
       (: lv <>s
          (? (id? <lv 'loc)
             (: e9 (site <>lv env)
                (? (id? <>e9 'home) (ck <>>s env pool ['reg <>>e9])
                   (ck <>>s env pool ['mem 'r4 <>>e9])))
             (: b9 (base9 lv env pool)
                (<>b9 + (ck <>>s env >pool ['mem <b9 <>>lv])))))
       (id? <s 'if)
       (: le (nlab 0) ld (nlab 0)
          ((ck <>s env pool ['br 'fall le]) + (cks <>>s env pool)
           + [['jmp ld] ['label le]] + (cks <>>>s env pool) + [['label ld]]))
       (id? <s 'ret) (ck <>s env pool 'tail)
       [['bad]])
   P '(r10 r11))                                   ; the scratch pool (clear of seats/homes)

; --- law: the member store. p->f = 7 -- seven forms become one si. ---
(: s1 '(asn (fld (v p) 8) (k 7)) e1 '((p home r9)))
(assert (= (cvs s1 e1)
           '((mov r0 r9) (sub sp sp 8) (st sp 0 r0) (li r0 7)
             (ld r1 sp 0) (add sp sp 8) (st r1 8 r0)))
        (= (cks s1 e1 P) '((si r9 8 7))))

; --- law: the home write. y = y + 1 -- the shuttle dance becomes one alu form
; (the ++ residency story in one line: dest = the home, operand = an immediate). ---
(: s2 '(asn (loc y) (bin add (v y) (k 1))) e2 '((y home r8)))
(assert (= 8 (tally (cvs s2 e2)))
        (= (cks s2 e2 P) '((add r8 r8 1))))

; --- law: control destinations. if (x<y && y<3) z=1; else z=0; -- cv reifies each
; relation (set) then retests; ck branches straight off the compares: zero set forms,
; both stores land as si through the slot destination. ---
(: s3 '(if (and (rel lt (v x) (v y)) (rel lt (v y) (k 3)))
           (asn (loc z) (k 1))
           (asn (loc z) (k 0)))
   e3 '((x home r8) (y home r9) (z slot -8)))
(: _ (relab 0)
   a3 (cvs s3 e3)
   _ (relab 0)
   b3 (cks s3 e3 P)
   _ (assert (member? 'set (heads a3))
             !(member? 'set (heads b3))
             (= 34 (tally a3))
             (= b3 '((cmp r8 r9) (br ge l0) (cmp r9 3) (br ge l0)
                     (si r4 -8 1) (jmp l1) (label l0) (si r4 -8 0) (label l1)))))

; --- law: the tail. return f(x, 3) -- cv stages every arg through r0 and ends
; call;ret; ck delivers each arg into its seat and the call IS the jump. ---
(: s4 '(ret (call f ((v x) (k 3)))) e4 '((x home r8)))
(: a4 (cvs s4 e4)
   _ (assert (= 6 (tally a4))
             (id? 'ret (cap (last a4)))
             (id? 'call (cap (cap (cup (rev a4)))))
             (= (cks s4 e4 P) '((mov r6 r8) (li r5 3) (jmp f)))))

; --- law: the degenerate die. () wants the protocol register, so a leaf compiles
; byte-for-byte as cv does -- which is what makes the migration lane-by-lane: an
; unmigrated consumer keeps finding its value in r0. compounds only improve. ---
(: e5 '((z slot -8) (y home r9)))
(assert (= (cv '(k 7) e5) (ck '(k 7) e5 P ()))
        (= (cv '(v z) e5) (ck '(v z) e5 P ()))
        (: b (ck '(bin add (v y) (k 1)) e5 P ())
           (&& (< (tally b) (tally (cv '(bin add (v y) (k 1)) e5)))
               (= b '((mov r0 r9) (add r0 r0 1))))))

(puts "dest: ok") (putc 10)
