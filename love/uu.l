; love/uu.l -- the uu kernel's HOME: MLTT with the UniMath Preamble's seven
; inductives (empty unit bool coprod nat paths total2), bidirectional checking,
; NbE conversion (de bruijn levels as fresh vars, sigma- and pi-eta),
; type-in-type -- the universe stance of the reference
; (UniMath/Foundations/Init.v: Unset Universe Checking). an axiom is a book
; entry with a type and no value: a sanctioned missing. names follow UniMath so
; the port reads literally.
;
; the kernel is written in `@` (love/pat.l, spliced before this layer): every
; term and value form below is destructured by the GRAMMAR just under this line
; rather than by counting <>>> arrows, so the code and the doc cannot drift.
; ⚠ every arm reads ([tag ..] + _) -- the run, then anything -- because that is
; what the arrow-counting did: it never checked arity, so (lam x b junk) still
; truncates silently. the exact spelling ([tag ..], no + _) costs 4.4% here: the
; terminator is the DEEPEST access in a clause, an (id? () (cup (cup (cup ..))))
; nobody else pays, and on the value side it only restates an invariant this file
; already keeps by construction.
;
; terms are quoted lists: UU nat bool unit empty zero tt true false <fixnum>
;   (pi x A B) (lam x b) (total2 x A B) (tpair a b) (pr1 w) (pr2 w) (succ n)
;   (coprod A B) (ii1 a) (ii2 b) (paths A a b) (idpath a) (the T t)
;   (nat_rect P z s n) (bool_rect P t f b) (unit_rect P u x)
;   (coprod_rect P f g v) (empty_rect P e) (paths_rect P pr e)
;   (let x a b) -- definitional: x binds the VALUE of a, transparent to conv
; and any other list is an application spine. (f) == f here too.
;
; a MODULE on every corpus target (host + love0 + the inle kernel + wasm): the
; frontend lists this source in its ai_libs table and its boot says (use 'uu), then
; rebinds `uu` to the accessor -- (: uu (from 'uu)) -- so the surface stays
; (uu 'vof), (uu 'defn), (uu 'chk), (uu 'lof), and every internal a probe away.
; the layer IS the module boundary now: the old uu-mark/sweep trailer retired
; with the brackets. the UniMath theorem corpus rides in test/uu.l (and the
; test/uu*.l ladder over it), binding this surface at its head; the defn/defq/
; axm macros land in this layer's macro slot and ride the boot splice.

; --- the kernel: values, evaluation, conversion, checking ---
(: GLOB (tablet 0) HELPC (tablet 0)
   SRC (tablet 0) CMEM (tablet 0) BRIDGE (tablet 0))

(: (vsucc v) (? (charm? v) (v + 1) ['succ v])

   (capp cl v) (@ cl (['clo en x b] + _) (vof ([x v] >< en) b))

   (vapp f a) (@ f (['lam cl] + _) (capp cl a) ['na f a])

   (dop1 v) (@ v (['chain a _] + _) a ['np1 v])
   (dop2 v) (@ v (['chain _ b] + _) b ['np2 v])

   (donr P z s m)
   (? (charm? m) (: (go k acc) (? (k = m) acc (go (k + 1) (vapp (vapp s k) acc)))
                  (go 0 z))
      (@ m (['succ n] + _) (vapp (vapp s n) (donr P z s n))
         ['nnr P z s m]))

   (dobr P x y v) (@ v 'true x  'false y  ['nbr P x y v])
   (dour P x v) (@ v 'tt x ['nur P x v])
   (docr P f g v) (@ v (['i1 a] + _) (vapp f a)
                      (['i2 b] + _) (vapp g b)
                      ['ncr P f g v])
   (doer P v) ['ner P v]
   (dojr P r ev) (@ ev (['refl _] + _) r ['njr P r ev])

   (vof-sym en x)
   ; found-tests are STRUCTURAL (two?): a `?` on the (name value) pair NETS
   ; the value -- a DAG-shared value walks as a TREE, and one lookup of a
   ; big bound value costs seconds (the gka tower: 4.5s -> 0ms).
   (: e (assoc x en)
      (? (two? e) <>e
         (: g (peep GLOB x 0)
            (? (two? g) <>g (scare 'uu-unbound x)))))

   (vof en t)
   (? (charm? t) t
      (nom? t) (? (member? t '(UU nat bool unit empty tt true false)) t
                  (id? t 'zero) 0
                  (vof-sym en t))
      (two? <t) (vof-spine en (vof en <t) >t)         ; a compound head is an application spine
      ; the patterns ARE the grammar at the head of this file. each is a SPLIT --
      ; the run, then `_` -- so the arity stays as loose as the arrows it replaces.
      (@ t
         (['UU l] + _)                 (? (l = 0) 'UU ['U l])   ; (UU level) -> a sort value (UU0 stays the symbol UU; level may be an expr, Stage 2)
         (['lam x b] + _)              ['lam ['clo en x b]]
         (['pi x A B] + _)             ['pi  (vof en A) ['clo en x B]]
         (['total2 x A B] + _)         ['sig (vof en A) ['clo en x B]]
         (['tpair a b] + _)            ['chain (vof en a) (vof en b)]
         (['pr1 w] + _)                (dop1 (vof en w))
         (['pr2 w] + _)                (dop2 (vof en w))
         (['succ n] + _)               (vsucc (vof en n))
         (['coprod A B] + _)           ['coprod (vof en A) (vof en B)]
         (['ii1 a] + _)                ['i1 (vof en a)]
         (['ii2 b] + _)                ['i2 (vof en b)]
         (['paths A a b] + _)          ['paths (vof en A) (vof en a) (vof en b)]
         (['idpath a] + _)             ['refl (vof en a)]
         (['the T tm] + _)             (vof en tm)
         (['let x a b] + _)            (vof ([x (vof en a)] >< en) b)
         (['nat_rect P z s n] + _)     (donr (vof en P) (vof en z) (vof en s) (vof en n))
         (['bool_rect P x y b] + _)    (dobr (vof en P) (vof en x) (vof en y) (vof en b))
         (['unit_rect P u x] + _)      (dour (vof en P) (vof en u) (vof en x))
         (['coprod_rect P f g v] + _)  (docr (vof en P) (vof en f) (vof en g) (vof en v))
         (['empty_rect P e] + _)       (doer (vof en P) (vof en e))
         (['paths_rect P r e] + _)     (dojr (vof en P) (vof en r) (vof en e))
         (vof-spine en (vof en <t) >t)))

   ; empty-list tests are STRUCTURAL (two?): a love list of zeros NETS zero,
   ; so !as would call (add n 0)'s final argument list (0) empty -- the net law.
   (vof-spine en f as)
   (? !(two? as) f (vof-spine en (vapp f (vof en <as)) >as))

   (conv d a b)
   (? (id? a b) 1                    ; the same heap object converts to itself:
      (charm? a && charm? b) (a = b)     ; O(1) across the shared neutral towers
      (nom? a && nom? b) (id? a b)
      (id? <a 'lam || id? <b 'lam)
      (: fr ['nv d] (conv (d + 1) (vapp a fr) (vapp b fr)))
      (id? <a 'chain && two? b && !(id? <b 'chain))
      (conv d <>a (dop1 b) && conv d <>>a (dop2 b))
      (id? <b 'chain && two? a && !(id? <a 'chain))
      (conv d (dop1 a) <>b && conv d (dop2 a) <>>b)
      (id? <a 'pi && id? <b 'pi)
      (conv d <>a <>b
          && (: fr ['nv d] (conv (d + 1) (capp <>>a fr) (capp <>>b fr))))
      (id? <a 'sig && id? <b 'sig)
      (conv d <>a <>b
          && (: fr ['nv d] (conv (d + 1) (capp <>>a fr) (capp <>>b fr))))
      (two? a && two? b && id? <a <b) (conv-args d >a >b)
      0)

   (conv-args d as bs)
   (? (!(two? as) && !(two? bs)) 1
      (!(two? as) || !(two? bs)) 0
      (conv d <as <bs && conv-args d >as >bs))

   (inf-sym cx x)
   (? (id? x 'UU) 'UU
      (member? x '(nat bool unit empty)) 'UU
      (id? x 'zero) 'nat
      (id? x 'tt) 'unit
      (member? x '(true false)) 'bool
      (: e (assoc x cx)
         (? (two? e) <>e
            (: g (peep GLOB x 0)
               (? (two? g) <g (scare 'uu-unbound x))))))

   (inf-spine cx en d f as)
   (? !(two? as) f
      (@ f (['pi A cl] + _) (: a <as
                         _ (chk cx en d a A)
                         (inf-spine cx en d (capp cl (vof en a)) >as))
         (scare 'uu-not-a-pi as)))

   ; the introduction forms, each CHECKED against the type it demands: the term's
   ; shape picks the clause, the type's shape is asked for inside it, and any
   ; other pairing is the clause's own scare. everything else infers and converts.
   (chk cx en d t T)
   (@ t
      (['lam x b] + _)
        (@ T (['pi A cl] + _) (: fr ['nv d]
                           (chk ([x A] >< cx) ([x fr] >< en) (d + 1) b (capp cl fr)))
           (scare 'uu-lam-needs-pi t))
      (['tpair a b] + _)
        (@ T (['sig A cl] + _) (: _ (chk cx en d a A)
                            (chk cx en d b (capp cl (vof en a))))
           (scare 'uu-tpair-needs-total2 t))
      (['ii1 a] + _) (@ T (['coprod A B] + _) (chk cx en d a A) (scare 'uu-ii1-needs-coprod t))
      (['ii2 b] + _) (@ T (['coprod A B] + _) (chk cx en d b B) (scare 'uu-ii2-needs-coprod t))
      (['idpath a] + _)
        (@ T (['paths A x y] + _)
             (: _ (chk cx en d a A)
                v (vof en a)
                (? (conv d v x && conv d v y) 1 (scare 'uu-idpath-mismatch t)))
           (scare 'uu-idpath-needs-paths t))
      (['let x a b] + _)
        (: Av (inf cx en d a)
           va (vof en a)
           (chk ([x Av] >< cx) ([x va] >< en) d b T))
      (: Ti (inf cx en d t)
         (? (conv d T Ti) 1 (scare 'uu-type-mismatch t))))

   (inf cx en d t)
   (? (charm? t) 'nat
      (nom? t) (inf-sym cx t)
      (two? <t) (inf-spine cx en d (inf cx en d <t) >t)     ; a compound head is an application spine
      (@ t
         (['pi x A B] + _)
           (: _ (chk cx en d A 'UU)
              fr ['nv d]
              _ (chk ([x (vof en A)] >< cx) ([x fr] >< en) (d + 1) B 'UU)
              'UU)
         (['total2 x A B] + _)
           (: _ (chk cx en d A 'UU)
              fr ['nv d]
              _ (chk ([x (vof en A)] >< cx) ([x fr] >< en) (d + 1) B 'UU)
              'UU)
         (['coprod A B] + _) (: _ (chk cx en d A 'UU) _ (chk cx en d B 'UU) 'UU)
         (['paths A a b] + _)
           (: _ (chk cx en d A 'UU)
              Av (vof en A)
              _ (chk cx en d a Av)
              _ (chk cx en d b Av)
              'UU)
         (['succ n] + _) (: _ (chk cx en d n 'nat) 'nat)
         (['idpath a] + _) (: Av (inf cx en d a) v (vof en a) ['paths Av v v])
         (['pr1 w] + _) (@ (inf cx en d w) (['sig A _] + _) A (scare 'uu-pr1-needs-total2 t))
         (['pr2 w] + _) (@ (inf cx en d w) (['sig _ cl] + _) (capp cl (dop1 (vof en w)))
                     (scare 'uu-pr2-needs-total2 t))
         (['the T tm] + _)
           (: _ (chk cx en d T 'UU)
              Tv (vof en T)
              _ (chk cx en d tm Tv)
              Tv)
         (['let x a b] + _)
           (: Av (inf cx en d a)
              va (vof en a)
              (inf ([x Av] >< cx) ([x va] >< en) d b))
         (['nat_rect P z s n] + _)
           (: _ (chk cx en d n 'nat)
              _ (chk cx en d P (vof 0 '(pi x nat UU)))
              Pv (vof en P)
              _ (chk cx en d z (vapp Pv 0))
              _ (chk cx en d s (vof [['P Pv]] '(pi k nat (pi r (P k) (P (succ k))))))
              (vapp Pv (vof en n)))
         (['bool_rect P x y b] + _)
           (: _ (chk cx en d b 'bool)
              _ (chk cx en d P (vof 0 '(pi x bool UU)))
              Pv (vof en P)
              _ (chk cx en d x (vapp Pv 'true))
              _ (chk cx en d y (vapp Pv 'false))
              (vapp Pv (vof en b)))
         (['unit_rect P u x] + _)
           (: _ (chk cx en d x 'unit)
              _ (chk cx en d P (vof 0 '(pi x unit UU)))
              Pv (vof en P)
              _ (chk cx en d u (vapp Pv 'tt))
              (vapp Pv (vof en x)))
         (['empty_rect P e] + _)
           (: _ (chk cx en d e 'empty)
              _ (chk cx en d P (vof 0 '(pi x empty UU)))
              (vapp (vof en P) (vof en e)))
         (['coprod_rect P f g v] + _)
           (@ (inf cx en d v)
              (['coprod A B] + _)
                (: _ (chk cx en d P (vof [['A A] ['B B]] '(pi x (coprod A B) UU)))
                   Pv (vof en P)
                   _ (chk cx en d f (vof [['A A] ['P Pv]] '(pi a A (P (ii1 a)))))
                   _ (chk cx en d g (vof [['B B] ['P Pv]] '(pi b B (P (ii2 b)))))
                   (vapp Pv (vof en v)))
              (scare 'uu-coprod_rect t))
         (['paths_rect P r e] + _)
           (@ (inf cx en d e)
              (['paths A av bv] + _)
                (: _ (chk cx en d P
                          (vof [['A A] ['a av]] '(pi b A (pi e (paths A a b) UU))))
                   Pv (vof en P)
                   _ (chk cx en d r (vapp (vapp Pv av) ['refl av]))
                   (vapp (vapp Pv bv) (vof en e)))
              (scare 'uu-paths_rect t))
         (? (member? <t '(lam tpair ii1 ii2)) (scare 'uu-cannot-infer t)
            (inf-spine cx en d (inf-sym cx <t) >t)))))

; --- drivers: a definition is checked then pinned; an axiom is a book entry
; --- with a type and no value (its value is its own neutral name); rejects
; --- runs a thunk expecting the kernel to refuse (the help cell escapes).
(: (defn nm T t)
   (: _ (chk 0 0 0 T 'UU)
      Tv (vof 0 T)
      _ (chk 0 0 0 t Tv)
      _ (pin GLOB nm [Tv (vof 0 t)])
      _ (pin SRC nm [t])
      nm)
   (axm nm T)
   (: _ (chk 0 0 0 T 'UU)
      _ (pin GLOB nm [(vof 0 T) ['nv nm]])
      nm)
   ; the Qed driver: CHECKED like defn, then sealed -- pinned with its own
   ; neutral as value, so applying the proof never evaluates it. Coq's
   ; Qed/Defined distinction, here because strict NbE would otherwise build
   ; the proof's whole beta-normal form at every use.
   (defq nm T t)
   (: _ (chk 0 0 0 T 'UU)
      Tv (vof 0 T)
      _ (chk 0 0 0 t Tv)
      _ (pin GLOB nm [Tv ['nv nm]])
      nm)
   (rejects th)
   (: r (call-cc (\ k (: _ (pin HELPC () (\ a b (k 'rejected)))
                         v (th 0)
                         'accepted)))
      _ (pin HELPC () 0)
      r))

; --- uu2l: the uu-to-l leg. uu2coq and uu2lean re-CERTIFY the corpus in two
; --- foreign kernels; uu2l runs it HOME -- a CLOSED term compiles to a love
; --- form whose value IS vof's answer (charms for nats, ('chain a b) for
; --- pairs), at compiled-love speed; lof (l-of, vof's fast twin) evaluates one.
; --- UNTRUSTED by design: chk/conv never touch it, so a uu2l bug can miss a
; --- demo, never fake a theorem -- and the corpus asserts (vof t = lof t)
; --- differentially wherever both lanes are tractable. generated code has NO
; --- free names: the runtime kit and every global embed as live literals
; --- under the quote sigil, so nothing a uu binder spells can capture.
; --- BRIDGE maps standard names to native love (add is +, the eager-NbE O(n)
; --- niszero is (= m 0)) -- each bridge rides the SAME differential gate.
; --- defn feeds SRC; a defq/axm name compiles to its own neutral, exactly
; --- the sealed book value.
(: Q <'(\ x)
   C <'(: x 1 2)
   (qlit v) [Q v]

   (kchain a b) ['chain a b]
   (kp1 v) (? (two? v) (? (id? <v 'chain) <>v ['np1 v]) ['np1 v])
   (kp2 v) (? (two? v) (? (id? <v 'chain) <>>v ['np2 v]) ['np2 v])
   (ksucc v) (v + 1)
   (ki1 a) ['i1 a]
   (ki2 a) ['i2 a]
   (krefl a) ['refl a]
   (knrec s z n) (: (go k acc) (? (k = n) acc (go (k + 1) (s k acc))) (go 0 z))
   (kbrec x y b) (? (id? b 'true) x y)
   (kcrec f g v) (? (id? <v 'i1) (f <>v) (g <>v))
   (kseq e r) r

   (bound? x bs) (? !(two? bs) 0 (? (id? x <bs) 1 (bound? x >bs)))

   (uglo nm)
   (: c (peep CMEM nm 0)
      (? (two? c) <>c
         (: b (peep BRIDGE nm 0)
            (? (two? b) <>b
               (: sc (peep SRC nm 0)
                  (? (two? sc) (: v (ev (u2 <sc ()))
                           _ (pin CMEM nm ['hit v])
                           v)
                     ['nv nm]))))))

   (u2app t bs)
   (: (walk as) (? !(two? as) () ((u2 <as bs) >< walk >as))
      ((u2 <t bs) >< walk >t))

   (u2 t bs)
   (? (charm? t) t
      (nom? t)
      (? (id? t 'zero) 0
         (member? t '(UU nat bool unit empty tt true false)) (qlit t)
         (bound? t bs) t
         (qlit (uglo t)))
      (: h <t
         (? (two? h) (u2app t bs)
            (id? h 'lam) [Q <>t (u2 <>>t (<>t >< bs))]
            (id? h 'let) [C <>t (u2 <>>t bs) (u2 <>>>t (<>t >< bs))]
            (id? h 'the) (u2 <>>t bs)
            (member? h '(pi total2 coprod paths)) (qlit ['ty h])
            (id? h 'tpair) [(qlit kchain) (u2 <>t bs) (u2 <>>t bs)]
            (id? h 'pr1)   [(qlit kp1) (u2 <>t bs)]
            (id? h 'pr2)   [(qlit kp2) (u2 <>t bs)]
            (id? h 'succ)  [(qlit ksucc) (u2 <>t bs)]
            (id? h 'ii1)   [(qlit ki1) (u2 <>t bs)]
            (id? h 'ii2)   [(qlit ki2) (u2 <>t bs)]
            (id? h 'idpath) [(qlit krefl) (u2 <>t bs)]
            (id? h 'nat_rect)    [(qlit knrec) (u2 <>>>t bs) (u2 <>>t bs) (u2 <>>>>t bs)]
            (id? h 'bool_rect)   [(qlit kbrec) (u2 <>>t bs) (u2 <>>>t bs) (u2 <>>>>t bs)]
            (id? h 'unit_rect)   [(qlit kseq) (u2 <>>>t bs) (u2 <>>t bs)]
            (id? h 'coprod_rect) [(qlit kcrec) (u2 <>>t bs) (u2 <>>>t bs) (u2 <>>>>t bs)]
            (id? h 'paths_rect)  [(qlit kseq) (u2 <>>>t bs) (u2 <>>t bs)]
            (id? h 'empty_rect)  [(qlit kseq) (u2 <>>t bs) (qlit 'ner)]
            (u2app t bs))))

   (uu2l t) (u2 t ())
   (lof t) (ev (u2 t ()))
   (bridge nm f) (: _ (pin BRIDGE nm ['hit f]) nm))

; --- the arity door. defn/defq/axm are fully curried, so a paren slip that
; --- hands one too few operands is not an error -- it is a truthy CLOSURE one
; --- curry away from running, an assert paints it green, and NOTHING pins
; --- (the (f)==f trap at proof grain; too many operands feed the returned nom
; --- and vanish the same way). the macros guard CALL position: exactly the
; --- right operand count, or the form scares carrying itself. the expansion
; --- heads (1 defn) -- the identity's head is no macro, so no re-expansion --
; --- and the functions keep their names, so the baked book still serves them
; --- ((uu 'defn) is boot/overlay.l's door, and book calls bypass the macro).
(: _ (:: 'defn (\ l (? (tally l = 3) ('(1 defn) >< l) ['scare ['\ 'uu-defn-arity] ['\ l]])))
   _ (:: 'defq (\ l (? (tally l = 3) ('(1 defq) >< l) ['scare ['\ 'uu-defq-arity] ['\ l]])))
   _ (:: 'axm  (\ l (? (tally l = 2) ('(1 axm)  >< l) ['scare ['\ 'uu-axm-arity]  ['\ l]])))
   0)
