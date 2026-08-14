; love/uu.l -- the uu kernel's HOME: MLTT with the UniMath Preamble's seven
; inductives (empty unit bool coprod nat paths total2), bidirectional checking,
; NbE conversion (de bruijn levels as fresh vars, sigma- and pi-eta),
; type-in-type -- the universe stance of the reference
; (UniMath/Foundations/Init.v: Unset Universe Checking). an axiom is a book
; entry with a type and no value: a sanctioned missing. names follow UniMath so
; the port reads literally.
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

(: (vsucc v) (charm? v ? (v + 1) ['succ v])

   ; a uu closure carries a LOVE FUNCTION, so applying one is an application and not a
   ; re-walk of the body's syntax. ⚠ ('clo f) is opaque above this line: conv reaches a
   ; closure only through capp (its lam/pi/sig branches all come before the spine case), and
   ; lof prints TERMS, so these two lines plus comp's three ('clo ..) sites are the whole
   ; representation.
   (capp cl v) ((<>cl) v)

   (vapp f a) (f @ ('lam cl) (capp cl a) ['na f a])

   (dop1 v) (v @ ('chain a _) a ['np1 v])
   (dop2 v) (v @ ('chain _ b) b ['np2 v])

   (donr P z s m)
   (? (charm? m) (: (go k acc) (k = m ? acc (go (k + 1) (vapp (vapp s k) acc)))
                  (go 0 z))
      (m @ ('succ n) (vapp (vapp s n) (donr P z s n)) ['nnr P z s m]))

   (dobr P x y v) (v @ 'true x  'false y  ['nbr P x y v])
   (dour P x v) (v @ 'tt x ['nur P x v])
   (docr P f g v) (v @ ('i1 a) (vapp f a)
                        ('i2 b) (vapp g b)
                        ['ncr P f g v])
   (doer P v) ['ner P v]
   (dojr P r ev) (ev @ ('refl _) r ['njr P r ev])

   (vof-sym en x)
   ; found-tests are STRUCTURAL (two?): a `?` on the (name value) pair NETS
   ; the value -- a DAG-shared value walks as a TREE, and one lookup of a
   ; big bound value costs seconds (the gka tower: 4.5s -> 0ms).
   (: e (assoc x en)
      (? (two? e) <>e
         (: g (peep GLOB x 0)
            (? (two? g) <>g (scare 'uu-unbound x)))))

   ; --- comp: the term walked ONCE, into a love function of the env ---
   ; every case below reads `(\ en ..)` where the old vof read the value directly, and that is
   ; the whole change: the head dispatch, the destructuring and the arity all happen HERE, once
   ; per term, where they used to happen once per EVALUATION. a lam's body becomes a love
   ; closure over the compiled body, so capp is an application.
   ; ⚠ vof-sym STAYS DYNAMIC. it reads GLOB, and a defn later in the corpus must be visible to
   ; a term compiled earlier -- so the lookup rides the `(\ en ..)`, never the compile.
   ; ⚠ ORDERED BY MEASURED FREQUENCY, and the tail is never reached at all: over the UniMath
   ; corpus lam and succ are 550740 and 526521 of 1102646 head dispatches, while total2 pr1 pr2
   ; ii1 ii2 are ZERO -- the reader spells them, but every USE arrives through a spine or a
   ; rect. succ once sat at position 8 behind four of those zeros. The tests are mutually
   ; exclusive symbols so the order is free; only `(two? h)` must stay first, the spine last.
   ; ⚠ the order matters LESS than it did (once per term, not per evaluation) and is kept
   ; because compiling is itself on the hot path: chk and inf compile each subterm they visit.
   (comp t)
   (? (charm? t) (\ en t)
      (nom? t) (? (member? t '(UU nat bool unit empty tt true false)) (\ en t)
                  (id? t 'zero) (\ en 0)
                  (\ en (vof-sym en t)))
      ; A PATTERN IS THE TERM GRAMMAR, so this dispatch now reads like the header's own
      ; list of shapes. It costs the arity guards, and here they are nearly free: comp runs
      ; 14778 times over the corpus where the old vof ran 1102646, so this whole chain is
      ; 1.3% of one capp's traffic. ⚠ THE (two? h) FAST PATH IS GONE with it: a LIST head is
      ; never id? to a tag, so a spine falls through every arm to the else on its own -- and
      ; it is 259 of those 14778, which is not worth a branch or a second copy of the body.
      (t @
         ('lam x b)             (: cb (comp b)
                                         (\ en ['lam ['clo (\ v (cb ([x v] >< en)))]]))
         ('succ a)              (: ca (comp a) (\ en (vsucc (ca en))))
         ('pi x A B)            (: cA (comp A) cB (comp B)
                                         (\ en ['pi (cA en) ['clo (\ v (cB ([x v] >< en)))]]))
         ('paths A a b)         (: cA (comp A) ca (comp a) cb (comp b)
                                         (\ en ['paths (cA en) (ca en) (cb en)]))
         ('nat_rect P z s n)    (: cP (comp P) cz (comp z) cs (comp s) cn (comp n)
                                         (\ en (donr (cP en) (cz en) (cs en) (cn en))))
         ('tpair a b)           (: ca (comp a) cb (comp b) (\ en ['chain (ca en) (cb en)]))
         ('idpath a)            (: ca (comp a) (\ en ['refl (ca en)]))
         ('paths_rect P r e)    (: cP (comp P) cr (comp r) ce (comp e)
                                         (\ en (dojr (cP en) (cr en) (ce en))))
         ('bool_rect P x y w)   (: cP (comp P) cx (comp x) cy (comp y) cw (comp w)
                                         (\ en (dobr (cP en) (cx en) (cy en) (cw en))))
         ('coprod A B)          (: cA (comp A) cB (comp B) (\ en ['coprod (cA en) (cB en)]))
         ('coprod_rect P f g w) (: cP (comp P) cf (comp f) cg (comp g) cw (comp w)
                                         (\ en (docr (cP en) (cf en) (cg en) (cw en))))
         ('empty_rect P e)      (: cP (comp P) ce (comp e) (\ en (doer (cP en) (ce en))))
         ('unit_rect P u x)     (: cP (comp P) cu (comp u) cx (comp x)
                                         (\ en (dour (cP en) (cu en) (cx en))))
         ; (UU level) -> a sort value (UU0 stays the symbol UU; level may be an expr, Stage 2)
         ('UU l)                (? (l = 0) (\ en 'UU) (\ en ['U l]))
         ('the T u)             (comp u)                    ; the ascription is the term
         ('let x a b)           (: ca (comp a) cb (comp b)
                                         (\ en (cb ([x (ca en)] >< en))))
         ('total2 x A B)        (: cA (comp A) cB (comp B)
                                         (\ en ['sig (cA en) ['clo (\ v (cB ([x v] >< en)))]]))
         ('pr1 w)               (: ca (comp w) (\ en (dop1 (ca en))))
         ('pr2 w)               (: ca (comp w) (\ en (dop2 (ca en))))
         ('ii1 a)               (: ca (comp a) (\ en ['i1 (ca en)]))
         ('ii2 b)               (: ca (comp b) (\ en ['i2 (ca en)]))
         (: cf (comp <t) cas (comp-args >t) (\ en (spine en (cf en) cas)))))

   ; the argument list compiles once too; `spine` then only applies.
   ; ⚠ empty-list tests are STRUCTURAL (two?): a love list of zeros NETS zero, so `!cas` would
   ; call (add n 0)'s final argument list (0) empty -- the net law. Asked in the positive here.
   (comp-args as) (two? as ? (comp <as >< comp-args >as))
   (spine en f cas) (two? cas ? (spine en (vapp f (<cas en)) >cas) f)

   ; the old door, kept for every caller (chk, inf, the globals, test/uu.l): compile, then run.
   (vof en t) ((comp t) en)

   ; ⚠ THE SCRUTINEE IS A PAIR, which is why this one stays a `?`: an arm reads both
   ; sides, and a pattern would build the [a b] per call to say less than the tests do.
   ; the two atom arms are GUARDS -- equal charms and equal noms are id? above, so
   ; reaching one means unequal, and it keeps `<a` off a non-list below.
   (conv d a b)
   (? (id? a b) 1                    ; the same heap object converts to itself
      (charm? a && charm? b) ()
      (nom? a && nom? b) ()
      (id? <a 'lam || id? <b 'lam)
      (: fr ['nv d] (conv (d + 1) (vapp a fr) (vapp b fr)))
      (id? <a 'chain && two? b && !(id? <b 'chain))
      (conv d <>a (dop1 b) && conv d <>>a (dop2 b))
      (id? <b 'chain && two? a && !(id? <a 'chain))
      (conv d (dop1 a) <>b && conv d (dop2 a) <>>b)
      (id? <a 'pi && id? <b 'pi)
      (conv d <>a <>b && (: fr ['nv d] (conv (d + 1) (capp <>>a fr) (capp <>>b fr))))
      (id? <a 'sig && id? <b 'sig)
      (conv d <>a <>b && (: fr ['nv d] (conv (d + 1) (capp <>>a fr) (capp <>>b fr))))
      (two? a && two? b && id? <a <b) (conv-args d >a >b)
      0)

   (conv-args d as bs)
   (? (atom? as && atom? bs) 1
      (atom? as || atom? bs) 0
      (conv d <as <bs && conv-args d >as >bs))

   (inf-sym cx x)
   (x @ ('UU | 'nat | 'bool | 'unit | 'empty) 'UU
        'zero 'nat
        'tt 'unit
        ('true | 'false) 'bool
        (assoc x cx @ (_ T) T
           (: g (peep GLOB x 0) (two? g ? <g (scare 'uu-unbound x)))))

   (inf-spine cx en d f as)
   (? !(two? as) f
      (f @ ('pi A q)
             (: a <as
                _ (chk cx en d a A)
                (inf-spine cx en d (capp q (vof en a)) >as))
             (scare 'uu-not-a-pi as)))

   ; ⚠ CHECKING IS TWO SHAPES AT ONCE -- the term's and the type's -- so every arm here
   ; is an `@` inside an `@`, and the inner else is the mismatch that names both.
   (chk cx en d t T)
   (t @
      ('lam x b)   (T @ ('pi A q) (: fr ['nv d]
                                     (chk ([x A] >< cx) ([x fr] >< en) (d + 1) b (capp q fr)))
                        (scare 'uu-lam-needs-pi t))
      ('tpair a b) (T @ ('sig A q) (: _ (chk cx en d a A)
                                      (chk cx en d b (capp q (vof en a))))
                        (scare 'uu-tpair-needs-total2 t))
      ('ii1 a)     (T @ ('coprod A _) (chk cx en d a A) (scare 'uu-ii1-needs-coprod t))
      ('ii2 b)     (T @ ('coprod _ B) (chk cx en d b B) (scare 'uu-ii2-needs-coprod t))
      ('idpath a)  (T @ ('paths A av bv)
                          (: _ (chk cx en d a A)
                             v (vof en a)
                             (conv d v av && conv d v bv ? 1 (scare 'uu-idpath-mismatch t)))
                        (scare 'uu-idpath-needs-paths t))
      ('let x a b) (: Av (inf cx en d a)
                      va (vof en a)
                      (chk ([x Av] >< cx) ([x va] >< en) d b T))
      (: Ti (inf cx en d t)
         (conv d T Ti ? 1 (scare 'uu-type-mismatch t))))

   ; ⚠ THE ELSE IS THE SPINE and needs no head test in front of it: a LIST head is never
   ; id? to a tag, and (inf .. <t) answers for a nom head and a nested spine alike.
   ; ⚠ a pattern carries the ARITY the old head test did not, so a tag at the wrong
   ; length reaches the spine and comes back unbound instead of reading () for operands.
   (inf cx en d t)
   (? (charm? t) 'nat
      (nom? t) (inf-sym cx t)
      (t @
         (('pi | 'total2) x A B)
          (: _ (chk cx en d A 'UU)
             fr ['nv d]
             _ (chk ([x (vof en A)] >< cx) ([x fr] >< en) (d + 1) B 'UU)
             'UU)
         ('coprod A B)
          (: _ (chk cx en d A 'UU) _ (chk cx en d B 'UU) 'UU)
         ('paths A a b)
          (: _ (chk cx en d A 'UU)
             Av (vof en A)
             _ (chk cx en d a Av)
             _ (chk cx en d b Av)
             'UU)
         ('succ n) (: _ (chk cx en d n 'nat) 'nat)
         ('idpath a)
          (: Av (inf cx en d a) v (vof en a) ['paths Av v v])
         ('pr1 w)
          (: S (inf cx en d w)
             (S @ ('sig A _) A (scare 'uu-pr1-needs-total2 t)))
         ('pr2 w)
          (: S (inf cx en d w)
             (S @ ('sig _ q) (capp q (dop1 (vof en w))) (scare 'uu-pr2-needs-total2 t)))
         ('the T u)
          (: _ (chk cx en d T 'UU)
             Tv (vof en T)
             _ (chk cx en d u Tv)
             Tv)
         ('let x a b)
          (: Av (inf cx en d a)
             va (vof en a)
             (inf ([x Av] >< cx) ([x va] >< en) d b))
         ('nat_rect P z s n)
          (: _ (chk cx en d n 'nat)
             _ (chk cx en d P (vof 0 '(pi x nat UU)))
             Pv (vof en P)
             _ (chk cx en d z (vapp Pv 0))
             _ (chk cx en d s (vof [['P Pv]] '(pi k nat (pi r (P k) (P (succ k))))))
             (vapp Pv (vof en n)))
         ('bool_rect P x y w)
          (: _ (chk cx en d w 'bool)
             _ (chk cx en d P (vof 0 '(pi x bool UU)))
             Pv (vof en P)
             _ (chk cx en d x (vapp Pv 'true))
             _ (chk cx en d y (vapp Pv 'false))
             (vapp Pv (vof en w)))
         ('unit_rect P u w)
          (: _ (chk cx en d w 'unit)
             _ (chk cx en d P (vof 0 '(pi x unit UU)))
             Pv (vof en P)
             _ (chk cx en d u (vapp Pv 'tt))
             (vapp Pv (vof en w)))
         ('empty_rect P e)
          (: _ (chk cx en d e 'empty)
             _ (chk cx en d P (vof 0 '(pi x empty UU)))
             (vapp (vof en P) (vof en e)))
         ('coprod_rect P f g w)
          (: S (inf cx en d w)
             (S @ ('coprod A B)
                    (: _ (chk cx en d P (vof [['A A] ['B B]] '(pi x (coprod A B) UU)))
                       Pv (vof en P)
                       _ (chk cx en d f (vof [['A A] ['P Pv]] '(pi a A (P (ii1 a)))))
                       _ (chk cx en d g (vof [['B B] ['P Pv]] '(pi b B (P (ii2 b)))))
                       (vapp Pv (vof en w)))
                  (scare 'uu-coprod_rect t)))
         ('paths_rect P r e)
          (: E (inf cx en d e)
             (E @ ('paths A av bv)
                    (: _ (chk cx en d P
                              (vof [['A A] ['a av]] '(pi b A (pi e (paths A a b) UU))))
                       Pv (vof en P)
                       _ (chk cx en d r (vapp (vapp Pv av) ['refl av]))
                       (vapp (vapp Pv bv) (vof en e)))
                  (scare 'uu-paths_rect t)))
         (('lam | 'tpair | 'ii1 | 'ii2) >< _) (scare 'uu-cannot-infer t)
         (inf-spine cx en d (inf cx en d <t) >t))))

; --- drivers: a definition is checked then pinned; an axiom is a book entry
; --- with a type and no value (its value is its own neutral name); rejects
; --- runs a thunk expecting the kernel to refuse (the help cell escapes).
(: (defn nm T t)
   (: _ (chk 0 0 0 T 'UU)
      Tv (vof 0 T)
      _ (chk 0 0 0 t Tv)
      _ (pin GLOB nm [Tv (vof 0 t)])
      _ (pin SRC nm (t >< ()))
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
   (kp1 v) (v @ ('chain a _) a ['np1 v])
   (kp2 v) (v @ ('chain _ b) b ['np2 v])
   (ksucc v) (v + 1)
   (ki1 a) ['i1 a]
   (ki2 a) ['i2 a]
   (krefl a) ['refl a]
   (knrec s z n) (: (go k acc) (k = n ? acc (go (k + 1) (s k acc))) (go 0 z))
   (kbrec x y b) (b @ 'true x y)
   (kcrec f g v) (v @ ('i1 a) (f a) (g <>v))
   (kseq e r) r

   (bound? x bs) (two? bs ? (id? x <bs || bound? x >bs) 0)

   (uglo nm)
   (peep CMEM nm 0 @ (_ v) v
      (peep BRIDGE nm 0 @ (_ v) v
         ; ⚠ a SRC row is the ONE-element (t), not a ['hit v] pair: defn pins the term itself.
         (peep SRC nm 0 @ (src) (: v (ev (u2 src ())) _ (pin CMEM nm ['hit v]) v)
                         ['nv nm])))

   (u2app t bs)
   (: (walk as) (two? as ? (u2 <as bs >< walk >as))
      (u2 <t bs >< walk >t))

   ; ⚠ THE TAG DOES NOT PICK THE ORDER: nat_rect and friends take their operands
   ; the way knrec wants them, not the way the term spells them.
   (u2 t bs)
   (? (charm? t) t
      (nom? t)
      (? (id? t 'zero) 0
         (member? t '(UU nat bool unit empty tt true false)) (qlit t)
         (bound? t bs) t
         (qlit (uglo t)))
      (t @
         ('lam x b)   (Q >< (x >< (u2 b (x >< bs) >< ())))
         ('let x a b) (C >< (x >< (u2 a bs >< (u2 b (x >< bs) >< ()))))
         ('the T u)   (u2 u bs)
         (('pi | 'total2 | 'coprod | 'paths) >< _) (qlit ['ty <t])
         ('tpair a b) [(qlit kchain) (u2 a bs) (u2 b bs)]
         ('pr1 w)     [(qlit kp1) (u2 w bs)]
         ('pr2 w)     [(qlit kp2) (u2 w bs)]
         ('succ n)    [(qlit ksucc) (u2 n bs)]
         ('ii1 a)     [(qlit ki1) (u2 a bs)]
         ('ii2 b)     [(qlit ki2) (u2 b bs)]
         ('idpath a)  [(qlit krefl) (u2 a bs)]
         ('nat_rect _ z s n)     [(qlit knrec) (u2 s bs) (u2 z bs) (u2 n bs)]
         ('bool_rect _ x y w)    [(qlit kbrec) (u2 x bs) (u2 y bs) (u2 w bs)]
         ('unit_rect _ u w)      [(qlit kseq) (u2 w bs) (u2 u bs)]
         ('coprod_rect _ f g w)  [(qlit kcrec) (u2 f bs) (u2 g bs) (u2 w bs)]
         ('paths_rect _ r e)     [(qlit kseq) (u2 e bs) (u2 r bs)]
         ('empty_rect _ e)       [(qlit kseq) (u2 e bs) (qlit 'ner)]
         (u2app t bs)))

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
(: _ (:: 'defn (\ l (? (3 = (tally l)) ('(1 defn) >< l) ['scare ['\ 'uu-defn-arity] ['\ l]])))
   _ (:: 'defq (\ l (? (3 = (tally l)) ('(1 defq) >< l) ['scare ['\ 'uu-defq-arity] ['\ l]])))
   _ (:: 'axm  (\ l (? (2 = (tally l)) ('(1 axm) >< l) ['scare ['\ 'uu-axm-arity]  ['\ l]])))
   0)
