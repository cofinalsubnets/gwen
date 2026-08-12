; test/uuhom.l -- doc/hom.md's monoid, checked: the composition band of love's `*`
; (prel's compose, End(R)'s multiplication) as a uu term, its monoid laws BY IDPATH.
; at the binary `=` is alpha+structural, so ((1 * f) = f) and assoc read FALSE and the
; laws hold only applied; here conv is NbE with pi-eta, so the SAME laws check
; definitionally -- the quotient hom.md gestures at, made a checker's equality. the
; differential foot ties homcomp to the binary's `*` at a numeral (vof/lof, the
; corpus's own gate). exports to Rocq + Lean with the corpus (uu2coq/uu2lean).
; guard: the uu kernel (test/uu.l) must load first -- $t front-loads it; fail by name if not.
(assert (member? 'vof (names ())))

(assert
 ; love's `*` on the top band, spelled in uu: (homcomp f g) x = f (g x) -- prel.l's
 ; (compose f g x) (f (g x)), argument order and all (funcomp is the diagram-order twin)
 (defn 'homcomp
   '(pi X UU (pi Y UU (pi Z UU (pi f (pi y Y Z) (pi g (pi x X Y) (pi x X Z))))))
   '(lam X (lam Y (lam Z (lam f (lam g (lam x (f (g x)))))))))
 ; the monoid laws, each by idpath: the units ride pi-eta on the neutral f, assoc rides NbE
 (defq 'hom-lunit
   '(pi X UU (pi Y UU (pi f (pi x X Y) (paths (pi x X Y) (homcomp X Y Y (idfun Y) f) f))))
   '(lam X (lam Y (lam f (idpath f)))))
 (defq 'hom-runit
   '(pi X UU (pi Y UU (pi f (pi x X Y) (paths (pi x X Y) (homcomp X X Y f (idfun X)) f))))
   '(lam X (lam Y (lam f (idpath f)))))
 (defq 'hom-assoc
   '(pi X UU (pi Y UU (pi Z UU (pi W UU (pi f (pi z Z W) (pi g (pi y Y Z) (pi h (pi x X Y)
      (paths (pi x X W)
        (homcomp X Y W (homcomp Y Z W f g) h)
        (homcomp X Z W f (homcomp X Y Z g h))))))))))
   '(lam X (lam Y (lam Z (lam W (lam f (lam g (lam h
      (idpath (homcomp X Z W f (homcomp X Y Z g h))))))))))))

; the differential foot: one composite, three lanes -- vof, lof, the binary's `*` --
; meeting at a numeral
(: hc '(homcomp nat nat nat (lam y (succ y)) (lam x (succ (succ x))) 3)
 (assert
  (6 = (vof 0 hc))
  ((vof 0 hc) = (lof hc))
  ((vof 0 hc) = (((\ y (+ y 1)) * (\ x (+ x 2))) 3))))

; --- the skeleton, worn: an object is a location (a nat), the state one map,
; --- a morphism a state transformer. hmov is the relocation the design declines
; --- to pay; the laws say why it MAY decline: every object is iso to R on the
; --- carried value (carry + retract), a mov is invisible off its endpoints
; --- (frame), and composition in the skeleton IS homcomp -- one monoid, worn twice.
(assert
 (defn 'hst 'UU '(pi l nat nat))
 (defn 'hupd '(pi l nat (pi v nat (pi s hst hst)))
   '(lam l (lam v (lam s (lam q (bool_rect (lam x nat) v (s q) (nateqb q l)))))))
 (defn 'hmov '(pi a nat (pi b nat (pi s hst hst)))
   '(lam a (lam b (lam s (hupd b (s a) s)))))
 (defq 'hmov-carry '(pi s hst (paths nat (hmov 1 0 s 0) (s 1)))
   '(lam s (idpath (s 1))))
 (defq 'hmov-frame '(pi s hst (paths nat (hmov 1 0 s 2) (s 2)))
   '(lam s (idpath (s 2))))
 (defq 'hmov-retract '(pi s hst (paths nat (hmov 0 1 (hmov 1 0 s) 1) (s 1)))
   '(lam s (idpath (s 1))))
 (defq 'hmov-homcomp
   '(pi s hst (paths nat (homcomp hst hst hst (hmov 1 2) (hmov 0 1) s 2) (s 0)))
   '(lam s (idpath (s 0)))))

; --- the machine: dest.l's IR denoted, so its laws can be THEOREMS. state =
; --- (regs, frame, stack cells, sp) -- sp CONCRETE (starts 0, moves by
; --- push/pop only), which is what lets a balanced shuttle compute under a
; --- neutral state. an operand is parity-tagged: reg r = 2r, imm k = 2k+1.
(assert
 (defn 'isodd '(pi n nat bool)
   '(lam n (nat_rect (lam q bool) false (lam k (lam r (negb r))) n)))
 (defn 'half '(pi n nat nat)
   '(lam n (nat_rect (lam q nat) 0
             (lam k (lam r (bool_rect (lam x nat) (succ r) r (isodd k)))) n)))
 (defn 'mst 'UU '(total2 r hst (total2 f hst (total2 k hst nat))))
 (defn 'mmk '(pi r hst (pi f hst (pi k hst (pi p nat mst))))
   '(lam r (lam f (lam k (lam p (tpair r (tpair f (tpair k p))))))))
 (defn 'mrg '(pi s mst hst) '(lam s (pr1 s)))
 (defn 'mf '(pi s mst hst) '(lam s (pr1 (pr2 s))))
 (defn 'mk '(pi s mst hst) '(lam s (pr1 (pr2 (pr2 s)))))
 (defn 'msp '(pi s mst nat) '(lam s (pr2 (pr2 (pr2 s)))))
 (defn 'rdop '(pi x nat (pi s mst nat))
   '(lam x (lam s (bool_rect (lam q nat) (half x) (mrg s (half x)) (isodd x)))))
 (defn 'msetr '(pi d nat (pi v nat (pi s mst mst)))
   '(lam d (lam v (lam s (mmk (hupd d v (mrg s)) (mf s) (mk s) (msp s))))))
 (defn 'msetf '(pi d nat (pi v nat (pi s mst mst)))
   '(lam d (lam v (lam s (mmk (mrg s) (hupd d v (mf s)) (mk s) (msp s))))))
 (defn 'mpush '(pi v nat (pi s mst mst))
   '(lam v (lam s (mmk (mrg s) (mf s) (hupd (msp s) v (mk s)) (succ (msp s))))))
 (defn 'mpop '(pi d nat (pi s mst mst))
   '(lam d (lam s (mmk (hupd d (mk s (npred (msp s))) (mrg s)) (mf s) (mk s) (npred (msp s)))))))

; --- one step, then the walker. opcodes: li 0, mov 1, ld 2, st 3, si 4, add 5,
; --- push 14, pop 15; anything else (the control ops) denotes the identity --
; --- a program holding one is only ever read SYNTACTICALLY (opsof).
(assert
 (defn 'istep '(pi o nat (pi d nat (pi a nat (pi b nat (pi s mst mst)))))
   '(lam o (lam d (lam a (lam b (lam s
      (bool_rect (lam q mst) (msetr (half d) (rdop a s) s)
        (bool_rect (lam q mst) (msetr (half d) (mf s (half b)) s)
          (bool_rect (lam q mst) (msetf (half a) (rdop b s) s)
            (bool_rect (lam q mst) (msetr (half d) (add (rdop a s) (rdop b s)) s)
              (bool_rect (lam q mst) (mpush (rdop d s) s)
                (bool_rect (lam q mst) (mpop (half d) s) s
                  (nateqb o 15))
                (nateqb o 14))
              (nateqb o 5))
            (orb (nateqb o 3) (nateqb o 4)))
          (nateqb o 2))
        (orb (nateqb o 0) (nateqb o 1)))))))))
 ; ⚠ ONE recursive call per level (the nateqb lesson, test/uu.l): under the
 ; eager NbE a bool_rect arm holding IH multiplies -- the acc slots branch on
 ; VALUES instead, and the one exec rides a let
 (defn 'dgo
   '(pi q nat (pi v (nvec q) (pi m nat (pi o nat (pi p nat (pi a nat (pi s mst mst)))))))
   '(lam q (nat_rect (lam n (pi v (nvec n) (pi m nat (pi o nat (pi p nat (pi a nat (pi s mst mst)))))))
             (lam v (lam m (lam o (lam p (lam a (lam s s))))))
             (lam k (lam IH (lam v (lam m (lam o (lam p (lam a (lam s
               (let h (pr1 v)
                 (IH (pr2 v)
                   (bool_rect (lam x nat) 0 (succ m) (nateqb m 3))
                   (bool_rect (lam x nat) h o (nateqb m 0))
                   (bool_rect (lam x nat) h p (nateqb m 1))
                   (bool_rect (lam x nat) h a (nateqb m 2))
                   (bool_rect (lam x mst) (istep o p a h s) s (nateqb m 3))))))))))))
             q)))
 (defn 'drun '(pi l nlist (pi s mst mst))
   '(lam l (lam s (dgo (pr1 l) (pr2 l) 0 0 0 0 s))))
 (defn 'opsgo '(pi q nat (pi v (nvec q) (pi m nat nlist)))
   '(lam q (nat_rect (lam n (pi v (nvec n) (pi m nat nlist)))
             (lam v (lam m nnil))
             (lam k (lam IH (lam v (lam m
               (let r (IH (pr2 v) (bool_rect (lam y nat) 0 (succ m) (nateqb m 3)))
                 (bool_rect (lam x nlist) (ncons (pr1 v) r) r (nateqb m 0)))))))
             q)))
 (defn 'opsof '(pi l nlist nlist) '(lam l (opsgo (pr1 l) (pr2 l) 0))))

; --- the machine pinned before the corpus rides it: li lands its immediate,
; --- and the push/pop shuttle carries r0 to r1 forall states
(assert
 (defq 'drun-li
   '(pi R hst (pi F hst (pi K hst
      (paths nat (mrg (drun (ncons 0 (ncons 0 (ncons 15 (ncons 0 nnil)))) (mmk R F K 0)) 0) 7))))
   '(lam R (lam F (lam K (idpath 7)))))
 (defq 'drun-shuttle
   '(pi R hst (pi F hst (pi K hst
      (paths nat
        (mrg (drun (ncons 14 (ncons 0 (ncons 0 (ncons 0
                  (ncons 15 (ncons 2 (ncons 0 (ncons 0 nnil))))))))
                  (mmk R F K 0)) 1)
        (R 0)))))
   '(lam R (lam F (lam K (idpath (R 0)))))))

; --- the stack machine (ev's shape; the model: doc/proto/spl.l): state =
; --- (stack cells, sp, locals, seats, trace cells, trace count) -- sp and the
; --- trace count CONCRETE, the maps neutral, same discipline as mst. ops are
; --- 2 nats (op x): pushk 0, pusha 1, pushl 2, pushs 3, setl 4, sets 5,
; --- add 6, emit 7 -- emit appends TOS to the trace and keeps the stack, so
; --- an effect is visible ONLY in the trace: the observation R cannot carry.
(assert
 (defn 'sst 'UU '(total2 s hst (total2 p nat (total2 l hst (total2 e hst (total2 t hst nat))))))
 (defn 'smk '(pi s hst (pi p nat (pi l hst (pi e hst (pi t hst (pi n nat sst))))))
   '(lam s (lam p (lam l (lam e (lam t (lam n (tpair s (tpair p (tpair l (tpair e (tpair t n))))))))))))
 (defn 'stos '(pi st sst nat) '(lam st ((pr1 st) (npred (pr1 (pr2 st))))))
 (defn 'strn '(pi st sst nat) '(lam st (pr2 (pr2 (pr2 (pr2 (pr2 st)))))))
 (defn 'strat '(pi k nat (pi st sst nat))
   '(lam k (lam st ((pr1 (pr2 (pr2 (pr2 (pr2 st))))) k))))
 (defn 'sstep '(pi o nat (pi x nat (pi A hst (pi st sst sst))))
   '(lam o (lam x (lam A (lam st
      (let SM (pr1 st) (let PM (pr1 (pr2 st)) (let LM (pr1 (pr2 (pr2 st)))
      (let EM (pr1 (pr2 (pr2 (pr2 st)))) (let TM (pr1 (pr2 (pr2 (pr2 (pr2 st)))))
      (let NM (pr2 (pr2 (pr2 (pr2 (pr2 st)))))
      (bool_rect (lam q sst) (smk (hupd PM x SM) (succ PM) LM EM TM NM)
        (bool_rect (lam q sst) (smk (hupd PM (A x) SM) (succ PM) LM EM TM NM)
          (bool_rect (lam q sst) (smk (hupd PM (LM x) SM) (succ PM) LM EM TM NM)
            (bool_rect (lam q sst) (smk (hupd PM (EM x) SM) (succ PM) LM EM TM NM)
              (bool_rect (lam q sst) (smk SM (npred PM) (hupd x (SM (npred PM)) LM) EM TM NM)
                (bool_rect (lam q sst) (smk SM (npred PM) LM (hupd x (SM (npred PM)) EM) TM NM)
                  (bool_rect (lam q sst)
                    (smk (hupd (npred (npred PM))
                               (add (SM (npred (npred PM))) (SM (npred PM))) SM)
                         (npred PM) LM EM TM NM)
                    (bool_rect (lam q sst) (smk SM PM LM EM (hupd NM (SM (npred PM)) TM) (succ NM)) st
                      (nateqb o 7))
                    (nateqb o 6))
                  (nateqb o 5))
                (nateqb o 4))
              (nateqb o 3))
            (nateqb o 2))
          (nateqb o 1))
        (nateqb o 0)))))))))))))
 ; ⚠ one recursive call per level, the dgo discipline
 (defn 'sgo '(pi q nat (pi v (nvec q) (pi m nat (pi o nat (pi A hst (pi st sst sst))))))
   '(lam q (nat_rect (lam n (pi v (nvec n) (pi m nat (pi o nat (pi A hst (pi st sst sst))))))
             (lam v (lam m (lam o (lam A (lam st st)))))
             (lam k (lam IH (lam v (lam m (lam o (lam A (lam st
               (let h (pr1 v)
                 (IH (pr2 v)
                   (bool_rect (lam x nat) 1 0 (nateqb m 0))
                   (bool_rect (lam x nat) h o (nateqb m 0))
                   A
                   (bool_rect (lam x sst) st (sstep o h A st) (nateqb m 0)))))))))))
             q)))
 (defn 'srun '(pi l nlist (pi A hst (pi st sst sst)))
   '(lam l (lam A (lam st (sgo (pr1 l) (pr2 l) 0 0 A st))))))

; --- pinned before the corpus rides it: a push lands at TOS, and emit writes
; --- the trace without moving the stack
(assert
 (defq 'srun-push
   '(pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (paths nat (stos (srun (ncons 0 (ncons 7 nnil)) A (smk S 0 L E T 0))) 7))))))
   '(lam A (lam S (lam L (lam E (lam T (idpath 7)))))))
 (defq 'srun-emit
   '(pi A hst (pi S hst (pi L hst (pi E hst (pi T hst
      (paths nat (strn (srun (ncons 0 (ncons 5 (ncons 7 (ncons 0 nnil)))) A (smk S 0 L E T 0))) 1))))))
   '(lam A (lam S (lam L (lam E (lam T (idpath 1))))))))
