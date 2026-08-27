; test/uuvallaw.l -- GENERATED from test/law.l by tools/law2uu.l. Do not edit;
; refresh with `make uuvallaw`. CLAUDE.md's laws, ONE SPELLING for both lanes:
; test/law.l quantifies each row over a corpus (refutation), and the rows the
; measure tower can state arrive here as uu OBLIGATIONS over test/uuval.l's
; model (proof). the source row prints over its term, and every proof was
; SEARCHED against the live kernel -- by conversion where the tower's round
; trips are definitional, by cases on the saturated measure where they are
; not, and otherwise by the lemma the model names for that row.
; a guard the tower cannot read erases (the model has one uniform value),
; which makes the obligation STRONGER than the row it came from, never
; weaker; each such row names the guard it dropped. a guard it CAN read
; stays, as the orb it always was. Substrate: test/uuval.l over test/uu.l.

(assert (member? 'vof (names ())))

(assert
 ; cap-total   (\ x (? (two? x) 1 (= x (cap x))))
 ; by the lemma the model names
 (defq law-cap-total
   (pi v val (paths bool (orb (vtwo v) (veq v (vcap v))) true))
   uv-cap-total)
 ; cup-total   (\ x (? (two? x) 1 (= () (cup x))))
 ; by the lemma the model names
 (defq law-cup-total
   (pi v val (paths bool (orb (vtwo v) (veq vnilv (vcup v))) true))
   uv-cup-total)
 ; link-apart   (\ x (: p (>< x x) (&& (= (cap p) x) (= (cup p) x))))
 ; by the lemma the model names
 (defq law-link-apart
   (pi v val (paths bool (andb (veq (vcap (vlink v v)) v) (veq (vcup (vlink v v)) v)) true))
   uv-link-apart)
 ; link-back   (\ x (? (two? x) (= x (link (cap x) (cup x))) 1))
 ; by the lemma the model names
 (defq law-link-back
   (pi v val (paths bool (orb (negb (vtwo v)) (veq v (vlink (vcap v) (vcup v)))) true))
   uv-link-back)
 ; =-total   (\ x (: b (= x x) (= b 1)))
 ; by the lemma the model names
 (defq law-eq-total
   (pi v val (paths nat (nbit (veq v v)) 1))
   uv-eq-total)
 ; id?-finer   (\ x (? (id? x x) (= x x) 1))
 ; by the lemma the model names; the guard (id? x x) erases
 (defq law-idq-finer
   (pi v val (paths bool (veq v v) true))
   uv-idq-finer)
 ; net-idem   (\ x (: n (net x) (= n (net n))))
 ; by conversion
 (defq law-net-idem
   (pi v val (paths msr (vnet v) (vnet (vnum (vnet v)))))
   (lam v (idpath (vnet v))))
 ; re-idem   (\ x (? (coin? x) 1 (: r (re (net x)) (= r (re (net r))))))
 ; by conversion; the guard (coin? x) erases
 (defq law-re-idem
   (pi v val (paths rea (mre (vnet v)) (mre (vnet (rval (mre (vnet v)))))))
   (lam v (idpath (mre (vnet v)))))
 ; ceil-idem   (\ x (: c (ceil x) (= c (ceil c))))
 ; by conversion
 (defq law-ceil-idem
   (pi v val (paths zed (vceil v) (vceil (zval (vceil v)))))
   (lam v (idpath (vceil v))))
 ; sat-idem   (\ x (: s (saturate x) (= s (saturate s))))
 ; by conversion
 (defq law-sat-idem
   (pi v val (paths nat (vsat v) (vsat (nval (vsat v)))))
   (lam v (idpath (vsat v))))
 ; bit-idem   (\ x (: b (bit x) (= b (bit b))))
 ; by cases on the saturated measure
 (defq law-bit-idem
   (pi v val (paths bool (vbit v) (vbit (bval (vbit v)))))
   (lam v (nat_rect (lam n (paths bool (vbit (nval n)) (vbit (bval (vbit (nval n))))))
                     (idpath (vbit (nval 0)))
                     (lam k (lam r (idpath (vbit (nval (succ k))))))
                     (vsat v))))
 ; ceil-net   (\ x (? (coin? x) 1 (= (ceil x) (ceil (re (net x))))))
 ; by conversion; the guard (coin? x) erases
 (defq law-ceil-net
   (pi v val (paths zed (vceil v) (vceil (rval (mre (vnet v))))))
   (lam v (idpath (vceil v))))
 ; sat-ceil   (\ x (= (saturate x) (? (< 0 (ceil x)) (ceil x) 0)))
 ; by conversion
 (defq law-sat-ceil
   (pi v val (paths nat (vsat v) (zsat (vceil v))))
   (lam v (idpath (vsat v))))
 ; bit-sat   (\ x (= (bit x) (< 0 (saturate x))))
 ; by cases on the saturated measure
 (defq law-bit-sat
   (pi v val (paths bool (vbit v) (natgtb (vsat v) 0)))
   (lam v (nat_rect (lam n (paths bool (vbit (nval n)) (natgtb (vsat (nval n)) 0)))
                     (idpath (vbit (nval 0)))
                     (lam k (lam r (idpath (vbit (nval (succ k))))))
                     (vsat v))))
 ; nil?-bit   (\ x (= (nil? x) (= 0 (saturate x))))
 ; by cases on the saturated measure
 (defq law-nilq-bit
   (pi v val (paths bool (vnil v) (niszero (vsat v))))
   (lam v (nat_rect (lam n (paths bool (vnil (nval n)) (niszero (vsat (nval n)))))
                     (idpath (vnil (nval 0)))
                     (lam k (lam r (idpath (vnil (nval (succ k))))))
                     (vsat v))))
 ; sat-net   (\ x (= (saturate x) (saturate (net x))))
 ; by conversion
 (defq law-sat-net
   (pi v val (paths nat (vsat v) (vsat (vnum (vnet v)))))
   (lam v (idpath (vsat v))))
 ; bit-nil   (\ x (= (bit x) (nil? (nil? x))))
 ; by cases on the saturated measure
 (defq law-bit-nil
   (pi v val (paths bool (vbit v) (vnil (bval (vnil v)))))
   (lam v (nat_rect (lam n (paths bool (vbit (nval n)) (vnil (bval (vnil (nval n))))))
                     (idpath (vbit (nval 0)))
                     (lam k (lam r (idpath (vbit (nval (succ k))))))
                     (vsat v))))
)

; --- rows whose CODOMAIN is the proof: the tower assigns the type, so the
; --- range check has nothing left to state ---
 ;   nil?-total    lands in bool
 ;   sat-green     lands in nat
 ;   bit-bool      lands in bool

; --- off the model, with the reason (skipped and counted, never dropped) ---
 ;   cap           the sigil lane: (= e e) after opfix
 ;   cup           the sigil lane: (= e e) after opfix
 ;   net           the sigil lane: (= e e) after opfix
 ;   saturate      the sigil lane: (= e e) after opfix
 ;   nil?          the sigil lane: (= e e) after opfix
 ;   abs           the sigil lane: (= e e) after opfix
 ;   negate        the sigil lane: (= e e) after opfix
 ;   jot           the sigil lane: (= e e) after opfix
 ;   ratio-rungs   off the model: a (: ..) binding the tower cannot follow
 ;   add-assoc     off the model: a (: ..) binding the tower cannot follow
 ;   mul-assoc     off the model: a (: ..) binding the tower cannot follow
 ;   mul-reassoc   off the model: ?
 ;   mul-count-out off the model: ?
 ;   mul-tally-hom off the model: ?
 ;   mul-dist      off the model: a (: ..) binding the tower cannot follow
 ;   apply-is-id   off the model: $
 ;   unit-f        off the model: a (: ..) binding the tower cannot follow
 ;   curry         off the model: (+ x)
 ;   seq           off the model: a (: ..) binding the tower cannot follow
 ;   empty-form    off the model: :

 ; 17 obligations / 40 law rows; 3 carried by a type, 20 off the model.
