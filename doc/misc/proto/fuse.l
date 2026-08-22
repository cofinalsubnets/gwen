; layer-? spike: fold/map FUSION as a source->source rewrite -- the Slice-0 vertical
; for the functor/nt machinery (net the +-hom, map the functor). a tiny law TABLE of
; (lhs-pattern  rhs-template), a structural matcher with metavariables, and an
; instantiator -- then we PROBE: for each law, ev the ORIGINAL and the FUSED form and
; check they agree on the binary. the matcher never evals, it is pure source->source,
; so there is no sampling-soundness gap (cf. loopclose, which samples a black-box body).
; the table is the seed `cprop` would read in ev.l; here it stands alone so we can
; settle the rewrite against `ai` before touching the compiler.
;
; metavars are 'f 'g 'x 'z 'a 'b; the rhs binders (acc elt w) are chosen OUTSIDE that
; set so subst leaves them alone (no capture). run: out/host/love doc/misc/proto/fuse.l
;
; --- fit with tele (autograd) --- this spike and crew/tele/tele.l are the two halves of
; one categorical picture from opposite directions. fuse is the COMPILE-TIME symbolic
; rewrite (functor/nt laws, no eval, forward); tele is the RUNTIME define-by-run trace
; (AD as the category of LENSES, reverse). same frame -- differentiation IS a
; structure-preserving functor (Elliott, "the simple essence of AD") -- so they LAYER,
; they don't merge: fuse optimizes the dataflow tele then differentiates. they meet at
; the graph. three joins, cheapest first:
;   1. fuse AS a graph optimizer over tele's forward source -- collapse elementwise
;      chains BEFORE taping, so the fused op gets one tape node + one adjoint instead of
;      three (kernel fusion, the XLA/torch.compile move). tele's forward exprs are
;      first-order ((add (mul a b) c)) shapes the matcher ALREADY handles -- no new
;      machinery. THE join to start with.
;   2. law 3 (net.cat => +.(net x net), the +-hom) IS tele's backward pass: `back`
;      sums a twice-consumed node's cotangents (tl-accum), and tl-sumto/tl-colsum are
;      the adjoint of broadcast -- the reduce<->broadcast nt. fuse's naturality laws and
;      tele's adjoint correctness are the SAME equations; stating sumto as fuse rules
;      would machine-check them via run1's ev-both-and-compare.
;   3. differentiation AS a rule IN the table (D(map g) = map (D g), D(f.g) = ..) would
;      give SYMBOLIC forward-mode AD, complementing tele's runtime reverse mode. BLOCKED
;      on the matcher: it is deliberately FIRST-ORDER (binders left untouched so subst
;      can't capture), but D must recurse INTO (\ x body). needs binding-aware /
;      higher-order matching -- a matcher upgrade, not a table row. the pretty endgame.

(:
 none (mint 0)                                              ; the unforgeable fail/absent token
 pvars '(f g x z a b)                                       ; the pattern metavariables

 ; --- set membership over a list, by identity (symbols) ---
 (inset s l) (? (atom? l) 0 (? (id? s (cap l)) 1 (inset s (cup l))))

 ; --- env: an assoc of (var frag) 2-lists; a miss reads `none` ---
 (lookv env v) (? (atom? env) none
   (: p (cap env) (? (= v (cap p)) (cap (cup p)) (lookv (cup env) v))))

 ; --- the structural matcher: pattern x subject x env -> env | none -------------
 ;     a metavar binds (or must agree); a literal atom must be `=`; a pair recurses.
 (match pat subj env) (?
   (id? env none) none                                     ; a prior failure propagates
   (inset pat pvars) (: b (lookv env pat)
                       (? (id? b none) (link (L pat subj) env)   ; fresh -> bind
                          (= b subj) env                          ; bound -> must agree
                          none))                                  ; ... or conflict -> fail
   (atom? pat) (? (= pat subj) env none)                   ; a literal atom (head, op, ())
   (atom? subj) none                                       ; pat a pair, subj an atom
   (: e1 (match (cap pat) (cap subj) env)                  ; structural descent
       (match (cup pat) (cup subj) e1)))

 ; --- instantiate a rhs template against a match env (binders left untouched) ---
 (subst tpl env) (?
   (inset tpl pvars) (: b (lookv env tpl) (? (id? b none) tpl b))
   (atom? tpl) tpl
   (link (subst (cap tpl) env) (subst (cup tpl) env)))

 ; --- one rule, then the whole table ---
 (fuse1 rule subj) (: m (match (cap rule) subj ())
                    (? (id? m none) none (subst (cap (cup rule)) m)))
 (fuse rules subj) (? (atom? rules) none
   (: r (fuse1 (cap rules) subj) (? (id? r none) (fuse (cup rules) subj) r)))

 ; --- the law TABLE: (lhs-pattern  rhs-template) -------------------------------
 ;     1. fold/map fusion  (the catamorphism)        foldl(+) . map g  ==>  one foldl
 ;     2. functor composition                        map f . map g     ==>  map (f.g)
 ;     3. the net/cat measure-hom (naturality)        net . cat         ==>  + . (net x net)
 laws (L
   (L '(foldl (+) z (map g x))
      '(foldl (\ acc elt (+ acc (g elt))) z x))
   (L '(map f (map g x))
      '(map (\ w (f (g w))) x))
   (L '(net (cat a b))
      '(+ (net a) (net b))))

 ; --- driver: fuse a subject, ev BOTH, check they agree on the binary ----------
 (run1 nm subj) (: f (fuse laws subj)
   (? (id? f none) (: _ (puts ";; ") _ (puts nm) (puts "   NO MATCH\n"))
      (: vo (ev subj) vf (ev f) ok (= vo vf)
        (: _ (puts ";; ") _ (puts nm) _ (puts (? ok "  ok    " "  FAIL  "))
           _ (puts "orig=") _ (puts (show vo)) _ (puts "  fused=") _ (puts (show vf)) _ (putc 10)
           _ (puts ";;      ") _ (puts (show subj)) _ (puts "\n;;   => ") _ (puts (show f)) (putc 10)))))

 _ (puts ";; --- fold/map fusion, probed against the binary ---\n")
 _ (run1 "fold/map " '(foldl (+) 0 (map (\ n (* n n)) (jot 5))))
 _ (run1 "map.map  " '(map (\ p (+ p 1)) (map (\ q (* q 2)) (jot 4))))
 _ (run1 "net.cat  " '(net (cat (jot 3) (jot 2))))
 0)
