; layer-2 spike: DEFORESTATION (`forest`) -- a standalone source->source list-fusion rewrite that
; removes the intermediate lists a map/filter pipeline builds. it walks a CONSUMER over a chain of
; PRODUCERS (map/filter) and collapses the whole nest into ONE pass over the base list -- no temporary
; cells allocated between stages. this is the rewrite-LEVEL half of the recognizer chain the glaze
; (love/glaze/) uses at the codegen level: the glaze lowers a FIRST-ORDER loop to native; deforestation
; is the algebraic, higher-order transform UPSTREAM that turns a combinator pipeline INTO such a loop.
; the two COMPOSE -- deforest a list pipeline to a fused fold, then glaze the fold -- exactly the way
; doc/misc/proto/mix.l's interpreter-specialization composes with the glaze.
;
; the engine is the TRANSDUCER view of shortcut fusion. every consumer reduces the source with a step
; over (acc, elem); every producer is a transformer of that step:
;   map f     : elem |-> (f elem)                          (substitute the element)
;   filter p  : step |-> (? (p elem) step <skip>)          (guard the step; <skip> leaves acc/tail)
; so absorbing the producers nearest the consumer first, inward to the base, threads ONE step body
; through the whole chain. map-after-map composes the element substitutions (g (f e)); filter-after-
; map guards the composed element. the base list is the only thing iterated; the only allocation left
; is whatever the CONSUMER itself builds (a scalar for foldl, one fresh list for a to-list pipeline).
;
; the consumers recognized: foldl foldr each all any, and a BARE producer chain (its result IS a list,
; rebuilt in one foldr/link pass). the producers: map filter. everything else is traversed into (so a
; pipeline nested inside a stage function, the fold seed, or the base list is fused too). the glaze-path
; subset (foldl consumer, map/filter producers, jot source) is now WIRED INTO love/glaze/auto.l (defoliate):
; a numeric map/jot/fold pipeline compiles to a native counted loop, == interp.
;
; run:    out/host/love doc/misc/proto/forest.l              (the demo; standalone `assert` is lenient -- it
;                                                       RETURNS a value, does not halt, so eyeball the shown forms)
; strict: cat test/00-init.l doc/misc/proto/forest.l | out/host/love   (00-init makes `assert` enforce -- the real gate)
;
; HYGIENE: the step's element/acc binders are fresh gensym noms ((nom 'e)/(nom 'a)) -- unforgeable, so
; the naive symbol substitution `sub` can never capture a name in the user's stage source (which only
; ever SEES those vars as the values passed to it). NB the house nil-trap: end-of-list is the SPINE
; test (two?/`nul?` below), never (! l) -- `!` reads the NET, and a list of falsy cells nets to nothing.
; forest/peel are mutually recursive (peel forests the stage fns and base), so they share one body-
; less `:` letrec -- boxfix resolves the forward edges.

(:
 (nul? l) (! (two? l))                                  ; end-of-list = spine test, not the net

 ; --- source builders (prefix everywhere; opfix never sees infix in what we emit) ---
 (ap1 f x)    (link f (link x ()))                      ; (f x)
 (ap2 f x y)  (link f (link x (link y ())))             ; (f x y)
 (lam1 e b)   (link '\ (link e (link b ())))            ; (\ e b)
 (lam2 a e b) (link '\ (link a (link e (link b ()))))   ; (\ a e b)
 (cnd3 t a b) (link '? (link t (link a (link b ()))))   ; (? t a b)

 ; --- naive symbol substitution: replace symbol `old` by expr `new` throughout B. `old` is a fresh
 ;     gensym, so it never appears as a binder -- a leaf rewrite is fully hygienic. it RECURSES into a
 ;     previously-inserted `new` (which contains old), so stacked maps compose: (g (f e)). ---
 (sub old new B) (? (id? B old) new
                    (two? B)    (link (sub old new (cap B)) (sub old new (cup B)))
                    B)

 ; --- stage recognizers over source ---
 (head? x h) (&& (two? x) (id? (cap x) h))
 (map? P)    (head? P 'map)
 (filt? P)   (head? P 'filter)
 (prod? P)   (|| (map? P) (filt? P))
 (sfn P)     (caup P)                                   ; (map f P') -> f      (the stage function)
 (ssrc P)    (cap (cuup P))                             ; (map f P') -> P'     (the upstream source)

 ; --- the producer peeler: thread the step body B over (elem `e`, with the filter skip-value `felse`)
 ;     through the producer chain P, inward to the base. returns (B' . BASE) as a 2-list (B' BASE). ---
 (peel B e felse P)
   (? (map? P)  (peel (sub e (ap1 (forest (sfn P)) e) B) e felse (ssrc P))   ; e := (f e)
      (filt? P) (peel (cnd3 (ap1 (forest (sfn P)) e) B felse) e felse (ssrc P)) ; (? (p e) B felse)
      (L B (forest P)))                                                       ; base: forest nested pipelines

 ; --- consumer drivers. each names a fresh step over its base body, peels the chain, reassembles. ---
 (deffold x tag)                                        ; (foldl/foldr c z P)
   (: as (cup x) c (cap as) z (caup as) P (caup (cup as))
      e (nom 'e) a (nom 'a)
      B0 (? (id? tag 'foldl) (ap2 c a e) (ap2 c e a))   ; foldl: (c acc elem); foldr: (c elem acc)
      r  (peel B0 e a P)
      hd (? (id? tag 'foldl) (lam2 a e (cap r)) (lam2 e a (cap r)))
      (L tag hd (forest z) (caup r)))
 (defeach x)                                            ; (each l f) -- effect; skip-value is ()
   (: as (cup x) l (cap as) f (caup as)
      e (nom 'e)  r (peel (ap1 f e) e () l)
      (L 'each (caup r) (lam1 e (cap r))))
 (defaa x tag unit)                                     ; (all/any q P) -- skip-value is the short-circuit unit
   (: as (cup x) q (cap as) P (caup as)
      e (nom 'e)  r (peel (ap1 q e) e unit P)
      (L tag (lam1 e (cap r)) (caup r)))
 (defto x)                                              ; a bare producer chain -> one foldr/link pass
   (: e (nom 'e) a (nom 'a)
      r (peel (ap2 'link e a) e a x)
      (L 'foldr (lam2 e a (cap r)) () (caup r)))

 ; --- a lambda: forest its body, keep its params; a one-operand (\ datum) is quote -- leave it. ---
 (lamforest l) (? (nul? (cup l)) (L (forest (cap l))) (link (cap l) (lamforest (cup l))))
 (deflam x)    (? (nul? (cuup x)) x (link '\ (lamforest (cup x))))

 ; --- simp: reduce the fused residual to FIRST-ORDER form so the glaze can take it. two rewrites,
 ;     bottom-up: BETA-inline a unary stage lambda ((\ p B) arg) -> B[p:=arg], and FLATTEN a curried
 ;     operator head ((op a..) x..) -> (op a.. x..) (sound by ((f a) b) == (f a b)). a (\ datum) quote
 ;     (incl. every '(..) source-list literal) is left whole. NB beta here assumes the stage lambda
 ;     does not SHADOW its own param -- sound for the simple element fns map/filter take; a general
 ;     pass would alpha-rename. the equivalence asserts run the residual through ev and would catch it. ---
 (special? h) (|| (id? h '?) (id? h '\) (id? h ':))
 (simp x)
   (? (atom? x)                              x
      (&& (id? (cap x) '\) (2 = (tally x)))  x                         ; (\ datum) -> quote, leave
      (: y (link (simp (cap x)) (map simp (cup x))) h (cap y) as (cup y)
         (? (atom? h)                                            y
            (&& (id? (cap h) '\) (&& (3 = (tally h)) (1 = (tally as))))  ; ((\ p B) arg) -> beta
               (simp (sub (caup h) (cap as) (cap (cuup h))))
            (&& (atom? (cap h)) (! (special? (cap h))))               ; ((op a..) x..) -> flatten
               (simp (link (cap h) (cat (cup h) as)))
            y)))
 (deforest x) (simp (forest x))             ; the public entry: fuse, then reduce to a clean loop

 ; --- the pass: fuse at a recognized consumer/chain, else traverse into the children ---
 (forest x)
   (? (atom? x)          x
      (head? x 'foldl)   (deffold x 'foldl)
      (head? x 'foldr)   (deffold x 'foldr)
      (head? x 'each)    (defeach x)
      (head? x 'all)     (defaa x 'all 1)
      (head? x 'any)     (defaa x 'any 0)
      (prod? x)          (defto x)
      (head? x '\)       (deflam x)
      (link (cap x) (map forest (cup x))))

 ; --- structural proofs (name-independent). clean?: no map/filter head survives (fusion happened).
 ;     noredex?: no lambda sits in operator position (the residual is FIRST-ORDER -- glaze-ready). ---
 (alls l)   (? (nul? l) 1 (? (cap l) (alls (cup l)) 0))
 (clean? x) (? (atom? x) 1 (prod? x) 0 (alls (map clean? x)))
 (noredex? x) (? (atom? x) 1 (&& (two? (cap x)) (id? (cap (cap x)) '\)) 0 (alls (map noredex? x)))
 (firstorder? x) (&& (clean? x) (noredex? x))

 ; ============================ self-checks ============================
 ; the equivalence method (as in mix.l): the deforested source computes the SAME value as the
 ; original, and is producer-free AND first-order (every intermediate list gone, fully reduced).
 (ok? orig) (&& (= (ev orig) (ev (deforest orig))) (firstorder? (deforest orig)))

 ; fold over a map -> ONE loop (the glaze-ready shape: a single foldl, no inner producer)
 A '(foldl (+) 0 (map (\ x (* x x)) '(1 2 3 4)))                            ; 1+4+9+16 = 30
 _ (assert (ok? A) "fold/map fuses, value preserved")
 _ (assert (head? (deforest A) 'foldl) "fold/map -> a single foldl")
 ; map after filter: the filter guards the composed step
 B '(foldl (+) 0 (map (+ 1) (filter (\ x (> x 2)) '(1 2 3 4 5))))           ; (3 4 5)->+1->(4 5 6)->15
 _ (assert (ok? B) "fold/map/filter fuses, value preserved")
 ; map after map: the element substitutions compose -- (+ 1 (* 2 e))
 C '(foldl (+) 0 (map (+ 1) (map (* 2) '(1 2 3))))                          ; (2 4 6)->+1->(3 5 7)->15
 _ (assert (ok? C) "map/map composes (no middle list)")
 ; a BARE pipeline whose result is a list -> rebuilt in one foldr/link pass
 D '(map (+ 1) (filter (\ x (> x 2)) '(1 2 3 4 5)))                         ; the list (4 5 6)
 _ (assert (ok? D) "bare chain -> one list-building pass")
 _ (assert (head? (deforest D) 'foldr) "bare chain -> a single foldr")
 ; foldr, each (effect), all, any consumers
 E '(foldr link () (map (+ 1) '(1 2 3)))                                    ; (2 3 4)
 _ (assert (ok? E) "foldr consumer fuses")
 F '(all (\ x (> x 0)) (map (+ 1) '(0 1 2)))                                ; +1 -> (1 2 3), all>0 = 1
 _ (assert (ok? F) "all consumer fuses (predicate absorbs the map)")
 G '(any (\ x (> x 9)) (map (* 2) '(1 2 3)))                                ; (2 4 6), any>9 = false
 _ (assert (ok? G) "any consumer fuses")
 ; a pipeline nested in the BASE position fuses too (forest recurses into the base)
 H '(foldl (+) 0 (filter (\ x (> x 4)) (map (* 2) (map (+ 1) '(1 2 3)))))   ; +1(2 3 4)*2(4 6 8) keep>4 (6 8)=14
 _ (assert (ok? H) "deep chain (filter . map . map) fuses end to end")

 ; glaze composition (NOW WIRED -- love/glaze/auto.l `defoliate`): a numeric fold-over-map fuses to a
 ; single first-order accumulator loop -- (foldl (\ a e (+ a (* e e))) 0 (jot N)) -- which auto.l lowers
 ; to the named-let LOOP its existing lift/group recognizers compile to a NATIVE counted loop. So a
 ; map/jot/fold pipeline glazes end to end (== interp, ~19x faster at N=2e5: the intermediate lists are
 ; gone). The source must be `jot` (the materialized list foldl/map iterate), NOT `iota` (the ARRAY,
 ; which chain-ops don't traverse -- a foldl over it is degenerate, so it is left interpreted).
 I '(foldl (+) 0 (map (\ x (* x x)) (jot 100)))                            ; sum of squares 0..99 = 328350
 _ (assert (= 328350 (ev I)) "interp baseline for the glaze-shape pipeline")
 _ (assert (ok? I) "fold/map over (jot N) fuses to a first-order loop")
 _ (assert (firstorder? (deforest I)) "...producer- & redex-free: the loop auto.l glazes")

 _ (. ";; doc/misc/proto/forest.l: deforestation self-checks pass -- ")
 _ (. "map/filter pipelines fuse to one first-order pass (value-preserving, producer- & redex-free)\n")
 _ (. ";; fold/map   -- ") _ (. (show A)) _ (. "\n;;            -> ") _ (. (show (deforest A))) _ (. "\n")
 _ (. ";; map/filter -- ") _ (. (show B)) _ (. "\n;;            -> ") _ (. (show (deforest B))) _ (. "\n")
 _ (. ";; glaze-ready-- ") _ (. (show I)) _ (. "\n;;            -> ") _ (. (show (deforest I))) _ (. "\n"))
