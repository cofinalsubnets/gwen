; tools/law2uu.l -- EMIT test/uuvallaw.l: CLAUDE.md's laws, ONE SPELLING for both
; lanes. test/law.l quantifies each law as a lambda over a corpus (refutation);
; this tool reads THOSE ROWS as data and compiles the ones the measure tower can
; state into uu obligations over test/uuval.l's model (proof). Run on the host:
;
;   love tools/law2uu.l > test/uuvallaw.l        (or: make uuvallaw)
;
; so a law is written once. edit its spelling in law.l and the obligation moves
; with it -- `make test_uuvallaw` regenerates and diffs, and the proof is re-found.
;
; the lift is KIND-DIRECTED by the tower itself: every expression has a level in
;   bool -- nat -- zed -- rea -- msr -- val
; and a word fixes the level of its argument (net wants a val, re wants a msr)
; and of its answer. an argument at the wrong level is coerced along the SECTIONS
; going up (nbit nzed zrea rmsr vnum, or one of the named val shortcuts nval zval
; rval bval) and the RETRACTIONS going down -- so (net (net x)), whose inner net
; answers a measure where the outer wants a value, lifts to (vnet (vnum (vnet v)))
; and needs no rule of its own. that is the whole type oracle: crew/lux/sigs.l does
; this job for wm2uu, and here the tower is its own.
;
; the PROOF is searched, not transcribed. three strategies, tried in order
; against the live kernel (the tool loads test/uu.l + test/uuval.l and runs defq):
;   1. BY CONVERSION -- (lam v (idpath LHS)). the tower's round trips are
;      definitional, so most rungs land here.
;   2. BY CASES ON THE SATURATED MEASURE -- nat_rect on (vsat v), both arms
;      idpath. the same translation run at v := (nval 0) and v := (nval (succ k))
;      gives the two arms and at v := (nval n) the motive; (vsat (nval n)) is n,
;      so the motive at (vsat v) converts back to the obligation.
;   3. BY THE LEMMA THE MODEL NAMES -- `uv-<row>` in test/uuval.l, cited bare. the
;      structural laws (cap/cup/link, veq's reflexivity) need an induction over
;      the tree and are hand-proved there; the OBLIGATION still comes from law.l,
;      so a row that moves leaves its lemma behind and the tool says so.
; a row no strategy discharges is SKIPPED and COUNTED, never silently dropped.
;
; a GUARD the tower cannot read erases -- (? (coin? x) 1 e) -- the way core.l's
; ()-lane erases under wm2uu, and the obligation it makes is then STRONGER than
; the row, not weaker; each such row names the guard it dropped. a guard the
; tower CAN read (two?) stays, as the orb it always was. a `:` binds into the
; environment, which is substitution at the term level. `?` and `=` are no name
; characters downstream (uu2coq emits Coq), so they spell q and eq in an emitted
; name, the way kinds2uu writes numq for num?.
;
; love's `=` between two VALUES is veq, not paths: the depth index is a height
; bound, so one value has many spellings and identity is finer than love's own
; equality. below val the carriers are canonical and the claim stays a path.
;
; untrusted like every translator here: a wrong lift can miss a law, never fake
; one, and each row prints its source form over its term.
;
; load the kernel and the model FIRST, in their own form -- the next form's bare
; references resolve at ITS compile, which must follow this run. their asserts
; ride a jug meanwhile so the artifact stays clean.
(: j (jug 0) _ (wear [in j err])
   _ (reads (open "test/uu.l" "r"))
   _ (reads (open "test/uuval.l" "r"))
   _ (wear ()) ())

(:
   nl (string 10)
   (join l) (foldl (+) "" l)
   (pl s) (puts (s + nl))
   (at l i) (? (0 < i) (at >l (i - 1)) <l)
   (idxin x l k) (? (! (two? l)) -1 (<l = x) k (idxin x >l (k + 1)))
   (look k tbl) (? (! (two? tbl)) () (id? <<tbl k) <tbl (look k >tbl))
   (zero? e) (charm? e && e = 0)
   (one? e) (charm? e && e = 1)
   (same? a b) ((show a) = (show b))
   ; an OPERATOR head is matched by its spelling: the lambda sigil has no quote
   ; form to compare against, and neither `?` nor `:` reads back as a bare nom.
   (is? x s) (nom? x && (string x) = s)
   (hd? e s) (two? e && is? <e s)
   bs (intern "\\")
   (unq x) (? (two? x && id? <x bs) (at x 1) x)     ; reads shows a quote as (\ ..)

   ; --- THE TOWER, as a type oracle: the levels, and the rung between each pair ---
   lv-order '(bool nat zed rea msr val)
   (ht l) (idxin l lv-order 0)
   up-rungs '((bool nbit nat) (nat nzed zed) (zed zrea rea) (rea rmsr msr) (msr vnum val))
   dn-rungs '((val vnet msr) (msr mre rea) (rea rceil zed) (zed zsat nat) (nat natbit bool))
   to-val   '((msr vnum) (rea rval) (zed zval) (nat nval) (bool bval))
   (co t f g)
    (? (id? f g) t
       (id? g 'val) [(at (look f to-val) 1) t]
       ((ht f) < (ht g)) (: r (look f up-rungs) (co [(at r 1) t] (at r 2) g))
       (: r (look f dn-rungs) (co [(at r 1) t] (at r 2) g)))

   ; --- the word map: each love word, the level it wants, its model name, its
   ; --- answer. love's ceil/saturate/bit/nil? read a WHOLE value (they compose
   ; --- the rungs below them), which is why they all want a val.
   wordmap '((net val vnet msr) (re msr mre rea) (ceil val vceil zed)
             (saturate val vsat nat) (bit val vbit bool) (nil? val vnil bool)
             (cap val vcap val) (cup val vcup val) (two? val vtwo bool))
   ; the binary words: what each side wants, the model's name, what it answers
   wordmap2 '((link val val vlink val) (>< val val vlink val)
              (&& bool bool andb bool) (|| bool bool orb bool))

   ; --- tx: an expression to [1 term level], or [0 the-word-that-stopped-it] ---
   (ok t l) [1 t l]
   (bad r) [0 r]
   (won? r) (two? r && one? <r)
   (un1 a env want mk res)
    (: r (tx a env) (? (! (won? r)) r (ok (mk (co (at r 1) (at r 2) want)) res)))
   (un2 e env w)
    (: A (tx (at e 1) env) B (tx (at e 2) env)
       (? (! (won? A)) A (! (won? B)) B
          (ok [(at w 3) (co (at A 1) (at A 2) (at w 1)) (co (at B 1) (at B 2) (at w 2))]
              (at w 4))))
   ; love's `=` between two VALUES is structural, not identity -- that is what veq
   ; is for. below val the carriers are canonical and their own eqb reads them.
   (eqof e env)
    (: A (tx (at e 1) env) B (tx (at e 2) env)
       (? (! (won? A)) A (! (won? B)) B
          (: la (at A 2) lb (at B 2) L (? ((ht la) < (ht lb)) lb la)
             ta (co (at A 1) la L) tb (co (at B 1) lb L)
             (? (id? L 'bool) (ok ['beqb ta tb] 'bool)
                (id? L 'nat)  (ok ['nateqb ta tb] 'bool)
                (ok ['veq (co ta L 'val) (co tb L 'val)] 'bool)))))
   ; a guard the tower CAN read stays: it becomes the implication it was
   (guard-orb e env)
    (: P (tx (at e 1) env)
       (? (! (won? P)) P
          (: yes (one? (at e 2))
             E (tx (? yes (at e 3) (at e 2)) env)
             (? (! (won? E)) E
                (: p (co (at P 1) (at P 2) 'bool)
                   (ok ['orb (? yes p ['negb p]) (co (at E 1) (at E 2) 'bool)] 'bool))))))
   ; (? (< 0 E) E 0) at the integers IS the clamp -- saturate's own shape
   (clamp? e)
    (: c (at e 1)
       (hd? c "<" && (tally c) = 3 && zero? (at c 1)
        && same? (at c 2) (at e 2) && zero? (at e 3)))
   (tx e env)
    (? (charm? e) (ok e 'nat)
       (nom? e) (: p (look e env) (? (two? p) (ok (at p 1) (at p 2)) (bad (show e))))
       (id? e ()) (ok 'vnilv 'val)                       ; the empty list IS the model's ()
       (! (two? e)) (bad (show e))
       (: h <e n (tally e) w (look h wordmap) w2 (look h wordmap2)
        (? (n = 2 && two? w) (un1 (at e 1) env (at w 1) (\ t [(at w 2) t]) (at w 3))
           (n = 3 && two? w2) (un2 e env w2)
           (n = 4 && is? h "?" && clamp? e)
            (un1 (at e 2) env 'zed (\ t ['zsat t]) 'nat)
           (n = 4 && is? h "?" && (one? (at e 2) || one? (at e 3))) (guard-orb e env)
           ; (< 0 E) and (= 0 E) on a saturated measure: the bit and its complement
           (n = 3 && is? h "<" && zero? (at e 1))
            (un1 (at e 2) env 'nat (\ t ['natgtb t 0]) 'bool)
           (n = 3 && is? h "=" && zero? (at e 1))
            (un1 (at e 2) env 'nat (\ t ['niszero t]) 'bool)
           (n = 3 && is? h "=" && zero? (at e 2))
            (un1 (at e 1) env 'nat (\ t ['niszero t]) 'bool)
           (n = 3 && is? h "=") (eqof e env)
           (bad (show h)))))

   ; --- the body: peel a band guard, bind a `:` into the environment, then read
   ; --- the claim. a guard that erases is reported, never dropped in silence.
   (guard1? b) (hd? b "?" && (tally b) = 4 && (one? (at b 2) || one? (at b 3)))
   ; only a guard the tower CANNOT read erases; one it can becomes an orb above
   (peel b env) (? (guard1? b && ! (won? (tx (at b 1) env)))
                   [(at (peel (? (one? (at b 2)) (at b 3) (at b 2)) env) 0) (show (at b 1))]
                   [b ()])
   (letish? b) (hd? b ":" && (tally b) = 4 && nom? (at b 1))

   ; --- claim: the translated obligation body, and its left side ---
   (claim b env)
    (? (letish? b)
        (: r (tx (at b 2) env)
           (? (! (won? r)) r
              (claim (at b 3) ([(at b 1) (at r 1) (at r 2)] >< env))))
       (hd? b "=" && (tally b) = 3)
        (: A (tx (at b 1) env) B (tx (at b 2) env)
           (? (! (won? A)) A (! (won? B)) B
              (: la (at A 2) lb (at B 2)
                 L (? ((ht la) < (ht lb)) lb la)
                 ta (co (at A 1) la L) tb (co (at B 1) lb L)
                 ; at val the claim is veq's; below it the carrier is canonical
                 (? (id? L 'val) (: q ['veq ta tb] [1 ['paths 'bool q 'true] q])
                    [1 ['paths L ta tb] ta]))))
       ; anything else the tower reads as a BOOL is the claim "..and it is true"
       (: r (tx b env)
          (? (won? r && id? (at r 2) 'bool) [1 ['paths 'bool (at r 1) 'true] (at r 1)]
             (won? r) (bad ("a claim at the " + (string (at r 2)) + " rung"))
             (bad (? (hd? b ":") "a (: ..) binding the tower cannot follow"
                     (two? b) (show <b)
                     (show b))))))

   ; --- a RANGE CHECK is not an obligation: the tower already assigns the
   ; --- codomain, so (|| (= b 0) (= b 1)) and (&& (<= 0 s) (charm? s)) have
   ; --- nothing left to say. detected by shape, answered by the type.
   (unlet b env) (? (letish? b)
                    (: r (tx (at b 2) env)
                       (? (! (won? r)) ()
                          (unlet (at b 3) ([(at b 1) (at r 1) (at r 2)] >< env))))
                    [b env])
   (eq0? c) (hd? c "=" && (tally c) = 3 && zero? (at c 2))
   (eq1? c) (hd? c "=" && (tally c) = 3 && one? (at c 2))
   (bool-range? e) (hd? e "||" && (tally e) = 3 && eq0? (at e 1) && eq1? (at e 2))
   (nat-range? e) (hd? e "&&" && (tally e) = 3
                   && (: c (at e 1) (hd? c "<=" && zero? (at c 1)))
                   && (hd? (at e 2) "charm?"))
   (range? b env)
    (: c (unlet (at (peel b env) 0) env)
       (? (! (two? c)) ()
          (: e (at c 0) en (at c 1)
             (? (! (bool-range? e) && ! (nat-range? e)) ()
                (: inner (? (bool-range? e) (at (at e 1) 1) (at (at e 1) 2))
                   r (tx inner en)
                   (? (won? r) (at r 2) ()))))))

   ; --- the kernel, live: a strategy is TRIED, not asserted ---
   dq (uu 'defq)
   _ (hear (\ p e (: h (peep HELPC () 0) (? h (h p e) 0))))
   (checks? nm ty tm) (id? 'accepted (rejects (\ z (dq nm ty tm))))

   ; --- the rows ---
   fs (forms (open "test/law.l" "r"))
   (findb l) (? (! (two? l)) () (id? <l 'law-rows) (at l 1) (findb >l))
   rows >(findb >(cap fs))
   (qstrip s) (: (go i) (? (i = tally s) ""
                            ((? (s i = 63) "q" (s i = 61) "eq" (string (s i))) + go (i + 1)))
                 (go 0))                                 ; ? is no name character downstream
   (surface? b) (hd? b "=" && (tally b) = 3 && same? (at b 1) (at b 2))

   ; --- one row, all the way to [nm src type proof how guard], [nm 'range level]
   ; --- or [nm () reason] ---
   (row1 r0)
    (: r (unglue (opfix r0)) nm (unq (at r 1)) lm (at r 2)
       (? (! (two? lm) || ! (id? <lm bs) || ! (nom? (at lm 1))) [nm () "not a lambda"]
        (: x (at lm 1) b0 (at lm 2)
           g (peel b0 [[x (quote v) (quote val)]]) b (at g 0)
           (? (one? b) [nm () "trivially 1"]
              (: rg (range? b0 [[x 'v 'val]])
                 (? (nom? rg) [nm 'range (string rg)]
                  (? (surface? b && (id? (at g 1) ())) [nm () "the sigil lane: (= e e) after opfix"]
                   (: cl (claim b [[x 'v 'val]])
                      (? (! (won? cl)) [nm () ("off the model: " + (at cl 1))]
                       (: ty ['pi 'v 'val (at cl 1)]
                          gnm (intern ("law-" + (qstrip (show nm))))
                          p1 ['lam 'v ['idpath (at cl 2)]]
                          (? (checks? gnm ty p1) [nm r0 ty p1 (quote conv) (at g 1)]
                             (: mo (claim b [[x ['nval 'n] 'val]])
                                b0v (claim b [[x ['nval 0] 'val]])
                                bkv (claim b [[x ['nval ['succ 'k]] 'val]])
                                p2 ['lam 'v ['nat_rect ['lam 'n (at mo 1)]
                                                       ['idpath (at b0v 2)]
                                                       ['lam 'k ['lam 'r ['idpath (at bkv 2)]]]
                                                       ['vsat 'v]]]
                                p3 (intern ("uv-" + (qstrip (show nm))))
                                (? (checks? gnm ty p2)
                                   [nm r0 ty p2 (quote cases) (at g 1)]
                                   (checks? gnm ty p3)
                                   [nm r0 ty p3 (quote lemma) (at g 1)]
                                   [nm () "no proof found"])))))))))))))

   done (map row1 rows)
   (rng? d) (id? (at d 1) 'range)
   proved (filter (\ d (two? (at d 1))) done)
   ranged (filter rng? done)
   missed (filter (\ d (! (two? (at d 1)) && ! (rng? d))) done)
   (pad s n) (? ((tally s) < n) (pad (s + " ") n) s)
   (srcof d) (show (at (unglue (opfix (at d 1))) 2))

   ; --- emit ---
   _ (pl "; test/uuvallaw.l -- GENERATED from test/law.l by tools/law2uu.l. Do not edit;")
   _ (pl "; refresh with `make uuvallaw`. CLAUDE.md's laws, ONE SPELLING for both lanes:")
   _ (pl "; test/law.l quantifies each row over a corpus (refutation), and the rows the")
   _ (pl "; measure tower can state arrive here as uu OBLIGATIONS over test/uuval.l's")
   _ (pl "; model (proof). the source row prints over its term, and every proof was")
   _ (pl "; SEARCHED against the live kernel -- by conversion where the tower's round")
   _ (pl "; trips are definitional, by cases on the saturated measure where they are")
   _ (pl "; not, and otherwise by the lemma the model names for that row.")
   _ (pl "; a guard the tower cannot read erases (the model has one uniform value),")
   _ (pl "; which makes the obligation STRONGER than the row it came from, never")
   _ (pl "; weaker; each such row names the guard it dropped. a guard it CAN read")
   _ (pl "; stays, as the orb it always was. a val-rung claim is veq's -- a guarded row
; needs a DECIDABLE claim, and a path is not a bool -- and where the guard
; erased the row gets its PATHS twin over the pinned carrier beside it, by
; uuval.l's bridge. Substrate: test/uuval.l over test/uu.l.")
   _ (pl "")
   _ (pl "(assert (member? 'vof (names ())))")
   _ (pl "")
   _ (pl "(assert")
   ; the cases proof wears its four operands one to a line: the motive is the
   ; obligation again at (nval n), and reading them stacked is the point
   (proofof d)
    (: p (at d 3)
       (? (! (id? (at d 4) 'cases)) ("   " + (show p) + ")")
          (: nr (at p 2)
             (join ["   (lam v (nat_rect " (show (at nr 1)) nl
                    "                     " (show (at nr 2)) nl
                    "                     " (show (at nr 3)) nl
                    "                     " (show (at nr 4)) ")))"]))))
   (howof d) (? (id? (at d 4) 'conv)  "by conversion"
                (id? (at d 4) 'cases) "by cases on the saturated measure"
                "by the lemma the model names")
   ; a val-rung claim is veq's, because a GUARDED row needs a decidable one --
   ; an orb takes a bool and a path is not one. where the guard erased and the
   ; claim stands alone, the bridge restates it over the pinned carrier, which
   ; is mechanical: the same law, as an equality rather than a decision.
   (veqclaim? ty)
    (: c (at ty 3)
       ((tally c) = 4 && id? (at c 0) 'paths && id? (at c 1) 'bool
        && id? (at c 3) 'true
        && (: q (at c 2) (two? q && (tally q) = 3 && id? (at q 0) 'veq))))
   (twinof d)
    (? (! (veqclaim? (at d 2))) 0
       (: q (at (at (at d 2) 3) 2) ta (at q 1) tb (at q 2)
          g (intern ("law-" + (qstrip (show (at d 0)))))
          _ (pl " ; ..and the same law over the PINNED carrier, by the bridge")
          _ (pl (" (defq law-" + (qstrip (show (at d 0))) + "-p"))
          _ (pl ("   " + (show ['pi 'v 'val ['paths 'pval ['pin ta] ['pin tb]]])))
          (pl ("   " + (show ['lam 'v ['uv-veq-paths ta tb [g 'v]]]) + ")"))))
   _ (map (\ d (: _ (pl (join [" ; " (show (at d 0)) "   " (srcof d)]))
                  _ (pl (join [" ; " (howof d)
                               (? (id? (at d 5) ()) "" ("; the guard " + (at d 5) + " erases"))]))
                  _ (pl (" (defq law-" + (qstrip (show (at d 0)))))
                  _ (pl ("   " + (show (at d 2))))
                  _ (pl (proofof d))
                  (twinof d)))
          proved)
   _ (pl ")")
   _ (pl "")
   _ (pl "; --- rows whose CODOMAIN is the proof: the tower assigns the type, so the")
   _ (pl "; --- range check has nothing left to state ---")
   _ (map (\ d (pl (join [" ;   " (pad (show (at d 0)) 14) "lands in " (at d 2)]))) ranged)
   _ (pl "")
   _ (pl "; --- off the model, with the reason (skipped and counted, never dropped) ---")
   _ (map (\ d (pl (join [" ;   " (pad (show (at d 0)) 14) (at d 2)]))) missed)
   _ (pl "")
   _ (pl (join [" ; " (show (tally proved)) " obligations / " (show (tally rows))
                " law rows; " (show (tally ranged)) " carried by a type, "
                (show (tally missed)) " off the model."]))
   (quit 0))
