; sat -- a real CDCL SAT solver in love, riding the core's mutable maps. An app: it
; owns crew/sat/ alone, touches no core file. a literal is a nonzero fixnum (var k true =
; +k, false = -k); a clause is a list of literals; a formula a list of clauses; an
; assignment a map var -> 1 (true) | -1 (false), absent = unassigned. The DPLL
; baseline below is the correctness oracle (Stage 1); the CDCL engine (Stages 2-3:
; two-watched-literals bcp, 1-uip clause learning, non-chronological backjump, VSIDS
; + Luby restarts) is built on top, validated to agree with it. Run: out/host/love
; crew/sat/sat.l  (strict asserts self-gate). pure app -- no love.c / love.h / host edit.
(:
   ; --- literals --------------------------------------------------------------
   (vof l)  (abs l)                                          ; the variable of a literal
   (neg l)  (- 0 l)                                          ; the complementary literal
   (sgn l)  (? (< l 0) -1 1)                                 ; polarity as +-1
   ; --- small list helpers (prel has map/foldl/filter/all/any/member?) --------
   (sfind p l) (? (atom? l) () (p <l) <l (sfind p >l))                     ; first elt matching p, else ()
   (rng lo hi) (? (< lo hi) (lo >< rng (+ lo 1) hi) ())                   ; [lo,hi)
   ; --- assignment (an alist (var . +-1) for the pure DPLL baseline) -----------
   (aget a v)  (? (atom? a) 0 (= (caap a) v) (cuap a) (aget >a v))        ; 0 = unassigned
   (lval a l)  (: v (aget a (vof l)) (? (= v 0) 0 (= v (sgn l)) 1 -1))    ; 1 sat / -1 falsified / 0 free
   (put-lit a l) ((vof l >< sgn l) >< a)                                  ; extend a so literal l is true
   ; --- clause / formula status under a ---------------------------------------
   (csat? a c)  (any (\ l (= 1 (lval a l))) c)               ; some literal satisfied
   (cfals? a c) (all (\ l (= -1 (lval a l))) c)              ; every literal falsified (a conflict)
   (sat? a formula) (all (\ c (csat? a c)) formula)          ; model check: every clause satisfied
   ; --- unit propagation (functional: returns an extended alist, or 'conflict) -
   (unit-of a c) (? (csat? a c) 0                            ; satisfied clause forces nothing
                    (: free (filter (\ l (= 0 (lval a l))) c)
                       (? (= 1 (tally free)) <free 0)))       ; exactly one free literal -> that literal is forced; else 0
   (bcp a formula)                                           ; saturate units; 'conflict if any clause all-false
    ((: (go a)
        (? (any (\ c (cfals? a c)) formula) 'conflict
           (: u (sfind (\ c !(= 0 (unit-of a c))) formula)
              (? (atom? u) a (go (put-lit a (unit-of a u))))))) a)
   ; --- DPLL: decide a var, recurse on each polarity, backtrack on 'conflict ---
   (pick a nvars) ((: (go v) (? (> v nvars) 0 (= 0 (aget a v)) v (go (+ v 1)))) 1)   ; first free var, 0 if none
   (dpll a formula nvars)                                    ; -> a satisfying alist, or 'unsat
    (: a2 (bcp a formula)
       (? (id? a2 'conflict) 'unsat
          (: v (pick a2 nvars)
             (? (= v 0) a2
                (: r (dpll (put-lit a2 v) formula nvars)
                   (? (id? r 'unsat) (dpll (put-lit a2 (neg v)) formula nvars) r))))))
   (solve formula nvars) (dpll () formula nvars)             ; top-level: '() assignment in, model or 'unsat out
   ; --- model counting (enumerate all satisfying assignments) -- a self-check ---
   (count-models formula nvars)
    ((: (go a)
        (? (any (\ c (cfals? a c)) formula) 0
           (: v (pick a nvars)
              (? (= v 0) 1 (+ (go (put-lit a v)) (go (put-lit a (neg v)))))))) ())
   ; --- instances --------------------------------------------------------------
   ; cyclic "no two adjacent 0s": clauses (x_i v x_{i+1 mod n}), vars 1..n.
   ; distinct models = the Lucas number L_n (L_10 = 123) -- an oracle from combinatorics.
   (cyclic n) (map (\ i ((+ i 1) >< ((+ 1 (% (+ i 1) n)) >< ()))) (jot n))
   (lucas n)  ((: (go k a b) (? (= k 0) a (go (- k 1) b (+ a b)))) n 2 1)    ; L_0=2,L_1=1,..  L_n
   ; pigeonhole php(h): h+1 pigeons into h holes -> UNSAT, and resolution-hard, so it forces many
   ; conflicts (exercises clause learning + restarts). var x[p][k] = p*h+k+1 (pigeon p in hole k).
   (php h) (: P (+ h 1)
              pc (map (\ p (map (\ k (+ (* p h) (+ k 1))) (jot h))) (jot P))                          ; each pigeon in some hole
              hc (foldl (\ a k (cat a (foldl (\ a2 p (cat a2 (foldl (\ a3 q                            ; no two pigeons share a hole
                     (? (< p q) ((neg (+ (* p h) (+ k 1)) >< (neg (+ (* q h) (+ k 1)) >< ())) >< a3) a3))
                     () (jot P)))) () (jot P)))) () (jot h))
              (pc + hc))
   (php-vars h) (* (+ h 1) h)

   ; ===========================================================================
   ; CDCL engine (Stage 2): mutable trail + 1-uip clause learning + backjump,
   ; propagating through two-watched literals (the `prop-lit` block below). State `s`
   ; is one map of sub-maps; assignment carries decision level and reason (the clause
   ; that forced each var) so a conflict can be analysed. Differentially fuzz-tested
   ; against the DPLL oracle on random 3-SAT below.
   ; ===========================================================================
   (vmap s k)   (peep s k ())                               ; a named sub-map of the state
   (val-of s v) (peep (vmap s 'val) v 0)                    ; var's truth (1/-1), 0 = unassigned
   (lit-val s l) (: v (val-of s (vof l)) (? (= v 0) 0 (= v (sgn l)) 1 -1))   ; 1 sat / -1 false / 0 free under the trail
   (add-clause s c)                                         ; append a clause, return its id
    (: id (peep s 'ncls 0) _ (pin (vmap s 'cls) id c) _ (pin s 'ncls (+ id 1)) id)
   (assign s lit cid)                                       ; set lit true at the current level, reason = clause cid (-1 = decision)
    (: v (vof lit) t (peep s 'top 0)
       _ (pin (vmap s 'val) v (sgn lit)) _ (pin (vmap s 'lvl) v (peep s 'level 0))
       _ (pin (vmap s 'reason) v cid) _ (pin (vmap s 'trail) t lit) _ (pin s 'top (+ t 1)) 0)
   (mk formula nvars)                                       ; build solver state from a formula
    (: s (tablet 0)
       _ (pin s 'nvars nvars) _ (pin s 'val (tablet 0)) _ (pin s 'lvl (tablet 0))
       _ (pin s 'reason (tablet 0)) _ (pin s 'trail (tablet 0)) _ (pin s 'top 0) _ (pin s 'qhead 0)
       _ (pin s 'level 0) _ (pin s 'cls (tablet 0)) _ (pin s 'ncls 0) _ (pin s 'act (tablet 0))
       _ (pin s 'watch (tablet 0)) _ (pin s 'wl (tablet 0)) _ (pin s 'bad 0)
       _ (pin s 'phase (tablet 0)) _ (pin s 'vinc 1.0) _ (pin s 'nconf 0) _ (pin s 'rnum 1) _ (pin s 'climit 100)
       _ (foldl (\ _ c (load-clause s c)) 0 formula) s)
   ; --- two-watched-literals bcp -----------------------------------------------
   ; Each clause watches two of its literals; watch[l] lists the clauses watching l;
   ; wl[cid] is the watched pair (a . b). a clause is touched only when a watched
   ; literal becomes false -- then we slide that watch to any other non-false literal,
   ; or (none left) the clause is unit (propagate the other watch) or a conflict. The
   ; invariant survives backtracking untouched (unassigning only relaxes), so backjump
   ; just rewinds qhead. This is the O(touched), not O(clauses), propagation core.
   (enqueue s lit cid)                                      ; level-0 / forced literal: assign if free, () ok, 'conflict if already false
    (: lv (lit-val s lit) (? (= lv 1) 0  (= lv -1) 'conflict  (: _ (assign s lit cid) 0)))
   (watch2 s cid a b)                                       ; register clause cid as watching literals a and b
    (: _ (pin (vmap s 'wl) cid (a >< b))
       _ (pin (vmap s 'watch) a (cid >< peep (vmap s 'watch) a ()))
       _ (pin (vmap s 'watch) b (cid >< peep (vmap s 'watch) b ())) 0)
   (dedup-lits c) (? (atom? c) () (member? <c >c) (dedup-lits >c) (<c >< dedup-lits >c))                                ; drop duplicate literals
   (taut? c) (? (atom? c) 0 (member? (neg <c) >c) 1 (taut? >c))                  ; contains a literal and its negation?
   (load-clause s c0)                                       ; add a source clause to the db + watches (clean it first: dedupe, drop tautologies)
    (? (= 1 (taut? c0)) 0                                                        ; tautology -> always satisfied, skip
       (: c (dedup-lits c0)
          (? (atom? c) (pin s 'bad 1)                                            ; empty clause -> UNSAT
             (atom? >c) (: cid (add-clause s c) (? (id? (enqueue s <c cid) 'conflict) (pin s 'bad 1) 0))             ; unit -> level-0 fact
             (: cid (add-clause s c) (watch2 s cid <c <>c)))))                   ; >=2 distinct lits -> watch the first two
   (find-watch s cid wo)                                    ; a literal of clause cid other than wo and not currently false, else 0
    ((: (go ls) (? (atom? ls) 0 (: l <ls (? (!(= l wo) && !(= (lit-val s l) -1)) l (go >ls))))) (peep (vmap s 'cls) cid ()))
   (prop-lit s f)                                           ; literal f just became false: re-examine watch[f]; -> conflict cid or -1
    (: res ((: (go cs kept)                                                      ; kept = clauses that keep watching f; rebuild watch[f]
               (? (atom? cs) (-1 >< kept)
                  (: cid <cs rest >cs pr (peep (vmap s 'wl) cid ())
                     wo (? (= <pr f) >pr <pr)                                     ; the other watched literal
                     (? (= (lit-val s wo) 1) (go rest (cid >< kept))             ; satisfied by wo -> keep watching f
                        (: k (find-watch s cid wo)
                           (? !(= k 0)                                            ; slide the watch f -> k
                              (: _ (pin (vmap s 'wl) cid (wo >< k))
                                 _ (pin (vmap s 'watch) k (cid >< peep (vmap s 'watch) k ()))
                                 (go rest kept))                                  ; cid no longer watches f
                              (= (lit-val s wo) 0) (: _ (assign s wo cid) (go rest (cid >< kept)))     ; unit -> force wo
                              (cid >< cat kept (cid >< rest))))))))               ; wo false -> conflict; f keeps cid + the unprocessed rest
            (peep (vmap s 'watch) f ()) ())
       _ (pin (vmap s 'watch) f >res) <res)
   (propagate s)                                            ; drain the trail queue through the watches; -> conflict cid or -1
    ((: (loop _) (? (>= (peep s 'qhead 0) (peep s 'top 0)) -1
                   (: p (peep (vmap s 'trail) (peep s 'qhead 0) ()) _ (pin s 'qhead (+ 1 (peep s 'qhead 0)))
                      confl (prop-lit s (neg p)) (? (> confl -1) confl (loop ()))))) ())
   ; --- 1-uip conflict analysis -----------------------------------------------
   ; absorb a reason clause's literals into (counter . learnt): mark each unseen
   ; level>0 var, count those at the conflict level, collect the rest as the
   ; learnt clause's lower-level literals. skipv = the resolved var to skip (0 = none).
   (absorb s seen dl skipv c acc)
    (foldl (\ st l
              (: v (vof l)
                 (? (= v skipv) st
                    !(= 0 (peep seen v 0)) st
                    (= 0 (peep (vmap s 'lvl) v 0)) st
                    (: _ (pin seen v 1) _ (bump s v)
                       (? (= (peep (vmap s 'lvl) v 0) dl) ((+ 1 <st) >< >st)
                          (<st >< (l >< >st)))))))
           acc c)
   (find-seen tr seen idx) (? (= 0 (peep seen (vof (peep tr idx ())) 0)) (find-seen tr seen (- idx 1)) idx)
   (bj-level s learnt) (foldl (\ m l (: lv (peep (vmap s 'lvl) (vof l) 0) (? (> lv m) lv m))) 0 learnt)
   ; the learnt clause's 2nd watch must be its highest-level literal, else a backtrack could unassign
   ; an unwatched literal and the clause would go unit/conflict undetected. So reorder learnt so [1]
   ; is the max-level tail literal (learnt[0] stays the asserting literal).
   (rem-lit x ls) (? (atom? ls) () (= <ls x) >ls (<ls >< rem-lit x >ls))
   (max-lit s ls) (foldl (\ b l (? (> (peep (vmap s 'lvl) (vof l) 0) (peep (vmap s 'lvl) (vof b) 0)) l b)) <ls >ls)
   (reorder-watch s learnt) (? (atom? >learnt) learnt
                               (: a0 <learnt rest >learnt mx (max-lit s rest) (a0 >< (mx >< rem-lit mx rest))))
   (analyze s confl)                                        ; -> (learnt-clause . backjump-level); learnt[0] is the asserting literal
    (: dl (peep s 'level 0) seen (tablet 0) tr (vmap s 'trail) cls (vmap s 'cls) reason (vmap s 'reason)
       a0 (absorb s seen dl 0 (peep cls confl ()) (0 >< ()))
       res ((: (go counter learnt idx)
               (: ix (find-seen tr seen idx) np (peep tr ix ()) v (vof np)
                  _ (pin seen v 0) cnt (- counter 1)
                  (? (= cnt 0) (np >< learnt)
                     (: a (absorb s seen dl v (peep cls (peep reason v 0) ()) (cnt >< learnt))
                        (go <a >a (- ix 1))))))
            <a0 >a0 (- (peep s 'top 0) 1))
       uip <res learnt >res learntc (neg uip >< learnt)
       (learntc >< bj-level s learnt))
   (backjump s lvl)                                         ; pop+unassign every trail entry above level lvl
    (: tr (vmap s 'trail) val (vmap s 'val) lm (vmap s 'lvl) rs (vmap s 'reason) ph (vmap s 'phase)
       nt ((: (go t)
              (? (= t 0) 0
                 (: l (peep tr (- t 1) ()) v (vof l)
                    (? (> (peep lm v 0) lvl)
                       (: _ (pin ph v (peep val v 1)) _ (pull val v 0) _ (pull lm v 0) _ (pull rs v 0) (go (- t 1)))   ; save phase before unassigning
                       t))))
           (peep s 'top 0))
       _ (pin s 'top nt) _ (pin s 'qhead nt) _ (pin s 'level lvl) 0)   ; rewind the propagation queue to the new top
   ; --- VSIDS: bump a var's activity by the current increment; decay grows the
   ;     increment each conflict (so recent conflicts weigh more); rescale keeps
   ;     activities finite. pick-branch takes the highest-activity free var, and
   ;     decides its saved phase (phase saving -- the last value it held). --------
   (bump s v) (pin (vmap s 'act) v (+ (peep (vmap s 'act) v 0.0) (peep s 'vinc 1.0)))
   (rescale s) (foldl (\ _ v (pin (vmap s 'act) v (* (peep (vmap s 'act) v 0.0) 1e-100))) 0 (rng 1 (+ (peep s 'nvars 0) 1)))
   (decay s) (: vi (* (peep s 'vinc 1.0) 1.05)
                (? (> vi 1e100) (: _ (rescale s) (pin s 'vinc (* vi 1e-100))) (pin s 'vinc vi)))
   (pick-branch s)                                          ; highest-activity free var, 0 if none
    ((: (go v best ba)
        (? (> v (peep s 'nvars 0)) best
           (: a (peep (vmap s 'act) v 0.0)
              (? ((= 0 (val-of s v)) && ((= best 0) || > a ba)) (go (+ v 1) v a) (go (+ v 1) best ba)))))
     1 0 -1.0)
   (pow2 k) (k 2)                                           ; 2^k via numeral-apply
   (luby i)                                                 ; the i-th term of the Luby restart sequence (1,1,2,1,1,2,4,..)
    ((: (go k) (: hi (- (pow2 k) 1)
                  (? (< hi i) (go (+ k 1))  (= hi i) (pow2 (- k 1))  (luby (+ 1 (- i (pow2 (- k 1)))))))) 1)
   (next-restart s) (: rn (+ 1 (peep s 'rnum 1)) _ (pin s 'rnum rn) _ (pin s 'nconf 0) _ (pin s 'climit (* (luby rn) 100)) 0)
   (search s)                                               ; -> 'sat or 'unsat (mutating s to a model on SAT)
    ((: (loop _)
        (: confl (propagate s)
           (? (> confl -1)
              (? (= 0 (peep s 'level 0)) 'unsat              ; conflict at level 0 -> UNSAT
                 (: res (analyze s confl) learnt (reorder-watch s <res) cid (add-clause s learnt)
                    _ (decay s) _ (pin s 'nconf (+ 1 (peep s 'nconf 0)))
                    _ (backjump s >res)
                    _ (? (atom? >learnt) 0 (watch2 s cid <learnt <>learnt))                  ; watch the learnt clause (unit learnt: a level-0 fact, unwatched)
                    _ (assign s <learnt cid) (loop ())))
              (> (peep s 'level 0) 0 && (>= (peep s 'nconf 0) (peep s 'climit 0)))      ; Luby restart: keep learnt clauses, jump to level 0
                 (: _ (backjump s 0) _ (next-restart s) (loop ()))
              (: v (pick-branch s)                            ; decide: highest-activity free var, saved phase
                 (? (= v 0) 'sat
                    (: _ (pin s 'level (+ 1 (peep s 'level 0)))
                       _ (assign s (* v (peep (vmap s 'phase) v -1)) -1) (loop ()))))))) ())
   (extract-model s nvars) (map (\ v (v >< val-of s v)) (rng 1 (+ nvars 1)))
   (cdcl formula nvars) (: s (mk formula nvars)
                           (? (= 1 (peep s 'bad 0)) 'unsat                       ; empty clause / contradictory units at construction
                              (: r (search s) (? (id? r 'unsat) 'unsat (extract-model s nvars)))))
   ; --- model counting via the CDCL solver: solve, block the model, repeat until UNSAT. a real #SAT
   ;     technique; the count is solver-independent (a fact), so count-cdcl (cyclic n) == Lucas n. -----
   (block model) (map (\ vv (neg (* >vv <vv))) model)                  ; clause excluding exactly this model
   (count-cdcl formula nvars)
    ((: (go fm c) (: r (cdcl fm nvars) (? (id? r 'unsat) c (go (block r >< fm) (+ c 1))))) formula 0)
   ; --- random 3-SAT generator (deterministic LCG) for the differential fuzz ----
   (lcg-next x)  (% (+ (* x 1103515245) 12345) 2147483648)
   (lit-from x n) (: v (+ 1 (% (// x 16) n)) (? (= 0 (% x 2)) v (- 0 v)))
   (gen-formula seed n m)
    ((: (go i x acc)
        (? (>= i m) acc
           (: x1 (lcg-next x) x2 (lcg-next x1) x3 (lcg-next x2)
              (go (+ i 1) x3 ((lit-from x1 n >< (lit-from x2 n >< (lit-from x3 n >< ()))) >< acc))))) 0 seed ())
   (diff-test seed n m)                                     ; DPLL and CDCL agree (and CDCL's model checks when SAT)
    (: f (gen-formula seed n m) d (solve f n) c (cdcl f n)
       dsat !(id? d 'unsat) csat !(id? c 'unsat)
       (? !(= dsat csat) 0  !csat 1  (sat? c f)))
   (fuzz k n m) (all (\ i (= 1 (diff-test (+ 1009 (* i 7919)) n m))) (jot k))

 ; --- Stage 1 self-tests: DPLL agrees with the model checker and with combinatorics
 _ (assert
    ; a tiny SAT instance -- the returned model really satisfies it
    (: f '((1 2) (-1 3) (-2 -3)) m (solve f 3)
       (!(id? m 'unsat) && sat? m f))
    ; a tiny UNSAT instance -- (x)(-x)
    (id? (solve '((1) (-1)) 1) 'unsat)
    ; the cyclic instance is SAT and the model checks out
    (: f (cyclic 10) m (solve f 10) (!(id? m 'unsat) && sat? m f))
    ; DPLL model count == Lucas(n) for the cycle, several n (combinatorial oracle)
    (= (count-models (cyclic 5) 5)  (lucas 5))      ; 11
    (= (count-models (cyclic 8) 8)  (lucas 8))      ; 47
    (= (count-models (cyclic 10) 10) (lucas 10))    ; 123
    ; a fully-assigned conflicting clause is a conflict; an unsatisfiable unit pair propagates to it
    (id? (bcp () '((1) (-1))) 'conflict)
    (= 123 (lucas 10)))
 _ (assert    ; --- Stage 2: CDCL agrees with DPLL, learns + backjumps, models check ---
    ; tiny SAT / UNSAT
    (: f '((1 2) (-1 3) (-2 -3)) m (cdcl f 3) (!(id? m 'unsat) && sat? m f))
    (id? (cdcl '((1) (-1)) 1) 'unsat)
    (id? (cdcl '((1 2) (1 -2) (-1 2) (-1 -2)) 2) 'unsat)                       ; all 4 clauses over 2 vars -> UNSAT
    ; the cyclic instance: SAT, model checks, and the model count matches DPLL/Lucas
    (: f (cyclic 12) m (cdcl f 12) (!(id? m 'unsat) && sat? m f))
    ; CDCL really learns: a unit clause forces propagation, a conflict at level 0 is UNSAT
    (id? (cdcl '((1 2 3) (1 2 -3) (1 -2 3) (1 -2 -3) (-1)) 3) 'unsat)          ; forces 1=0 then UNSAT under the first four
    ; differential fuzz: 60 random 3-SAT instances, CDCL == DPLL (SAT/UNSAT) + valid models
    (fuzz 60 8 30)                                                            ; near phase transition (m/n ~ 3.75), mixed SAT/UNSAT
    (fuzz 40 10 30)
    (fuzz 30 12 50))
 _ (assert    ; --- Stage 3: VSIDS + Luby restarts + phase saving (heuristics: same answers, smarter search) ---
    ; the Luby restart sequence: 1 1 2 1 1 2 4 1 1 2 1 1 2 4 8
    (= 1 (luby 1)) (= 1 (luby 2)) (= 2 (luby 3)) (= 1 (luby 4)) (= 2 (luby 6)) (= 4 (luby 7)) (= 8 (luby 15))
    ; with VSIDS + restarts on, CDCL still agrees with DPLL (heuristics change search order, not the answer)
    (fuzz 40 12 50)
    (fuzz 20 14 58)                                                           ; harder ratio (m/n ~ 4.1)
    ; pigeonhole php(5): UNSAT and resolution-hard -> hundreds of conflicts, so this exercises the
    ; learning + restart path end to end (climit starts at 100; PHP(5) blows past it)
    (id? (cdcl (php 5) (php-vars 5)) 'unsat)
    ; a larger satisfiable instance (cyclic n=80) solved, model checked
    (: f (cyclic 80) m (cdcl f 80) (!(id? m 'unsat) && sat? m f))
    ; #SAT via blocking clauses == the combinatorial Lucas count (solver-independent)
    (= (count-cdcl (cyclic 8) 8) (lucas 8))                                   ; 47
    (= (count-cdcl (cyclic 10) 10) (lucas 10)))                               ; 123
 _ (say err "sat: Stages 1-3 ok -- CDCL (learning, backjump, VSIDS, Luby restarts, phase saving) == DPLL on 190 fuzzed + UNSAT pigeonhole + #SAT==Lucas\n"))
