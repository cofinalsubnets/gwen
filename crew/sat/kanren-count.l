; kanren-count -- SAT model-counting that falls out of love's GENERIC RING, as an expressiveness
; demo (NOT the solver -- see sat.l for the real CDCL engine; NOT a bench -- it's love-only and
; exponential). A CNF formula is literally a PRODUCT of SUMS of unification literals: `*` is the
; semiring product (conjunction), `+` the sum (disjunction), over kanren's `===`/`var`/`load`
; (a registered module: love/kanren.l). The solution STREAM is counted tail-recursively (scount), never
; materialized, so RAM stays flat; the count carries SEARCH MULTIPLICITY (a model re-derived once
; per clause that justifies it -- the stream is a semiring, not a set). The point is that #SAT
; needs no bespoke code here: the same `+`/`*` that add numbers and append lists express it.
; over n boolean vars x0..x(n-1), clauses (x_i | x_(i+1)) cyclically -- "no two adjacent 0s".
; Run: out/host/love crew/sat/kanren-count.l   ->   6726 (n=10).
(: (boolean x) (x === 0 + x === 1)
   scount (from 'kanren 'scount)                   ; count the stream without materializing it
   st0 (from 'kanren 'st0)                         ; the empty state: substitution + constraints
   (sat n) (:
     vs (map var (map (\ i (intern (+ "x" (show i)))) (jot n)))
     bools (map boolean vs)
     clauses (map (\ i (+ ((vs i) === 1) ((vs (% (+ i 1) n)) === 1))) (jot n))
     formula (foldl (*) 1 (bools + clauses))
     (scount ((load formula) st0)))
 _ (assert (= 6726 (sat 10)))
 _ (puts "kanren-count: #SAT-with-multiplicity of the cyclic n=10 CNF = ") _ (putn (sat 10)) _ (putc 10))
