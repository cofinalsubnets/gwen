; cdcl bench -- the real CDCL solver (crew/sat/sat.l, prepended by run.sh for b=cdcl) on a battery:
; four UNSAT pigeonhole instances PHP(2..5) -- resolution-hard, so conflict learning + restarts get a
; workout -- plus #SAT model-counting of the cyclic n=8 CNF (= 47 = Lucas(8), via blocking clauses).
; checksum = 100*14 + 47 = 1447, a SOLVER-INDEPENDENT fact (the UNSAT verdicts sum 2+3+4+5=14; the
; Lucas count is combinatorial), so it stays stable across heuristic changes. love-only -- no
; cross-language port. The kanren model-counter that used to live here moved to crew/sat/kanren-count.l.
(: (sat-work _)
    (: u (foldl (\ a h (+ a (? (id? (cdcl (php h) (php-vars h)) 'unsat) h 0))) 0 (L 2 3 4 5))   ; 2+3+4+5 = 14 (all UNSAT)
       c (count-cdcl (cyclic 8) 8)                                                                 ; 47 = Lucas(8)
       (+ (* 100 u) c))
   (bench "cdcl" sat-work))
