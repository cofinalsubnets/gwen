; dimacs -- a dimacs cnf reader for the sat solver, built from the parser combinators in love/peg.l
; (a parser is (\ s y n): a string s, a success continuation y = (rest payload), a failure
; continuation n = rest; payloads are lists that `seq` concatenates). peg shares that set
; (eps/lit/span/alt/seq/seqs/skip/many/runp) with cook; here we add only a signed-int leaf `pint` (love
; has no regex) and the grammar. `dimacs text` -> (formula . nvars): skip `c` comment lines, read the
; `p cnf N M` header for N, parse the body integers, split them into clauses at each terminating 0.
; The self-tests below read `cdcl` for the parse->solve leg, so this file is run behind the solver:
; cat crew/sat/sat.l crew/sat/dimacs.l | out/host/love.
; the combinators (love/peg.l) -- its own top-level form; this reader is all lenient `runp`.
(use 'peg)

(:
   ; -- the parser combinators (eps/lit/span/alt/seq/seqs/skip/many + the lenient runner runp) come from
   ;    love/peg.l via the (use 'peg) above -- shared with cook, not re-derived here.
   ; -- character classes + the signed-integer leaf --
   (ws? c)   (c @ (32 || 9 || 13 || 10) 1 0)     ; space/tab/cr/lf -- a ?-name owes a bit
   (dig? c)  (47 < c < 58)                 ; '0'..'9'
   (notnl? c) (c != 10)
   (str->nat ds) ((: (g i a) (i >= #ds ? a (g (i + 1) (a * 10 + ds i - 48)))) 0 0)
   (pint s y n)                                            ; an optional '-' then >=1 digits -> (the int); fail otherwise
    (: t #s neg (0 < t && s 0 = 45) j0 ?neg
       j (: (g i) (i < t && dig? (s i) ? (g (i + 1)) i) (g j0))
     (j = j0 ? (n s) (: v (str->nat (snip s j0 j)) (y (snip s j t) [(neg ? -v v)]))))
   ; -- the dimacs grammar --
   ws        (skip (span ws?))                              ; a (possibly empty) whitespace run
   cline     (seq (lit "c") (span notnl?))                  ; a 'c' comment: 'c' + rest of line (the newline is ws)
   pre       (skip (many (seq ws cline)))                   ; leading comment lines
   header    (seqs [ws (lit "p") ws (lit "cnf") ws pint ws pint])     ; -> ("p" "cnf" nvars nclauses)
   body      (many (seq ws pint))                           ; -> all body integers (lits + 0 terminators)
   cnf       (seqs [pre header body])                       ; -> ("p" "cnf" nvars nclauses . ints)
   (split0 ints)                                            ; split an int stream into clauses, dropping the 0 terminators
    ((: (g ls cur acc)
        (? (atom? ls) (atom? cur ? (rev acc) (rev (rev cur >< acc)))         ; flush a final un-terminated clause
           (<ls = 0) (g >ls () (rev cur >< acc))
           (g >ls (<ls >< cur) acc))) ints () ())
   (dimacs text) (: p (runp cnf text) (split0 (drop 4 p) >< p 2))         ; (formula . nvars)

 ; -- parse self-tests (standalone; no solver needed) --
 _ (assert
    ; comments, a 3-var header, three clauses (one 1-lit), all 0-terminated
    (: r (dimacs "c hi
c there
p cnf 3 3
1 -2 0
2 3 -1 0
-3 0
")  ((= 3 >r) && (= '((1 -2) (2 3 -1) (-3)) <r)))
    ; no leading comments, a clause split across whitespace, a missing final 0 (lenient flush)
    (: r (dimacs "p cnf 4 2
1 2 -3 0
4 -1")  ((= 4 >r) && (= '((1 2 -3) (4 -1)) <r)))
    ; tabs + extra blank lines + a big var number parse
    (: r (dimacs "p cnf 100 1
	42	-99	0
")  ((= 100 >r) && (= '((42 -99)) <r))))
 ; -- integration self-test. `cdcl` is crew/sat/sat.l's, and this file is catted after it --
 ; a name that is not there is missing at compile, so no predicate can stand in front of one.
 _ (assert
     (: r (dimacs "p cnf 3 3
1 2 0
-1 3 0
-2 -3 0
") f <r nv >r m (cdcl f nv) (!(id? m 'unsat) && sat? m f))   ; SAT -- model checks
     (id? (cdcl <(dimacs "p cnf 1 2
1 0
-1 0
") 1) 'unsat))                                                             ; (x)(-x) -> UNSAT
 _ (say err "crew/sat/dimacs: ok -- DIMACS reader (parser combinators) + parse->solve\n"))
