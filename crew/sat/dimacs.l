; dimacs -- a DIMACS CNF reader for the sat solver, built from the PARSER COMBINATORS in love/peg.l
; (a parser is (\ s y n): a string s, a success continuation y = (rest payload), a failure
; continuation n = rest; payloads are lists that `seq` concatenates). peg shares that set
; (eps/lit/span/alt/seq/seqs/skip/many/runp) with cook; here we add only a signed-int LEAF `pint` (love
; has no regex) and the grammar. `dimacs text` -> (formula . nvars): skip `c` comment lines, read the
; `p cnf N M` header for N, parse the body integers, split them into clauses at each terminating 0.
; Standalone (parse self-tests below); when loaded AFTER crew/sat/sat.l it also self-tests parse->solve via
; `cdcl`. Run: out/host/love crew/sat/dimacs.l   (or: cat crew/sat/sat.l crew/sat/dimacs.l | out/host/love).
; the combinators (love/peg.l) -- its own top-level form; this reader is all lenient `runp`.
(use 'peg)

(:
   ; -- the parser combinators (eps/lit/span/alt/seq/seqs/skip/many + the lenient runner runp) come from
   ;    love/peg.l via the (use 'peg) above -- shared with cook, not re-derived here.
   ; -- character classes + the signed-integer leaf --
   (ws? c)   ((= c 32) || (= c 9) || (= c 13) || (= c 10))          ; space/tab/CR/LF
   (dig? c)  ((< 47 c) && (< c 58))                                 ; '0'..'9'
   (notnl? c) !(= c 10)
   (str->nat ds) ((: (g i a) (? (< i (tally ds)) (g (+ i 1) (+ (* a 10) (- (ds i) 48))) a)) 0 0)
   pint (\ s y n                                            ; an optional '-' then >=1 digits -> (the int); fail otherwise
          (: t (tally s) neg ((< 0 t) && (= (s 0) 45)) j0 (? neg 1 0)
             j ((: (g i) (? ((< i t) && dig? (s i)) (g (+ i 1)) i)) j0)
             (? (= j j0) (n s) (: v (str->nat (snip s j0 j)) (y (snip s j t) ((? neg (- 0 v) v) >< ()))))))
   ; -- the DIMACS grammar --
   ws        (skip (span ws?))                              ; a (possibly empty) whitespace run
   cline     (seq (lit "c") (span notnl?))                  ; a 'c' comment: 'c' + rest of line (the newline is ws)
   pre       (skip (many (seq ws cline)))                   ; leading comment lines
   header    (seqs [ws (lit "p") ws (lit "cnf") ws pint ws pint])     ; -> ("p" "cnf" nvars nclauses)
   body      (many (seq ws pint))                           ; -> all body integers (lits + 0 terminators)
   cnf       (seqs [pre header body])                       ; -> ("p" "cnf" nvars nclauses . ints)
   (nthl l k) (? (= k 0) <l (nthl >l (- k 1)))
   (dropl l k) (? (= k 0) l (dropl >l (- k 1)))
   (split0 ints)                                            ; split an int stream into clauses, dropping the 0 terminators
    ((: (g ls cur acc)
        (? (atom? ls) (? (atom? cur) (rev acc) (rev (rev cur >< acc)))         ; flush a final un-terminated clause
           (= <ls 0) (g >ls () (rev cur >< acc))
           (g >ls (<ls >< cur) acc))) ints () ())
   (dimacs text) (: p (runp cnf text) (split0 (dropl p 4) >< nthl p 2))         ; (formula . nvars)

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
 ; -- integration self-test (only when crew/sat/sat.l is loaded first, so `cdcl` is bound) --
 _ (? (lit? cdcl)
      (assert
       (: r (dimacs "p cnf 3 3
1 2 0
-1 3 0
-2 -3 0
") f <r nv >r m (cdcl f nv) (!(id? m 'unsat) && sat? m f))   ; SAT -- model checks
       (id? (cdcl <(dimacs "p cnf 1 2
1 0
-1 0
") 1) 'unsat))                                                             ; (x)(-x) -> UNSAT
      1)
 _ (say err (+ "crew/sat/dimacs: ok -- DIMACS reader (parser combinators)" (+ (? (lit? cdcl) " + parse->solve" "") "\n"))))
