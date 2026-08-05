; read: charms -> top -- the reverse shell of the compiler, the lane every
; shell feeds into; ev is the data-level shell over the same core. a charm
; is a sat (a non-negative fixnum), a charms is a charlist. read reads one
; form structurally and compiles it through opfix/feel/ana/cata, so the
; whole hull -- characters to top -- closes in lisp.
(: (zs s) ((: (g i) (? (< i (tally s)) (link (s i) (g (+ 1 i))))) 0)   ; string -> chars
 (assert
  (3 = (read (zs "(+ 1 2)")))                  ; the plain lane
  (3 = (read (zs "(1 + 2)")))                  ; infix factors through the charm lane
  (7 = (read (zs "(1 + 2 * 3)")))              ; right-assoc, same as ev
  (197 = (read (zs "($\"ab\" + 2)")))          ; one binds tightest, same as ev
  (1 = (read (zs "(!0)")))                     ; sigils factor mid-form; a BARE "!0" is
                                              ; two datums -- wrapping a whole line is a
                                              ; shell's move (the repl's), not read's
  ('x = (read (zs "'x")))                      ; quote is the reader's
  ('! = (caup (read (zs "'(a ! b)"))))         ; quoted operators stay data
  (lit? (read (zs "(\\ x (x + 1))")))          ; read is the source of tops
  (5 = ((read (zs "(\\ x (x + 2))")) 3))       ; ... which apply
  (9 = (read (zs "(: q (\\ x (x * x)) (q 3))")))
  !(read (zs ""))                              ; nothing readable is nothing
  !(read (zs "(+ 1"))                          ; an unfinished shape is nothing
  (1 = (read (zs "1")))
  ; reads: the stream shell -- a port is the hot charms; every form evals,
  ; the last value is the value. the host/love0/kernel runners ARE reads.
  (42 = (reads (tap (zs "(: zq 41) (zq + 1)"))))  ; defines land: the second form reads the first's
  !(reads (tap (zs "")))
  (7 = (reads (tap (zs "1 2 (3 + 4)"))))))     ; forms eval in order; the last one answers

; the colist lift (`char`) + realizer are now CLOSURE-PRIVATE inside read; what stays
; observable is that read drinks any input: chars (above), a port, or a hand-built charms.
(: (zs2 s) ((: (g i) (? (< i (tally s)) (link (s i) (g (+ 1 i))))) 0)
 (assert
  (6 = (read (tap (zs2 "(* 2 3)"))))             ; a port: read lifts the hot charms
  (1 = (read (link 49 (\ _ ()))))))              ; a hand-built one-cell charms: "1"
