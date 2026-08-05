; read: text -> top -- the reverse shell of the compiler, the lane every
; shell feeds into; ev is the data-level shell over the same core. read reads
; one form structurally and compiles it through opfix/feel/ana/cata, so the
; whole hull -- characters to top -- closes in lisp.
(:
 (assert
  (3 = (read "(+ 1 2)"))                  ; the plain lane
  (3 = (read "(1 + 2)"))                  ; infix factors through the charm lane
  (7 = (read "(1 + 2 * 3)"))              ; right-assoc, same as ev
  (197 = (read "($\"ab\" + 2)"))          ; one binds tightest, same as ev
  (1 = (read "(!0)"))                     ; sigils factor mid-form; a BARE "!0" is
                                              ; two datums -- wrapping a whole line is a
                                              ; shell's move (the repl's), not read's
  ('x = (read "'x"))                      ; quote is the reader's
  ('! = (caup (read "'(a ! b)")))         ; quoted operators stay data
  (lit? (read "(\\ x (x + 1))"))          ; read is the source of tops
  (5 = ((read "(\\ x (x + 2))") 3))       ; ... which apply
  (9 = (read "(: q (\\ x (x * x)) (q 3))"))
  !(read "")                              ; nothing readable is nothing
  !(read "(+ 1")                          ; an unfinished shape is nothing
  (1 = (read "1"))
  ; reads: the stream shell -- over a port or, as here, TEXT; every form evals,
  ; the last value is the value. the host/love0/kernel runners ARE reads.
  (42 = (reads "(: zq 41) (zq + 1)"))     ; defines land: the second form reads the first's
  !(reads "")
  (7 = (reads "1 2 (3 + 4)"))))           ; forms eval in order; the last one answers

; what stays observable is that read drinks either input: text (above) or a PORT,
; which it spouts into a chunk source itself.
(: (zs2 s) ((: (g i) (? (< i (tally s)) (link (s i) (g (+ 1 i))))) 0)
 (assert
  (6 = (read (tap (zs2 "(* 2 3)"))))             ; a port: read spouts it
  (1 = (read (tap (link 49 ()))))))              ; ... a hand-built one-cell tap: "1"
