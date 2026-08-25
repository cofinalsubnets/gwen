; lcat: serialize a l source file to a C string literal for #include (the lcat
; lib headers the final l is assembled from). Each top-level form is re-printed
; via show (canonical), the forms are joined into one string, and that string is
; MINIFIED -- a space is dropped wherever removing it does not change how the reader
; PARSES. The boundary test is empirical, not a hardcoded delimiter set: the left
; token (bytes since the last HARD boundary) and the whole next FORM are re-parsed
; with and without the space, and the space is redundant iff both spellings yield
; the SAME datums (`o(` reads the same as `o (`, but `-5` and `- 5` do not, and
; `two? s` must keep its space -- `two?s` is ONE name token). Two things make
; this reader-change-proof, where a count over one byte was not: the probe reaches
; the whole next form (so a quote sees its operand -- `ev'ev` vs `ev 'ev`), and it
; compares the datums themselves, not a count (a re-tokenization can keep the count
; yet change the parse). Spaces inside a string literal are data and never touched.
; Surviving bytes are C-escaped.

(: (s2cl s) ((: (g i) (? (< i (tally s)) (link (peep s i 0) (g (+ 1 i))))) 0)  ; string -> charlist
   ; the datums readable from s (stop on the eof sentinel or an incomplete form,
   ; which read signals by returning the port itself) -- the full forms, compared
   ; STRUCTURALLY, not just counted: a re-tokenization can preserve the count yet
   ; change the parse (name'(a b) and name '(a b) both read two datums).
   (datums s) ((: (g x a) (: r (sound x) (? (two? r) (g (cup r) (link (cap r) a)) (rev a))))
               s 0)
   ; the next complete form of a charlist, re-shown; 0 if none reads cleanly.
   (form1 cs) (: r (sound cs) (? (two? r) (show (cap r)) 0))
   ; a HARD token boundary: whitespace, delimiters, string quote, comment, and
   ; the value-surface chars that REACH ACROSS a space (' ` each skip whitespace
   ; to find their datum, so nothing a token could span survives them).
   ; ⚠ @ # ~ are NOT here, though they are sigils: those three READ GLUE, so a
   ; token CAN end at one (fs@) and gluing CAN change the parse (@ x is the symbol
   ; @ then x, @x is (tuple x); '# is the sigil's own name, '#x the run around x).
   ; resetting the left context there would hide the very byte the probe has to
   ; weigh -- and opb? then keeps their space outright.
   (hardb c) (? (= c 32) 1 (= c 10) 1 (= c 9) 1
                (= c 40) 1 (= c 41) 1 (= c 91) 1 (= c 93) 1 (= c 123) 1 (= c 125) 1
                (= c 34) 1 (= c 59) 1 (= c 39) 1 (= c 96) 1 (= c 44) 1
                0)
   ; an operator-run byte: punct that is neither a hard boundary nor a name
   ; char -- \ included, though it never fuses. under the VALENCE LAW a run
   ; glued to a datum is monadic, and the probe below tests in isolation --
   ; head position, where fusion is suppressed -- so a space between a run's
   ; end and a datum start is kept conservatively: it is semantic mid-list.
   (opb? c) (? (hardb c) 0
               (&& (<= 48 c) (<= c 57)) 0
               (&& (<= 65 c) (<= c 90)) 0
               (&& (<= 97 c) (<= c 122)) 0
               (= c 95) 0
               1)
   (dstart? b) (? (= b 41) 0 (= b 93) 0 (= b 125) 0 (= b 59) 0
                  (= b 32) 0 (= b 10) 0 (= b 9) 0
                  1)
   ; ctx = the bytes since the last hard boundary, reversed (the left token);
   ; cs = the remaining charlist after the space. the space is redundant iff
   ; lt+rt re-parses to the SAME datums as lt rt, where rt is the WHOLE next
   ; form -- a quote reaches its operand, so any lexer rule is honoured, not
   ; just today's. the opb? clause still keeps an operator-then-datum space:
   ; the probe reads in isolation (head position, fusion off) but the run is
   ; monadic mid-list.
   (redundant? ctx cs) (? (&& (two? ctx) (opb? (cap ctx)) (dstart? (cap cs))) 0
                          (: lt (string (rev ctx)) rt (form1 cs)
                             (? rt (= (datums (+ lt rt)) (datums (+ (+ lt " ") rt))) 0)))
   ; emit one byte to o, C-escaped for the string literal.
   (eb o c) (? (= c 10) (say o "\\n")
               (| (= c 92) (= c 34)) (: _ (put o 92) (put o c))
               (put o c))
   ; a path's module stem: past the last /, minus a trailing .l
   (stem s) (: n (tally s)
               b ((: (go k r) (? (< k n) (go (+ k 1) (? (= 47 (peep s k 0)) (+ k 1) r)) r)) 0 0)
               e (? (? (< 2 (- n b)) (= ".l" (snip s (- n 2) n)) 0) (- n 2) n)
               (snip s b e))
   ; serialize src into dst as one C string literal.
   (lcat1 src dst)
    (: p (open src "r")
       _ (? p 0 (: _ (say err (+ "lcat: cannot open " src)) (quit 1)))
       ; gather every canonical form into one space-joined string. ⚠ a TORN input dies
       ; loudly: a header silently short of its last forms is the empty-lcat disaster
       ; wearing a subtler face -- the build would take it as fresh and fail somewhere else.
       acc ((: (g acc cl first)
              (: r (sound cl)
                 (? (two? r) (g (? first (show (cap r)) (+ (+ acc " ") (show (cap r)))) (cup r) 0)
                    (id? r 'torn) (: _ (say err (+ "lcat: unfinished form in " src)) (quit 1))
                    acc)))
            "" (slurp p) 1)
       _ (close p)
       o (open dst "w")
       _ (? o 0 (: _ (say err (+ "lcat: cannot write " dst)) (quit 1)))
       _ (put o 34)                                    ; opening "
       ; walk acc: drop redundant spaces (outside string literals), C-escape the rest.
       ; ctx = bytes since the last hard boundary (reversed); instr = inside a "..."
       ; literal (where \ escapes).
       _ ((: (g cs ctx instr)
            (? (two? cs)
               (: c (cap cs)
                  (? instr
                     (? (= c 92) (: _ (eb o c) _ (eb o (cap (cup cs))) (g (cup (cup cs)) 0 1))
                        (: _ (eb o c) (g (cup cs) 0 (? (= c 34) 0 1))))
                     (? (= c 32)
                        (? (redundant? ctx (cup cs))
                           (g (cup cs) ctx 0)             ; drop the space; ctx keeps accruing
                           (: _ (eb o c) (g (cup cs) 0 0)))   ; keep it: a real boundary
                        (: _ (eb o c)
                           (g (cup cs) (? (hardb c) 0 (link c ctx)) (= c 34))))))))
            (s2cl acc) 0 0)
       _ (put o 34)                                    ; closing "
       _ (put o 10)
       (close o)))

; usage: lcat.l OUTDIR SRC.l ..  -- one header per source, <OUTDIR>/<stem>.h.
; ⚠ MANY SOURCES, ONE RUN, and that is the whole point: ~98% of an lcat invocation is
; love0 booting (2.45G of the 2.52G a 2.2K file costs), so the per-file lane spent its
; time starting up. the caller lays into a scratch dir and moves what CHANGED, which is
; why nothing here compares or renames.
(: dir (cap (cup argv))
   srcs (cup (cup argv))
   _ (? (two? srcs) 0 (: _ (say err "lcat: usage: lcat.l OUTDIR SRC.l ..") (quit 1)))
   (each srcs (\ s (lcat1 s (+ (+ (+ dir "/") (stem s)) ".h")))))