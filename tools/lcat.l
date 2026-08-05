; lcat: serialize a l source file to a C string literal for #include (the lcat
; lib headers the final l is assembled from). Each top-level form is re-printed
; via show (canonical), the forms are joined into one string, and that string is
; MINIFIED -- a space is dropped wherever removing it does not change how the reader
; PARSES. The boundary test is empirical, not a hardcoded delimiter set: the left
; token (bytes since the last HARD boundary) and the whole next FORM are re-parsed
; with and without the space, and the space is redundant iff both spellings yield
; the SAME datums (`o(` reads the same as `o (`, but `-5` and `- 5` do not, and
; `scare? s` must keep its space -- `scare?s` is ONE name token). Two things make
; this reader-change-proof, where a count over one byte was not: the probe reaches
; the whole next form (so a quote sees its operand -- `ev'ev` vs `ev 'ev`), and it
; compares the datums themselves, not a count (a re-tokenization can keep the count
; yet change the parse). Spaces inside a string literal are data and never touched.
; Surviving bytes are C-escaped.

(: ; the datums readable from s (stop on the eof sentinel or an incomplete form,
   ; which read signals by returning the port itself) -- the full forms, compared
   ; STRUCTURALLY, not just counted: a re-tokenization can preserve the count yet
   ; change the parse (name'(a b) and name '(a b) both read two datums).
   (datums s) ((: (g x a) (: r (sound x) (? (two? r) (g (cup r) (link (cap r) a)) (rev a))))
               s 0)
   ; the next complete form of t AT k, re-shown; 0 if none reads cleanly. reading
   ; at a `spot` is what keeps the walk below linear: snipping the tail per probe
   ; would copy the whole remainder at every space.
   (form1 t k) (: r (sound (spot t k)) (? (two? r) (show (cap r)) 0))
   ; a HARD token boundary: whitespace, delimiters, string quote, comment, and
   ; the value-surface chars. tokens never span these under any lexer law, so
   ; the redundancy probe's left context resets there.
   (hardb c) (? (= c 32) 1 (= c 10) 1 (= c 9) 1
                (= c 40) 1 (= c 41) 1 (= c 91) 1 (= c 93) 1 (= c 123) 1 (= c 125) 1
                (= c 34) 1 (= c 59) 1 (= c 39) 1 (= c 96) 1 (= c 44) 1
                (= c 64) 1 (= c 126) 1 (= c 35) 1
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
   (redundant? t ctx k) (? (&& (two? ctx) (opb? (cap ctx)) (dstart? (peep t k 0))) 0
                           (: lt (string (rev ctx)) rt (form1 t k)
                              (? rt (= (datums (+ lt rt)) (datums (+ (+ lt " ") rt))) 0)))
   ; emit one byte to out, C-escaped for the string literal.
   (eb c) (? (= c 10) (say out "\\n")
              (| (= c 92) (= c 34)) (: _ (put out 92) (put out c))
              (put out c)))

(: p (open (cap (cup argv)) "r")
   _ (? p 0 (: _ (say err "lcat: cannot open input") (quit 1)))
   ; gather every canonical form into one space-joined string. ⚠ a TORN input dies
   ; loudly: a header silently short of its last forms is the empty-lcat disaster
   ; wearing a subtler face -- the build would take it as fresh and fail somewhere else.
   acc ((: (g acc x first)
          (: r (sound x)
             (? (two? r) (g (? first (show (cap r)) (+ (+ acc " ") (show (cap r)))) (cup r) 0)
                (id? r torn) (: _ (say err "lcat: unfinished form in input") (quit 1))
                acc)))
        "" (slurp p) 1)
   _ (put out 34)                                      ; opening "
   ; walk acc: drop redundant spaces (outside string literals), C-escape the rest.
   ; ctx = bytes since the last hard boundary (reversed); instr = inside a "..."
   ; literal (where \ escapes).
   n (tally acc)
   _ ((: (g k ctx instr)
        (? (< k n)
           (: c (peep acc k 0)
              (? instr
                 (? (= c 92) (: _ (eb c) _ (eb (peep acc (k + 1) 0)) (g (k + 2) 0 1))
                    (: _ (eb c) (g (k + 1) 0 (? (= c 34) 0 1))))
                 (? (= c 32)
                    (? (redundant? acc ctx (k + 1))
                       (g (k + 1) ctx 0)                  ; drop the space; ctx keeps accruing
                       (: _ (eb c) (g (k + 1) 0 0)))      ; keep it: a real boundary
                    (: _ (eb c)
                       (g (k + 1) (? (hardb c) 0 (link c ctx)) (= c 34))))))))
        0 0 0)
   _ (put out 34)                                      ; closing "
   (put out 10))