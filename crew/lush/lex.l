; --- lush 🐚 the lexer -- one line (or a joined continuation) to tokens ---
; NO BRACKETS HERE: the runtime supplies the layer (see job.l for the cat law).
;
; (sh-lex s) -> (1 . tokens) | ('more . why) -- 'more is the PS2 protocol: an
; unfinished shape (open quote, open $( ), trailing \) asks the caller for
; another line; join with "\n" and re-lex the whole buffer. tokens:
;   ('w . segs)          a WORD as a segment list, quote structure kept:
;     ('lit . s)           unquoted run (glob chars live, $ already carved out)
;     ('sq . s)            '..' literal
;     ('dq . inner)        ".." -- inner is lit/var/sub segs, all splitting off
;     ('var . name)        $NAME / $? / $0-9 (one digit, the POSIX law)
;     ('sub . text)        $( text ) or `text`, run at expansion time
;   ('op . o)            | || & && ; ;; ( ) and "nl" (a newline is a separator,
;                        NOT whitespace -- multi-line -c strings and joined
;                        continuations sequence on it)
;   ('rd . (ion . o))    a redirect: < > >> >& <& << glued or spaced; ion is
;                        the io-number OUT OF BAND -- () default, (n) attached
;                        (`2>&1` lexes ((2) . ">&") + word "1"; a QUOTED 2 is
;                        a word, per POSIX)
; a # at a word START kills the rest of the line; \X is X literal (glued into
; the word), \<newline> vanishes (the join law); "" keeps \ escapes for $ ` " \
; only. operators GLUE now -- `a>b` `a&&b` need no spaces.

(: (lx-ws? b) (|| (= b 32) (= b 9))
   (sh-digits? s)
     (: n (tally s)
        (go i) (? (= i n) (< 0 n)
                  (: b (s i) (? (&& (<= 48 b) (< b 58)) (go (+ i 1)) 0)))
        (go 0))
   (sh-name? b) (? (&& (<= 48 b) (< b 58)) 1
                   (&& (<= 65 b) (< b 91)) 1
                   (&& (<= 97 b) (< b 123)) 1
                   (= b 95))
   (sh-name1? b) (|| (&& (<= 65 b) (< b 91)) (&& (<= 97 b) (< b 123)) (= b 95))

   (sh-lex s)
   (: n (tally s)
      (addl cur segs) (? (tally cur) (link (link 'lit cur) segs) segs)
      (fw cur started segs toks)                    ; flush the word in progress
        (: sg (addl cur segs)
           (? (|| started (two? sg)) (link (link 'w (sh-rev sg)) toks) toks))
      (fin toks) (link 1 (sh-rev toks))
      ; '...' -> index of the closing quote | ()
      (sq j) (? (= j n) () (? (= (s j) 39) j (sq (+ j 1))))
      ; the \ escapes that stay escapes inside ".."
      (esc? b) (|| (= b 34) (= b 36) (= b 92) (= b 96))
      (btesc? b) (|| (= b 96) (= b 92) (= b 36))
      ; `...` -> (text . j-past) | ()
      (bt i acc)
        (? (= i n) ()
           (: b (s i)
              (? (= b 96) (link acc (+ i 1))
                 (&& (= b 92) (< (+ i 1) n) (btesc? (s (+ i 1))))
                   (bt (+ i 2) (+ acc (s (+ i 1))))
                 (bt (+ i 1) (+ acc b)))))
      ; $( body ): balanced parens, blind to quoted ones -> j past ) | ()
      (subp i depth q pe)
        (? (= i n) ()
           pe (subp (+ i 1) depth q 0)
           (: b (s i)
              (? (= q 39) (subp (+ i 1) depth (? (= b 39) 0 39) 0)
                 (= b 92) (subp (+ i 1) depth q 1)
                 (= q 34) (subp (+ i 1) depth (? (= b 34) 0 34) 0)
                 (= b 39) (subp (+ i 1) depth 39 0)
                 (= b 34) (subp (+ i 1) depth 34 0)
                 (= b 40) (subp (+ i 1) (+ depth 1) 0 0)
                 (= b 41) (? (= depth 1) (+ i 1) (subp (+ i 1) (- depth 1) 0 0))
                 (subp (+ i 1) depth 0 0))))
      (nrun i acc) (? (&& (< i n) (sh-name? (s i))) (nrun (+ i 1) (+ acc (s i))) (link acc i))
      ; a $-form at i (past the $) -> (seg . j) | ('dollar . i) literal | 'unterm
      (dol i)
        (? (= i n) (link 'dollar i)
           (: b (s i)
              (? (= b 63) (link (link 'var "?") (+ i 1))
                 (= b 40) (: e (subp (+ i 1) 1 0 0)
                             (? (nil? e) 'unterm
                                (link (link 'sub (snip s (+ i 1) (- e 1))) e)))
                 (&& (<= 48 b) (< b 58)) (link (link 'var (+ "" b)) (+ i 1))
                 (sh-name1? b) (: r (nrun i "") (link (link 'var (cap r)) (cup r)))
                 (link 'dollar i))))
      ; ".." -> (segs . j-past) | () unfinished
      (dq i cur segs)
        (? (= i n) ()
           (: b (s i)
              (? (= b 34) (link (sh-rev (addl cur segs)) (+ i 1))
                 (&& (= b 92) (< (+ i 1) n) (= (s (+ i 1)) 10)) (dq (+ i 2) cur segs)
                 (&& (= b 92) (< (+ i 1) n) (esc? (s (+ i 1))))
                   (dq (+ i 2) (+ cur (s (+ i 1))) segs)
                 (= b 36) (: d (dol (+ i 1))
                             (? (two? (cap d)) (dq (cup d) "" (link (cap d) (addl cur segs)))
                                (id? (cap d) 'dollar) (dq (cup d) (+ cur "$") segs)
                                ()))
                 (= b 96) (: r (bt (+ i 1) "")
                             (? (nil? r) ()
                                (dq (cup r) "" (link (link 'sub (cap r)) (addl cur segs)))))
                 (dq (+ i 1) (+ cur b) segs))))
      (op2 i cur started segs toks o k)             ; flush + emit an operator
        (: tks (fw cur started segs toks)
           (lx (+ i k) "" 0 () (link (link 'op o) tks)))
      (peek? i b) (&& (< (+ i 1) n) (= (s (+ i 1)) b))
      (lx i cur started segs toks)
        (? (= i n) (fin (fw cur started segs toks))
           (: b (s i)
              (? (lx-ws? b) (lx (+ i 1) "" 0 () (fw cur started segs toks))
                 (= b 10) (op2 i cur started segs toks "nl" 1)
                 (&& (= b 35) (! started)) (fin toks)
                 (= b 39) (: j (sq (+ i 1))
                             (? (nil? j) (link 'more 'quote)
                                (lx (+ j 1) "" 1 (link (link 'sq (snip s (+ i 1) j)) (addl cur segs)) toks)))
                 (= b 34) (: r (dq (+ i 1) "" ())
                             (? (nil? r) (link 'more 'quote)
                                (lx (cup r) "" 1 (link (link 'dq (cap r)) (addl cur segs)) toks)))
                 (= b 36) (: d (dol (+ i 1))
                             (? (two? (cap d)) (lx (cup d) "" 1 (link (cap d) (addl cur segs)) toks)
                                (id? (cap d) 'dollar) (lx (cup d) (+ cur "$") 1 segs toks)
                                (link 'more 'sub)))
                 (= b 96) (: r (bt (+ i 1) "")
                             (? (nil? r) (link 'more 'sub)
                                (lx (cup r) "" 1 (link (link 'sub (cap r)) (addl cur segs)) toks)))
                 (= b 92) (? (= (+ i 1) n) (link 'more 'cont)
                             (= (s (+ i 1)) 10) (lx (+ i 2) cur started segs toks)
                             (lx (+ i 2) (+ cur (s (+ i 1))) 1 segs toks))
                 (|| (= b 60) (= b 62))
                   (: ion (? (&& (! (two? segs)) (sh-digits? cur)) (L (sh-int cur)) ())
                      tks (? (two? ion) toks (fw cur started segs toks))
                      o (? (= b 62) (? (peek? i 62) ">>" (peek? i 38) ">&" ">")
                           (? (peek? i 60) "<<" (peek? i 38) "<&" "<"))
                      (lx (+ i (tally o)) "" 0 () (link (link 'rd (link ion o)) tks)))
                 (= b 124) (? (peek? i 124) (op2 i cur started segs toks "||" 2)
                              (op2 i cur started segs toks "|" 1))
                 (= b 38) (? (peek? i 38) (op2 i cur started segs toks "&&" 2)
                             (op2 i cur started segs toks "&" 1))
                 (= b 59) (? (peek? i 59) (op2 i cur started segs toks ";;" 2)
                             (op2 i cur started segs toks ";" 1))
                 (= b 40) (op2 i cur started segs toks "(" 1)
                 (= b 41) (op2 i cur started segs toks ")" 1)
                 (lx (+ i 1) (+ cur b) 1 segs toks))))
      (lx 0 "" 0 () ())))
