; lib/lint.l -- the .l balance scan, structured so its consumers can share one
; scanner: `ltidy` (tools/ltidy.l) prints what it finds and gates `make lint`,
; `lupa` (crew/lupa/lupa.l) turns the same warnings into lsp diagnostics. it
; reads .l correctly -- `;` and `#!` start comments to end of line, `"..."` is
; the only string, and `'` / backtick are READER OPERATORS (quote, list-ctor),
; never delimiters, so they scan as ordinary code (the trap a C lexer falls
; into). it is a BALANCE scan, not a parse: it knows delimiters and strings and
; nothing else about the language.
;
; (lint-scan src) answers a list of warnings, each `(line col text) with line
; and col 1-BASED -- a position, so a consumer can point AT the character rather
; than re-derive one from a sentence. no warnings is (), which here means what
; it says: nothing to report.
;
; an UNCLOSED opener reports where it OPENED, not end-of-file -- the stack
; carries each opener's position for exactly that -- and an unclosed string the
; same way. (a dropped paren is the classic .l slip; pointing at the far end of
; the file is the least useful place to point.)
(: (lclose c) (? (c = 40) 41 (c = 91) 93 125)          ; ( -> )   [ -> ]   { -> }
   (lopen c)  (? (c = 41) 40 (c = 93) 91 123)          ; ...and back, for the report

   (lint-scan src)
    (: n (tally src)
       ; stk: `(closer line col) per open delimiter, a matched-pair stack, so ([)]
       ; is caught and not just a net count. sl/sc hold a string's opening spot.
       ; states: code | cmt (; to EOL) | str (".." with \ escapes).
       (w i st stk line col sl sc warns)
        (? (n <= i)
            (: w1 (? (id? st 'str) (link `(sl sc "unclosed string") warns) warns)
               (rev (foldl (\ a o (link `(<>o <>>o ("unclosed " + string (lopen <o))) a)) w1 stk)))
           (: c (src i)
              (? (id? st 'str)                                    ; inside "..."
                  (? (c = 92) (w (i + 2) 'str stk line (col + 2) sl sc warns)   ; \ escape: skip next
                     (c = 34) (w (i + 1) 'code stk line (col + 1) sl sc warns)
                     (c = 10) (w (i + 1) 'str stk (line + 1) 1 sl sc warns)
                     (w (i + 1) 'str stk line (col + 1) sl sc warns))
                 (id? st 'cmt)                                    ; ; comment to EOL
                  (? (c = 10) (w (i + 1) 'code stk (line + 1) 1 sl sc warns)
                     (w (i + 1) 'cmt stk line (col + 1) sl sc warns))
                 (c = 59) (w (i + 1) 'cmt stk line (col + 1) sl sc warns)       ; ; -- start comment
                 (&& (c = 35) (&& ((i + 1) < n) ((src (i + 1)) = 33)))          ; #! shebang, to EOL
                  (w (i + 2) 'cmt stk line (col + 2) sl sc warns)
                 (c = 34) (w (i + 1) 'str stk line (col + 1) line col warns)    ; " -- start string
                 (c = 10) (w (i + 1) 'code stk (line + 1) 1 sl sc warns)
                 (|| (c = 40) (|| (c = 91) (c = 123)))                          ; ( [ {  -- push
                  (w (i + 1) 'code (link `((lclose c) line col) stk) line (col + 1) sl sc warns)
                 (|| (c = 41) (|| (c = 93) (c = 125)))                          ; ) ] }  -- match
                  (? (! (two? stk))
                      (w (i + 1) 'code stk line (col + 1) sl sc
                         (link `(line col ("stray " + string c)) warns))
                     (! (<<stk = c))
                      (w (i + 1) 'code >stk line (col + 1) sl sc
                         (link `(line col ("mismatched " + string c)) warns))
                     (w (i + 1) 'code >stk line (col + 1) sl sc warns))
                 (w (i + 1) 'code stk line (col + 1) sl sc warns))))            ; ' ` \ and the rest
       (w 0 'code () 1 1 1 1 ())))
