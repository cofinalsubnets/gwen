; tools/ltidy.l -- the .l linter/janitor. NOT a pretty-printer: .l indentation is
; paren-depth + deliberate hand-alignment (aligned ? clauses, : bindings, go
; continuations), which no mechanical pass reproduces without fighting the house
; style. so it LINTS instead -- paren/bracket/brace balance (a matched-pair stack,
; so ([)] is caught), unclosed strings, and stray tabs -- QUIET when clean, warnings
; on err, and EXIT 1 on any imbalance (so `make lint` gates on it; tabs warn but
; never fail). -w also rewrites each file with trailing whitespace stripped.
; usage: ltidy [-w] FILE ..
;
; the SCAN is lib/lint.l's, shared with lupa (the language server), which turns the
; same warnings into lsp diagnostics -- one scanner, so the editor and the gate
; always agree about what is broken. a warning is `(line col text), and ltidy lays
; it out as `path:line:col: text` -- the standard compiler shape, which an editor's
; error list and a terminal's file-link both already know how to read.
(use 'lint)
(:
   (ws? c) (|| (c = 32) (c = 9))
   (rtrim s) (: n (tally s) (g j) (? (&& (0 < j) (ws? (s (j - 1)))) (g (j - 1)) (snip s 0 j)) (g n))
   (linesof s) (: n (tally s)
                  (g i st acc) (? (n <= i) (rev (link (snip s st n) acc))
                                  ((s i) = 10) (g (i + 1) (i + 1) (link (snip s st i) acc))
                                  (g (i + 1) st acc))
                  (g 0 0 ()))
   (unlines ls) (? (! (two? ls)) "" (foldl (\ a x (a + "\n" + x)) <ls >ls))
   ; the first tab's 1-based column, or () -- a position, so a tab warning lays out
   ; the same way a balance one does
   (tabcol s) (: n (tally s) (g i) (? (n <= i) () ((s i) = 9) (i + 1) (g (i + 1))) (g 0))

   ; lines carrying a tab, in lint-scan's `(line col text) shape so one formatter
   ; serves both kinds
   (tabwarns lines)
    (: (g ls k acc) (? (! (two? ls)) (rev acc)
                       (: c (tabcol <ls)
                          (g >ls (k + 1) (? c (link `(k c "tab") acc) acc))))
       (g lines 1 ()))

   ; path:line:col: text -- what a compiler prints and an editor already parses
   (fmt path wr) (path + ":" + show <wr + ":" + show <>wr + ": " + <>>wr + "\n")

   ; lint one file: warnings (with line numbers) to err, QUIET when clean. -w also
   ; rewrites it with trailing whitespace stripped. answers the count of FATAL
   ; (balance/string) warnings -- tabs warn but never fail the gate.
   (tidy src path fix)
    (: lines (linesof src)
       bwarns (lint-scan src)
       warns (bwarns + (tabwarns lines))
       _ (foldl (\ _ m (: _ (say err (fmt path m)) 0)) 0 warns)
       _ (? fix (: q (open path "w") _ (say q (unlines (map rtrim lines))) (close q)) 0)
       (tally bwarns))

   ; --- driver: ltidy [-w] FILE ..  (lint -> err, exit 1 on any imbalance; -w strips trailing ws) ---
   (rdf p) (: q (open p "r") (? q (: t (slurp q) _ (close q) `(1 t)) ()))
   (each fs fix bad) (? (! (two? fs)) bad
                        (: r (rdf <fs)
                           n (? (two? r) (tidy <>r <fs fix) (: _ (say err ("ltidy: cannot open " + <fs + "\n")) 1))
                           (each >fs fix (bad + n))))
   (flags as fix acc) (? (! (two? as)) `(fix (rev acc))
                         (<as = "-w") (flags >as 1 acc)
                         (flags >as fix (link <as acc)))
   f (flags >>cmdline () ())
   files <>f
   nbad (? (two? files) (each files <f 0) (: _ (say err "usage: ltidy [-w] FILE ..\n") 0))
   (? (0 < nbad) (quit 1) 0))
