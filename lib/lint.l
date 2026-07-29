; lib/lint.l -- the .l balance scan, structured so its consumers can share one
; scanner: `ltidy` (tools/ltidy.l) prints what it finds and gates `make lint`,
; `libra` (crew/libra/libra.l) turns the same warnings into lsp diagnostics. it
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
; (lint-lay src) answers the INDENT PLAN: one entry per line, `(1 n) for "this
; line's first character belongs at 0-based column n", or () for "leave this
; line exactly as it is" (a line inside a string, where the leading spaces ARE
; the string). ⚠ the wrapper is not decoration -- column 0 is the commonest
; plan there is, so a bare number could not say "no plan" (the presence law).
;
; the rule is ONE sentence: a continuation line aligns under its form's FIRST
; OPERAND, or one past the open delimiter when the head stands alone on its
; line. that is the classic lisp rule and nothing more -- it is LINE-PRESERVING
; by construction, since it only ever says where a line STARTS. it never moves a
; break, never re-flows, never touches what is on the line. everything the house
; style does that this cannot predict (aligned comment columns, hand-laid
; tables) survives untouched for exactly that reason.
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
       (w 0 'code () 1 1 1 1 ()))

   ; --- the indent plan ------------------------------------------------------
   ; a frame is `(base openline ntok pair): where this form's continuation lines
   ; align (0-based), the line it opened on, how many of its tokens we have passed
   ; (the HEAD is token 1), and whether it is a PAIRING form.
   ;
   ; ⚠ THE PAIRING RULE is the whole difference between a formatter that is close
   ; and one that is right. `:` and `?` take their operands TWO AT A TIME -- : is
   ; name/value, ? is test/result -- and the house sets the second of each pair
   ; ONE PAST the first, so a value or a result is never mistaken for another name
   ; or another test at a glance. every other form aligns all its operands flat.
   ; without this the pass is off by one space on every second operand, which is
   ; most of a file.
   ; ⚠ A WIDTH GUARD WAS TRIED HERE AND PAYS NOWHERE -- do not re-add it without
   ; new evidence. the idea: cap alignment, so a first operand sitting far right
   ; falls back to one past its own open paren (black's bargain), on the theory
   ; that it would rescue the hand-laid HANGING files. measured over the tree at
   ; thresholds 24/32/40/56: the hanging files barely move (love/ev.l 494 -> 506
   ; changed lines, crew/holo/holotest.l flat), because their authors broke to a
   ; column LEFT of the enclosing paren -- which no cap-and-fallback reproduces --
   ; while every well-laid file gets worse (crew/libra/libra.l 7 -> 22, this file
   ; 1 -> 4) and tree-wide churn RISES from 12591 to 16448. alignment it is.
   (ldelim? c) (|| (c < 33) (|| (c = 40) (|| (c = 41) (|| (c = 91)
                                (|| (c = 93) (|| (c = 123) (c = 125)))))))
   (lpair? c nx) (&& (|| (c = 58) (c = 63)) (ldelim? nx))     ; a LONE : or ? , not a name starting with one

   ; the next line's indent: the frame's base, plus one when the token that will
   ; start it is the second of a pair
   (lcont stk) (? (! (two? stk)) 0
                  (: f <stk
                     base <f
                     nt <>>f
                     pr <>>>f
                     (? (&& pr (0 = (nt % 2))) (base + 1) base)))

   ; the HEAD token just ENDED at 1-based column x (the delimiter after it). a form
   ; whose head stands ALONE on its line -- `(:` opening a module, `(assert` opening
   ; a block -- indents its body where operand ONE would have gone had it been
   ; written there: one space past the head. so a `(:` body lands at 3 and an
   ; `(assert` body at 8, which is what the house does and what "open paren + 1"
   ; never gets right.
   (lhend stk line x)
    (? (! (two? stk)) stk
       (: f <stk
          ol <>f
          nt <>>f
          pr <>>>f
          ; ⚠ ONLY the pairing forms. `(assert` alone on its line indents its body
          ; ONE past the open paren, while `(:` alone indents THREE -- the house
          ; style is per-head, the way every lisp's indent spec is, and one rule
          ; forced over both moves whichever file it was not written for.
          (? (&& pr (&& (nt = 1) (ol = line)))
              (link `(x ol 1 pr) >stk)
             stk)))

   ; a token (or a nested opener -- it is a token to its parent) begins at 1-based
   ; column tc; c is its first character and nx the one after
   (ltok stk line tc c nx)
    (? (! (two? stk)) stk
       (: f <stk
          base <f
          ol <>f
          nt <>>f
          pr <>>>f
          (? (nt = 0)                (link `(base ol 1 (? (lpair? c nx) 1 0)) >stk)
             ; operand one: this is where the form aligns
             (&& (nt = 1) (ol = line)) (link `((tc - 1) ol 2 pr) >stk)
             (link `(base ol (nt + 1) pr) >stk))))

   ; ⚠ `it` is 1 while we are INSIDE a token: ltok must fire once per token, at
   ; its first character, never once per character -- a three-letter name would
   ; otherwise count as three tokens and its second letter would be mistaken for
   ; the form's first operand.
   (lint-lay src)
    (: n (tally src)
       (w i st stk line col it plan)
        (? (n <= i) (rev plan)
           (: c (src i)
              (? (id? st 'str)
                  (? (c = 92) (w (i + 2) 'str stk line (col + 2) 1 plan)
                     (c = 34) (w (i + 1) 'code stk line (col + 1) 0 plan)
                     ; a line BEGINNING inside a string is the string's own text
                     (c = 10) (w (i + 1) 'str stk (line + 1) 1 1 (link () plan))
                     (w (i + 1) 'str stk line (col + 1) 1 plan))
                 (id? st 'cmt)
                  (? (c = 10) (w (i + 1) 'code stk (line + 1) 1 0 (link `(1 (lcont stk)) plan))
                     (w (i + 1) 'cmt stk line (col + 1) 0 plan))
                 (c = 59) (w (i + 1) 'cmt stk line (col + 1) 0 plan)
                 (&& (c = 35) (&& ((i + 1) < n) ((src (i + 1)) = 33)))
                  (w (i + 2) 'cmt stk line (col + 2) 0 plan)
                 (c = 34) (w (i + 1) 'str (ltok stk line col c 32) line (col + 1) 1 plan)
                 ; ⚠ the head's end must land BEFORE the plan is read, or the very
                 ; line the body starts on still carries the old fallback
                 (c = 10) (: s2 (? it (lhend stk line col) stk)
                             (w (i + 1) 'code s2 (line + 1) 1 0 (link `(1 (lcont s2)) plan)))
                 (|| (c = 40) (|| (c = 91) (c = 123)))
                  ; an opener is a token to its PARENT -- UNLESS it is GLUED to the
                  ; token before it. ⚠ `~(0 1)`, `'(a b)`, `#(k v)`, `@(1 2)` are ONE
                  ; operand wearing a reader sigil, not two, and counting them as two
                  ; flips the pairing parity for every line left in the form. it is
                  ; the one miscount that CASCADES, so it is the one worth naming:
                  ; `it` is already "the previous character was part of a token".
                  (w (i + 1) 'code
                     (link `(col line 0 0) (? it stk (ltok stk line col c 32)))
                     line (col + 1) 0 plan)
                 (|| (c = 41) (|| (c = 93) (c = 125)))
                  (w (i + 1) 'code (? (two? stk) >stk stk) line (col + 1) 0 plan)
                 (|| (c = 32) (c = 9))
                  (w (i + 1) 'code (? it (lhend stk line col) stk) line (col + 1) 0 plan)
                 ; an ordinary character: a token START only if we were not in one
                 (w (i + 1) 'code
                    (? it stk (ltok stk line col c (? ((i + 1) < n) (src (i + 1)) 10)))
                    line (col + 1) 1 plan))))
       (w 0 'code () 1 1 0 (link `(1 0) ()))))
