; crew/vi/hue.l -- the .l syntax, written down ONCE, for two readers: the
; painter in crew/vi/core.l's vframe, and vim/syntax.vim, which tools/hue2vim.l
; GENERATES from the very table below and tools/Makefile's test_hue then diffs,
; so the two readings cannot drift apart. (they had: syntax.vim used to carry a
; hand-curated copy of the vocabulary whose own comment asked a human to re-probe
; it on every rename.)
;
; it reads .l by lib/lint.l's rules -- `;` and `#!` comment to end of line,
; "..." is the only string with \ escapes, `'` and backtick are READER OPERATORS
; and never delimiters (the trap a C lexer falls into) -- one step further than
; lint: lint knows delimiters, this knows TOKENS. it is still not a parse.
;
; --- the table ---------------------------------------------------------------
; a row is `(class vim-group rule)`. the rule's head is its TAG, and the tag set
; is CLOSED at eight: eol span mark char delim num word punct. each tag has
; exactly two interpreters -- hue-lex below (bytes -> runs) and hue-vim in the
; generator (tag -> syn lines) -- and neither is a regex engine. when a class
; wants a shape neither can say, that is the signal to add a TAG, not to hand-
; patch either output.
;
; ROW ORDER IS PRIORITY, and the word lanes sit above the punct lanes on purpose:
; `?` is both a special form and a sigil char, `&&` is both a macro and a punct
; run, and putting the word lanes first is exactly how vim reads the same rows
; (a syn keyword beats a syn match).
;
; NOTHING HERE IS A LIST OF WORDS. every word lane names a PROBE and the runtime
; answers it: `names` the whole vocabulary, `macros` the macro table (prel's
; macro-names, the twin of names), `values` the names bound to a value rather
; than a nif. only the three special forms are written out, and they are the
; grammar, not the book. a list would rot; a question cannot.
(:
   hue-classes
    '((comment    Comment    (eol (";" "#!")))
      (string     String     (span 34 92))                 ; " with \ escapes
      (atom       Identifier (mark 39))                    ; 'foo -- the mark and its token
      (ctor       Special    (char 96))                    ; ` -- the list ctor
      (paren      Delimiter  (delim "()[]{}"))
      (number     Number     (num))
      (form       Statement  (word (":" "?" "\\")))        ; letrec* / cond / lambda
      (macro      Operator   (word macros))                ; prel's macro-names
      (const      Constant   (word values))                ; bound to a value, not a nif
      (book       Function   (word names))
      (mono       Special    (punct glued))
      (dyad       Operator   (punct spaced)))

   ; the sigil alphabet, shared by both punct lanes and emitted verbatim into the
   ; generated character class -- stated here, never twice.
   hue-sigils "@#$~.!?%^*+/<>=|&-"

   ; --- reading the table -----------------------------------------------------
   (hue-nth l k) (? (k < 1) <l (hue-nth >l (k - 1)))
   (hue-cls r)  <r
   (hue-grp r)  <>r
   (hue-rule r) <>>r
   (hue-tag r)  <(hue-rule r)
   (hue-arg r)  <>(hue-rule r)
   (hue-tagged tag) (filter (\ r (id? (hue-tag r) tag)) hue-classes)
   (hue-one tag) <(hue-tagged tag)

   hue-eols (hue-arg (hue-one 'eol))                       ; the comment openers
   hue-brk  (hue-arg (hue-one 'delim))                     ; the bracket bytes
   hue-quo  (hue-arg (hue-one 'mark))
   hue-tick (hue-arg (hue-one 'char))
   hue-str  (hue-arg (hue-one 'span))                      ; the quote byte
   hue-esc  (hue-nth (hue-rule (hue-one 'span)) 2)         ; the escape byte
   ; a token ends only on whitespace, a bracket, or a reader operator
   hue-stop (hue-brk + "\"'`;,")

   (hue-has? t c) (: n (tally t) (go i) (? (n <= i) () ((t i) = c) 1 (go (i + 1))) (go 0))
   (hue-blank? c) (|| (c = 32) (c = 9))
   (hue-delim? c) (hue-has? hue-brk c)
   (hue-term? c)  (|| (c < 33) (hue-has? hue-stop c))
   (hue-sig? c)   (hue-has? hue-sigils c)
   (hue-at? s i t)                                         ; does s carry t at i?
    (: n (tally s) m (tally t)
       (go k) (? (m <= k) 1 (n <= (i + k)) () ((s (i + k)) = (t k)) (go (k + 1)) ())
       (go 0))

   ; --- the book --------------------------------------------------------------
   ; pulchritude runs INSIDE love, so the vocabulary is not a list to curate, it
   ; is a question to ask. a bare symbol paints as vocabulary iff the book knows
   ; it -- so the house words colour and your own locals do not, and a rename
   ; re-curates itself. tools/hue2vim.l asks the same question at generation time
   ; and bakes the answer into syn keyword lines.
   ;
   ; ⚠ a NAME, not every binding: `+` `$` `<=` `->` are in the book too, and they
   ; belong to the punct lanes, where the glued/spaced valence shows. one filter
   ; here serves both readers -- syntax.vim has always said the same thing in
   ; prose ("the sigil matches below own every punct run"), which is precisely the
   ; kind of promise that goes stale when only prose keeps it.
   (hue-name? w) (: t (string w) (&& (0 < (tally t)) (! (hue-sig? (t 0)))))

   ; the words a lane claims. a PROBE nom asks the runtime; anything else is the
   ; literal list itself. SORTED, because the generator freezes this into a
   ; diff-gated file and tablet-key order is nobody's promise.
   ;
   ; the two BOOK-derived probes drop sigil-leading names, for the reason above.
   ; `macros` is NOT filtered: `&&` `||` `>>=` `:-` are genuinely macros and should
   ; say so, and they come from the macro table rather than the book, so they were
   ; never competing with the punct lanes in the first place.
   ;
   ; `values` is the CONSTANTS, asked rather than listed. `lit?` is true of a nif
   ; or closure and false of a value, so its complement is the names that ARE
   ; something instead of DOING something -- but that alone is too wide: it also
   ; catches argv, cmdline and `parsed`, which are the RUN, not the language.
   ; ⚠ and `parsed` is bound only when love was handed a file, so the wide test
   ; made the answer depend on HOW THE TOOL WAS INVOKED -- a gate that flaps
   ; between `love f.l` and `cat f.l | love`. so ask the sharper question: a
   ; constant is a SCALAR OR A NAME, a number or a string or a nom. every
   ; run-dependent binding is a list or a tablet, and none of those is any of the
   ; three, so one positive test excludes the lot without naming a single one.
   ; the two book lanes are BOTH positive tests, and that symmetry is what makes
   ; the answer independent of how love was started: a name DOES something (lit?
   ; -- a nif or a closure) or it IS something (a scalar or a name). anything
   ; that is neither is SESSION DATA -- argv, cmdline, parsed, verbs -- and falls
   ; out of both lanes rather than being named and excluded. that costs argv and
   ; cmdline their colour, which is the price of a file that cannot flap.
   (hue-verb? w) (lit? (ev w))
   (hue-const? w) (: v (ev w)
                     (&& (! (lit? v))
                         (|| (constellation? v) (|| (string? v) (nom? v)))))
   (hue-words a)
    (? (id? a 'names)  (sort (map string (filter hue-name? (filter hue-verb? (names ())))))
       (id? a 'macros) (sort (map string (macro-names ())))
       (id? a 'values) (sort (map string (filter hue-name? (filter hue-const? (names ())))))
       a)

   ; ONE tablet, token -> class, filled in REVERSE table order so an earlier row
   ; wins -- the same priority hue-lex reads top-down, paid once at birth instead
   ; of walked per token.
   (hue-book x)
    (foldl (\ b r (foldl (\ b2 w (: _ (pin b2 w (hue-cls r)) b2)) b (hue-words (hue-arg r))))
           (tablet ()) (rev (hue-tagged 'word)))

   ; --- is this token a number? -----------------------------------------------
   ; -3  .5  1.  1e10  1.5e-3 -- an optional sign, a mantissa with at least one
   ; digit, an optional exponent, and NOTHING left over. it runs before the punct
   ; split so a leading `-` or `.` stays part of the number, which is the order
   ; syntax.vim gets by defining LoveNumber after the sigil matches.
   (hue-num? t)
    (: n (tally t)
       (digs i) (? (&& (i < n) (&& (48 <= (t i)) ((t i) <= 57))) (digs (i + 1)) i)
       s0 (? (&& (0 < n) ((t 0) = 45)) 1 0)
       a (digs s0)                                         ; the integer part
       b (? (&& (a < n) ((t a) = 46)) (digs (a + 1)) a)    ; the fraction
       md (? (s0 < a) 1 (? ((a + 1) < b) 1 ()))            ; at least one mantissa digit
       x (? (&& md (&& (b < n) (|| ((t b) = 101) ((t b) = 69))))
             (: e1 (? (&& ((b + 1) < n) (|| ((t (b + 1)) = 45) ((t (b + 1)) = 43)))
                       (b + 2) (b + 1))
                e2 (digs e1)
                (? (e1 < e2) e2 b))
             b)
       (&& md (x = n)))

   ; the word lanes, already folded into one tablet by hue-book; () when no lane
   ; claims the token
   (hue-word bk t) (? (tablet? bk) (bk t) ())

   ; --- the lexer -------------------------------------------------------------
   ; (hue-lex bk s in keep) answers `(runs out): the classified runs over the one
   ; line s, each `(class from to) with `to` EXCLUSIVE, and the string state at
   ; the line's end -- 0 code, 1 still inside a string, which is what carries a
   ; multi-line literal (crew/papel/papel.l's css is forty lines of one string)
   ; onto the next line.
   ;
   ; `keep` gates the EMIT and nothing else, so hue-carry below is literally this
   ; same walk with its answers thrown away. two scanners would drift; there is
   ; one, and this is what makes that true rather than merely intended.
   (hue-lex bk s in keep)
    (: n (tally s)
       (emit acc k a b) (? (&& keep (a < b)) (link `(k a b) acc) acc)
       (tend i) (? (|| (n <= i) (hue-term? (s i))) i (tend (i + 1)))
       (srun i) (? (&& (i < n) (hue-sig? (s i))) (srun (i + 1)) i)
       ; GLUED means the whole run touches a datum start: syntax.vim's rule, and
       ; the excluded set is its own -- blank, `)`, `,`, `;` and more sigil chars
       ; (the last so a greedy run cannot backtrack and split itself). at the end
       ; of a line there is no datum, so a trailing run is dyadic.
       (glued? j) (&& (j < n)
                      (: c (s j)
                         (! (|| (hue-blank? c)
                                (|| (c = 41) (|| (c = 44) (|| (c = 59) (hue-sig? c))))))))
       (word i j acc)
        (: t (snip s i j)
           (? (hue-num? t) (w j (emit acc 'number i j))
              (: k (hue-word bk t)
                 (? (nom? k) (w j (emit acc k i j))
                    (: p (srun i)                          ; a LEADING punct run only --
                       (? (i < p)                          ; punct inside a name is the name
                           (w p (emit acc (? (glued? p) 'mono 'dyad) i p))
                          (w j acc)))))))
       (wstr i a acc)                                      ; inside "..."
        (? (n <= i) `((rev (emit acc 'string a n)) 1)
           (: c (s i)
              (? (c = hue-esc) (wstr (i + 2) a acc)
                 (c = hue-str) (w (i + 1) (emit acc 'string a (i + 1)))
                 (wstr (i + 1) a acc))))
       (w i acc)
        (? (n <= i) `((rev acc) 0)
           (: c (s i)
              (? (hue-blank? c) (w (i + 1) acc)
                 (any (\ t (hue-at? s i t)) hue-eols) `((rev (emit acc 'comment i n)) 0)
                 (c = hue-str) (wstr (i + 1) i acc)
                 (c = hue-quo) (: j (tend (i + 1)) (w j (emit acc 'atom i j)))
                 (c = hue-tick) (w (i + 1) (emit acc 'ctor i (i + 1)))
                 (hue-delim? c) (w (i + 1) (emit acc 'paren i (i + 1)))
                 (word i (tend i) acc))))
       (? in (wstr 0 0 ()) (w 0 ())))

   ; one start-state per line: fold the lexer down the buffer, keeping nothing
   (hue-carry bk ls)
    (: (go l st acc) (? (! (two? l)) (rev acc)
                        (: r (hue-lex bk <l st ()) (go >l <>r (link st acc))))
       (go ls 0 ()))

   ; --- the pens --------------------------------------------------------------
   ; one finished escape per class, built ONCE, so painting a frame is a lookup
   ; and never string-building per token (crew/quay/ink.l's bargain). the theme
   ; is keyed by vim group and the class table joins the two, so swapping
   ; crew/vi/config.l's numbers for another colorscheme's moves the editor and
   ; leaves the generated syn file -- which links to the groups -- alone.
   hue-off "\e[0m"
   (hue-hx c) (? (&& (48 <= c) (c <= 57)) (c - 48) (&& (65 <= c) (c <= 70)) (c - 55) (c - 87))
   (hue-byte t k) ((hue-hx (t k)) * 16 + (hue-hx (t (k + 1))))
   (hue-faces fs) (foldl (\ a f (a + (? (id? f 'bold) "1;" "3;"))) "" fs)
   (hue-pen row deep)
    (: n (hue-nth row 1) g (hue-nth row 2) fs (hue-nth row 3)
       ("\e[" + (hue-faces fs)
        + (? deep ("38;2;" + show (hue-byte g 1) + ";" + show (hue-byte g 3)
                           + ";" + show (hue-byte g 5))
             ("38;5;" + show n))
        + "m"))
   (hue-pens rows deep)
    (: gp (foldl (\ b r (: _ (pin b <r (hue-pen r deep)) b)) (tablet ()) rows)
       (foldl (\ b c (: _ (pin b (hue-cls c) (gp (hue-grp c))) b)) (tablet ()) hue-classes))

   ; --- painting one line -----------------------------------------------------
   ; escapes are ZERO WIDTH, so a caller clips FIRST and paints the clipped text:
   ; every column it already worked out stays worked out.
   (hue-paint pens s runs)
    (: n (tally s)
       (go i l acc)
        (? (! (two? l)) (acc + (snip s i n))
           (: r <l k <r a <>r b <>>r p (pens k)
              (? (string? p) (go b >l (acc + (snip s i a) + p + (snip s a b) + hue-off))
                 (go b >l (acc + (snip s i b))))))
       (go 0 runs ""))
   (hue-line pens bk s in) (hue-paint pens s <(hue-lex bk s in 1))

   ; --- the theme, and the user's own ------------------------------------------
   ; kore ships as a baked image, so crew/vi/config.l is not a knob anyone
   ; downstream has. a theme file beside the seat overrides it, read as DATA with
   ; `sound` (the datum reader) and never evaled. ⚠ PRESENCE IS THE OPEN: a theme
   ; file with no rows in it means "no overrides", which is a perfectly good
   ; thing for a theme file to mean, so the question is whether it OPENED -- not
   ; whether it had bytes. a row that is not a row is simply not a row: a bad
   ; theme must not take the editor down with it.
   (hue-over rows r) (? (two? r) (map (\ o (? (id? <o <r) r o)) rows) rows)
   (hue-rows x)
    (: h (getenv "HOME")
       q (? (string? h) (open (h + hue-theme-file) "r") ())
       (? q (: t (slurp q)
               (go cl acc) (: r (sound cl)
                              (? (two? r) (go (cup r) (link (cap r) acc)) (rev acc)))
               rs (go (map t (jot (tally t))) ())
               _ (close q)
               (foldl hue-over hue-theme rs))
          hue-theme))
   (hue-deep x) (: ct (getenv "COLORTERM")
                   (? (string? ct) (|| (ct = "truecolor") (ct = "24bit")) ()))
   (hue-wake x) (hue-pens (hue-rows ()) (hue-deep ())))
