#!/usr/bin/env -S love -l
; libra ⚖ -- the scales: the .l balance tool. it weighs a file two ways and they
; are the same weighing -- `libra` says what is wrong with it, and `libra doc`
; lifts its header out as a document. one scanner (lib/lint.l) under both, so the
; gate and the doc lifter can never disagree about what a paren or a comment is.
;
; usage:  libra FILE ..      weigh them: balance is the default verb, so a bare
;                            file list is the whole command
;         libra doc FILE     its header as a document -- md, -ht html, -rf man
;     or: love -l crew/libra/libra.l ..
;
; settings come from lib/salt.l -- ~/.love/etc/libra.l, then ./.libra.l over it.
; the rules, what they mean, and which fail a gate: doc/misc/libra.md.

; the head names the exports, so the scanner's ~30 helpers stay off the tablet;
; lint and salt are spliced in here rather than at the top level, where a catted
; libra would hand the whole session their names as well as its own.
; `fatal?` is out because test/host/libra.l pins the gate rule against it.
(module (libra libra-main libra-check libra-doc ldoc-head ldoc-md fatal?)
(use 'lint)
(use 'salt)

(:
   ; --- lines ----------------------------------------------------------------
   (ltrim s) (: n (tally s)
                (g i) (? ((i < n) && ((s i = 32) || (s i = 9))) (g (i + 1)) (snip s i n))
                (g 0))
   (linesof s) (: n (tally s)
                  (g i st acc) (? (n <= i) (rev (snip s st n >< acc))
                                  (s i = 10) (g (i + 1) (i + 1) (snip s st i >< acc))
                                  (g (i + 1) st acc))
                  (g 0 0 ()))

   ; --- the check ------------------------------------------------------------
   ; `path:line:col: text`, the shape an editor's error list already parses
   (warnline path w) (path + ":" + show <w + ":" + show <>w + ": " + <>>w + "\n")

   ; --- the verbs ------------------------------------------------------------
   (rdf p) (: q (open p "r") (? q (: t (slurp q) _ (close q) `(1 t)) ()))

   ; BALANCE ALONE fails. the other rules SPEAK -- an editing aid, read by hand --
   ; and nothing promotes one, so libra never refuses a file over a habit.
   (fatal? w) (id? <>>>w 'balance)
   (tally-fatal ws) (foldl (\ a w (? (fatal? w) (a + 1) a)) 0 ws)

   (libra-check-files conf fs bad)
    (? !(two? fs) bad
       (: r (rdf <fs)
          (? !(two? r) (: _ (say err ("libra: cannot open " + <fs + "\n"))
                           (libra-check-files conf >fs (bad + 1)))
             (: src <>r
                bw (lint-of src conf)
                _ (foldl (\ _ w (: _ (say err (warnline <fs w)) 0)) 0 bw)
                (libra-check-files conf >fs (bad + tally-fatal bw))))))

   (libra-check conf fs) (: n (libra-check-files conf fs 0) (? (0 < n) (quit 1) 0))

   (flag? x) ((0 < tally x) && (x 0 = 45))
   (files as) (filter (\ x !(flag? x)) as)

   ; --- the header, as a document --------------------------------------------
   ; a text walk, and it has to be: no reader in the tree keeps comments, so a
   ; header lifted from a datum would be empty.
   ;
   ; the header runs to the first line carrying code, blanks included -- not to
   ; the first blank line, which would cut most of the corpus's headers in half.
   (dblank? s) !(0 < tally (ltrim s))
   ; one leading space off, so `;   foo` keeps the display's two and a bare `;`
   ; is the blank line it looks like
   (dbody s) (: n (tally s) (? ((0 < n) && (s 0 = 32)) (snip s 1 n) s))
   (dcmts src) (: t (tablet 0) _ (each (lint-cmts src) (\ c (pin t <c c))) t)
   (dalone ln c) (: k (<>c - 1)
                    (? (dblank? (snip ln 0 k)) `(1 (snip ln 0 k + dbody <>>c)) ()))
   (drtrim l) (? !(two? l) () (: r (drtrim >l) (? ((two? r) || !(dblank? <l)) (<l >< r) ())))

   ; a `#!` line is a seat, not a sentence -- six crew tools open with one
   (ldoc-head src)
    (: t (dcmts src)
       (g ls k acc)
        (? !(two? ls) (drtrim (rev acc))
           (: c (peep t k ())
              a (? (two? c) (dalone <ls c) ())      ; the line's own sentence, if it is one
              (? !(two? a) (? (dblank? <ls) (g >ls (k + 1) ("" >< acc))
                               (drtrim (rev acc))) ; ..else this line is code: the header ends
                 (id? <>>>c 'bang) (g >ls (k + 1) acc)             ; the seat line says nothing
                 (g >ls (k + 1) (<>a >< acc)))))
       (g (linesof src) 1 ()))

   ; the prose is taken as written; three of its habits carry structure markdown
   ; would fill flat -- a `--- name ---` banner is a heading, an indented run is a
   ; fence because its alignment is the content, and a leading ⚠ is bolded.
   (dind s) (: n (tally s) (g i) (? ((i < n) && ((s i = 32) || (s i = 9))) (g (i + 1)) i) (g 0))
   (ddashy? c) ((c = 45) || (c = 32))
   ; `--- name ------------` -> `(1 "name"); dashes both sides are marker and padding
   (dbanner s)
    (: n (tally s)
       (? !((3 <= n) && (snip s 0 3 = "---")) ()
          (: a ((: (f i) (? ((i < n) && (ddashy? (s i))) (f (i + 1)) i)) 0)
             b ((: (f j) (? ((a < j) && (ddashy? (s (j - 1)))) (f (j - 1)) j)) n)
             (? (a < b) `(1 (snip s a b)) ()))))
   (dstarts? s t) (: n (tally s) m (tally t) ((m <= n) && (snip s 0 m = t)))
   (ditem? s) (: n (tally s)
                 (((2 <= n) && (snip s 0 2 = "- "))
                  || ((3 <= n) && ((47 < s 0) && (s 0 < 58) && (snip s 1 3 = ". ")))))
   ; the marker in bold; a hazard is a paragraph and only its first line wears it
   (dwarn s) (? (dstarts? s "⚠ ") ("**⚠**" + snip s 3 (tally s)) s)
   ; ..and it starts one -- a hazard jammed against the prose above it would
   ; otherwise fill into that paragraph, glyph and all
   (dnl2? s) (: n (tally s) (? (n < 2) (n = 0) ((s (n - 1) = 10) && (s (n - 2) = 10))))
   (dbreak acc s) (? (&& (dstarts? s "⚠ ") !(dnl2? acc)) (acc + "\n") acc)

   ; a display: the run of indented lines at the head of ls, and what is left.
   (dtake ls)
    (: (g l acc) (? (!(two? l) || (dblank? <l) || !(0 < dind <l)) `((rev acc) l)
                    (g >l (<l >< acc)))
       (g ls ()))
   ; ..fenced, dedented by its own least indent: the alignment inside a display is
   ; the content, the margin in front of it is not
   (dmin a b) (? (a < b) a b)
   (dfence ls)
    (: k (foldl (\ a s (? (dblank? s) a (dmin a (dind s)))) (tally <ls) ls)
       ("```\n" + foldl (+) "" (map (\ s ((snip s (dmin k (tally s)) (tally s)) + "\n")) ls)
        + "```\n"))

   ; the header as markdown text; lapiz decides what a document is, and every
   ; surface it has then arrives free. li: last line was a list item or its wrap
   ; -- an indented line means two different things depending on it.
   (ldoc-md src)
    (: (go ls li acc)
        (? !(two? ls) acc
           (: s <ls
              b (dbanner s)
              (? (dblank? s)         (go >ls 0 (acc + "\n"))
                 (two? b)            (go >ls 0 (acc + "## " + <>b + "\n"))
                 (dstarts? s "usage:")
                  (: r (dtake >ls) (go <>r 0 (acc + dfence (s >< <r))))
                 (&& (0 < dind s) li) (go >ls 1 (acc + s + "\n"))
                 (0 < dind s)        (: r (dtake ls) (go <>r 0 (acc + dfence <r)))
                 (ditem? s)          (go >ls 1 (acc + dwarn s + "\n"))
                 (go >ls 0 (dbreak acc s + dwarn s + "\n")))))
       (go (ldoc-head src) 0 ""))

   ; --- doc ------------------------------------------------------------------
   ; the `# name` is not decoration -- papel reads a page's title, anchors and
   ; contents nav off the headings, so without it a page carries no navigation.
   ; lapiz loads here, not at the top: `make lint` runs check over every tracked
   ; .l and pays for whatever this file names.
   (libra-doc-files fs surf bad)
    (? !(two? fs) bad
       (: r (rdf <fs)
          (? !(two? r) (: _ (say err ("libra: cannot open " + <fs + "\n"))
                           (libra-doc-files >fs surf (bad + 1)))
             (: body (ldoc-md <>r)
                md ("# " + <fs + "\n\n" + body)
                (? !(0 < tally body)
                    (: _ (say err (<fs + ": no header comment\n"))
                       (libra-doc-files >fs surf (bad + 1)))
                   (: _ (say out (? (id? surf 'md) md
                                     ((from 'lapiz (? (id? surf 'ht) 'md->ht 'md->rf)) md)))
                      (libra-doc-files >fs surf bad)))))))
   (libra-doc fs surf)
    (: _ (? (id? surf 'md) 0 (use 'lapiz))
       n (libra-doc-files fs surf 0)
       (? (0 < n) (quit 1) 0))

   ; above its reader: only lambda-to-lambda recursion resolves lazily, so a
   ; closure over a later non-lambda binding of this `:` raises missing usage at load.
   ; an infix chain, not (+ a b c) -- three operands is application, ((+ a b) c)
   usage ("usage: libra [check] FILE ..   balance: path:line:col: on err, quiet when\n"
          + "                               clean, exit 1. THE DEFAULT.\n"
          + "       libra doc FILE ..      the file's HEADER as a document, on stdout\n"
          + "                              markdown by default; -ht for html, -rf for man\n"
          + "  config: ~/.love/etc/libra.l, then ./.libra.l over it -- (singleton 0),\n"
          + "          (deprecated old (worse \"use better\")). doc/misc/libra.md\n")

   ; the config is read once, here, and handed down -- never re-read per file
   (libra-main as)
    (? (member? "-h" as || member? "--help" as)
        (: _ (say err "libra ⚖ -- the .l balance tool.\n") _ (say err usage) 0)
       (: conf (salt 'libra)
          v (? (two? as) <as "")
          r >as
          (? (v = "check") (libra-check conf (files r))
             (v = "doc")   (libra-doc (files r) (? (member? "-ht" r) 'ht
                                                   (member? "-rf" r) 'rf
                                                   'md))
             (v = "")      (: _ (say err usage) (quit 1))     ; nothing named, nothing to weigh
             ; no verb: the whole line is files. so an unknown verb reads as a
             ; filename and says "cannot open" rather than "no such verb"
             (libra-check conf (files as)))))
)

; the verb, and the seat (kiosko's shape): the table makes `love libra ..` a word
; on the artifact, the seat fires libra-main iff this file sits in the program
; seat of the real command line. body-having, so nothing leaks.
(: _ (pins (from 'verbs 'tab) 'libra libra-main)
   s ((from 'verbs 'fire) ["libra" "libra.l"])
   ; quit, never return: the shebang's -l makes this a preload, so a path that
   ; merely answers leaves cli holding our arguments and loads the first as a
   ; program. the seat miss does return -- that arm is (use 'libra).
   (? (two? s) (: _ (libra-main >s) (quit 0)) ()))
) ; module libra
