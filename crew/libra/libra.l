#!/usr/bin/env -S love -l
; libra ⚖ -- the scales: the .l balance tool. it weighs a file two ways and they
; are the same weighing -- `libra` says what is wrong with it, and `libra doc`
; lifts its header out as a document. one scanner (lib/lint.l) under both, so the
; gate and the doc lifter can never disagree about what a paren or a comment is.
;
; both verbs read and print; nothing here writes a file.
;
; the SETTINGS come from lib/salt.l -- ~/.love/etc/libra.l for your taste, then
; ./.libra.l over it for the tree's own. balance is unconditional. singleton is on
; by default -- (singleton 0) turns it off -- and this tree's ./.libra.l makes it
; STRICT: the gate is no gripes, and a gripe is fixed BY HAND, never auto-applied
; -- (f) == f is false for a macro head and for a match pattern, and no scanner
; can know which is which. the other OPINION rules (shadow, the deprecated
; roster) are off until a config asks. see doc/libra.md.
;
; usage:  libra FILE ..      weigh them: balance is the DEFAULT verb, so a bare
;                            file list is the whole command
;         libra doc FILE     its HEADER as a document -- md, -ht html, -rf man
;     or: love -l crew/libra/libra.l ..
;
; check is the default because it is the errand that actually recurs: `make lint`
; runs it over every tracked .l, while doc is run BY HAND.
;
; a FORMATTER lived here until 2026-08-19 -- `fmt`, a reindenter with a `-p` paren
; minifier, and the `infix`/`unfix` respellers. nobody ran them; layout here is a
; hand's business. lint.l's lint-lay survives as a PLAN, for vi's `=` to apply to
; one buffer at a hand's request.
;
; an LSP SERVER lived here until 2026-08-16 -- `libra serve`, a json-rpc lane
; publishing lint's warnings as diagnostics. nobody ran it, so it went; lib/json.l
; stays, and the scan it published is the one `libra check` prints.
(use 'lint)
(use 'salt)

(:
   ; --- lines ----------------------------------------------------------------
   ; the tab warning wants a file split into lines; ltrim is the doc lifter's,
   ; deciding whether what stands left of a comment is blank.
   (ltrim s) (: n (tally s)
                (g i) (? ((i < n) && ((s i = 32) || (s i = 9))) (g (i + 1)) (snip s i n))
                (g 0))
   (linesof s) (: n (tally s)
                  (g i st acc) (? (n <= i) (rev (snip s st n >< acc))
                                  (s i = 10) (g (i + 1) (i + 1) (snip s st i >< acc))
                                  (g (i + 1) st acc))
                  (g 0 0 ()))

   ; --- the check ------------------------------------------------------------
   ; lint's warnings laid out for a terminal: `path:line:col: text`, the shape a
   ; compiler prints and an editor's error list already parses. QUIET when clean;
   ; a tab warns but never fails.
   (tabcol s) (: n (tally s)
                 (g i) (? (n <= i) () (s i = 9) (i + 1) (g (i + 1)))
                 (g 0))
   (tabwarns ls) (: (g l k acc) (? !(two? l) (rev acc)
                                   (: c (tabcol <l)
                                      (g >l (k + 1) (? c (`(k c "tab" 'tab) >< acc) acc))))
                    (g ls 1 ()))
   (warnline path w) (path + ":" + show <w + ":" + show <>w + ": " + <>>w + "\n")

   ; --- the verbs ------------------------------------------------------------
   (rdf p) (: q (open p "r") (? q (: t (slurp q) _ (close q) `(1 t)) ()))

   ; ⚠ WHICH WARNINGS FAIL THE GATE, and which merely speak. an imbalance always
   ; fails -- the file does not read, and that is not a matter of opinion. a tab
   ; never has. the config rules sit with the tab by default: they print, the
   ; editor underlines them, and `libra` still exits 0, so turning a rule on can
   ; never redden a tree that was green. `(strict singleton)` promotes one when a
   ; project has actually finished with it.
   (fatal? conf w) (? (id? <>>>w 'balance) 1 (two? (salt-has? conf 'strict <>>>w)))
   (tally-fatal conf ws) (foldl (\ a w (? (fatal? conf w) (a + 1) a)) 0 ws)

   (libra-check-files conf fs bad)
    (? !(two? fs) bad
       (: r (rdf <fs)
          (? !(two? r) (: _ (say err ("libra: cannot open " + <fs + "\n"))
                           (libra-check-files conf >fs (bad + 1)))
             (: src <>r
                bw (lint-of src conf)
                _ (foldl (\ _ w (: _ (say err (warnline <fs w)) 0)) 0
                         (bw + tabwarns (linesof src)))
                (libra-check-files conf >fs (bad + tally-fatal conf bw))))))

   (libra-check conf fs) (: n (libra-check-files conf fs 0) (? (0 < n) (quit 1) 0))

   (flag? x) ((0 < tally x) && (x 0 = 45))
   (files as) (filter (\ x !(flag? x)) as)

   ; --- the header, as a document --------------------------------------------
   ; twelve of the nineteen crew tools have no doc/*.md at all: their header
   ; comment IS their documentation, 30 to 70 lines of it, and nothing could show
   ; it because nothing could reach it. reading .l is libra's beat, so lifting it
   ; is libra's errand and lives here -- nothing else in the tree has to learn
   ; what a comment is to get a page out of a file.
   ;
   ; ⚠ IT IS A TEXT WALK, and it has to be. no reader in the tree keeps comments
   ; -- `sound` drops them and p0 drops them -- so a header lifted from a datum
   ; would be empty.
   ; lint-cmts is the scanner, one more rider on the .l scan all of libra already
   ; shares, and this is only what to do with what it finds.
   ;
   ; THE HEADER is the run of full-line comments at the top of the file, blank
   ; lines included, ending at the first line that carries CODE. wider than "to
   ; the first blank line" on purpose: papel's own header ends, blanks, and then
   ; goes on for two more paragraphs, and both are documentation. what stands to a
   ; comment's LEFT decides -- blank and the line is a sentence, code and it is a
   ; remark about that code, which is never the header.
   (dblank? s) !(0 < tally (ltrim s))
   ; one leading space off a comment body: `; foo` is "foo" and `;   foo` keeps
   ; the display's two. a `;` alone is the blank line it looks like.
   (dbody s) (: n (tally s) (? ((0 < n) && (s 0 = 32)) (snip s 1 n) s))
   (dcmts src) (: t (tablet 0) _ (each (lint-cmts src) (\ c (pin t <c c))) t)
   (dalone ln c) (: k (<>c - 1)
                    (? (dblank? (snip ln 0 k)) `(1 (snip ln 0 k + dbody <>>c)) ()))
   (drtrim l) (? !(two? l) () (: r (drtrim >l) (? ((two? r) || !(dblank? <l)) (<l >< r) ())))

   ; ⚠ a `#!` line is a SEAT, not a sentence -- `#!/usr/bin/env -S love -l` is the
   ; first line of six of the crew tools and the first line of no documentation.
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

   ; ⚠ AND THE PROSE IS TAKEN AS WRITTEN. the corpus was not composed against a
   ; convention and is not going to be rewritten into one. but it IS consistent,
   ; and three of its habits carry structure that markdown would otherwise fill
   ; flat. each is read as what it plainly is, and nothing else is touched:
   ;
   ;   `; --- name ---`  a SECTION banner -> a level-2 heading. 41 of them in the
   ;                     tree's headers, 40 padded out to the right margin with
   ;                     dashes, which are the padding and not the name.
   ;   `; usage:` and    a DISPLAY -> a fence, because the ALIGNMENT is the
   ;   any indented run  content and filling it into a paragraph destroys it.
   ;                     ⚠ unless the indent is a list item's WRAP, where it is a
   ;                     margin and lapiz already joins the line to the item.
   ;   a leading `⚠`     a HAZARD -> the marker in bold, and no more than that: a
   ;                     real hazard style wants a class, and a new lapiz node is
   ;                     a three-surface obligation nobody has asked for yet.
   ;
   ; ⚠ NOTHING IS SHIELDED HERE ANY MORE. a `--- banner ---` used to stop mdread
   ; dead (`starter` claims the line, no block parses it) and the rest of the
   ; header went with it, so this pass wrapped such lines in a backslash. lapiz's
   ; reader is TOTAL as of 2026-08-16 -- the loss was a defect in the lens, and it
   ; was fixed there rather than papered over here.
   (dind s) (: n (tally s) (g i) (? ((i < n) && ((s i = 32) || (s i = 9))) (g (i + 1)) i) (g 0))
   (ddashy? c) ((c = 45) || (c = 32))
   ; `--- name ------------` -> `(1 "name"), and () for anything else that opens
   ; with three dashes. the dashes on both sides are the marker and the padding.
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
   ; the ⚠ marker in bold. a hazard is a whole paragraph in this corpus and only
   ; its first line wears the glyph, so this is the one place it can be said.
   (dwarn s) (? (dstarts? s "⚠ ") ("**⚠**" + snip s 3 (tally s)) s)
   ; ..and it STARTS one. 52 of the tree's 74 hazard lines already stand off with a
   ; blank line above them and 22 are jammed against the prose before them -- the
   ; author put the glyph at the start of a line either way, which is the whole
   ; signal there is. without this those 22 fill into the paragraph above and the
   ; page carries a wall with two ⚠ in the middle of it.
   (dnl2? s) (: n (tally s) (? (n < 2) (n = 0) ((s (n - 1) = 10) && (s (n - 2) = 10))))
   (dbreak acc s) (? (&& (dstarts? s "⚠ ") !(dnl2? acc)) (acc + "\n") acc)

   ; a display: the run of indented lines at the head of ls, and what is left.
   (dtake ls)
    (: (g l acc) (? (!(two? l) || (dblank? <l) || !(0 < dind <l)) `((rev acc) l)
                    (g >l (<l >< acc)))
       (g ls ()))
   ; ..shown as a fence, dedented by its own LEAST indent: the alignment inside a
   ; display is the content, the margin in front of it is not. (a `usage:` run
   ; starts at column 0, so its own least indent is 0 and nothing moves.)
   (dmin a b) (? (a < b) a b)
   (dfence ls)
    (: k (foldl (\ a s (? (dblank? s) a (dmin a (dind s)))) (tally <ls) ls)
       ("```\n" + foldl (+) "" (map (\ s ((snip s (dmin k (tally s)) (tally s)) + "\n")) ls)
        + "```\n"))

   ; the header as MARKDOWN TEXT, which is the whole of what libra says about a
   ; document: lapiz decides what a document IS, and every surface it has then
   ; arrives free. (a bespoke prose parser here would be a second markdown reader
   ; in the tree, drifting from the first.)
   ;
   ; `li` is "the last line was a list item or its wrap" -- the one piece of state
   ; the walk needs, because an indented line means two different things depending
   ; on it.
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
   ; ⚠ THE VERB THAT KEEPS COMMENTS -- check reads them only to know where code is
   ; not; this prints the comments and nothing else.
   ;
   ; ⚠ THE FILE'S NAME IS THE LEVEL-1 HEADING, and it is the one thing on the page
   ; that is not in the prose. it belongs here rather than in `ldoc-md`, which is
   ; text-to-text and never saw a path -- and it is not decoration: papel reads a
   ; page's title, its anchors and its whole contents nav off the headings, so
   ; without it a generated page is titled by its file name and carries no
   ; navigation at all. on the roff surface it is the `.SH` a man page opens with.
   ;
   ; the surfaces are lapiz's three, and lapiz is loaded HERE rather than at the
   ; top: `make lint` runs libra over every tracked .l and pays for whatever the
   ; file names, and no other verb wants a document lens.
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

   ; ⚠ ABOVE its reader, and not for tidiness: a lambda that closes over a LATER
   ; binding of the same `:` reads the cell before pin when the value is not
   ; itself a lambda (only lambda-to-lambda recursion resolves lazily), and the
   ; read raises `missing usage` at LOAD.
   ; ⚠ an infix CHAIN, not (+ a b c) -- three operands is APPLICATION, ((+ a b) c),
   ; so the third line would be indexed into the first two rather than joined
   usage ("usage: libra [check] FILE ..   balance: path:line:col: on err, quiet when\n"
          + "                               clean, exit 1. THE DEFAULT.\n"
          + "       libra doc FILE ..      the file's HEADER as a document, on stdout\n"
          + "                              markdown by default; -ht for html, -rf for man\n"
          + "  config: ~/.love/etc/libra.l, then ./.libra.l over it -- (singleton 0),\n"
          + "          (deprecated old (worse \"use better\")), (strict singleton). doc/libra.md\n")

   ; ⚠ the config is read ONCE, here, and handed down -- never re-read per file.
   ; `libra` over a whole tree opens two files at the top and no more.
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
             ; NO VERB: the whole line is files, and CHECK is what happens to them.
             ; balance is the everyday errand -- `libra *.l` should simply answer
             ; it -- while every other verb is run by hand and named. ⚠ so an
             ; unknown verb reads as a FILENAME now, and says
             ; "cannot open" rather than "no such verb". that is the right trade
             ; while the file list is the common case, but it is a trade.
             (libra-check conf (files as)))))
)

; the VERB, and the SEAT (kiosko's shape). the verb table is what makes `love libra
; ..` a word on the artifact -- crew/build.mk bakes this file in, so a seed can
; weigh, lay out and document the .l it carries with nothing else on the box. the
; seat fires libra-main iff this file, or the installed `libra` script, sits in the
; program seat of the real command line. body-having, so nothing leaks.
(: _ (pins (from 'verbs 'tab) 'libra libra-main)
   s ((from 'verbs 'fire) ["libra" "libra.l"])
   ; ⚠ QUIT, never return: the shebang's -l makes this file a PRELOAD, so every
   ; path that merely answers -- a clean check, doc, -h -- leaves
   ; cli holding the tool's own arguments and it LOADS the first one as a love
   ; program. a clean `libra f.l` ran f.l. the seat miss still returns: that arm
   ; is (use 'libra), where quitting would take the session down with it.
   (? (two? s) (: _ (libra-main >s) (quit 0)) ()))
