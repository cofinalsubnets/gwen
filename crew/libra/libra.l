#!/usr/bin/env -S love -l
; libra ⚖ -- the scales: the .l balance tool. it weighs a file four ways and they
; are all the same weighing -- `libra` says what is wrong with it, `libra fmt`
; lays it out, `libra infix`/`unfix` turn it inside out, and `libra doc` lifts its
; header out as a document. one scanner (lib/lint.l) under all of them, so the
; formatter, the gate and the doc lifter can never disagree about what a paren or
; a comment is.
;
; the FORMATTER is a REINDENTER: it replaces leading whitespace and leaves every
; line break, every alignment inside a line, and every comment exactly where its
; author put them (moonfmt's posture for C, and the reason a reindent is safe to
; run over a whole tree). it cannot re-flow and will not try. under `-p` it also
; MINIFIES PARENS -- deletes the pairs that do nothing and not one byte besides --
; which is the one edit it makes inside a line, and it is a DELETION, so no comment
; and no break can move. the decision is lint.l's (lint-drops) and it is opfix's
; own: fold the form both ways and compare. ⚠ it is NOT a pretty-printer and must
; not become one by accident -- printing from the datum would strip every comment
; in the file, since `sound` does not keep them.
;
; the SETTINGS come from lib/salt.l -- ~/.love/etc/libra.l for your taste, then
; ./.libra.l over it for the tree's own. balance is unconditional; the rules that
; are matters of OPINION (singleton, the deprecated roster) are off until a config
; asks for them, and even then they only speak. see doc/libra.md.
;
; usage:  libra FILE ..      weigh them: balance is the DEFAULT verb, so a bare
;                            file list is the whole command (`-w` also strips
;                            trailing whitespace, WITHOUT reindenting)
;         libra fmt FILE ..  lay it out (-w rewrites, -n only checks)
;         libra infix FILE   print it max-infix and min-paren  (lib/infix.l)
;         libra unfix FILE   print it back as pure prefix lisp (the opfix pass)
;         libra doc FILE     its HEADER as a document -- md, -ht html, -rf man
;     or: love -l crew/libra/libra.l ..
;
; ⚠ infix and unfix PRINT FROM THE DATUM, so they do not carry comments -- `sound`
; does not keep them. that is exactly why they are verbs of their own and not modes
; of fmt, which is a reindenter and never touches what is on a line. stdout only,
; and no -w: a rewrite that silently deleted every comment in a file is not a thing
; to offer. they are inverses, and the round trip is gated in test/host/infix.l.
; `doc` is the other side of that coin: it carries the comments and nothing else.
;
; check is the default because it is the errand that actually recurs: `make lint`
; runs it over every tracked .l, while fmt and doc are run BY HAND and nothing is
; gated on layout. libra replaced mk/tools/ltidy.l outright (deleted 2026-07-28) --
; including its janitor `-w`, which is why that flag strips whitespace without
; touching indentation.
;
; an LSP SERVER lived here until 2026-08-16 -- `libra serve`, a json-rpc lane
; publishing lint's warnings as diagnostics. nobody ran it, so it went; lib/json.l
; stays, and the scan it published is the one `libra check` prints.
(use 'lint)
(use 'salt)
(use 'infix)

(:
   ; --- the formatter --------------------------------------------------------
   ; lint-lay says where each line STARTS; laying it out is then just "replace the
   ; leading whitespace, keep the rest". every line break, every alignment inside
   ; a line, every comment stays exactly where its author put it -- moonfmt's
   ; posture for C, and the reason a reindent is safe to run over a whole tree.
   (spaces k) (? (k < 1) "" (string 32 + spaces (k - 1)))
   (ltrim s) (: n (tally s)
                (g i) (? ((i < n) && ((s i = 32) || (s i = 9))) (g (i + 1)) (snip s i n))
                (g 0))
   (rtrim s) (: n (tally s)
                (g j) (? ((0 < j) && ((s (j - 1) = 32) || (s (j - 1) = 9))) (g (j - 1)) (snip s 0 j))
                (g n))
   (linesof s) (: n (tally s)
                  (g i st acc) (? (n <= i) (rev (snip s st n >< acc))
                                  (s i = 10) (g (i + 1) (i + 1) (snip s st i >< acc))
                                  (g (i + 1) st acc))
                  (g 0 0 ()))
   (unlines ls) (? !(two? ls) "" (foldl (\ a x (a + "\n" + x)) <ls >ls))

   ; one line against its plan: no plan (inside a string) leaves it ALONE, an
   ; all-blank line empties, anything else takes the planned indent.
   (libra-lay ln pl)
    (? !(two? pl) ln
       (: b (ltrim ln) (? !(0 < tally b) "" (spaces <>pl + rtrim b))))

   ; `-p`, the paren minifier: delete the pairs lint-drops names, and NOTHING else.
   ; lint says where, this applies it -- the same division libra-lay keeps with
   ; lint-lay, and the reason there is no second opinion about what a paren does.
   ;
   ; ⚠ MINIFY FIRST, LAY SECOND, and the order is not a preference: deleting two
   ; bytes moves every column on the line after them, and the reindenter aligns to
   ; first-operand COLUMNS -- a lay computed on the old text would align to parens
   ; that are no longer there. so this answers new SOURCE and libra-fmt runs on it.
   (libra-drop src conf)
    (: ds (lint-drops src conf)
       (? !(two? ds) src
          (: n (tally src)
             (g k l acc)
              (? (n <= k) (string (rev acc))
                 (two? l && (k = <l)) (g (k + 1) >l acc)
                 (g (k + 1) l (src k >< acc)))
             (g 0 ds ()))))

   (libra-fmt src)
    (: ls (linesof src)
       pp (lint-lay src)
       (g l p acc) (? !(two? l) (unlines (rev acc))
                      (g >l (? (two? p) >p ())
                         (libra-lay <l (? (two? p) <p ()) >< acc)))
       (g ls pp ()))

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

   ; `fix` is the janitor pass: strip trailing whitespace, WITHOUT reindenting.
   ; that separation is the point -- a tree can be kept tidy without anyone
   ; agreeing to a layout, which is why this lives on check and not on fmt.
   (libra-check-files conf fs fix bad)
    (? !(two? fs) bad
       (: r (rdf <fs)
          (? !(two? r) (: _ (say err ("libra: cannot open " + <fs + "\n"))
                           (libra-check-files conf >fs fix (bad + 1)))
             (: src <>r
                ls (linesof src)
                bw (lint-of src conf)
                _ (foldl (\ _ w (: _ (say err (warnline <fs w)) 0)) 0
                         (bw + tabwarns ls))
                _ (? fix (: q (open <fs "w") _ (say q (unlines (map rtrim ls))) (close q)) 0)
                (libra-check-files conf >fs fix (bad + tally-fatal conf bw))))))

   (libra-check conf fs fix) (: n (libra-check-files conf fs fix 0) (? (0 < n) (quit 1) 0))

   (flag? x) ((0 < tally x) && (x 0 = 45))
   (files as) (filter (\ x !(flag? x)) as)

   ; fmt: lay each file out on stdout; -w rewrites in place; -n (check) says
   ; nothing and answers the count that WOULD change, which is the gate's shape.
   (libra-fmt-files conf fs mode drop bad)
    (? !(two? fs) bad
       (: r (rdf <fs)
          (? !(two? r) (: _ (say err ("libra: cannot open " + <fs + "\n"))
                           (libra-fmt-files conf >fs mode drop (bad + 1)))
             (: src <>r
                lay (libra-fmt (? drop (libra-drop src conf) src))
                same (lay = src)
                _ (? (id? mode 'write) (? same 0 (: q (open <fs "w") _ (say q lay) (close q)))
                     (id? mode 'check) (? same 0 (say err (<fs + ": would reformat\n")))
                     (say out lay))
                (libra-fmt-files conf >fs mode drop (? same bad (bad + 1)))))))

   ; --- infix and unfix ------------------------------------------------------
   ; the two directions, one datum per line. `infix` sends a file TO the infix
   ; surface (lib/infix.l, the right inverse); `unfix` sends it back to pure lisp,
   ; which is just the `opfix` pass a compile already runs. ⚠ FROM THE DATUM, so
   ; comments do not survive -- stdout only, and see the header for why there is no -w.
   (libra-data src acc) (: r (sound src)
                           (? (two? r) (libra-data >r (link <r acc))
                              (id? r 'torn) 'torn
                              (rev acc)))
   (libra-fix-files fs way bad)
    (? !(two? fs) bad
       (: r (rdf <fs)
          (? !(two? r) (: _ (say err ("libra: cannot open " + <fs + "\n"))
                           (libra-fix-files >fs way (bad + 1)))
             (: ds (libra-data <>r ())
                (? (id? ds 'torn)
                    (: _ (say err (<fs + ": unfinished form -- weigh it first\n"))
                       (libra-fix-files >fs way (bad + 1)))
                   (: _ (each ds (\ x (: _ (say out (? (id? way 'infix) (infix-spell x)
                                                         (show (opfix x))))
                                         (say out "\n"))))
                      (libra-fix-files >fs way bad)))))))
   (libra-fix fs way) (: n (libra-fix-files fs way 0) (? (0 < n) (quit 1) 0))

   ; --- the header, as a document --------------------------------------------
   ; twelve of the nineteen crew tools have no doc/*.md at all: their header
   ; comment IS their documentation, 30 to 70 lines of it, and nothing could show
   ; it because nothing could reach it. reading .l is libra's beat, so lifting it
   ; is libra's errand and lives here -- nothing else in the tree has to learn
   ; what a comment is to get a page out of a file.
   ;
   ; ⚠ IT IS A TEXT WALK, and it has to be. no reader in the tree keeps comments
   ; -- `sound` drops them, p0 drops them, and infix/unfix above are stdout-only
   ; for exactly that reason -- so a header lifted from a datum would be empty.
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
   ; convention and is not going to be rewritten into one: where the header is
   ; markdown already -- and it mostly is, `- ` lists, `code`, indented displays --
   ; markdown is what it means. where it is not, the mapping bends, never the file.
   ;
   ; the bending is one rule today: a line markdown would claim as a block opener
   ; and then refuse to parse wears one backslash, lapiz's own escape, and stands
   ; alone. that is the `; --- banner ---` every crew file rules its sections off
   ; with (and a bare `:   ` body line, markdown's definition marker). WITHOUT IT
   ; mdread STOPS DEAD at the first banner and the whole rest of the header is
   ; dropped in silence -- no error, a shorter page, and nothing to notice it by.
   ; the banner also keeps its own break: it names the section under it, so
   ; letting markdown fill the two into one paragraph would lose a break the file
   ; actually made. rung 1 of doc/plan/doc-system.md turns these into headings.
   ;
   ; ⚠ ONE operand for the body: `(: n .. A || B)` pairs A with `||` and answers B
   ; alone -- the binder takes its names two at a time and reads them BEFORE the
   ; infix fold ever sees them.
   (dhazard? s) (: n (tally s)
                   (((3 <= n) && (snip s 0 3 = "---") && !(dblank? (snip s 3 n)))
                    || ((4 <= n) && (snip s 0 4 = ":   "))))
   (dshield s) (? (dhazard? s) ("\n\\" + s + "\n\n") (s + "\n"))

   ; the header as MARKDOWN TEXT, which is the whole of what libra says about a
   ; document: lapiz decides what a document IS, and every surface it has then
   ; arrives free. (a bespoke prose parser here would be a second markdown reader
   ; in the tree, drifting from the first.)
   (ldoc-md src) (foldl (+) "" (map dshield (ldoc-head src)))

   ; --- doc ------------------------------------------------------------------
   ; ⚠ THE ONE VERB THAT KEEPS COMMENTS -- infix and unfix print from the datum and
   ; lose every one; this prints the comments and nothing else.
   ;
   ; the surfaces are lapiz's three, and lapiz is loaded HERE rather than at the
   ; top: `make lint` runs libra over every tracked .l and pays for whatever the
   ; file names, and no other verb wants a document lens.
   (libra-doc-files fs surf bad)
    (? !(two? fs) bad
       (: r (rdf <fs)
          (? !(two? r) (: _ (say err ("libra: cannot open " + <fs + "\n"))
                           (libra-doc-files >fs surf (bad + 1)))
             (: md (ldoc-md <>r)
                (? !(0 < tally md)
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
   usage ("usage: libra [check] FILE ..   balance: path:line:col: on err, quiet when clean,\n"
          + "                               exit 1; -w also strips trailing whitespace. THE DEFAULT.\n"
          + "       libra fmt FILE ..      lay it out on stdout; -w rewrites, -n only checks (exit 1)\n"
          + "                              -p also MINIFIES PARENS: drops the pairs that do nothing\n"
          + "       libra infix FILE ..    print it MAX-INFIX and MIN-PAREN, on stdout\n"
          + "       libra unfix FILE ..    print it back as PURE PREFIX LISP, on stdout\n"
          + "                              ⚠ both print from the datum: COMMENTS ARE NOT CARRIED\n"
          + "       libra doc FILE ..      the file's HEADER as a document, on stdout\n"
          + "                              markdown by default; -ht for html, -rf for man\n"
          + "  config: ~/.love/etc/libra.l, then ./.libra.l over it -- (singleton 1),\n"
          + "          (deprecated old (worse \"use better\")), (strict singleton),\n"
          + "          (drop-parens 1) for -p by default, (drop-singles 0) / (drop-nullary 1)\n"
          + "          to tune what it drops. doc/libra.md\n")

   ; ⚠ the config is read ONCE, here, and handed down -- never re-read per file.
   ; `libra` over a whole tree opens two files at the top and no more.
   (libra-main as)
    (? (member? "-h" as || member? "--help" as)
        (: _ (say err "libra ⚖ -- the .l balance tool.\n") _ (say err usage) 0)
       (: conf (salt 'libra)
          v (? (two? as) <as "")
          r >as
          (? (v = "check") (libra-check conf (files r) (member? "-w" r))
             ; ⚠ `-p` is a FORMAT option, not a verb of its own: minifying parens
             ; and reindenting are one errand -- make the file read the way the
             ; house writes it -- and they compose in one pass over one file.
             ; the config can stand it up without the flag, for a tree that has
             ; settled the question.
             (v = "fmt")   (: n (libra-fmt-files conf (files r)
                                                 (? (member? "-w" r) 'write
                                                    (member? "-n" r) 'check
                                                    'show)
                                                 ; ⚠ salt-one, never salt-all: the tail
                                                 ; of (drop-parens 0) is two? just as
                                                 ; much as (drop-parens 1)'s is, so
                                                 ; presence would read an OFF as an on.
                                                 (member? "-p" r ||
                                                  salt-one conf 'drop-parens 0)
                                                 0)
                              (? (member? "-n" r && (0 < n)) (quit 1) 0))
             (v = "infix") (libra-fix (files r) 'infix)
             (v = "unfix") (libra-fix (files r) 'lisp)
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
             (libra-check conf (files as) (member? "-w" as)))))
)

; the SEAT (kiosko's shape): fire libra-main iff this file -- or the installed
; `libra` script -- sits in the program seat of the real command line. body-having,
; so nothing leaks.
(: s ((from 'verbs 'fire) ["libra" "libra.l"])
   ; ⚠ QUIT, never return: the shebang's -l makes this file a PRELOAD, so every
   ; path that merely answers -- a clean check, fmt, infix, doc, -h -- leaves
   ; cli holding the tool's own arguments and it LOADS the first one as a love
   ; program. a clean `libra f.l` ran f.l. the seat miss still returns: that arm
   ; is (use 'libra), where quitting would take the session down with it.
   (? (two? s) (: _ (libra-main >s) (quit 0)) ()))
