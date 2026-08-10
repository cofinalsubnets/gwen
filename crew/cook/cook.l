#!/usr/bin/env -S love -l
; cook -- love make
;
; love reads the `#!` hashbang line above as a line comment (love.c: `#!` runs to
; end of line, like `;`), so loading this file as a library is unaffected -- the
; same line serves the OS as a shebang. with the file marked executable and a `cook`
; symlink to it on PATH (the user installs that), running `cook Cookfile [ticket]`
; execs `love -l <this-file> Cookfile [ticket]`: the -l loads cook, then Cookfile runs.
;
; (this tool was first written as tools/bake.l; it was renamed to crew/cook/cook.l so
; the file name does not collide with the OTHER, unrelated sense of "bake" in this
; tree -- baking the heap image / the egg (love/egg.l), the array-perf bake. the
; build VERB here is `cook`; "bake" is reserved for the egg. the messages this tool
; prints are prefixed `cook:` for the same reason.)
;
; an item is brought up to date by first prepping its prerequisites up to date
; (depth first), then -- if the item is missing or OLDER than any prerequisite --
; running its recipe. that is the whole of make's idea; the rest is bookkeeping.
;
; a recipe is registered with (recipe item ingredients steps):
;   item -- a filename STRING (an artifact on disk) or a PHONY SYMBOL ('all,
;             'clean ..). a phony item owns no file, so it has no age and so
;             always runs, cascading a rebuild to everything that needs it.
;   ingredients -- a list of prerequisite items (strings and/or symbols).
;   steps  -- a list of STEPS. each step is either an argv list of strings, run as
;             a subprocess via the host `hark` nif, or a THUNK (any lit?), called as
;             (step 0) for its effect. an empty step list is legal (a pure aggregate).
;
; file ages come from stat(1) -- love has no stat nif, but it has `hark`, and that
; is enough. a missing file and a phony item both read as the ZERO POINT (nil):
; ageless, hence ancient, hence always rebuilt. the FIRST recipe declared is the
; default ticket, exactly as in make. drive it with (cook item) -- to cook an item
; is to prep it up to date by following its recipe over fresh ingredients.
;
; usage:  cook [-f FILE] [--emit] [recipe ..]  (with the symlink installed)
;     or:  love -l crew/cook/cook.l [-f FILE] [recipe ..]
; A Cookfile is ordinary love source that lays out the recipes (recipe ..) then
; drives them: (cook-all 0) builds every recipe named on the command line (or the
; default when none), and (cook (ticket 0)) builds just the first. cook can ALSO
; read a real Makefile directly: ONLY a file NAMED like one (Makefile, makefile,
; GNUmakefile, *.mk -- makefilep) is read as make; every other recipe file (Cards.l,
; Cookfile, or a -f FILE) is love source. See "importing a Makefile" at the foot of
; this file. The first rule is the default ticket either way.

; NO BRACKETS HERE: the runtime supplies the layer. a module consumer says
; (use 'cook) (lib/cook.l) or probes (from 'cook 'recipe); riding a cat (kore's),
; everything binds in the cat's session layer instead -- never orth. before cook
; was a module it leaked 32 globals into the base, `join` `date` `check` `one`
; `span` `pick` among them.

; the combinators (love/peg.l) -- its own top-level form, so the names are spliced
; before anything below compiles against them.
(use 'peg)

(: cards (tablet 0)          ; the recipe cards: item -> (ingredients orderonly . steps)
   check (tablet 0)        ; a 1-cell box: the standing check -- the first item declared (the default when no ticket is named)
   tickets (tablet 0)      ; a 1-cell box: the recipe words named on the command line (the non-flag args), () until the dispatch parses them
   pgen (tablet 0)         ; a 1-cell box: a pattern-rule resolver (item -> a card | 0), installed by the Makefile importer (() until then)

   origins (tablet 0)      ; make's variable provenance: name -> "default" | "environment"
                           ; | "file" | "command line" | "override" ($(origin), and the
                           ; rule that a file assignment never beats a command-line one)
   clivars (tablet 0)      ; a 1-cell box: the command line's VAR=VAL words, ((name . val) ..)

   ; "a b c" from ("a" "b" "c"); "" from the empty list.
   (join sep l) (? (two? l) (foldl (\ a x (a + sep + x)) <l >l) "")

   ; register one card, MERGING with any prior card for the same item: ingredients
   ; and order-only prereqs accumulate, and a later NON-EMPTY recipe wins (as make
   ; does -- e.g. an explicit "prereqs only" rule that adds a dependency to a target
   ; whose recipe comes from a pattern rule). The first item ever registered is the
   ; default ticket. card = (ingredients orderonly . steps); orderonly prereqs are
   ; built-if-missing but never make a target stale (make's `| prereqs`).
   ; ⚠ the merge DEDUPES. a prerequisite named twice is named once (make agrees), and
   ; without that a caller who re-registers the same recipe grows the card without bound:
   ; papel's dev server calls `build` per source change, and its index card climbed 28
   ; ingredients per rebuild -- 2828 after a hundred -- which `prep` then walks every pass.
   ; unbounded memory and quadratic time, from a tool that looked idle.
   (adds a b) (foldl (\ acc x (? (member? x acc) acc (acc + `(x)))) a b)
   ; a Cookfile is written on one seat and read on another, so its paths name the
   ; seat symbolically: a LEADING $HOME or $PWD opens here, at registration. step
   ; strings want none of this -- sh -c expands them itself. a non-string item is
   ; a phony symbol ('all, 'install) and rides through untouched.
   (unpre w tok val)
   (: n (tally tok)
      (? (string? val && (tally w >= n) && (tok = snip w 0 n))
         (val + snip w n (tally w)) w))
   (unseat w) (? (string? w) (unpre (unpre w "$HOME" (getenv "HOME")) "$PWD" (cwd 0)) w)
   (unseatl l) (? (two? l) (map unseat l) l)
   (reg item ing oo steps)
   (: at (unseat item) ins (unseatl ing) ors (unseatl oo)
      prev (peep cards at ())
      (? prev
         (pin cards at (link (adds <prev ins)
                             (link (adds <>prev ors)
                                   (? (two? steps) steps >>prev))))
         (: _ (pin cards at (ins >< (ors >< steps)))
             _ (? (peep check 0 0) 0 (pin check 0 at)) at)))
   ; the public Cookfile API: (recipe item ingredients steps) -- no order-only prereqs.
   (recipe item ingredients steps) (reg item ingredients 0 steps)

   ; the modification time of an item in NANOSECONDS (one charm, the stat nif's
   ; fourth element -- sub-millisecond, so two writes inside one tick still order),
   ; or () (the zero point) when it has none: a phony symbol, or a file that is
   ; not on disk. (this WAS stat(1) through a shell, batch-cached against the ~2N
   ; forks of a no-work pass; the nif is a syscall, so the cache went with the fork.)
   (date item)
   (? (nom? item) () (: st (stat item) (? (two? st) <>>>st ()))))

; --- prepping an item up to date ---------------------------------------------
; one cook call carries two books: `done` memoizes each item's settled age so a
; shared prerequisite is visited once, and `shipped` records the items whose recipe
; actually CHANGED the file this pass (its mtime advanced), so a rebuilt prerequisite
; forces its dependents even when the clock granularity would hide it -- but a recipe
; that re-runs without touching its output (the love_version.h cmp idiom) does NOT
; cascade, matching make's mtime-only decision.

(: (prep item done shipped)
   (? (peep done item ())                                   ; settled already
      (peep done item ())
      ; the card: an explicit recipe wins outright; otherwise a pattern rule may
      ; synthesize one (merging any explicit "prereqs only" card), else the explicit
      ; prereq-only card, else nothing (a source leaf).
      ; presence is tested with `two?` (a card is a chain), never net-truthiness:
      ; a card of all-empty parts -- (() ()) -- nets 0 (blue), so (? card ..) would
      ; wrongly read it absent. A phony aggregate with no prereqs is exactly that.
      (: exr (peep cards item 'none)
         ex (? (two? exr) exr 0)
         f (peep pgen 0 ())
         r (? (ex && two? >>ex) ex
              (: g (? f (f item) 0) (? (two? g) g ex)))
         (? (two? r)
            ; item has a card: prep every prerequisite up to date, then decide.
            (: ingredients <r orderonly <>r steps >>r
               _ (each ingredients (\ d (prep d done shipped)))
               _ (each orderonly (\ d (prep d done shipped)))  ; order-only: built if missing, never makes us stale
               ta0 (date item)
               fresh (any (\ d (peep shipped d 0)) ingredients)   ; a prerequisite's file changed
               older (any (\ d (: da (peep done d ())     ; a prerequisite is newer
                                  (? !da 1 (? !ta0 1 (da > ta0))))) ingredients)
               stale (!ta0 || fresh || older)
               ; run the recipe when stale; mark shipped only if the recipe actually
               ; advanced the file's mtime (created it, or rewrote it newer) -- a
               ; no-op rebuild must not force dependents.
               _ (? stale (: _ (recur item steps)
                             nw (date item)
                             (? (two? steps && (? !ta0 (? nw 1 0) (? nw (nw > ta0) 0)))
                                (pin shipped item 1) 0)) 0)
               age (date item)
               _ (pin done item age)
               age)
            ; no card: a source leaf. it must already exist on disk.
            (: ta (date item)
               _ (? ta 0 (: _ (say err ("cook: no recipe to make " + show item))
                            _ (put err 10) (quit 1)))
               _ (pin done item ta)
               ta))))

   ; run a recipe: each step in order.
   (recur item steps) (each steps (\ step (step1 step)))

   ; one step. a list HEADED BY THE SYMBOL `exec` -- (exec PROG ARGS..) -- is an
   ; INTERACTIVE step: it runs via the `exec` nif, which REPLACES cook and inherits
   ; the terminal, so the child (the repl, gdb, qemu) drives the real tty. A plain
   ; list of strings is an argv subprocess -- streamed, but on a pipe with /dev/null
   ; stdin, so it still cannot be interactive; any other lit? is a thunk.
   (step1 step)
   (? (two? step && (<step = 'exec)) (do-exec >step)     ; (exec ..) -- the interactive step
      (two? step) (shell step)
      (lit? step) (step 0)
      0)

   ; an interactive step: exec the argv, replacing this process (tty inherited).
   ; the exec nif returns only on FAILURE -- then an errno fixnum -- so anything
   ; past it is the error path. exec is terminal by nature (the repl never returns),
   ; so cook's "cooked X" summary is correctly never reached.
   (do-exec argv)
   (: _ (flush out)
       _ (exec argv)
       _ (say err ("cook: cannot exec " + join " " argv)) _ (put err 10) (quit 1))

   ; run one argv: echo it, run it STREAMING its output (herald relays each chunk as
   ; it arrives, so a long step is not silent until it exits), abort on failure.
   ; The flush is load-bearing: herald writes the child's bytes past the `out`
   ; buffer, so an unflushed echo would land after them.
   (shell step)
   (: _ (puts ("  " + join " " step)) _ (putc 10)
       _ (flush out)
       r (herald step)
       (? (two? r)
          (? (0 = <r) 0
             (: _ (say err ("cook: command failed (exit " + show <r + ")"))
                _ (put err 10) (quit 1)))
          (: _ (say err ("cook: cannot run " + join " " step))
             _ (put err 10) (quit 1))))

   ; run one make recipe line (via SHELL -c, default sh), make-style: echo the
   ; RAW command (the expanded line, not the shell wrapper) unless @ silent, STREAM its
   ; stdout as it is produced, and on a nonzero exit either carry on (- ignore,
   ; with a note to err) or abort. The Makefile path's analogue of `shell`, and
   ; the one that carries a recursive $(MAKE) -- the whole reason it must stream.
   (sh-line silent ignore sh flags cmd)
   (: _ (? silent 0 (: _ (puts cmd) (putc 10)))
       _ (flush out)
       r (herald ((`(sh) + flags) + `(cmd)))
       ec (? (two? r) <r -1)
       (? (0 = ec) 0
          !(two? r) (: _ (say err ("cook: cannot run " + cmd)) _ (put err 10) (quit 1))
          ignore (: _ (say err ("cook: " + cmd + " (exit " + show ec + ", ignored)")) (put err 10))
          (: _ (say err ("cook: command failed (exit " + show ec + ")")) _ (put err 10) (quit 1)))))

; --- the entry points ----------------------------------------------------------

; prep item up to date, QUIETLY, and answer the tablet of items whose recipe actually
; advanced their file. `cook` prints a verdict off it; a caller that wants the number
; takes `$` of it (papel's build does).
(: (prepped item)
   (: shipped (tablet 0) _ (prep item (tablet 0) shipped) shipped)

   (cook item)                                ; prep item up to date and report
   (? !item (: _ (say err "cook: nothing to cook") _ (put err 10) (quit 1))
      (: shipped (prepped item)
         ; $ of a map is its key count: how many items actually ran a recipe.
         (? (0 < $shipped)
            (: _ (puts ("cook: cooked " + show item)) (putc 10))
            (: _ (puts ("cook: " + show item + " is already up to date")) (putc 10)))))

   ; resolve a command-line word to an item: the bare string when it names a file
   ; recipe, the interned symbol when it names a phony one, else the DEFAULT -- the
   ; first recipe declared. an unknown word thus quietly cooks the default, as make's
   ; trailing target does when nothing matches.
   (pick a) (? (peep cards a 0) a
              (: s (intern a) (? (peep cards s 0) s (peep check 0 ()))))

   ; the ticket to build: the FIRST recipe word named on the command line (the
   ; `tickets` box, filled by the dispatch from the non-flag args), else the default.
   ; (ticket 0) reads it live, so a Cookfile's own (cook (ticket 0)) sees the args.
   (ticket _) (: ts (peep tickets 0 ()) (? (two? ts) (pick <ts) (peep check 0 ())))

   ; cook every recipe word named on the command line (make foo bar), or the default
   ; when none were named. drives the Makefile path; a Cookfile drives itself.
   (cook-all _) (: ts (peep tickets 0 ())
                (? (two? ts) (each ts (\ w (cook (pick w)))) (cook (peep check 0 ())))))

; --- importing a Makefile ------------------------------------------------------
; cook reads a Cookfile (ordinary love source) -- but it can also read a real
; Makefile directly, the same dependency idea wearing make's notation. The reader
; is built on the PARSER COMBINATORS (after the gw.js style) now shared from the
; love/peg.l: a parser is (\ s y n) over a string s, with a success
; continuation y = rest parsed and a failure continuation n = rest; the parsed
; payload is a list that `seq` concatenates. love has no regex, so the leaves are
; `lit` (a literal prefix), `one` (one char by predicate) and `span` (a maximal
; run); alt/seq/opt/many build the rest. The import is reasonably GNU-make-
; compatible: recursive $(VAR) expansion and substitution refs, the common
; functions ($(shell)/$(wildcard)/$(dir)/$(patsubst)/$(filter)/...), ifeq/ifdef
; conditionals, `include`, the = := ?= += flavors, pattern + static-pattern rules,
; order-only prereqs, and the $@ $< $^ $* automatics. Each recipe LINE runs via
; ("sh" "-c" line). See crew/cook/cooktest.l.
;
; $(wildcard) prefers PURE LOVE: lush's glob (crew/lush/glob.l, the mask law)
; over readdir/stat -- no /bin/sh underneath, so `kore make` globs before a
; shell exists. riding a cat that carries the lush parts (kore's), the name is
; already aboard; a standalone -l load splices lib/lush/glob.l off the cwd --
; BEFORE the importer below compiles, so its bare sh-glob read folds (the
; capture law). both probes ride the book: a bare read of an unbound nom
; scares, and a use miss is fatal in file mode. where neither lands (a dev
; binary on a foreign cwd -- no lib/ to walk), globw below keeps the POSIX
; shell-out lane. the verdict pins HERE, before the importer compiles: once a
; form reading sh-glob bare has compiled, the nom shows on the book (its
; missing cell) and a late (names ()) probe would lie.
(? (member? 'sh-glob (names ())) 0
   (two? (stat "lib/lush/glob.l")) (use 'lush/glob)
   0)
; cook-glob: the pathname glob, a CLOSURE when lush's glob is aboard, () when
; not (the importer then keeps its POSIX shell-out lane). built through ev so
; sh-glob is never read bare in a session without it -- even a dead branch's
; compile would scare on the unbound nom.
(: cook-glob (? (member? 'sh-glob (names ()))
                (ev '(\ pat (sh-glob pat (sh-ones (tally pat)))))
                ()))

; The whole importer lives in ONE body-having `:`, so none of its many helper
; names leak to the global scope. The form reads outer globals (the peg
; combinators, recipe, cook, ticket, the prel) freely; its body ANSWERS cook-main
; (args -> the whole job, exiting via quit). The outer `_`-holder form (cli.l's own
; trick) leaks cook-main as the ONE public -- the multi-call `kore` (crew/kore/kore.l)
; calls it as `make` -- and the SEAT at the bottom fires it only when this very
; file was invoked (the installed `cook` symlink, a -l preload, or positional).
(: _ (:
   ; --emit mode: a 1-cell box (0 = off). when set, expand-steps emits a
   ; serializable ("sh" "-c" cmd) argv step instead of a sh-line thunk, so the
   ; resolved cards can be SHOWN back as a Cookfile (see emit-cookfile below).
   emitp (tablet 0)

   ; -- the combinators (eps/lit/one/span/alt/seq/seqs/opt/skip/pmap/many/parse) come from love/peg.l,
   ;    spliced by the (use 'peg) above; this body-having `:` reads them bare down the walk.

   ; -- character classes and string utilities --
   (chws c)     ((c = 32) || (c = 9))
   (digitp c)   ((48 <= c) && (c <= 57))
   (alphap c)   (((65 <= c) && (c <= 90)) || ((97 <= c) && (c <= 122)))
   (namech c)   (alphap c || digitp c || (c = 95) || (c = 46))
   ; notcolon ADMITS whitespace: a rule's target text runs to the first colon,
   ; and `a.out b.out: src` is one rule with two targets -- excluding blanks here
   ; made every multi-target rule classify as 'skip, SILENTLY, and the built
   ; tree masked it (an existing output is a source leaf). assignP runs first,
   ; so assignment lines never reach this span.
   (notcolon c) !(c = 58)
   (anyc c)     1
   (ltrim s) (: i ((: (g j) (? ((j < tally s) && chws (s j)) (g (1 + j)) j)) 0)
               (snip s i (tally s)))
   (rtrim s) (: i ((: (g j) (? ((0 < j) && chws (s (j - 1))) (g (j - 1)) j)) (tally s))
               (snip s 0 i))
   (trim s)  (rtrim (ltrim s))
   (findc s i ch) (? (i >= tally s) i (? (s i = ch) i (findc s (1 + i) ch)))
   (split-ws s)
   ((: (g i st acc)
       (? (i >= tally s) (rev (? (st < i) (snip s st i >< acc) acc))
          (? (chws (s i))
             (g (1 + i) (1 + i) (? (st < i) (snip s st i >< acc) acc))
             (g (1 + i) st acc))))
    0 0 0)

   ; -- a Makefile is read line by line; first fold \<newline> continuations --
   (uncont s)
   ((: (g i acc)
       (? (i >= tally s) acc
          (? ((s i = 92) && ((1 + i) < tally s) && (s (1 + i) = 10))
             (g (2 + i) (acc + 32)) (g (1 + i) (acc + s i)))))
    0 "")
   (lines s)
   ((: (g i st acc)
       (? (i >= tally s) (rev (snip s st i >< acc))
          (? (s i = 10) (g (1 + i) (1 + i) (snip s st i >< acc))
             (g (1 + i) st acc))))
    0 0 0)

   ; -- string utilities for the make layer --
   (findc-ws s i) (? (i >= tally s) i (? (chws (s i)) i (findc-ws s (1 + i))))
   (first-word s) (snip s 0 (findc-ws s 0))
   (after-word s) (ltrim (snip s (findc-ws s 0) (tally s)))
   (nth l i d) (? (two? l) (? (0 = i) <l (nth >l (i - 1) d)) d)
   (lastl l) (? (two? >l) (lastl >l) <l)
   ; an empty snip (start>=end) yields the zero point 0, not "" -- and (+ 0 str)
   ; prepends a NUL byte (the byte law). `sub` is the empty-safe snip: "" for empty.
   (sub s a b) (? (a < b) (snip s a b) "")
   ; strip a make comment (an unescaped #) from a NON-recipe line; \# is a literal #.
   (strip-comment l)
   ((: (g i acc)
       (? (i >= tally l) acc
          (: c (l i)
             (? (c = 35) acc                                  ; 35 = #
                (c = 92) (? ((1 + i) < tally l)               ; 92 = backslash
                            (: d (l (1 + i)) (? (d = 35) (g (2 + i) (acc + 35)) (g (2 + i) (acc + 92 + d))))
                            (acc + 92))
                (g (1 + i) (acc + c))))))
    0 "")
   ; split a string on a top-level char `ch` -- parens/braces are balanced, so a
   ; delimiter inside $(..) / ${..} does not split (used for function args and the
   ; static-pattern colon).
   (splittop s ch)
   ((: (g i depth st acc)
       (? (i >= tally s) (rev (snip s st i >< acc))
          (: c (s i)
             (? ((c = 40) || (c = 123)) (g (1 + i) (1 + depth) st acc)
                ((c = 41) || (c = 125)) (g (1 + i) (depth - 1) st acc)
                ((c = ch) && (depth = 0)) (g (1 + i) depth (1 + i) (snip s st i >< acc))
                (g (1 + i) depth st acc)))))
    0 0 0 0)

   ; -- the line grammar: NAME OP VALUE (op kept) and a rule header (raw text) --
   opP (alt (lit "::=") (alt (lit ":=") (alt (lit "?=") (alt (lit "+=") (lit "=")))))
   assignP (seqs `((skip (span chws)) (span namech)
                    (skip (span chws)) (pmap (\ x `(<x)) opP)
                    (skip (span chws)) (span anyc)))
   ruleP (seqs `((skip (span chws)) (span notcolon) (skip (lit ":")) (span anyc)))

   ; classify a line -> (recipe TEXT) | (assign NAME OP VAL) | (rule TGTS PREREQS)
   ; | (inc TEXT) | (ifeq|ifneq|ifdef|ifndef|else REST) | (endif) | (skip).
   (recipep l)  ((0 < tally l) && (l 0 = 9))
   ; parse a recipe body: strip a leading run of @ - + line modifiers (and any
   ; interspersed whitespace), RECORDING whether @ (silent) and - (ignore-error)
   ; appeared. -> (silent ignore . body). + (force) is a no-op here -- cook has no
   ; dry-run mode -- so it is stripped without a flag. (64 = @, 45 = -, 43 = +.)
   (parse-mods s)
   ((: (g i sil ign)
       (? ((i < tally s) && (chws (s i) || (s i = 64) || (s i = 45) || (s i = 43)))
          (g (1 + i) (sil || (s i = 64)) (ign || (s i = 45)))
          (sil >< (ign >< snip s i (tally s))))) 0 0 0)
   (recipe-parse l) (parse-mods (snip l 1 (tally l)))   ; drop the leading tab, then the modifier run
   (classify l)
   (? (recipep l) `('recipe (recipe-parse l))
      (: t (strip-comment l) tt (trim t)
         (? (0 = tally tt) '(skip)
            (: w (first-word tt) rest (after-word tt)
               (? (w = "include") `('inc rest)
                  (w = "-include") `('inc rest)
                  (w = "sinclude") `('inc rest)
                  ; `override NAME op VAL` assigns past a command-line VAR=VAL;
                  ; `export NAME [op VAL]` assigns (when an op is there) and hands the
                  ; final value to the process environment, so recipes inherit it.
                  (w = "override") (: a (parse assignP rest) (? (two? a) ('oassign >< a) '(skip)))
                  (w = "export") (: a (parse assignP rest)
                                    (? (two? a) ('xassign >< a) `('xname (trim rest))))
                  (w = "ifeq") `('ifeq rest)
                  (w = "ifneq") `('ifneq rest)
                  (w = "ifdef") `('ifdef rest)
                  (w = "ifndef") `('ifndef rest)
                  (w = "else") `('else rest)
                  (w = "endif") '(endif)
                  (: a (parse assignP t) (? (two? a) ('assign >< a)
                     (: r (parse ruleP t) (? (two? r) ('rule >< r) '(skip))))))))))

   ; -- nested-paren-aware variable expansion + make functions --
   ; the index of the ) that matches the ( just opened, counting nested $(..).
   (findclose s i open close)
   ((: (g j depth)
       (? (j >= tally s) j
          (: c (s j)
             (? (c = open) (g (1 + j) (1 + depth))
                (c = close) (? (0 = depth) j (g (1 + j) (depth - 1)))
                (g (1 + j) depth)))))
    i 0)

   ; GNU make substitution on one word of $(VAR:from=to). With a % in `from` it is
   ; a pattern (prefix%suffix) whose stem fills the % in `to`; without a % it is a
   ; trailing-text replacement (the .o=.c shorthand). Non-match passes through.
   (patsub frm to w)
   (: pi (findc frm 0 37)                                    ; 37 = %
      (? (pi < tally frm)
         (: pre (sub frm 0 pi) suf (sub frm (1 + pi) (tally frm))
            lp (tally pre) ls (tally suf) lw (tally w)
            (? ((lw >= (lp + ls)) && (pre = sub w 0 lp) && (suf = sub w (lw - ls) lw))
               (: stem (sub w lp (lw - ls)) ti (findc to 0 37)
                  (? (ti < tally to) (sub to 0 ti + stem + sub to (1 + ti) (tally to)) to))
               w))
         (: lf (tally frm) lw (tally w)
            (? ((lw >= lf) && (frm = sub w (lw - lf) lw)) (sub w 0 (lw - lf) + to) w))))

   ; substitute the stem into a pattern prereq (./%.c -> ./<stem>.c); no %, no change.
   (stemsub p stem) (: pi (findc p 0 37)
                       (? (pi < tally p) (sub p 0 pi + stem + sub p (1 + pi) (tally p)) p))
   ; match a pattern (one %) against a string -> (stem) | () ; no % -> exact match.
   (pmatch pat s)
   (: pi (findc pat 0 37)
      (? (pi >= tally pat) (? (pat = s) '("") ())
         (: pre (snip pat 0 pi) suf (snip pat (1 + pi) (tally pat))
            lp (tally pre) ls (tally suf) ls2 (tally s)
            (? ((ls2 >= (lp + ls)) && (pre = snip s 0 lp) && (suf = snip s (ls2 - ls) ls2))
               `((snip s lp (ls2 - ls))) ()))))
   (haspct t) (findc t 0 37 < tally t)
   (matchany pats w) (any (\ p (two? (pmatch p w))) pats)

   (subst-ref eb as d)         ; VAR:from=to (already inner-expanded)
   (: ci (findc eb 0 58) nm (sub eb 0 ci)                    ; 58 = :
      spec (sub eb (1 + ci) (tally eb))
      ei (findc spec 0 61) frm (sub spec 0 ei)               ; 61 = =
      to (sub spec (1 + ei) (tally spec))
      v (expand (peep as nm "") as (1 + d))
      (join " " (map (\ w (patsub frm to w)) (split-ws v))))

   ; -- function helpers --
   (chomp s) ((: (g i) (? ((0 < i) && (s (i - 1) = 10)) (g (i - 1)) (snip s 0 i))) (tally s))
   (nl->sp s) ((: (g i acc) (? (i >= tally s) acc (g (1 + i) (acc + (? (s i = 10) 32 (s i)))))) 0 "")
   (fn-shell sh cmd) (: r (hark `(sh "-c" cmd)) (? (two? r) (nl->sp (chomp >r)) ""))
   ; $(wildcard), native both ways: lush's glob (cook-glob -- classes, multi-
   ; segment patterns, the mask law) when it is aboard; else glob1 below -- a
   ; word with no * or ? is an existence test; otherwise the
   ; LAST path segment is the pattern (the tree's globs are all leaf-shaped), matched
   ; over readdir -- * a run, ? one char, a leading . only by an explicit . -- and the
   ; matches SORTED under their directory prefix (glob(3) order, what sh gave before).
   (globch s) ((: (g i) (? (i >= tally s) 0 ((s i = 42) || (s i = 63) || g (1 + i)))) 0)
   (gmatch p s)
   ((: (m i j)
       (? (i >= tally p) (j >= tally s)
          (p i = 42) (m (1 + i) j || ((j < tally s) && m i (1 + j)))
          (j >= tally s) 0
          ((p i = 63) || (p i = s j)) (m (1 + i) (1 + j))
          0))
    0 0)
   (glob1 pat)
   (? !(globch pat) (? (exists? pat) `(pat) 0)
      (: k (mk-lastslash pat)
         dir (? (0 < k) (snip pat 0 (? (1 < k) (k - 1) k)) ".")
         pfx (sub pat 0 k)
         leaf (snip pat k (tally pat))
         hid ((0 < tally leaf) && (leaf 0 = 46))
         ms (filter (\ e ((hid || !(e 0 = 46)) && gmatch leaf e))
                    (: es (readdir dir) (? (two? es) es ())))
         (map (\ e (? (0 < tally pfx) (pfx + e) e)) (sort ms))))
   (globw pat) (? (lit? cook-glob) (cook-glob pat) (glob1 pat))
   (fdir w) (: k (mk-lastslash w) (? (0 < k) (sub w 0 k) "./"))
   (fnotdir w) (sub w (mk-lastslash w) (tally w))
   (flastdot w) (: sl (mk-lastslash w)
                   ((: (g j) (? (j > sl) (? (w (j - 1) = 46) (j - 1) (g (j - 1))) (tally w))) (tally w)))
   (fbase w) (: dot (flastdot w) (? (dot < tally w) (sub w 0 dot) w))
   (fsuf w)  (: dot (flastdot w) (? (dot < tally w) (sub w dot (tally w)) ""))
   ; make's $(abspath): rooted on the cwd, then LEXICALLY cleaned -- no repeated
   ; separators, no . or .. components (.. pops a segment; no filesystem asked).
   (segs s) ((: (g i st acc) (? (i >= tally s) (rev (? (st < i) (snip s st i >< acc) acc))
                                (? (s i = 47) (g (1 + i) (1 + i) (? (st < i) (snip s st i >< acc) acc))
                                   (g (1 + i) st acc)))) 0 0 0)
   (fabs w)
   (: r (? ((0 < tally w) && (w 0 = 47)) w
           (: p (cwd 0) (? (string? p) (p + "/" + w) w)))
      cl ((: (g l acc) (? (two? l)
                          (g >l (? (<l = ".") acc
                                   (<l = "..") (? (two? acc) >acc acc)
                                   (<l >< acc)))
                          (rev acc)))
          (segs r) 0)
      ("/" + join "/" cl))
   (dedup l) (? (two? l) (? (two? >l) (? (<l = <>l) (dedup >l) (<l >< dedup >l)) l) l)
   (sortu l) (dedup (sort l))
   (strrep frm to s)
   (? (0 = tally frm) s
      ((: (g i acc)
          (? ((i + tally frm) > tally s) (acc + sub s i (tally s))
             (? (frm = sub s i (i + tally frm)) (g (i + tally frm) (acc + to))
                (g (1 + i) (acc + s i)))))
       0 ""))
   (fn-or args as d)  ((: (g l) (? (two? l) (: v (expand <l as (1 + d)) (? (0 < tally (trim v)) v (g >l))) "")) args)
   (fn-and args as d) ((: (g l last) (? (two? l) (: v (expand <l as (1 + d)) (? (0 < tally (trim v)) (g >l v) "")) last)) args "")
   funcnames '("shell" "wildcard" "dir" "notdir" "basename" "suffix" "patsubst"
               "subst" "filter" "filter-out" "sort" "strip" "if" "or" "and"
               "addprefix" "addsuffix" "firstword" "lastword" "word" "words"
               "abspath" "realpath" "call" "origin")
   ; $(call fn,a,..): bind $0 (the name) and $1.. to the EXPANDED args, expand
   ; fn's body, put the old bindings back (a shadowed outer $1 survives).
   (fn-call args as d)
   (: fname (trim (nth args 0 ""))
      body (peep as fname "")
      n ((: (g l i) (? (two? l) (g >l (1 + i)) i)) args 0)
      olds ((: (g i acc) (? (i < n) (g (1 + i) ((show i >< peep as (show i) 'cook.undef) >< acc)) acc)) 0 0)
      _ ((: (g i l) (? (two? l) (: _ (pin as (show i) <l) (g (1 + i) >l)) 0)) 0 args)
      r (expand body as (1 + d))
      _ (each olds (\ pr (? (nom? >pr) (pull as <pr 0) (pin as <pr >pr))))
      r)
   (funcp nm) (any (\ f (f = nm)) funcnames)
   (findws s) ((: (g i) (? (i >= tally s) i (? (chws (s i)) i (g (1 + i))))) 0)
   (apply-func nm at as d)
   (: ex1 (\ x (expand x as (1 + d)))
      argl (\ _ (map ex1 (splittop at 44)))     ; comma-split + expand (most funcs); 44 = ,
      rawl (\ _ (splittop at 44))               ; comma-split, NOT expanded (if/or/and)
      (? (nm = "shell")     (fn-shell (shname as) (ex1 at))
         (nm = "wildcard")  (join " " (catmap globw (split-ws (ex1 at))))
         (nm = "dir")       (join " " (map fdir (split-ws (ex1 at))))
         (nm = "notdir")    (join " " (map fnotdir (split-ws (ex1 at))))
         (nm = "basename")  (join " " (map fbase (split-ws (ex1 at))))
         (nm = "suffix")    (join " " (map fsuf (split-ws (ex1 at))))
         (nm = "strip")     (join " " (split-ws (ex1 at)))
         (nm = "sort")      (join " " (sortu (split-ws (ex1 at))))
         (nm = "patsubst")  (: a (argl 0) (join " " (map (\ w (patsub (nth a 0 "") (nth a 1 "") w)) (split-ws (nth a 2 "")))))
         (nm = "subst")     (: a (argl 0) (strrep (nth a 0 "") (nth a 1 "") (nth a 2 "")))
         (nm = "filter")    (: a (argl 0) (join " " (filter (\ w (matchany (split-ws (nth a 0 "")) w)) (split-ws (nth a 1 "")))))
         (nm = "filter-out")(: a (argl 0) (join " " (filter (\ w !(matchany (split-ws (nth a 0 "")) w)) (split-ws (nth a 1 "")))))
         (nm = "addprefix") (: a (argl 0) (join " " (map (\ w (nth a 0 "" + w)) (split-ws (nth a 1 "")))))
         (nm = "addsuffix") (: a (argl 0) (join " " (map (\ w (w + nth a 0 "")) (split-ws (nth a 1 "")))))
         (nm = "firstword") (: ws (split-ws (ex1 at)) (? (two? ws) <ws ""))
         (nm = "lastword")  (: ws (split-ws (ex1 at)) (? (two? ws) (lastl ws) ""))
         (nm = "word")      (: a (argl 0) ws (split-ws (nth a 1 "")) n (: r (sound (trim (nth a 0 ""))) (? (two? r) <r 0)) (nth ws (n - 1) ""))
         (nm = "words")     (show (tally (split-ws (ex1 at))))
         (nm = "if")        (: a (rawl 0) c (trim (ex1 (nth a 0 ""))) (? (0 < tally c) (ex1 (nth a 1 "")) (ex1 (nth a 2 ""))))
         (nm = "or")        (fn-or (rawl 0) as d)
         (nm = "and")       (fn-and (rawl 0) as d)
         (nm = "call")      (fn-call (argl 0) as d)
         (nm = "origin")    (peep origins (trim (ex1 at)) "undefined")
         ((nm = "abspath") || (nm = "realpath")) (join " " (map fabs (split-ws (ex1 at))))
         ""))                                   ; unknown -> empty

   ; a group body is a function call ($(name args)), a substitution reference
   ; ($(VAR:from=to)), or a plain variable ($(VAR)).
   (eval-group body as d)
   (: sp (findws body)
      (? ((sp < tally body) && funcp (snip body 0 sp))
         (apply-func (snip body 0 sp) (ltrim (snip body (1 + sp) (tally body))) as d)
         (: eb (expand body as (1 + d)) ci (findc eb 0 58)
            (? (ci < tally eb) (subst-ref eb as d) (expand (peep as eb "") as (1 + d))))))

   ; expansion: $(..) / ${..} groups (functions / subst refs / vars), the single-char
   ; $V form (including the automatic vars $@ $< $^ $*, which live in the table during
   ; recipe expansion), and $$ -> $.
   (expand s as d)
   (? (d > 100) s
      ((: (g i acc)
          (? (i >= tally s) acc
             (? ((s i = 36) && ((1 + i) < tally s))
                (: nx (s (1 + i))
                   (? ((nx = 40) || (nx = 123))
                      (: close (? (nx = 40) 41 125) j (findclose s (2 + i) nx close)
                         (g (1 + j) (acc + eval-group (snip s (2 + i) j) as d)))
                      (nx = 36) (g (2 + i) (acc + 36))
                      (namech nx || (nx = 64) || (nx = 60) || (nx = 94) || (nx = 42))
                      (g (2 + i) (acc + expand (peep as (snip s (1 + i) (2 + i)) "") as (1 + d)))
                      (g (1 + i) (acc + s i))))
                (g (1 + i) (acc + s i)))))
       0 ""))
   (expand-words ws as) (catmap (\ w (split-ws (expand w as 0))) ws)
   ; a recipe LINE that begins with the shell `exec` builtin is an INTERACTIVE
   ; line: `exec <cmd>` replaces the shell, so make already runs it on the real
   ; tty -- and cook honours the same convention, running it through the exec nif
   ; (the `(exec ..)` step) rather than capturing it via `hark`. So one notation --
   ; `exec` -- marks the repl / gdb / qemu targets interactive in BOTH builders.
   (starts? s pre) ((tally pre <= tally s) && (pre = snip s 0 (tally pre)))
   (execp cmd) (: t (ltrim cmd)
                  (starts? t "exec" && ((4 = tally t) || chws (t 4))))
   ; build a recipe's argv steps, pinning the automatic vars into the table first.
   ; build a card's steps. each raw is (silent ignore . body) from recipe-parse;
   ; expand the body NOW (the $@/$</$^/$* automatics are live in `as` at this
   ; point, and get re-pinned per item) and close over the expanded command in a
   ; thunk that runs it make-style via sh-line, carrying the @/- flags. An
   ; exec-prefixed line instead becomes an (exec "sh" "-c" cmd) interactive step
   ; (serialized as such under --emit, run via the exec nif otherwise).
   ; the shell a recipe line rides. .SHELLFLAGS is a plain make VARIABLE, so it is
   ; already in `as` (the .DEFAULT_GOAL precedent); its default is make's own -c.
   (shflags as) (: f (trim (peep as ".SHELLFLAGS" ""))
                   (? (0 < tally f) (split-ws f) '("-c")))
   ; $(@D): the target's directory sans trailing slash ("." for a bare name), $(@F)
   ; its basename -- make's D/F faces, pinned beside the plain automatics.
   (dslash w) (: k (mk-lastslash w) (? (0 < k) (snip w 0 (? (1 < k) (k - 1) k)) "."))
   ; ..and SHELL picks the shell itself (a Makefile saying SHELL := lush runs
   ; its recipes -- and its $(shell) calls -- under lush), default sh.
   (shname as) (: s (trim (peep as "SHELL" "")) (? (0 < tally s) s "sh"))
   (expand-steps raws t pr stem as)
   (: _ (pins as "@" t "<" (? (two? pr) <pr "") "^" (join " " pr) "*" stem)
       _ (pins as "@D" (? (nom? t) "." (dslash t)) "@F" (? (nom? t) (show t) (fnotdir t)))
       fl (shflags as)
       sn (shname as)
       ; make reads the @/-/+ modifiers AFTER expansion too (the $(Q)-as-@
       ; verbose-build idiom; mk/lib.mk's lcat_h leads with @ from inside the
       ; variable) -- so re-strip the EXPANDED line and fold its flags in.
       cmds (map (\ raw (>>(parse-mods (expand >>raw as 0)))) raws)
       ; .ONESHELL: the WHOLE recipe rides ONE shell, so a recipe reads as a script
       ; instead of a run of \-joined lines. make's rule -- the @/- modifiers of the
       ; FIRST line govern the lot. ⚠ with plain -c only the LAST line's status is
       ; checked, so a failing middle line passes SILENTLY; a oneshell Makefile wants
       ; .SHELLFLAGS := -ec, and that is the shape this tree's own Makefile sets.
       ; An `exec` line still wants a process of its own (it REPLACES cook to inherit
       ; the tty), so a recipe carrying one stays per-line.
       one? ((0 < tally (peep as ".ONESHELL" "")) &&
             two? raws && !(any execp cmds))
       (? one?
          (: r0 <raws
             (? (peep emitp 0 0) `(((`(sn) + fl) + `((join "\n" cmds))))
                `((\ _ (sh-line <r0 <>r0 sn fl (join "\n" cmds))))))
          (map (\ raw (: e (parse-mods (expand >>raw as 0))
                          sil (<raw || <e) ign (<>raw || <>e) cmd >>e
                          (? (execp cmd)
                             (? (peep emitp 0 0) ('exec >< `(sn "-c" cmd))
                                (\ _ (do-exec `(sn "-c" cmd))))
                             (peep emitp 0 0) ((`(sn) + fl) + `(cmd))
                             (\ _ (sh-line sil ign sn fl cmd))))) raws)))

   ; -- conditionals & assignment ops over the variable table `as` --
   (unquote-str s) (? ((tally s >= 2) && ((s 0 = 34) || (s 0 = 39))) (snip s 1 (tally s - 1)) s)
   (cond-args rest)            ; ifeq/ifneq REST -> (arg1 . arg2)
   (: t (trim rest)
      (? ((0 < tally t) && (t 0 = 40))
         (: cl (findclose t 1 40 41) parts (splittop (snip t 1 cl) 44)
            ((? (two? parts) <parts "") >< (? (two? >parts) <>parts "")))
         (: ws (map unquote-str (split-ws t))
            ((? (two? ws) <ws "") >< (? (two? >ws) <>ws "")))))
   (eval-ifeq rest as) (: a (cond-args rest) (trim (expand <a as 0) = trim (expand >a as 0)))
   (eval-ifdef rest as) (0 < tally (expand (peep as (trim (expand rest as 0)) "") as 0))
   ; ov nonzero is the `override` directive: the one assignment that beats a
   ; command-line VAR=VAL (which otherwise silences every file assignment to
   ; that name, ?= and += included -- make's rule). origins records provenance
   ; for $(origin); an unfired ?= leaves both value and origin alone.
   (do-assign as nm op val) (assign1 as nm op val 0)
   (assign1 as nm op val ov)
   (? (!ov && (peep origins nm "undefined" = "command line")) 0
      (: v (trim val)
         (org _) (pin origins nm (? ov "override" "file"))
         (? (op = ":=")  (: _ (pin as nm (expand v as 0)) (org ()))
            (op = "::=") (: _ (pin as nm (expand v as 0)) (org ()))
            (op = "?=")  (: cur (peep as nm 'cook.undef)
                            (? (nom? cur) (: _ (pin as nm v) (org ())) 0))
            (op = "+=")  (: o (peep as nm "")
                            _ (pin as nm (? (0 < tally o) (o + " " + v) v)) (org ()))
            (: _ (pin as nm v) (org ())))))

   ; -- fold classified lines into a rules list, mutating `as`. The conditional
   ; stack is a list of frames (pon taken active); `on` is the top frame's active
   ; (1 when there is no frame). include splices the named files' lines inline.
   (r1 c) <>c (r2 c) <>>c (r3 c) <>>>c
   (addstep cur st) (<cur >< (<>cur >< (st >< >>cur)))
   (finalize cur) (<cur >< (<>cur >< rev >>cur))
   (flushr cur rules) (? cur (finalize cur >< rules) rules)
   (mkrule c)                  ; -> (TGTS-text PREREQS-text . STEPS-rev); split off an inline ; recipe
   (: prr (r2 c) sc (splittop prr 59)
      inl (? (two? >sc && (0 < tally (trim <>sc))) `((parse-mods (trim <>sc))) 0)
      (r1 c >< (<sc >< inl)))
   (condon cond) (? (two? cond) <>><cond 1)
   (push-cond cond pon active) (`(pon active active) >< cond)
   (else-cond cond) (: f <cond pon <f taken <>f na (pon && !taken)
                       (`(pon (taken || na) na) >< >cond))
   (build cls as)
   ((: (loop cs rules cur cond)
       (? !(two? cs) (rev (flushr cur rules))
          (: c <cs k <c rest >cs on (condon cond)
             (? (k = 'recipe) (loop rest rules (? (on && cur) (addstep cur (r1 c)) cur) cond)
                (k = 'rule)   (? on (loop rest (flushr cur rules) (mkrule c) cond)
                                    (loop rest (flushr cur rules) 0 cond))
                (k = 'assign) (: _ (? on (do-assign as (r1 c) (r2 c) (r3 c)) 0) (loop rest (flushr cur rules) 0 cond))
                (k = 'oassign) (: _ (? on (assign1 as (r1 c) (r2 c) (r3 c) 1) 0) (loop rest (flushr cur rules) 0 cond))
                (k = 'xassign) (: _ (? on (: _ (assign1 as (r1 c) (r2 c) (r3 c) 0)
                                             (setenv (r1 c) (expand (peep as (r1 c) "") as 0))) 0)
                                  (loop rest (flushr cur rules) 0 cond))
                (k = 'xname)   (: _ (? on (setenv (r1 c) (expand (peep as (r1 c) "") as 0)) 0)
                                  (loop rest (flushr cur rules) 0 cond))
                (k = 'inc)    (? on (: more (catmap file-lines (split-ws (expand (r1 c) as 0)))
                                       (loop (more + rest) (flushr cur rules) 0 cond))
                                    (loop rest (flushr cur rules) 0 cond))
                ; conditionals thread `cur` (do NOT flush): a top-level rule is
                ; already flushed by the blank line before a conditional, while a
                ; conditional INSIDE a recipe block gates its lines via `on` above.
                (k = 'ifeq)   (loop rest rules cur (push-cond cond on (on && eval-ifeq (r1 c) as)))
                (k = 'ifneq)  (loop rest rules cur (push-cond cond on (on && !(eval-ifeq (r1 c) as))))
                (k = 'ifdef)  (loop rest rules cur (push-cond cond on (on && eval-ifdef (r1 c) as)))
                (k = 'ifndef) (loop rest rules cur (push-cond cond on (on && !(eval-ifdef (r1 c) as))))
                (k = 'else)   (loop rest rules cur (else-cond cond))
                (k = 'endif)  (loop rest rules cur >cond)
                (loop rest (flushr cur rules) 0 cond)))))
    cls 0 0 0)

   ; read a file whole (for `include`), then split it into classified lines.
   (slurp-of path) (: q (open path "r") (? q (slurp q) (: _ (say err ("cook: cannot read " + path)) _ (put err 10) (quit 1))))
   (file-lines ip) (map classify (lines (uncont (slurp-of ip))))

   ; seed the variable table the way make does: the ENVIRONMENT first (every
   ; NAME=value, origin "environment" -- $(HOME) and friends expand for real),
   ; then the builtin defaults for whatever the environment left unsaid, then
   ; the command line's VAR=VAL words, whose origin silences file assignments.
   (seed-defaults as)
   (: _ (each (keys origins) (\ k (pull origins k 0)))       ; a fresh provenance per import
      ; ..EXCEPT SHELL: make's documented exception -- the user's login shell
      ; must never become the recipe shell (only a Makefile's own SHELL= does)
      _ (each (environ 0)
              (\ e (: i (findc e 0 61)                       ; 61 = =
                      (? (i < tally e)
                         (: nm (snip e 0 i)
                            (? (nm = "SHELL") 0
                               (: _ (pin as nm (sub e (1 + i) (tally e)))
                                  (pin origins nm "environment"))))
                         0))))
      (dflt nm v) (? (nom? (peep as nm 'cook.undef))
                     (: _ (pin as nm v) (pin origins nm "default")) 0)
      _ (dflt "CC" "cc") _ (dflt "MAKE" "make") _ (dflt "AR" "ar") _ (dflt "RM" "rm -f")
      _ (each (peep clivars 0 ())
              (\ pr (: _ (pin as <pr >pr) (pin origins <pr "command line"))))
      as)

   ; split a prereq word list at a `|` into (normal . order-only).
   (splitbar ws) ((: (g l acc) (? !(two? l) (rev acc >< 0)
                       (? (<l = "|") (rev acc >< >l) (g >l (<l >< acc))))) ws 0)

   ; parse a Makefile and REGISTER its rules. .PHONY members become phony SYMBOLS;
   ; every other item a file STRING. A rule line is one of: a STATIC pattern rule
   ; (targets: tgtpat: prereqpats), an implicit PATTERN rule (a % target -> stored
   ; for prep-time resolution), or a NORMAL rule. The first registered is the ticket.
   (register-makefile src)
   (: as (seed-defaults (tablet 0))
      rules (build (map classify (lines (uncont src))) as)
      phony (: ps (tablet 0)
               _ (each rules (\ r (: tg (expand-words `(<r) as)
                    (? (any (\ t (t = ".PHONY")) tg)
                       (each (expand-words `(<>r) as) (\ p (pin ps p 1))) 0)))) ps)
      ; .ONESHELL is a TARGET, not a variable, but what it governs is recipe
      ; expansion -- so record it where expand-steps already looks: `as`.
      _ (each rules (\ r (? (any (\ t (t = ".ONESHELL")) (expand-words `(<r) as))
                            (pin as ".ONESHELL" "1") 0)))
      (mk-item nm) (? (peep phony nm 0) (intern nm) nm)
      patbox (tablet 0)        ; accumulates implicit pattern rules: (tgtpat prs oos . raws)
      _ (each rules (\ r
           (: tgtstext <r prtext <>r raws >>r
              tgts (expand-words `(tgtstext) as)
              (? (any (\ t ((0 < tally t) && (t 0 = 46))) tgts) 0        ; drop .PHONY / .SUFFIXES / ...
                 (: cp (splittop prtext 58)
                    (? (two? >cp)
                       ; static pattern rule
                       (: tpat <(expand-words `(<cp) as) ppats (expand-words `(<>cp) as)
                          (each tgts (\ t (: m (pmatch tpat t)
                             (? (two? m) (: stem <m ping (map (\ pp (stemsub pp stem)) ppats)
                                            (reg (mk-item t) (map mk-item ping) 0 (expand-steps raws (mk-item t) ping stem as))) 0)))))
                       ; implicit pattern rule, or normal rule
                       (: sb (splitbar (expand-words `(prtext) as)) prs <sb oos >sb
                          (? (any haspct tgts)
                             (each tgts (\ t (? (haspct t) (pin patbox 0 (`(t prs oos raws) >< peep patbox 0 0)) 0)))
                             ; ⚠ the automatics read the target's WHOLE prerequisite list,
                             ; which an earlier line may have stated and this one not repeat:
                             ;   a.out: a.c
                             ;   a.out b.out: ; cc $<
                             ; make expands $</$^ when the recipe RUNS, against everything
                             ; merged; cook expands here, so it asks the card already
                             ; registered before adding this line's own words. (prereqs
                             ; arriving AFTER their recipe still read short -- that one wants
                             ; expansion deferred to prep, a bigger change than this.)
                             (each tgts (\ t (: it (mk-item t) ps (map mk-item prs)
                                                all (adds <(peep cards (unseat it) '(())) ps)
                                                (reg it ps (map mk-item oos)
                                                     (expand-steps raws it all "" as)))))))))))))
      ; make's .DEFAULT_GOAL overrides "first rule declared" -- it is a plain
      ; variable, so it survives the dot-target drop above and lands in `as`.
      dg (trim (peep as ".DEFAULT_GOAL" ""))
      _ (? (0 < tally dg) (pin check 0 (mk-item dg)) 0)
      patlist (peep patbox 0 0)
      (psrc p item) (: stem <(pmatch <p item) prs <>p (? (two? prs) (stemsub <prs stem) ""))
      (best-of ms item)        ; prefer a pattern whose stem-source exists, then the shortest stem
      (: ok (filter (\ p (exists? (psrc p item))) ms) cand (? (two? ok) ok ms)
         (foldl (\ b p (? !b p (? (tally <(pmatch <p item) < tally <(pmatch <b item)) p b))) 0 cand))
      (resolve item)           ; synthesize a card from the best matching pattern rule (merging an explicit prereq-only card)
      (: ms (filter (\ p (two? (pmatch <p item))) patlist) best (best-of ms item)
         (? best
            (: stem <(pmatch <best item)
               ping (map (\ pp (stemsub pp stem)) <>best)
               poo  (map (\ pp (stemsub pp stem)) <>>best)
               ex (peep cards item ())
               allp (? ex (map mk-item ping + <ex) (map mk-item ping))
               oo   (? ex (map mk-item poo + <>ex) (map mk-item poo))
               (allp >< (oo >< expand-steps <>>>best item ping stem as)))
            0))
      (pin pgen 0 resolve))

   ; -- emit: serialize the RESOLVED cards back as a Cookfile ------------------
   ; after register-makefile (with emitp set) the cards hold concrete, fully
   ; expanded items and ("sh" "-c" cmd) steps. We walk the dependency graph from
   ; every root -- the default ticket FIRST (so it stays the default in the output),
   ; then every other registered card -- resolving pattern rules to concrete cards
   ; exactly as prep does, and print each as a (recipe ..) / (reg ..) form. A
   ; pattern-only target with no card prints nothing (a source leaf).
   (qshow x) ("'" + show x)                  ; a quoted datum literal: 'all, '("a" b)
   ; the emitted Cookfile must not carry THIS seat's nest, or it installs into a
   ; home nobody else has: write $HOME and $PWD back over the resolved prefixes.
   ; ⚠ the cwd goes FIRST -- it usually sits UNDER the home, and a $HOME pass
   ; would hide it from the $PWD one. `reg` opens both again at the far end.
   (seat w)
   (? (string? w)
      (: c (cwd 0) h (getenv "HOME")
         a (? (string? c && (1 < tally c)) (strrep c "$PWD" w) w)
         (? (string? h && (1 < tally h)) (strrep h "$HOME" a) a))
      w)
   (seatl l) (? (two? l) (map seat l) l)
   (estep steps) (? (two? steps) (qshow (map seatl steps)) "0")
   (emit-line o item ings oo steps)
   (: row (? (two? oo)
             `("(reg" (qshow (seat item)) (qshow (? (two? ings) (seatl ings) 0)) (qshow (seatl oo)) (estep steps))
             `("(recipe" (qshow (seat item)) (qshow (? (two? ings) (seatl ings) 0)) (estep steps)))
      _ (say o (join " " row)) _ (say o ")") (put o 10))
   ; pre-order walk; `seen` (a tablet) keeps a shared prerequisite to one card.
   ; the card lookup mirrors prep: an explicit recipe wins, else a pattern rule.
   (emit-walk o item seen)
   (? (peep seen item 0) 0
      (: _ (pin seen item 1)
         exr (peep cards item 'none)
         ex (? (two? exr) exr 0)
         f (peep pgen 0 ())
         r (? (ex && two? >>ex) ex
              (: g (? f (f item) 0) (? (two? g) g ex)))
         (? (two? r)
            (: ings <r oo <>r steps >>r
               _ (emit-line o item ings oo steps)
               _ (each ings (\ d (emit-walk o d seen)))
               _ (each oo (\ d (emit-walk o d seen))) 0)
            0)))
   (emit-cookfile o)
   (: _ (say o "; generated by `cook --emit` from the Makefile -- do not edit by hand.") _ (put o 10)
       _ (say o "; the Makefile's $(shell)/$(wildcard)/vars/patterns are RESOLVED here; each") _ (put o 10)
       _ (say o "; recipe line became an (\"sh\" \"-c\" cmd) step. make's @ (silent) and -") _ (put o 10)
       _ (say o "; (ignore-error) line modifiers are not carried across.") _ (put o 10) _ (put o 10)
       seen (tablet 0)
       dflt (peep check 0 ())
       _ (? dflt (emit-walk o dflt seen) 0)
       _ (each (keys cards) (\ k (emit-walk o k seen)))
       _ (put o 10) _ (say o "(cook-all 0)") (put o 10))

   ; is a recipe-file path named like a Makefile? then cook reads it as one.
   (mk-lastslash p) ((: (g j) (? (0 < j) (? (p (j - 1) = 47) j (g (j - 1))) 0)) (tally p))
   (mk-base p) (snip p (mk-lastslash p) (tally p))
   (mk-ends p suf) (: ls (tally suf) lp (tally p) ((ls <= lp) && (suf = snip p (lp - ls) lp)))
   (makefilep p) (: b (mk-base p)
      ((b = "Makefile") || (b = "makefile") || (b = "GNUmakefile") || mk-ends p ".mk"))

   ; the build file may be named explicitly (the first arg word) or DISCOVERED in
   ; the cwd; the SEAT at the bottom already stripped the program head + -l chain.
   (exists? p) (: q (open p "r") (? q 1 0))     ; a one-shot probe (the fd dies with the process)
   ; discovery precedence: a love-native recipe file wins over a Makefile -- a
   ; legacy Cards.l first, then a Cookfile, then a Makefile. Only a Makefile-named
   ; file (makefilep) is ever read as make; Cards.l and Cookfile are love source.
   ; (a dummy param: a nullary (f) would be \ with one operand = quote.)
   (discover _) ((: (g ps) (? (two? ps) (? (exists? <ps) <ps (g >ps)) 0))
               '("Cards.l" "Cookfile" "Makefile"))

   ; empty the import books so a fresh build starts clean (cards/check/pgen are
   ; the global tablets the Makefile importer and the Cookfile eval both fill).
   (clear-cards _) (: _ (each (keys cards) (\ k (pull cards k 0)))
                      _ (pull check 0 0) _ (pull pgen 0 0) 0)
   ; transpile Makefile `mk` into the Cookfile at path `ck`, then wipe the import
   ; books so the regenerated Cookfile can be read back into a clean scope.
   (regen-cookfile mk ck)
   (: _ (pin emitp 0 1)
       q (open mk "r")
       _ (register-makefile (slurp q))
       o (open ck "w")
       _ (emit-cookfile o)
       _ (close o)
       _ (pin emitp 0 0)
       _ (clear-cards 0)
       _ (say err ("cook: " + ck + " regenerated from " + mk)) (put err 10))
   ; when a Cookfile and a Makefile both sit in the cwd and the Makefile is the
   ; NEWER of the two, refresh the Cookfile from it before discovery picks it up;
   ; otherwise the Cookfile is preferred as-is. (date is () for a missing file.)
   (autoregen _)
   (: tm (date "Makefile") tc (date "Cookfile")
      (? (tm && tc && tm > tc) (regen-cookfile "Makefile" "Cookfile") 0))

   ; -- the command line ---------------------------------------------------------
   ; cook owns its own arg parse over the rebound argv (cli.l stops at the first
   ; non-flag; cook reads the rest via mk-args). Like make, it takes flags in any
   ; order and treats every non-flag word as a recipe to build:
   ;   -f, --file FILE   the build file (a Makefile or a Cookfile); else discovered
   ;   --emit            transpile a Makefile to a resolved Cookfile on stdout, then exit
   ;   -h, --help, help  print usage and exit
   ;   -v, --version, version   print the version and exit
   ;   RECIPE ...        the items to cook (default: the first recipe declared)
   ; for back-compat with `cook <Makefile> [recipe]`, when no -f is given the first
   ; positional word that names an existing file is taken as the build file and the
   ; rest are recipes; with -f, every positional word is a recipe.
   (bye s) (: _ (puts s) _ (putc 10) _ (flush out) (quit 0))
   (version _) (bye ("cook (love " + love-version + ")"))
   (usage _) (bye ("usage: cook [-f FILE] [--emit] [-v] [-h] [VAR=VAL ...] [RECIPE ...]\n"
                   + "  -f, --file FILE   the build file (Makefile or Cookfile); else discovered\n"
                   + "      --emit        transpile a Makefile to a resolved Cookfile, then exit\n"
                   + "  -v, --version     print the version and exit\n"
                   + "  -h, --help        print this help and exit\n"
                   + "  VAR=VAL           a command-line variable (silences the file's assignments)\n"
                   + "  RECIPE ...        the items to build (default: the first recipe declared)\n"
                   + "a Makefile (Makefile/makefile/GNUmakefile/*.mk) is imported and cooked; any other build file is a Cookfile of ordinary love source."))
   (need-arg h r) (? (two? r) 0 (: _ (say err ("cook: " + h + " needs an argument")) _ (put err 10) (quit 2)))
   ; a NAME=VAL word (make's command-line variable; NAME:=VAL folds to the same,
   ; the split point's leading : stripped) -> (name . val), or () for a plain word.
   (varw w)
   (: i (findc w 0 61)                                       ; 61 = =
      (? ((i >= tally w) || (0 = i)) ()
         (: e (? (w (i - 1) = 58) (i - 1) i)                 ; 58 = :
            (? (0 = e) () (snip w 0 e >< sub w (1 + i) (tally w))))))
   (parse-args ws)             ; -> a tablet: 'file, 'emit, 'help, 'ver, 'var, 'pos (the recipe words, in order)
   ((: (loop ws opt) (? !(two? ws) opt
       (: w <ws r >ws
          (? ((w = "-f") || (w = "--file")) (: _ (need-arg w r) _ (pin opt 'file <r) (loop >r opt))
             (w = "--emit")                                       (: _ (pin opt 'emit 1) (loop r opt))
             ((w = "-h") || (w = "--help") || (w = "help"))      (: _ (pin opt 'help 1) (loop r opt))
             ((w = "-v") || (w = "--version") || (w = "version")) (: _ (pin opt 'ver 1) (loop r opt))
             (: vw (varw w)
                (? (two? vw) (: _ (pin opt 'var (peep opt 'var () + `(vw))) (loop r opt))
                   (: _ (pin opt 'pos (peep opt 'pos () + `(w))) (loop r opt))))))))
    ws (tablet 0))

   ; -- cook-main (this form's body ANSWERS it) --
   ; the whole job from the args: parse flags, find the build file, cook, and exit
   ; first (quit), so when fired from a -l preload the CLI driver never tries to
   ; read a Makefile (or re-read the chosen file) as l source. A Makefile is
   ; imported and its recipes cooked; a Cookfile is read and evaluated -- it drives
   ; cook itself, the ticket read live by (ticket 0) from the tickets box.
   ;
   ; it SPLICES ITS OWN LAYER first, and owns that for every caller (the seat, kore's
   ; dispatcher, a host embedding cook): a Cookfile is love source read at RUNTIME, so
   ; its bare (recipe ..) (cook-all 0) (ticket 0) only resolve on the module walk. one
   ; (use 'cook), at the one place that needs it -- a caller just needs the closure.
   (cook-main as)
   (: _ (? (lit? (from 'cook)) (use 'cook) 0)   ; splice my REGISTERED self if I am one (the module
                                                ; path); riding a cat, my names already walk -- the
                                                ; presence guard keeps the loader out of it
      opt (parse-args as)
      _ (? (peep opt 'help 0) (usage 0) 0)
      _ (? (peep opt 'ver 0) (version 0) 0)
      _ (pin clivars 0 (peep opt 'var ()))                   ; the VAR=VAL words, for seed-defaults
      emit? (peep opt 'emit 0)
      pos (peep opt 'pos ())
      xf (peep opt 'file 0)                                       ; an explicit -f file (0 if none)
      _ (? (xf && !(exists? xf)) (: _ (say err ("cook: cannot open " + xf)) _ (put err 10) (quit 1)) 0)
      posfile (? (!xf && two? pos && exists? <pos) <pos 0)           ; a positional build file (back-compat)
      file (? xf xf (? posfile posfile (: _ (autoregen 0) (discover 0))))     ; discovery may auto-regen a stale Cookfile from a newer Makefile
      _ (pin tickets 0 (? posfile >pos pos))                 ; the recipe words; the positional file is not a recipe
      (? emit?
         (: _ (? file 0 (: _ (say err "cook: --emit needs a Makefile") _ (put err 10) (quit 1)))
             q (open file "r")
             _ (pin emitp 0 1)
             _ (register-makefile (slurp q))
             _ (emit-cookfile out)
             (quit 0))
         file
         (: q (open file "r")
            _ (? (makefilep file)
                 (: _ (register-makefile (slurp q)) (cook-all 0))
                 ((: (g x) (: r (sound x)
                                  (? (two? r) (: _ (ev 'ev <r) (g >r))
                                     (id? r 'torn)
                                       (: _ (say err ("cook: unfinished form in " + file))
                                          _ (put err 10) (quit 1))
                                     0)))
                   (slurp q)))
            (quit 0))
         (two? pos)
         (: _ (say err "cook: no Cards.l, Cookfile or Makefile found") _ (put err 10) (quit 1))
         0))

   cook-main)

   cook-main _)

; the SEAT (tools/ain.l's shape): fire cook-main iff this very file -- or the
; installed `cook` symlink -- sits in the program seat of the real command line
; (positional program or a -l preload), taking the args AFTER the seat. cmdline
; is the full C argv, never rebound, so the read is the same in both load
; orders; inside the multi-call `kore` nothing matches and kore's dispatcher calls
; cook-main itself. body-having: the seat machinery leaks nothing.
;
; cook-main reads BARE: whichever layer holds this file's binds (the loader's, or
; a cat's session) is still the head here. cook-main splices its registered self.
(: (bn p) (: n (tally p)
             (go k r) (? (k < n) (go (k + 1) (? (p k = 47) (k + 1) r)) (snip p r n))
             (go 0 0))
   (me? w) (: b (bn w) ((b = "cook") || (b = "cook.l")))
   (seat a) (? !(two? a) ()
               (<a = "-l") (? (two? >a)
                                   (? (me? <>a) (1 >< >>a) (seat >>a))
                                   ())
               (me? <a) (1 >< >a)
               ())
   s (seat >cmdline)
   (? (two? s) (cook-main >s) ()))
