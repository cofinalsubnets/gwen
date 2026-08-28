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
; the file name does not collide with the other, unrelated sense of "bake" in this
; tree -- baking the heap image / the egg (love/egg.l), the array-perf bake. the
; build verb here is `cook`; "bake" is reserved for the egg. the messages this tool
; prints are prefixed `cook:` for the same reason.)
;
; an item is brought up to date by first prepping its prerequisites up to date
; (depth first), then -- if the item is missing or older than any prerequisite --
; running its recipe. that is the whole of make's idea; the rest is bookkeeping.
;
; a recipe is registered with (recipe item ingredients steps):
;   item -- a filename string (an artifact on disk) or a PHONY symbol ('all,
;             'clean ..). a phony item owns no file, so it has no age and so
;             always runs, cascading a rebuild to everything that needs it.
;   ingredients -- a list of prerequisite items (strings and/or symbols).
;   steps  -- a list of steps. each step is either an argv list of strings, run as
;             a subprocess via the host `hark` nif, or a thunk (any lit?), called as
;             (step 0) for its effect. an empty step list is legal (a pure aggregate).
;
; file ages come from stat(1) -- love has no stat nif, but it has `hark`, and that
; is enough. a missing file and a phony item both read as the zero point (nil):
; ageless, hence ancient, hence always rebuilt. the first recipe declared is the
; default ticket, exactly as in make. drive it with (cook item) -- to cook an item
; is to prep it up to date by following its recipe over fresh ingredients.
;
; usage:  cook [-f file] [--emit] [recipe ..]  (with the symlink installed)
;     or:  love -l crew/cook/cook.l [-f file] [recipe ..]
; A Cookfile is ordinary love source that lays out the recipes (recipe ..) then
; drives them: (cook-all 0) builds every recipe named on the command line (or the
; default when none), and (cook (ticket 0)) builds just the first. cook can also
; read a real Makefile directly: only a file named like one (Makefile, makefile,
; GNUmakefile, *.mk -- makefilep) is read as make; every other recipe file (Cards.l,
; Cookfile, or a -f FILE) is love source. See "importing a Makefile" at the foot of
; this file. The first rule is the default ticket either way.

; the wrap: every lane rides module 'cook. a consumer says (use 'cook)
; (lib/cook.l) or probes (from 'cook 'recipe); riding a cat (kore's), the wrap
; registers the same module, so the ~30 globals (`join` `date` `check` `shell`
; among them) stay off the cat's session layer too. a path load composes: the
; loader keeps a file's own registration over its wrapper layer (prel's t re-pin).
(module 'cook

; the combinators (love/peg.l) -- the wrap's first form, so the names walk
; before anything below compiles against them, scoped to this module's compile.
(use 'peg)

(: cards (tablet 0)          ; the recipe cards: item -> (ingredients orderonly . steps)
   check (tablet 0)        ; a 1-cell box: the standing check -- the first item declared (the default when no ticket is named)
   tickets (tablet 0)      ; a 1-cell box: the recipe words named on the command line (the non-flag args), () until the dispatch parses them
   pgen (tablet 0)         ; a 1-cell box: a pattern-rule resolver (item -> a card | 0), installed by the Makefile importer (() until then)
   cookbox (tablet 0)      ; a 1-cell box: the in-image shell verdict (sh-line's `inimg`), decided once a run
   jobs (tablet 0)         ; a 1-cell box: the -j width (1 = serial, and nothing forks)
   dryrun (tablet 0)       ; ..and one for -n: set, every step is said and none is run
   running (tablet 0)      ; pid -> the item whose recipe that job is running
   verdicts (tablet 0)     ; pid -> the status a reaped job answered, until its land takes it

   origins (tablet 0)      ; make's variable provenance: name -> "default" | "environment"
                           ; | "file" | "command line" | "override" ($(origin), and the
                           ; rule that a file assignment never beats a command-line one)
   flavors (tablet 0)      ; name -> "simple" | "recursive", what $(flavor) answers: the one
                           ; thing a stored value cannot say, since := expands at assign time
                           ; and = stores the text, leaving both plain text afterwards
   clivars (tablet 0)      ; a 1-cell box: the command line's VAR=VAL words, ((name . val) ..)

   ; "a b c" from ("a" "b" "c"); "" from the empty list.
   (join sep l) (? (two? l) (foldl (\ a x (a + sep + x)) <l >l) "")

   ; register one card, merging with any prior card for the same item: ingredients
   ; and order-only prereqs accumulate, and a later non-empty recipe wins (as make
   ; does -- e.g. an explicit "prereqs only" rule that adds a dependency to a target
   ; whose recipe comes from a pattern rule). The first item ever registered is the
   ; default ticket. card = (ingredients orderonly . steps); orderonly prereqs are
   ; built-if-missing but never make a target stale (make's `| prereqs`).
   ; the merge dedupes. a prerequisite named twice is named once (make agrees), and
   ; without that a caller who re-registers the same recipe grows the card without bound:
   ; papel's dev server calls `build` per source change, and its index card climbed 28
   ; ingredients per rebuild -- 2828 after a hundred -- which `prep` then walks every pass.
   ; unbounded memory and quadratic time, from a tool that looked idle.
   (adds a b) (foldl (\ acc x (? (member? x acc) acc (acc + [x]))) a b)
   ; a Cookfile is written on one seat and read on another, so its paths name the
   ; seat symbolically: a leading $HOME or $PWD opens here, at registration. step
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
   ; a double-colon target is the one place a second recipe does not replace the first:
   ; make runs every `tgt:: ..` rule's steps, in the order written. reg's own rule -- a
   ; later recipe wins -- is `:`'s, and stays.
   ; the targets carrying a target-specific variable -- see classify. recur asks.
   tvarbox (tablet 0)
   (regd item ing oo steps)
   (: at (unseat item) ins (unseatl ing) ors (unseatl oo)
      prev (peep cards at ())
      (? prev (pin cards at (link (adds <prev ins) (link (adds <>prev ors) (>>prev + steps))))
         (reg item ing oo steps)))
   ; the public Cookfile API: (recipe item ingredients steps) -- no order-only prereqs.
   (recipe item ingredients steps) (reg item ingredients 0 steps)

   ; the modification time of an item in nanoseconds (one charm, the stat nif's
   ; fourth element -- sub-millisecond, so two writes inside one tick still order),
   ; or () (the zero point) when it has none: a phony symbol, or a file that is
   ; not on disk. (this WAS stat(1) through a shell, batch-cached against the ~2N
   ; forks of a no-work pass; the nif is a syscall, so the cache went with the fork.)
   (date item)
   (? (nom? item) () (: st (stat item) (? (two? st) <>>>st ()))))

; --- prepping an item up to date ---------------------------------------------
; one cook call carries two books: `done` memoizes each item's settled age so a
; shared prerequisite is visited once, and `shipped` records the items whose recipe
; actually changed the file this pass (its mtime advanced), so a rebuilt prerequisite
; forces its dependents even when the clock granularity would hide it -- but a recipe
; that re-runs without touching its output (the love_version.h cmp idiom) does not
; cascade, matching make's mtime-only decision.

(: (prep item done shipped pending)
   (? (peep done item ())                                   ; settled already
      (peep done item ())
      (two? (peep pending item 0)) ()                       ; a job is up for it; landing it settles it
      ; the card: an explicit recipe wins outright; otherwise a pattern rule may
      ; synthesize one (merging any explicit "prereqs only" card), else the explicit
      ; prereq-only card, else nothing (a source leaf).
      ; presence is tested with `two?` (a card is a chain), never net-truthiness:
      ; a card of all-empty parts -- (() ()) -- nets 0 (blue), so (? card ..) would
      ; wrongly read it absent. a phony aggregate with no prereqs is exactly that.
      (: exr (peep cards item 'none)
         ex (? (two? exr) exr 0)
         f (peep pgen 0 ())
         r (? (ex && two? >>ex) ex
              (: g (? f (f item) 0) (? (two? g) g ex)))
         (? (two? r)
            ; item has a card: prep every prerequisite up to date, then decide.
            ; prepping and landing are two passes, and that is the whole of -j:
            ; prep leaves a stale prerequisite's recipe running, so the next one's
            ; subtree starts beside it; only then does the second pass wait, because
            ; an age cannot be read off a file still being written. at -j1 nothing is
            ; ever left running and the two passes read as the one walk they were.
            (: ingredients <r orderonly <>r steps >>r
               _ (each ingredients (\ d (prep d done shipped pending)))
               _ (each orderonly (\ d (prep d done shipped pending)))  ; order-only: built if missing, never makes us stale
               _ (each ingredients (\ d (jobland d done shipped pending)))
               _ (each orderonly (\ d (jobland d done shipped pending)))
               ta0 (date item)
               fresh (any (\ d (peep shipped d 0)) ingredients)   ; a prerequisite's file changed
               older (any (\ d (: da (peep done d ())     ; a prerequisite is newer
                                  (? !da 1 (? !ta0 1 (da > ta0))))) ingredients)
               stale (!ta0 || fresh || older)
               ; run the recipe when stale -- as a job where there is room to fork one,
               ; else right here. a job's item settles when it lands; everything else
               ; settles now.
               j (? stale (jobstart item steps) 0)
               (? j (: _ (pin pending item (link j (link ta0 steps))) ta0)
                    (settle item ta0 (? stale steps ()) done shipped)))
            ; no card: a source leaf. it must already exist on disk.
            (: ta (date item)
               _ (? ta 0 (: _ (say err ("cook: no recipe to make " + show item))
                            _ (put err 10) (quit 1)))
               _ (pin done item ta)
               ta))))

   ; settle an item: read its age and pin it. mark shipped only if the recipe actually
   ; advanced the file's mtime (created it, or rewrote it newer) -- a no-op rebuild
   ; must not force dependents. `steps` is () for an item that ran no recipe.
   (settle item ta0 steps done shipped)
   (: nw (date item)
      _ (? (two? steps && (? !ta0 (? nw 1 0) (? nw (nw > ta0) 0)))
           (pin shipped item 1) 0)
      _ (pin done item nw)
      nw)

   ; a prerequisite's job must land before its age can be read. () for an item that
   ; never took one, which is every item at -j1.
   (jobland item done shipped pending)
   (: v (peep pending item 0)
      (? !(two? v) ()
         (: st (jobwait <v)
            _ (pull pending item 0)
            _ (? (0 = st) 0 (jobdie item st))
            (settle item <>v >>v done shipped))))

   ; run a recipe: each step in order. first, the one make feature we can see is
   ; wanted here and cannot give: a variable scoped to this target (classify's 'tvar).
   ; Refusing at parse would refuse a Makefile over a scope no build ever enters.
   (recur item steps)
   (: k (unseat item)
      nm (? (string? k) k (show k))
      _ (? (tally (peep tvarbox k ""))
           (: _ (say err ("cook: a target-specific variable (" + nm
                          + ") is a GNU make feature cook does not implement.  Stop."))
              _ (put err 10) (quit 2))
           0)
      (each steps (\ step (step1 step))))

   ; --- the job pool (-j N) --------------------------------------------------
   ; a stale recipe runs in a forked child -- the fork nif, no exec: the child owns a
   ; copy of this heap and runs the recipe exactly as a serial cook would, thunk steps
   ; and the in-image shell door included, then quits with its verdict. So a job's only
   ; outputs are the files it wrote and the status it answered, which is already make's
   ; whole contract for a recipe. and that is why a thunk step's heap effect does not
   ; survive -j: it lands in the child. A Cookfile that builds by mutating cook's own
   ; tables wants -j1, and gets it by default.
   ; green threads (twirl) would not do here. They are cooperative, so where the
   ; in-image door is live -- lush as /bin/sh, the distro -- the compile runs in this
   ; process and N tasks would take turns on one core. a fork is parallel in both worlds.
   ; and every name here is job-prefixed on purpose: this scope leaks into the same
   ; layer the expander walks, where `join` and `start` already mean other things.
   (jobw _) (: n (peep jobs 0 1) (? (0 < n) n 1))

   ; an (exec ..) step replaces the process, so a recipe carrying one can never be a
   ; job: the child would be replaced and cook would carry on as if it had run.
   (jobforkable? steps) (!(any (\ s (two? s && (<s = 'exec))) steps))

   ; start item's recipe. answers the job's pid when one took it -- the caller must
   ; land it before reading the item's age -- or 0 when it ran here and is done.
   ; the serial lane drains first: a recipe run here calls herald, which waits on
   ; its own child, and `glean` reaps any child -- so the two must never be in the air
   ; at once. a fork we asked for and did not get runs the recipe here, one job narrower.
   (jobstart item steps)
   (? (!(two? steps)) 0                                     ; a pure aggregate runs nothing: no job, and no drain either
      (!((1 < jobw ()) && (jobforkable? steps))) (: _ (jobdrain 0) _ (recur item steps) 0)
      (: _ (? (jobw () <= $running) (jobreap 0) 0)          ; wait for a slot
         _ (flush out) _ (flush err)
         p (fork 0)
         (? (0 = p) (: _ (recur item steps) (quit 0))       ; the child: run it and go
            (0 < p) (: _ (pin running p item) p)
            (: _ (recur item steps) 0))))

   ; reap one job home and file its verdict under its pid, so whichever land wants it
   ; finds it however the reaps fall. `glean` is waitpid(-1, WNOHANG), so the wait is
   ; a poll -- `rest` parks it, which costs a millisecond and not a core.
   (jobreap _)
   (: r (glean 0)
      (? (two? r) (: _ (pin verdicts <r >r) _ (pull running <r 0) <r)
         (charm? r) ()                                      ; -errno: no children left
         (: _ (rest 1) (jobreap 0))))

   ; this pid's status, reaping as needed. a pid that is neither filed nor running was
   ; reaped by something that is not this pool, and its verdict is gone -- which is not
   ; a green one, so it is said rather than assumed.
   (jobwait p)
   (: v (peep verdicts p 'no)
      (? !(id? v 'no) (: _ (pull verdicts p 0) v)
         (0 = $running) (: _ (say err ("cook: lost the job for pid " + show p))
                            _ (put err 10) (quit 1))
         (: _ (jobreap 0) (jobwait p))))

   (jobdrain _) (? (0 < $running) (: _ (jobreap 0) (jobdrain 0)) 0)

   ; a job came home red. the child already printed which command failed and with what
   ; status; this adds the item, which is the half that goes missing when jobs
   ; interleave, and says nothing about a number -- the child's exit is not the
   ; command's. outstanding jobs are let finish first, as make does: a half-written
   ; file left behind by a kill would read as up to date on the next run.
   (jobdie item ec)
   (: _ (jobdrain 0)
      _ (say err ("cook: the recipe for " + show item + " failed"))
      _ (put err 10) (quit ec))

   ; one step. a list headed by the symbol `exec` -- (exec PROG ARGS..) -- is an
   ; interactive step: it runs via the `exec` nif, which replaces cook and inherits
   ; the terminal, so the child (the repl, gdb, qemu) drives the real tty. a plain
   ; list of strings is an argv subprocess -- streamed, but on a pipe with /dev/null
   ; stdin, so it still cannot be interactive; any other lit? is a thunk.
   (step1 step)
   (? (two? step && (<step = 'exec)) (do-exec >step)     ; (exec ..) -- the interactive step
      (two? step) (shell step)
      (lit? step) (step 0)
      0)

   ; an interactive step: exec the argv, replacing this process (tty inherited).
   ; the exec nif returns only on failure -- then an errno fixnum -- so anything
   ; past it is the error path. exec is terminal by nature (the repl never returns),
   ; so cook's "cooked X" summary is correctly never reached.
   (do-exec argv)
   (: _ (flush out)
       _ (exec argv)
       _ (say err ("cook: cannot exec " + join " " argv)) _ (put err 10) (quit 1))

   ; run one argv: echo it, run it streaming its output (herald relays each chunk as
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

   ; the in-image shell, decided once and kept. lush rides the same cats cook does
   ; (kore's, the dist artifact's), so when the shell we would spawn is the binary we
   ; are already running, the recipe line can run here -- one image wake for a whole
   ; make instead of one per line, and every other tool in this image (mooncc above
   ; all) is then in-image too, so a CC=mooncc build stops paying a wake per TU.
   ; asked through the module, never by a bare reference: cook runs standalone
   ; as well (`love -l crew/cook/cook.l`), where lush's engine is not aboard and
   ; the from-reads answer () -- the spawn lane, unchanged. the policy --
   ; "would spawning this land on us?" -- is lush's, not ours.
   (inimg sh)
   (: v (peep cookbox 'inimg 'no)
       (? !(id? v 'no) v
          (: ours (from 'lush 'sh-ours?) one (from 'lush 'sh-oneline)
             f (? ((lit? ours) && (lit? one) && (ours sh)) one 0)
             _ (pin cookbox 'inimg f)
             f)))

   ; run one make recipe line (via SHELL -c, default sh), make-style: echo the
   ; raw command (the expanded line, not the shell wrapper) unless @ silent, stream its
   ; stdout as it is produced, and on a nonzero exit either carry on (- ignore,
   ; with a note to err) or abort. The Makefile path's analogue of `shell`, and
   ; the one that carries a recursive $(MAKE) -- the whole reason it must stream.
   ; the in-image door goes first and answers (1 status) when it took the line; a ()
   ; from it (flags we do not serve) falls through to the spawn, unchanged.
   (sh-line silent ignore sh flags cmd)
   (: _ (? silent 0 (: _ (puts cmd) (putc 10)))
       _ (flush out)
       f (inimg sh)
       o (? f (f flags cmd) ())
       r (? (two? o) [<>o] (herald (([sh] + flags) + [cmd])))
       ec (? (two? r) <r -1)
       (? (0 = ec) 0
          !(two? r) (: _ (say err ("cook: cannot run " + cmd)) _ (put err 10) (quit 1))
          ignore (: _ (say err ("cook: " + cmd + " (exit " + show ec + ", ignored)")) (put err 10))
          (: _ (say err ("cook: command failed (exit " + show ec + ")")) _ (put err 10) (quit 1)))))

; --- the entry points ----------------------------------------------------------

; prep item up to date, quietly, and answer the tablet of items whose recipe actually
; advanced their file. `cook` prints a verdict off it; a caller that wants the number
; takes `$` of it (papel's build does).
(: (prepped item)
   (: done (tablet 0) shipped (tablet 0) pending (tablet 0)
      _ (prep item done shipped pending)
      _ (jobland item done shipped pending)       ; the top item's own job, if it took one
      shipped)

   (cook item)                                ; prep item up to date and report
   (? !item (: _ (say err "cook: nothing to cook") _ (put err 10) (quit 1))
      (: shipped (prepped item)
         ; $ of a map is its key count: how many items actually ran a recipe.
         (? (0 < $shipped)
            (: _ (puts ("cook: cooked " + show item)) (putc 10))
            (: _ (puts ("cook: " + show item + " is already up to date")) (putc 10)))))

   ; resolve a command-line word to an item: the bare string when it names a file
   ; recipe, the interned symbol when it names a phony one, else the default -- the
   ; first recipe declared. an unknown word thus quietly cooks the default, as make's
   ; trailing target does when nothing matches.
   (pick a) (? (peep cards a 0) a
              (: s (intern a) (? (peep cards s 0) s (peep check 0 ()))))

   ; the ticket to build: the first recipe word named on the command line (the
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
; is built on the parser combinators (after the gw.js style) now shared from the
; love/peg.l: a parser is (\ s y n) over a string s, with a success
; continuation y = rest parsed and a failure continuation n = rest; the parsed
; payload is a list that `seq` concatenates. love has no regex, so the leaves are
; `lit` (a literal prefix), `one` (one char by predicate) and `span` (a maximal
; run); alt/seq/opt/many build the rest. The import is reasonably GNU-make-
; compatible: recursive $(VAR) expansion and substitution refs, the common
; functions ($(shell)/$(wildcard)/$(dir)/$(patsubst)/$(filter)/...), ifeq/ifdef
; conditionals, `include`, the = := ?= += flavors, pattern + static-pattern rules,
; order-only prereqs, and the $@ $< $^ $* automatics. Each recipe line runs via
; ("sh" "-c" line). See test/host/cook.l.
;
; $(wildcard) prefers pure love: lush's glob (crew/lush/glob.l, the mask law)
; over readdir/stat -- no /bin/sh underneath, so `kore make` globs before a
; shell exists. the glob rides module 'lush: a cat carrying the lush parts
; (kore's) registered it already, and a standalone -l load registers it off
; lib/lush/glob.l on the cwd, here, before cook-glob folds its from-read.
; where neither lands (a dev binary on a foreign cwd -- no lib/ to walk),
; globw below keeps the POSIX shell-out lane.
(? (tablet? (from 'lush)) 0
   (two? (stat "lib/lush/glob.l")) (use 'lush/glob)   ; the wrapped file registers module 'lush
   0)
; cook-glob: the pathname glob, a closure when lush's glob is aboard, () when
; not (the importer then keeps its POSIX shell-out lane). the glob rides
; module 'lush now, so the door is a from-read -- a miss answers ().
(: cook-glob (: g (from 'lush 'sh-glob) o (from 'lush 'sh-ones)
                (? ((lit? g) && (lit? o)) (\ pat (g pat (o (tally pat)))) ())))

; The whole importer lives in one body-having `:`, so none of its many helper
; names leak to the global scope. The form reads outer globals (the peg
; combinators, recipe, cook, ticket, the prel) freely; its body answers cook-main
; (args -> the whole job, exiting via quit). The outer `_`-holder form (cli.l's own
; trick) leaks cook-main as the one public -- the multi-call `kore` (crew/kore/kore.l)
; calls it as `make` -- and the seat at the bottom fires it only when this very
; file was invoked (the installed `cook` symlink, a -l preload, or positional).
(: _ (:
   ; --emit mode: a 1-cell box (0 = off). when set, expand-steps emits a
   ; serializable ("sh" "-c" cmd) argv step instead of a sh-line thunk, so the
   ; resolved cards can be shown back as a Cookfile (see emit-cookfile below).
   emitp (tablet 0)

   ; -- the combinators (eps/lit/one/span/alt/seq/seqs/opt/skip/pmap/many/parse) come from love/peg.l,
   ;    spliced by the (use 'peg) above; this body-having `:` reads them bare down the walk.

   ; -- character classes and string utilities --
   (chws c)     ((c = 32) || (c = 9))
   (digitp c)   ((48 <= c) && (c <= 57))
   (alphap c)   (((65 <= c) && (c <= 90)) || ((97 <= c) && (c <= 122)))
   (namech c)   (alphap c || digitp c || (c = 95) || (c = 46))
   ; notcolon admits whitespace: a rule's target text runs to the first colon,
   ; and `a.out b.out: src` is one rule with two targets -- excluding blanks here
   ; made every multi-target rule classify as 'skip, silently, and the built
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
   ; two rules, because make has two. off a recipe, `<ws>*\<newline><ws>*` is one
   ; space and the whitespace either side goes with it. in a recipe the join is the
   ; SHELL's: the backslash-newline goes entirely, nothing stands in its place, and
   ; only the continuation's leading TAB is eaten. a recipe is decided by the first
   ; physical line of the logical one, so the flag survives every fold.
   (rtws s) (: i ((: (g j) (? ((0 < j) && ((s (j - 1) = 32) || (s (j - 1) = 9))) (g (j - 1)) j))
                  (tally s))
               (snip s 0 i))
   (uncont s)
   ((: (g i acc rec bol)
       (? (i >= tally s) acc
          (: c (s i)
             (? ((c = 92) && ((1 + i) < tally s) && (s (1 + i) = 10))
                (? rec (: j (2 + i) (g (? ((j < tally s) && (s j = 9)) (1 + j) j) acc rec 0))
                   (: j ((: (h k) (? ((k < tally s) && ((s k = 32) || (s k = 9))) (h (1 + k)) k))
                         (2 + i))
                      (g j (rtws acc + " ") rec 0)))
                (c = 10) (g (1 + i) (acc + "\n") rec 1)
                (g (1 + i) (acc + string c) (? bol (c = 9) rec) 0)))))
    0 "" 0 1)
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
   (lastl l) (? (two? >l) (lastl >l) <l)
   ; `sub` is snip with the ends ordered: "" when start >= end.
   (sub s a b) (? (a < b) (snip s a b) "")
   ; strip a make comment (an unescaped #) from a non-recipe line; \# is a literal #.
   (strip-comment l)
   ((: (g i acc)
       (? (i >= tally l) acc
          (: c (l i)
             (? (c = 35) acc                                  ; 35 = #
                (c = 92) (? ((1 + i) < tally l)               ; 92 = backslash
                            (: d (l (1 + i)) (? (d = 35) (g (2 + i) (acc + "#")) (g (2 + i) (acc + "\\" + string d))))
                            (acc + "\\"))
                (g (1 + i) (acc + string c))))))
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
   ; `!=` runs its value and takes the output, the portable spelling of $(shell ..):
   ; bmake and GNU both read it, where neither reads the other's conditionals.
   opP (alt (lit "::=") (alt (lit ":=") (alt (lit "?=") (alt (lit "+=") (alt (lit "!=") (lit "="))))))
   assignP (seqs [(skip (span chws)) (span namech)
                   (skip (span chws)) (pmap (\ x [<x]) opP)
                   (skip (span chws)) (span anyc)])
   ruleP (seqs [(skip (span chws)) (span notcolon) (skip (lit ":")) (span anyc)])

   ; classify a line -> (recipe TEXT) | (assign NAME OP VAL) | (rule TGTS PREREQS)
   ; | (inc TEXT) | (ifeq|ifneq|ifdef|ifndef|else REST) | (endif) | (skip).
   (recipep l)  ((0 < tally l) && (l 0 = 9))
   ; parse a recipe body: strip a leading run of @ - + line modifiers (and any
   ; interspersed whitespace), recording whether @ (silent) and - (ignore-error)
   ; appeared. -> (silent ignore . body). + (force) is a no-op here -- cook has no
   ; dry-run mode -- so it is stripped without a flag. (64 = @, 45 = -, 43 = +.)
   (parse-mods s)
   ((: (g i sil ign)
       (? ((i < tally s) && (chws (s i) || (s i = 64) || (s i = 45) || (s i = 43)))
          (g (1 + i) (sil || (s i = 64)) (ign || (s i = 45)))
          (sil >< (ign >< snip s i (tally s))))) 0 0 0)
   (recipe-parse l) (parse-mods (snip l 1 (tally l)))   ; drop the leading tab, then the modifier run
   (classify l)
   (? (recipep l) ['recipe (recipe-parse l)]
      (: t (strip-comment l) tt (trim t)
         (? (0 = tally tt) '(skip)
            (: w (first-word tt) rest (after-word tt)
               (? (w = "include") ['inc rest]
                  (w = "-include") ['inc rest]
                  (w = "sinclude") ['inc rest]
                  ; `override NAME op VAL` assigns past a command-line VAR=VAL;
                  ; `export NAME [op VAL]` assigns (when an op is there) and hands the
                  ; final value to the process environment, so recipes inherit it.
                  (w = "override") (: a (parse assignP rest) (? (two? a) ('oassign >< a) '(skip)))
                  (w = "export") (: a (parse assignP rest)
                                    (? (two? a) ('xassign >< a) ['xname (trim rest)]))
                  ; a lone $(..) line -- the whole line, its closer last: make expands
                  ; it, so its side effect fires. `$(error ..)` is a guard, and a guard
                  ; cook walks past is worse than no guard. classify has no variable
                  ; table, so the loop does the work. a line that merely begins with
                  ; $( is an ordinary rule or assignment named by a variable, and stays
                  ; one -- `$(outs): %.out: %.in` is a static pattern rule.
                  ((1 < tally tt) && (tt 0 = 36)
                   && (? (tt 1 = 40) ((findclose tt 2 40 41) = ((tally tt) - 1))
                         (tt 1 = 123) ((findclose tt 2 123 125) = ((tally tt) - 1))
                         0))
                    ['expand tt]
                  ((w = "undefine") || (w = "vpath")) (cook-gap w)
                  (w = "ifeq") ['ifeq rest]
                  (w = "ifneq") ['ifneq rest]
                  (w = "ifdef") ['ifdef rest]
                  (w = "ifndef") ['ifndef rest]
                  (w = "else") ['else rest]
                  (w = "endif") '(endif)
                  (: a (parse assignP t)
                     (? (two? a) ('assign >< a)
                        (: r (parse ruleP t)
                           ; `tgts: NAME op VALUE` is a target-specific variable, and its
                           ; prerequisite half is an assignment. cook keeps one variable
                           ; table and cannot scope one to a target -- and read as a rule
                           ; the line invents a prerequisite named for the variable, which
                           ; is how `no recipe to make "kcppflags"` reads. So it is
                           ; recorded against its targets, and refuses only if one is
                           ; ever remade: a scope nothing enters changes no build.
                           (? !(two? r) '(skip)
                              (two? (parse assignP <>r)) ['tvar <r]
                              ('rule >< r))))))))))

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
               [(snip s lp (ls2 - ls))] ()))))
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
   (nl->sp s) ((: (g i acc) (? (i >= tally s) acc (g (1 + i) (acc + string (? (s i = 10) 32 (s i)))))) 0 "")
   (fn-shell sh cmd) (: r (hark [sh "-c" cmd]) (? (two? r) (nl->sp (chomp >r)) ""))
   ; $(wildcard), native both ways: lush's glob (cook-glob -- classes, multi-
   ; segment patterns, the mask law) when it is aboard; else glob1 below -- a
   ; word with no * or ? is an existence test; otherwise the
   ; last path segment is the pattern (the tree's globs are all leaf-shaped), matched
   ; over readdir -- * a run, ? one char, a leading . only by an explicit . -- and the
   ; matches sorted under their directory prefix (glob(3) order, what sh gave before).
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
   (? !(globch pat) (? (exists? pat) [pat] 0)
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
   ; make's $(abspath): rooted on the cwd, then lexically cleaned -- no repeated
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
                (g (1 + i) (acc + string (s i))))))
       0 ""))
   (fn-or args as d)  ((: (g l) (? (two? l) (: v (expand <l as (1 + d)) (? (0 < tally (trim v)) v (g >l))) "")) args)
   (fn-and args as d) ((: (g l last) (? (two? l) (: v (expand <l as (1 + d)) (? (0 < tally (trim v)) (g >l v) "")) last)) args "")
   ; -- the helpers the added builtins stand on ------------------------------------
   (findsub s sub)                        ; index of sub in s | -1
    (: n (tally s) m (tally sub)
       (at i) (? ((i + m) > n) -1
                 (: (eq k) (? (k >= m) 1 (((s (i + k)) = (sub k)) ? (eq (1 + k)) 0))
                    (? (eq 0) i (at (1 + i)))))
       (? (m = 0) 0 (at 0)))
   (num1 s) (: r (sound (trim s)) (? (two? r) <r 0))     ; a leading integer | 0
   (fn-join a b)                          ; $(join): pairwise, the longer list's tail rides alone
    (: (g x y acc) (? (two? x) (g >x (? (two? y) >y ()) ((<x + (? (two? y) <y "")) >< acc))
                      (two? y) (g () >y (<y >< acc))
                      (join " " (rev acc)))
       (g a b ()))
   (fn-slice ws n1 n2)                    ; $(wordlist): 1-based, inclusive, clamped both ends
    (: (g l i acc) (? (!(two? l) || (i > n2)) (rev acc)
                      (i < n1) (g >l (1 + i) acc)
                      (g >l (1 + i) (<l >< acc)))
       (g ws 1 ()))
   funcnames '("shell" "wildcard" "dir" "notdir" "basename" "suffix" "patsubst"
               "subst" "filter" "filter-out" "sort" "strip" "if" "or" "and"
               "addprefix" "addsuffix" "firstword" "lastword" "word" "words"
               "abspath" "realpath" "call" "origin"
               ; A NAME off this list is a variable -- `$(foo bar)` reads a variable of that
               ; name and answers empty, which is make's own rule, and it stays.
               "findstring" "join" "wordlist" "foreach" "value" "flavor"
               "error" "warning" "info")
   ; ..but a name make implements and we do not is not that case, and answering empty
   ; there is not fidelity: make never takes the variable branch for these, so an empty
   ; answer is a rule quietly not existing -- and no output diff can see a target that
   ; was never made. These refuse instead. The list is GNU's roster minus ours, so it is
   ; finite and knowable; cookdiff only ever catches the gaps somebody wrote a case for.
   gapnames '("file" "let" "intcmp" "guile")
   (cook-gap what)
    (: _ (say err ("cook: " + what + " is a GNU make feature cook does not implement.  Stop."))
       _ (put err 10) (quit 2))
   ; $(call fn,a,..): bind $0 (the name) and $1.. to the expanded args, expand
   ; fn's body, put the old bindings back (a shadowed outer $1 survives).
   (fn-call args as d)
   (: fname (trim (peep args 0 ""))
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
      rawl (\ _ (splittop at 44))               ; comma-split, not expanded (if/or/and)
      (? (nm = "shell")     (fn-shell (shname as) (ex1 at))
         (nm = "wildcard")  (join " " (catmap globw (split-ws (ex1 at))))
         (nm = "dir")       (join " " (map fdir (split-ws (ex1 at))))
         (nm = "notdir")    (join " " (map fnotdir (split-ws (ex1 at))))
         (nm = "basename")  (join " " (map fbase (split-ws (ex1 at))))
         (nm = "suffix")    (join " " (map fsuf (split-ws (ex1 at))))
         (nm = "strip")     (join " " (split-ws (ex1 at)))
         (nm = "sort")      (join " " (sortu (split-ws (ex1 at))))
         (nm = "patsubst")  (: a (argl 0) (join " " (map (\ w (patsub (peep a 0 "") (peep a 1 "") w)) (split-ws (peep a 2 "")))))
         (nm = "subst")     (: a (argl 0) (strrep (peep a 0 "") (peep a 1 "") (peep a 2 "")))
         (nm = "filter")    (: a (argl 0) (join " " (filter (\ w (matchany (split-ws (peep a 0 "")) w)) (split-ws (peep a 1 "")))))
         (nm = "filter-out")(: a (argl 0) (join " " (filter (\ w !(matchany (split-ws (peep a 0 "")) w)) (split-ws (peep a 1 "")))))
         (nm = "addprefix") (: a (argl 0) (join " " (map (\ w (peep a 0 "" + w)) (split-ws (peep a 1 "")))))
         (nm = "addsuffix") (: a (argl 0) (join " " (map (\ w (w + peep a 0 "")) (split-ws (peep a 1 "")))))
         (nm = "firstword") (: ws (split-ws (ex1 at)) (? (two? ws) <ws ""))
         (nm = "lastword")  (: ws (split-ws (ex1 at)) (? (two? ws) (lastl ws) ""))
         (nm = "word")      (: a (argl 0) ws (split-ws (peep a 1 "")) n (: r (sound (trim (peep a 0 ""))) (? (two? r) <r 0)) (peep ws (n - 1) ""))
         (nm = "words")     (show (tally (split-ws (ex1 at))))
         (nm = "if")        (: a (rawl 0) c (trim (ex1 (peep a 0 ""))) (? (0 < tally c) (ex1 (peep a 1 "")) (ex1 (peep a 2 ""))))
         (nm = "or")        (fn-or (rawl 0) as d)
         (nm = "and")       (fn-and (rawl 0) as d)
         (nm = "call")      (fn-call (argl 0) as d)
         (nm = "origin")    (peep origins (trim (ex1 at)) "undefined")
         ((nm = "abspath") || (nm = "realpath")) (join " " (map fabs (split-ws (ex1 at))))
         (nm = "findstring") (: a (argl 0) (? (0 <= (findsub (peep a 1 "") (peep a 0 ""))) (peep a 0 "") ""))
         (nm = "join")      (: a (argl 0) (fn-join (split-ws (peep a 0 "")) (split-ws (peep a 1 ""))))
         (nm = "wordlist")  (: a (argl 0)
                               ws (split-ws (peep a 2 ""))
                               n1 (num1 (peep a 0 "")) n2 (num1 (peep a 1 ""))
                               (join " " (fn-slice ws n1 n2)))
         (nm = "foreach")   (: a (rawl 0)
                               v (trim (ex1 (peep a 0 "")))
                               body (peep a 2 "")
                               old (peep as v "")
                               r (join " " (map (\ w (: _ (pin as v w) (ex1 body)))
                                                (split-ws (ex1 (peep a 1 "")))))
                               _ (pin as v old)              ; put the outer binding back
                               r)
         ; value/flavor read the variable without expanding it -- the one pair that must not ex1
         (nm = "value")     (peep as (trim (ex1 at)) "")
         (nm = "flavor")    (: o (peep origins (trim (ex1 at)) "undefined")
                               (? (o = "undefined") "undefined" (peep flavors (trim (ex1 at)) "recursive")))
         ; the diagnostics. silence here was the costly one: a guard spelled $(error ..) simply
         ; vanished, so a makefile that meant to refuse built on with an empty variable.
         (nm = "info")      (: _ (puts (ex1 at)) _ (putc 10) "")
         (nm = "warning")   (: _ (say err ("cook: " + ex1 at)) _ (put err 10) "")
         (nm = "error")     (: _ (say err ("cook: *** " + ex1 at + ".  Stop.")) _ (put err 10) (quit 2))
         ""))                                   ; unknown -> empty (make's own rule; see funcnames)

   ; a group body is a function call ($(name args)), a substitution reference
   ; ($(VAR:from=to)), or a plain variable ($(VAR)).
   (eval-group body as d)
   (: sp (findws body)
      nm (snip body 0 (? (sp < tally body) sp (tally body)))
      _ (? (any (\ g (g = nm)) gapnames) (cook-gap ("$(" + nm + " ..)")) 0)
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
                      (nx = 36) (g (2 + i) (acc + "$"))
                      (namech nx || (nx = 64) || (nx = 60) || (nx = 94) || (nx = 42) || (nx = 63))
                      (g (2 + i) (acc + expand (peep as (snip s (1 + i) (2 + i)) "") as (1 + d)))
                      (g (1 + i) (acc + string (s i)))))
                (g (1 + i) (acc + string (s i))))))
       0 ""))
   (expand-words ws as) (catmap (\ w (split-ws (expand w as 0))) ws)
   ; a recipe line that begins with the shell `exec` builtin is an interactive
   ; line: `exec <cmd>` replaces the shell, so make already runs it on the real
   ; tty -- and cook honours the same convention, running it through the exec nif
   ; (the `(exec ..)` step) rather than capturing it via `hark`. So one notation --
   ; `exec` -- marks the repl / gdb / qemu targets interactive in both builders.
   (starts? s pre) ((tally pre <= tally s) && (pre = snip s 0 (tally pre)))
   (execp cmd) (: t (ltrim cmd)
                  (starts? t "exec" && ((4 = tally t) || chws (t 4))))
   ; build a recipe's argv steps, pinning the automatic vars into the table first.
   ; build a card's steps. each raw is (silent ignore . body) from recipe-parse;
   ; expand the body now (the $@/$</$^/$* automatics are live in `as` at this
   ; point, and get re-pinned per item) and close over the expanded command in a
   ; thunk that runs it make-style via sh-line, carrying the @/- flags. An
   ; exec-prefixed line instead becomes an (exec "sh" "-c" cmd) interactive step
   ; (serialized as such under --emit, run via the exec nif otherwise).
   ; the shell a recipe line rides. .SHELLFLAGS is a plain make variable, so it is
   ; already in `as` (the .DEFAULT_GOAL precedent); its default is make's own -c.
   (shflags as) (: f (trim (peep as ".SHELLFLAGS" ""))
                   (? (0 < tally f) (split-ws f) '("-c")))
   ; $(@D): the target's directory sans trailing slash ("." for a bare name), $(@F)
   ; its basename -- make's D/F faces, pinned beside the plain automatics.
   (dslash w) (: k (mk-lastslash w) (? (0 < k) (snip w 0 (? (1 < k) (k - 1) k)) "."))
   ; ..and SHELL picks the shell itself (a Makefile saying SHELL := lush runs
   ; its recipes -- and its $(shell) calls -- under lush), default sh.
   (shname as) (: s (trim (peep as "SHELL" "")) (? (0 < tally s) s "sh"))
   ; a PHONY item is a symbol, not a string (mk-item interns it) -- and an automatic
   ; must read as the NAME either way. lua's top Makefile is `all: $(PLAT)` with the
   ; platforms phony and `cd src && $(MAKE) $@`; unconverted, $@ expands to nothing and
   ; the recursive make is handed an empty target. @F already knew this; @, < and ^ did
   ; not, which is the whole bug wearing three faces.
   (itext x) (? (nom? x) (show x) x)
   (expand-steps raws t pr stem as)
   (: pt (map itext pr)
       td (date t)
       ; $?: the prerequisites newer than the target -- all of them when the target is
       ; not on disk, which is make's answer for a phony and for a first build. a phony
       ; prerequisite counts as newer whatever the clock says: it is remade every pass.
       nwr (filter (\ x (? (nom? x) 1 (: xd (date x) (? !td (? xd 1 0) (? xd (xd > td) 0))))) pr)
       _ (pins as "@" (itext t) "<" (? (two? pt) <pt "") "^" (join " " pt) "*" stem)
       _ (pins as "?" (join " " (map itext nwr)))
       _ (pins as "@D" (? (nom? t) "." (dslash t)) "@F" (? (nom? t) (show t) (fnotdir t)))
       fl (shflags as)
       sn (shname as)
       quiet? (0 < tally (peep as ".SILENT" ""))
       ; make reads the @/-/+ modifiers after expansion too (the $(Q)-as-@
       ; verbose-build idiom; mk/lib.mk's lcat_h leads with @ from inside the
       ; variable) -- so re-strip the expanded line and fold its flags in.
       ; expand once, and keep the whole parse. Both the oneshell test below and the
       ; per-line lane want the expanded text, and expanding twice ran every $(shell ..)
       ; in a recipe twice -- the side effects included, and silently, since only the
       ; second run's value was ever used.
       exs (map (\ raw (parse-mods (expand >>raw as 0))) raws)
       cmds (map (\ e >>e) exs)
       ; .ONESHELL: the whole recipe rides one shell, so a recipe reads as a script
       ; instead of a run of \-joined lines. make's rule -- the @/- modifiers of the
       ; first line govern the lot. with plain -c only the last line's status is
       ; checked, so a failing middle line passes silently; a oneshell Makefile wants
       ; .SHELLFLAGS := -ec, and that is the shape this tree's own Makefile sets.
       ; An `exec` line still wants a process of its own (it REPLACES cook to inherit
       ; the tty), so a recipe carrying one stays per-line.
       one? ((0 < tally (peep as ".ONESHELL" "")) &&
             two? raws && !(any execp cmds))
       (? one?
          (: r0 <raws
             (? (peep dryrun 0 0) [(\ _ (: _ (puts (join "\n" cmds)) (putc 10)))]
                (peep emitp 0 0) [(([sn] + fl) + [(join "\n" cmds)])]
                [(\ _ (sh-line <r0 <>r0 sn fl (join "\n" cmds)))]))
          (: (g rs es acc)
              (? !(two? rs) (rev acc)
                 (: raw <rs e <es
                    sil (<raw || <e || quiet?) ign (<>raw || <>e) cmd >>e
                    ; -n says the line and runs nothing -- and @ does not hide it, which is
                    ; make's rule: the point of -n is to show the silent ones too.
                    st (? (peep dryrun 0 0) (\ _ (: _ (puts cmd) (putc 10)))
                          (execp cmd)
                          (? (peep emitp 0 0) ('exec >< [sn "-c" cmd])
                             (\ _ (do-exec [sn "-c" cmd])))
                          (peep emitp 0 0) (([sn] + fl) + [cmd])
                          (\ _ (sh-line sil ign sn fl cmd)))
                    (g >rs >es (st >< acc))))
             (g raws exs ()))))

   ; -- conditionals & assignment ops over the variable table `as` --
   (unquote-str s) (? ((tally s >= 2) && ((s 0 = 34) || (s 0 = 39))) (snip s 1 (tally s - 1)) s)
   (cond-args rest)            ; ifeq/ifneq rest -> (arg1 . arg2)
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
         ; the flavor rides beside the origin: an expanded `:=` and a stored `=` are the
         ; same text afterwards, so nothing in the value itself can answer $(flavor).
         (org f) (: _ (pin origins nm (? ov "override" "file")) (pin flavors nm f))
         (? (op = "!=")  (: _ (pin as nm (fn-shell (shname as) (expand v as 0))) (org "simple"))
            (op = ":=")  (: _ (pin as nm (expand v as 0)) (org "simple"))
            (op = "::=") (: _ (pin as nm (expand v as 0)) (org "simple"))
            (op = "?=")  (: cur (peep as nm 'cook.undef)
                            (? (nom? cur) (: _ (pin as nm v) (org "recursive")) 0))
            (op = "+=")  (: o (peep as nm "")
                            _ (pin as nm (? (0 < tally o) (o + " " + v) v))
                            (org (peep flavors nm "recursive")))
            (: _ (pin as nm v) (org "recursive")))))

   ; -- fold classified lines into a rules list, mutating `as`. The conditional
   ; stack is a list of frames (pon taken active); `on` is the top frame's active
   ; (1 when there is no frame). include splices the named files' lines inline.
   (r1 c) <>c (r2 c) <>>c (r3 c) <>>>c
   (addstep cur st) (<cur >< (<>cur >< (st >< >>cur)))
   (finalize cur) (<cur >< (<>cur >< rev >>cur))
   (flushr cur rules) (? cur (finalize cur >< rules) rules)
   (mkrule c)                  ; -> (TGTS-text PREREQS-text . STEPS-rev); split off an inline ; recipe
   ; the recipe is everything past the first `;`, semicolons included: `a:;x ; y` is one
   ; recipe line the shell splits, not two fields. taking split-field 2 alone dropped every
   ; command after the second `;` -- silently, since what remained still ran.
   (: prr (r2 c) sc (splittop prr 59)
      rec (? (two? >sc) (join ";" >sc) "")
      inl (? (0 < tally (trim rec)) [(parse-mods (trim rec))] 0)
      (r1 c >< (<sc >< inl)))
   (condon cond) (? (two? cond) <>><cond 1)
   (push-cond cond pon active) ([pon active active] >< cond)
   (else-cond cond) (: f <cond pon <f taken <>f na (pon && !taken)
                       ([pon (taken || na) na] >< >cond))
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
                ; conditional inside a recipe block gates its lines via `on` above.
                (k = 'defassign) (: _ (? on (do-assign as (r1 c) "=" (r2 c)) 0)
                                    (loop rest (flushr cur rules) 0 cond))
                ; a target-specific variable: remember which targets, so recur can
                ; refuse if one is remade. the value is never wanted -- cook has no
                ; scope to put it in, and a half-applied one would be the silence.
                (k = 'tvar) (: _ (? on (each (split-ws (expand (r1 c) as 0))
                                             (\ w (pin tvarbox (unseat w) "1"))) 0)
                               (loop rest (flushr cur rules) 0 cond))
                (k = 'expand) (? (on && evalp (r1 c))
                                 (: more (text-lines (expand (eval-args (r1 c)) as 0))
                                    (loop (more + rest) (flushr cur rules) 0 cond))
                                 (: _ (? on (expand (r1 c) as 0) 0) (loop rest rules cur cond)))
                (k = 'ifeq)   (loop rest rules cur (push-cond cond on (on && eval-ifeq (r1 c) as)))
                (k = 'ifneq)  (loop rest rules cur (push-cond cond on (on && !(eval-ifeq (r1 c) as))))
                (k = 'ifdef)  (loop rest rules cur (push-cond cond on (on && eval-ifdef (r1 c) as)))
                (k = 'ifndef) (loop rest rules cur (push-cond cond on (on && !(eval-ifdef (r1 c) as))))
                (k = 'else)   (loop rest rules cur (else-cond cond))
                (k = 'endif)  (loop rest rules cur >cond)
                (loop rest (flushr cur rules) 0 cond)))))
    cls 0 0 0)

   ; `$(eval TEXT)` alone on a line: expand TEXT, then parse it -- the splice `include`
   ; already does, with a string where the file was. Nested inside an expression it cannot
   ; work at all, since expand answers a string and cannot reach the rule list, and
   ; gapnames refuses it there rather than dropping it.
   (evalp s) (: n (tally s)
                ((7 < n) && (s 0 = 36) && ((s 1 = 40) || (s 1 = 123))
                 && (snip s 2 6 = "eval") && (chws (s 6))
                 && ((findclose s 2 (s 1) (? (s 1 = 40) 41 125)) = (n - 1))))
   (eval-args s) (ltrim (snip s 6 (tally s - 1)))
   ; a define block is a multi-line recursive assignment whose body must survive verbatim:
   ; classify strips comments, trims, and reads a tab-led line as a recipe, and a body
   ; forbids all three. so the walk takes those lines itself and hands classify the rest.
   (text-lines src)
    ((: (g l acc)
        (? !(two? l) (rev acc)
           (: t (trim (strip-comment <l))
              (? (first-word t = "define")
                 (: nm (trim (after-word t))
                    (body r bs) (? !(two? r) (: _ (say err "cook: define with no endef.  Stop.")
                                                 _ (put err 10) (quit 2))
                                    (trim (strip-comment <r) = "endef") [>r bs]
                                    (body >r (bs + <r + "\n")))
                    p (body >l "")
                    (g <p (['defassign nm <>p] >< acc)))
                 (g >l (classify <l >< acc))))))
     (lines (uncont src)) ())

   ; read a file whole (for `include`), then split it into classified lines.
   (slurp-of path) (: q (open path "r") (? q (slurp q) (: _ (say err ("cook: cannot read " + path)) _ (put err 10) (quit 1))))
   (file-lines ip) (text-lines (slurp-of ip))

   ; seed the variable table the way make does: the environment first (every
   ; NAME=value, origin "environment" -- $(HOME) and friends expand for real),
   ; then the builtin defaults for whatever the environment left unsaid, then
   ; the command line's VAR=VAL words, whose origin silences file assignments.
   ; `.POSIX:` is not decoration: it changes make's own defaults, CC to c99 and CFLAGS
   ; to -O1 (every suckless Makefile opens with it, and st's whole build reads them
   ; through STCFLAGS). read off the raw source, since the defaults are seeded before
   ; a single rule is parsed -- and as defaults, so a file's own CC= still wins.
   (posixp src)
   (any (\ l (: t (trim (strip-comment l))
                (&& (7 <= tally t) (snip t 0 7 = ".POSIX:"))))
        (lines src))
   (seed-defaults as px)
   (: _ (each (keys origins) (\ k (pull origins k 0)))       ; a fresh provenance per import
      ; ..except SHELL: make's documented exception -- the user's login shell
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
      _ (dflt "CC" (? px "c99" "cc")) _ (? px (dflt "CFLAGS" "-O1") 0)
      _ (dflt "MAKE" "make") _ (dflt "AR" "ar") _ (dflt "RM" "rm -f")
      _ (dflt "AS" "as") _ (dflt "ARFLAGS" "rv")      ; the builtin rules below read these
      _ (each (peep clivars 0 ())
              (\ pr (: _ (pin as <pr >pr) (pin origins <pr "command line"))))
      as)

   ; make's old-style suffix rules, the form every suckless Makefile is written in:
   ; `.c.o:` is `%.o: %.c`, and a single `.c:` is `%: %.c`. answers (tgtpat srcpat),
   ; or 0 when the target is not one. keyed on the suffix list, not on "starts with
   ; a dot": .PHONY would otherwise read as a single-suffix rule the moment somebody
   ; hung a recipe off it. and it must be recognized at all -- the dot-target drop
   ; eats these in silence, which is worse than not having suffix rules, because the
   ; builtin %.o: %.c then compiles the file with default flags instead of the ones
   ; the Makefile spent its whole config.mk computing (st, exactly).
   sufs '("c" "cc" "C" "cpp" "S" "s" "o" "a" "h" "y" "l" "f" "F" "p" "r" "m" "sh")
   ; ..and the list a Makefile says, which is the whole point of `.SUFFIXES`: bare, it
   ; clears; with prerequisites, it adds. Held in a box because sufpat is asked per rule
   ; and the declaration may be any line above. sufs is the seed, make's own default.
   sufbox (tablet 0)
   (sufnow _) (peep sufbox 0 sufs)
   (sufknown x) (any (\ v (v = x)) (sufnow 0))
   (sufpat t)
   (: n (tally t)
      (? (|| (n < 2) !(t 0 = 46)) 0
         (: (lastdot k r) (? (n <= k) r (? (t k = 46) (lastdot (1 + k) k) (lastdot (1 + k) r)))
            d (lastdot 1 0)
            (? (d = 0)                                        ; .c -> % : %.c
               (? (sufknown (snip t 1 n)) ["%" ("%" + t)] 0)
               (: sr (snip t 1 d) tg (snip t (1 + d) n)       ; .c.o -> %.o : %.c
                  (? (&& (sufknown sr) (sufknown tg))
                     [("%." + tg) ("%." + sr)] 0))))))

   ; split a prereq word list at a `|` into (normal . order-only).
   (splitbar ws) ((: (g l acc) (? !(two? l) (rev acc >< 0)
                       (? (<l = "|") (rev acc >< >l) (g >l (<l >< acc))))) ws 0)

   ; parse a Makefile and register its rules. .PHONY members become phony symbols;
   ; every other item a file string. a rule line is one of: a static pattern rule
   ; (targets: tgtpat: prereqpats), an implicit pattern rule (a % target -> stored
   ; for prep-time resolution), or a normal rule. The first registered is the ticket.
   (register-makefile src)
   (: as (seed-defaults (tablet 0) (posixp src))
      rules (build (text-lines src) as)
      phony (: ps (tablet 0)
               _ (each rules (\ r (: tg (expand-words [<r] as)
                    (? (any (\ t (t = ".PHONY")) tg)
                       (each (expand-words [<>r] as) (\ p (pin ps p 1))) 0)))) ps)
      ; .ONESHELL is a target, not a variable, but what it governs is recipe
      ; expansion -- so record it where expand-steps already looks: `as`.
      _ (each rules (\ r (? (any (\ t (t = ".ONESHELL")) (expand-words [<r] as))
                            (pin as ".ONESHELL" "1") 0)))
      ; `.SUFFIXES:` bare empties the vocabulary, and each one after it adds -- read in
      ; file order, before any rule asks sufpat, or a rule above its own declaration
      ; would be judged by the defaults.
      _ (each rules (\ r (? (any (\ t (t = ".SUFFIXES")) (expand-words [<r] as))
                            (: ws (expand-words [<>r] as)
                               (? (two? ws)
                                  (pin sufbox 0 ((sufnow 0) + map (\ w (? ((0 < tally w) && (w 0 = 46))
                                                                          (snip w 1 (tally w)) w)) ws))
                                  (pin sufbox 0 ())))
                            0)))
      ; .silent with no prerequisites quiets every recipe -- the whole-file form, which
      ; is the one anybody writes. Named prerequisites quiet only those targets, and
      ; that wants a per-target ask expand-steps has no room for yet.
      _ (each rules (\ r (? ((any (\ t (t = ".SILENT")) (expand-words [<r] as))
                             && (0 = tally (trim <>r)))
                            (pin as ".SILENT" "1") 0)))
      (mk-item nm) (? (peep phony nm 0) (intern nm) nm)
      patbox (tablet 0)        ; accumulates implicit pattern rules: (tgtpat prs oos . raws)
      _ (each rules (\ r
           (: tgtstext <r prtext0 <>r raws >>r
              ; `tgt:: ..` -- ruleP eats one colon, so a second one leading the
              ; prerequisites is the marker. Left in place it reads as a static pattern
              ; rule with an empty target pattern, and the rule vanishes.
              pl (ltrim prtext0)
              dbl ((0 < tally pl) && (pl 0 = 58))
              prtext (? dbl (snip pl 1 (tally pl)) prtext0)
              tgts (expand-words [tgtstext] as)
              sp (? (two? raws) (sufpat <tgts) 0)   ; an old-style suffix rule, .c.o style
              (? sp (pin patbox 0 ([<sp (list <>sp) () raws] >< peep patbox 0 0))
                 (any (\ t ((0 < tally t) && (t 0 = 46))) tgts) 0        ; drop .PHONY / .SUFFIXES / ...
                 (: cp (splittop prtext 58)
                    (? (two? >cp)
                       ; static pattern rule
                       (: tpat <(expand-words [<cp] as) ppats (expand-words [<>cp] as)
                          (each tgts (\ t (: m (pmatch tpat t)
                             (? (two? m) (: stem <m ping (map (\ pp (stemsub pp stem)) ppats)
                                            (reg (mk-item t) (map mk-item ping) 0 (expand-steps raws (mk-item t) ping stem as))) 0)))))
                       ; implicit pattern rule, or normal rule
                       (: sb (splitbar (expand-words [prtext] as)) prs <sb oos >sb
                          (? (any haspct tgts)
                             ; an empty recipe is make's cancellation idiom ("an empty recipe
                             ; unmakes the rule"). we carry no lex builtin, so cancelling is
                             ; registering nothing. (a cancellation over OUR builtpats has no
                             ; consumer yet; it would filter there.)
                             (? !(two? raws) 0
                                (each tgts (\ t (? (haspct t) (pin patbox 0 ([t prs oos raws] >< peep patbox 0 0)) 0))))
                             ; the automatics read the target's whole prerequisite list,
                             ; which an earlier line may have stated and this one not repeat:
                             ;   a.out: a.c
                             ;   a.out b.out: ; cc $<
                             ; make expands $</$^ when the recipe runs, against everything
                             ; merged; cook expands here, so it asks the card already
                             ; registered before adding this line's own words. (prereqs
                             ; arriving after their recipe still read short -- that one wants
                             ; expansion deferred to prep, a bigger change than this.)
                             (each tgts (\ t (: it (mk-item t) ps (map mk-item prs)
                                                all (adds <(peep cards (unseat it) '(())) ps)
                                                ((? dbl regd reg) it ps (map mk-item oos)
                                                     (expand-steps raws it all "" as)))))))))))))
      ; make's .DEFAULT_GOAL overrides "first rule declared" -- it is a plain
      ; variable, so it survives the dot-target drop above and lands in `as`.
      dg (trim (peep as ".DEFAULT_GOAL" ""))
      _ (? (0 < tally dg) (pin check 0 (mk-item dg)) 0)
      patlist (peep patbox 0 0)
      ; make's builtin implicit rules -- the database a Makefile never has to state,
      ; and the reason lua 5.4 writes recipes for three of its 34 objects and leaves
      ; the rest to the tool. same shape as a file's own pattern: (tgtpat prs oos raws).
      ; two fences, and both are load-bearing. tried only after the file's patterns,
      ; so a Makefile that states its own %.o: %.c still wins outright. and taken only
      ; when the stem-source exists: `%: %.c` matches literally every word, so a phony
      ; `all` that carries prereqs but no recipe of its own would otherwise resolve to
      ; a link of `all.c` and fail asking for a file nobody ever mentioned.
      ; a raw recipe line is (silent ignore . text), what parse-mods answers -- so
      ; the builtins are built through it rather than written as bare strings, and go
      ; down the one door the file's own recipes take. a plain string here reads as a
      ; recipe of no lines: the card resolves, nothing runs, and the miss surfaces
      ; later wearing somebody else's name.
      (bpat tgt prs cmd) (list tgt prs () (list (parse-mods cmd)))
      builtpats
       (list (bpat "%.o" '("%.c") "$(CC) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<")
             (bpat "%.o" '("%.S") "$(CC) $(ASFLAGS) $(CPPFLAGS) -c -o $@ $<")
             (bpat "%.o" '("%.s") "$(AS) $(ASFLAGS) -o $@ $<"))
      ; make's match-anything builtins -- `%: %.c` and `%: %.o` -- are deliberately
      ; not here. they match every target there is, and GNU only dares them under
      ; conditions this resolver does not model (it will not apply one to a target
      ; that already has any rule, nor to an intermediate). without those conditions
      ; `%: %.o` hijacks a real link: this tree's own `out/host/love` has love.o
      ; sitting right there, so the builtin links that alone and the host objects and
      ; the math floor never reach the command -- a wall of undefined references from
      ; a rule nobody wrote. the narrow %.o: rules above cannot do that: their target
      ; must already end in .o.
      (psrc p item) (: stem <(pmatch <p item) prs <>p (? (two? prs) (stemsub <prs stem) ""))
      ; a pattern whose source cannot be had does not apply -- make rejects it and
      ; tries the next, and with none left the target is a source leaf that is
      ; already on disk. Taking one as a last resort demands a file nobody makes:
      ; `%.c: %.l` (mk/common.mk cancels make's lex rule, since this tree is full of
      ; `<name>.l` beside `<name>.c`) had cook asking for core/love.l. "Can be had"
      ; is exists-or-has-a-card -- no intermediates, per the note above.
      (phave? p item) (: s (psrc p item) (|| (exists? s) (two? (peep cards s ()))))
      (best-of ms item)        ; the shortest stem among the patterns that apply
      (: ok (filter (\ p (phave? p item)) ms)
         (? !(two? ok) 0
            (foldl (\ b p (? !b p (? (tally <(pmatch <p item) < tally <(pmatch <b item)) p b))) 0 ok)))
      (builtin-of item)        ; the builtin lane: a match whose source is really there
      (: ms (filter (\ p (&& (two? (pmatch <p item)) (exists? (psrc p item)))) builtpats)
         (best-of ms item))
      (resolve item)           ; synthesize a card from the best matching pattern rule (merging an explicit prereq-only card)
      (: ms (filter (\ p (two? (pmatch <p item))) patlist)
         best (: b (best-of ms item) (? b b (builtin-of item)))
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

   ; -- emit: serialize the resolved cards back as a Cookfile ------------------
   ; after register-makefile (with emitp set) the cards hold concrete, fully
   ; expanded items and ("sh" "-c" cmd) steps. We walk the dependency graph from
   ; every root -- the default ticket first (so it stays the default in the output),
   ; then every other registered card -- resolving pattern rules to concrete cards
   ; exactly as prep does, and print each as a (recipe ..) / (reg ..) form. A
   ; pattern-only target with no card prints nothing (a source leaf).
   (qshow x) ("'" + show x)                  ; a quoted datum literal: 'all, '("a" b)
   ; the emitted Cookfile must not carry this seat's nest, or it installs into a
   ; home nobody else has: write $HOME and $PWD back over the resolved prefixes.
   ; the cwd goes first -- it usually sits under the home, and a $HOME pass
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
             ["(reg" (qshow (seat item)) (qshow (? (two? ings) (seatl ings) 0)) (qshow (seatl oo)) (estep steps)]
             ["(recipe" (qshow (seat item)) (qshow (? (two? ings) (seatl ings) 0)) (estep steps)])
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
   ; the love-recipe names, and there are only these: a Cards.l, a Cookfile, or
   ; anything else spelled .l.
   (lovefilep p) (: b (mk-base p) (mk-ends p ".l" || (b = "Cookfile")))
   ; -f names the makefile, whatever it is called -- make's does. Asking
   ; makefilep of an explicit -f meant `cook -f build.rules` read a Makefile as
   ; love source, and love's complaint about the first line (`;; missing CC`)
   ; reads as a broken build file rather than as cook looking at the wrong
   ; language. discovery still needs the name to tell it what it found; a -f
   ; already did, and only a love-recipe name argues back.
   (asmake? p explicit?) (? explicit? !(lovefilep p) (makefilep p))

   ; the build file may be named explicitly (the first arg word) or discovered in
   ; the cwd; the seat at the bottom already stripped the program head + -l chain.
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
   ; newer of the two, refresh the Cookfile from it before discovery picks it up;
   ; otherwise the Cookfile is preferred as-is. (date is () for a missing file.)
   (autoregen _)
   (: tm (date "Makefile") tc (date "Cookfile")
      (? (tm && tc && tm > tc) (regen-cookfile "Makefile" "Cookfile") 0))

   ; -- the command line ---------------------------------------------------------
   ; cook owns its own arg parse over the rebound argv (cli.l stops at the first
   ; non-flag; cook reads the rest via mk-args). Like make, it takes flags in any
   ; order and treats every non-flag word as a recipe to build:
   ;   -f, --file file   the build file (a Makefile or a Cookfile); else discovered
   ;   -j, --jobs N      run up to N recipes at once (default 1). N is required -- a
   ;                     bare -j is make's "unlimited", and one translation unit here
   ;                     peaks at 2.3 GB, so the number is the caller's to say.
   ;                     the width does not travel: there is no jobserver, so a
   ;                     recursive $(MAKE) runs at -j1 unless its own line says otherwise.
   ;   --emit            transpile a Makefile to a resolved Cookfile on stdout, then exit
   ;   -h, --help, help  print usage and exit
   ;   -v, --version, version   print the version and exit
   ;   recipe ...        the items to cook (default: the first recipe declared)
   ; for back-compat with `cook <Makefile> [recipe]`, when no -f is given a first
   ; positional word that names an existing file may be the build file -- but only where
   ; discovery would find nothing else, or where the word is the very file it would find.
   ; everywhere else a positional is a recipe, whether or not a file of that name exists.
   ; with -f, every positional word is a recipe.
   (bye s) (: _ (puts s) _ (putc 10) _ (flush out) (quit 0))
   (version _) (bye ("cook (love " + love-version + ")"))
   (usage _) (bye ("usage: cook [-f FILE] [-j N] [--emit] [-v] [-h] [VAR=VAL ...] [RECIPE ...]\n"
                   + "  -f, --file FILE   the build file (Makefile or Cookfile); else discovered\n"
                   + "  -j, --jobs N      run up to N recipes at once (default 1)\n"
                   + "      --emit        transpile a Makefile to a resolved Cookfile, then exit\n"
                   + "  -v, --version     print the version and exit\n"
                   + "  -h, --help        print this help and exit\n"
                   + "  VAR=VAL           a command-line variable (silences the file's assignments)\n"
                   + "  RECIPE ...        the items to build (default: the first recipe declared)\n"
                   + "a Makefile (Makefile/makefile/GNUmakefile/*.mk) is imported and cooked; any other build file is a Cookfile of ordinary love source."))
   (need-arg h r) (? (two? r) 0 (: _ (say err ("cook: " + h + " needs an argument")) _ (put err 10) (quit 2)))
   ; a job count: all digits and above zero, or the run ends here. `-j0` and `-j x`
   ; are the same mistake and get the same word.
   (jobn w)
   (: n (tally w)
      (go k a) (? (n <= k) a
                  (!(digitp (w k))) 0
                  (go (k + 1) (a * 10 + (w k) - 48)))
      v (? (0 < n) (go 0 0) 0)
      (? (0 < v) v (: _ (say err ("cook: -j wants a job count, not " + show w))
                      _ (put err 10) (quit 2))))
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
             ((w = "-j") || (w = "--jobs"))  (: _ (need-arg w r) _ (pin opt 'jobs (jobn <r)) (loop >r opt))
             ((2 < tally w) && ("-j" = snip w 0 2))          ; glued: -j8
                                             (: _ (pin opt 'jobs (jobn (snip w 2 (tally w)))) (loop r opt))
             ((w = "-n") || (w = "--dry-run") || (w = "--just-print"))
                                                  (: _ (pin opt 'dry 1) (loop r opt))
             (w = "--emit")                                       (: _ (pin opt 'emit 1) (loop r opt))
             ((w = "-h") || (w = "--help") || (w = "help"))      (: _ (pin opt 'help 1) (loop r opt))
             ((w = "-v") || (w = "--version") || (w = "version")) (: _ (pin opt 'ver 1) (loop r opt))
             (: vw (varw w)
                (? (two? vw) (: _ (pin opt 'var (peep opt 'var () + [vw])) (loop r opt))
                   (: _ (pin opt 'pos (peep opt 'pos () + [w])) (loop r opt))))))))
    ws (tablet 0))

   ; -- cook-main (this form's body ANSWERS it) --
   ; the whole job from the args: parse flags, find the build file, cook, and exit
   ; first (quit), so when fired from a -l preload the CLI driver never tries to
   ; read a Makefile (or re-read the chosen file) as l source. A Makefile is
   ; imported and its recipes cooked; a Cookfile is read and evaluated -- it drives
   ; cook itself, the ticket read live by (ticket 0) from the tickets box.
   ;
   ; it splices its own layer first, and owns that for every caller (the seat, kore's
   ; dispatcher, a host embedding cook): a Cookfile is love source read at runtime, so
   ; its bare (recipe ..) (cook-all 0) (ticket 0) only resolve on the module walk. one
   ; (use 'cook), at the one place that needs it -- a caller just needs the closure.
   (cook-main as)
   (: _ (? (lit? (from 'cook)) (use 'cook) 0)   ; splice my registered self if I am one (the module
                                                ; path); riding a cat, my names already walk -- the
                                                ; presence guard keeps the loader out of it
      opt (parse-args as)
      _ (? (peep opt 'help 0) (usage 0) 0)
      _ (? (peep opt 'ver 0) (version 0) 0)
      _ (pin clivars 0 (peep opt 'var ()))                   ; the VAR=VAL words, for seed-defaults
      _ (pin jobs 0 (peep opt 'jobs 1))                      ; the -j width; 1 is the serial cook
      _ (pin dryrun 0 (peep opt 'dry 0))                     ; -n: say the steps, run none
      emit? (peep opt 'emit 0)
      pos (peep opt 'pos ())
      xf (peep opt 'file 0)                                       ; an explicit -f file (0 if none)
      _ (? (xf && !(exists? xf)) (: _ (say err ("cook: cannot open " + xf)) _ (put err 10) (quit 1)) 0)
      ; a positional is a recipe wherever discovery can name the build file itself.
      ; taking any existing file meant `cook prog` read `prog` as the build file the moment
      ; a previous run had made one -- no rules in it, no tickets, exit 0 having built
      ; nothing, and the goal you named going silently unbuilt. the back-compat form is
      ; kept for the case it was written for: a build file discovery cannot find.
      disc0 (? xf 0 (discover 0))                                    ; what discovery would name, if anything
      posfile (? (!xf && two? pos && exists? <pos && (? disc0 (disc0 = <pos) 1)) <pos 0)
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
            _ (? (asmake? file xf)
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

; the seat (tools/ain.l's shape): fire cook-main iff this very file -- or the
; installed `cook` symlink -- sits in the program seat of the real command line
; (positional program or a -l preload), taking the args after the seat. cmdline
; is the full C argv, never rebound, so the read is the same in both load
; orders; inside the multi-call `kore` nothing matches and kore's dispatcher calls
; cook-main itself. body-having: the seat machinery leaks nothing.
;
; cook-main reads bare: the module's layer holds this file's binds and is still
; the head here. cook-main splices its registered self when called from outside.
(: _ (pins (from 'verbs 'tab) 'cook cook-main)
   s ((from 'verbs 'fire) ["cook" "cook.l"])
   (? (two? s) (cook-main >s) ()))

) ; module 'cook
