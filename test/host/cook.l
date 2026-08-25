; the gate for cook's Makefile importer (crew/cook/cook.l).
;
; Run on the host binary:  out/host/love test/host/cook.l
; (wired into the build as `make test_cook`, part of `make test`).
;
; cook reads a real Makefile -- variable expansion, the make functions,
; conditionals, include, pattern + static-pattern rules -- so these tests build
; fixture Makefiles (a flat list of lines, joined by newlines), run cook on them
; as a SUBPROCESS (the host `hark` nif), and assert over its captured stdout. The
; recipes mostly `echo` a computed value, so the assertion is a substring check;
; the pattern-rule tests build real files and check they appear.

(: dir "/tmp/cooktest"
   love  "out/host/love"                          ; run from the project root (where make runs)

   ; -- a tiny assert harness (this file is standalone, not in the corpus) --
   pass (tablet 0) fail (tablet 0)
   _ (pin pass 0 0) _ (pin fail 0 0)
   (ok nm c) (? c (pin pass 0 (+ 1 (peep pass 0 0)))
                (: _ (pin fail 0 (+ 1 (peep fail 0 0)))
                    _ (say out (+ "FAIL " nm)) (put out 10)))

   ; join a list of lines into Makefile text (each line gets a trailing newline).
   (mk l) (? (two? l) (+ <l (+ "\n" (mk >l))) "")
   ; does string s contain the substring sub?
   (has s sub) ((: (g i) (? (> (+ i (tally sub)) (tally s)) 0
                            (? (= sub (snip s i (+ i (tally sub)))) 1 (g (+ 1 i))))) 0)

   (write-file path content) (: q (open path "w") _ (say q content) (close q))
   (sh cmd) (hark ["sh" "-c" cmd])
   ; run cook on a fixture Makefile, return its captured stdout.
   ; ⚠ -f NAMES THE BUILD FILE. a positional is a goal wherever discovery can name the
   ; build file itself, and every spawn here runs from the repo root, where it names
   ; ./Makefile -- so a positional fixture would read as a goal against THIS tree.
   (run-cook src tgt)
   (: _ (write-file (+ dir "/Makefile") src)
       r (hark [love "-l" "crew/cook/cook.l" "-f" (+ dir "/Makefile") tgt])
       (? (two? r) >r ""))
   ; --emit a fixture Makefile, return the generated Cookfile text (stdout).
   (run-emit src)
   (: _ (write-file (+ dir "/Makefile") src)
       r (hark [love "-l" "crew/cook/cook.l" "--emit" "-f" (+ dir "/Makefile")])
       (? (two? r) >r ""))

   _ (sh (+ "rm -rf " (+ dir "; mkdir -p " dir)))

   ; ---------------------------------------------------------------- expansion
   ev (run-cook (mk '("R := ."
                      "ho = out/host"
                      "love_c = $R/core/love.c $R/data.c"
                      "h_o = $(love_c:$(R)/%.c=$(ho)/%.o)"
                      "suf = $(love_c:.c=.o)"
                      "all:"
                      "\techo \"E1[$(h_o)]\""
                      "\techo \"E2[$(suf)]\""
                      "\techo \"E3[$R/$(ho)]\"")) "all")
   _ (ok "nested-paren + pattern subst-ref" (has ev "E1[out/host/core/love.o out/host/data.o]"))
   _ (ok "suffix subst-ref"                 (has ev "E2[./core/love.o ./data.o]"))
   _ (ok "single-char $R + $(VAR)"          (has ev "E3[./out/host]"))

   ; ---------------------------------------------------------------- functions
   fn (run-cook (mk '("files = a.c b.c sub/c.c"
                       "empty ="
                       "all:"
                       "\techo \"F1[$(dir $(files))]\""
                       "\techo \"F2[$(notdir $(files))]\""
                       "\techo \"F3[$(patsubst %.c,%.o,$(files))]\""
                       "\techo \"F4[$(filter-out sub/%,$(files))]\""
                       "\techo \"F5[$(filter %.c,$(files))]\""
                       "\techo \"F6[$(sort b a c a)]\""
                       "\techo \"F7[$(subst x,y,axbx)]\""
                       "\techo \"F8[$(strip  a   b  )]\""
                       "\techo \"F9[$(addprefix o/,x y)]\""
                       "\techo \"F10[$(addsuffix .o,x y)]\""
                       "\techo \"F11[$(firstword x y z)]\""
                       "\techo \"F12[$(words a b c)]\""
                       "\techo \"F13[$(if $(empty),yes,no)]\""
                       "\techo \"F14[$(if x,yes,no)]\""
                       "\techo \"F15[$(shell echo SH)]\"")) "all")
   _ (ok "dir"        (has fn "F1[./ ./ sub/]"))
   _ (ok "notdir"     (has fn "F2[a.c b.c c.c]"))
   _ (ok "patsubst"   (has fn "F3[a.o b.o sub/c.o]"))
   _ (ok "filter-out" (has fn "F4[a.c b.c]"))
   _ (ok "filter"     (has fn "F5[a.c b.c sub/c.c]"))
   _ (ok "sort+uniq"  (has fn "F6[a b c]"))
   _ (ok "subst"      (has fn "F7[ayby]"))
   _ (ok "strip"      (has fn "F8[a b]"))
   _ (ok "addprefix"  (has fn "F9[o/x o/y]"))
   _ (ok "addsuffix"  (has fn "F10[x.o y.o]"))
   _ (ok "firstword"  (has fn "F11[x]"))
   _ (ok "words"      (has fn "F12[3]"))
   _ (ok "if (false)" (has fn "F13[no]"))
   _ (ok "if (true)"  (has fn "F14[yes]"))
   _ (ok "shell"      (has fn "F15[SH]"))

   ; ------------------------------------------------------------- conditionals
   cnd (run-cook (mk '("x = 1"
                        "ifeq ($(x),1)"  "a = eqyes"  "else" "a = eqno"  "endif"
                        "ifneq ($(x),1)" "b = neqyes" "else" "b = neqno" "endif"
                        "ifdef x"  "c = def"  "endif"
                        "ifndef y" "d = ndef" "endif"
                        "all:"
                        "\techo \"C[$(a) $(b) $(c) $(d)]\"")) "all")
   _ (ok "ifeq/ifneq/ifdef/ifndef" (has cnd "C[eqyes neqno def ndef]"))

   ; a conditional INSIDE a recipe block selects which recipe lines run.
   rci (run-cook (mk '("on = 1" "all:" "\techo R-a"
                        "ifeq ($(on),1)" "\techo R-b" "endif" "\techo R-c")) "all")
   _ (ok "recipe-internal ifeq" (has rci "R-b" && has rci "R-c"))

   ; -------------------------------------------------------------------- include
   ; (make resolves an include relative to the cwd; the fixture lives elsewhere,
   ; so name it absolutely.)
   _ (sh (+ "mkdir -p " (+ dir "/mk")))
   _ (write-file (+ dir "/mk/common.mk") "FROM_INC = included-ok\n")
   inc (run-cook (mk [(+ "include " (+ dir "/mk/common.mk")) "all:" "\techo \"I[$(FROM_INC)]\""]) "all")
   _ (ok "include" (has inc "I[included-ok]"))

   ; ----------------------------------------------------- automatic vars + dir
   av (run-cook (mk '("all: foo/bar.o"
                       "foo/bar.o:" "\techo \"A[@=$@ star=$* d=$(dir $@)]\"")) "foo/bar.o")
   _ (ok "auto-vars + $(dir $@)" (has av "A[@=foo/bar.o star= d=foo/]"))

   ; --------------------------------------------------------- pattern rules
   ; build x.o from x.c via an implicit pattern rule; check the file appears.
   _ (sh (+ "mkdir -p " (+ (+ dir "/p; echo SRC > ") (+ dir "/p/x.c"))))
   _ (run-cook (mk [(+ "all: " (+ dir "/p/x.o"))
                     (+ (+ dir "/p/%.o: ") (+ dir "/p/%.c")) "\tcp $< $@"]) (+ dir "/p/x.o"))
   _ (ok "pattern rule builds target" (= 0 <(sh (+ "test -f " (+ dir "/p/x.o")))))

   ; static pattern rule: targets: tgtpat: prereqpats
   _ (sh (+ "echo S > " (+ dir "/p/y.in")))
   _ (run-cook (mk [(+ "outs = " (+ dir "/p/y.out"))
                     "all: $(outs)"
                     (+ "$(outs): " (+ dir (+ "/p/%.out: " (+ dir "/p/%.in")))) "\tcp $< $@"]) "all")
   _ (ok "static pattern rule builds target" (= 0 <(sh (+ "test -f " (+ dir "/p/y.out")))))

   ; the CANCELLATION idiom: an EMPTY recipe unmakes a pattern rule ("an empty
   ; recipe unmakes the rule" -- mk/common.mk cancels make's lex builtin exactly
   ; so). a live `%.c: %.l` would make every .c demand a sibling .l.
   _ (sh (+ "echo C > " (+ dir "/p/n.c")))
   ca (run-cook (mk ["%.c: %.l"
                     (+ "all: " (+ dir "/p/n.c")) "\t@echo \"CA[ok]\""]) "all")
   _ (ok "an empty pattern recipe cancels, not registers" (has ca "CA[ok]"))

   ; ---------------------------------------------- the BUILTIN implicit rules
   ; make's own database, which a Makefile never states: lua 5.4 writes recipes for
   ; three of its 34 objects and leaves the rest to it. ⚠ CC is overridden per-run so
   ; the test never depends on a host compiler.
   ; the assertion is on the COMMAND cook issues, not on an object -- so the gate
   ; needs no host compiler, and it reads the rule's shape (-c -o $@ $<) directly.
   _ (sh (+ "mkdir -p " (+ dir "/b; echo SRCB > " (+ dir "/b/z.c"))))
   bi (run-cook (mk ["CC = echo-cc"
                     (+ "all: " (+ dir "/b/z.o")) "\t@echo BUILTIN-DONE"]) "all")
   _ (ok "builtin %.o: %.c fires" (&& (has bi "echo-cc") (has bi "-c -o") (has bi "z.c")))
   ; ..and the MATCH-ANYTHING builtins (`%: %.c`, `%: %.o`) must stay OUT: they match
   ; every target there is, and this resolver does not model the conditions GNU only
   ; dares them under. with `x.o` on disk, a link target `x` must still be the
   ; Makefile's own recipe -- not a builtin linking that one object alone.
   _ (sh (+ "echo O > " (+ dir "/b/lone.o")))
   ma (run-cook (mk [(+ "all: " (+ dir "/b/lone")) (+ (+ dir "/b/lone: ") (+ dir "/b/lone.o"))
                     "\t@echo \"MA[mine]\""]) "all")
   _ (ok "no match-anything builtin hijacks a link" (has ma "MA[mine]"))

   ; -------------------------------------------------- old-style suffix rules
   ; `.c.o:` IS `%.o: %.c` -- st and every suckless Makefile is written this way, and
   ; the dot-target drop would otherwise eat it in SILENCE, leaving the builtin above
   ; to compile the file with default flags instead of the Makefile's own.
   _ (sh (+ "echo SRCS > " (+ dir "/b/s.c")))
   _ (run-cook (mk [".SUFFIXES: .c .o"
                    ".c.o:" "\tcp $< $@"
                    (+ "all: " (+ dir "/b/s.o"))]) "all")
   _ (ok "suffix rule .c.o builds the target" (= 0 <(sh (+ "test -f " (+ dir "/b/s.o")))))

   ; ------------------------------------------------------------------ .POSIX
   ; not decoration: it moves make's DEFAULTS to c99 and -O1, which is where st's
   ; whole build gets its flags. as defaults, so a file's own CC= still wins.
   px (run-cook (mk '(".POSIX:" "all:" "\t@echo \"PX[$(CC) $(CFLAGS)]\"")) "all")
   _ (ok ".POSIX moves CC/CFLAGS" (has px "PX[c99 -O1]"))
   nx (run-cook (mk '("all:" "\t@echo \"NX[$(CC)]\"")) "all")
   _ (ok "without .POSIX, CC stays cc" (has nx "NX[cc]"))

   ; ------------------------------------------------- $@ of a PHONY target
   ; a phony item is a SYMBOL, and an automatic must read as the NAME either way --
   ; lua's top Makefile is `all: guess` + `cd src && $(MAKE) $@`, and an empty $@
   ; hands the recursive make no target at all.
   pa (run-cook (mk '(".PHONY: guess" "all: guess" "guess:" "\t@echo \"PA[$@]\"")) "all")
   _ (ok "$@ of a phony target is its name" (has pa "PA[guess]"))

   ; ------------------------------------------------------------- assignment ops
   ops (run-cook (mk '("a := base" "a += more" "b ?= keep" "b ?= nope"
                        "all:" "\techo \"O[$(a)|$(b)]\"")) "all")
   _ (ok "+= and ?=" (has ops "O[base more|keep]"))

   ; ------------------------------------- the environment, the command line, origin
   ; cook seeds its variable table the way make does: the environment first, then
   ; the builtin defaults, then the command line's VAR=VAL words -- which SILENCE
   ; file assignments to the same name, unless the file says `override`. run-cook's
   ; subprocess inherits this process's environment, so setenv reaches the fixture.
   (D2 f) (+ dir f)
   (run-cookv src args)
   (: _ (write-file (+ dir "/Makefile") src)
       r (hark ([love "-l" "crew/cook/cook.l" "-f" (+ dir "/Makefile")] + args))
       (? (two? r) >r ""))
   _ (setenv "COOKT_ENV" "from-env")
   env (run-cookv (mk '("COOKT_ENV ?= nope"
                        "fv = infile"
                        "all:"
                        "\techo \"N1[$(COOKT_ENV)]\""
                        "\techo \"N2[$(origin COOKT_ENV) $(origin fv) $(origin COOKT_NO)]\""))
                  '("all"))
   _ (ok "environment import (and ?= yields to it)" (has env "N1[from-env]"))
   _ (ok "$(origin): environment / file / undefined"
         (has env "N2[environment file undefined]"))
   _ (setenv "COOKT_ENV" ())
   cli (run-cookv (mk '("cv = filewins"
                        "override ov := forced"
                        "all:"
                        "\techo \"L[$(cv) $(ov) $(origin cv)]\""))
                  '("all" "cv=cli" "ov=cli"))
   _ (ok "a command-line VAR=VAL silences the file assignment"
         (has cli "L[cli forced command line]"))
   xp (run-cook (mk '("export EX := exported"
                      "all:"
                      "\techo \"X[$$EX]\"")) "all")
   _ (ok "export hands the value to the recipe's environment" (has xp "X[exported]"))
   cal (run-cook (mk '("greet = hi-$(1)-$(2)"
                       "g := $(call greet,a,b)"
                       "all:"
                       "\techo \"G[$(g)]\"")) "all")
   _ (ok "$(call) binds $1..$n over the body" (has cal "G[hi-a-b]"))

   ; ------------------------------------------------- @D/@F, wildcard, abspath
   adf (run-cook (mk '("all: foo/bar.o"
                       "foo/bar.o:" "\techo \"D[$(@D) $(@F)]\"")) "foo/bar.o")
   _ (ok "$(@D) and $(@F)" (has adf "D[foo bar.o]"))
   _ (sh (+ "mkdir -p " (+ (D2 "/w; touch ") (+ (D2 "/w/b.c ") (+ (D2 "/w/a.c ") (D2 "/w/.h.c"))))))
   wc (run-cook (mk [(+ "ws := $(wildcard " (+ (D2 "/w/*.c") ")"))
                     (+ "ex := $(wildcard " (+ (D2 "/w/a.c") ")"))
                     "no := $(wildcard nothing/here/*.q)"
                     "all:"
                     "\techo \"W[$(notdir $(ws))|$(notdir $(ex))|$(no)]\""]) "all")
   _ (ok "wildcard: sorted glob, no dotfiles, existence word, empty miss"
         (has wc "W[a.c b.c|a.c|]"))
   ab (run-cook (mk '("p := $(abspath /x//y/./../z)"
                      "all:" "\techo \"P[$(p)]\"")) "all")
   _ (ok "abspath cleans // . .." (has ab "P[/x/z]"))

   ; ---------------------------------------------------------------------- emit
   ; --emit transpiles a Makefile to a RESOLVED Cookfile: vars/patterns expanded,
   ; .PHONY targets -> symbols, files -> strings, each recipe line an
   ; ("sh" "-c" cmd) step, the default ticket first, a trailing (cook (ticket 0)).
   ; ⚠ THE SOURCES ARE REAL, like every other pattern test here (the head note says
   ; so). A pattern whose source cannot be had does not apply -- make rejects it
   ; whether the target exists or not (asked, both ways) -- and emit resolves against
   ; the disk it is run on. A fixture with no .c proves nothing about resolution; this
   ; one passed only on the last-resort fallback 4bfac33b removed.
   _ (sh (+ "mkdir -p " (+ dir "/e; touch " (+ dir "/e/a.c " (+ dir "/e/b.c")))))
   eo (+ dir "/e/a.o " (+ dir "/e/b.o"))
   em (run-emit (mk ["CC := cc"  (+ "OBJ := " eo)  ".PHONY: all clean"
                      "all: prog"
                      "prog: $(OBJ)"  "\t$(CC) -o $@ $(OBJ)"
                      "%.o: %.c"      "\t$(CC) -c -o $@ $<"
                      "clean:"        "\t-rm -f prog $(OBJ)"]))
   _ (ok "emit: .PHONY target -> symbol"   (has em "(recipe 'all "))
   _ (ok "emit: file target -> string"     (has em "(recipe '\"prog\" "))
   _ (ok "emit: var-resolved step"         (has em (+ "(\"sh\" \"-c\" \"cc -o prog " (+ eo "\")"))))
   _ (ok "emit: pattern-resolved step"     (has em (+ "(\"sh\" \"-c\" \"cc -c -o " (+ dir (+ "/e/a.o " (+ dir "/e/a.c\")"))))))
   _ (ok "emit: default ticket emitted first" (has em "(recipe 'all "))
   _ (ok "emit: trailing cook call"        (has em "(cook-all 0)"))

   ; round-trip: cook the emitted Cookfile and confirm it runs the recipe. The
   ; rule just writes a file (no compiler / source lookup), so the test is hermetic.
   _ (sh (+ "mkdir -p " (+ dir "/e")))
   emc (run-emit (mk [".PHONY: all"
                       (+ "all: " (+ dir "/e/out.txt"))
                       (+ dir "/e/out.txt:") "\techo HI > $@"]))
   _ (write-file (+ dir "/e/Cookfile") emc)
   _ (sh (+ "rm -f " (+ dir "/e/out.txt")))
   _ (hark [love "-l" "crew/cook/cook.l" "-f" (+ dir "/e/Cookfile") "all"])
   _ (ok "emit: round-trip cooks the emitted Cookfile" (= 0 <(sh (+ "test -f " (+ dir "/e/out.txt")))))

   ; .DEFAULT_GOAL overrides "the first rule declared" as the default ticket --
   ; both when cooking the Makefile directly and through the emitted Cookfile
   ; (which must therefore lead with that recipe).
   dgsrc (mk '(".PHONY: first second"
               ".DEFAULT_GOAL := second"
               "first:"  "\techo FIRST"
               "second:" "\techo SECOND"))
   _ (write-file (+ dir "/Makefile") dgsrc)
   dgr (: r (hark [love "-l" "crew/cook/cook.l" "-f" (+ dir "/Makefile")]) (? (two? r) >r ""))
   _ (ok ".DEFAULT_GOAL is the default ticket" (has dgr "SECOND"))
   _ (write-file (+ dir "/e/Cookfile") (run-emit dgsrc))
   dgc (: r (hark [love "-l" "crew/cook/cook.l" "-f" (+ dir "/e/Cookfile")]) (? (two? r) >r ""))
   _ (ok "emit: .DEFAULT_GOAL survives the round-trip" (has dgc "SECOND"))

   ; ------------------------------------------------- side-effect writes are seen
   ; a step may write anything, not only its own target (make's two-outputs-one-recipe
   ; idiom writes a sibling), so an item's date must be read FRESH when its turn comes:
   ; `first` writes a file it does not own, and `second` must see the new one. (this
   ; once guarded the warmed stat(1) date cache; the stat nif retired the cache, and
   ; this fixture is what fails if any date memo ever comes back unsoundly.)
   ; (⚠ every path in a fixture recipe must be ABSOLUTE: cook runs from the project root,
   ;  not from `dir`, so a bare `> out.txt` writes into the repo.)
   (D f) (+ dir f)
   _ (sh (+ "echo OLD > " (+ (D "/shared.txt") (+ " && sleep 0.01 && echo STALE > " (D "/second")))))
   sidesrc (mk [".PHONY: all first"
                (+ "all: first " (D "/second"))
                "first:" (+ "\techo NEW > " (D "/shared.txt"))
                (+ (D "/second: ") (D "/shared.txt"))
                (+ "\tcp " (+ (D "/shared.txt") (+ " " (D "/second"))))])
   _ (write-file (D "/Makefile") sidesrc)
   _ (: r (hark [love "-l" "crew/cook/cook.l" "-f" (D "/Makefile") "all"]) 0)
   side (: r (sh (+ "cat " (D "/second"))) (? (two? r) >r ""))
   _ (ok "a recipe's side-effect write invalidates the date cache" (has side "NEW"))

   ; (the cache's OTHER rule -- it records only what stat actually reported, never a miss
   ;  -- has no fixture here, and deliberately: from a Makefile there is no way to make
   ;  `stat` itself fail, which is the case it guards. it costs speed, never an answer.)

   ; ------------------------------------------------------- .ONESHELL / .SHELLFLAGS
   ; make runs each recipe line in its OWN shell, which is why a recipe with any
   ; state has to be one \-joined line. .ONESHELL hands the whole recipe to one
   ; shell instead, so a variable set on one line is live on the next -- the single
   ; observable that tells the two apart, and the reason a recipe can read as a
   ; script. Differential against GNU make, not just against itself.
   one (run-cook (mk '(".ONESHELL:"
                       ".SHELLFLAGS := -ec"
                       "all:"
                       "\tx=hello"
                       "\techo \"O1[$$x world]\"")) "all")
   _ (ok ".ONESHELL: one shell for the whole recipe" (has one "O1[hello world]"))
   ; ..and WITHOUT it the default still holds: separate shells, so x is gone
   sep (run-cook (mk '("all:"
                       "\tx=hello"
                       "\techo \"O2[$$x world]\"")) "all")
   _ (ok "no .ONESHELL: a line still gets its own shell" (has sep "O2[ world]"))
   ; ⚠ with one shell, a plain -c checks only the LAST line's status, so a failing
   ; middle line would pass SILENTLY. -e is what keeps a recipe honest: the third
   ; line must never run. The recipe is SILENCED (@ on the first line governs the
   ; lot) because a one-shell recipe is echoed WHOLE -- an un-silenced echo would
   ; print the text of the third line and the assertion would read its own echo.
   ec (run-cook (mk '(".ONESHELL:"
                      ".SHELLFLAGS := -ec"
                      "all:"
                      "\t@echo O3-one"
                      "\tfalse"
                      "\techo O3-three")) "all")
   _ (ok ".SHELLFLAGS -ec: a failing middle line stops the recipe"
         (has ec "O3-one" && !(has ec "O3-three")))

   ; ------------------------------------------------------- multi-target rules
   ; `a b: src` is ONE rule registered under BOTH targets (the Makefile's
   ; love/love.cand link rule is this shape). the target span runs to the colon,
   ; BLANKS INCLUDED -- the old span stopped at whitespace, so the whole line
   ; classified 'skip and every multi-target rule silently vanished; a built
   ; tree masked it, since an existing output reads as a source leaf. asking
   ; cook for the SECOND target is what proves the registration went wide.
   _ (sh (+ "echo x > " (D "/msrc")))
   _ (run-cook (mk [(+ (D "/m1.out ") (+ (D "/m2.out: ") (D "/msrc")))
                    (+ "\techo M4 > " (D "/m1.out"))
                    (+ "\techo M4 > " (D "/m2.out"))])
               (D "/m2.out"))
   m2 (: r (sh (+ "cat " (D "/m2.out"))) (? (two? r) >r ""))
   _ (ok "a multi-target rule registers every target" (has m2 "M4"))

   ; --------------------------------------------------- modifiers via expansion
   ; make reads the @/-/+ prefixes AFTER expansion (the `Q = @` verbose-build
   ; idiom; mk/lib.mk's lcat_h leads with @ from inside the variable) -- an
   ; expanded @ silences the line, it never reaches the shell as a word.
   qm (run-cook (mk '("Q = @"
                      "all:"
                      "\t$(Q)echo M5[quiet]")) "all")
   _ (ok "an expanded @ is a modifier, not a word"
         (has qm "M5[quiet]" && !(has qm "@echo")))

   ; ------------------------------------------------------------ SHELL + wildcard
   ; SHELL picks the shell recipe lines AND $(shell) ride -- observable through
   ; $0, which lush pins to "lush" in its -c lane (a stock sh answers "sh").
   ; the repo's own shell is the fixture's shell: make-with-lush, the rung-4
   ; distro loop closed. $(wildcard) is lush's pure glob at the repo root
   ; (sorted; a no-match pattern contributes nothing -- nullglob).
   shl (run-cook (mk '("SHELL := out/host/lush"
                       "all:"
                       "\t@echo \"L1[$$0]\""
                       "\t@echo \"L2[$(shell echo $$0)]\""))
                 "all")
   _ (ok "SHELL: recipe lines ride lush"   (has shl "L1[lush]"))
   _ (ok "SHELL: $(shell) rides lush too"  (has shl "L2[lush]"))
   wld (run-cook (mk '("all:"
                       "\t@echo \"WC[$(wildcard crew/lush/jo*.l crew/lush/zz-nope*.q)]\""))
                 "all")
   _ (ok "wildcard: pure-love glob, nullglob" (has wld "WC[crew/lush/job.l]"))

   ; ------------------------------------------------------------------ -j jobs
   ; the job pool: under -j N a stale recipe runs in a forked child. THE CLAIM IS
   ; THAT WIDTH CHANGES NOTHING BUT THE WALL CLOCK, so every case here asks the same
   ; fixture at -j4 and answers it against the serial law -- not against a timing,
   ; which is the one thing a gate cannot hold still.
   (run-cookj src tgt j)
   (: _ (write-file (+ dir "/Makefile") src)
       r (hark [love "-l" "crew/cook/cook.l" "-j" j "-f" (+ dir "/Makefile") tgt])
       (? (two? r) >r ""))
   _ (sh (+ "mkdir -p " (D "/j")))
   jsrc (mk [".PHONY: all"
             (+ "all: " (+ (D "/j/a ") (+ (D "/j/b ") (+ (D "/j/c ") (D "/j/d")))))
             "\t@echo J[all]"
             (D "/j/a:") (+ "\t@echo A > " (D "/j/a"))
             (D "/j/b:") (+ "\t@echo B > " (D "/j/b"))
             (D "/j/c:") (+ "\t@echo C > " (D "/j/c"))
             (D "/j/d:") (+ "\t@echo D > " (D "/j/d"))])
   _ (sh (+ "rm -f " (D "/j/[abcd]")))
   j1 (run-cookj jsrc "all" "1")
   _ (sh (+ "rm -f " (D "/j/[abcd]")))
   j4 (run-cookj jsrc "all" "4")
   _ (ok "-j4 answers what -j1 answered" (= j1 j4))
   _ (ok "-j4 ran every independent recipe"
         (= 0 <(sh (+ "test -f " (+ (D "/j/a") (+ " && test -f " (+ (D "/j/b")
                      (+ " && test -f " (+ (D "/j/c") (+ " && test -f " (D "/j/d")))))))))))
   ; a glued -jN is the same flag
   _ (sh (+ "rm -f " (D "/j/[abcd]")))
   jg (: r (hark [love "-l" "crew/cook/cook.l" "-j4" "-f" (+ dir "/Makefile") "all"]) (? (two? r) >r ""))
   _ (ok "-j4 glued reads as -j 4" (= j1 jg))

   ; an EDGE still orders: one must finish before two starts, however wide the pool.
   _ (sh (+ "rm -f " (+ (D "/j/log") (+ " " (+ (D "/j/one ") (D "/j/two"))))))
   jchain (mk [".PHONY: all"
               (+ "all: " (D "/j/two"))
               (D "/j/one:") (+ "\t@sleep 1; echo one >> " (+ (D "/j/log") (+ "; touch " (D "/j/one"))))
               (+ (D "/j/two: ") (D "/j/one")) (+ "\t@echo two >> " (+ (D "/j/log") (+ "; touch " (D "/j/two"))))])
   _ (run-cookj jchain "all" "4")
   jlog (: r (sh (+ "cat " (D "/j/log"))) (? (two? r) >r ""))
   _ (ok "-j4 keeps an edge in order" (has jlog "one\ntwo"))

   ; a red recipe under -j is still red, and its siblings are let finish rather than
   ; killed -- a half-written file left behind would read as up to date next run.
   _ (sh (+ "rm -f " (+ (D "/j/ok ") (D "/j/red"))))
   jred (mk [".PHONY: all"
             (+ "all: " (+ (D "/j/ok ") (D "/j/red")))
             (D "/j/ok:") (+ "\t@sleep 1; echo OK > " (D "/j/ok"))
             (D "/j/red:") "\t@exit 3"])
   _ (write-file (+ dir "/Makefile") jred)
   jrst (: r (hark [love "-l" "crew/cook/cook.l" "-j" "4" "-f" (+ dir "/Makefile") "all"]) (? (two? r) <r 0))
   _ (ok "-j4: a failed recipe fails the run" (!(= 0 jrst)))
   _ (ok "-j4: outstanding jobs finish first" (= 0 <(sh (+ "test -f " (D "/j/ok")))))

   ; the width is the caller's to say: a bare -j is make's "unlimited", and one
   ; translation unit here peaks at 2.3 GB, so cook asks for the number instead.
   jbad (: r (hark [love "-l" "crew/cook/cook.l" "-j" "x" "-f" (+ dir "/Makefile") "all"]) (? (two? r) <r 0))
   _ (ok "-j wants a count" (!(= 0 jbad)))

   ; ---------------------------------------------------------------- summary
   np (peep pass 0 0) nf (peep fail 0 0)
   _ (puts (+ (show np) (+ " cook tests pass" (? (< 0 nf) (+ ", " (+ (show nf) " FAIL")) ""))))
   _ (putc 10)
   (? (< 0 nf) (quit 1) 0))
