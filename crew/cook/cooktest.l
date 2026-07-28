; cooktest -- tests for cook's Makefile importer (crew/cook/cook.l).
;
; Run on the host binary:  out/host/love tools/cooktest.l
; (wired into the build as `make test_cook`, part of `make test`).
;
; cook reads a real Makefile -- variable expansion, the make functions,
; conditionals, include, pattern + static-pattern rules -- so these tests build
; fixture Makefiles (a flat list of lines, joined by newlines), run cook on them
; as a SUBPROCESS (the host `run` nif), and assert over its captured stdout. The
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
   (mk l) (? (two? l) (+ (cap l) (+ "\n" (mk (cup l)))) "")
   ; does string s contain the substring sub?
   (has s sub) ((: (g i) (? (> (+ i (tally sub)) (tally s)) 0
                            (? (= sub (snip s i (+ i (tally sub)))) 1 (g (+ 1 i))))) 0)

   (write-file path content) (: q (open path "w") _ (say q content) (close q))
   (sh cmd) (run `("sh" "-c" cmd))
   ; run cook on a fixture Makefile, return its captured stdout.
   (run-cook src tgt)
   (: _ (write-file (+ dir "/Makefile") src)
       r (run `(love "-l" "crew/cook/cook.l" (+ dir "/Makefile") tgt))
       (? (two? r) (cup r) ""))
   ; --emit a fixture Makefile, return the generated Cookfile text (stdout).
   (run-emit src)
   (: _ (write-file (+ dir "/Makefile") src)
       r (run `(love "-l" "crew/cook/cook.l" "--emit" (+ dir "/Makefile")))
       (? (two? r) (cup r) ""))

   _ (sh (+ "rm -rf " (+ dir "; mkdir -p " dir)))

   ; ---------------------------------------------------------------- expansion
   ev (run-cook (mk `("R := ."
                      "ho = out/host"
                      "love_c = $R/love.c $R/data.c"
                      "h_o = $(love_c:$(R)/%.c=$(ho)/%.o)"
                      "suf = $(love_c:.c=.o)"
                      "all:"
                      "\techo \"E1[$(h_o)]\""
                      "\techo \"E2[$(suf)]\""
                      "\techo \"E3[$R/$(ho)]\"")) "all")
   _ (ok "nested-paren + pattern subst-ref" (has ev "E1[out/host/love.o out/host/data.o]"))
   _ (ok "suffix subst-ref"                 (has ev "E2[./love.o ./data.o]"))
   _ (ok "single-char $R + $(VAR)"          (has ev "E3[./out/host]"))

   ; ---------------------------------------------------------------- functions
   fn (run-cook (mk `("files = a.c b.c sub/c.c"
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
   cnd (run-cook (mk `("x = 1"
                        "ifeq ($(x),1)"  "a = eqyes"  "else" "a = eqno"  "endif"
                        "ifneq ($(x),1)" "b = neqyes" "else" "b = neqno" "endif"
                        "ifdef x"  "c = def"  "endif"
                        "ifndef y" "d = ndef" "endif"
                        "all:"
                        "\techo \"C[$(a) $(b) $(c) $(d)]\"")) "all")
   _ (ok "ifeq/ifneq/ifdef/ifndef" (has cnd "C[eqyes neqno def ndef]"))

   ; a conditional INSIDE a recipe block selects which recipe lines run.
   rci (run-cook (mk `("on = 1" "all:" "\techo R-a"
                        "ifeq ($(on),1)" "\techo R-b" "endif" "\techo R-c")) "all")
   _ (ok "recipe-internal ifeq" (&& (has rci "R-b") (has rci "R-c")))

   ; -------------------------------------------------------------------- include
   ; (make resolves an include relative to the cwd; the fixture lives elsewhere,
   ; so name it absolutely.)
   _ (write-file (+ dir "/common.mk") "FROM_INC = included-ok\n")
   inc (run-cook (mk `((+ "include " (+ dir "/common.mk")) "all:" "\techo \"I[$(FROM_INC)]\"")) "all")
   _ (ok "include" (has inc "I[included-ok]"))

   ; ----------------------------------------------------- automatic vars + dir
   av (run-cook (mk `("all: foo/bar.o"
                       "foo/bar.o:" "\techo \"A[@=$@ star=$* d=$(dir $@)]\"")) "foo/bar.o")
   _ (ok "auto-vars + $(dir $@)" (has av "A[@=foo/bar.o star= d=foo/]"))

   ; --------------------------------------------------------- pattern rules
   ; build x.o from x.c via an implicit pattern rule; check the file appears.
   _ (sh (+ "mkdir -p " (+ (+ dir "/p; echo SRC > ") (+ dir "/p/x.c"))))
   _ (run-cook (mk `((+ "all: " (+ dir "/p/x.o"))
                      (+ (+ dir "/p/%.o: ") (+ dir "/p/%.c")) "\tcp $< $@")) (+ dir "/p/x.o"))
   _ (ok "pattern rule builds target" (= 0 (cap (sh (+ "test -f " (+ dir "/p/x.o"))))))

   ; static pattern rule: targets: tgtpat: prereqpats
   _ (sh (+ "echo S > " (+ dir "/p/y.in")))
   _ (run-cook (mk `((+ "outs = " (+ dir "/p/y.out"))
                      "all: $(outs)"
                      (+ "$(outs): " (+ dir (+ "/p/%.out: " (+ dir "/p/%.in")))) "\tcp $< $@")) "all")
   _ (ok "static pattern rule builds target" (= 0 (cap (sh (+ "test -f " (+ dir "/p/y.out"))))))

   ; ------------------------------------------------------------- assignment ops
   ops (run-cook (mk `("a := base" "a += more" "b ?= keep" "b ?= nope"
                        "all:" "\techo \"O[$(a)|$(b)]\"")) "all")
   _ (ok "+= and ?=" (has ops "O[base more|keep]"))

   ; ---------------------------------------------------------------------- emit
   ; --emit transpiles a Makefile to a RESOLVED Cookfile: vars/patterns expanded,
   ; .PHONY targets -> symbols, files -> strings, each recipe line an
   ; ("sh" "-c" cmd) step, the default ticket first, a trailing (cook (ticket 0)).
   em (run-emit (mk `("CC := cc"  "OBJ := a.o b.o"  ".PHONY: all clean"
                       "all: prog"
                       "prog: $(OBJ)"  "\t$(CC) -o $@ $(OBJ)"
                       "%.o: %.c"      "\t$(CC) -c -o $@ $<"
                       "clean:"        "\t-rm -f prog $(OBJ)")))
   _ (ok "emit: .PHONY target -> symbol"   (has em "(recipe 'all "))
   _ (ok "emit: file target -> string"     (has em "(recipe '\"prog\" "))
   _ (ok "emit: var-resolved step"         (has em "(\"sh\" \"-c\" \"cc -o prog a.o b.o\")"))
   _ (ok "emit: pattern-resolved step"     (has em "(\"sh\" \"-c\" \"cc -c -o a.o a.c\")"))
   _ (ok "emit: default ticket emitted first" (has em "(recipe 'all "))
   _ (ok "emit: trailing cook call"        (has em "(cook-all 0)"))

   ; round-trip: cook the emitted Cookfile and confirm it runs the recipe. The
   ; rule just writes a file (no compiler / source lookup), so the test is hermetic.
   _ (sh (+ "mkdir -p " (+ dir "/e")))
   emc (run-emit (mk `(".PHONY: all"
                        (+ "all: " (+ dir "/e/out.txt"))
                        (+ dir "/e/out.txt:") "\techo HI > $@")))
   _ (write-file (+ dir "/e/Cookfile") emc)
   _ (sh (+ "rm -f " (+ dir "/e/out.txt")))
   _ (run `(love "-l" "crew/cook/cook.l" (+ dir "/e/Cookfile") "all"))
   _ (ok "emit: round-trip cooks the emitted Cookfile" (= 0 (cap (sh (+ "test -f " (+ dir "/e/out.txt"))))))

   ; .DEFAULT_GOAL overrides "the first rule declared" as the default ticket --
   ; both when cooking the Makefile directly and through the emitted Cookfile
   ; (which must therefore lead with that recipe).
   dgsrc (mk `(".PHONY: first second"
               ".DEFAULT_GOAL := second"
               "first:"  "\techo FIRST"
               "second:" "\techo SECOND"))
   _ (write-file (+ dir "/Makefile") dgsrc)
   dgr (: r (run `(love "-l" "crew/cook/cook.l" (+ dir "/Makefile"))) (? (two? r) (cup r) ""))
   _ (ok ".DEFAULT_GOAL is the default ticket" (has dgr "SECOND"))
   _ (write-file (+ dir "/e/Cookfile") (run-emit dgsrc))
   dgc (: r (run `(love "-l" "crew/cook/cook.l" (+ dir "/e/Cookfile"))) (? (two? r) (cup r) ""))
   _ (ok "emit: .DEFAULT_GOAL survives the round-trip" (has dgc "SECOND"))

   ; ------------------------------------------------- the warmed date cache
   ; cook fills a date cache with ONE `stat` over the whole graph, because shelling out
   ; per node made an up-to-date pass ~2N forks. that cache is only sound until the first
   ; recipe runs -- a step may write anything, not only its own target -- so `recur` drops
   ; it. this is the fixture that fails if anyone ever narrows that to "drop just my own
   ; target": `first` writes a file it does not own, and `second` must see the new one.
   ;
   ; the order matters. shared.txt and second both exist and second is NEWER, so a stale
   ; cache says second is up to date and skips it. only a re-read after `first` ran shows
   ; shared.txt overtaking it.
   ; (⚠ every path in a fixture recipe must be ABSOLUTE: cook runs from the project root,
   ;  not from `dir`, so a bare `> out.txt` writes into the repo.)
   (D f) (+ dir f)
   _ (sh (+ "echo OLD > " (+ (D "/shared.txt") (+ " && sleep 0.01 && echo STALE > " (D "/second")))))
   sidesrc (mk `(".PHONY: all first"
                 (+ "all: first " (D "/second"))
                 "first:" (+ "\techo NEW > " (D "/shared.txt"))
                 (+ (D "/second: ") (D "/shared.txt"))
                 (+ "\tcp " (+ (D "/shared.txt") (+ " " (D "/second"))))))
   _ (write-file (D "/Makefile") sidesrc)
   _ (: r (run `(love "-l" "crew/cook/cook.l" (D "/Makefile") "all")) 0)
   side (: r (sh (+ "cat " (D "/second"))) (? (two? r) (cup r) ""))
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
   one (run-cook (mk `(".ONESHELL:"
                       ".SHELLFLAGS := -ec"
                       "all:"
                       "\tx=hello"
                       "\techo \"O1[$$x world]\"")) "all")
   _ (ok ".ONESHELL: one shell for the whole recipe" (has one "O1[hello world]"))
   ; ..and WITHOUT it the default still holds: separate shells, so x is gone
   sep (run-cook (mk `("all:"
                       "\tx=hello"
                       "\techo \"O2[$$x world]\"")) "all")
   _ (ok "no .ONESHELL: a line still gets its own shell" (has sep "O2[ world]"))
   ; ⚠ with one shell, a plain -c checks only the LAST line's status, so a failing
   ; middle line would pass SILENTLY. -e is what keeps a recipe honest: the third
   ; line must never run. The recipe is SILENCED (@ on the first line governs the
   ; lot) because a one-shell recipe is echoed WHOLE -- an un-silenced echo would
   ; print the text of the third line and the assertion would read its own echo.
   ec (run-cook (mk `(".ONESHELL:"
                      ".SHELLFLAGS := -ec"
                      "all:"
                      "\t@echo O3-one"
                      "\tfalse"
                      "\techo O3-three")) "all")
   _ (ok ".SHELLFLAGS -ec: a failing middle line stops the recipe"
         (&& (has ec "O3-one") (! (has ec "O3-three"))))

   ; ------------------------------------------------------------ SHELL + wildcard
   ; SHELL picks the shell recipe lines AND $(shell) ride -- observable through
   ; $0, which lush pins to "lush" in its -c lane (a stock sh answers "sh").
   ; the repo's own shell is the fixture's shell: make-with-lush, the rung-4
   ; distro loop closed. $(wildcard) is lush's pure glob at the repo root
   ; (sorted; a no-match pattern contributes nothing -- nullglob).
   shl (run-cook (mk `("SHELL := out/host/lush"
                       "all:"
                       "\t@echo \"L1[$$0]\""
                       "\t@echo \"L2[$(shell echo $$0)]\""))
                 "all")
   _ (ok "SHELL: recipe lines ride lush"   (has shl "L1[lush]"))
   _ (ok "SHELL: $(shell) rides lush too"  (has shl "L2[lush]"))
   wld (run-cook (mk `("all:"
                       "\t@echo \"WC[$(wildcard crew/lush/jo*.l crew/lush/zz-nope*.q)]\""))
                 "all")
   _ (ok "wildcard: pure-love glob, nullglob" (has wld "WC[crew/lush/job.l]"))

   ; ---------------------------------------------------------------- summary
   np (peep pass 0 0) nf (peep fail 0 0)
   _ (puts (+ (show np) (+ " cook tests pass" (? (< 0 nf) (+ ", " (+ (show nf) " FAIL")) ""))))
   _ (putc 10)
   (? (< 0 nf) (quit 1) 0))
