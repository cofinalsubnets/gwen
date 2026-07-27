# test/test.mk -- the test_* gates (and the uuwm/uukind corpus generators)
#
# Fragment of the root Makefile (split out 2026-07-15). Included by ./Makefile,
# which is invoked from the project root; paths resolve from there. Shared vars
# live in common.mk. Every recipe here is unchanged from the single-file Makefile.

# love0 bakes prel+ev+repl + the whole test corpus (sed headers) and self-tests
# BOTH compilers in one run: eval prel (c0), run the corpus, bootstrap ev.l
# through c0, run the corpus again via the self-hosted ev. Built with -Dai_tco=0,
# so this also exercises the non-tail-threaded trampoline dispatch path.
# stdin is /dev/null: the corpus reads from the baked string, not stdin, but
# test/io.l exercises the real `in` port (a bare fgetc), which would otherwise
# block on a tty (the old `cat $t | love0` fed the test stream in on stdin).
# Both gates require the zz-fin summary line, not just exit 0: a reader stop
# (e.g. a stray `)` mid-corpus) silently drops the rest of the stream and
# exits 0 without ever reaching zz-fin -- exit code alone green-lights a run
# that only executed a prefix of the corpus. love0 must print TWO summaries
# (the corpus runs under both c0 and the self-hosted ev).
# Both gates STREAM through `tee` (the dots appear live -- love flushes per write, so
# you watch progress and a stall marks a slow test) while still capturing the run for
# the sentinel grep. The exit status rides a `.rc` file, not `$?` (the pipe's exit is
# tee's, and /bin/sh has no pipefail), so the exit-0 AND sentinel checks both hold.
test_love0: $(love0)
	@echo TEST $(love0)
	@{ $(love0) </dev/null; echo $$? > out/host/.test_love0.rc; } | tee out/host/.test_love0.out; \
	  s=$$(cat out/host/.test_love0.rc); \
	  [ $$s -eq 0 ] && [ `grep -c "tests pass" out/host/.test_love0.out` -eq 2 ]
test_host: $m
	@echo TEST $m
	@{ cat $t | $m; echo $$? > out/host/.test_host.rc; } | tee out/host/.test_host.out; \
	  s=$$(cat out/host/.test_host.rc); \
	  [ $$s -eq 0 ] && grep -q "tests pass" out/host/.test_host.out
# Host-nif smoke tests: nifs defined in host/*.c link into `love` but NOT love0
# (which bakes the test/*.l corpus), so they cannot sit DIRECTLY in test/ -- love0
# would bake them, read the nif names as missing, and fail its self-test. They
# live under test/host/ instead: the corpus glob ($t in common.mk) is a
# NON-RECURSIVE test/*.l wildcard, so a subfolder is invisible to the bake. Run
# them standalone against the built binary. Each script prints a "<name>: ok"
# sentinel and uses the test/00-init.l assert harness (which exits 1 on the first
# failure), so the gate checks BOTH exit 0 AND the sentinel -- a silent
# reader-stop exits 0 without it.
# Add a thread's smoke script to hostnif_tests (ain: test/host/net.l, &c).
# haven.l is OUT of the gate: it can wedge on a wayland resource (a stray holding
# the socket) and stall the whole run indefinitely. run it standalone when working
# on the compositor: `cat test/00-init.l test/host/haven.l | out/host/love`.
hostnif_tests = test/host/gcpause.l test/host/run.l test/host/pty.l test/host/net.l test/host/lux.l test/host/luxui.l test/host/baoedit.l test/host/baotest.l test/host/init.l test/host/fs.l test/host/sh.l test/host/cb.l test/host/berth.l test/host/manifest.l test/host/pier.l test/host/font.l test/host/drm.l test/host/overlay.l test/host/bake.l test/host/rove.l test/host/rune.l test/host/lapiz.l test/host/papel.l test/host/kiosko.l test/host/seedhttp.l
# haven's real-client smoke binary: libwayland-client + the generated
# xdg-shell glue -- deliberately NOT zero-dep, it exists to be the OTHER side
# of haven's wire. built only where wayland-scanner + libwayland live;
# test/host/haven.l skips its smoke act when the binary is absent. pinned to the
# canonical out/host like love0 (parse-time prereqs; it never links love).
smoke = out/host/haven-smoke
xdgxml = $(shell pkg-config --variable=pkgdatadir wayland-protocols 2>/dev/null)/stable/xdg-shell/xdg-shell.xml
$(smoke): crew/haven/smoke.c
	@mkdir -p out/host
	@if command -v wayland-scanner >/dev/null 2>&1 && pkg-config --exists wayland-client 2>/dev/null && [ -f "$(xdgxml)" ]; then \
	  echo "CC $@"; \
	  wayland-scanner client-header "$(xdgxml)" out/host/xdg-shell-client-protocol.h; \
	  wayland-scanner private-code "$(xdgxml)" out/host/xdg-shell-protocol.c; \
	  $(CC) -O1 -Wall -Wextra -o $@ crew/haven/smoke.c out/host/xdg-shell-protocol.c -Iout/host `pkg-config --cflags --libs wayland-client`; \
	else echo "SKIP $@ (no libwayland here)"; fi
# haven's keyboard map: the REAL compiled xkb text (what every wayland
# compositor ships its clients), emitted by libxkbcommon's own tool where it
# lives. absent -> an empty file, and haven ships keymap format 0 instead.
havenkm = out/lib/haven-keymap.xkb
$(havenkm):
	@mkdir -p out/lib
	@if command -v xkbcli >/dev/null 2>&1; then \
	  echo "KM $@"; xkbcli compile-keymap > $@; \
	else echo "SKIP $@ (no xkbcli here)"; : > $@; fi
test_hostnif: host $(smoke) $(havenkm)
	@for s in $(hostnif_tests); do echo "HOSTNIF $$s"; \
	  cat test/00-init.l $$s | $m > out/host/.test_hostnif.out 2>&1; r=$$?; \
	  cat out/host/.test_hostnif.out; \
	  { [ $$r -eq 0 ] && grep -q ': ok' out/host/.test_hostnif.out; } \
	    || { echo "FAIL $$s (exit $$r)"; exit 1; }; \
	done
# Runnable design companions in doc/ -- pure-love models that pin the shape a C
# design takes (doc/stream.l ~ doc/stream.md). Zero-dep (no host nifs), so unlike
# hostnif_tests they COULD ride the corpus -- but they leak generic helper names
# into the one global scope, so they run standalone instead. Gated only to keep
# them from rotting (this file's drain-floor bug slipped in while ungated). Same
# contract: exit 0 AND a "<name>: ok" sentinel. (tag.l is a sketch, no asserts.)
doc_tests = doc/stream.l
test_doc: host
	@for s in $(doc_tests); do echo "DOC $$s"; \
	  cat test/00-init.l $$s | $m > out/host/.test_doc.out 2>&1; r=$$?; \
	  cat out/host/.test_doc.out; \
	  { [ $$r -eq 0 ] && grep -q ': ok' out/host/.test_doc.out; } \
	    || { echo "FAIL $$s (exit $$r)"; exit 1; }; \
	done
# Native-codegen self-tests (the love/glaze/ x86-64 jit): emit (the SSE emitter) +
# auto (ev's source-recognizer: counted loops, float grids, recursive arith
# groups) are exercised by test/glaze-x86.l (the asserts moved out of emit.l/
# auto.l so they no longer run at every glaze load/bake); it cats emit.l + auto.l
# ahead of itself, then runs each block through base-ev (the loader's global ev is
# now auto-ev, which mis-opfixes some pathological self-test forms). hook (ev.l's
# ala creation-hook) keeps its inline self-test, run with emit.l prepended. Each
# needs the built binary (the `nat` host nif) and prel's strict `assert` (SCARES
# on a false claim, terminal exit 1). x86-64 ONLY (real machine code); skipped
# elsewhere. Gate = exit 0 AND the sentinel (a reader-stop exits 0 without it).
.PHONY: test_glaze
ifeq ($a,x86_64)
test_glaze: host
	@echo "GLAZE test/glaze-x86.l (emit + auto)"; \
	  { echo "(enter ()) (use 'holo)"; cat crew/holo/x64.l crew/holo/arm64.l; echo "(leave ())"; \
	    cat test/glaze-x86.l; } | $m > out/host/.test_glaze.out 2>&1; r=$$?; \
	  cat out/host/.test_glaze.out; \
	  { [ $$r -eq 0 ] && grep -q "test/glaze-x86:" out/host/.test_glaze.out; } \
	    || { echo "FAIL glaze x86 (exit $$r)"; exit 1; }; \
	  echo "GLAZE love/glaze/hook.l"; \
	  { echo "(use 'holo)"; cat love/glaze/hook.l; printf '\n(leave ())(puts "glaze-hook-ran")(putc 10)'; } | $m > out/host/.test_glaze.out 2>&1; r=$$?; \
	  cat out/host/.test_glaze.out; \
	  { [ $$r -eq 0 ] && grep -q "glaze-hook-ran" out/host/.test_glaze.out; } \
	    || { echo "FAIL glaze/hook (exit $$r)"; exit 1; }
else
test_glaze:
	@echo "test_glaze: skipped (host arch $a is not x86_64)"
endif
# crew/sat/ -- the CDCL SAT solver app. Portable love (no glaze), so it runs on every arch.
# Gate = exit 0 AND the sentinel (a reader-stop or a strict-assert scare both miss it).
.PHONY: test_sat
test_sat: host
	@echo "SAT crew/sat/sat.l + crew/sat/dimacs.l + crew/sat/flat.l"; \
	  cat crew/sat/sat.l crew/sat/dimacs.l crew/sat/flat.l | $m > out/host/.test_sat.out 2>&1; r=$$?; \
	  cat out/host/.test_sat.out; \
	  { [ $$r -eq 0 ] && grep -q "sat: Stages 1-3 ok" out/host/.test_sat.out && grep -q "crew/sat/dimacs: ok" out/host/.test_sat.out && grep -q "crew/sat/flat: ok" out/host/.test_sat.out; } \
	    || { echo "FAIL sat (exit $$r)"; exit 1; }
# The DRAT lane's EXTERNAL check: flat.l's emitted refutations (fdrat0) verified by
# drat-trim, the SAT competition's independent checker (fetched + built into out/drat
# on first use; skips gracefully offline). The in-gate twin (fd-check, no external
# dependency) runs inside test_sat; this aims the third-party eye at php5-8 + a
# raw-RUP row. Not in test_all (network on first run); run after touching the emitter.
.PHONY: test_drat
test_drat: host
	@cd crew/sat && ./dratcheck.sh || { echo "FAIL drat"; exit 1; }
# The lux app's pure core (crew/lux/core.l): xmonad's StackSet -- the focus zipper, the
# workspace sheaf, the floating half -- with xmonad's QuickCheck laws + a seeded
# fuzz (crew/lux/law.l). Pure love (no nif), so it self-tests portably; the X layers
# (wire.l/lux.l) need connectu and are proven against Xephyr, not here. Gate = the
# sentinel AND exit 0 (a reader-stop or strict-assert scare both miss it).
.PHONY: test_lux
test_lux: host
	@echo "LUX crew/lux/core.l ... crew/lux/config.l + crew/lux/law.l (the whole app, host)"; \
	  cat test/00-init.l crew/lux/core.l crew/lux/layout.l crew/lux/wire.l crew/lux/ewmh.l crew/lux/manage.l crew/lux/keys.l crew/lux/config.l crew/lux/law.l | $m > out/host/.test_lux.out 2>&1; r=$$?; \
	  cat out/host/.test_lux.out; \
	  { [ $$r -eq 0 ] && grep -q "crew/lux/law: StackSet" out/host/.test_lux.out; } \
	    || { echo "FAIL lux (exit $$r)"; exit 1; }
.PHONY: test_seed
test_seed: host out/host$(hsuf)/seed
	@echo "SEED crew/seed/{seed,seedtest}.l"; \
	  rm -rf out/host/.seedtest; \
	  cat test/00-init.l $(seedfiles) crew/seed/seedtest.l | $m > out/host/.test_seed.out 2>&1; r=$$?; \
	  cat out/host/.test_seed.out; \
	  { [ $$r -eq 0 ] && grep -q "seed: ok" out/host/.test_seed.out; } \
	    || { echo "FAIL seed (exit $$r)"; exit 1; }
# the kore smokes drive the BAKED image (`--wake kore.image`), not the cold cat --
# ~0.02s vs ~0.75s per spawn across the ~68 tool runs below (the mooncc.image precedent).
# the argv0-symlink smoke execs the real `$(ho)/kore` shim (it proves the shim's
# basename-$0 dispatch the image wake bypasses). the synthetic "kore" argv0 (link)
# makes an unknown tool usage+quit exactly like the cli, so the exit faces are unchanged.
korerun = $m --wake $(ho)/kore.image -e '(kore-main (link "kore" (cuup (cup cmdline))))'
.PHONY: test_kore
test_kore: host out/host$(hsuf)/kore out/host$(hsuf)/kore.image
	@echo "UTILS crew/kore/{text,core,fs,re,sed,diff,law}.l"; \
	  cat test/00-init.l crew/kore/text.l crew/kore/core.l crew/kore/fs.l crew/kore/re.l crew/kore/sed.l crew/kore/proc.l crew/vi/core.l crew/vi/vi.l crew/kore/diff.l crew/kore/law.l | $m > out/host/.test_kore.out 2>&1; r=$$?; \
	  cat out/host/.test_kore.out; \
	  { [ $$r -eq 0 ] && grep -q "crew/kore/law: myers" out/host/.test_kore.out; } \
	    || { echo "FAIL utils (exit $$r)"; exit 1; }
	@printf 'a\nb\nc\n' > $(ho)/.au1; printf 'a\nX\nc\n' > $(ho)/.au2; \
	  $(korerun) diff $(ho)/.au1 $(ho)/.au1 > $(ho)/.kore-same.out 2>&1; r=$$?; \
	  { [ $$r -eq 0 ] && [ ! -s $(ho)/.kore-same.out ]; } || { echo "FAIL kore diff same (exit $$r)"; exit 1; }; \
	  $(korerun) diff $(ho)/.au1 $(ho)/.au2 > $(ho)/.kore-diff.out 2>&1; r=$$?; \
	  [ $$r -eq 1 ] || { echo "FAIL kore diff differ (exit $$r)"; exit 1; }; \
	  diff -u $(ho)/.au1 $(ho)/.au2 | tail -n +3 > $(ho)/.kore-gnu.out; tail -n +3 $(ho)/.kore-diff.out > $(ho)/.kore-ours.out; \
	  cmp -s $(ho)/.kore-gnu.out $(ho)/.kore-ours.out || { echo "FAIL kore diff vs GNU"; exit 1; }; \
	  ln -sf kore $(ho)/diff; \
	  $(ho)/diff $(ho)/.au1 $(ho)/.au2 > $(ho)/.kore-sym.out 2>&1; r=$$?; \
	  { [ $$r -eq 1 ] && cmp -s $(ho)/.kore-diff.out $(ho)/.kore-sym.out; } || { echo "FAIL kore argv0 symlink (exit $$r)"; exit 1; }; \
	  $(korerun) bogus > /dev/null 2>&1; r=$$?; \
	  [ $$r -eq 2 ] || { echo "FAIL kore usage (exit $$r)"; exit 1; }; \
	  if [ "$$(uname -m)" = x86_64 ]; then \
	    printf '(li r0 60) (li r6 7) (sys)\n' > $(ho)/.kore-as.l; \
	    $(korerun) as x64 $(ho)/.kore-as.l $(ho)/.kore-as.elf > /dev/null 2>&1 || { echo "FAIL kore as"; exit 1; }; \
	    chmod +x $(ho)/.kore-as.elf; $(ho)/.kore-as.elf; r=$$?; \
	    [ $$r -eq 7 ] || { echo "FAIL kore as run (exit $$r)"; exit 1; }; \
	  fi; \
	  echo "kore: diff (GNU-identical) + argv0 symlink + usage + as ok"
	@printf 'b\na\nc\nb\n' > $(ho)/.cu1; printf 'x y\nz\n' > $(ho)/.cu2; \
	  LC_ALL=C sort $(ho)/.cu1 > $(ho)/.cu-g; $(korerun) sort $(ho)/.cu1 > $(ho)/.cu-o; \
	  cmp -s $(ho)/.cu-g $(ho)/.cu-o || { echo "FAIL kore sort vs GNU"; exit 1; }; \
	  LC_ALL=C sort -u $(ho)/.cu1 > $(ho)/.cu-g; $(korerun) sort -u $(ho)/.cu1 > $(ho)/.cu-o; \
	  cmp -s $(ho)/.cu-g $(ho)/.cu-o || { echo "FAIL kore sort -u vs GNU"; exit 1; }; \
	  LC_ALL=C sort $(ho)/.cu1 | uniq -c > $(ho)/.cu-g; $(korerun) sort $(ho)/.cu1 | $(korerun) uniq -c > $(ho)/.cu-o; \
	  cmp -s $(ho)/.cu-g $(ho)/.cu-o || { echo "FAIL kore uniq -c vs GNU"; exit 1; }; \
	  head -n 2 $(ho)/.cu1 $(ho)/.cu2 > $(ho)/.cu-g; $(korerun) head -n 2 $(ho)/.cu1 $(ho)/.cu2 > $(ho)/.cu-o; \
	  cmp -s $(ho)/.cu-g $(ho)/.cu-o || { echo "FAIL kore head vs GNU"; exit 1; }; \
	  tail -n 2 $(ho)/.cu1 $(ho)/.cu2 > $(ho)/.cu-g; $(korerun) tail -n 2 $(ho)/.cu1 $(ho)/.cu2 > $(ho)/.cu-o; \
	  cmp -s $(ho)/.cu-g $(ho)/.cu-o || { echo "FAIL kore tail vs GNU"; exit 1; }; \
	  wc $(ho)/.cu1 $(ho)/.cu2 > $(ho)/.cu-g; $(korerun) wc $(ho)/.cu1 $(ho)/.cu2 > $(ho)/.cu-o; \
	  cmp -s $(ho)/.cu-g $(ho)/.cu-o || { echo "FAIL kore wc vs GNU"; exit 1; }; \
	  wc -l < $(ho)/.cu1 > $(ho)/.cu-g; $(korerun) wc -l < $(ho)/.cu1 > $(ho)/.cu-o; \
	  cmp -s $(ho)/.cu-g $(ho)/.cu-o || { echo "FAIL kore wc -l stdin vs GNU"; exit 1; }; \
	  cat $(ho)/.cu1 $(ho)/.cu2 > $(ho)/.cu-g; $(korerun) cat $(ho)/.cu1 $(ho)/.cu2 > $(ho)/.cu-o; \
	  cmp -s $(ho)/.cu-g $(ho)/.cu-o || { echo "FAIL kore cat vs GNU"; exit 1; }; \
	  seq 5 > $(ho)/.cu-g; $(korerun) seq 5 > $(ho)/.cu-o; \
	  cmp -s $(ho)/.cu-g $(ho)/.cu-o || { echo "FAIL kore seq vs GNU"; exit 1; }; \
	  echo hi there > $(ho)/.cu-g; $(korerun) echo hi there > $(ho)/.cu-o; \
	  cmp -s $(ho)/.cu-g $(ho)/.cu-o || { echo "FAIL kore echo vs GNU"; exit 1; }; \
	  basename /a/b.txt .txt > $(ho)/.cu-g; $(korerun) basename /a/b.txt .txt > $(ho)/.cu-o; \
	  cmp -s $(ho)/.cu-g $(ho)/.cu-o || { echo "FAIL kore basename vs GNU"; exit 1; }; \
	  printf 'q\nq\nr\n' | tee $(ho)/.cu-g2 > $(ho)/.cu-g; printf 'q\nq\nr\n' | $(korerun) tee $(ho)/.cu-o2 > $(ho)/.cu-o; \
	  { cmp -s $(ho)/.cu-g $(ho)/.cu-o && cmp -s $(ho)/.cu-g2 $(ho)/.cu-o2; } || { echo "FAIL kore tee vs GNU"; exit 1; }; \
	  echo "kore: line tools (sort/uniq/head/tail/wc/cat/seq/echo/basename/tee GNU-identical) ok"
	@printf 'a:b:c\nnodelim\nx:y\n' > $(ho)/.fu1; \
	  cut -d: -f1,3 $(ho)/.fu1 > $(ho)/.fu-g; $(korerun) cut -d: -f1,3 $(ho)/.fu1 > $(ho)/.fu-o; \
	  cmp -s $(ho)/.fu-g $(ho)/.fu-o || { echo "FAIL kore cut -f vs GNU"; exit 1; }; \
	  cut -d: -f1,3 -s $(ho)/.fu1 > $(ho)/.fu-g; $(korerun) cut -d: -f1,3 -s $(ho)/.fu1 > $(ho)/.fu-o; \
	  cmp -s $(ho)/.fu-g $(ho)/.fu-o || { echo "FAIL kore cut -s vs GNU"; exit 1; }; \
	  cut -d: -f2- $(ho)/.fu1 > $(ho)/.fu-g; $(korerun) cut -d: -f2- $(ho)/.fu1 > $(ho)/.fu-o; \
	  cmp -s $(ho)/.fu-g $(ho)/.fu-o || { echo "FAIL kore cut -f2- vs GNU"; exit 1; }; \
	  printf 'hello\nhi\n' | cut -c2-4 > $(ho)/.fu-g; printf 'hello\nhi\n' | $(korerun) cut -c2-4 > $(ho)/.fu-o; \
	  cmp -s $(ho)/.fu-g $(ho)/.fu-o || { echo "FAIL kore cut -c vs GNU"; exit 1; }; \
	  printf 'hi there\n' | tr a-z A-Z > $(ho)/.fu-g; printf 'hi there\n' | $(korerun) tr a-z A-Z > $(ho)/.fu-o; \
	  cmp -s $(ho)/.fu-g $(ho)/.fu-o || { echo "FAIL kore tr vs GNU"; exit 1; }; \
	  printf 'abcd\n' | tr abcd xy > $(ho)/.fu-g; printf 'abcd\n' | $(korerun) tr abcd xy > $(ho)/.fu-o; \
	  cmp -s $(ho)/.fu-g $(ho)/.fu-o || { echo "FAIL kore tr pad vs GNU"; exit 1; }; \
	  printf 'hello world\n' | tr -d aeiou > $(ho)/.fu-g; printf 'hello world\n' | $(korerun) tr -d aeiou > $(ho)/.fu-o; \
	  cmp -s $(ho)/.fu-g $(ho)/.fu-o || { echo "FAIL kore tr -d vs GNU"; exit 1; }; \
	  printf 'aa  bb   cc\n' | tr -s ' ' > $(ho)/.fu-g; printf 'aa  bb   cc\n' | $(korerun) tr -s ' ' > $(ho)/.fu-o; \
	  cmp -s $(ho)/.fu-g $(ho)/.fu-o || { echo "FAIL kore tr -s vs GNU"; exit 1; }; \
	  printf 'a\n\nb\n' | nl > $(ho)/.fu-g; printf 'a\n\nb\n' | $(korerun) nl > $(ho)/.fu-o; \
	  cmp -s $(ho)/.fu-g $(ho)/.fu-o || { echo "FAIL kore nl vs GNU"; exit 1; }; \
	  printf 'abc\nde\n' | rev > $(ho)/.fu-g; printf 'abc\nde\n' | $(korerun) rev > $(ho)/.fu-o; \
	  cmp -s $(ho)/.fu-g $(ho)/.fu-o || { echo "FAIL kore rev vs GNU"; exit 1; }; \
	  echo "kore: field tools (cut/tr/nl/rev GNU-identical) ok"
	@P=$(ho)/.fsplay; rm -rf $$P; mkdir $$P; \
	  $(korerun) mkdir -p $$P/a/b/c && [ -d $$P/a/b/c ] || { echo "FAIL kore mkdir -p"; exit 1; }; \
	  printf 'hi there\n' > $$P/f1; \
	  $(korerun) cp $$P/f1 $$P/f2 && cmp -s $$P/f1 $$P/f2 || { echo "FAIL kore cp"; exit 1; }; \
	  $(korerun) cp $$P/f1 $$P/a && cmp -s $$P/f1 $$P/a/f1 || { echo "FAIL kore cp into dir"; exit 1; }; \
	  $(korerun) mv $$P/f2 $$P/f3 && [ ! -e $$P/f2 ] && cmp -s $$P/f1 $$P/f3 || { echo "FAIL kore mv"; exit 1; }; \
	  $(korerun) ln -s f1 $$P/l1 && [ "$$(readlink $$P/l1)" = f1 ] || { echo "FAIL kore ln -s"; exit 1; }; \
	  $(korerun) ln $$P/f1 $$P/h1 && [ $$P/h1 -ef $$P/f1 ] || { echo "FAIL kore ln"; exit 1; }; \
	  $(korerun) touch $$P/new $$P/.hidden && [ -f $$P/new ] && [ -f $$P/.hidden ] || { echo "FAIL kore touch"; exit 1; }; \
	  $(korerun) chmod 600 $$P/f1 && [ "$$(stat -c %a $$P/f1)" = 600 ] || { echo "FAIL kore chmod"; exit 1; }; \
	  LC_ALL=C ls -1 $$P > $(ho)/.fs-g; $(korerun) ls $$P > $(ho)/.fs-o; \
	  cmp -s $(ho)/.fs-g $(ho)/.fs-o || { echo "FAIL kore ls vs GNU"; exit 1; }; \
	  LC_ALL=C ls -A -1 $$P > $(ho)/.fs-g; $(korerun) ls -a $$P > $(ho)/.fs-o; \
	  cmp -s $(ho)/.fs-g $(ho)/.fs-o || { echo "FAIL kore ls -a vs GNU -A"; exit 1; }; \
	  [ "$$($(korerun) pwd)" = "$$(pwd)" ] || { echo "FAIL kore pwd"; exit 1; }; \
	  $(korerun) rm $$P/f3 && [ ! -e $$P/f3 ] || { echo "FAIL kore rm"; exit 1; }; \
	  $(korerun) rm -r $$P/a && [ ! -e $$P/a ] || { echo "FAIL kore rm -r"; exit 1; }; \
	  $(korerun) mkdir $$P/empty && $(korerun) rmdir $$P/empty && [ ! -e $$P/empty ] || { echo "FAIL kore rmdir"; exit 1; }; \
	  $(korerun) rm $$P/nope > /dev/null 2>&1; r=$$?; [ $$r -eq 1 ] || { echo "FAIL kore rm miss exit"; exit 1; }; \
	  $(korerun) rm -f $$P/nope > /dev/null 2>&1; r=$$?; [ $$r -eq 0 ] || { echo "FAIL kore rm -f quiet"; exit 1; }; \
	  echo "kore: fs tools (mkdir/cp/mv/ln/touch/chmod/ls/pwd/rm/rmdir) ok"
	@printf 'abc\nxbz\nzzz\n+q\n*r\n' > $(ho)/.gr1; printf 'nope\nbc here\n' > $(ho)/.gr2; \
	  grep b $(ho)/.gr1 > $(ho)/.gr-g; $(korerun) grep b $(ho)/.gr1 > $(ho)/.gr-o; \
	  cmp -s $(ho)/.gr-g $(ho)/.gr-o || { echo "FAIL kore grep vs GNU"; exit 1; }; \
	  grep -n b $(ho)/.gr1 $(ho)/.gr2 > $(ho)/.gr-g; $(korerun) grep -n b $(ho)/.gr1 $(ho)/.gr2 > $(ho)/.gr-o; \
	  cmp -s $(ho)/.gr-g $(ho)/.gr-o || { echo "FAIL kore grep -n multi vs GNU"; exit 1; }; \
	  grep -c b $(ho)/.gr1 $(ho)/.gr2 > $(ho)/.gr-g; $(korerun) grep -c b $(ho)/.gr1 $(ho)/.gr2 > $(ho)/.gr-o; \
	  cmp -s $(ho)/.gr-g $(ho)/.gr-o || { echo "FAIL kore grep -c vs GNU"; exit 1; }; \
	  grep -v b $(ho)/.gr1 > $(ho)/.gr-g; $(korerun) grep -v b $(ho)/.gr1 > $(ho)/.gr-o; \
	  cmp -s $(ho)/.gr-g $(ho)/.gr-o || { echo "FAIL kore grep -v vs GNU"; exit 1; }; \
	  grep -l b $(ho)/.gr1 $(ho)/.gr2 > $(ho)/.gr-g; $(korerun) grep -l b $(ho)/.gr1 $(ho)/.gr2 > $(ho)/.gr-o; \
	  cmp -s $(ho)/.gr-g $(ho)/.gr-o || { echo "FAIL kore grep -l vs GNU"; exit 1; }; \
	  printf 'q\n' | grep -l q > $(ho)/.gr-g; printf 'q\n' | $(korerun) grep -l q > $(ho)/.gr-o; \
	  cmp -s $(ho)/.gr-g $(ho)/.gr-o || { echo "FAIL kore grep -l stdin vs GNU"; exit 1; }; \
	  grep '' $(ho)/.gr1 > $(ho)/.gr-g; $(korerun) grep '' $(ho)/.gr1 > $(ho)/.gr-o; \
	  cmp -s $(ho)/.gr-g $(ho)/.gr-o || { echo "FAIL kore grep empty pattern vs GNU"; exit 1; }; \
	  for p in 'ab*c' '^x' 'z$$' '[abx]b' '[^a]b' 'b\+' 'xb\?z' '\(zz\)*z' '.z' '^\+q' '^*r' 'x[b-z]z'; do \
	    grep -c "$$p" $(ho)/.gr1 > $(ho)/.gr-g 2>/dev/null; a=$$?; \
	    $(korerun) grep -c "$$p" $(ho)/.gr1 > $(ho)/.gr-o; b=$$?; \
	    { cmp -s $(ho)/.gr-g $(ho)/.gr-o && [ $$a -eq $$b ]; } \
	      || { echo "FAIL kore grep BRE '$$p' vs GNU"; exit 1; }; \
	  done; \
	  $(korerun) grep b $(ho)/.gr1 > /dev/null; r=$$?; [ $$r -eq 0 ] || { echo "FAIL kore grep hit exit"; exit 1; }; \
	  $(korerun) grep qqq $(ho)/.gr1 > /dev/null; r=$$?; [ $$r -eq 1 ] || { echo "FAIL kore grep miss exit"; exit 1; }; \
	  grep b $(ho)/.gr-nope 2> $(ho)/.gr-g; a=$$?; $(korerun) grep b $(ho)/.gr-nope 2> $(ho)/.gr-o; b=$$?; \
	  { cmp -s $(ho)/.gr-g $(ho)/.gr-o && [ $$a -eq 2 ] && [ $$b -eq 2 ]; } || { echo "FAIL kore grep missing file vs GNU"; exit 1; }; \
	  $(korerun) grep b $(ho)/.gr1 $(ho)/.gr-nope > /dev/null 2>&1; r=$$?; \
	  [ $$r -eq 2 ] || { echo "FAIL kore grep err beats match exit"; exit 1; }; \
	  echo "kore: grep (plain/-n/-c/-v/-l + BRE battery GNU-identical, the exit triple) ok"
	@printf 'abc\nxbz\nzzz\nq4\nw5\n' > $(ho)/.sd1; \
	  for sc in 's/b/X/' 's/z/Q/g' '2d' '/x/,/q/d' '$$d' '2q' 's/x*/-/g' 's/\(b*\)z/[\1]/' 's/b/[&]/' 's|z|_|g' 's/a/1/; s/b/2/' 's/q\(.\)/<\1>/'; do \
	    sed "$$sc" $(ho)/.sd1 > $(ho)/.sd-g; a=$$?; \
	    $(korerun) sed "$$sc" $(ho)/.sd1 > $(ho)/.sd-o; b=$$?; \
	    { cmp -s $(ho)/.sd-g $(ho)/.sd-o && [ $$a -eq $$b ]; } \
	      || { echo "FAIL kore sed '$$sc' vs GNU"; exit 1; }; \
	  done; \
	  for sc in '2,4p' '/z/p' 's/b/X/p' '/x/,/q/p'; do \
	    sed -n "$$sc" $(ho)/.sd1 > $(ho)/.sd-g; \
	    $(korerun) sed -n "$$sc" $(ho)/.sd1 > $(ho)/.sd-o; \
	    cmp -s $(ho)/.sd-g $(ho)/.sd-o || { echo "FAIL kore sed -n '$$sc' vs GNU"; exit 1; }; \
	  done; \
	  printf 'ab\n' | sed 's/a/1/' > $(ho)/.sd-g; printf 'ab\n' | $(korerun) sed 's/a/1/' > $(ho)/.sd-o; \
	  cmp -s $(ho)/.sd-g $(ho)/.sd-o || { echo "FAIL kore sed stdin vs GNU"; exit 1; }; \
	  printf 'a\n' | sed 's/a' > /dev/null 2>&1; a=$$?; printf 'a\n' | $(korerun) sed 's/a' > /dev/null 2>&1; b=$$?; \
	  { [ $$a -eq 1 ] && [ $$b -eq 1 ]; } || { echo "FAIL kore sed bad-script exit (gnu $$a ours $$b)"; exit 1; }; \
	  sed p $(ho)/.sd-nope $(ho)/.sd1 > $(ho)/.sd-g 2>&1; a=$$?; \
	  $(korerun) sed p $(ho)/.sd-nope $(ho)/.sd1 > $(ho)/.sd-o 2>&1; b=$$?; \
	  { cmp -s $(ho)/.sd-g $(ho)/.sd-o && [ $$a -eq 2 ] && [ $$b -eq 2 ]; } \
	    || { echo "FAIL kore sed missing file vs GNU"; exit 1; }; \
	  echo "kore: sed (s///gp + d/p/q + number/\$$/regex/range addresses GNU-identical, exits 1/2) ok"
	@printf 'a b\nc\n' | xargs > $(ho)/.pc-g; printf 'a b\nc\n' | $(korerun) xargs > $(ho)/.pc-o; \
	  cmp -s $(ho)/.pc-g $(ho)/.pc-o || { echo "FAIL kore xargs vs GNU"; exit 1; }; \
	  printf '1\n2\n3\n4\n5\n' | xargs -n 2 echo > $(ho)/.pc-g; printf '1\n2\n3\n4\n5\n' | $(korerun) xargs -n 2 echo > $(ho)/.pc-o; \
	  cmp -s $(ho)/.pc-g $(ho)/.pc-o || { echo "FAIL kore xargs -n 2 vs GNU"; exit 1; }; \
	  printf '' | xargs echo > $(ho)/.pc-g; printf '' | $(korerun) xargs echo > $(ho)/.pc-o; \
	  cmp -s $(ho)/.pc-g $(ho)/.pc-o || { echo "FAIL kore xargs empty vs GNU"; exit 1; }; \
	  printf 'x\n' | $(korerun) xargs false; r=$$?; [ $$r -eq 123 ] || { echo "FAIL kore xargs fail exit (rc $$r)"; exit 1; }; \
	  printf 'x\n' | $(korerun) xargs /no/such/cmd 2>/dev/null; r=$$?; [ $$r -eq 127 ] || { echo "FAIL kore xargs 127 (rc $$r)"; exit 1; }; \
	  env AUP=44 sh -c 'printf %s "$$AUP"' > $(ho)/.pc-g; $(korerun) env AUP=44 sh -c 'printf %s "$$AUP"' > $(ho)/.pc-o; \
	  cmp -s $(ho)/.pc-g $(ho)/.pc-o || { echo "FAIL kore env assign vs GNU"; exit 1; }; \
	  env | grep -v '^_=' | LC_ALL=C sort > $(ho)/.pc-g; $(korerun) env | grep -v '^_=' | LC_ALL=C sort > $(ho)/.pc-o; \
	  cmp -s $(ho)/.pc-g $(ho)/.pc-o || { echo "FAIL kore env print vs GNU"; exit 1; }; \
	  $(korerun) env sh -c 'exit 3'; r=$$?; [ $$r -eq 3 ] || { echo "FAIL kore env child exit (rc $$r)"; exit 1; }; \
	  $(korerun) sleep 0.1 || { echo "FAIL kore sleep"; exit 1; }; \
	  $(korerun) sleep xx 2>/dev/null; r=$$?; [ $$r -eq 1 ] || { echo "FAIL kore sleep bad exit (rc $$r)"; exit 1; }; \
	  sleep 3 & sp=$$!; $(korerun) kill -9 $$sp || { echo "FAIL kore kill send"; exit 1; }; \
	  wait $$sp; r=$$?; [ $$r -eq 137 ] || { echo "FAIL kore kill effect (rc $$r)"; exit 1; }; \
	  $(korerun) kill -0 999999 2>/dev/null; r=$$?; [ $$r -eq 1 ] || { echo "FAIL kore kill dead pid (rc $$r)"; exit 1; }; \
	  echo "kore: process tools (env/sleep/kill/xargs -- GNU-identical output, the exit faces) ok"
# The editor (crew/vi/): the pure modal engine's laws (no tty -- vstep driven
# byte by byte), then scripted end-to-end passes through the `kore vi` face over a
# pipe (keys off stdin, frames onto a captured stdout, :wq writes) -- driven through
# the baked kore.image (--wake, like test_kore), not the cold cat.
.PHONY: test_vi
test_vi: host out/host$(hsuf)/kore.image
	@echo "VI crew/vi/{core,law}.l"; \
	  cat test/00-init.l crew/kore/text.l crew/kore/core.l crew/kore/re.l crew/vi/core.l crew/vi/law.l | $m > out/host/.test_vi.out 2>&1; r=$$?; \
	  cat out/host/.test_vi.out; \
	  { [ $$r -eq 0 ] && grep -q "crew/vi/law:" out/host/.test_vi.out; } \
	    || { echo "FAIL vi laws (exit $$r)"; exit 1; }
	@rm -f $(ho)/.vi1; \
	  printf 'ihello world\033:wq\n' | $(korerun) vi $(ho)/.vi1 > /dev/null 2>&1; r=$$?; \
	  { [ $$r -eq 0 ] && [ "$$(cat $(ho)/.vi1)" = "hello world" ]; } \
	    || { echo "FAIL kore vi create+write (exit $$r)"; exit 1; }; \
	  printf 'ddZZ' | $(korerun) vi $(ho)/.vi1 > /dev/null 2>&1; r=$$?; \
	  { [ $$r -eq 0 ] && [ "$$(cat $(ho)/.vi1)" = "" ]; } \
	    || { echo "FAIL kore vi dd+ZZ (exit $$r)"; exit 1; }; \
	  printf 'ix\033:q!\n' | $(korerun) vi $(ho)/.vi1 > /dev/null 2>&1; r=$$?; \
	  { [ $$r -eq 0 ] && [ "$$(cat $(ho)/.vi1)" = "" ]; } \
	    || { echo "FAIL kore vi q! holds fire (exit $$r)"; exit 1; }; \
	  printf 'AX\033u:wq\n' | $(korerun) vi $(ho)/.vi1 > /dev/null 2>&1; r=$$?; \
	  { [ $$r -eq 0 ] && [ "$$(cat $(ho)/.vi1)" = "" ]; } \
	    || { echo "FAIL kore vi undo (exit $$r)"; exit 1; }; \
	  echo "kore: vi (laws + piped create/dd/q!/undo end-to-end) ok"
# The C compiler (crew/moon/, rung 3 -- doc/moon.md): the pure pipeline's laws
# (lexer/parser/gen goldens), then the stage-0 end to end through the real
# `mooncc`: compile, run, exit 42 -- and the gcc -O0 differential is born
# (same source, both compilers, same exit). x86-64 only until arm64 parity.
# the battery drives the WARM-baked image (what `make install` ships and users
# run), not the cold source script -- ~0.68s -> ~0.02s per compile, 88 of them.
moonrun = $m --wake $(ho)/mooncc.image -e '(moon-main (cuup (cup cmdline)))'
.PHONY: test_moon
test_moon: host out/host$(hsuf)/mooncc out/host$(hsuf)/mooncc.image
	@echo "CC crew/moon/{lex,cpp,parse,gen,law}.l"; \
	  { echo "(use 'holo)"; cat test/00-init.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l crew/holo/text.l crew/moon/gen.l crew/moon/law.l; } | $m > out/host/.test_moon.out 2>&1; r=$$?; \
	  cat out/host/.test_moon.out; \
	  { [ $$r -eq 0 ] && grep -q "crew/moon/law:" out/host/.test_moon.out; } \
	    || { echo "FAIL cc laws (exit $$r)"; exit 1; }
	@if [ "$$(uname -m)" = x86_64 ]; then \
	  printf 'int main() { return 42; }\n' > $(ho)/.cc1.c; \
	  $(moonrun) $(ho)/.cc1.c $(ho)/.cc1 > /dev/null 2>&1 || { echo "FAIL mooncc compile"; exit 1; }; \
	  $(ho)/.cc1; a=$$?; \
	  cc_g=$$(command -v gcc || command -v cc); \
	  $$cc_g -O0 -o $(ho)/.cc1g $(ho)/.cc1.c && $(ho)/.cc1g; b=$$?; \
	  { [ $$a -eq 42 ] && [ $$a -eq $$b ]; } || { echo "FAIL mooncc vs gcc (ours $$a gcc $$b)"; exit 1; }; \
	  printf '// c\nint f() { return 1; }\nint main() { return 7; }\n' > $(ho)/.cc2.c; \
	  $(moonrun) $(ho)/.cc2.c $(ho)/.cc2 > /dev/null 2>&1 && $(ho)/.cc2; a=$$?; \
	  $$cc_g -O0 -o $(ho)/.cc2g $(ho)/.cc2.c && $(ho)/.cc2g; b=$$?; \
	  [ $$a -eq $$b ] || { echo "FAIL mooncc two-fn vs gcc (ours $$a gcc $$b)"; exit 1; }; \
	  for f in test/cc/*.c; do \
	    $(moonrun) $$f $(ho)/.ccb > /dev/null 2>&1 || { echo "FAIL mooncc compile $$f"; exit 1; }; \
	    $(ho)/.ccb; a=$$?; \
	    $$cc_g -O0 -o $(ho)/.ccbg $$f && $(ho)/.ccbg; b=$$?; \
	    [ $$a -eq $$b ] || { echo "FAIL mooncc battery $$f (ours $$a gcc $$b)"; exit 1; }; \
	  done; \
	  $(moonrun) $(ho)/.cc-none.c $(ho)/.ccx > /dev/null 2>&1; r=$$?; \
	  [ $$r -eq 1 ] || { echo "FAIL mooncc missing input exit (rc $$r)"; exit 1; }; \
	  printf 'int main() { return 42 }\n' > $(ho)/.cc3.c; \
	  $(moonrun) $(ho)/.cc3.c $(ho)/.ccx > /dev/null 2>&1; r=$$?; \
	  [ $$r -eq 1 ] || { echo "FAIL mooncc parse-error exit (rc $$r)"; exit 1; }; \
	  printf 'int vals[3] = {10,20,12};\nchar *tag = "x";\nint pick(int i){return vals[i];}\n' > $(ho)/.olib.c; \
	  printf 'extern int vals[];\nint pick(int i);\nint ext_add(int a,int b);\nint main(){return pick(0)+vals[2]+ext_add(15,5);}\n' > $(ho)/.omain.c; \
	  printf 'int ext_add(int a,int b){return a+b;}\n' > $(ho)/.oext.c; \
	  $(moonrun) -c $(ho)/.olib.c  $(ho)/.olib.o  > /dev/null 2>&1 || { echo "FAIL mooncc -c lib"; exit 1; }; \
	  $(moonrun) -c $(ho)/.omain.c $(ho)/.omain.o > /dev/null 2>&1 || { echo "FAIL mooncc -c main"; exit 1; }; \
	  $$cc_g -O0 -c -o $(ho)/.oext.o $(ho)/.oext.c; \
	  $$cc_g -no-pie -o $(ho)/.oexe $(ho)/.omain.o $(ho)/.olib.o $(ho)/.oext.o > /dev/null 2>&1 || { echo "FAIL ld cc objects"; exit 1; }; \
	  $(ho)/.oexe; a=$$?; \
	  $$cc_g -O0 -o $(ho)/.oexeg $(ho)/.omain.c $(ho)/.olib.c $(ho)/.oext.c && $(ho)/.oexeg; b=$$?; \
	  { [ $$a -eq 42 ] && [ $$a -eq $$b ]; } || { echo "FAIL mooncc -c link+run (ours $$a gcc $$b)"; exit 1; }; \
	  printf '#include <ans.h>\nint main() { return ANS + BONUS; }\n' > $(ho)/.flg.c; \
	  mkdir -p $(ho)/.flginc && printf '#define ANS 30\n' > $(ho)/.flginc/ans.h; \
	  $(moonrun) -I $(ho)/.flginc -D BONUS=12 -o $(ho)/.flg $(ho)/.flg.c > /dev/null 2>&1 || { echo "FAIL mooncc -I/-D/-o"; exit 1; }; \
	  $(ho)/.flg; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL mooncc -I/-D/-o run (got $$a want 42)"; exit 1; }; \
	  printf 'int main() { long v; asm("li %%0, 40" : "=r"(v)); return v + 2; }\n' > $(ho)/.casm1.c; \
	  $(moonrun) $(ho)/.casm1.c $(ho)/.casm1 > /dev/null 2>&1 || { echo "FAIL mooncc asm compile"; exit 1; }; \
	  $(ho)/.casm1; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL mooncc asm output operand (got $$a want 42)"; exit 1; }; \
	  printf 'int main() { asm volatile("sys" : : "r0"(60), "r6"(42)); return 0; }\n' > $(ho)/.casm2.c; \
	  $(moonrun) $(ho)/.casm2.c $(ho)/.casm2 > /dev/null 2>&1 || { echo "FAIL mooncc asm syscall compile"; exit 1; }; \
	  $(ho)/.casm2; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL mooncc asm pinned-reg syscall (got $$a want 42)"; exit 1; }; \
	  printf 'int main() { long x = 30; asm("add %%0, %%0, %%1" : "+r"(x) : "r"(12L)); return x; }\n' > $(ho)/.casm3.c; \
	  $(moonrun) $(ho)/.casm3.c $(ho)/.casm3 > /dev/null 2>&1 || { echo "FAIL mooncc asm in-out compile"; exit 1; }; \
	  $(ho)/.casm3; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL mooncc asm in-out (got $$a want 42)"; exit 1; }; \
	  $(moonrun) -t arm64 -o $(ho)/.casm1a $(ho)/.casm1.c > /dev/null 2>&1 || { echo "FAIL mooncc asm arm64 compile"; exit 1; }; \
	  printf 'int f();\nint main() { return f() + 2; }\n' > $(ho)/.mi1.c; \
	  printf 'int f() { return 40; }\n' > $(ho)/.mi2.c; \
	  mabs="$$PWD/$(ho)"; (cd $(ho) && $$mabs/love --wake $$mabs/mooncc.image -e '(moon-main (cuup (cup cmdline)))' -c .mi1.c .mi2.c) > /dev/null 2>&1 || { echo "FAIL mooncc multi-input -c"; exit 1; }; \
	  $$cc_g -no-pie -o $(ho)/.mi $(ho)/.mi1.o $(ho)/.mi2.o > /dev/null 2>&1 || { echo "FAIL ld multi-input objects"; exit 1; }; \
	  $(ho)/.mi; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL mooncc multi-input run (got $$a want 42)"; exit 1; }; \
	  printf '#include <stdarg.h>\nint isum(int n,...){va_list ap;va_start(ap,n);long s=0;for(int i=0;i<n;i++)s+=va_arg(ap,int);va_end(ap);return s;}\n' > $(ho)/.valib.c; \
	  printf 'int isum(int n,...);\nint main(){return isum(4,10,11,12,9);}\n' > $(ho)/.vamain.c; \
	  $(moonrun) -c $(ho)/.valib.c $(ho)/.valib.o > /dev/null 2>&1 || { echo "FAIL mooncc -c variadic"; exit 1; }; \
	  $$cc_g -O0 -c -o $(ho)/.vamain.o $(ho)/.vamain.c; \
	  $$cc_g -no-pie -o $(ho)/.vaexe $(ho)/.vamain.o $(ho)/.valib.o > /dev/null 2>&1 || { echo "FAIL ld cc-variadic + gmoon-main"; exit 1; }; \
	  $(ho)/.vaexe; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL cc-variadic <- gcc-caller (SysV va ABI, got $$a want 42)"; exit 1; }; \
	  printf '__attribute__((weak)) int wpick(void){return 7;}\nint main(){return wpick() + 30;}\n' > $(ho)/.wklib.c; \
	  printf 'int wpick(void){return 12;}\n' > $(ho)/.wkstr.c; \
	  $(moonrun) -c $(ho)/.wklib.c $(ho)/.wklib.o > /dev/null 2>&1 || { echo "FAIL mooncc -c weak"; exit 1; }; \
	  $$cc_g -no-pie -o $(ho)/.wkdef $(ho)/.wklib.o > /dev/null 2>&1 && $(ho)/.wkdef; a=$$?; \
	  [ $$a -eq 37 ] || { echo "FAIL weak default (got $$a want 37)"; exit 1; }; \
	  $$cc_g -O0 -c -o $(ho)/.wkstr.o $(ho)/.wkstr.c; \
	  $$cc_g -no-pie -o $(ho)/.wkovr $(ho)/.wklib.o $(ho)/.wkstr.o > /dev/null 2>&1 && $(ho)/.wkovr; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL weak override (got $$a want 42)"; exit 1; }; \
	  printf 'long bump(long x){long r;__builtin_add_overflow(x,1,&r);return r;}\n' > $(ho)/.rblib.c; \
	  printf 'long bump(long);\nint main(void){volatile long a=0;long s=a;for(int i=42;i--;)s=bump(s);return (int)s;}\n' > $(ho)/.rbmain.c; \
	  $(moonrun) -c $(ho)/.rblib.c $(ho)/.rblib.o > /dev/null 2>&1 || { echo "FAIL mooncc -c rbx-callee"; exit 1; }; \
	  $$cc_g -O2 -c -o $(ho)/.rbmain.o $(ho)/.rbmain.c; \
	  $$cc_g -no-pie -o $(ho)/.rbexe $(ho)/.rbmain.o $(ho)/.rblib.o > /dev/null 2>&1 || { echo "FAIL ld rbx interop"; exit 1; }; \
	  $(ho)/.rbexe; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL callee-saved rbx across cc call (-O2 caller loop bound, got $$a want 42)"; exit 1; }; \
	  printf 'static long cd(long n,long a){if(n==0)return a;return cd(n-1,a+1);}\nstatic long tb(long n);\nstatic long ta(long n){if(n==0)return 21;return tb(n-1);}\nstatic long tb(long n){if(n==0)return 22;return ta(n-1);}\nstatic long dp(long(*f)(long,long),long n){return f(n,0);}\nint main(void){long a=cd(50000000,0)/2500000;long b=ta(30000000);long c=dp(cd,1000000)/1000000;return (int)(a+b+c);}\n' > $(ho)/.sib.c; \
	  $(moonrun) $(ho)/.sib.c $(ho)/.sibx > /dev/null 2>&1 || { echo "FAIL mooncc sibcall"; exit 1; }; \
	  $(ho)/.sibx; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL sibcall flat recursion (50M-deep self + mutual + fn-ptr, got $$a want 42 -- a non-tail call would stack-overflow to 139)"; exit 1; }; \
	  printf 'long fork(void);long waitpid(long,int*,long);void _exit(long);long g;long id(long a){return a;}int main(void){g=id(fork());if(g==0)_exit(42);int st;waitpid(-1,&st,0);return ((st&127)==0&&((st>>8)&255)==42)?42:1;}\n' > $(ho)/.aln.c; \
	  $(moonrun) -c $(ho)/.aln.c $(ho)/.aln.o > /dev/null 2>&1 || { echo "FAIL mooncc -c stack-align"; exit 1; }; \
	  $$cc_g -no-pie -o $(ho)/.alnx $(ho)/.aln.o > /dev/null 2>&1 || { echo "FAIL ld stack-align"; exit 1; }; \
	  $(ho)/.alnx; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL 16-byte stack alignment ('g=id(fork())' holds a temp across the call; a bare-push spill leaves rsp at 8 mod 16 and glibc fork's child movaps #GPs -- got $$a want 42)"; exit 1; }; \
	  $(moonrun) -c $(ho)/.oext.c $(ho)/.oext2.o > /dev/null 2>&1 || { echo "FAIL mooncc -c ext"; exit 1; }; \
	  $(moonrun) $(ho)/.omain.o $(ho)/.olib.o $(ho)/.oext2.o -o $(ho)/.lnk1 > /dev/null 2>&1 || { echo "FAIL mooncc link .o"; exit 1; }; \
	  $(ho)/.lnk1; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL mooncc-linked exe (own static linker, got $$a want 42)"; exit 1; }; \
	  $(moonrun) $(ho)/.mi1.c $(ho)/.mi2.c -o $(ho)/.lnk2 > /dev/null 2>&1 || { echo "FAIL mooncc link multi-.c"; exit 1; }; \
	  $(ho)/.lnk2; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL mooncc multi-.c link (got $$a want 42)"; exit 1; }; \
	  $(moonrun) -c $(ho)/.wkstr.c $(ho)/.wkstr2.o > /dev/null 2>&1 || { echo "FAIL mooncc -c weak-strong"; exit 1; }; \
	  $(moonrun) $(ho)/.wklib.o -o $(ho)/.lnk3 > /dev/null 2>&1 && $(ho)/.lnk3; a=$$?; \
	  [ $$a -eq 37 ] || { echo "FAIL mooncc-link weak default (got $$a want 37)"; exit 1; }; \
	  $(moonrun) $(ho)/.wklib.o $(ho)/.wkstr2.o -o $(ho)/.lnk4 > /dev/null 2>&1 && $(ho)/.lnk4; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL mooncc-link weak override (got $$a want 42)"; exit 1; }; \
	  printf 'typedef struct { char *n; long v; } ent;\n__attribute__((section("ai_nifs"))) ent e1 = { "a", 30 };\n' > $(ho)/.nf1.c; \
	  printf 'typedef struct { char *n; long v; } ent;\n__attribute__((section("ai_nifs"))) ent e2 = { "b", 12 };\nextern ent __start_ai_nifs[];\nextern ent __stop_ai_nifs[];\nint main(){ long s=0; for (ent *p=__start_ai_nifs; p<__stop_ai_nifs; p++) s+=p->v; return (int)s; }\n' > $(ho)/.nf2.c; \
	  $(moonrun) $(ho)/.nf2.c $(ho)/.nf1.c -o $(ho)/.lnk5 > /dev/null 2>&1 || { echo "FAIL mooncc link ai_nifs"; exit 1; }; \
	  $(ho)/.lnk5; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL ai_nifs bracket walk (two TUs packed + __start_/__stop_ synthesized, got $$a want 42)"; exit 1; }; \
	  echo "mooncc: cc (laws + return-42 + a $$(ls test/cc/*.c | wc -l)-program gcc battery + .o link/interop + -I/-D/-o + multi-input -c + SysV varargs cross-toolchain + weak override + callee-saved rbx + guaranteed sibcalls + 16-byte stack alignment + our own static linker: multi-.o/.c link, weak strong-over, ai_nifs brackets) ok"; \
	else echo "mooncc: cc (laws only -- x86_64 e2e skipped on $$(uname -m)) ok"; fi
# The rung-2 self-host gate ([[love-distro]]): compile love.c AND every host/*.c with
# mooncc (gcc/clang only LINKS), then run the whole corpus through the all-mooncc
# binary. Proves the compiler compiles the runtime it runs on. OPT-IN, not in
# test_all -- it rebuilds ~14 objects + links + runs the corpus, and needs the
# system static linker. x86-64 only (mooncc emits x64). The binary carries
# no baked image, so LOVE_NO_IMAGE forces the fresh-egg boot.
.PHONY: test_selfhost
test_selfhost: host out/host$(hsuf)/mooncc
	@echo SELFHOST $(ho)/love-selfhost
	@if [ "`uname -m`" != x86_64 ]; then echo "test_selfhost: x86-64 only, skipped on `uname -m`"; exit 0; fi; \
	  d=$(ho)/selfhost; mkdir -p $$d; rm -f $$d/*.o; \
	  $(ho)/mooncc -D ai_tco=$(tco) -I$(ho) -I. -Iout/lib -c love.c $$d/love.o \
	    || { echo "FAIL mooncc -c love.c"; exit 1; }; \
	  for f in host/*.c; do b=`basename $$f .c`; \
	    $(ho)/mooncc -D ai_tco=$(tco) -I$(ho) -I. -Iout/lib -c $$f $$d/$$b.o \
	      || { echo "FAIL mooncc -c $$f"; exit 1; }; done; \
	  $(ho)/mooncc -Icrew/moon/include -c crew/moon/lib/math/am.c $$d/am.o \
	    || { echo "FAIL mooncc -c am.c"; exit 1; }; \
	  $(host_cc) -static -o $(ho)/love-selfhost $$d/*.o $(host_ldflags) \
	    || { echo "FAIL link all-mooncc binary"; exit 1; }; \
	  cat $t | LOVE_NO_IMAGE=1 $(ho)/love-selfhost > $(ho)/.test_selfhost.out 2>&1; s=$$?; \
	  tail -1 $(ho)/.test_selfhost.out; \
	  { [ $$s -eq 0 ] && grep -q "tests pass" $(ho)/.test_selfhost.out; } \
	    || { echo "FAIL all-mooncc corpus (exit $$s)"; exit 1; }; \
	  echo "test_selfhost: love.c + all `ls host/*.c | wc -l` host/*.c built by mooncc, corpus passes"
# The rung-4 gate ([[love-distro]]): the GCC-FREE fixpoint. Everything test_selfhost
# builds, PLUS our own raw libc -- crew/moon/lib/nolibc.c (raw-syscall wrappers, mini
# stdio, mmap malloc), the math floor crew/moon/lib/math/am.c (ours), and sys.o (the
# syscall trampoline + our sigsetjmp/longjmp, laid by crew/moon/lib/mksys.l) -- then
# OUR OWN static linker (crew/holo/link.l via `mooncc a.o..`) binds them. No gcc, no
# glibc, no ld anywhere: the whole chain is love. Corpus green over the fresh egg.
# In test_all (the gcc-free fixpoint is a headline invariant); skips off x86-64.
# mksys/nolibc/math are x64. Supersedes test_selfhost's coverage (which stays
# opt-in as the lighter gcc-links-only check).
.PHONY: test_raw
test_raw: host out/host$(hsuf)/mooncc
	@echo RAW $(ho)/love-raw
	@if [ "`uname -m`" != x86_64 ]; then echo "test_raw: x86-64 only, skipped on `uname -m`"; exit 0; fi; \
	  d=$(ho)/raw; mkdir -p $$d; rm -f $$d/*.o; \
	  $(ho)/mooncc -D ai_tco=1 -I$(ho) -I. -Iout/lib -c love.c $$d/love.o \
	    || { echo "FAIL mooncc -c love.c"; exit 1; }; \
	  for f in host/*.c; do b=`basename $$f .c`; \
	    $(ho)/mooncc -D ai_tco=1 -I$(ho) -I. -Iout/lib -c $$f $$d/$$b.o \
	      || { echo "FAIL mooncc -c $$f"; exit 1; }; done; \
	  $(ho)/mooncc -Icrew/moon/include -c crew/moon/lib/nolibc.c $$d/nolibc.o \
	    || { echo "FAIL mooncc -c nolibc.c"; exit 1; }; \
	  for f in crew/moon/lib/math/*.c; do b=`basename $$f .c`; \
	    $(ho)/mooncc -Icrew/moon/lib/math -Icrew/moon/include -c $$f $$d/m_$$b.o \
	      || { echo "FAIL mooncc -c $$f"; exit 1; }; done; \
	  { cat crew/kore/text.l crew/kore/core.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l; \
	    echo "(mksys \"$$d/sys.o\")"; } | $m \
	    || { echo "FAIL mksys sys.o"; exit 1; }; \
	  $(ho)/mooncc $$d/*.o -o $(ho)/love-raw \
	    || { echo "FAIL our-linker bind love-raw"; exit 1; }; \
	  cat $t | LOVE_NO_IMAGE=1 $(ho)/love-raw > $(ho)/.test_raw.out 2>&1; s=$$?; \
	  tail -1 $(ho)/.test_raw.out; \
	  { [ $$s -eq 0 ] && grep -q "tests pass" $(ho)/.test_raw.out; } \
	    || { echo "FAIL all-raw corpus (exit $$s)"; exit 1; }; \
	  echo "test_raw: love.c + host/*.c + nolibc + am math + sys.o, our linker, no gcc/glibc/ld -- corpus passes"
# test_raw_bake -- the WAKE half of the gcc-free image cycle. mooncc could never bake
# before (an EXEC's low load address collides the codec's pointer/fixnum index range);
# a -pie ET_DYN loads high and clears it. Reuses test_raw's raw objects, links them
# PIE, bakes the post-warm image into the binary's OWN .image (host/image.c self-patch),
# then WAKES that image and runs the corpus over the woken heap. This is the ONLY gate
# that exercises ai_image_load on a mooncc binary -- the seam where [[mooncc-fn-parity]]
# hid (an odd-addressed lvm_* ap mis-encodes as a fixnum, invisible to every egg-boot
# gate; the fix aligns fns to even in gen.l). glaze.l is EXCLUDED: the bake pulls the
# nif/nifx seam off the book (host/main.c), so (lit? nif) cannot hold on a woken heap
# -- the gcc build fails it identically, so it is not a mooncc regression.
# Opt-in (not test_all): needs the -pie toolchain. x86-64 only.
.PHONY: test_raw_bake
test_raw_bake: test_raw
	@echo BAKE-WAKE $(ho)/love-raw-pie
	@if [ "`uname -m`" != x86_64 ]; then echo "test_raw_bake: x86-64 only, skipped on `uname -m`"; exit 0; fi; \
	  d=$(ho)/raw; \
	  $(ho)/mooncc -pie $$d/*.o -o $(ho)/love-raw-pie \
	    || { echo "FAIL -pie link love-raw-pie"; exit 1; }; \
	  cp $(ho)/love-raw-pie $(ho)/love-raw-baked; \
	  $(ho)/love-raw-baked --bake >/dev/null 2>&1 \
	    || { echo "FAIL --bake (mooncc-PIE binary refused to snapshot its own image)"; exit 1; }; \
	  cat $(filter-out %/glaze.l,$t) | $(ho)/love-raw-baked > $(ho)/.test_raw_bake.out 2>&1; s=$$?; \
	  tail -1 $(ho)/.test_raw_bake.out; \
	  { [ $$s -eq 0 ] && grep -q "tests pass" $(ho)/.test_raw_bake.out; } \
	    || { echo "FAIL woken corpus (exit $$s) -- ai_image_load desync? see [[mooncc-fn-parity]]"; exit 1; }; \
	  echo "test_raw_bake: the mooncc-PIE binary bakes its own image and WAKES it -- corpus passes on the woken heap"
# test_riscv -- the riscv64 codegen rung end to end: the whole test/cc battery
# compiled `mooncc -t riscv64` (EM_RISCV static ELF, the holo riscv backend),
# run under qemu-riscv64 (user mode), and DIFFERENTIAL against the native x64
# mooncc build of the same file -- mooncc is its own reference (the frontend is
# shared, so only codegen can diverge). the three x64-only features
# (100-complex / 101-vla / 102-bigstruct) are excluded exactly as arm64
# refuses them. skips clean without qemu-riscv64 or off x86_64.
.PHONY: test_riscv
test_riscv: host out/host$(hsuf)/mooncc out/host$(hsuf)/mooncc.image
	@echo "RISCV test/cc battery (mooncc -t riscv64 vs native x64, under qemu-riscv64)"
	@if ! command -v qemu-riscv64 >/dev/null 2>&1 || [ "$$(uname -m)" != x86_64 ]; then \
	   echo "test_riscv: skipped (needs qemu-riscv64 + an x86_64 host)"; exit 0; fi; \
	  d=$(ho)/riscv; mkdir -p $$d; p=0; \
	  for f in test/cc/*.c; do b=$$(basename $$f .c); \
	    case $$b in 100-complex|101-vla|102-bigstruct) continue;; esac; \
	    $(moonrun) -t riscv64 $$f $$d/rv_$$b > /dev/null 2>&1 || { echo "FAIL riscv compile $$f"; exit 1; }; \
	    qemu-riscv64 $$d/rv_$$b; a=$$?; \
	    $(moonrun) $$f $$d/x_$$b > /dev/null 2>&1 || { echo "FAIL x64 compile $$f"; exit 1; }; \
	    $$d/x_$$b; x=$$?; \
	    [ $$a -eq $$x ] || { echo "FAIL riscv battery $$f (rv $$a x64 $$x)"; exit 1; }; \
	    p=$$((p+1)); \
	  done; \
	  echo "test_riscv: $$p/$$p battery files agree riscv-vs-x64"
# test_raw's riscv64 twin: mooncc -t riscv64 lays every object (the holo riscv
# backend + the .o/link reloc path), mksys-riscv the syscall leaf (same
# asm-generic table as arm64), OUR linker binds, qemu-riscv64 (user) runs the
# whole corpus over the fresh egg. The riscv backend loads into the sealed holo
# module at runtime for the mksys step (the host bake carries only the native
# backend); mooncc.image carries all backends already. Opt-in (not in
# test_all): the qemu corpus costs a minute. Skips without qemu-riscv64.
.PHONY: test_raw_riscv
test_raw_riscv: host out/host$(hsuf)/mooncc out/lib/riscv.h
	@echo RAW-RISCV $(ho)/love-raw-rv
	@if ! command -v qemu-riscv64 >/dev/null 2>&1; then echo "test_raw_riscv: no qemu-riscv64, skipped"; exit 0; fi; \
	  d=$(ho)/raw-rv; mkdir -p $$d; rm -f $$d/*.o; \
	  $(ho)/mooncc -t riscv64 -D ai_tco=1 -I$(ho) -I. -Iout/lib -c love.c $$d/love.o \
	    || { echo "FAIL mooncc -t riscv64 -c love.c"; exit 1; }; \
	  for f in host/*.c; do b=`basename $$f .c`; \
	    $(ho)/mooncc -t riscv64 -D ai_tco=1 -I$(ho) -I. -Iout/lib -c $$f $$d/$$b.o \
	      || { echo "FAIL mooncc -t riscv64 -c $$f"; exit 1; }; done; \
	  $(ho)/mooncc -t riscv64 -Icrew/moon/include -c crew/moon/lib/nolibc.c $$d/nolibc.o \
	    || { echo "FAIL mooncc -t riscv64 -c nolibc.c"; exit 1; }; \
	  for f in crew/moon/lib/math/*.c; do b=`basename $$f .c`; \
	    $(ho)/mooncc -t riscv64 -Icrew/moon/lib/math -Icrew/moon/include -c $$f $$d/m_$$b.o \
	      || { echo "FAIL mooncc -t riscv64 -c $$f"; exit 1; }; done; \
	  { echo "(enter ()) (use 'holo)"; cat crew/holo/riscv.l; echo "(leave ())"; \
	    cat crew/kore/text.l crew/kore/core.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l; \
	    echo "(mksys-riscv \"$$d/sys.o\")"; } | $m \
	    || { echo "FAIL mksys-riscv sys.o"; exit 1; }; \
	  $(ho)/mooncc -t riscv64 $$d/*.o -o $(ho)/love-raw-rv \
	    || { echo "FAIL our-linker bind love-raw-rv"; exit 1; }; \
	  cat $t \
	    | LOVE_NO_IMAGE=1 qemu-riscv64 $(ho)/love-raw-rv > $(ho)/.test_raw_rv.out 2>&1; s=$$?; \
	  tail -1 $(ho)/.test_raw_rv.out; \
	  { [ $$s -eq 0 ] && grep -q "tests pass" $(ho)/.test_raw_rv.out; } \
	    || { echo "FAIL raw-riscv corpus (exit $$s)"; exit 1; }; \
	  echo "test_raw_riscv: the gcc-free riscv64 love -- mooncc objects, mksys-riscv, our linker, corpus under qemu"
# test_raw's aarch64 twin (rung D): mooncc -t arm64 lays every object, mksys-arm64
# the syscall leaf, OUR linker binds, qemu-user runs the corpus over the fresh
# egg. Runs the WHOLE C-sorted $t (uukind{,law}.l included): the raw binary and
# the GCC-built reference agree file-for-file -- tools/arm64check.sh is the
# differential. ($t is C/byte order, so test/uu.l loads before test/uukindlaw.l,
# which calls the kernel it defines; a locale `ls` sorts uukind* first and the
# uk-jj assert runs before uu.l -- an ordering trap, never a GC/arm64 bug.)
# Opt-in (not in test_all): the qemu corpus costs minutes. Skips without qemu.
.PHONY: test_raw_arm64
test_raw_arm64: host out/host$(hsuf)/mooncc
	@echo RAW-ARM64 $(ho)/love-raw-a64
	@if ! command -v qemu-aarch64 >/dev/null 2>&1; then echo "test_raw_arm64: no qemu-aarch64, skipped"; exit 0; fi; \
	  d=$(ho)/raw-a64; mkdir -p $$d; rm -f $$d/*.o; \
	  $(ho)/mooncc -t arm64 -D ai_tco=1 -I$(ho) -I. -Iout/lib -c love.c $$d/love.o \
	    || { echo "FAIL mooncc -t arm64 -c love.c"; exit 1; }; \
	  for f in host/*.c; do b=`basename $$f .c`; \
	    $(ho)/mooncc -t arm64 -D ai_tco=1 -I$(ho) -I. -Iout/lib -c $$f $$d/$$b.o \
	      || { echo "FAIL mooncc -t arm64 -c $$f"; exit 1; }; done; \
	  $(ho)/mooncc -t arm64 -Icrew/moon/include -c crew/moon/lib/nolibc.c $$d/nolibc.o \
	    || { echo "FAIL mooncc -t arm64 -c nolibc.c"; exit 1; }; \
	  for f in crew/moon/lib/math/*.c; do b=`basename $$f .c`; \
	    $(ho)/mooncc -t arm64 -Icrew/moon/lib/math -Icrew/moon/include -c $$f $$d/m_$$b.o \
	      || { echo "FAIL mooncc -t arm64 -c $$f"; exit 1; }; done; \
	  { echo "(enter ()) (use 'holo)"; cat crew/holo/arm64.l; echo "(leave ())"; \
	    cat crew/kore/text.l crew/kore/core.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l; \
	    echo "(mksys-arm64 \"$$d/sys.o\")"; } | $m \
	    || { echo "FAIL mksys-arm64 sys.o"; exit 1; }; \
	  $(ho)/mooncc -t arm64 $$d/*.o -o $(ho)/love-raw-a64 \
	    || { echo "FAIL our-linker bind love-raw-a64"; exit 1; }; \
	  cat $t \
	    | LOVE_NO_IMAGE=1 qemu-aarch64 $(ho)/love-raw-a64 > $(ho)/.test_raw_a64.out 2>&1; s=$$?; \
	  tail -1 $(ho)/.test_raw_a64.out; \
	  { [ $$s -eq 0 ] && grep -q "tests pass" $(ho)/.test_raw_a64.out; } \
	    || { echo "FAIL raw-arm64 corpus (exit $$s)"; exit 1; }; \
	  echo "test_raw_arm64: the gcc-free aarch64 love -- mooncc objects, mksys-arm64, our linker, corpus under qemu"
# test_thumb1 -- the ELF32/EM_ARM object writer (crew/holo/obj.l objelf32) end to end,
# and the 32-bit data model. mooncc -t thumb1 -c lays objects exercising a cross-object
# BL (R_ARM_THM_CALL), the inline v6-M soft divide/rem, a scalar global read+write via
# the literal-pool `la` (R_ARM_ABS32), and -- the cross-ABI catch for the struct-layout
# bug -- a pointer-bearing struct BUILT by arm-none-eabi-gcc (the reference ABI: 4-byte
# pointer, `x` at offset 4) whose field a mooncc function reads back. A parse-side
# pointer-width regression (8 not 4) makes mooncc read offset 8 and the answer diverge.
# arm-none-eabi-ld binds it all against a gas startup; qemu runs it on an emulated
# Cortex-M0 (semihosting SYS_EXIT_EXTENDED carries the answer out). tiny (~1s), rides
# test_all; skips clean without the arm toolchain or qemu-system-arm.
# qemu reads </dev/null: -nographic muxes the guest serial+monitor onto stdio, so
# without a definite-EOF stdin qemu BLOCKS on the host chardev when the recipe runs
# without an interactive tty -- the guest exits via semihosting instantly (a bare run
# returns 127) but qemu-in-make would hang to the timeout (124). Host I/O, not codegen.
.PHONY: test_thumb1
test_thumb1: host out/host$(hsuf)/mooncc
	@echo THUMB1 $(ho)/thumb1
	@if ! command -v arm-none-eabi-gcc >/dev/null 2>&1 || ! command -v arm-none-eabi-ld >/dev/null 2>&1 || ! command -v qemu-system-arm >/dev/null 2>&1; then \
	   echo "test_thumb1: no arm-none-eabi toolchain / qemu-system-arm, skipped"; exit 0; fi; \
	  d=$(ho)/thumb1; mkdir -p $$d; rm -f $$d/*.o; \
	  { printf 'int acc = 40;\n'; \
	    printf 'int arr[4];\n'; \
	    printf 'struct S { int *p; int x; };\n'; \
	    printf 'int addto(int x){ acc = acc + x; return acc; }\n'; \
	    printf 'int divmod(int a,int b){ return a/b + a%%b; }\n'; \
	    printf 'int sx(struct S *s){ return s->x; }\n'; \
	    printf 'int aset(int i,int v){ arr[i] = v; return 0; }\n'; \
	    printf 'int aget(int i){ return arr[i]; }\n'; } > $$d/lib.c; \
	  $(ho)/mooncc -t thumb1 -c $$d/lib.c  $$d/lib.o  || { echo "FAIL mooncc -t thumb1 -c lib"; exit 1; }; \
	  { echo 'struct S { int *p; int x; };'; \
	    echo 'int addto(int); int divmod(int,int); int sx(struct S*); int aset(int,int); int aget(int);'; \
	    echo 'int run(void){ int t = 0; struct S s; s.p = &t; s.x = 30;'; \
	    echo '  int dd = divmod(-17,5); addto(50); aset(3, 12);'; \
	    echo '  return addto(dd) + sx(&s) + aget(3); }'; } > $$d/harness.c; \
	  arm-none-eabi-gcc -mcpu=cortex-m0 -mthumb -ffreestanding -O2 -c $$d/harness.c -o $$d/harness.o || { echo "FAIL gcc harness"; exit 1; }; \
	  { echo '.syntax unified'; echo '.cpu cortex-m0'; echo '.thumb'; \
	    echo '.section .vectors,"a"'; echo '.word 0x20004000'; echo '.word _start+1'; \
	    echo '.text'; echo '.thumb_func'; echo '.global _start'; echo '_start:'; \
	    echo '  bl run'; echo '  ldr r1, =0x20026'; echo '  push {r0}'; echo '  push {r1}'; \
	    echo '  mov r1, sp'; echo '  movs r0, #0x20'; echo '  bkpt 0xAB'; echo '  b .'; } > $$d/start.S; \
	  { echo 'MEMORY'; echo '{'; echo '  FLASH (rx) : ORIGIN = 0, LENGTH = 256K'; \
	    echo '  RAM  (rwx) : ORIGIN = 0x20000000, LENGTH = 16K'; echo '}'; \
	    echo 'SECTIONS'; echo '{'; echo '  .text : { KEEP(*(.vectors)) *(.text*) *(.rodata*) } > FLASH'; \
	    echo '  .data : { *(.data*) } > RAM'; echo '  .bss : { *(.bss*) } > RAM'; echo '}'; } > $$d/link.ld; \
	  arm-none-eabi-gcc -mcpu=cortex-m0 -mthumb -c $$d/start.S -o $$d/start.o || { echo "FAIL as start.S"; exit 1; }; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harness.o $$d/lib.o -o $$d/t1.elf || { echo "FAIL ld thumb1 objects"; exit 1; }; \
	  timeout 30 qemu-system-arm -M microbit -semihosting -nographic -kernel $$d/t1.elf </dev/null; a=$$?; \
	  [ $$a -eq 127 ] || { echo "FAIL thumb1 -c link+run (got $$a, want 127 = addto(40+50) then addto(-17/5 + -17%%5)=85 + s->x=30 + arr[3]=12; a wrong struct offset misreads s->x, a wrong leax scale/base misreads arr[3])"; exit 1; }; \
	  cp test/thumb1/libv.c test/thumb1/harnessv.c $$d/; \
	  $(ho)/mooncc -t thumb1 -c $$d/libv.c $$d/libv.o || { echo "FAIL mooncc -t thumb1 -c libv"; exit 1; }; \
	  arm-none-eabi-gcc -mcpu=cortex-m0 -mthumb -ffreestanding -O2 -c $$d/harnessv.c -o $$d/harnessv.o || { echo "FAIL gcc harnessv"; exit 1; }; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harnessv.o $$d/libv.o -o $$d/t1v.elf || { echo "FAIL ld thumb1 vararg objects"; exit 1; }; \
	  timeout 30 qemu-system-arm -M microbit -semihosting -nographic -kernel $$d/t1v.elf </dev/null; a=$$?; \
	  [ $$a -eq 7 ] || { echo "FAIL thumb1 varargs (got $$a, want 7 = every differential check vs gcc; 100+n names the first miss -- see test/thumb1/harnessv.c; the pop-r3/bx epilogue or the r0-r3 push block over lr/fp/r4 is the usual suspect)"; exit 1; }; \
	  cp test/thumb2/lib64.c test/thumb2/harness64.c $$d/; \
	  $(ho)/mooncc -t thumb1 -c $$d/lib64.c $$d/lib64.o || { echo "FAIL mooncc -t thumb1 -c lib64"; exit 1; }; \
	  arm-none-eabi-gcc -mcpu=cortex-m0 -mthumb -ffreestanding -O2 -c $$d/harness64.c -o $$d/harness64.o || { echo "FAIL gcc harness64"; exit 1; }; \
	  lg=`arm-none-eabi-gcc -mcpu=cortex-m0 -mthumb -print-libgcc-file-name`; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harness64.o $$d/lib64.o $$lg -o $$d/t1p.elf || { echo "FAIL ld thumb1 pair objects"; exit 1; }; \
	  timeout 30 qemu-system-arm -M microbit -semihosting -nographic -kernel $$d/t1p.elf </dev/null; a=$$?; \
	  [ $$a -eq 45 ] || { echo "FAIL thumb1 64-bit pairs (got $$a, want 45 = every differential check vs gcc; 100+n names the first miss -- see test/thumb2/harness64.c; the v6-M lanes ride ADCS/SBCS inline + __aeabi_lmul/(u)ldivmod/shift libcalls)"; exit 1; }; \
	  cp test/thumb2/libd.c test/thumb2/harnessd.c $$d/; \
	  $(ho)/mooncc -t thumb1 -c $$d/libd.c $$d/libd.o || { echo "FAIL mooncc -t thumb1 -c libd"; exit 1; }; \
	  arm-none-eabi-gcc -mcpu=cortex-m0 -mthumb -ffreestanding -O2 -c $$d/harnessd.c -o $$d/harnessd.o || { echo "FAIL gcc harnessd"; exit 1; }; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harnessd.o $$d/libd.o $$lg -o $$d/t1d.elf || { echo "FAIL ld thumb1 double objects"; exit 1; }; \
	  timeout 30 qemu-system-arm -M microbit -semihosting -nographic -kernel $$d/t1d.elf </dev/null; a=$$?; \
	  [ $$a -eq 45 ] || { echo "FAIL thumb1 soft doubles (got $$a, want 45 = every differential check vs gcc's base-ABI soft float; 100+n names the first miss -- see test/thumb2/harnessd.c; doubles ride gp pairs at every seam, f0/f1/f15 are frame cells inside a fn (soften6))"; exit 1; }; \
	  cp test/thumb2/harnessam.c $$d/; \
	  $(ho)/mooncc -t thumb1 -Icrew/moon/lib/math -Icrew/moon/include -c crew/moon/lib/math/am.c $$d/am.o || { echo "FAIL mooncc -t thumb1 -c am.c"; exit 1; }; \
	  arm-none-eabi-gcc -mcpu=cortex-m0 -mthumb -ffreestanding -O2 -c $$d/harnessam.c -o $$d/harnessam.o || { echo "FAIL gcc harnessam"; exit 1; }; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harnessam.o $$d/am.o $$lg -o $$d/t1am.elf || { echo "FAIL ld thumb1 am objects"; exit 1; }; \
	  timeout 60 qemu-system-arm -M microbit -semihosting -nographic -kernel $$d/t1am.elf </dev/null; a=$$?; \
	  [ $$a -eq 9 ] || { echo "FAIL thumb1 am.c (got $$a, want 9 = the seven transcendentals BIT-IDENTICAL to the host am floor through the shared __aeabi soft float, incl. the Payne-Hanek big-argument reduction)"; exit 1; }; \
	  cp test/thumb1/libf.c test/thumb1/harnessf.c $$d/; \
	  $(ho)/mooncc -t thumb1 -c $$d/libf.c $$d/libf.o || { echo "FAIL mooncc -t thumb1 -c libf"; exit 1; }; \
	  arm-none-eabi-gcc -mcpu=cortex-m0 -mthumb -ffreestanding -O2 -c $$d/harnessf.c -o $$d/harnessf.o || { echo "FAIL gcc harnessf"; exit 1; }; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harnessf.o $$d/libf.o $$lg -o $$d/t1f.elf || { echo "FAIL ld thumb1 float objects"; exit 1; }; \
	  timeout 30 qemu-system-arm -M microbit -semihosting -nographic -kernel $$d/t1f.elf </dev/null; a=$$?; \
	  [ $$a -eq 7 ] || { echo "FAIL thumb1 bare floats (got $$a, want 7 = every differential check vs gcc; 100+n names the first miss -- see test/thumb1/harnessf.c; a bare float is ONE WORD on v6-M (ai_flo_t IS float on a 32-bit love -- the widened-pair mismatch here kept the egg from hatching))"; exit 1; }; \
	  cp test/thumb1/libzn.c test/thumb1/harnesszn.c $$d/; \
	  $(ho)/mooncc -t thumb1 -c $$d/libzn.c $$d/libzn.o || { echo "FAIL mooncc -t thumb1 -c libzn"; exit 1; }; \
	  arm-none-eabi-gcc -mcpu=cortex-m0 -mthumb -ffreestanding -O2 -c $$d/harnesszn.c -o $$d/harnesszn.o || { echo "FAIL gcc harnesszn"; exit 1; }; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harnesszn.o $$d/libzn.o $$lg -o $$d/t1z.elf || { echo "FAIL ld thumb1 composite objects"; exit 1; }; \
	  timeout 30 qemu-system-arm -M microbit -semihosting -nographic -kernel $$d/t1z.elf </dev/null; a=$$?; \
	  [ $$a -eq 9 ] || { echo "FAIL thumb1 composites (got $$a, want 9 = every differential check vs gcc; 100+n names the first miss -- see test/thumb1/harnesszn.c; the MEMORY-return (sret) lane and the position-0 16B r0-r3 quad are the featured shapes)"; exit 1; }; \
	  echo "test_thumb1: mooncc -t thumb1 -c -> ELF32/EM_ARM (R_ARM_THM_CALL + soft divide + la/R_ARM_ABS32 + 32-bit struct layout + leax + AAPCS32 varargs + 64-bit pairs + soft doubles + am.c bit-exact + composites vs gcc), ld binds, runs on qemu Cortex-M0"
# test_thumb2 -- the thumb1 gate's ARMv7E-M twin, ON THE DEVICE CPU (qemu mps2-an500 is a
# Cortex-M7 -- the Teensy 4.1 / Playdate silicon). the featured lane is `la`, thumb2's
# MOVW/MOVT absolute pair (movw16/movt16 -> R_ARM_THM_MOVW_ABS_NC/MOVT_ABS): every binding
# shape rides once -- a global fn's address (own FUNC symbol, thumb bit in st_value), a
# STATIC fn's (the .text section symbol, thumb bit in the in-field addend -- a miss there
# faults the BLX), a string literal (.data section + offset baked into the imm16 scatter),
# and a global var. same qemu-stdin trap as thumb1: </dev/null or qemu hangs to timeout.
.PHONY: test_thumb2
test_thumb2: host out/host$(hsuf)/mooncc
	@echo THUMB2 $(ho)/thumb2
	@if ! command -v arm-none-eabi-gcc >/dev/null 2>&1 || ! command -v arm-none-eabi-ld >/dev/null 2>&1 || ! command -v qemu-system-arm >/dev/null 2>&1; then \
	   echo "test_thumb2: no arm-none-eabi toolchain / qemu-system-arm, skipped"; exit 0; fi; \
	  d=$(ho)/thumb2; mkdir -p $$d; rm -f $$d/*.o; \
	  { printf 'int acc = 40;\n'; \
	    printf 'int f1(void){ return 30; }\n'; \
	    printf 'int f2(void){ return 12; }\n'; \
	    printf 'static int sf(void){ return 5; }\n'; \
	    printf 'int callidx(int i){ int (*a[2])(void) = {f1,f2}; return i ? a[1]() : a[0](); }\n'; \
	    printf 'int callsf(void){ int (*p)(void) = sf; return p(); }\n'; \
	    printf 'char *msg(void){ return "AZ"; }\n'; \
	    printf 'int addacc(int x){ acc = acc + x; return acc; }\n'; } > $$d/lib.c; \
	  $(ho)/mooncc -t thumb2 -c $$d/lib.c  $$d/lib.o  || { echo "FAIL mooncc -t thumb2 -c lib"; exit 1; }; \
	  { echo 'int callidx(int); int callsf(void); char *msg(void); int addacc(int);'; \
	    echo 'int run(void){ return callidx(0) + callidx(1) + msg()[1] + addacc(3) + callsf(); }'; } > $$d/harness.c; \
	  arm-none-eabi-gcc -mcpu=cortex-m7 -mthumb -mfloat-abi=hard -mfpu=fpv5-d16 -ffreestanding -O2 -c $$d/harness.c -o $$d/harness.o || { echo "FAIL gcc harness"; exit 1; }; \
	  { echo '.syntax unified'; echo '.cpu cortex-m7'; echo '.fpu fpv5-d16'; echo '.thumb'; \
	    echo '.section .vectors,"a"'; echo '.word 0x20004000'; echo '.word _start+1'; \
	    echo '.text'; echo '.thumb_func'; echo '.global _start'; echo '_start:'; \
	    echo '  ldr r0, =0xE000ED88'; echo '  ldr r1, [r0]'; echo '  orr r1, r1, #0xF00000'; \
	    echo '  str r1, [r0]'; echo '  dsb'; echo '  isb'; \
	    echo '  bl run'; echo '  ldr r1, =0x20026'; echo '  push {r0}'; echo '  push {r1}'; \
	    echo '  mov r1, sp'; echo '  movs r0, #0x20'; echo '  bkpt 0xAB'; echo '  b .'; } > $$d/start.S; \
	  { echo 'MEMORY'; echo '{'; echo '  FLASH (rx) : ORIGIN = 0, LENGTH = 256K'; \
	    echo '  RAM  (rwx) : ORIGIN = 0x20000000, LENGTH = 16K'; echo '}'; \
	    echo 'SECTIONS'; echo '{'; echo '  .text : { KEEP(*(.vectors)) *(.text*) *(.rodata*) } > FLASH'; \
	    echo '  .data : { *(.data*) } > RAM'; echo '  .bss : { *(.bss*) } > RAM'; echo '}'; } > $$d/link.ld; \
	  arm-none-eabi-gcc -mcpu=cortex-m7 -mthumb -mfloat-abi=hard -mfpu=fpv5-d16 -c $$d/start.S -o $$d/start.o || { echo "FAIL as start.S"; exit 1; }; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harness.o $$d/lib.o -o $$d/t2.elf || { echo "FAIL ld thumb2 objects"; exit 1; }; \
	  timeout 30 qemu-system-arm -M mps2-an500 -semihosting -nographic -kernel $$d/t2.elf </dev/null; a=$$?; \
	  [ $$a -eq 180 ] || { echo "FAIL thumb2 -c link+run (got $$a, want 180 = fnptr 30+12 + 'Z' 90 + addacc 43 + static-fn 5; a missing thumb bit on the static fn faults the BLX, a bad section addend misreads the string)"; exit 1; }; \
	  cp test/thumb2/lib64.c test/thumb2/harness64.c $$d/; \
	  $(ho)/mooncc -t thumb2 -c $$d/lib64.c $$d/lib64.o || { echo "FAIL mooncc -t thumb2 -c lib64"; exit 1; }; \
	  arm-none-eabi-gcc -mcpu=cortex-m7 -mthumb -mfloat-abi=hard -mfpu=fpv5-d16 -ffreestanding -O2 -c $$d/harness64.c -o $$d/harness64.o || { echo "FAIL gcc harness64"; exit 1; }; \
	  lg=`arm-none-eabi-gcc -mcpu=cortex-m7 -mthumb -mfloat-abi=hard -mfpu=fpv5-d16 -print-libgcc-file-name`; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harness64.o $$d/lib64.o $$lg -o $$d/t2p.elf || { echo "FAIL ld thumb2 pair objects"; exit 1; }; \
	  timeout 30 qemu-system-arm -M mps2-an500 -semihosting -nographic -kernel $$d/t2p.elf </dev/null; a=$$?; \
	  [ $$a -eq 45 ] || { echo "FAIL thumb2 64-bit pairs (got $$a, want 45 = every differential check vs gcc; 100+n names the first miss -- see test/thumb2/harness64.c)"; exit 1; }; \
	  cp test/thumb2/libd.c test/thumb2/harnessd.c $$d/; \
	  $(ho)/mooncc -t thumb2 -c $$d/libd.c $$d/libd.o || { echo "FAIL mooncc -t thumb2 -c libd"; exit 1; }; \
	  arm-none-eabi-gcc -mcpu=cortex-m7 -mthumb -mfloat-abi=hard -mfpu=fpv5-d16 -ffreestanding -O2 -c $$d/harnessd.c -o $$d/harnessd.o || { echo "FAIL gcc harnessd"; exit 1; }; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harnessd.o $$d/libd.o $$lg -o $$d/t2d.elf || { echo "FAIL ld thumb2 double objects"; exit 1; }; \
	  timeout 30 qemu-system-arm -M mps2-an500 -semihosting -nographic -kernel $$d/t2d.elf </dev/null; a=$$?; \
	  [ $$a -eq 45 ] || { echo "FAIL thumb2 VFP doubles (got $$a, want 45 = every differential check vs gcc -mfloat-abi=hard; 100+n names the first miss -- see test/thumb2/harnessd.c)"; exit 1; }; \
	  cp test/thumb2/harnessam.c $$d/; \
	  $(ho)/mooncc -t thumb2 -Icrew/moon/lib/math -Icrew/moon/include -c crew/moon/lib/math/am.c $$d/am.o || { echo "FAIL mooncc -t thumb2 -c am.c"; exit 1; }; \
	  arm-none-eabi-gcc -mcpu=cortex-m7 -mthumb -mfloat-abi=hard -mfpu=fpv5-d16 -ffreestanding -O2 -c $$d/harnessam.c -o $$d/harnessam.o || { echo "FAIL gcc harnessam"; exit 1; }; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harnessam.o $$d/am.o $$lg -o $$d/t2am.elf || { echo "FAIL ld thumb2 am objects"; exit 1; }; \
	  timeout 60 qemu-system-arm -M mps2-an500 -semihosting -nographic -kernel $$d/t2am.elf </dev/null; a=$$?; \
	  [ $$a -eq 9 ] || { echo "FAIL thumb2 am.c (got $$a, want 9 = the seven transcendentals BIT-IDENTICAL to the host am floor, incl. the Payne-Hanek big-argument reduction)"; exit 1; }; \
	  cp test/thumb2/libz.c test/thumb2/harnessz.c $$d/; \
	  $(ho)/mooncc -t thumb2 -Icrew/moon/include -c $$d/libz.c $$d/libz.o || { echo "FAIL mooncc -t thumb2 -c libz"; exit 1; }; \
	  arm-none-eabi-gcc -mcpu=cortex-m7 -mthumb -mfloat-abi=hard -mfpu=fpv5-d16 -ffreestanding -O2 -c $$d/harnessz.c -o $$d/harnessz.o || { echo "FAIL gcc harnessz"; exit 1; }; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harnessz.o $$d/libz.o $$lg -o $$d/t2z.elf || { echo "FAIL ld thumb2 struct/vararg objects"; exit 1; }; \
	  timeout 30 qemu-system-arm -M mps2-an500 -semihosting -nographic -kernel $$d/t2z.elf </dev/null; a=$$?; \
	  [ $$a -eq 18 ] || { echo "FAIL thumb2 composites+varargs (got $$a, want 18 = HFA d-pairs + 8B blob + <=4B int one + the AAPCS32 word walk, gcc<->mooncc both directions; 100+n names the first miss -- see test/thumb2/harnessz.c)"; exit 1; }; \
	  echo "test_thumb2: mooncc -t thumb2 -c -> ELF32/EM_ARM (la + pairs + VFP + am.c bit-exact + composites/varargs: 45+45+9+18 differential checks), ld binds, runs on qemu Cortex-M7"
# test_mps2 -- LOVE ITSELF on the M7: the whole runtime (love.c + am + libc + the
# port glue) compiled end to end by mooncc -t thumb2 (port/mps2/, the qemu sim
# port), linked by arm-none-eabi-ld, booted on qemu's Cortex-M7. The boot bakes
# the egg FROM SOURCE on the emulated M7 -- the self-hosting double-bake under
# emulation -- then the driver tail asserts spec laws over the hatched image and
# exits 42 through semihosting. 98 = a fault (start.S names the stacked pc/lr).
# test_virt -- LOVE ITSELF on the bare riscv64 hart: the whole runtime
# (love.c + am + libc + the port glue) compiled end to end by mooncc
# -t riscv64 (port/virt/, the qemu sim port for the ox64 arc), start.o laid
# from holo IR, OUR linker binds -- no foreign toolchain ANYWHERE, the only
# port that can say so. The boot bakes the egg from source on the emulated
# hart, then the driver tail asserts spec laws over the hatched image and
# exits 42 through the sifive test finisher. 98 = a machine trap (the mtvec
# tail names mcause/mepc on the console). Skips without qemu-system-riscv64.
.PHONY: test_virt
test_virt: host out/host$(hsuf)/mooncc
	@echo VIRT out/virt/love.elf
	@if ! command -v qemu-system-riscv64 >/dev/null 2>&1; then \
	   echo "test_virt: no qemu-system-riscv64, skipped"; exit 0; fi; \
	  $(MAKE) -C port/virt || { echo "FAIL virt build"; exit 1; }; \
	  timeout 300 qemu-system-riscv64 -M virt -bios none -nographic -kernel out/virt/love.elf </dev/null; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL love-on-virt boot (got $$a, want 42 = the egg hatched + the driver laws held; 98 = trap, 1 = a law failed)"; exit 1; }; \
	  echo "test_virt: love (all-mooncc riscv64, our linker, holo start.o) boots on qemu -M virt -- egg baked on-hart, laws hold, exit 42"
.PHONY: test_mps2
test_mps2: host out/host$(hsuf)/mooncc
	@echo MPS2 out/mps2/love.elf
	@if ! command -v arm-none-eabi-gcc >/dev/null 2>&1 || ! command -v arm-none-eabi-ld >/dev/null 2>&1 || ! command -v qemu-system-arm >/dev/null 2>&1; then \
	   echo "test_mps2: no arm-none-eabi toolchain / qemu-system-arm, skipped"; exit 0; fi; \
	  $(MAKE) -C port/mps2 || { echo "FAIL mps2 build"; exit 1; }; \
	  timeout 300 qemu-system-arm -M mps2-an500 -semihosting -nographic -kernel out/mps2/love.elf </dev/null; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL love-on-M7 boot (got $$a, want 42 = the egg hatched + the driver laws held; 98 = fault, 1 = a law failed)"; exit 1; }; \
	  echo "test_mps2: love (all-mooncc thumb2) boots on qemu Cortex-M7 -- egg baked on-device, laws hold, exit 42"
# test_mps2_t1 -- LOVE ON THE RP2040'S ISA: the same port compiled end to end by
# mooncc -t thumb1 (ARMv6-M -- the Cortex-M0+/RP2040 instruction set, ai_tco=0's
# trampoline, soft floats through libgcc's v6-m __aeabi set). v6-M code is a
# strict subset of ARMv7E-M, so qemu's M7 executes it natively (no RP2040
# machine exists in qemu; the microbit legs prove the v6-M encoding floor).
# The egg bakes from source on the emulated core; exit 42 = hatched + laws held.
.PHONY: test_mps2_t1
test_mps2_t1: host out/host$(hsuf)/mooncc
	@echo MPS2T1 out/mps2/love-t1.elf
	@if ! command -v arm-none-eabi-gcc >/dev/null 2>&1 || ! command -v arm-none-eabi-ld >/dev/null 2>&1 || ! command -v qemu-system-arm >/dev/null 2>&1; then \
	   echo "test_mps2_t1: no arm-none-eabi toolchain / qemu-system-arm, skipped"; exit 0; fi; \
	  $(MAKE) -C port/mps2 mps2t1 || { echo "FAIL mps2t1 build"; exit 1; }; \
	  timeout 600 qemu-system-arm -M mps2-an500 -semihosting -nographic -kernel out/mps2/love-t1.elf </dev/null; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL love-on-v6M boot (got $$a, want 42 = the egg hatched + the driver laws held; 98 = fault, 1 = a law failed)"; exit 1; }; \
	  echo "test_mps2_t1: love (all-mooncc thumb1/ARMv6-M, the RP2040 ISA) boots -- egg baked on-device, laws hold, exit 42"
# test_mps2_wake -- the IMAGE lane: the baker bakes the corpus on qemu's M7 and
# dumps a fully-symbolic heap image (build-time bake); the WAKER -- a different
# binary, arena deliberately offset -- wakes it and re-runs the driver laws.
# The gate that finally wakes a mooncc image (and the teensy's build rides the
# same love.img).
.PHONY: test_mps2_wake
test_mps2_wake: host out/host$(hsuf)/mooncc
	@echo MPS2WAKE out/mps2/waker.elf
	@if ! command -v arm-none-eabi-gcc >/dev/null 2>&1 || ! command -v arm-none-eabi-ld >/dev/null 2>&1 || ! command -v qemu-system-arm >/dev/null 2>&1; then \
	   echo "test_mps2_wake: no arm-none-eabi toolchain / qemu-system-arm, skipped"; exit 0; fi; \
	  $(MAKE) -C port/mps2 img ../../out/mps2/waker.elf || { echo "FAIL mps2 waker build"; exit 1; }; \
	  test -s out/mps2/love.img || { echo "test_mps2_wake: empty image (no qemu at bake), skipped"; exit 0; }; \
	  timeout 300 qemu-system-arm -M mps2-an500 -semihosting -nographic -kernel out/mps2/waker.elf </dev/null; a=$$?; \
	  [ $$a -eq 42 ] || { echo "FAIL image wake (got $$a, want 42)"; exit 1; }; \
	  echo "test_mps2_wake: the qemu-baked image WAKES in a different binary -- laws hold, exit 42"
# test_thumb2sp -- the SP-only-FPU face (the playdate's STM32F746): f64
# arithmetic SOFTENS to __aeabi_* libgcc calls while the 64-bit transfers keep
# the d-reg value model. Gated on qemu's mps2-an386 -- a Cortex-M4 whose
# FPv4-SP FPU FAULTS on any f64 arithmetic that slipped through -- against
# gcc -mfpu=fpv4-sp-d16, which softens through the SAME libgcc helpers, so
# even am.c stays BIT-exact. Three legs: VFP doubles (45), am.c (9),
# composites+varargs (18).
.PHONY: test_thumb2sp
test_thumb2sp: host out/host$(hsuf)/mooncc
	@echo THUMB2SP $(ho)/thumb2sp
	@if ! command -v arm-none-eabi-gcc >/dev/null 2>&1 || ! command -v arm-none-eabi-ld >/dev/null 2>&1 || ! command -v qemu-system-arm >/dev/null 2>&1; then \
	   echo "test_thumb2sp: no arm-none-eabi toolchain / qemu-system-arm, skipped"; exit 0; fi; \
	  d=$(ho)/thumb2sp; mkdir -p $$d; rm -f $$d/*.o; \
	  { echo '.syntax unified'; echo '.cpu cortex-m4'; echo '.fpu fpv4-sp-d16'; echo '.thumb'; \
	    echo '.section .vectors,"a"'; echo '.word 0x20004000'; echo '.word _start+1'; \
	    echo '.text'; echo '.thumb_func'; echo '.global _start'; echo '_start:'; \
	    echo '  ldr r0, =0xE000ED88'; echo '  ldr r1, [r0]'; echo '  orr r1, r1, #0xF00000'; \
	    echo '  str r1, [r0]'; echo '  dsb'; echo '  isb'; \
	    echo '  bl run'; echo '  ldr r1, =0x20026'; echo '  push {r0}'; echo '  push {r1}'; \
	    echo '  mov r1, sp'; echo '  movs r0, #0x20'; echo '  bkpt 0xAB'; echo '  b .'; } > $$d/start.S; \
	  { echo 'MEMORY'; echo '{'; echo '  FLASH (rx) : ORIGIN = 0, LENGTH = 256K'; \
	    echo '  RAM  (rwx) : ORIGIN = 0x20000000, LENGTH = 16K'; echo '}'; \
	    echo 'SECTIONS'; echo '{'; echo '  .text : { KEEP(*(.vectors)) *(.text*) *(.rodata*) } > FLASH'; \
	    echo '  .data : { *(.data*) } > RAM'; echo '  .bss : { *(.bss*) } > RAM'; echo '}'; } > $$d/link.ld; \
	  arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -c $$d/start.S -o $$d/start.o || { echo "FAIL as start.S"; exit 1; }; \
	  lg=`arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -print-libgcc-file-name`; \
	  cp test/thumb2/libd.c test/thumb2/harnessd.c test/thumb2/libz.c test/thumb2/harnessz.c test/thumb2/harnessam.c $$d/; \
	  $(ho)/mooncc -t thumb2sp -c $$d/libd.c $$d/libd.o || { echo "FAIL mooncc -t thumb2sp -c libd"; exit 1; }; \
	  arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -ffreestanding -O2 -c $$d/harnessd.c -o $$d/harnessd.o || { echo "FAIL gcc harnessd"; exit 1; }; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harnessd.o $$d/libd.o $$lg -o $$d/tsd.elf || { echo "FAIL ld sp double objects"; exit 1; }; \
	  timeout 30 qemu-system-arm -M mps2-an386 -semihosting -nographic -kernel $$d/tsd.elf </dev/null; a=$$?; \
	  [ $$a -eq 45 ] || { echo "FAIL thumb2sp doubles (got $$a, want 45; 100+n names the first miss -- soft f64 vs gcc's __aeabi)"; exit 1; }; \
	  $(ho)/mooncc -t thumb2sp -Icrew/moon/lib/math -Icrew/moon/include -c crew/moon/lib/math/am.c $$d/am.o || { echo "FAIL mooncc -t thumb2sp -c am.c"; exit 1; }; \
	  arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -ffreestanding -O2 -c $$d/harnessam.c -o $$d/harnessam.o || { echo "FAIL gcc harnessam"; exit 1; }; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harnessam.o $$d/am.o $$lg -o $$d/tsam.elf || { echo "FAIL ld sp am objects"; exit 1; }; \
	  timeout 60 qemu-system-arm -M mps2-an386 -semihosting -nographic -kernel $$d/tsam.elf </dev/null; a=$$?; \
	  [ $$a -eq 9 ] || { echo "FAIL thumb2sp am.c (got $$a, want 9 = BIT-identical through the shared __aeabi helpers)"; exit 1; }; \
	  $(ho)/mooncc -t thumb2sp -Icrew/moon/include -c $$d/libz.c $$d/libz.o || { echo "FAIL mooncc -t thumb2sp -c libz"; exit 1; }; \
	  arm-none-eabi-gcc -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -ffreestanding -O2 -c $$d/harnessz.c -o $$d/harnessz.o || { echo "FAIL gcc harnessz"; exit 1; }; \
	  arm-none-eabi-ld -T $$d/link.ld $$d/start.o $$d/harnessz.o $$d/libz.o $$lg -o $$d/tsz.elf || { echo "FAIL ld sp struct/vararg objects"; exit 1; }; \
	  timeout 30 qemu-system-arm -M mps2-an386 -semihosting -nographic -kernel $$d/tsz.elf </dev/null; a=$$?; \
	  [ $$a -eq 18 ] || { echo "FAIL thumb2sp composites+varargs (got $$a, want 18)"; exit 1; }; \
	  echo "test_thumb2sp: mooncc -t thumb2sp (soft f64 over __aeabi) -> 45+9+18 differential checks vs gcc on qemu Cortex-M4"
# test_playdate -- the playdate build gate: the device half compiled by mooncc
# -t thumb2sp behind pdglue's word-only SDK seam, the whole pdx built by pdc.
# Verifies the DEVICE elf: fully resolved (no UND), eventHandler exported, and
# ZERO movw/movt relocations -- the loader relocates ABS32 words only, which
# is why la rides the literal pool on this target. Needs PLAYDATE_SDK_PATH
# (and arm-none-eabi-gcc); skips cleanly without, like moon-tar.
.PHONY: test_playdate
test_playdate: host out/host$(hsuf)/mooncc
	@echo PLAYDATE out/playdate/love.pdx
	@if [ -z "$$PLAYDATE_SDK_PATH" ] || ! command -v arm-none-eabi-gcc >/dev/null 2>&1; then \
	   echo "test_playdate: no PLAYDATE_SDK_PATH / arm-none-eabi toolchain, skipped"; exit 0; fi; \
	  $(MAKE) -C port/playdate || { echo "FAIL playdate build"; exit 1; }; \
	  u=`llvm-readelf -s out/playdate/pdex.elf | grep -c "UND [a-zA-Z_]"`; \
	  [ "$$u" -eq 0 ] || { echo "FAIL pdex.elf has $$u undefined symbols"; exit 1; }; \
	  llvm-readelf -s out/playdate/pdex.elf | grep -qw eventHandler || { echo "FAIL no eventHandler"; exit 1; }; \
	  m=`llvm-readelf -r out/playdate/pdex.elf | grep -c "MOVW\|MOVT"`; \
	  [ "$$m" -eq 0 ] || { echo "FAIL $$m movw/movt relocs (the loader can't relocate them)"; exit 1; }; \
	  echo "test_playdate: love.pdx (device half all-mooncc -t thumb2sp, soft f64) -- resolved, word-relocs only"
# test_teensy41 -- the REAL-METAL cousin's build gate: the whole teensy41 port
# (love.c + am + libc + the arch backend) compiled by mooncc -t thumb2, linked
# against the XIP flash map, and the ROM-facing boot image VERIFIED (FCFB tag
# at flash 0, IVT at 0x1000, thumb-bit entry -- the fields whose mislayout
# cost first silicon its boot). No RT1062 emulation exists, so the runtime
# itself is proven by test_mps2 (same CPU, same compiler, same runtime); the
# silicon flash stays a human step (make -C port/teensy41 flash).
.PHONY: test_teensy41
test_teensy41: host out/host$(hsuf)/mooncc
	@echo TEENSY41 out/teensy41/love.hex
	@if ! command -v arm-none-eabi-gcc >/dev/null 2>&1 || ! command -v arm-none-eabi-ld >/dev/null 2>&1; then \
	   echo "test_teensy41: no arm-none-eabi toolchain, skipped"; exit 0; fi; \
	  $(MAKE) -C port/teensy41 || { echo "FAIL teensy41 build (the boot-image verify is inside)"; exit 1; }; \
	  echo "test_teensy41: love (all-mooncc thumb2) links against the XIP flash map, boot image verified"
# test_nucleo446 -- the Nucleo-F446RE firmware BOOT gate: the whole port (arch
# backend + main + am) compiled by mooncc -t thumb2sp (the F446's Cortex-M4 has
# the single-precision FPv4 FPU -- the playdate's target), then the -D QSMOKE
# face BOOTS on qemu's STM32F405 cousin (netduinoplus2: same USART2/RCC map),
# runs the on-board self-check battery over the emulated USART2, and leaves
# through semihosting with the tally. This exercises the port's OWN vectors/
# crt0/clock-fallback/USART2 driver, not just the ISA (that floor is
# test_thumb2sp); 100+n names the first miss, 98 a fault. The silicon flash
# stays a human step (make -C port/nucleo446 flash).
.PHONY: test_nucleo446
test_nucleo446: host out/host$(hsuf)/mooncc
	@echo NUCLEO446 out/nucleo446/smoke.elf
	@if ! command -v arm-none-eabi-gcc >/dev/null 2>&1 || ! command -v arm-none-eabi-ld >/dev/null 2>&1 || ! command -v qemu-system-arm >/dev/null 2>&1; then \
	   echo "test_nucleo446: no arm-none-eabi toolchain / qemu-system-arm, skipped"; exit 0; fi; \
	  $(MAKE) -C port/nucleo446 || { echo "FAIL nucleo446 build (the boot-image verify is inside)"; exit 1; }; \
	  $(MAKE) -C port/nucleo446 smoke || { echo "FAIL nucleo446 smoke build"; exit 1; }; \
	  timeout 60 qemu-system-arm -M netduinoplus2 -semihosting -nographic -monitor none \
	    -serial null -serial stdio -kernel out/nucleo446/smoke.elf </dev/null; a=$$?; \
	  [ $$a -eq 28 ] || { echo "FAIL nucleo446 smoke (got $$a, want 28; 100+n first miss, 98 fault)"; exit 1; }; \
	  echo "test_nucleo446: mooncc -t thumb2sp firmware boots the F4 (28 on-board checks green on qemu)"
# moon-tar -- the userland cousin of test_raw: build GNU tar 1.13 (a real third-
# party GNU package) with mooncc + nolibc + the holo linker, no gcc/glibc/ld, and
# prove the binary RUNS -- cf/xf + czf/xzf roundtrips byte-identical + system-tar
# interop. The third moon-userland rung (doc/moon-userland.md). Opt-in (not in
# test_all): tar's source is imported -- point TARSRC at a ./configure'd tar-1.13
# tree; SKIPS cleanly without one, like test_raw_arm64 without qemu.
.PHONY: moon-tar
moon-tar: host out/host$(hsuf)/mooncc
	@TARSRC="$(TARSRC)" ./tools/moon-tar.sh
# moon-m4 -- the fourth moon-userland rung: GNU m4 1.4 (macro processor, so it
# exercises tmpfile/rewind diversions, popen'd esyscmd, float format), built by
# mooncc + nolibc + holo and gated on m4's OWN 57-check suite. Opt-in like
# moon-tar: point M4SRC at a ./configure'd m4-1.4 tree; SKIPS cleanly without.
.PHONY: moon-m4
moon-m4: host out/host$(hsuf)/mooncc
	@M4SRC="$(M4SRC)" ./tools/moon-m4.sh
# moon-lua: point LUASRC at an extracted lua-5.4.x tree (no configure needed);
# SKIPS cleanly without. Builds + runs the interpreter battery.
.PHONY: moon-lua
moon-lua: host out/host$(hsuf)/mooncc
	@LUASRC="$(LUASRC)" ./tools/moon-lua.sh
# moon-sqlite: point SQLSRC at an extracted sqlite-amalgamation dir; SKIPS
# cleanly without. Compiles the whole amalgamation + runs the VFS battery.
.PHONY: moon-sqlite
moon-sqlite: host out/host$(hsuf)/mooncc
	@SQLSRC="$(SQLSRC)" ./tools/moon-sqlite.sh
# The neutral assembler (crew/holo/) + its x86-64 backend: every encoder golden is
# objdump-checked (crew/holo/holotest.l). A host-only app (like sat) -- it rides the
# core's lists/tablets, adds no nif, and is NOT baked into love0. The gate greps
# the "N passed, 0 failed" sentinel AND exit 0 (a silent reader-stop exits 0).
.PHONY: test_holo
test_holo: host
	@echo "HOLO crew/holo/holotest.l"; \
	  cat crew/holo/holo.l crew/holo/x64.l crew/holo/arm64.l crew/holo/thumb2.l crew/holo/riscv.l crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l crew/holo/holotest.l | $m > out/host/.test_holo.out 2>&1; r=$$?; \
	  cat out/host/.test_holo.out; \
	  { [ $$r -eq 0 ] && grep -q ", 0 failed" out/host/.test_holo.out; } \
	    || { echo "FAIL holo (exit $$r)"; exit 1; }
# as.l -- the real AT&T x86-64 front over holo. astest.l's goldens are byte-identical to
# /usr/bin/as (frozen, no shell-out at gate time). Same sentinel gate as test_holo.
.PHONY: test_as
test_as: host
	@echo "AS crew/holo/astest.l"; \
	  cat crew/holo/holo.l crew/holo/x64.l crew/holo/as.l crew/holo/astest.l | $m > out/host/.test_as.out 2>&1; r=$$?; \
	  cat out/host/.test_as.out; \
	  { [ $$r -eq 0 ] && grep -q ", 0 failed" out/host/.test_as.out; } \
	    || { echo "FAIL as (exit $$r)"; exit 1; }
# ain's two-process loopback gate: a server and a client over real TCP on
# 127.0.0.1, full-duplex, asserting each side received what the other sent (the
# socket nifs in host/net.c + the pump loops in tools/ain.l). In `test_all`
# (the thorough gate) but NOT the fast `test` -- it needs two live processes and
# a free loopback port. It is the ONLY net gate that drives the real
# `love tools/ain.l` cli path: the in-process `test/host/net.l` smoke (in
# test_hostnif) pipes straight into the binary, so it covers the nifs portably
# but can't catch an invocation regression (e.g. a stale -l preload). Override
# the port with `make nettest PORT=NNNN`.
PORT ?= 7390
nettest: host
	@echo NETTEST $m "(127.0.0.1:$(PORT))"
	@sh $R/test/net/loopback.sh $m $(PORT)
# Validate the l tool rewrites against their frozen Python references in
# tools/py/ (gen_data / vmret). See tools/Makefile + tools/py/README.md.
test_tools: host
	@$(MAKE) -C tools
# Machine-check proof/rocq/spec.v -- love's headline laws (the numeral / function /
# absence core of test/spec.l) as Rocq theorems, axiom-free (every proof
# "Closed under the global context"). This is what upgrades the executable
# spec from DEMONSTRATED on every target to PROVED in a consistent metatheory
# -- the README's "verified specification ... up to explosion of world" made
# load-bearing (the caveat is uu.l's type-in-type detonation; this file runs
# universe-checked, so it does not explode). coqc writes artifacts next to the
# source; clean them on success. No-op when coqc is missing, so the gate stays
# green without a Rocq install (like test_kernel / test_wasm).
COQC ?= $(shell command -v coqc 2>/dev/null)
LEAN ?= $(shell command -v lean 2>/dev/null)
ifeq ($(COQC),)
test_proof:
	@echo "test_proof: skipped (needs rocq/coqc)"
else
test_proof:
	@echo TEST proof/rocq/spec.v "(coqc)"
	@$(COQC) -q proof/rocq/spec.v
	@rm -f proof/rocq/spec.vo proof/rocq/spec.vok proof/rocq/spec.vos proof/rocq/spec.glob proof/rocq/.spec.aux
	@echo TEST proof/rocq/patch.v "(coqc)"
	@$(COQC) -q proof/rocq/patch.v
	@rm -f proof/rocq/patch.vo proof/rocq/patch.vok proof/rocq/patch.vos proof/rocq/patch.glob proof/rocq/.patch.aux
endif
# Machine-check proof/rocq/gc.v -- the generational MINOR is SOUND: under a complete
# write barrier (rem_complete) the nursery scan reaches every live young object,
# so no live young is lost (barrier_sound) -- the Coq proof of doc/proto/gengc.l's
# load-bearing self-check (3b, the barrier is necessary). And the minor's PAUSE
# has its shape: work bounded by the nursery alone, survivor set identical under
# tenure-blind growth (minor_work_bounded / minor_flat -- test/host/gcpause.l is
# the gauge that instance-checks them). And the COPY LOOP has its shape: the
# Cheney drain terminates, copies each reachable object exactly once and only
# the reachable ones, and is a true fixpoint (drain_* -- test_gcheck is the
# build that instance-checks the fixpoint on every minor). Axiom-free like
# spec.v; the C stays connected by the differential oracle + gen_audit. No-op
# without coqc.
ifeq ($(COQC),)
test_gc:
	@echo "test_gc: skipped (needs rocq/coqc)"
else
test_gc:
	@echo TEST proof/rocq/gc.v "(coqc)"
	@$(COQC) -q proof/rocq/gc.v
	@rm -f proof/rocq/gc.vo proof/rocq/gc.vok proof/rocq/gc.vos proof/rocq/gc.glob proof/rocq/.gc.aux
endif
# The .l -> .v pipeline: tools/spec2coq.l (run on the host binary $m) reads
# test/spec.l and EMITS proof/rocq/gen.v -- the spec generating Coq theorems for its
# own pure-numeral corpus facts, each closed by computation (over Z, since nat
# is unary and 3^27 would blow up vm_compute). coqc then checks them. Drift-proof:
# the asserts and their proofs cannot diverge -- regenerated every run from .l.
# Needs the host binary AND coqc; no-ops without coqc, like test_proof.
ifeq ($(COQC),)
test_gen:
	@echo "test_gen: skipped (needs rocq/coqc)"
else
test_gen: host
	@echo AI	proof/rocq/gen.v "(tools/spec2coq.l on $m)"
	@$m tools/spec2coq.l > proof/rocq/gen.v
	@echo TEST proof/rocq/gen.v "(coqc, against spec.v's shared model)"
	@cd proof/rocq && $(COQC) -R . "" spec.v >/dev/null && $(COQC) -R . "" gen.v
	@rm -f proof/rocq/spec.vo proof/rocq/spec.vok proof/rocq/spec.vos proof/rocq/spec.glob proof/rocq/.spec.aux \
	  proof/rocq/gen.vo proof/rocq/gen.vok proof/rocq/gen.vos proof/rocq/gen.glob proof/rocq/.gen.aux
endif
# The PROOF half of the .l -> .v pipeline (cf. test_gen, which exports concrete
# ASSERTS): tools/uu2coq.l loads uu's kernel (test/uu.l), has it TYPE-CHECK a proof
# term against its theorem, and EMITS proof/rocq/uugen.v -- the same term in Gallina, which
# coqc re-checks independently. So a LAW (forall x, x^0 = 1 -- spec.v's const_one) is
# proved in love's own kernel and certified by Rocq, axiom-free. Drift-proof like gen.v:
# regenerated every run, so the internal proof and the exported one cannot diverge.
# The skeleton of the internal-prover bridge. Needs the host binary AND coqc.
ifeq ($(COQC),)
test_uugen:
	@echo "test_uugen: skipped (needs rocq/coqc)"
else
test_uugen: host
	@echo AI	proof/rocq/uugen.v "(tools/uu2coq.l on $m)"
	@$m tools/uu2coq.l > proof/rocq/uugen.v
	@echo TEST proof/rocq/uugen.v "(coqc)"
	@$(COQC) -q proof/rocq/uugen.v
	@rm -f proof/rocq/uugen.vo proof/rocq/uugen.vok proof/rocq/uugen.vos proof/rocq/uugen.glob proof/rocq/.uugen.aux
endif

# The LEAN leg of the proof bridge (cf. test_uugen, the Rocq leg): tools/uu2lean.l emits
# the SAME uu corpus to Lean 4, which re-checks it -- a SECOND independent kernel, so each
# law is agreed by two unrelated implementations (the de Bruijn criterion, diversified).
# Drift-proof: regenerated every run. Needs the host binary AND lean.
ifeq ($(LEAN),)
test_uulean:
	@echo "test_uulean: skipped (needs lean)"
else
test_uulean: host
	@mkdir -p lean
	@echo AI	proof/lean/uugen.lean "(tools/uu2lean.l on $m)"
	@$m tools/uu2lean.l > proof/lean/uugen.lean
	@echo TEST proof/lean/uugen.lean "(lean)"
	@$(LEAN) proof/lean/uugen.lean > out/host/.uulean.out 2>&1; r=$$?; \
	  if [ $$r -ne 0 ] || grep -q sorryAx out/host/.uulean.out; then cat out/host/.uulean.out; exit 1; fi
endif

# test_extract: the differential oracle with a ROCQ-EXTRACTED reference. coqc
# extracts proof/rocq/extract.v (the n-ary/CBV/weak/saturating normalizer built on
# spec.v's PROVEN subst/shift) to OCaml; proof/rocq/oracle_drive.ml generates random
# closed affine terms, normalizes each with the extracted `nf`, and emits a love
# program that checks ev EXTENSIONALLY agrees. So the reference the fuzzer runs
# IS the proven definitions (up to the standard nat->int mapping) -- machine-
# checked end to end. The hand-transcribed twin (test/oracle.l) stays in the
# fast `make test`; this heavier, higher-assurance variant needs coqc + ocamlopt,
# so it lives in test_all and no-ops when either tool is absent (like test_proof).
OCAMLOPT ?= $(shell command -v ocamlopt 2>/dev/null)
ifeq ($(and $(COQC),$(OCAMLOPT)),)
test_extract:
	@echo "test_extract: skipped (needs coqc + ocamlopt)"
else
test_extract: host
	@echo TEST proof/rocq/extract.v "(coqc extraction -> ocaml ref vs ev)"
	@cd proof/rocq && $(COQC) -R . "" spec.v >/dev/null && $(COQC) -R . "" extract.v >/dev/null \
	  && rm -f normalizer.mli && $(OCAMLOPT) -w -a normalizer.ml oracle_drive.ml -o oracle_drive
	@proof/rocq/oracle_drive 2000 6 1 > out/.extract_oracle.l
	@$m out/.extract_oracle.l | grep -q "2000 / 2000 PASS" \
	  || { echo "EXTRACT ORACLE FAILED:"; $m out/.extract_oracle.l; exit 1; }
	@$m out/.extract_oracle.l
	@rm -f proof/rocq/spec.vo proof/rocq/spec.vok proof/rocq/spec.vos proof/rocq/spec.glob proof/rocq/.spec.aux \
	  proof/rocq/extract.vo proof/rocq/extract.vok proof/rocq/extract.vos proof/rocq/extract.glob proof/rocq/.extract.aux \
	  proof/rocq/normalizer.ml proof/rocq/normalizer.mli proof/rocq/oracle_drive proof/rocq/*.cmi proof/rocq/*.cmx proof/rocq/*.o \
	  out/.extract_oracle.l
endif
# test_big: the BIGNUM lane vs a Rocq-extracted reference. coqc proves big.v's
# decimal codec roundtrip (parse_print -- the process-boundary seam) plus the
# quot-rem/gcd law witnesses, and extracts stdlib's binary Z (an independent
# rep of love's native limbs / Karatsuba / Knuth-D) with the codec to OCaml.
# big_drive.ml generates operand pairs -- the charm/sun/word rep edges, then
# random digit strings up to ~900 digits -- computes each op with the extracted
# reference, and emits a love program of decimal string comparisons: love's
# READER, limb arithmetic, and PRINTER all against the proven codec. Found
# abs-of-INTPTR_MIN wrapping on its first run. Needs coqc + ocamlopt; no-ops
# without either, like test_extract.
ifeq ($(and $(COQC),$(OCAMLOPT)),)
test_big:
	@echo "test_big: skipped (needs coqc + ocamlopt)"
else
test_big: host
	@echo TEST proof/rocq/big.v "(coqc codec proof + extracted Z ref vs the limb lane)"
	@cd proof/rocq && $(COQC) -q big.v >/dev/null \
	  && rm -f bigref.mli && $(OCAMLOPT) -w -a bigref.ml big_drive.ml -o big_drive
	@proof/rocq/big_drive 2000 1 > out/.big_oracle.l
	@$m out/.big_oracle.l | grep -q "2000 / 2000 PASS" \
	  || { echo "BIG ORACLE FAILED:"; $m out/.big_oracle.l; exit 1; }
	@$m out/.big_oracle.l
	@rm -f proof/rocq/big.vo proof/rocq/big.vok proof/rocq/big.vos proof/rocq/big.glob proof/rocq/.big.aux \
	  proof/rocq/bigref.ml proof/rocq/bigref.mli proof/rocq/big_drive proof/rocq/*.cmi proof/rocq/*.cmx proof/rocq/*.o \
	  out/.big_oracle.l
endif
# test_mx: the +/* dispatch matrices as DATA, their shape machine-checked.
# tools/mxdump.c (a TU including love.c whole -- the tables are static by
# design, so the dump reads them out of the same compilation) prints kind and
# lane names; tools/mx2coq.l DERIVES the band partition from row+column
# equality and generates proof/rocq/mx.v: the 256-cell tables factor through
# the band quotient with nothing left over, dispatch commutes over the WHOLE
# square (KMint is its own band, the unit lane -- the dispatchers' mint
# early-out is the fast path, never load-bearing), and the diagonal reads
# the lattice. Regenerated every run, so the tables cannot drift from the
# theorems. Needs coqc (the dump itself needs only $(CC)); no-ops without.
# test_gcheck: the copy loop's FIXPOINT instance check. AI_GC_CHECK makes
# gen_minor re-drive its WHOLE scan after the drain -- roots, rem set, the
# promoted window -- and trap if the second pass copies a single word: gc.v's
# drain_second_pass_copies_nothing, instanced on every minor the corpus fires.
# A trap means the first pass LOST an object the mutator can still reach (a
# scan-window or walker bug), caught at the collection that lost it instead of
# corrupting silently. The check build lives in its own tree (out/host/gck),
# so the fast binary rides clean; needs only $(CC), never skips. The `host`
# prerequisite keeps the SHARED lanes (love0, the lcat headers) canonical --
# fresh before the sub-make, so the flag never leaks into them.
test_gcheck: host
	@$(MAKE) --no-print-directory hsuf=/gck EXTRA_CFLAGS=-DAI_GC_CHECK test_host
ifeq ($(COQC),)
test_mx:
	@echo "test_mx: skipped (needs coqc)"
else
test_mx: host
	@echo TEST proof/rocq/mx.v "(the dispatch matrices: band factorization + dispatch commutativity, coqc)"
	@$(CC) $(ai_cflags) -o out/.mxdump tools/mxdump.c $R/crew/moon/lib/math/am.c
	@out/.mxdump > out/.mx.l
	@$m tools/mx2coq.l > proof/rocq/mx.v
	@cd proof/rocq && $(COQC) -q mx.v >/dev/null
	@rm -f out/.mxdump out/.mx.l proof/rocq/mx.vo proof/rocq/mx.vok proof/rocq/mx.vos proof/rocq/mx.glob proof/rocq/.mx.aux
endif
# the PROVE rung of the holo encoder ladder: machine-checked reference x86-64
# encoders, each proving decode inverts encode (axiom-free, vm_compute over the
# finite domain), extracted to OCaml, then differentially checked BYTE-IDENTICAL
# against holo's holo-hex. So holo is validated against a machine-checked oracle,
# not a trusted disassembler (that is the fuzz rung, test_holofuzz).
#   enc.v    -- register-direct core (mov + reg-reg ALU), 16x16 matrix x 7 ops (1792)
#   encmem.v -- base+disp load/store: ModRM+SIB, the rsp-SIB / rbp-forced-disp
#               quirks, disp sizing, REX.R/B; 16x16 x offsets x {ld,st} (6144)
#   encli.v  -- immediate load `li`: the 3-way form choice (b8 imm32 / C7 imm32 /
#               movabs imm64) by value range; 16 regs x immediates (320)
# Needs coqc + ocamlopt; no-ops without either, like test_extract.
ifeq ($(and $(COQC),$(OCAMLOPT)),)
test_encver:
	@echo "test_encver: skipped (needs coqc + ocamlopt)"
else
test_encver: host
	@echo TEST proof/rocq/enc.v proof/rocq/encmem.v proof/rocq/encli.v "(coqc round-trip proofs -> ocaml refs vs holo, byte-exact)"
	@cd proof/rocq && $(COQC) -q enc.v >/dev/null && $(COQC) -q encmem.v >/dev/null && $(COQC) -q encli.v >/dev/null \
	  && rm -f enc_ref.mli encmem_ref.mli encli_ref.mli \
	  && $(OCAMLOPT) -w -a enc_ref.ml enc_drive.ml -o enc_drive >/dev/null \
	  && $(OCAMLOPT) -w -a encmem_ref.ml encmem_drive.ml -o encmem_drive >/dev/null \
	  && $(OCAMLOPT) -w -a encli_ref.ml encli_drive.ml -o encli_drive >/dev/null
	@proof/rocq/enc_drive > out/.enc_oracle.l
	@proof/rocq/encmem_drive > out/.encmem_oracle.l
	@proof/rocq/encli_drive > out/.encli_oracle.l
	@cat crew/holo/holo.l crew/holo/x64.l out/.enc_oracle.l | $m | grep -q "1792 / 1792 PASS" \
	  || { echo "ENC (reg-direct) ORACLE FAILED:"; cat crew/holo/holo.l crew/holo/x64.l out/.enc_oracle.l | $m; exit 1; }
	@cat crew/holo/holo.l crew/holo/x64.l out/.encmem_oracle.l | $m | grep -q "6144 / 6144 PASS" \
	  || { echo "ENCMEM (memory) ORACLE FAILED:"; cat crew/holo/holo.l crew/holo/x64.l out/.encmem_oracle.l | $m; exit 1; }
	@cat crew/holo/holo.l crew/holo/x64.l out/.encli_oracle.l | $m | grep -q "320 / 320 PASS" \
	  || { echo "ENCLI (immediate) ORACLE FAILED:"; cat crew/holo/holo.l crew/holo/x64.l out/.encli_oracle.l | $m; exit 1; }
	@cat crew/holo/holo.l crew/holo/x64.l out/.enc_oracle.l | $m
	@cat crew/holo/holo.l crew/holo/x64.l out/.encmem_oracle.l | $m
	@cat crew/holo/holo.l crew/holo/x64.l out/.encli_oracle.l | $m
	@rm -f proof/rocq/enc.vo proof/rocq/enc.vok proof/rocq/enc.vos proof/rocq/enc.glob proof/rocq/.enc.aux \
	  proof/rocq/encmem.vo proof/rocq/encmem.vok proof/rocq/encmem.vos proof/rocq/encmem.glob proof/rocq/.encmem.aux \
	  proof/rocq/encli.vo proof/rocq/encli.vok proof/rocq/encli.vos proof/rocq/encli.glob proof/rocq/.encli.aux \
	  proof/rocq/enc_ref.ml proof/rocq/enc_ref.mli proof/rocq/enc_drive \
	  proof/rocq/encmem_ref.ml proof/rocq/encmem_ref.mli proof/rocq/encmem_drive \
	  proof/rocq/encli_ref.ml proof/rocq/encli_ref.mli proof/rocq/encli_drive \
	  proof/rocq/*.cmi proof/rocq/*.cmx proof/rocq/*.o \
	  out/.enc_oracle.l out/.encmem_oracle.l out/.encli_oracle.l
endif
# the fuzz-first rung of the holo encoder verification ladder (crew/holo/fuzz/):
# generate random IR forms, encode via holo, disassemble the bytes, and check the
# decode matches intent -- the goldens' hand round-trip, automated over many forms.
# x64 disassembles with objdump; arm64 with llvm-mc (host objdump lacks aarch64).
# Needs python3; each arch runs only if its disassembler is present, no-op like
# test_extract. Fixed seed, small n so it stays a few seconds in test_all; run
# bigger campaigns by hand (see crew/holo/fuzz/README.md).
PYTHON3 ?= $(shell command -v python3 2>/dev/null)
ifeq ($(PYTHON3),)
test_holofuzz:
	@echo "test_holofuzz: skipped (needs python3)"
else
test_holofuzz: host
	@echo TEST crew/holo/fuzz/fuzz.py "(holo x64+arm64+riscv encoder differential fuzz)"
	@if command -v objdump >/dev/null 2>&1; then \
	   $(PYTHON3) crew/holo/fuzz/fuzz.py --arch x64 -n 8 --seed 20250717 --no-llvm \
	     || { echo "FAIL holofuzz x64 -- a holo encoding disagrees with objdump"; exit 1; }; \
	 else echo "  (x64 skipped: no objdump)"; fi
	@if command -v llvm-mc >/dev/null 2>&1; then \
	   $(PYTHON3) crew/holo/fuzz/fuzz.py --arch arm64 -n 8 --seed 20250717 \
	     || { echo "FAIL holofuzz arm64 -- a holo encoding disagrees with llvm-mc"; exit 1; }; \
	 else echo "  (arm64 skipped: no llvm-mc)"; fi
	@if command -v llvm-mc >/dev/null 2>&1; then \
	   $(PYTHON3) crew/holo/fuzz/fuzz.py --arch riscv -n 8 --seed 20250717 \
	     || { echo "FAIL holofuzz riscv -- a holo encoding disagrees with llvm-mc"; exit 1; }; \
	 else echo "  (riscv skipped: no llvm-mc)"; fi
endif
# uu's NbE kernel lives at love/uu.l (mark + kernel + the sweep into the `uu`
# book at its tail) and bakes post.l-style through the lib_h/%0.h pattern
# rules -- into the host, love0, the inle kernel and wasm, so the corpus's uu
# files (test/uu*.l, binding the book surface at test/uu.l's head) run on
# every target, and an overlay can reach (uu 'vof) in a bare binary.
# test/uuwm.l is a COMMITTED GENERATED artifact: lux's zipper ops compiled
# from crew/lux/core.l into uu terms (tools/uuwmgen.l over tools/wm2uu.l, kind-
# directed by crew/lux/sigs.l), so test/uuwmlaw.l proves its theorems OF THE
# IMPLEMENTATION at corpus time. `make uuwm` refreshes it after a core.l edit;
# test_uuwm (in test_all) regenerates and diffs, failing loudly on drift.
uuwm: host
	@echo AI	test/uuwm.l "(tools/uuwmgen.l on $m)"
	@$m tools/uuwmgen.l > test/uuwm.l
test_uuwm: host
	@echo TEST test/uuwm.l "(regenerate + diff)"
	@$m tools/uuwmgen.l > out/host/.uuwm.l.tmp
	@cmp -s out/host/.uuwm.l.tmp test/uuwm.l \
	  || { echo "FAIL: test/uuwm.l is stale (crew/lux/core.l moved?) -- run: make uuwm"; exit 1; }
	@rm -f out/host/.uuwm.l.tmp
# test/uukind.l is a COMMITTED GENERATED artifact: doc/proto/kinds.l's abstract
# kinds-lattice JOIN compiled into uu terms (tools/kinds2uu.l), so test/uukindlaw.l
# proves the semilattice laws OF THE ANALYSIS at corpus time. `make uukind` refreshes
# it after a kinds.l edit; test_uukind (in test_all) regenerates and diffs.
uukind: host
	@echo AI	test/uukind.l "(tools/kinds2uu.l on $m)"
	@$m tools/kinds2uu.l > test/uukind.l
test_uukind: host
	@echo TEST test/uukind.l "(regenerate + diff)"
	@$m tools/kinds2uu.l > out/host/.uukind.l.tmp
	@cmp -s out/host/.uukind.l.tmp test/uukind.l \
	  || { echo "FAIL: test/uukind.l is stale (doc/proto/kinds.l moved?) -- run: make uukind"; exit 1; }
	@rm -f out/host/.uukind.l.tmp
# test_wake: the WOKEN-IMAGE lane -- the one lane no other gate runs (LOVE_NO_IMAGE
# is exported for every recipe above, so every gate exercises the fresh egg; only
# a user's direct run wakes the image). Bakes a CANDIDATE COPY (love.wake -- the
# canonical binary untouched, ETXTBSY-proof) and runs test/uu.l through the woken
# image under a budget the wake storm cannot meet (fresh lane ~1s, the storm was
# >90s -- doc/wake-storm.md). GREEN since the dump's dead-native revert landed
# (img_nif_interp: references re-aim at the bytecode twin); in test_all.
test_wake: $(ho)/love
	@echo TEST wake "(the woken-image lane, doc/wake-storm.md)"
	@cp $(ho)/love $(ho)/love.wake && $(ho)/love.wake --bake
	@cat test/00-init.l test/uu.l > $(ho)/wake-corpus.l
	@if env -u LOVE_NO_IMAGE timeout 60 $(ho)/love.wake $(ho)/wake-corpus.l > /dev/null 2>&1; \
	  then echo "test_wake: green (the woken image checks uu at speed)"; rm -f $(ho)/love.wake $(ho)/wake-corpus.l; \
	  else echo "test_wake: FAILED -- the wake storm (doc/wake-storm.md)"; rm -f $(ho)/love.wake $(ho)/wake-corpus.l; exit 1; fi
