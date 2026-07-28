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
	@sh test/gate/kore.sh $(ho) $m
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
	@sh test/gate/moon.sh $(ho) $m
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
	@sh test/gate/raw.sh x64 $(ho) $m $t
# test_raw_bake -- the mooncc-PIE binary bakes its own image and wakes it. The
# procedure lives in test/gate/raw-bake.sh (and the why with it); make keeps the
# dependency and the file list, whose $(filter-out) drops glaze.l.
# Opt-in (not test_all): needs the -pie toolchain. x86-64 only.
.PHONY: test_raw_bake
test_raw_bake: test_raw
	@sh test/gate/raw-bake.sh $(ho) $(filter-out %/glaze.l,$t)
# test_riscv -- the riscv64 codegen rung end to end: the whole test/cc battery
# compiled `mooncc -t riscv64` (EM_RISCV static ELF, the holo riscv backend),
# run under qemu-riscv64 (user mode), and DIFFERENTIAL against the native x64
# mooncc build of the same file -- mooncc is its own reference (the frontend is
# shared, so only codegen can diverge). the three x64-only features
# (100-complex / 101-vla / 102-bigstruct) are excluded exactly as arm64
# refuses them. skips clean without qemu-riscv64 or off x86_64.
.PHONY: test_riscv
test_riscv: host out/host$(hsuf)/mooncc out/host$(hsuf)/mooncc.image
	@sh test/gate/riscv.sh $(ho) $m
# test_raw's riscv64 twin: mooncc -t riscv64 lays every object (the holo riscv
# backend + the .o/link reloc path), mksys-riscv the syscall leaf (same
# asm-generic table as arm64), OUR linker binds, qemu-riscv64 (user) runs the
# whole corpus over the fresh egg. The riscv backend loads into the sealed holo
# module at runtime for the mksys step (the host bake carries only the native
# backend); mooncc.image carries all backends already. Opt-in (not in
# test_all): the qemu corpus costs a minute. Skips without qemu-riscv64.
.PHONY: test_raw_riscv
test_raw_riscv: host out/host$(hsuf)/mooncc out/lib/riscv.h
	@sh test/gate/raw.sh riscv64 $(ho) $m $t
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
	@sh test/gate/raw.sh arm64 $(ho) $m $t
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
	@sh test/gate/thumb.sh thumb1 $(ho)
# test_thumb2 -- the thumb1 gate's ARMv7E-M twin, ON THE DEVICE CPU (qemu mps2-an500 is a
# Cortex-M7 -- the Teensy 4.1 / Playdate silicon). the featured lane is `la`, thumb2's
# MOVW/MOVT absolute pair (movw16/movt16 -> R_ARM_THM_MOVW_ABS_NC/MOVT_ABS): every binding
# shape rides once -- a global fn's address (own FUNC symbol, thumb bit in st_value), a
# STATIC fn's (the .text section symbol, thumb bit in the in-field addend -- a miss there
# faults the BLX), a string literal (.data section + offset baked into the imm16 scatter),
# and a global var. same qemu-stdin trap as thumb1: </dev/null or qemu hangs to timeout.
.PHONY: test_thumb2
test_thumb2: host out/host$(hsuf)/mooncc
	@sh test/gate/thumb.sh thumb2 $(ho)
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
	@sh test/gate/boot.sh virt "$(MAKE)"
.PHONY: test_mps2
test_mps2: host out/host$(hsuf)/mooncc
	@sh test/gate/boot.sh mps2 "$(MAKE)"
# test_mps2_t1 -- LOVE ON THE RP2040'S ISA: the same port compiled end to end by
# mooncc -t thumb1 (ARMv6-M -- the Cortex-M0+/RP2040 instruction set, ai_tco=0's
# trampoline, soft floats through libgcc's v6-m __aeabi set). v6-M code is a
# strict subset of ARMv7E-M, so qemu's M7 executes it natively (no RP2040
# machine exists in qemu; the microbit legs prove the v6-M encoding floor).
# The egg bakes from source on the emulated core; exit 42 = hatched + laws held.
.PHONY: test_mps2_t1
test_mps2_t1: host out/host$(hsuf)/mooncc
	@sh test/gate/boot.sh mps2_t1 "$(MAKE)"
# test_mps2_wake -- the IMAGE lane: the baker bakes the corpus on qemu's M7 and
# dumps a fully-symbolic heap image (build-time bake); the WAKER -- a different
# binary, arena deliberately offset -- wakes it and re-runs the driver laws.
# The gate that finally wakes a mooncc image (and the teensy's build rides the
# same love.img).
.PHONY: test_mps2_wake
test_mps2_wake: host out/host$(hsuf)/mooncc
	@sh test/gate/boot.sh mps2_wake "$(MAKE)"
# test_thumb2sp -- the SP-only-FPU face (the playdate's STM32F746): f64
# arithmetic SOFTENS to __aeabi_* libgcc calls while the 64-bit transfers keep
# the d-reg value model. Gated on qemu's mps2-an386 -- a Cortex-M4 whose
# FPv4-SP FPU FAULTS on any f64 arithmetic that slipped through -- against
# gcc -mfpu=fpv4-sp-d16, which softens through the SAME libgcc helpers, so
# even am.c stays BIT-exact. Three legs: VFP doubles (45), am.c (9),
# composites+varargs (18).
.PHONY: test_thumb2sp
test_thumb2sp: host out/host$(hsuf)/mooncc
	@sh test/gate/thumb.sh thumb2sp $(ho)
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
	@sh test/gate/boot.sh nucleo446 "$(MAKE)"
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
# socket nifs in host/sock.c + the pump loops in tools/ain.l). In `test_all`
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
