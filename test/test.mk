# test/test.mk -- the test_* gates (and the uuwm/uukind corpus generators).
# A fragment of the root Makefile, included by ./Makefile and invoked from the
# project root; paths resolve from there. Shared vars live in mk/common.mk.

# every gate below is phony: one roster, so adding a gate is one line and not two.
.PHONY: \
  test_filemode test_stdinbuf test_embed test_glaze test_hook test_glazefuzz test_sat test_drat test_lux \
  test_sb test_kore test_refuzz test_nest test_cookdiff test_dist test_up test_vi test_moon test_clay test_moonfuzz \
  test_ccarm64 test_ccriscv test_cts test_cts_arm64 test_cts_riscv test_libc test_ulp \
  test_selfhost test_raw test_drv test_asmops test_vec test_fixpoint test_raw_bake test_riscv \
  test_raw_riscv test_raw_arm64 test_thumb1 test_thumb2 test_virt test_mps2 test_mps2_t1 \
  test_mps2_wake test_thumb2sp test_playdate test_teensy41 test_nucleo446 test_nucleo446_smoke \
  test_rp2040 moon-tar moon-tar-arm64 moon-tar-riscv moon-m4 moon-m4-arm64 moon-m4-riscv \
  moon-lua moon-lua-arm64 moon-lua-riscv moon-sqlite moon-sqlite-arm64 moon-sqlite-riscv \
  moon-gzip moon-gzip-arm64 moon-gzip-riscv moon-bzip2 moon-bzip2-arm64 moon-bzip2-riscv \
  test_holo test_as test_elf32 test_objcopy test_gz test_splice test_distboot test_bakerep

# $(mw) -- the WARM love: the freshly-baked image woken instead of the egg compiled
# from source (12 ms against 1.05 s). Both lanes carry the same vocabulary, so warm
# is a speed choice; the EGG gates take $m, whose LOVE_NO_IMAGE this Makefile exports.
mw = env -u LOVE_NO_IMAGE $m

# love0 bakes prel+ev+repl + the whole corpus and self-tests BOTH compilers in one run
# (-Dai_tco=0, the trampoline lane too), so it must print TWO "tests pass" summaries: a
# reader stop drops the rest of the stream and exits 0. Status rides `.rc` -- no pipefail.
# ⚠ corpus.list IS A RUNTIME INPUT NOW, not only a stamp: love0 reads it to find the corpus
# (host/main.c), so it has to EXIST before love0 runs. It used to be pulled in as tests0.h's
# prerequisite; with the corpus off the bootstrap's dependency graph, nothing else asks for it,
# and a fresh tree died with `love0: corpus: cannot open out/lib/corpus.list` -- which the
# unpacked-release path found and no in-tree run could, out/lib always being warm here.
test_love0: $(love0) out/lib/corpus.list
	@echo TEST $(love0)
	@{ $(love0) </dev/null; echo $$? > out/host/.test_love0.rc; } | tee out/host/.test_love0.out; \
	  s=$$(cat out/host/.test_love0.rc); \
	  [ $$s -eq 0 ] && [ `grep -c "tests pass" out/host/.test_love0.out` -eq 2 ]
# test_filemode -- FILE MODE IS TERMINAL, and nothing inside the corpus can gate that:
# a test that proves the run dies cannot also report. so a shell runs one two-line file
# and asks both halves of the law -- the face on err AND exit 1 -- for a missing name.
test_filemode: $m
	@echo TEST file mode is terminal
	@printf '(: _ (puts "reached\\n") _ (an-name-the-book-lacks 1) (puts "past\\n"))\n' > out/host/.test_filemode.l
	@$m out/host/.test_filemode.l > out/host/.test_filemode.out 2>&1; r=$$?; \
	  { [ $$r -eq 1 ] && grep -q "^reached$$" out/host/.test_filemode.out \
	      && grep -q "^;; missing an-name-the-book-lacks$$" out/host/.test_filemode.out \
	      && ! grep -q "^past$$" out/host/.test_filemode.out; } \
	    || { cat out/host/.test_filemode.out; echo "FAIL file mode not terminal (exit $$r)"; exit 1; }
# test_stdinbuf -- WHAT WE BORROW OF fd 0 IS INVISIBLE, and we borrow two things. Both doors
# read the device in 4096-byte gulps (core/love.c's rbio_of), so the first law is that BOTH ANSWER
# THE SAME: the bytes our reader has not taken are still there for an in-form (slurp in), and
# still there for a child that inherits the fd. A seekable door puts them back with an lseek;
# a pipe has no rewind, so stdin_hand DELIVERS them down a fresh pipe instead -- which is why
# the handoff laws below are asked of the PIPE output directly and not only of the diff. The
# pipe also lends its O_NONBLOCK bit (`inflag`), and the law for that one is read straight off
# /proc: a child must inherit fd 0 BLOCKING, or it takes an empty pipe for an ended one.
# The corpus cannot gate any of this; it exists only BETWEEN two ways of being fed.
test_stdinbuf: $m
	@echo TEST stdin borrows a run
	@printf '(say out (+ "rest: [" (+ (slurp in) "]")))\n(say out "tail form")\n' > out/host/.test_stdinbuf1.l
	@printf '(exec (L "cat"))\nHANDOFF-TAIL\n' > out/host/.test_stdinbuf2.l
	@for f in out/host/.test_stdinbuf1.l out/host/.test_stdinbuf2.l; do \
	   $m < $$f > $$f.seek 2>&1; cat $$f | $m > $$f.pipe 2>&1; \
	   cmp -s $$f.seek $$f.pipe \
	     || { echo "FAIL $$f: the buffered door differs from the bare one"; \
	          diff $$f.pipe $$f.seek; exit 1; }; done
	@grep -qF 'rest: [(say out "tail form")' out/host/.test_stdinbuf1.l.seek \
	  || { cat out/host/.test_stdinbuf1.l.seek; echo "FAIL an in-form (slurp in) lost the remainder"; exit 1; }
	@for w in seek pipe; do grep -qF HANDOFF-TAIL out/host/.test_stdinbuf2.l.$$w \
	  || { cat out/host/.test_stdinbuf2.l.$$w; echo "FAIL the exec'd child lost the fd position ($$w)"; exit 1; }; done
	@# ..and the residue is only the FIRST gulp: past it the pumper must splice the rest of the
	@# pipe, whose writer is still going (200000 bytes against a 64K pipe, so cat really blocks).
	@{ printf '(exec (L "cat"))\n'; yes HANDOFF-BULK | head -c 200000; } > out/host/.test_stdinbuf4.l
	@cat out/host/.test_stdinbuf4.l | $m 2>/dev/null | wc -c > out/host/.test_stdinbuf4.n
	@n=`cat out/host/.test_stdinbuf4.n`; [ $$n -eq 200000 ] \
	  || { echo "FAIL the pumper delivered $$n of 200000 -- the splice past the residue stopped short"; exit 1; }
	@printf '(exec (L "cat" "/proc/self/fdinfo/0"))\n' > out/host/.test_stdinbuf3.l
	@cat out/host/.test_stdinbuf3.l | $m > out/host/.test_stdinbuf3.out 2>&1; \
	  fl=$$(sed -n 's/^flags:[[:space:]]*//p' out/host/.test_stdinbuf3.out); \
	  [ -n "$$fl" ] && [ $$(( $$fl & 04000 )) -eq 0 ] \
	    || { cat out/host/.test_stdinbuf3.out; \
	         echo "FAIL fd 0 handed on nonblocking (flags $$fl) -- stdin_give did not put the bit back"; exit 1; }
# ..and the GIVE-BACK rides the same seek: `unchug` puts drained bytes back into the run, so
# ai_io_pending counts them again and the child inherits fd 0 in front of them. ⚠ THE CONTRAST
# IS THE LAW: `chug` drains the WHOLE borrowed run, so without the give-back the child inherits
# NOTHING and with it all ten. It is what an ai_io_unread reaching by bio_of would break -- the
# run is BORROWED under a static, so only rbio_of finds it, and a heap-port-only door would
# answer 0 here while every file-port law in test/io.l still passed.
	@printf 'abcdefghij' > out/host/.test_stdinbuf4.in
	@p='(: c (see in) _ (unsee in c) t (chug in)'; \
	  a=`$m -e "$$p k (unchug in 99) (exec (L \"cat\")))" < out/host/.test_stdinbuf4.in`; \
	  b=`$m -e "$$p (exec (L \"cat\")))" < out/host/.test_stdinbuf4.in`; \
	  { [ "$$a" = abcdefghij ] && [ -z "$$b" ]; } \
	    || { echo "FAIL unchug is not in the inherited fd offset (with=[$$a] without=[$$b])"; exit 1; }
# test_host takes the corpus as a FILE, and that is a SPEED choice, not a necessity:
# stdin works (test/io.l used to poke `in` and eat a byte of whatever fed the suite --
# it taps a charlist now), and it is equally strict, quitting 1 on a scare either way.
# What is left of the gap is the READER, not the device: a redirect gulps 4096 like
# the file does (core/love.c's rbio_of), but `reads` trickles `in` a byte at a time to keep
# its position exact, which costs ~1.45x here. This is the gate that runs constantly.
# cat'ing also keeps the corpus's one-global-scope property.
test_host: $m
	@echo TEST $m
	@cat $t > out/host/.test_host.l
	@{ $m out/host/.test_host.l </dev/null; echo $$? > out/host/.test_host.rc; } | tee out/host/.test_host.out; \
	  s=$$(cat out/host/.test_host.rc); \
	  [ $$s -eq 0 ] && grep -q "tests pass" out/host/.test_host.out
# test_stdincorpus -- THE ORACLE FOR `reads` OVER STDIN, and nothing else was one. test_host
# takes the corpus as a FILE (the speed choice above) and test_stdinbuf runs two-line programs,
# so at the scale where a reader's window arithmetic actually breaks, nothing looked: a `reads`
# that parsed the corpus's own English COMMENTS as code still printed "3959 tests pass" on the
# file door and exited 0. It was caught by hand-diffing the doors; this is that diff, kept.
# ⚠ ALL THREE DOORS, because they are three different readers -- a file and a redirect share the
# borrowed run (core/love.c's rbio_of), a pipe has none and drips.
# ⚠ the summary line carries a DURATION, so that is normalised away and everything else must
# match byte for byte -- the dots included, since a dropped assert is exactly what this catches.
# ⚠ AND BOTH LOVES. $m is the EGG lane (LOVE_NO_IMAGE); $(mw) is the BAKED image, which is
# what `love` is on a user's PATH. They are not interchangeable here: a reader bug that lost two
# bytes of the corpus showed on the baked lane and NOT on the egg one, so a gate that ran only
# $m reported ok while the shipped binary read 2286 of 3959 asserts and quit 1.
test_stdincorpus: $m
	@echo TEST the corpus down file, redirect and pipe -- egg and baked
	@cat $t > out/host/.test_sc.l
	@for L in "$m" "$(mw)"; do \
	 for d in file seek pipe; do \
	   case $$d in \
	     file) $$L out/host/.test_sc.l < /dev/null > out/host/.test_sc.$$d 2>&1;; \
	     seek) $$L < out/host/.test_sc.l > out/host/.test_sc.$$d 2>&1;; \
	     pipe) cat out/host/.test_sc.l | $$L > out/host/.test_sc.$$d 2>&1;; \
	   esac; \
	   r=$$?; \
	   [ $$r -eq 0 ] \
	     || { echo "FAIL [$$L] the $$d door exited $$r"; tail -4 out/host/.test_sc.$$d; exit 1; }; \
	   grep -q "tests pass" out/host/.test_sc.$$d \
	     || { echo "FAIL [$$L] the $$d door printed no summary"; tail -4 out/host/.test_sc.$$d; exit 1; }; \
	   ! grep -q "^;;" out/host/.test_sc.$$d \
	     || { echo "FAIL [$$L] the $$d door scared"; grep -m3 "^;;" out/host/.test_sc.$$d; exit 1; }; \
	   sed 's/in [0-9.]* seconds/in Xs/' out/host/.test_sc.$$d > out/host/.test_sc.$$d.n; \
	 done; \
	 for d in seek pipe; do \
	   cmp -s out/host/.test_sc.file.n out/host/.test_sc.$$d.n \
	     || { echo "FAIL [$$L] the $$d door read a different corpus than the file door"; \
	          diff out/host/.test_sc.file.n out/host/.test_sc.$$d.n | head -8; exit 1; }; \
	 done; \
	 done
	@echo "  ok   file, redirect and pipe read the corpus identically on both loves"
# test_front -- the TEST-ONLY FRONTEND: out/host/front links liblove.a (core/love.c only)
# and supplies the frontend contract itself, so its port vt can answer WOULD-BLOCK on
# cue (doc/io.md). ⚠ it EXITS 97 on a wait with no deadline -- a deadlock, said loudly.
$(ho)/front: test/front/main.c $(love_h) $(ho)/liblove.a $(ho)/.hostcc $(R)/core/love_data.ld \
    out/lib/egg.h out/lib/post.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/bao.h
	@echo CC	$@
	@mkdir -p $(dir $@)
	@$(hcc) -o $@ test/front/main.c $(ho)/liblove.a $(data_ld) $(nifs_ld)
test_front: $(ho)/front
	@echo TEST $(ho)/front
	@sh test/gate/run.sh -a front "$(ho)/front" "front: ok" test/front/io.l
# test_embed -- EVERY FRONTEND BUILT, NONE BOOTED. Each embed site spells core/love.h's structs and
# its own ai_libs table, so a core ABI edit breaks them -- but every gate that would SAY so
# boots, sits in test_extra, and costs minutes. Two phases: COMPILE every frontend (seconds,
# ours is the only toolchain, no qemu), then LINK every binary that can be linked here.
# ⚠ each frontend is asked for ITS OWN object and ITS OWN ELF through ITS OWN makefile:
# re-spelling the flags would drift from the build this claims to gate, and the green would
# mean nothing. That is also why the link phase skips rather than improvises.
# ⚠ wasm compiles and does NOT link here -- love.js is a tracked committed artifact and a
# gate must not rewrite the working tree. test_wasm owns that link, out of tree.
# ⚠ THE TIERS ARE THE POINT. host and free (the kernel) are the TWO PRIMARY TARGETS; everything
# else under port/ is a real target and a SECONDARY one, and secondary means it cannot hold a
# commit. This gate used to compile every board, so an mps2 that would not build stopped work on
# the two targets that matter -- backwards. The boards moved to test_extra, which is where a
# secondary target belongs. ⚠ port/virt is the exception that proves the rule: it is the
# FREESTANDING KERNEL on riscv, so it is free's third arch and rides with the primaries.
embed_ports = virt
embed_boards = mps2 teensy41 nucleo446 playdate rp2040
# what each linkable port calls its ELF. ⚠ teensy41 is NOT here: its ELF embeds the baked
# heap image, whose rule delegates to port/mps2's `img` -- a BAKE UNDER QEMU, on a FORCE rule
# with no opt-out. Linking it here would cost 80 s and quietly boot a machine, which is the
# one thing this gate promises not to do. test_teensy41 (test_extra) owns that link.
embed_elfs = mps2/love.elf nucleo446/firm.elf rp2040/love.elf
# the aarch64 kernel face needs a CROSS-CAPABLE KCC -- ours or clang, never a native gcc
embed_a64 = $(or $(KCC_IS_MOON),$(filter 1,$(KCC_IS_CLANG)))
embed_arm := $(and $(shell command -v arm-none-eabi-gcc 2>/dev/null),\
                   $(shell command -v arm-none-eabi-ld 2>/dev/null))
test_embed: host $(ho)/mooncc
	@echo TEST the frontends compile against core/love.h "(object only)"
	@$(MAKE) -s kmain_o
	@$(if $(embed_a64),$(MAKE) -s a=aarch64 kmain_o,echo "  (aarch64 kmain.c skipped: $(KCC) cannot cross)")
	@$(MAKE) -s K_TEST=1 kmain_o
	@for p in $(embed_ports); do \
	   $(MAKE) -s -C port/$$p ../../out/$$p/main.o \
	     || { echo "FAIL port/$$p/main.c does not compile against core/love.h"; exit 1; }; \
	 done
	@$(if $(wildcard $(EMCC)),$(MAKE) -s -C wasm ../out/wasm/host.o,echo "  (wasm/host.c skipped: no emcc)")
	@echo TEST the frontends link "(no boot, no qemu)"
	@$(MAKE) -s kernel || { echo "FAIL the $a kernel does not link"; exit 1; }
	@$(if $(embed_a64),$(MAKE) -s a=aarch64 kernel,echo "  (aarch64 kernel link skipped: $(KCC) cannot cross)")
	@$(MAKE) -s -C port/virt ../../out/virt/love.elf || { echo "FAIL port/virt does not link"; exit 1; }
	@echo "test_embed: host, free (x86_64 + aarch64 + riscv) and wasm build against core/love.h"

# the SECONDARY boards, same checks one tier down -- real targets that cannot hold a commit.
test_embed_boards: host $(ho)/mooncc
	@echo TEST the secondary boards compile against core/love.h "(object only)"
	@for p in $(embed_boards); do \
	   $(MAKE) -s -C port/$$p ../../out/$$p/main.o \
	     || { echo "FAIL port/$$p/main.c does not compile against core/love.h"; exit 1; }; \
	 done
	@if [ -n "$(embed_arm)" ]; then \
	   for t in $(embed_elfs); do p=$${t%%/*}; f=$${t#*/}; \
	     $(MAKE) -s -C port/$$p ../../out/$$p/$$f \
	       || { echo "FAIL port/$$p does not link"; exit 1; }; done; \
	 else echo "  (mps2 nucleo446 links skipped: no arm-none-eabi)"; fi
	@if [ -n "$(embed_arm)" ] && [ -n "$$PLAYDATE_SDK_PATH" ]; then \
	   $(MAKE) -s -C port/playdate ../../out/playdate/pdex.elf \
	     || { echo "FAIL port/playdate does not link"; exit 1; }; \
	 else echo "  (playdate link skipped: needs arm-none-eabi + PLAYDATE_SDK_PATH)"; fi
	@echo "test_embed_boards: the secondary boards build (any skip is named above)"
# Host-nif smoke tests: host/*.c nifs link into `love` but NOT love0, so they live under
# test/host/, invisible to the corpus glob ($t is a non-recursive test/*.l). Gate = exit 0
# AND a "<name>: ok"; WARM but for hostnif_cold.
hostnif_tests = test/host/rdiff.l test/host/loader.l test/host/gcpause.l test/host/run.l test/host/pty.l test/host/net.l test/host/lux.l test/host/luxui.l test/host/baoedit.l test/host/baotest.l test/host/init.l test/host/fs.l test/host/sh.l test/host/cb.l test/host/berth.l test/host/wharf.l test/host/limn.l test/host/manifest.l test/host/overlay.l test/host/bake.l test/host/rove.l test/host/rune.l test/host/lapiz.l test/host/papel.l test/host/kiosko.l test/host/sbhttp.l test/host/json.l test/host/salt.l test/host/libra.l test/host/infix.l test/host/clay.l test/host/fat.l test/host/tls.l test/host/tlsc.l test/host/gz.l
# out/host/lush: test/host/sh.l drives the BUILT shell end to end, via out/host/love and
# never env's PATH love -- the tree's nifs, not the nest's.
hostnif_cold =                                   # empty: no gate needs the cold lane
test_hostnif: host out/host$(hsuf)/lush
	@for s in $(hostnif_tests); do echo "TEST $$s"; \
	  case " $(hostnif_cold) " in *" $$s "*) L="$m";; *) L="$(mw)";; esac; \
	  cat test/00-init.l $$s | sh test/gate/run.sh hostnif "$$L" ": ok" \
	    || { echo "  (the gate above is $$s)"; exit 1; }; \
	done
# Runnable design companions in doc/ -- pure-love models that pin the shape a C design
# takes (doc/stream.l ~ doc/io.md part II). Zero-dep, but they leak helper names into the
# one global scope, so they run standalone. Same contract: exit 0 AND a "<name>: ok".
doc_tests = doc/stream.l doc/proto/dest.l doc/proto/spl.l
test_doc: host
	@for s in $(doc_tests); do echo "TEST $$s"; \
	  cat test/00-init.l $$s | sh test/gate/run.sh doc "$(mw)" ": ok" \
	    || { echo "  (the gate above is $$s)"; exit 1; }; \
	done
# the vmsplice JIT pipeline end to end (test/bench/vmsplice/auto.sh): dis a live closure,
# compose its ops to C, mooncc it, bind against THIS process, nif, differential vs the
# interp twin. NOT in test_slow (needs mooncc + a host cc, bench-shaped); run by hand.
# `dis` itself is gated in the corpus by test/dis.l. x86-64 only (compose emits x86 ABI).
ifeq ($a,x86_64)
test_vmsplice: host
	@sh test/bench/vmsplice/auto.sh
endif
# Native-codegen self-tests (the love/glaze/ x86-64 jit): test/glaze-x86.l covers emit
# (the SSE emitter) + auto (ev's source-recognizer), cats the holo backends ahead of
# itself, and runs each block through base-ev. Needs the `nat` nif; x86-64 only.
ifeq ($a,x86_64)
test_glaze: host
	@echo TEST test/glaze-x86.l "(emit + auto)"
	@{ echo "(use 'holo)"; cat crew/holo/x64.l crew/holo/arm64.l test/glaze-x86.l; } \
	  | sh test/gate/run.sh glaze "$m" "test/glaze-x86:"
else
test_glaze:
	@echo "test_glaze: skipped (host arch $a is not x86_64)"
endif
# test_hook -- the natjit CREATION-HOOK laws: every law claims BOTH the answer and that the hook
# owned it (`fired?`), twice over the hook's two lives -- the IMAGE's ($(mw), what ships) and the
# EGG BOOT's ($m). ⚠ never by cat'ing hook.l in: a woken image has `nif` off the book.
ifneq ($(filter $a,x86_64 aarch64),)
test_hook: host
	@echo TEST test/glaze-hook.l "(the baked image)"
	@sh test/gate/run.sh -a hook "$(mw)" "glaze-hook: ok" test/glaze-hook.l
	@echo TEST test/glaze-hook.l "(egg boot, hook.l out of the tree)"
	@sh test/gate/run.sh -a hook "$m" "glaze-hook: ok" test/glaze-hook.l
else
test_hook:
	@echo "test_hook: skipped (the hook emits for x86_64 / aarch64; host arch is $a)"
endif
# test_glazefuzz -- the glaze's DIFFERENTIAL fuzz (love/glaze/fuzz.l): 3000 random closures
# run TWICE against the SAME binary (plain, then LOVE_NO_GLAZE=1), stdouts byte-identical.
# `fires=` is the checked proof of work; stderr is dropped (the two runs scare differently).
ifneq ($(filter $a,x86_64 aarch64),)
test_glazefuzz: host
	@echo TEST love/glaze/fuzz.l "(glaze differential fuzz: glazed vs interpreted)"
	@on=out/host/.gfuzz_on.out; off=out/host/.gfuzz_off.out; \
	  $m love/glaze/fuzz.l > $$on 2>/dev/null \
	    || { echo "FAIL glazefuzz: the GLAZED run died"; exit 1; }; \
	  LOVE_NO_GLAZE=1 $m love/glaze/fuzz.l > $$off 2>/dev/null \
	    || { echo "FAIL glazefuzz: the INTERPRETED run died"; exit 1; }; \
	  fon=`sed -n 's/^fires=//p' $$on`; foff=`sed -n 's/^fires=//p' $$off`; \
	  [ -n "$$fon" ] && [ -n "$$foff" ] \
	    || { echo "FAIL glazefuzz: no fires= trailer -- a run stopped early"; exit 1; }; \
	  [ "$$fon" -gt 0 ] \
	    || { echo "FAIL glazefuzz: the glazed run native-backed NOTHING (fires=0) -- the fuzz proved nothing"; exit 1; }; \
	  [ "$$foff" -eq 0 ] \
	    || { echo "FAIL glazefuzz: LOVE_NO_GLAZE=1 still fired (fires=$$foff)"; exit 1; }; \
	  sed '$$d' $$on > $$on.body; sed '$$d' $$off > $$off.body; \
	  cmp -s $$on.body $$off.body \
	    || { echo "FAIL glazefuzz: the glaze DISAGREES with the interpreter"; \
	         echo "  first differing case:"; \
	         cmp $$on.body $$off.body 2>&1 | head -1; \
	         n=`cmp $$on.body $$off.body 2>/dev/null | sed -n 's/.*line \([0-9]*\).*/\1/p'`; \
	         [ -n "$$n" ] && { echo "  glazed: `sed -n $${n}p $$on.body`"; \
	                           echo "  interp: `sed -n $${n}p $$off.body`"; }; \
	         exit 1; }; \
	  echo "  $$fon closures native-backed, `wc -l < $$on.body` cases, all agree"
else
test_glazefuzz:
	@echo "test_glazefuzz: skipped (the glaze emits for x86_64 / aarch64; host arch is $a)"
endif
# crew/sat/ -- the CDCL SAT solver app. Portable love (no glaze), so it runs on every arch.
# Gate = exit 0 AND the sentinels. COLD ($m, not $(mw)) -- the one app gate that is: the
# solver answers in 3.6 s over the fresh egg and 14.4 s over the woken image.
test_sat: host
	@echo TEST crew/sat/sat.l + crew/sat/dimacs.l + crew/sat/flat.l
	@cat crew/sat/sat.l crew/sat/dimacs.l crew/sat/flat.l \
	  | sh test/gate/run.sh sat "$m" "sat: Stages 1-3 ok|crew/sat/dimacs: ok|crew/sat/flat: ok"
# The DRAT lane's EXTERNAL check: flat.l's refutations verified by drat-trim, the SAT
# competition's own checker (fetched + built into out/drat on first use; skips offline).
# The in-gate twin (fd-check) runs inside test_sat. Not in test_slow (network).
test_drat: host
	@cd crew/sat && ./dratcheck.sh || { echo "FAIL drat"; exit 1; }
# The lux app's pure core (crew/lux/core.l): xmonad's StackSet -- focus zipper, workspace
# sheaf, floating half -- with xmonad's QuickCheck laws + a seeded fuzz. Pure love, so it
# self-tests portably; the X layers need connectu and are proven against Xephyr, not here.
test_lux: host
	@echo TEST crew/lux/core.l ... crew/lux/config.l + crew/lux/law.l "(the whole app, host)"
	@cat test/00-init.l crew/lux/core.l crew/lux/layout.l crew/lux/wire.l crew/lux/ewmh.l \
	    crew/lux/manage.l crew/lux/keys.l crew/lux/config.l crew/lux/law.l \
	  | sh test/gate/run.sh lux "$(mw)" "crew/lux/law: StackSet"
# the SEAT lane -- an app fired by its own FILE NAME (positional, or a -l preload), which
# is the one dispatch door no other gate reaches: every app gate below drives its subject
# through the verb rail or a baked image instead. ~2.5s, most of it one bake, and it rides
# test_slow because the failure it catches is silent by construction (a seat that answers
# () is indistinguishable from an app with nothing to say).
.PHONY: test_seat
test_seat: host
	@echo TEST test/gate/seat.sh "(the file-seat lane)"
	@sh test/gate/seat.sh $m

test_sb: host out/host$(hsuf)/sb
	@echo TEST crew/sb/sb.l + test/host/sb.l
	@rm -rf out/host/.sbtest
	@cat test/00-init.l test/host/sb.l | sh test/gate/run.sh sb "$(mw)" "sb: ok"
# the kore smokes drive the BAKED image (`wake kore.image`), ~0.02s vs ~0.75s per spawn
# over the ~68 tool runs; the argv0-symlink smoke execs the real shim, whose basename-$0
# dispatch the wake bypasses. the synthetic "kore" argv0 keeps the exit faces unchanged.
korerun = $m wake $(ho)/kore.image kore
test_kore: host out/host$(hsuf)/kore out/host$(hsuf)/kore.image out/host$(hsuf)/mooncc out/host$(hsuf)/mooncc.image
	@sh test/gate/kore.sh $(ho) $m
# grep + sed against GNU over SEEDED RANDOM patterns (doc: the script's own head).
# test_kore's battery is a list someone thought of; this one is not, which is why it
# found the (a*)+ empty-iteration bug and the -w greedy-span bug that the battery,
# the laws and a green test_slow all sat happily on top of. Skips (exit 0) where GNU
# grep/sed are absent -- and checks --version, since an interactive `grep` may be a
# ugrep shim whose BRE differs. Rides test_extra: it costs ~a minute.
test_refuzz: host out/host$(hsuf)/kore out/host$(hsuf)/kore.image
	@sh test/gate/refuzz.sh $m out/host$(hsuf)/kore.image
# cook against GNU MAKE, differentially (doc: the script's own head). The oracle is a
# SECOND IMPLEMENTATION, and it has to be: a builtin cook never implemented is a VARIABLE
# reference in make's grammar, so it expands to EMPTY and the build carries on -- invisible
# to any test that only asks whether cook agrees with itself. Skips (exit 0) where GNU make
# is not on the box, since there is no oracle to ask.
test_cookdiff: host out/host$(hsuf)/kore out/host$(hsuf)/kore.image
	@sh test/gate/cookdiff.sh $(ho)/kore
# the install nest, three ways (make / cook / cook+kore PATH lane) -- one shape.
test_nest: host out/host$(hsuf)/kore out/host$(hsuf)/kore.image out/host$(hsuf)/mooncc.image
	@sh test/gate/nest.sh $(ho) $m
# the dist artifact: test_dist smokes the verb rail on the baked one-file binary (seconds,
# test_slow); test_up runs the WHOLE download door -- origin recorded, kiosko serving it,
# `love up` cooking a scratch nest from source -- and is OPT-IN (minutes).
test_dist: out/dist/love-$a
	@sh test/gate/dist.sh smoke out/dist/love-$a
test_up: out/dist/love-$a
	@sh test/gate/dist.sh up out/dist/love-$a
# The editor (crew/vi/): the pure modal engine's laws (no tty -- vstep driven byte by
# byte), then scripted end-to-end passes through the `kore vi` face over a pipe (keys off
# stdin, frames onto a captured stdout, :wq writes), driven through the baked kore.image.
test_vi: host out/host$(hsuf)/kore.image
	@echo TEST crew/vi/{hue,core,law}.l
	@cat test/00-init.l crew/kore/text.l crew/kore/u.l crew/kore/core.l crew/kore/re.l lib/lint.l \
	    crew/vi/config.l crew/vi/hue.l crew/vi/core.l crew/vi/law.l \
	  | sh test/gate/run.sh vi "$(mw)" "crew/vi/law:"
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
# The C compiler (crew/moon/, doc/moon.md): the pure pipeline's goldens, then stage-0 end
# to end through the real `mooncc` -- compile, run, exit 42, against a gcc -O0 differential
# on the same source. Drives the WARM image (~0.68s -> ~0.02s per compile, 88 of them).
moonrun = $m wake $(ho)/mooncc.image mooncc
# love0 rides along for the inline-asm checks: templates parse through holo/text.l, whose
# combinators come off the bare `post` each frontend's boot binds ITSELF, so the bootstrap
# lane can lose the feature while this one keeps it.
test_moon: host $(love0) out/host$(hsuf)/mooncc out/host$(hsuf)/mooncc.image
	@sh test/gate/moon.sh $(ho) $m $(love0)
# the COMMITTED GENERATED artifacts, laid from the tables that define them (core/mx.l the +/*
# dispatch matrices and the kind lattice they index, core/nifs.l the nif + instruction registry,
# quay.l the xterm-256 palette host and kernel share). `make mx` refreshes; test_clay diffs.
# ⚠ core/mx.h/kinds.h/nifs.h are CORE headers -- a refresh rebuilds the tree, so the gate to run
# after is `make test`, not test_clay alone. Each is written aside and moved only once the
# whole set lays, so a shape check that quits leaves every committed file untouched.
# core/love_data.ld is laid WHOLE; a board's own script is its own memory map, so core/mx.l takes that
# text and answers it with the marked block relaid -- the .lds recipe's IO is a pipe.
mx_lds = free/x86_64/x86_64.lds free/aarch64/aarch64.lds
mx_lay = (: _ (? mx-ok 0 (quit 1)) _ (puts (mx-lds \"$$f\" (slurp in))) (quit 0))
# dest:source:value:shape-check -- ONE roster, read by `make mx` (which writes) and by
# test_clay (which regenerates and diffs). Two spellings of this list is how they drift.
mx_gen = core/mx.h:core/mx.l:mx-h:mx-ok core/kinds.h:core/mx.l:kinds-h:mx-ok core/nifs.h:core/nifs.l:nifs-h:nifs-ok \
         crew/quay/xterm256.h:crew/quay/quay.l:q-c:q-ok core/love_data.ld:core/mx.l:mx-ld:mx-ok
# /warn the \# escapes are load-bearing: a bare # in a make VARIABLE starts a comment and
# would eat the rest of the line (a recipe line passes # through, a variable does not).
mxsplit = d=$${s%%:*}; r=$${s\#*:}; l=$${r%%:*}; r=$${r\#*:}; v=$${r%%:*}; k=$${r\#*:}; o=out/.`basename $$d`
mxlay   = $(mw) -l $$l -e "(: _ (? $$k 0 (quit 1)) _ (puts $$v) (quit 0))"
mx: host
	@echo LOVE	core/mx.h core/kinds.h core/nifs.h xterm256.h core/love_data.ld "+5 .lds (core/mx.l + core/nifs.l + quay.l on $m)"
	@for s in $(mx_gen); do $(mxsplit); $(mxlay) > $$o || exit 1; done
	@for s in $(mx_gen); do $(mxsplit); mv $$o $$d; done
	@t=out/.lds.$$$$; for f in $(mx_lds); do \
	   $(mw) -l core/mx.l -e "$(mx_lay)" < $$f > $$t || { rm -f $$t; exit 1; }; \
	   cmp -s $$t $$f || { mv $$t $$f; echo "LOVE	$$f"; }; \
	 done; rm -f $$t
# ...and the DEPENDENCY, off the same roster: a committed generated file is stale the moment
# its table moves, and the objects that include it then rebuild from the fresh one. Without
# this a new core/nifs.l row builds clean and gates GREEN with its nom still off the book -- the
# drift diff lives in test_clay, which only test_extra reaches.
# ⚠ the prerequisite is the TABLE ALONE, never $(m): love is built FROM these headers, so
# naming it as a prerequisite closes the loop and make drops the lot. The recipe instead takes
# whatever love ALREADY exists -- sound because the generator is core/nifs.l/mx.l themselves, and a
# stale love lays a fresh table. A tree with no love yet is the bootstrap case: the committed
# file is what builds the first one, so the rule stands aside and only marks it seen.
define mx_dep
$(word 1,$(subst :, ,$(1))): $(word 2,$(subst :, ,$(1)))
	@if test -x $$(m); then \
	   $$(mw) -l $$< -e "(: _ (? $(word 4,$(subst :, ,$(1))) 0 (quit 1)) _ (puts $(word 3,$(subst :, ,$(1)))) (quit 0))" > out/.$$(@F) || exit 1; \
	   cmp -s out/.$$(@F) $$@ || echo "LOVE	$$@ (relaid -- $$< moved)"; \
	   mv out/.$$(@F) $$@; \
	 else touch $$@; fi
endef
$(foreach s,$(mx_gen),$(eval $(call mx_dep,$(s))))
# test_clay -- G1, clay's faithfulness gate (crew/moon/clay.l, doc/clay.md): for every file
# in test/cc/, (cparse (clay-show ast)) == ast, STRUCTURALLY. ⚠ the run PARTITIONS and names
# both halves: what it can say, and the declarations cparse did not keep -- a measured gap.
test_clay: host out/host$(hsuf)/mooncc.image
	@echo TEST test/gate/clay.l "(clay G1: (cparse (clay-show c)) == c over test/cc)"
	@$m wake $(ho)/mooncc.image -l test/gate/clay.l < /dev/null
# ...and the CONSUMERS: the generated headers regenerate and DIFF here -- a hand edit to any,
# or a table edit with no regen, is a red. The roster is mx_gen above; `cmp`, not rtk diff.
	@for s in $(mx_gen); do $(mxsplit); $(mxlay) > $$o; \
	   cmp -s $$o $$d || { echo "FAIL $$d is not what $$l lays -- run: make mx"; \
	                       diff -u $$d $$o | head -20; exit 1; }; done
	@t=out/.lds.$$$$; for f in $(mx_lds); do \
	   $(mw) -l core/mx.l -e "$(mx_lay)" < $$f > $$t || { rm -f $$t; exit 1; }; \
	   cmp -s $$t $$f || { echo "FAIL $$f is not what core/mx.l lays -- run: make mx"; diff -u $$f $$t | head -20; rm -f $$t; exit 1; }; \
	 done; rm -f $$t
	@echo "clay-mx: core/mx.h, core/kinds.h, core/nifs.h, xterm256.h and the 6 love_data scripts regenerate identically"
	@for s in $(mx_gen); do $(mxsplit); rm -f $$o; done
# test_moonfuzz -- moon's REFUSAL surface: each test/cc file broken
# eight ways from a fixed seed. Two reds -- no SCARE, no hang -- plus G1 on every mutant that
# still parses, and a printed CENSUS of named-vs-bare refusals. stderr is KEPT: cpp speaks there.
test_moonfuzz: host out/host$(hsuf)/mooncc.image
	@echo TEST test/gate/moonfuzz.l "(moon refusal fuzz: 888 mutants of test/cc)"
	@$m wake $(ho)/mooncc.image -l test/gate/moonfuzz.l < /dev/null
# test_splice -- the splice JIT end to end (lib/splice.l, test/bench/vmsplice/README.md): a live
# closure's op rows, each op's own IR taken out of THIS BINARY's .rodata (mooncc -fir=lvm_ put
# it there), spliced into one body, assembled by holo, its one external reference bound to a
# live address, nif'd -- and required to agree with the closure it came from. ⚠ a PLAIN love:
# no compiler, no source tree, no object file, which is the whole point of the IR being in the
# binary. LOVE_NO_GLAZE because a glazed closure is a native cell `dis` reads as a husk. A
# declined sample is a named coverage gap, not a red; a DISAGREEMENT is, and so is zero spliced.
test_splice: host
	@echo TEST test/gate/splice.l "(splice JIT: own IR -> holo -> nif -> differential)"
	@LOVE_NO_GLAZE=1 $m -l test/gate/splice.l < /dev/null
# test_ccarm64 / test_ccriscv -- the battery on a CROSS TARGET (two targets, one procedure
# in ccarch.sh): every test/cc/*.c built by `mooncc -t <arch>`, run under qemu-user, required
# to answer what x64 answers. The three programs no cross lane can build must REFUSE, not skip.
test_ccarm64: host out/host$(hsuf)/mooncc out/host$(hsuf)/mooncc.image
	@sh test/gate/ccarch.sh arm64 $(ho) $m
test_ccriscv: host out/host$(hsuf)/mooncc out/host$(hsuf)/mooncc.image
	@sh test/gate/ccarch.sh riscv64 $(ho) $m
# test_cts -- an OUTSIDE corpus: c-testsuite's 220 single-file programs, each held to the
# stdout the corpus itself ships, on all three targets. Every file in test/cc/ was written
# here to pin a fault we had already met; these were not, and the first run found nine
# programs mooncc compiles clean and answers wrong. The failures are ROSTERED with a cause
# apiece in cts.sh, refusals and wrong answers kept apart. Opt-in on an imported tree
# (`make dl/c-testsuite`), skips whole without it.
test_cts: host out/host$(hsuf)/mooncc out/host$(hsuf)/mooncc.image
	@sh test/gate/cts.sh x64 $(ho) $m
test_cts_arm64: host out/host$(hsuf)/mooncc out/host$(hsuf)/mooncc.image
	@sh test/gate/cts.sh arm64 $(ho) $m
test_cts_riscv: host out/host$(hsuf)/mooncc out/host$(hsuf)/mooncc.image
	@sh test/gate/cts.sh riscv64 $(ho) $m
# the corpus itself -- 220 files, cloned once and kept in dl/ like limine, so `make clean`
# leaves it and only `make distclean` asks the network again. NOTHING depends on this rule:
# a gate that downloads is a gate that fails on a train.
$(dl)/c-testsuite:
	@echo MK	c-testsuite
	@git clone --depth=1 https://github.com/c-testsuite/c-testsuite.git $@ > /dev/null 2>&1
# test_libc -- OUR C LIBRARY against the system's, function by function:
# test/libc/*.c built by mooncc (pulling crew/moon/lib/nolibc.c by need) and by gcc, run,
# and the two OUTPUTS compared, so a drift names the function and the case.
test_libc: host out/host$(hsuf)/mooncc out/host$(hsuf)/mooncc.image
	@sh test/gate/libc.sh $(ho) $m
# test_ulp -- THE MATH FLOOR, built by both compilers and required to agree. `make ulp`
# measures am.c's accuracy for the $(CC) build alone, which asks whether the algorithm is
# right, never whether OUR compiler builds it -- and float BITS are where codegen hides.
test_ulp: host out/host$(hsuf)/mooncc out/host$(hsuf)/mooncc.image
	@sh test/gate/ulp.sh $(ho) $m
# The rung-2 self-host gate: compile core/love.c AND every host/*.c with mooncc (gcc/clang only
# LINKS), then run the whole corpus through the all-mooncc binary -- the compiler compiles
# the runtime it runs on. OPT-IN; x86-64 only; the binary carries no image, so a fresh egg.
test_selfhost: host out/host$(hsuf)/mooncc
	@echo TEST $(ho)/love-selfhost
	@if [ "`uname -m`" != x86_64 ]; then echo "test_selfhost: x86-64 only, skipped on `uname -m`"; exit 0; fi; \
	  d=$(ho)/selfhost; mkdir -p $$d; rm -f $$d/*.o; \
	  $(ho)/mooncc -D ai_tco=$(tco) -I$(ho) -I. -Icore -Iout/lib -c core/love.c $$d/love.o \
	    || { echo "FAIL mooncc -c core/love.c"; exit 1; }; \
	  for f in host/*.c; do b=`basename $$f .c`; \
	    $(ho)/mooncc -D ai_tco=$(tco) -I$(ho) -I. -Icore -Iout/lib -c $$f $$d/$$b.o \
	      || { echo "FAIL mooncc -c $$f"; exit 1; }; done; \
	  $(ho)/mooncc -Icrew/moon/include -c crew/moon/lib/math/am.c $$d/am.o \
	    || { echo "FAIL mooncc -c am.c"; exit 1; }; \
	  $(host_cc) -static -o $(ho)/love-selfhost $$d/*.o $(host_ldflags) \
	    || { echo "FAIL link all-mooncc binary"; exit 1; }; \
	  cat $t > $(ho)/.selfhost-corpus.l; \
	  LOVE_NO_IMAGE=1 $(ho)/love-selfhost $(ho)/.selfhost-corpus.l </dev/null > $(ho)/.test_selfhost.out 2>&1; s=$$?; \
	  tail -1 $(ho)/.test_selfhost.out; \
	  { [ $$s -eq 0 ] && grep -q "tests pass" $(ho)/.test_selfhost.out; } \
	    || { echo "FAIL all-mooncc corpus (exit $$s)"; exit 1; }; \
	  echo "test_selfhost: core/love.c + all `ls host/*.c | wc -l` host/*.c built by mooncc, corpus passes"
# The rung-4 gate: the GCC-FREE fixpoint. Everything test_selfhost builds PLUS our own raw
# libc (nolibc.c), math floor (am.c) and sys.o, bound by OUR OWN static linker -- no gcc,
# no glibc, no ld anywhere. In test_slow, x86-64 only; supersedes test_selfhost.
test_raw: host out/host$(hsuf)/mooncc
	@sh test/gate/raw.sh x64 $(ho) $m $t
# test_hdiff -- the FOREIGN-CC differential at the host (KCC's twin one level up). gcc and
# clang each link the whole vm at ai_tco=1, which the default mooncc lane never does, and
# each must build, answer, pass the quick host suite and come out ret-free. NOT the corpus
# twice over: semantics are the interpreter's, and they do not move with the compiler.
test_hdiff: host
	@echo TEST test/gate/hdiff.sh
	@sh test/gate/hdiff.sh gcc clang
# the cc-DRIVER conventions (the `CC=mooncc` door's floor): the REAL $(ai_cflags) soup
# rides through -c, a link owing libc symbols pulls the runtime by need, and the loud edges
# stay loud (-shared usage-refuses, -nostdlib names its undefined references). In test_slow.
test_drv: host out/host$(hsuf)/mooncc
	@sh test/gate/drv.sh $(ho) $(ai_cflags)
# the kernel's inline-asm SEAM: free/<a>/asmops.h says every
# privileged instruction twice -- holo's neutral template for mooncc, GNU's for clang -- so
# the gate compiles one probe with both and compares op by op. Skips without llvm-objdump.
test_asmops: host out/host$(hsuf)/mooncc
	@sh test/gate/asmops.sh $(ho)
# test_vec -- the INTERRUPT gate: raises a real CPU exception with (fault n) and reads the
# report, the only way to reach free/mkvec.l's 32 stubs and the fault vector, then
# checks the stubs no boot can reach against the architecture's own error-code list.
test_vec: host
	@$(MAKE) -s a=x86_64 kernel
	@sh test/gate/vec.sh x86_64 out/free/love-x86_64.elf out/free/x86_64/free/x86_64/vec.o
	@$(MAKE) -s a=aarch64 kernel
	@sh test/gate/vec.sh aarch64 out/free/love-aarch64.elf out/free/aarch64/free/aarch64/vec.o
# THE FIXPOINT: the default love IS mooncc-built, so this gate has it rebuild ITSELF --
# love1 (love0's lane, relinked) bakes its own compiler image, recompiles every TU, links
# love2, and the two must be byte-identical. A headline invariant -- but it runs in
# test_extra only, so a deleted host/*.c goes green through test_slow either way.
# $(moon_o) is the link list: the gate is handed make's objects, it never globs the odir.
test_fixpoint: host $(love0) out/host/mooncc0.image
	@sh test/gate/fixpoint.sh $(ho) $(love0) $(moon_fir) $(moon_o)
# test_raw_bake -- the mooncc-PIE binary bakes its own image and wakes it. The procedure
# (and the why) lives in test/gate/raw-bake.sh; make keeps the dependency and the file list,
# the WHOLE corpus. Opt-in: needs the -pie toolchain, x86-64 only.
test_raw_bake: test_raw
	@sh test/gate/raw-bake.sh $(ho) $t
# test_riscv -- the test/cc battery `mooncc -t riscv64` under qemu-riscv64, exit code
# against the native x64 build. OUT of test_slow: test_ccriscv runs the same battery and
# compares STDOUT, so this is its strict subset -- the lighter opt-in lane.
test_riscv: host out/host$(hsuf)/mooncc out/host$(hsuf)/mooncc.image
	@sh test/gate/riscv.sh $(ho) $m
# test_raw's riscv64 twin: mooncc -t riscv64 lays every object, mksys-riscv the syscall
# leaf, OUR linker binds, qemu-riscv64 runs the whole corpus over the fresh egg. The riscv
# backend loads into the sealed holo module at runtime for mksys. Opt-in; skips w/o qemu.
test_raw_riscv: host out/host$(hsuf)/mooncc out/lib/riscv.h
	@sh test/gate/raw.sh riscv64 $(ho) $m $t
# test_raw's aarch64 twin: mooncc -t arm64 lays every object, mksys-arm64 the syscall leaf,
# OUR linker binds, qemu-user runs the WHOLE C-sorted $t over the fresh egg. ⚠ $t must stay
# in C/byte order: test/uu.l defines the kernel test/uukindlaw.l calls. Opt-in; needs qemu.
test_raw_arm64: host out/host$(hsuf)/mooncc
	@sh test/gate/raw.sh arm64 $(ho) $m $t
# test_thumb1 -- the ELF32/EM_ARM object writer (crew/holo/obj.l objsecs32) end to end and the
# 32-bit data model: a cross-object BL, the inline v6-M soft divide/rem, a global via the
# literal-pool `la`, a gcc-built pointer-bearing struct mooncc reads a field back from, and a
# NAMED SECTION holding a function-pointer table -- the vector-table shape, thumb bit and all.
# Then the other direction: test/gate/ld32.l reads an object back through link.l's ld-read,
# the only exercise the 32-bit rows of its field table get.
test_thumb1: host out/host$(hsuf)/mooncc
	@sh test/gate/thumb.sh thumb1 $(ho)
# test_thumb2 -- the thumb1 gate's ARMv7E-M twin, ON THE DEVICE CPU (qemu mps2-an500 is a
# Cortex-M7, the Teensy 4.1 / Playdate silicon). the featured lane is `la`, thumb2's
# MOVW/MOVT pair: every binding shape rides once -- global fn, static fn, literal, var.
test_thumb2: host out/host$(hsuf)/mooncc
	@sh test/gate/thumb.sh thumb2 $(ho)
# test_virt -- LOVE ITSELF on the bare riscv64 hart: the whole runtime compiled end to end
# by mooncc -t riscv64 (port/virt/), start.o laid from holo IR, OUR linker binds -- no
# foreign toolchain ANYWHERE. Bakes the egg, asserts, exits 42; 98 = a machine trap.
test_virt: host out/host$(hsuf)/mooncc
	@sh test/gate/boot.sh virt "$(MAKE)"
# test_mps2 -- LOVE ITSELF on the M7: the whole runtime by mooncc -t thumb2 (port/mps2/),
# start.o laid from holo IR, ldbare32 binding one RWX segment at 0 -- no foreign toolchain
# ANYWHERE, the second port after virt to reach that. On qemu's Cortex-M7 it bakes the egg
# FROM SOURCE and asserts spec laws over the hatched image; exits 42, and 98 = fault.
test_mps2: host out/host$(hsuf)/mooncc
	@sh test/gate/boot.sh mps2 "$(MAKE)"
# test_mps2_t1 -- LOVE ON THE RP2040'S ISA: the same port by mooncc -t thumb1 (ARMv6-M,
# ai_tco=0's trampoline, soft floats through libgcc's v6-m __aeabi set). v6-M is a strict
# subset of ARMv7E-M, so qemu's M7 runs it natively; exit 42 = hatched + laws held. Our linker
# binds this one too -- libgcc.a is named on the line and its members pulled by need through
# the ranlib index, so the .a is a LIBRARY the link READS, not a tool it runs.
test_mps2_t1: host out/host$(hsuf)/mooncc
	@sh test/gate/boot.sh mps2_t1 "$(MAKE)"
# test_mps2_wake -- the IMAGE lane: the baker bakes the corpus on qemu's M7 and dumps a
# fully-symbolic heap image; the WAKER -- a different binary, arena deliberately offset --
# wakes it and re-runs the driver laws. The teensy's build rides the same love.img.
test_mps2_wake: host out/host$(hsuf)/mooncc
	@sh test/gate/boot.sh mps2_wake "$(MAKE)"
# test_thumb2sp -- the SP-only-FPU face (the playdate's STM32F746): f64 arithmetic SOFTENS
# to __aeabi_* libgcc calls while the 64-bit transfers keep the d-reg value model. Gated on
# qemu's mps2-an386, whose FPv4-SP FPU FAULTS on any f64 arithmetic that slipped through.
test_thumb2sp: host out/host$(hsuf)/mooncc
	@sh test/gate/thumb.sh thumb2sp $(ho)
# test_playdate -- the playdate build gate: the device half compiled by mooncc -t thumb2sp
# behind pdglue's word-only SDK seam, the pdx built by pdc. Verifies the DEVICE elf: no UND,
# eventHandler exported, ZERO movw/movt relocs -- the loader relocates ABS32 words only.
test_playdate: host out/host$(hsuf)/mooncc
	@echo TEST out/playdate/love.pdx
	@if [ -z "$$PLAYDATE_SDK_PATH" ] || ! command -v arm-none-eabi-gcc >/dev/null 2>&1; then \
	   echo "test_playdate: no PLAYDATE_SDK_PATH / arm-none-eabi toolchain, skipped"; exit 0; fi; \
	  $(MAKE) -C port/playdate || { echo "FAIL playdate build"; exit 1; }; \
	  u=`llvm-readelf -s out/playdate/pdex.elf | grep -c "UND [a-zA-Z_]"`; \
	  [ "$$u" -eq 0 ] || { echo "FAIL pdex.elf has $$u undefined symbols"; exit 1; }; \
	  llvm-readelf -s out/playdate/pdex.elf | grep -qw eventHandler || { echo "FAIL no eventHandler"; exit 1; }; \
	  m=`llvm-readelf -r out/playdate/pdex.elf | grep -c "MOVW\|MOVT"`; \
	  [ "$$m" -eq 0 ] || { echo "FAIL $$m movw/movt relocs (the loader can't relocate them)"; exit 1; }; \
	  echo "test_playdate: love.pdx (device half all-mooncc -t thumb2sp, soft f64) -- resolved, word-relocs only"
# test_teensy41 -- the REAL-METAL build gate, and the one port asking for NO foreign tool at
# all: mooncc -t thumb2 compiles, tlink.l binds (no ld, no linker script -- the XIP flash map
# is the map in that file), mkimg.l wraps the baked heap image, ocopy.l writes the .hex/.bin,
# and the ROM-facing boot image is VERIFIED out of that .bin (FCFB tag at flash 0, IVT at
# 0x1000, thumb-bit entry). So this one never skips; test_mps2 is the runtime (no RT1062 qemu).
test_teensy41: host out/host$(hsuf)/mooncc
	@echo TEST out/teensy41/love.hex
	@$(MAKE) -C port/teensy41 || { echo "FAIL teensy41 build (the boot-image verify is inside)"; exit 1; }
	@echo "test_teensy41: love (all-mooncc thumb2), OUR linker, flatten and boot image -- nothing foreign"
# test_nucleo446 -- the Nucleo-F446RE firmware BUILD gate: mooncc -t thumb2sp compiles,
# nlink.l binds (no ld, no linker script -- the F4's memory map is the map in that file),
# ocopy.l flattens, and the boot image is VERIFIED (initial SP inside SRAM, thumb-bit reset
# entry inside flash). arm-none-eabi-gcc is still asked where its cortex-m4 hard-float
# libgcc.a lives, but the .a is READ as an archive, by need, not run. The 128 KB SRAM never
# held love: this port is the TOOLCHAIN on silicon, its arithmetic gated by test_thumb2sp
# and its boot by test_nucleo446_smoke below.
test_nucleo446: host out/host$(hsuf)/mooncc
	@echo TEST out/nucleo446/firm.hex
	@if ! command -v arm-none-eabi-gcc >/dev/null 2>&1; then \
	   echo "test_nucleo446: no arm-none-eabi toolchain, skipped"; exit 0; fi; \
	  $(MAKE) -C port/nucleo446 || { echo "FAIL nucleo446 build (the boot-image verify is inside)"; exit 1; }; \
	  echo "test_nucleo446: firmware (all-mooncc thumb2sp), OUR linker and flatten, no linker script, boot image verified"
# test_nucleo446_smoke -- the same port RUN, not read: the -D QSMOKE twin on qemu's Cortex-M4,
# its exit code the self-check tally carried out through mkboot.l's sh_exit. The only lane that
# executes crt0, the semihosting block and the fault vectors. ~0.7s.
# ⚠ qemu only -- on silicon a bkpt with no debugger escalates to lockup.
test_nucleo446_smoke: host out/host$(hsuf)/mooncc
	@sh test/gate/boot.sh nucleo446_smoke "$(MAKE)"
# test_rp2040 -- the Pico firmware BUILD gate, nucleo446-shaped, and the one port with NO .S:
# vector table and crt0 are C, and boot2 -- the 256-byte stage the mask ROM checksums before
# it runs anything -- is laid straight into a named section by mkboot2.l. So the boot image
# verify has THREE words, not two: boot2's CRC-32/MPEG-2 must be 0x7a4eb274, the SP inside the
# 264 KB SRAM, the reset entry thumb-bit and inside flash. rlink.l binds, ocopy.l flattens; the
# skip asks after the last foreign thing here, gcc's cortex-m0 libgcc, READ as an archive.
# qemu has no RP2040 machine, so this builds and never boots -- test_thumb1 gates the ISA.
test_rp2040: host out/host$(hsuf)/mooncc
	@echo TEST out/rp2040/love.bin
	@if ! command -v arm-none-eabi-gcc >/dev/null 2>&1; then \
	   echo "test_rp2040: no arm-none-eabi toolchain, skipped"; exit 0; fi; \
	  $(MAKE) -C port/rp2040 || { echo "FAIL rp2040 build (the boot-image verify is inside)"; exit 1; }; \
	  echo "test_rp2040: firmware (all-mooncc thumb1, boot2 laid by holo, no .S), OUR linker and flatten, flash R|X, boot surface verified"
# the userland packages: each built by mooncc + nolibc + the holo
# linker -- no gcc/glibc/ld anywhere -- then RUN and held to the package's own answers:
# tar 1.13 cf/xf + czf/xzf roundtrips and system-tar interop, m4 1.4's own 57-check suite,
# lua 5.4's interpreter battery, sqlite's amalgamation + VFS battery. Opt-in: point the
# package's SRC var at a prepared tree (./configure'd for tar/m4, extracted for lua/sqlite)
# and each script SKIPS cleanly without one. A cross lane puts a qemu wrapper on PATH, so a
# suite that execs the binary by name runs unmodified; riscv routes around faults the other
# two share (nhome = 0, so nothing rides), which is why it is not redundant.
# /warn the sqlite cross lanes wait on moon-sqlite: they read its x86-64 answers as the oracle.
moon_arch_arm64 = arm64
moon_arch_riscv = riscv64
# $1 package, $2 its source-tree var, $3 what the two CROSS lanes wait on
define moon_pkg
moon-$1: host out/host$$(hsuf)/mooncc
moon-$1-arm64 moon-$1-riscv: $3
moon-$1 moon-$1-arm64 moon-$1-riscv:
	@$2="$$($2)" ./mk/tools/moon-$1.sh $$(moon_arch_$$(patsubst moon-$1-%,%,$$@))
endef
$(eval $(call moon_pkg,tar,TARSRC,host out/host$(hsuf)/mooncc))
$(eval $(call moon_pkg,m4,M4SRC,host out/host$(hsuf)/mooncc))
$(eval $(call moon_pkg,lua,LUASRC,host out/host$(hsuf)/mooncc))
$(eval $(call moon_pkg,sqlite,SQLSRC,moon-sqlite))
$(eval $(call moon_pkg,gzip,GZIPSRC,host out/host$(hsuf)/mooncc))
$(eval $(call moon_pkg,bzip2,BZIP2SRC,host out/host$(hsuf)/mooncc))
# test_distboot -- THE RELEASE CLAIM: take either artifact, type make, get the same
# binary. SOURCE bootstraps through the machine's own cc; SEED carries its source in
# .rodata, lays it with `love source`, and builds with cc/gcc/clang shadowed by scripts
# that fail loudly -- so "the bundled love was the compiler" is proved, not assumed.
# Then the circle: the seed rebuilds ITSELF, byte for byte. Minutes, opt-in, by name --
# and the reason the claim can hold at all is that the local cc builds love0 and
# nothing else (see the script).
# test_bakerep -- A BAKE IS A FUNCTION OF THE TREE. Seconds, and it rides the slow gate
# because test_distboot proves the same law over the whole circle but is opt-in and
# minutes long; a regression would otherwise wait for a release to surface.
test_bakerep: host
	@echo TEST test/gate/bakerep.sh
	@sh test/gate/bakerep.sh $(ho)
test_distboot: dist
	@echo TEST test/gate/distboot.sh
	@sh test/gate/distboot.sh $(dist_source) $(dist_seed) $(ho)/love
# test_gz -- lib/tar.l + lib/gz.l against the two programs they replace. The LAWS are
# test/host/gz.l (in test_hostnif, needing nothing outside the tree); this is the half
# only the outside world can say, and it is a separate gate because a coder and a
# decoder written by one hand round-trip cleanly through a format nobody else speaks.
# Skips where either system tool is missing.
test_gz: host
	@echo TEST test/gate/targz.sh
	@sh test/gate/targz.sh $(ho)/love
# The neutral assembler (crew/holo/) + its x86-64 backend: every encoder golden is
# objdump-checked (test/holo/golden.l). A host-only app -- it adds no nif and is NOT
# baked into love0. The sources are cat'd in because the host bakes its NATIVE backend
# only; the other four reach the gate no other way. Gate = exit 0 AND the "N passed,
# 0 failed" sentinel.
test_holo: host
	@echo TEST test/holo/golden.l
	@cat crew/holo/holo.l crew/holo/x64.l crew/holo/arm64.l crew/holo/thumb2.l \
	    crew/holo/riscv.l crew/holo/thumb1.l crew/holo/text.l crew/holo/elf.l \
	    test/holo/golden.l | sh test/gate/run.sh holo "$(mw)" ", 0 failed"
# as.l -- the real AT&T x86-64 front over holo. test/holo/as.l's goldens are byte-identical
# to /usr/bin/as (frozen, no shell-out at gate time). Same sentinel gate as test_holo.
test_as: host
	@echo TEST test/holo/as.l
	@cat crew/holo/holo.l crew/holo/x64.l crew/holo/as.l test/holo/as.l \
	  | sh test/gate/run.sh as "$(mw)" ", 0 failed"
# test_elf32 -- holo's ELF32 executable writer, judged by a real loader: both thumb backends
# lay write+exit, Linux maps the segment and enters in Thumb state, and 42 must come back.
# test/holo/golden.l pins the header fields; this pins the only opinion that counts. Needs qemu-arm
# and NOTHING else -- no as, no ld, no arm-none-eabi -- so it runs where the thumb gates skip.
test_elf32: host
	@sh test/gate/elf32.sh $(ho)
# test_objcopy -- crew/holo/copy.l, the flatten, against the objcopy it replaces: a BYTE
# comparison of both output formats over fixtures our own linker mints plus every ELF on
# hand. Intel HEX is a wire (a Teensy loader reads it), so nothing softer would do. The
# gate skips where no objcopy exists -- see the script for what the fixtures are for.
test_objcopy: host
	@sh test/gate/objcopy.sh $(ho)
# ain's two-process loopback gate: a server and a client over real TCP on 127.0.0.1,
# full-duplex, each asserting it got what the other sent. The ONLY net gate driving the real
# `love mk/tools/ain.l` cli path. In test_slow; override the port with `make nettest PORT=N`.
PORT ?= 7390
nettest: host
	@echo TEST $m "(127.0.0.1:$(PORT))"
	@sh $R/test/net/loopback.sh $m $(PORT)
# Validate the l tool rewrites against their frozen Python references in mk/tools/py/
# (gen_data / vmret). See mk/tools/Makefile + mk/tools/py/README.md. ⚠ lush is a real
# prerequisite: test/host/cook.l's SHELL pair sets `SHELL := out/host/lush` to prove cook honors it.
test_tools: host out/host$(hsuf)/lush
	@$(MAKE) -C tools
# test_gcheck: the copy loop's FIXPOINT instance check. AI_GC_CHECK makes gen_minor re-drive
# its WHOLE scan after the drain and trap if the second pass copies a word, in its own tree.
# /warn the knob is GCDBG: EXTRA_CFLAGS rides $(ai_cflags), which the mooncc recipes do not use.
test_gcheck: host
	@$(MAKE) --no-print-directory hsuf=/gck GCDBG=-DAI_GC_CHECK test_host
# test_gcstress: the MUTATOR's side -- whether the C around the collector holds a raw pointer
# across a call that collects. AI_GC_STRESS always collects, poisons the vacated nursery, and
# majors every 32nd. ~4 min, own tree.
test_gcstress: host
	@$(MAKE) --no-print-directory hsuf=/gcs GCDBG=-DAI_GC_STRESS test_host

# --- the machine-checked half: test/proof/rocq/ + test/proof/lean/ ---------------------------------
# Each gate below is a no-op that SAYS SO when its checker is missing, so a bare box stays
# green. The tool guards are grouped by what they need, not by gate.
COQC     ?= $(shell command -v coqc 2>/dev/null)
OCAMLOPT ?= $(shell command -v ocamlopt 2>/dev/null)
LEAN     ?= $(shell command -v lean 2>/dev/null)
PYTHON3  ?= $(shell command -v python3 2>/dev/null)
# the scratch coqc strews beside a .v, and what an extracted ocaml ref leaves
vclean = rm -f $(foreach n,$1,test/proof/rocq/$n.vo test/proof/rocq/$n.vok test/proof/rocq/$n.vos test/proof/rocq/$n.glob test/proof/rocq/.$n.aux)
dclean = rm -f $(foreach n,$1,test/proof/rocq/$n_ref.ml test/proof/rocq/$n_ref.mli test/proof/rocq/$n_drive)

ifeq ($(COQC),)
test_proof test_gc test_gen test_uugen test_mx:
	@echo "$@: skipped (needs rocq/coqc)"
else
# spec.v -- love's headline laws (the numeral / function / absence core of test/spec.l) as Rocq
# theorems, axiom-free and universe-checked: the executable spec upgraded from SHOWN to PROVED.
# spec.vo is a FILE target, compiled once and KEPT: test_gen and test_extract both `Require
# Import spec`. /warn one spelling everywhere (`cd test/proof/rocq && -R . ""`), or spec.vo's logical
# name is not the one gen.v asks for. A static pattern: big/mx/enc take their own flags.
rocq_kept = test/proof/rocq/spec.vo test/proof/rocq/patch.vo
$(rocq_kept): test/proof/rocq/%.vo: test/proof/rocq/%.v
	@echo TEST test/proof/rocq/$*.v "(coqc)"
	@cd test/proof/rocq && $(COQC) -q -R . "" $*.v
test_proof: $(rocq_kept)
	@echo "test_proof: spec.v + patch.v check (the .vo IS the evidence, so a re-run is quiet)"
# gc.v -- the generational MINOR is SOUND (under a complete write barrier no live young is
# lost), its PAUSE is bounded by the nursery, and the Cheney drain terminates as a true
# fixpoint. Axiom-free; test_gcheck instance-checks the drain.
test_gc:
	@echo TEST test/proof/rocq/gc.v "(coqc)"
	@$(COQC) -q test/proof/rocq/gc.v
	@$(call vclean,gc)
# The .l -> .v pipeline: mk/tools/spec2coq.l reads test/spec.l and EMITS gen.v, the spec generating
# theorems for its own numeral facts. Regenerated every run, so asserts and proofs cannot diverge.
test_gen: host $(rocq_kept)
	@echo LOVE	test/proof/rocq/gen.v "(mk/tools/spec2coq.l on $m)"
	@$(mw) mk/tools/spec2coq.l > test/proof/rocq/gen.v
	@echo TEST test/proof/rocq/gen.v "(coqc, against spec.v's shared model)"
	@cd test/proof/rocq && $(COQC) -R . "" gen.v
	@$(call vclean,gen)
# The PROOF half of that pipeline (cf. test_gen, which exports concrete ASSERTS): mk/tools/uu2coq.l
# has uu's kernel TYPE-CHECK a proof term and emits the same term in Gallina for coqc to re-check
# -- a law proved in love's own kernel and certified by Rocq.
test_uugen: host
	@echo LOVE	test/proof/rocq/uugen.v "(mk/tools/uu2coq.l on $m)"
	@$(mw) mk/tools/uu2coq.l > test/proof/rocq/uugen.v
	@echo TEST test/proof/rocq/uugen.v "(coqc)"
	@$(COQC) -q test/proof/rocq/uugen.v
	@$(call vclean,uugen)
# core/mx.l IS the +/* dispatch matrices; core/mx.h is laid from it through clay and mk/tools/mx2coq.l models
# it in Rocq -- two derivations of ONE datum.
test_mx: host
	@echo TEST test/proof/rocq/mx.v "(the dispatch matrices: band factorization + dispatch commutativity, coqc)"
	@cat core/mx.l mk/tools/mx2coq.l | $(mw) > test/proof/rocq/mx.v
	@cd test/proof/rocq && $(COQC) -q mx.v >/dev/null
	@$(call vclean,mx)
endif

ifeq ($(and $(COQC),$(OCAMLOPT)),)
test_extract test_big test_encver:
	@echo "$@: skipped (needs coqc + ocamlopt)"
else
# extract.v's normalizer (on spec.v's PROVEN subst/shift) extracted to OCaml and fuzzed against
# ev. /warn RUN THE ORACLE ONCE, into a file (2>&1 too): grep it, then cat it -- a second run
# to display doubles the work.
test_extract: host $(rocq_kept)
	@echo TEST test/proof/rocq/extract.v "(coqc extraction -> ocaml ref vs ev)"
	@cd test/proof/rocq && $(COQC) -R . "" extract.v >/dev/null \
	  && rm -f normalizer.mli && $(OCAMLOPT) -w -a normalizer.ml oracle_drive.ml -o oracle_drive
	@test/proof/rocq/oracle_drive 2000 6 1 > out/.extract_oracle.l
	@$m out/.extract_oracle.l > out/.extract_oracle.out 2>&1; r=$$?; \
	  { [ $$r -eq 0 ] && grep -q "2000 / 2000 PASS" out/.extract_oracle.out; } \
	    || { echo "FAIL the extract oracle (exit $$r):"; cat out/.extract_oracle.out; exit 1; }
	@cat out/.extract_oracle.out
	@$(call vclean,extract)
	@rm -f test/proof/rocq/normalizer.ml test/proof/rocq/normalizer.mli test/proof/rocq/oracle_drive \
	  test/proof/rocq/*.cmi test/proof/rocq/*.cmx test/proof/rocq/*.o out/.extract_oracle.l out/.extract_oracle.out
# big.v proves the decimal codec roundtrip and the quot-rem/gcd witnesses, then extracts stdlib's
# binary Z with the codec; big_drive.ml emits decimal comparisons -- love's READER, limbs and
# PRINTER against it.
test_big: host
	@echo TEST test/proof/rocq/big.v "(coqc codec proof + extracted Z ref vs the limb lane)"
	@cd test/proof/rocq && $(COQC) -q big.v >/dev/null \
	  && rm -f bigref.mli && $(OCAMLOPT) -w -a bigref.ml big_drive.ml -o big_drive
	@test/proof/rocq/big_drive 2000 1 > out/.big_oracle.l
	@$m out/.big_oracle.l > out/.big_oracle.out 2>&1; r=$$?; \
	  { [ $$r -eq 0 ] && grep -q "2000 / 2000 PASS" out/.big_oracle.out; } \
	    || { echo "FAIL the big oracle (exit $$r):"; cat out/.big_oracle.out; exit 1; }
	@cat out/.big_oracle.out
	@$(call vclean,big)
	@rm -f test/proof/rocq/bigref.ml test/proof/rocq/bigref.mli test/proof/rocq/big_drive \
	  test/proof/rocq/*.cmi test/proof/rocq/*.cmx test/proof/rocq/*.o out/.big_oracle.l out/.big_oracle.out
# the PROVE rung of the holo encoder ladder: reference x86-64 encoders each proving decode
# inverts encode, extracted to OCaml and checked BYTE-IDENTICAL against holo -- an oracle, not a
# disassembler. enc.v is reg-direct, encmem.v ModRM/SIB, encli.v `li`'s form choice.
encver = enc:1792:reg-direct encmem:6144:memory encli:320:immediate
test_encver: host
	@echo TEST test/proof/rocq/enc.v test/proof/rocq/encmem.v test/proof/rocq/encli.v "(coqc round-trip proofs -> ocaml refs vs holo, byte-exact)"
	@cd test/proof/rocq && $(COQC) -q enc.v >/dev/null && $(COQC) -q encmem.v >/dev/null && $(COQC) -q encli.v >/dev/null \
	  && rm -f enc_ref.mli encmem_ref.mli encli_ref.mli \
	  && $(OCAMLOPT) -w -a enc_ref.ml enc_drive.ml -o enc_drive >/dev/null \
	  && $(OCAMLOPT) -w -a encmem_ref.ml encmem_drive.ml -o encmem_drive >/dev/null \
	  && $(OCAMLOPT) -w -a encli_ref.ml encli_drive.ml -o encli_drive >/dev/null
	@for s in $(encver); do n=$${s%%:*}; r=$${s#*:}; c=$${r%%:*}; l=$${r#*:}; \
	   o=out/.$${n}_oracle; \
	   test/proof/rocq/$${n}_drive > $$o.l; \
	   cat crew/holo/holo.l crew/holo/x64.l $$o.l | $m > $$o.out 2>&1; r=$$?; \
	   { [ $$r -eq 0 ] && grep -q "$$c / $$c PASS" $$o.out; } \
	     || { echo "FAIL the $$n oracle, $$l (exit $$r):"; cat $$o.out; exit 1; }; \
	   cat $$o.out; done
	@$(call vclean,enc encmem encli)
	@$(call dclean,enc encmem encli)
	@rm -f test/proof/rocq/*.cmi test/proof/rocq/*.cmx test/proof/rocq/*.o out/.enc_oracle.* out/.encmem_oracle.* out/.encli_oracle.*
endif

# the LEAN leg of the proof bridge (cf. test_uugen, the Rocq leg): mk/tools/uu2lean.l emits the SAME
# uu corpus to Lean 4, which re-checks it -- a SECOND independent kernel, so each law is agreed
# by two unrelated implementations. Regenerated every run.
ifeq ($(LEAN),)
test_uulean:
	@echo "test_uulean: skipped (needs lean)"
else
test_uulean: host
	@mkdir -p proof/lean
	@echo LOVE	test/proof/lean/uugen.lean "(mk/tools/uu2lean.l on $m)"
	@$(mw) mk/tools/uu2lean.l > test/proof/lean/uugen.lean
	@echo TEST test/proof/lean/uugen.lean "(lean)"
	@$(LEAN) test/proof/lean/uugen.lean > out/host/.uulean.out 2>&1; r=$$?; \
	  if [ $$r -ne 0 ] || grep -q sorryAx out/host/.uulean.out; then cat out/host/.uulean.out; exit 1; fi
endif

# the fuzz-first rung of the holo encoder ladder (test/holo/fuzz/): random IR forms encoded via
# holo, disassembled (objdump for x64, llvm-mc elsewhere), decode checked against intent.
# sysdiff.py rides the same lane for the SYSTEM ops, off holo's own arm64.l tables.
holofuzz = x64:objdump:--no-llvm arm64:llvm-mc: riscv:llvm-mc:
ifeq ($(PYTHON3),)
test_holofuzz:
	@echo "test_holofuzz: skipped (needs python3)"
else
test_holofuzz: host
	@echo TEST test/holo/fuzz/fuzz.py "(holo x64+arm64+riscv encoder differential fuzz)"
	@for s in $(holofuzz); do a=$${s%%:*}; r=$${s#*:}; t=$${r%%:*}; x=$${r#*:}; \
	   if command -v $$t >/dev/null 2>&1; then \
	     $(PYTHON3) test/holo/fuzz/fuzz.py --arch $$a -n 8 --seed 20250717 $$x \
	       || { echo "FAIL holofuzz $$a -- a holo encoding disagrees with $$t"; exit 1; }; \
	   else echo "  ($$a skipped: no $$t)"; fi; done
	@if command -v llvm-mc >/dev/null 2>&1; then \
	   $(PYTHON3) test/holo/fuzz/sysdiff.py \
	     || { echo "FAIL sysdiff -- a holo SYSTEM encoding disagrees with llvm-mc"; exit 1; }; \
	 else echo "  (sysdiff skipped: no llvm-mc)"; fi
endif
# test/uuwm.l is a COMMITTED GENERATED artifact: lux's zipper ops compiled from crew/lux/core.l
# into uu terms (mk/tools/uuwmgen.l), so test/uuwmlaw.l proves its theorems OF THE IMPLEMENTATION
# at corpus time. `make uuwm` refreshes it; test_uuwm regenerates and diffs.
uuwm: host
	@echo LOVE	test/uuwm.l "(mk/tools/uuwmgen.l on $m)"
	@$(mw) mk/tools/uuwmgen.l > test/uuwm.l
test_uuwm: host
	@echo TEST test/uuwm.l "(regenerate + diff)"
	@$(mw) mk/tools/uuwmgen.l > out/host/.uuwm.l.tmp
	@cmp -s out/host/.uuwm.l.tmp test/uuwm.l \
	  || { echo "FAIL: test/uuwm.l is stale (crew/lux/core.l moved?) -- run: make uuwm"; exit 1; }
	@rm -f out/host/.uuwm.l.tmp
# test/uukind.l is a COMMITTED GENERATED artifact: doc/proto/kinds.l's abstract kinds-lattice
# JOIN compiled into uu terms (mk/tools/kinds2uu.l), so test/uukindlaw.l proves the semilattice
# laws OF THE ANALYSIS at corpus time. `make uukind` refreshes it; test_uukind diffs it.
uukind: host
	@echo LOVE	test/uukind.l "(mk/tools/kinds2uu.l on $m)"
	@$(mw) mk/tools/kinds2uu.l > test/uukind.l
test_uukind: host
	@echo TEST test/uukind.l "(regenerate + diff)"
	@$(mw) mk/tools/kinds2uu.l > out/host/.uukind.l.tmp
	@cmp -s out/host/.uukind.l.tmp test/uukind.l \
	  || { echo "FAIL: test/uukind.l is stale (doc/proto/kinds.l moved?) -- run: make uukind"; exit 1; }
	@rm -f out/host/.uukind.l.tmp
# test/uuhomgen.l is a COMMITTED GENERATED artifact: doc/proto/dest.l's two code generators
# run on its law sites, the emissions lifted to uu terms (mk/tools/dest2uu.l), so test/uuhomlaw.l
# proves the destination-die laws OF THE EMISSIONS at corpus time. `make uuhomgen` refreshes it; test_uuhomgen regenerates and diffs.
uuhomgen: host
	@echo LOVE	test/uuhomgen.l "(mk/tools/dest2uu.l on $m)"
	@$(mw) mk/tools/dest2uu.l > test/uuhomgen.l
test_uuhomgen: host
	@echo TEST test/uuhomgen.l "(regenerate + diff)"
	@$(mw) mk/tools/dest2uu.l > out/host/.uuhomgen.l.tmp
	@cmp -s out/host/.uuhomgen.l.tmp test/uuhomgen.l \
	  || { echo "FAIL: test/uuhomgen.l is stale (doc/proto/dest.l moved?) -- run: make uuhomgen"; exit 1; }
	@rm -f out/host/.uuhomgen.l.tmp
# test/uusplgen.l is a COMMITTED GENERATED artifact: doc/proto/spl.l's three call-site
# compilers (call, binding splice, substitution splice) run on its samples, the threads
# lifted to uu terms (mk/tools/spl2uu.l), so test/uuspllaw.l proves the SPLICE LICENSE of
# the emissions at corpus time.
# `make uusplgen` refreshes it; test_uusplgen regenerates and diffs.
uusplgen: host
	@echo LOVE	test/uusplgen.l "(mk/tools/spl2uu.l on $m)"
	@$(mw) mk/tools/spl2uu.l > test/uusplgen.l
test_uusplgen: host
	@echo TEST test/uusplgen.l "(regenerate + diff)"
	@$(mw) mk/tools/spl2uu.l > out/host/.uusplgen.l.tmp
	@cmp -s out/host/.uusplgen.l.tmp test/uusplgen.l \
	  || { echo "FAIL: test/uusplgen.l is stale (doc/proto/spl.l moved?) -- run: make uusplgen"; exit 1; }
	@rm -f out/host/.uusplgen.l.tmp
# test_wake: the BAKE-THEN-WAKE ROUND TRIP, which no other gate runs -- every other lane
# wakes an image some earlier recipe baked. A CANDIDATE COPY bakes (love.wake, ETXTBSY-proof)
# under a timeout the wake storm cannot meet (fresh lane ~1s, storm >90s; doc/wake-storm.md).
test_wake: $(ho)/love
	@echo TEST wake "(the woken-image lane, doc/wake-storm.md)"
	@cp $(ho)/love $(ho)/love.wake && $(ho)/love.wake bake
	@cat test/00-init.l test/uu.l > $(ho)/wake-corpus.l
	@if env -u LOVE_NO_IMAGE timeout 60 $(ho)/love.wake $(ho)/wake-corpus.l > /dev/null 2>&1; \
	  then echo "test_wake: green (the woken image checks uu at speed)"; rm -f $(ho)/love.wake $(ho)/wake-corpus.l; \
	  else echo "test_wake: FAILED -- the wake storm (doc/wake-storm.md)"; rm -f $(ho)/love.wake $(ho)/wake-corpus.l; exit 1; fi
