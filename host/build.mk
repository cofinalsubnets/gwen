# host/build.mk -- the host (POSIX CLI) build, out/host. Was host/Makefile.
#
# Fragment of the root Makefile (split out 2026-07-15). Included by ./Makefile,
# which is invoked from the project root; paths resolve from there. Shared vars
# live in common.mk. Every recipe here is unchanged from the single-file Makefile.

# ====================================================================
# host (POSIX CLI) build -- outputs under out/host. Was host/Makefile.
# ====================================================================
# The DEFAULT flavor owns out/host; the other flavor builds in its own hsuf'd
# tree (common.mk: out/host-glibc when musl is the default, out/host-musl when
# it isn't), so a musl build never overwrites glibc objects -- musl's bare
# `sigsetjmp` vs glibc's `__sigsetjmp` macro would otherwise poison a cross-libc
# relink. The .hostcc stamp below catches the IN-PLACE flips (musl-clang
# appearing/vanishing changes what out/host means). The bootstrap (love0) + the
# generated out/lib/*.h headers stay PINNED to canonical out/host paths and
# plain $(CC): love0 never goes musl.
ho = out/host$(hsuf)
h_o = $(love_c:$(R)/%.c=$(ho)/%.o)
# host/*.c: per-app host-nif files (auto-globbed, auto-registered via AI_NIF).
# Linked DIRECTLY into the binary (not via liblove.a) so the ai_nifs section is
# never archive-collected. Drop a host/<app>.c in and it builds -- no rule edit.
host_o = $(patsubst host/%.c,$(ho)/host/%.o,$(wildcard host/*.c))
# the host runs $(tco) (common.mk; default 1 = tail-threaded, vmret-checked);
# love0 below stays pinned 0, the deliberate trampoline-coverage lane.
# (-I$(ho) -Iout/lib for the generated egg/cli headers.)
# host_cc: STATIC picks musl-clang unless CC was set explicitly (the musl-gcc
# fallback below); love0 and the lib tools stay on plain $(CC) either way.
host_cc = $(if $(STATIC),$(if $(cc_user),$(CC),musl-clang),$(CC))
# ⚠ GCDBG: the GC DEBUG LANES' knob, and it is NOT $(EXTRA_CFLAGS) on purpose.
# It reaches the SHIPPED love only -- both compilers, since the default binary is
# mooncc-built -- and never love0. Two reasons, both learned the hard way:
#  * EXTRA_CFLAGS does not reach $(moon0) at all, so test_gcheck compiled love.c
#    CLEAN and ran the corpus on a binary that had never had the check in it. A
#    gate answering a question it was not asking is worse than no gate.
#  * love0 IS shared and unsuffixed (out/host/0), so a flag that reaches it leaks
#    out of the debug lane's own tree -- and a stress-built love0 segfaults baking
#    mooncc0.image, taking the whole tree down with it.
hcc = $(host_cc) $(ai_cflags) $(GCDBG) -Dai_tco=$(tco) -fpic -I$(ho) -I. -Iout/lib
# whole-archive flag differs by linker (ld64 vs GNU ld); ai_typ is now a plain
# compare in love.h, so there is no data.ld / generated data.h on any platform.
ifeq ($(shell uname -s),Darwin)
so_archive = -Wl,-force_load,$(ho)/liblove.a       # ld64's whole-archive
# the host contract (ai_clock, ai_fd_port_vt, ai_stdin/out/err -- defined in
# host/main.c, linked into `love` itself, NOT the archive) is UNRESOLVED in the .so
# by design: the loading executable provides it. GNU ld allows that by default;
# ld64 rejects undefined symbols in a dylib unless told to defer them.
so_undef = -Wl,-undefined,dynamic_lookup
else
so_archive = -Wl,--whole-archive $(ho)/liblove.a -Wl,--no-whole-archive
endif
# the boot image gets its OWN segment at the top of the address space, so `love --bake`
# can GROW it: the blob is appended at the tail of the file and the one phdr + one shdr
# naming it are rewritten, with nothing else moving (host/image.c's bake_tail). That is
# what --section-start buys -- ld gives a section at a far address a PT_LOAD to itself,
# above .bss and alone in it. 0x2000000 (32 MiB) clears .bss (~8 MiB) with room to grow
# and is page-aligned, which the loader's offset/vaddr congruence needs; an overlap is a
# LOUD ld error, never a silent one. holo lays the same shape its own way (link.l's image
# lane, riding the tail of its single segment), so both toolchains bake alike.
# The flag is GNU-ld/lld spelling. Nothing here is conditional on mach-o because NOTHING
# in this file builds there: host/image.c includes <link.h> and calls dl_iterate_phdr,
# neither of which Darwin has, and a mach-o section attribute needs `segment,section`.
# A mac lane is writable -- getsectbyname + _NSGetExecutablePath are the easy half of it,
# and `-segaddr` places a segment high -- but on Apple silicon every executable carries at
# least an ad-hoc signature over its own bytes, so a self-patching binary has to re-sign
# itself before it can exec again. That is the real wall, and it is not one a reserve
# would have got around either.
image_ldflags = -Wl,--section-start=.image=0x2000000
# STATIC=1 links a fully static `love` against musl (and skips liblove.so, which a
# static build can't produce) -- the OPT-IN portable-binary lane (was briefly
# the Linux default; demoted 2026-07-07, the why lives in common.mk's flavor
# block): the binary runs on ANY Linux distro regardless of
# glibc version AND still does DNS -- static *glibc* can't resolve hostnames
# (getaddrinfo needs NSS via dlopen, impossible when static), but musl resolves
# itself, so ain's `connect host port` works. Costs +1% size (~55K of text, the
# whole libc; the ~4M baked image dwarfs it) at the same test-corpus speed.
# musl-clang is clang (matches our clang default) + the musl libc -- the clean
# path. VALIDATED: fully static, `ldd` = not-a-dynamic-executable, runs,
# getaddrinfo baked in, full corpus green. `make STATIC=1` builds in its own
# out/host-musl tree -- no need to clean between flavors.
# musl is Linux-only -- this is the Linux portable-binary artifact, NOT the mac
# build (mac = a native Apple-clang build).
# FALLBACK: `STATIC=1 CC=musl-gcc` works too but is a gcc wrapper; on Arch its
# spec injects a phantom `-latomic_asneeded` (we use no real atomics -- only
# volatile sig_atomic_t flags), so it needs an empty stub on the link path:
#   ar rcs /tmp/libatomic_asneeded.a; make STATIC=1 CC=musl-gcc EXTRA_CFLAGS=-L/tmp
ifneq ($(STATIC),)
host_ldflags = -static
# the musl-clang wrapper injects LINK flags (-fuse-ld, -L…) into every clang call,
# incl. -c compiles, where clang warns "unused during compilation" -> our -Werror
# makes it fatal. Silence that one (harmless; gcc ignores unknown -Wno-*).
ai_cflags += -Wno-unused-command-line-argument
endif
# .hostcc -- the tree's compiler+link identity, content-stamped (cmp keeps the
# mtime when nothing changed). Every host object and the link depend on it, so
# an in-place flavor flip (musl-clang installed/removed flips what out/host
# means; an explicit CC=) rebuilds the tree instead of relinking mixed-libc
# objects (the sigsetjmp poison above -- a loud link error at best).
.PHONY: force_hostcc
force_hostcc: ;
$(ho)/.hostcc: force_hostcc
	@mkdir -p $(ho)
	@printf '%s\n' '$(host_cc) $(host_ldflags) $(image_ldflags)' > $@.tmp
	@if cmp -s $@.tmp $@ 2>/dev/null; then rm -f $@.tmp; else mv $@.tmp $@; echo SH $@; fi
host: $(ho)/love $(ho)/ai $(ho)/love.baked $(if $(STATIC),,$(ho)/liblove.so) $(ho)/love.1 $(ho)/cook.1
love0: $(love0)

# dock: launch the steering dock (port/inle/serve.l) from a stable COPY out/host/dock,
# so `adopt` can rebuild the canonical out/host/love in place without ETXTBSY (the RELINK
# writes the exe file in place; the bake itself is rename-safe). loads the full crew -- the probe ladder
# (judge), the server (serve), and the self-modify loop (drive + the model proposer patch).
# PORT overrides the mooring; bind loopback and firewall/tunnel it -- it evals what it reads.
.PHONY: dock
DOCK_PORT ?= 7620
dock: host
	@cp $(ho)/love $(ho)/dock
	exec $(ho)/dock -l port/inle/judge.l -l port/inle/serve.l -l port/inle/drive.l -l port/inle/patch.l -e "(dock $(DOCK_PORT))"
# the default BOOT IMAGE: `$< --bake` boots the freshly-linked binary, snapshots the post-warm
# heap (the glaze baked in, x86-64), and lays it back into the binary's OWN .image section --
# host/image.c copies the exe, pwrites the blob at the section's file offset, and atomically
# renames over the original (no objcopy/objdump, ETXTBSY-proof: a new inode, so anyone still
# executing keeps the old one). A plain `love` then wakes it at ~4 ms cold start (glazed by
# default) instead of eval'ing the egg (~230 ms). The load is an OPTIMIZATION -- main.c falls
# back to a normal egg boot on any mismatch, so a stale bake is never fatal, only slower.
# (~1.5 s to bake: the glaze self-tests native-compile; paid once per love rebuild, not per run.)
# The .baked STAMP carries the dependency (the bake mutates the binary itself); a static
# pattern so the CANDIDATE bakes by the same recipe at its side path.
$(ho)/love.baked $(ho)/love.cand.baked: %.baked: %
	@echo BAKE	$<
	@$< --bake
	@touch $@


# candidate: build + bake the NEXT GENERATION at the side path out/host/love.cand.
# nothing executes that name, so the in-place bake can never hit ETXTBSY -- a
# rebuild succeeds no matter who is running `love` (a repl, a test, the dock's own
# client). gate it with `make test m=$(ho)/love.cand` (m routes the whole corpus;
# love0 is independent), then promote on green with an ATOMIC RENAME (a new inode:
# executing processes keep the old one) -- the dock's `adopt` does exactly this.
# on a red gate the canonical binary is UNTOUCHED; the failed candidate dies at
# the side path like a to-space that never flips.
.PHONY: candidate
candidate: $(ho)/love.cand.baked


# rm the archive first: `ar r` REPLACES/ADDS but never REMOVES, so a renamed/dropped
# source (e.g. ai.c -> love.c) would leave a stale .o in the archive -> multiple-
# definition at link. the rm rebuilds it fresh, so a rename no longer needs `make clean`.
$(ho)/liblove.a: $(h_o)
	@echo AR	$@
	@mkdir -p $(dir $@)
	@rm -f $@; ar rcs $@ $^

$(ho)/liblove.so: $(ho)/liblove.a $(R)/love_data.ld
	@echo LD	$@
	@mkdir -p $(dir $@)
	@$(hcc) -shared -o $@ $(so_archive) $(so_undef) $(data_ld)

# Bootstrap interpreter, compiled against the fallback top-level data.h (no
# -I$(ho)) + -DGL_BOOTSTRAP -Dai_tco=0 (also exercises the non-threaded trampoline
# dispatch). Runs the l build tools that generate the lcat headers, so it can't
# depend on those; instead it #includes the sed-wrapped $(gl0_h) (cli0 + the baked
# prel/ev/egg/repl + the test corpus), all produced without an interpreter --
# hence -Iout/lib. Per-object into $(ho)/0/ so ccache caches each TU.
# love0 links the WHOLE host/*.c glob now (main.c among it): the posix nifs and
# host/image.c's bake/--wake are what let love0 bake and wake mooncc0.image and
# drive the mooncc-built default `love` (the self-host rung) with CC only here.
# -DAI_VERSION="bootstrap": love0 does NOT carry the version-control id, and that is
# the point. It bakes the lcat headers every frontend shares ($(lib_h): $(love0)), so a
# love0 that relinks re-lays all 22 of them and rebuilds every object behind them -- a
# ~25 s full-tree cascade, fired by nothing but a new commit hash. The bootstrap is not
# a release artifact and its `love-version` is read by nobody; the shipped `love` still
# carries the real id (the love.o dep below).
gl0_cc = $(CCACHE) $(CC) $(ai_cflags) -DGL_BOOTSTRAP -Dai_tco=0 -DAI_VERSION='"bootstrap"' -I. -Iout/lib
love0_host_o = $(patsubst host/%.c,out/host/0/host/%.o,$(wildcard host/*.c))
love0_o = $(love0_host_o) $(love_c:$(R)/%.c=out/host/0/%.o)   # PINNED (not $(ho)/0)
out/host/0/host/main.o: $(gl0_h)
out/host/0/host/cb.o: crew/quay/quay.c crew/quay/quay.h
# the LOVE_NO_IMAGE= prefix (empty = unset, main.c's auto-load) hands the
# compiler its baked image back under the blanket corpus export up top: when
# CC is the dist artifact's own mooncc verb (`love up`'s default -- the
# zero-ambient-toolchain door), the verb table lives in that image, and an
# egg boot would read "mooncc" as a filename. a real cc ignores the noise.
out/host/0/%.o: $(R)/%.c $(love_h)
	@echo CC	$@
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= $(gl0_cc) -c $< -o $@
# -pie is LOAD-BEARING, not hygiene: love0 bakes mooncc0.image, and the image
# codec refuses a binary whose text sits in its index range (love.c's
# img_encode_ "binary ptr below TBOUND") -- a PIE loads high and clears it.
# gcc/clang default to PIE anyway; mooncc (the download door's CC) does not.
$(love0): $(love0_o) $(R)/love_data.ld
	@echo LD	$@
	@mkdir -p $(dir $@)
	@LOVE_NO_IMAGE= $(CC) $(ai_cflags) -pie -o $@ $(love0_o) $(data_ld)

# love.c -> out/host/*.o
$(ho)/%.o: $(R)/%.c $(love_h) $(ho)/.hostcc
	@echo CC	$@
	@mkdir -p $(dir $@)
	@$(hcc) -c $< -o $@

# l.o carries the version string (love_version.h); relink it when the id changes. love0's
# twin is deliberately NOT here -- see the -DAI_VERSION note on gl0_cc.
$(ho)/love.o: out/lib/love_version.h
# host/main.o bakes the lcat lib headers inline (egg + prel/ev/cli/bao -- bao is the
# baked shell core now, subsuming the old repl.h). Now that it rides the host/*.c
# glob (compiled once, not recompiled on every link, as the old inline `$(hcc)
# main.c` did), recompile it when any baked header changes.
$(ho)/host/main.o: out/lib/egg.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/cli.h out/lib/bao.h out/lib/coin.h out/lib/rng.h out/lib/q.h out/lib/kanren.h out/lib/overlay.h out/lib/peg.h out/lib/uu.h $(holo_h) $(glaze_h)
# host/cb.c rides crew/quay/quay.c by unity include -- recompile when the engine moves.
$(ho)/host/cb.o: crew/quay/quay.c crew/quay/quay.h

# host/main.c (auto-globbed into $(host_o)) carries main() + the egg, assembled
# inline via G_EGG_PRE/POST. No separate main.c compile -- it rides the host/*.c
# glob now; the recompile-on-header-change dep is the line just above.
# one link rule, two names: `love` (canonical) and `love.cand` (the CANDIDATE -- the next
# generation built at a side path nothing executes, so the RELINK can never hit
# ETXTBSY no matter who is running `love`; see the candidate target below).
#
# ==== the DEFAULT love is MOONCC-BUILT now (self-host rung 2) ====
# test/gate/raw.sh's lane, promoted: every TU compiles under love0 waking
# mooncc0.image (mooncc + all holo backends, anchor-checked to love0), our own
# nolibc + am math + mksys sys.o replace glibc, and holo links it -pie (an
# ET_DYN loads high, clearing the image codec's index range -- what lets the
# binary self-bake, test_raw_bake's lesson). CC's remaining jobs here: love0
# and the liblove.a/.so lane (a shared object wants PIC codegen + the dynamic
# section, which holo does not lay). STATIC=1 keeps the musl-cc link below --
# the raw default is already fully static, so the flavor is legacy/opt-in.
moon0 = $(love0) --wake out/host/mooncc0.image -e '(moon-main (cuup (cup cmdline)))' $(GCDBG)
moon_d = $(ho)/moon
moon_host_o = $(patsubst host/%.c,$(moon_d)/host_%.o,$(wildcard host/*.c))
moon_math_o = $(patsubst crew/moon/lib/math/%.c,$(moon_d)/m_%.o,$(wildcard crew/moon/lib/math/*.c))
moon_o = $(moon_d)/love.o $(moon_host_o) $(moon_d)/nolibc.o $(moon_math_o) $(moon_d)/sys.o
# -D AI_HAVE_VERSION_H + the love_version.h dep: this TU carries the version id into the
# SHIPPED binary, and mooncc has no __has_include for love.c's fallback probe to use.
$(moon_d)/love.o: love.c $(love_h) out/host/mooncc0.image out/lib/love_version.h
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(moon0) -D ai_tco=$(tco) -D AI_HAVE_VERSION_H -I$(ho) -I. -Iout/lib -c $< $@
$(moon_d)/host_%.o: host/%.c $(love_h) out/host/mooncc0.image
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(moon0) -D ai_tco=$(tco) -I$(ho) -I. -Iout/lib -c $< $@
$(moon_d)/host_main.o: out/lib/egg.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/cli.h out/lib/bao.h out/lib/coin.h out/lib/rng.h out/lib/q.h out/lib/kanren.h out/lib/overlay.h out/lib/peg.h out/lib/uu.h $(holo_h) $(glaze_h)
$(moon_d)/host_cb.o: crew/quay/quay.c crew/quay/quay.h
$(moon_d)/nolibc.o: crew/moon/lib/nolibc.c out/host/mooncc0.image
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(moon0) -Icrew/moon/include -c $< $@
$(moon_d)/m_%.o: crew/moon/lib/math/%.c out/host/mooncc0.image
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(moon0) -Icrew/moon/lib/math -Icrew/moon/include -c $< $@
# sys.o is LAID, not compiled: the syscall trampoline + our sigsetjmp/longjmp
# have no C spelling (crew/moon/lib/mksys.l). love0 runs the lay -- the holo
# module is registered in its boot with EVERY backend baked, so the cross
# entries resolve natively. the entry is per-arch (mksys lays x64), picked by
# $(hosta) -- the HOST's arch, never $a: a cross lane overrides $a and this object
# is out/host's, so it is the host's or it is wrong (an aarch64 sys.o laid here
# fails the link with `link-machine`, one remove from its cause).
ifeq ($(hosta),aarch64)
mksys_e = mksys-arm64
else
mksys_e = mksys
endif
mksys_l = crew/kore/text.l crew/kore/core.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
$(ho)/.mksys-cat.l: $(mksys_l)
	@echo AI	$@
	@mkdir -p $(dir $@)
	@cat $(mksys_l) > $@
$(moon_d)/sys.o: $(ho)/.mksys-cat.l $(love0)
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(love0) -l $(ho)/.mksys-cat.l -n -e '($(mksys_e) "$@")' && test -s $@
ifneq ($(STATIC),)
$(ho)/love $(ho)/love.cand: $(host_o) $(ho)/liblove.a $(ho)/.hostcc $(R)/love_data.ld out/lib/egg.h out/lib/p1.h out/lib/prel.h out/lib/ev.h out/lib/cli.h out/lib/bao.h out/lib/coin.h out/lib/rng.h out/lib/q.h out/lib/kanren.h out/lib/overlay.h out/lib/peg.h out/lib/uu.h $(holo_h) $(glaze_h)
	@echo CC	$@
	@mkdir -p $(dir $@)
	@$(hcc) -o $@ $(host_o) $(ho)/liblove.a $(host_ldflags) $(image_ldflags) $(data_ld)
else
$(ho)/love $(ho)/love.cand: $(moon_o)
	@echo MOON	$@
	@mkdir -p $(dir $@)
	@$(moon0) -pie $(moon_o) -o $@
endif

# compat: `ai` was the name from 2026-06-15 until the reversion to `love`. The old
# name stays as a symlink so the doc/proto long tail -- and any external script that
# hardcodes out/host/ai -- keeps working without a rewrite.
$(ho)/ai: $(ho)/love
	@echo LN	$@
	@ln -sf love $@

# the man pages are WRITTEN in doc/*.md and generated here through the lapiz lens
# (tools/mkman.l): one source, so the roff cannot drift from the prose. the generated
# roff renders byte-identically to the hand-written pages it replaced.
$(ho)/love.1: doc/love.md tools/mkman.l crew/lapiz/lapiz.l out/lib/love_version.h $(ho)/love
	@echo LOVE	$@
	@mkdir -p $(dir $@)
	@v=$$(sed -n 's/.*AI_VERSION "\(.*\)"/\1/p' out/lib/love_version.h); \
	 $(ho)/love tools/mkman.l doc/love.md | sed "s/@VERSION@/$$v/" > $@

$(ho)/cook.1: doc/cook.md tools/mkman.l crew/lapiz/lapiz.l out/lib/love_version.h $(ho)/love
	@echo LOVE	$@
	@mkdir -p $(dir $@)
	@v=$$(sed -n 's/.*AI_VERSION "\(.*\)"/\1/p' out/lib/love_version.h); \
	 $(ho)/love tools/mkman.l doc/cook.md | sed "s/@VERSION@/$$v/" > $@

$(ho)/lush.1: doc/lush.md tools/mkman.l crew/lapiz/lapiz.l out/lib/love_version.h $(ho)/love
	@echo LOVE	$@
	@mkdir -p $(dir $@)
	@v=$$(sed -n 's/.*AI_VERSION "\(.*\)"/\1/p' out/lib/love_version.h); \
	 $(ho)/love tools/mkman.l doc/lush.md | sed "s/@VERSION@/$$v/" > $@

