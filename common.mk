# Shared variables for the host and kernel builds and for out-of-tree ports (the l-ports
# repo). An includer sets R to the project root first (the root Makefile sets R := .), so
# these resolve from any cwd; per-frontend output lands in $R/out/<frontend>/.
R ?= .

m = $R/out/host$(hsuf)/love
# ⚠ the HOST's arch, which $a is NOT: a cross lane overrides $a on the command line, and
# anything under out/host reading $a then lays a cross artifact into the host tree.
hosta := $(shell uname -m)
# ⚠ `?=` MAKES A RECURSIVE VARIABLE, so `a ?= $(shell uname -m)` re-forks uname at every
# single reference -- 203 of them before this build even reached out/lib/egg.h. Deferring
# to the simply-expanded $(hosta) keeps the override and spends one fork for the tree.
a ?= $(hosta)

# THE BASE VERSION, the checked-in ./VERSION with no VCS suffix on it -- what a release
# is called, moving only when a release does. mk/lib.mk's love_version adds the suffix,
# dist names the tarball for the whole id, and `.comment` carries THIS half alone (which
# is what lets love0's stamp agree with a real one -- see gl0_cc).
love_base := $(shell cat $R/VERSION 2>/dev/null || echo 0)

# ⚠ IS THIS TREE A CHECKOUT OR AN UNPACKED RELEASE? `git -C DIR` walks UP, so the test is for
# THIS tree's own .git and never an ancestor's (crew/build.mk learned that the hard way). Two
# things read it: dist cuts its tarball from the index only where there is one, and the DEFAULT
# GOAL differs -- a checkout wants the fast gate for its edit loop, an unpacked release wants
# the product, because whoever unpacked it came for love and not for our test binaries.
in_git := $(wildcard $R/.git)

# clang is the default host/love0 compiler. ⚠ `CC ?= clang` would be a NO-OP: make ships a
# built-in default `CC = cc` whose origin is `default`, not `undefined`, so `?=` never
# fires -- the origin test is what overrides it while still honoring `make CC=gcc`.
# cc_user marks an explicit choice, and opts out of the host block's musl-clang pick.
ifeq ($(origin CC),default)
CC = clang
else
cc_user := 1
endif

# The host binary's FLAVOR: the default is dynamic glibc (plus liblove.so, which the crew
# shares). STATIC=1 links fully static against musl, the OPT-IN portable lane -- not the
# default, because valgrind emulates x87 at 64 bits where musl's strtod leans on the full
# 80, so under memcheck every float literal misparsed ~1e-13, and a static build cannot
# produce liblove.so. STATIC gets its own out/host-musl tree so the two libcs never share
# objects, and $m follows it, so a test runs the flavor you asked for.
override STATIC := $(filter-out 0,$(STATIC))
hsuf := $(if $(STATIC),-musl,)

# ai_tco for the builds that can take it: 1 = the tail-threaded VM (aps tail-jump, never
# return -- `make vmret` verifies it per binary), 0 = the trampoline loop. The host runs
# $(tco). PINNED to 0 elsewhere: love0 and wasm (the deliberate trampoline-coverage lanes),
# and the two seats with no sibcall -- mps2's thumb1 face and the playdate simulator.
tco ?= 1

# the corpus: 00-init's harness first, the spec second, then uu.l, then the rest. ⚠ uu.l is
# front-loaded EXPLICITLY so its dependents (uukind*, uulay, uupatch, uuwm*) see it whatever
# the collation -- a locale `ls` orders uukind* first and the laws would run against an
# unloaded kernel. ⚠ glaze-x86 and glaze-hook are EXCLUDED: both EXECUTE native machine
# code, so they ride their own arch-guarded targets, never the arch-neutral corpus.
t = $R/test/00-init.l $R/test/spec.l $R/test/uu.l $(filter-out %/00-init.l %/spec.l %/glaze-x86.l %/glaze-hook.l %/uu.l,$(sort $(wildcard $R/test/*.l)))

love_h = $(wildcard $R/*.h)
# the core rides with its math floor: our own transcendentals, no libm anywhere
love_c = $R/love.c $R/crew/moon/lib/math/am.c
# the quay engine every seat carries. paint.c (32bpp) and nif.c (the love door) are
# per-seat -- a 1-bit device wants neither, the host unity-includes nif.c -- so a seat that
# wants one NAMES it rather than taking it here.
f_c = $(filter-out %/paint.c %/nif.c,$(wildcard $R/crew/quay/*.c))
c_c = $(wildcard $R/libc/*.c)

# the dialect we target, and mooncc's own aim -- doc/moon-c-gaps.md is the ledger.
ai_std := c11

ai_cflags = -std=$(ai_std) -g -O2 -pipe $(EXTRA_CFLAGS) \
  -Wall -Wextra -Werror -Wstrict-prototypes -Wno-unused-parameter \
  -Wmissing-field-initializers -Wno-implicit-fallthrough\
  -falign-functions=16 -fomit-frame-pointer -fno-stack-check -fno-stack-protector \
  -fno-exceptions -fno-asynchronous-unwind-tables
# ⚠ a strict -std sets __STRICT_ANSI__ and glibc then hides its POSIX half -- host/main.c
# owes clock_gettime and kill, so the level is asked for by name.
# -fcf-protection (Intel CET) is x86-only and Apple/arm clang rejects it outright, so it
# rides every non-Darwin build and macOS does without -- it has no CET to turn off.
ifneq ($(shell uname -s),Darwin)
ai_cflags += -fcf-protection=none -D_POSIX_C_SOURCE=200809L
# the data-sentinel tiling love.h's ai_typ reads (love.c's DSENT), on every ld/lld link.
# mach-o goes without: it names sections `segment,section`, so love.h asks them by name.
data_ld = -Wl,-T,$R/love_data.ld
else
# apple's is one word for the whole surface, so it needs no _POSIX_C_SOURCE beside it.
ai_cflags += -D_DARWIN_C_SOURCE
endif
