# Shared variables for the host and kernel builds and for out-of-tree ports (the l-ports
# repo). An includer sets R to the project root first (the root Makefile sets R := .), so
# these resolve from any cwd; per-frontend output lands in $R/out/<frontend>/.
R ?= .

m = $R/out/host$(hsuf)/love
a ?= $(shell uname -m)
# ⚠ the HOST's arch, which $a is NOT: a cross lane overrides $a on the command line, and
# anything under out/host reading $a then lays a cross artifact into the host tree.
hosta := $(shell uname -m)

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

# -std spelling: clang takes `gnu23` only from ~18 (Xcode 16); older Apple clang wants the
# pre-final `gnu2x`. Probe $(CC) once and fall back, so the host builds on what ships.
ai_std := $(shell printf 'int main(void){return 0;}' | $(CC) -std=gnu23 -x c -c -o /dev/null - 2>/dev/null && echo gnu23 || echo gnu2x)

ai_cflags = -std=$(ai_std) -g -O2 -pipe $(EXTRA_CFLAGS) \
  -Wall -Wextra -Werror -Wstrict-prototypes -Wno-unused-parameter \
  -Wmissing-field-initializers -Wno-implicit-fallthrough\
  -falign-functions=16 -fomit-frame-pointer -fno-stack-check -fno-stack-protector \
  -fno-exceptions -fno-asynchronous-unwind-tables
# -fcf-protection (Intel CET) is x86-only and Apple/arm clang rejects it outright, so it
# rides every non-Darwin build and macOS does without -- it has no CET to turn off.
ifneq ($(shell uname -s),Darwin)
ai_cflags += -fcf-protection=none
# the data-sentinel tiling love.h's ai_typ reads (love.c's DSENT), on every ld/lld link.
# mach-o goes without: it names sections `segment,section`, so love.h asks them by name.
data_ld = -Wl,-T,$R/love_data.ld
endif
