# Shared variables for the host and free (freestanding kernel) builds, and
# for out-of-tree ports (the l-ports repo). Each includer sets R to the
# relative path to the project root before including this file (the root
# Makefile sets R := .) so the paths below resolve from any cwd.
# Per-frontend build output lives under $R/out/<frontend>/.
R ?= .

m = $R/out/host$(hsuf)/love
a ?= $(shell uname -m)

# clang is the default host/love0 compiler (every dev machine here has it; mac's
# `cc` is clang anyway, and the kernel build already defaults KCC=clang). NB: a
# plain `CC ?= clang` is a no-op -- make ships a built-in default `CC = cc` (origin
# `default`, not `undefined`), so `?=` never fires. Override the built-in default
# explicitly while still honoring an env/CLI `CC=` (origin command line/environment):
# `make CC=gcc`. cc_user marks an explicit choice -- it opts out of the musl
# default below and the host block's musl-clang pick.
ifeq ($(origin CC),default)
CC = clang
else
cc_user := 1
endif

# The host binary's FLAVOR. The default is the dynamic glibc build (plus
# liblove.so, which the crew can share). STATIC=1 links `love` fully static against
# musl -- the OPT-IN portable lane (the STATIC block in the root Makefile has
# the whole story), demoted from Linux default 2026-07-07: valgrind emulates
# x87 at 64 bits and musl's strtod leans on the full 80, so under memcheck
# every float literal misparsed ~1e-13 (`make valg` tripped spec.l's exact
# euler law) -- and a static build can't produce liblove.so. A STATIC build gets
# its own out/host-musl tree so the two libcs never share objects -- and $m
# below follows, so tests run the flavor you asked for.
override STATIC := $(filter-out 0,$(STATIC))
hsuf := $(if $(STATIC),-musl,)

# ai_tco for the builds that can take it: 1 = the tail-threaded VM (aps tail-jump,
# never return -- `make vmret` verifies it per binary), 0 = the trampoline loop.
# The host runs $(tco). PINNED ELSEWHERE: love0 stays 0 (the deliberate
# trampoline-coverage lane) and the kernel test build stays 0 (hangs at 1 --
# see the K_TEST block in the root Makefile).
tco ?= 1

# the corpus: 00-init's harness first, the spec second, then uu.l (the uu kernel: vof/defn/kjoin/...),
# then the rest. uu.l is front-loaded EXPLICITLY so its dependents (uukind*, uulay, uupatch, uuwm*)
# always see it, whatever the sort collation -- a locale `ls` orders uukind* before uu.l and the
# laws would run against an unloaded kernel (the byte-sort here happens to put uu.l first, but that
# is implicit and a rename could flip it; test/uukindlaw.l also guards with an explicit assert).
# glaze-x86 and glaze-hook are EXCLUDED: both EXECUTE native machine code, so they run only under
# their own arch-guarded targets (`test_glaze`, `test_hook`), never the arch-neutral corpus.
t = $R/test/00-init.l $R/test/spec.l $R/test/uu.l $(filter-out %/00-init.l %/spec.l %/glaze-x86.l %/glaze-hook.l %/uu.l,$(sort $(wildcard $R/test/*.l)))

love_h = $(wildcard $R/*.h)
# the core rides with its math floor (crew/moon/lib/math/am.c -- our own
# transcendentals; love.c's ai_* defines resolve there, no libm anywhere)
love_c = $R/love.c $R/crew/moon/lib/math/am.c
f_c = $(wildcard $R/crew/quay/*.c)
c_c = $(wildcard $R/libc/*.c)

# -std spelling: clang accepts `gnu23` only from ~clang 18 (Xcode 16). Older Apple
# clang on an old mac wants `gnu2x` (the pre-final spelling). Probe $(CC) once and
# fall back, so the host builds on whatever clang the machine ships.
ai_std := $(shell printf 'int main(void){return 0;}' | $(CC) -std=gnu23 -x c -c -o /dev/null - 2>/dev/null && echo gnu23 || echo gnu2x)

ai_cflags = -std=$(ai_std) -g -O2 -pipe $(EXTRA_CFLAGS) \
  -Wall -Wextra -Werror -Wstrict-prototypes -Wno-unused-parameter \
  -Wmissing-field-initializers -Wno-implicit-fallthrough\
  -falign-functions=16 -fomit-frame-pointer -fno-stack-check -fno-stack-protector \
  -fno-exceptions -fno-asynchronous-unwind-tables
# -fcf-protection (Intel CET) is x86-only -- Apple/arm clang rejects it as an
# error. Keep it on every non-Darwin build (Linux x86 + the cross kernels take
# it as before); macOS does without (it has no CET to turn off).
ifneq ($(shell uname -s),Darwin)
ai_cflags += -fcf-protection=none
endif
