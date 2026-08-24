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

# THE VERSION, the checked-in ./VERSION and the whole of it -- what a build is called,
# moving only when a release does. No VCS suffix anywhere: dist names the tarball for it,
# love_version.h compiles it into love.o, and `.comment` carries the same string (which
# is what lets love0's stamp agree with a real one -- see boot_cc).
love_base := $(shell cat $R/VERSION 2>/dev/null || echo 0)

# ⚠ IS THIS TREE A CHECKOUT OR AN UNPACKED RELEASE? `git -C DIR` walks UP, so the test is for
# THIS tree's own .git and never an ancestor's (crew/build.mk learned that the hard way). One
# thing reads it: the DEFAULT GOAL -- a checkout wants the fast gate for its edit loop, an
# unpacked release wants the product, because whoever unpacked it came for love and not for
# our test binaries.
in_git := $(wildcard $R/.git)

# clang is the default host/love0 compiler. ⚠ `CC ?= clang` would be a NO-OP: make ships a
# built-in default `CC = cc` whose origin is `default`, not `undefined`, so `?=` never
# fires -- the origin test is what overrides it while still honoring `make CC=gcc`.
ifeq ($(origin CC),default)
CC = clang
endif

# WHO LINKS `love`: mooncc by default, and the whole vm with it. HCC=1 takes the $(CC) lane
# instead -- the differential the kernel spells KCC, worn at the host. It is the only build
# that puts a foreign cc on the vm at ai_tco=1, which is where ai_musttail is live and where
# a prototype mismatch our own sibcall pass waves through is refused (doc/misc/moon-c-gaps.md).
# ⚠ ITS OWN TREE, because the two loves are the same path otherwise: out/host-cc keeps the
# objects and the binary apart, and $m follows it so a test runs the one you asked for.
override HCC := $(filter-out 0,$(HCC))
hsuf := $(if $(HCC),-cc,)

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

love_h = $(wildcard $R/core/*.h)
# the core rides with its math floor: our own transcendentals, no libm anywhere
love_c = $R/core/love.c $R/crew/moon/lib/math/am.c
# the quay engine every seat carries. paint.c (32bpp) and nif.c (the love door) are
# per-seat -- a 1-bit device wants neither, the host unity-includes nif.c -- so a seat that
# wants one NAMES it rather than taking it here.
f_c = $(filter-out %/paint.c %/nif.c,$(wildcard $R/crew/quay/*.c))
# inle's libc is nolibc's, named member by member; os.c is the map every syscall
# reaches it through -- and a negative __ai_osv (written at kmain) takes the
# __ai_inle arm, free/sys.c answering the canonical numbers in C. mooncc builds
# the kernel, so it builds
# the kernel's libc too -- there is no second copy to drift. this is host/posix.c's
# closure exactly (plan A3): the members its nifs call, what those pull, and
# nothing more. core.c stays OUT -- it carries malloc, the process entry and the
# std streams, every one of which the kernel owns; free/sys.c answers its four
# seat symbols (environ, stdout/stderr, the sigaction restorer) instead.
# ⚠ NAMING A MEMBER HERE IS A DECISION, and stdio was the one weighed: printf and
# friends write fd 1 themselves, and free/sys.c is seat-blind, so a seated task's
# C-level printf reaches the console where its port reaches the pipe. That is the
# documented divergence (free/sys.c) -- love code writes through ports, which seat.
c_c = $(addprefix $R/crew/moon/lib/nolibc/string/,memchr.c memcmp.c memcpy.c memmove.c memset.c strlen.c) \
  $(addprefix $R/crew/moon/lib/nolibc/sys/,read.c write.c \
    chdir.c chmod.c chown.c clock_gettime.c close.c dup2.c fcntl.c fork.c fstat.c getcwd.c \
    getgid.c getpgrp.c getpid.c getuid.c ioctl.c kevent.c kill.c kqueue.c \
    link.c lseek.c lstat.c madvise.c mkdir.c mount.c open.c pipe.c raise.c readlink.c \
    rename.c rmdir.c setpgid.c setsid.c stat.c symlink.c sysctl.c umask.c \
    unlink.c unshare.c utimensat.c waitpid.c) \
  $(addprefix $R/crew/moon/lib/nolibc/dirent/,closedir.c opendir.c readdir.c) \
  $(addprefix $R/crew/moon/lib/nolibc/signal/,grantpt.c posix_openpt.c ptsname.c \
    sigaction.c sigaddset.c sigemptyset.c signal.c signalfd.c sigprocmask.c \
    tcgetattr.c tcsetattr.c tcsetpgrp.c unlockpt.c) \
  $(addprefix $R/crew/moon/lib/nolibc/proc/,atexit.c execv.c execvp.c exit.c) \
  $(addprefix $R/crew/moon/lib/nolibc/env/,getenv.c setenv.c unsetenv.c) \
  $(addprefix $R/crew/moon/lib/nolibc/stdio/,fflush.c femit.c pad.c semit.c) \
  $R/crew/moon/lib/nolibc/fmt/fprintf.c \
  $R/crew/moon/lib/nolibc/os.c

# ⚠ CANCEL MAKE'S LEX RULE. `.l` is Lex's extension to make, so a built-in `%.c: %.l`
# stands over every source file in this tree -- and where a `<name>.l` sits beside a real
# `<name>.c`, make runs lex on it, fails, and DELETES THE C. An empty recipe unmakes the
# rule. (crew/quay/ is the pair that found it; nothing here has ever wanted lex.)
%.c: %.l
%.r: %.l
%.ln: %.l
.l.c:
.l.r:
.l.ln:

# the dialect we target, and mooncc's own aim -- doc/misc/moon-c-gaps.md is the ledger.
ai_std := c11

ai_cflags = -std=$(ai_std) -g -O2 -pipe $(EXTRA_CFLAGS) \
  -Wall -Wextra -Werror -Wstrict-prototypes -Wno-unused-parameter \
  -Wmissing-field-initializers -Wno-implicit-fallthrough\
  -falign-functions=16 -fno-stack-protector
# ⚠ a strict -std sets __STRICT_ANSI__ and glibc then hides its POSIX half -- host/main.c
# owes clock_gettime and kill, so the level is asked for by name.
# -fcf-protection (Intel CET) is x86-only; the non-x86 seats have no CET to turn off and
# take it as a no-op.
ai_cflags += -fcf-protection=none -D_POSIX_C_SOURCE=200809L
# the data-sentinel tiling core/love.h's ai_typ reads (core/love.c's DSENT), on every ld/lld link.
data_ld = -Wl,-T,$R/core/love_data.ld
# ⚠ AN EMPTY BRACKET IS STILL A BRACKET. core/love.c indexes the host nif slice off
# [__start_ai_nifs, __stop_ai_nifs), which the toolchain synthesises only where the
# SECTION exists -- so an embedder registering its defs by hand owns no AiNif and the
# pair goes undefined at the link. weak declarations do not answer it: ld leaves a weak
# undefined at 0 even where the section IS there, which silently unregisters every host
# nif. naming the empty pair at the one link that wants it keeps the host lane untouched.
nifs_ld = -Wl,--defsym=__start_ai_nifs=0,--defsym=__stop_ai_nifs=0
