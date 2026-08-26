# Shared variables for the host and kernel builds and for out-of-tree ports (the l-ports
# repo). An includer sets R to the project root first (the root Makefile sets R := .), so
# these resolve from any cwd; per-frontend output lands in $R/out/<frontend>/.
R ?= .

# THE RECIPE TAG COLUMN: `@echo 'MOON<TAB>'$@`, and the quote is load-bearing. A bare tab
# in an echo line only separates argv, which echo rejoins with a space -- so the column
# has to be a character echo passes through. Paths print relative to the tree; `make
# install` is the exception, where the artifact lands outside it.

m = $R/out/host$(hsuf)/love
# ⚠ the HOST's arch, which $a is NOT: a cross lane overrides $a on the command line, and
# anything under out/host reading $a then lays a cross artifact into the host tree.
# ⚠ AND `uname -m` IS NOT THE ISA. It answers the kernel's MACHINE, which only linux
# spells the way free/<a>/, the mksys leaves and the holo backends do: the BSDs say
# amd64 for x86_64, freebsd says arm64 and netbsd evbarm for aarch64. evbarm names a
# 32-bit port too, so there the ISA has to come from `uname -p` -- the one place a
# second fork is owed, and only on that branch. flat ifeqs, no else-chain: cook reads
# `else ifeq` as a bare else and drops the condition.
uname_m := $(shell uname -m)
hosta := $(uname_m)
ifeq ($(uname_m),amd64)
hosta := x86_64
endif
ifeq ($(uname_m),arm64)
hosta := aarch64
endif
ifeq ($(uname_m),evbarm)
hosta := $(shell uname -p)
endif
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

# $(CC) is the ambient compiler and the tree names no favourite: mooncc builds everything but
# love0, which by definition cannot be built by the compiler it exists to bootstrap.

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

# the runtime's own headers. named, not globbed: src/ holds the metal seat's k.h and the
# per-ISA asmops beside these, and a touch on those must not rebuild every love object.
love_h = $R/src/love.h $R/src/love_int.h $R/src/kinds.h $R/src/nifs.h $R/src/mx.h
# the core rides with its math floor: our own transcendentals, no libm anywhere.
# love.c broke into TUs so the biggest one is not the whole build's critical path;
# src/love_int.h is what they share. the order here is the link's, not a dependency.
love_tu = love.c ev.c io.c map.c snap.c num.c arr.c
love_tu_c = $(patsubst %,$R/src/%,$(love_tu))
love_c = $(love_tu_c) $R/crew/moon/lib/math/am.c
# src/ is ONE folder, so these name the lanes a directory used to: the metal seat
# (src/kernel.mk builds them) and the per-ISA files, which `a` picks by prefix.
kernel_tu = kmain.c sys.c blk.c doom.c
kernel_c = $(patsubst %,$R/src/%,$(kernel_tu))
arch_c = $(wildcard $R/src/x86_64_*.c) $(wildcard $R/src/aarch64_*.c) $(wildcard $R/src/uefi_*.c)
# ..and the per-ISA set ONE machine's build takes. the rebuild gates link what the
# artifact links, and that is the host's arch alone -- empty on an arch with no seat,
# which is what those gates read to skip their kernel half.
hosta_c = $(wildcard $R/src/$(hosta)_*.c)
# ..and the host lane is the remainder, still a glob: drop a src/<app>.c in and its
# nifs register with no rule edit, exactly as the old host/*.c wildcard promised.
host_c = $(filter-out $(love_c) $(kernel_c) $(arch_c),$(wildcard $R/src/*.c))
# the quay engine every seat carries. paint.c (32bpp) and nif.c (the love door) are
# per-seat -- a 1-bit device wants neither, the host unity-includes nif.c -- so a seat that
# wants one NAMES it rather than taking it here.
f_c = $(filter-out %/paint.c %/nif.c,$(wildcard $R/crew/quay/*.c))
# inle's libc is nolibc's, named member by member; os.c is the map every syscall
# reaches it through -- and a negative __ai_osv (written at kmain) takes the
# __ai_inle arm, src/sys.c answering the canonical numbers in C. mooncc builds
# the kernel, so it builds
# the kernel's libc too -- there is no second copy to drift. this is src/posix.c's
# closure (plan A3) plus the members love.c's hosted compile reaches (plan C1:
# the mmap family behind the W^X arena's runtime branch, refused -ENOSYS on
# metal). core.c stays OUT -- it carries malloc, the process entry and the
# std streams, every one of which the kernel owns; src/sys.c answers its four
# seat symbols (environ, stdout/stderr, the sigaction restorer) instead.
# ⚠ NAMING A MEMBER HERE IS A DECISION, and stdio was the one weighed: printf and
# friends write fd 1 themselves, and src/sys.c is seat-blind, so a seated task's
# C-level printf reaches the console where its port reaches the pipe. That is the
# documented divergence (src/sys.c) -- love code writes through ports, which seat.
c_c = $(addprefix $R/crew/moon/lib/nolibc/string/,memchr.c memcmp.c memcpy.c memmove.c memset.c strlen.c) \
  $(addprefix $R/crew/moon/lib/nolibc/sys/,read.c write.c \
    chdir.c chmod.c chown.c clock_gettime.c close.c dup2.c fcntl.c fork.c fstat.c getcwd.c \
    getgid.c getpgrp.c getpid.c getuid.c ioctl.c kevent.c kill.c kqueue.c \
    link.c lseek.c lstat.c madvise.c mkdir.c mmap.c mount.c mprotect.c munmap.c open.c pipe.c poll.c raise.c readlink.c \
    rename.c rmdir.c setpgid.c setsid.c stat.c symlink.c sysconf.c sysctl.c umask.c \
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
# ⚠ a strict -std sets __STRICT_ANSI__ and glibc then hides its POSIX half -- src/main.c
# owes clock_gettime and kill, so the level is asked for by name.
# -fcf-protection (Intel CET) is x86-only; the non-x86 seats have no CET to turn off and
# take it as a no-op.
ai_cflags += -fcf-protection=none -D_POSIX_C_SOURCE=200809L
# the data-sentinel tiling src/love.h's ai_typ reads (src/love.c's DSENT), on every ld/lld link.
data_ld = -Wl,-T,$R/src/love_data.ld
# ⚠ AN EMPTY BRACKET IS STILL A BRACKET. src/love.c indexes the host nif slice off
# [__start_ai_nifs, __stop_ai_nifs), which the toolchain synthesises only where the
# SECTION exists -- so an embedder registering its defs by hand owns no AiNif and the
# pair goes undefined at the link. weak declarations do not answer it: ld leaves a weak
# undefined at 0 even where the section IS there, which silently unregisters every host
# nif. naming the empty pair at the one link that wants it keeps the host lane untouched.
nifs_ld = -Wl,--defsym=__start_ai_nifs=0,--defsym=__stop_ai_nifs=0
