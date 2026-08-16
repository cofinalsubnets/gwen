# 🌑 love

love is a fully-curried language with an infix, low-paren surface that factors down
to a small parenthesized core. every value is a total function of one argument, and
`(f x y)` is `((f x) y)` while `(f)` is `f` -- application is just left-to-right
currying. integers are church numerals, so a numeric list read left-associatively is
a reversed exponential tower, and any recursive operator is iterated by a
non-negative integer -- iteration is its function action. all recursion lives on the
heap.

it stays internally sound because every operator is total (an undefined op answers
`()`, the unit, which rides through every lane) and every thread yields cooperatively
regardless of user program behavior; one uniform mechanism handles conditions such as
OOM.

## software

love includes its own independent cross-platform C toolchain and userland,
including its own cc, as, ld, sh, make, coreutils, binutils, etc. the normal
install path bootstraps the whole system from source using the ambient
toolchain in less than a minute. love also includes an X11 window manager, a
version control system, a text editor, a freestanding build, and ports for
several embedded platforms.

## language

love is fundamentally a scheme-like lisp with three special forms:

- `\` lambda
- `?` cond
- `:` let

quote is a special case of lambda, all functions are curried, and
every value is a function, which simplifies the evaluator.

beyond classic lisp syntax, love syntax includes as syntactic sugar
haskell-like infix notation, and apl-like prefix notation, and `[] {}`
as data constructors.


### true facts

these evaluate to 1, try them in the repl

```
1 = 0 5                      ; 0 is const-1, 1 is the identity
8 = 3 2                      ; n x = x ** n
262144 = 2 3 4               ; the tower 4 ** (3 ** 2)
3.0 = (1 / 2) 9              ; (1 / 2) x = sqrt x
12 = foldl (+) 0 '(3 4 5)
'(1 2 3) = sort '(3 1 2)
'(2 3 4) = map (+ 1) '(1 2 3)
10 = +(jot 5)
1 < 2 < 3                    ; comparisons chain, and the shared operand is read once
'(0 0 1 3 6 10 15 21) = (flip compose jot (map (compose sat jot))) 8
```
### hello world

```
."hello world\n"
```

### fizzbuzz

```
(100
 (\ n (: f (? (n % 3) "" "fizz")
         b (? (n % 5) "" "buzz")
         _ .(? ($f | $b) (f + b) n)
         _ ."\n"
       (n + 1)))
 1)
```

### build test run

- `make` -- the default: build + the fast gate
- `make test_slow` before committing, `make test_extra` before merging
- `make repl`, `make lint`, `make install` (into `~/.love`, a self-implying nest)
- `make kernel` + `make run` boot the freestanding kernel under qemu; `make uefi`
  builds our own `BOOTX64.EFI`
- `make wasm` the browser image, `make site` the static site
- `make dist` the one-file artifact: `out/dist/love-<arch>` is the default love re-baked
  with the crew warm. `love up URL` clones + builds + installs the whole nest from a
  served `.sb/` tree, and the same binary is multi-call
  (`love sb|cook|kore|mooncc ..`); `love down` uninstalls
- `out/host/love` is the binary. `love file.l` runs a file, `love -e expr` an
  expression, `love -l lib` preloads one; with no program and a terminal on stdin it is
  a repl. `man love`, or [doc/love.md](doc/love.md)

`echo .ev | love` prints the compiler -- `.` prints, `ev` is the self-hosted evaluator,
and what comes out is the lambda the compiler compiled itself into.
that is not a joke.

### the crew

the **inle crew** is the cast of programs and pieces that make up love -- each a tiny
love layer over a handful of host nifs. the runnable ones land on PATH beside `love`
via `make install`.

- 🌑 **inle** -- the vessel: the freestanding kernel, booting on bare metal with no OS
  under it. [free/](free/)
- 🐇 **bellberry** -- the navigator: the evaluator. [love/ev.l](love/ev.l)
- 🐐 **mow** -- the grass chewer: the two-gen, two-space copying collector. [love.c](love.c)
- 🕷️ **holo** -- the assembler: amd64/arm64/riscv64/thumb, and the linker.
  [crew/holo/](crew/holo/)
- 🍄 **moon** -- the C compiler: `mooncc`, preprocessor to optimizing backend, holo its
  assembler and linker. [crew/moon/](crew/moon/)
- 🐀 **cook** -- the build system: a gnu make clone that reads a real Makefile, g's own
  included. [doc/cook.md](doc/cook.md)
- 🌱 **svalbard** (`sb`) -- the vcs: a patch-set DAG folded together with the installer.
  [doc/sb.md](doc/sb.md)
- 🦨 **kore** -- the utility skunk: one multi-call coreutils binary, busybox's trick.
  [crew/kore/](crew/kore/)
- 🐕 **bao** -- the shell: an rlwrap clone. raw `love` shrinks to a read/eval/write
  filter and bao is the editor, history and fault-face on top. [love/bao.l](love/bao.l)
- 🐚 **lush** -- the command shell: POSIX, the distro's console and its `/bin/sh`.
  [doc/lush.md](doc/lush.md)
- ⚖ **libra** -- the scale: the .l balance scan, formatter, infix pass and doc lifter,
  one scanner under all of them. [doc/libra.md](doc/libra.md)
- 🐈 **ain** -- the netcat: an openbsd netcat clone in ~70 lines. [mk/tools/ain.l](mk/tools/ain.l)
- 🦐 **lux** -- the window manager: an xmonad clone. [crew/lux/](crew/lux/)
- 🦑 **quay** -- the terminal emulator: a cuttlefish with 256-color skin that likes
  writing screensavers -- and roguelikes: rove plays live on the front page.
  [crew/quay/](crew/quay/)
- 🪸 **pulchritude** -- the editor: a vi clone. [crew/vi/](crew/vi/)
- 🦇 **thom** -- the SAT bat: a CDCL solver. [crew/sat/](crew/sat/)
- 🔭 **tele** -- the pilot: a pytorch clone, autograd over the celestial numerics.
  [crew/tele/](crew/tele/)
- 🕊 **gwen** -- the synthesist.

### under the hood

- one word per value: a fixnum is a tagged odd word, anything else a heap object whose
  first word is its hot -- a live external reference, the wire out of the heap to the
  ap that runs it. the vm is tail-threaded -- aps jump, never return -- over a two-space
  copying heap; `make vmret` proves the no-return claim by disassembling the binary.
- every operation is generic, dispatched on a value's kind through NxN tables (`+`,
  `*`, apply); the kind enum is the type lattice, the total order over all values is the
  enum order, and the lattice is literally the diagonal of the dispatch tables. `sort`
  is one C comparison per chain -- the total order is the comparator.
- the compiler is written in love. at build time the evaluator sits on the egg (the
  quoted compiler source) twice -- the C bootstrap compiles the compiler, which
  recompiles itself -- and the hatchling bakes into the binary; `born` records the hatch
  time. just before birth the egg mops every compiler-internal nom, the book itself
  included. the same image runs on linux, bare metal (x86_64 through three doors, one
  ELF: its own PVH boot for `qemu -kernel`, its own `BOOTX64.EFI`, or limine; aarch64
  via limine and its own EL1 stub), and wasm.
- moon is the default build: the host cc compiles only the bootstrap `love0`, which runs
  mooncc over `love.c` and every `host/*.c` and lets holo link the result -- no
  gcc/glibc/ld in the inner loop, and `make test_fixpoint` has the binary rebuild ITSELF
  byte-identically. `KCC ?= mooncc` builds the kernel too, assembly included.

### where the truth lives

[test/spec.l](test/spec.l) is the spec -- the reference and the test in one, each
section stating its laws in a comment over the asserts that prove them, a real test in
the corpus, so every claim stays green. [test/proof/rocq/spec.v](test/proof/rocq/spec.v) is the
machine-checked, axiom-free half. [CLAUDE.md](CLAUDE.md) is the narrative: how to work
here, the traps, the architecture. [doc/](doc/) goes deep, one file per subsystem.

### license

love is free: [0BSD](LICENSE). use it, change it, sell it, ship it, fold it into
anything -- no attribution owed, no notice to keep, nothing asked back. the few files
that came from elsewhere keep their own terms and are rostered in [NOTICE](NOTICE);
none of them is copyleft, and none of them reaches love.
