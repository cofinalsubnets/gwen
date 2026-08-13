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

- numeric tower with shaped array broadcasting
- lambdas, macros, closures, multitasking
- freestanding bare metal kernel build
- free portable C with zero dependencies
- self-hosting compiler written in love

### the surface

three special forms, everything else a function call:

- `\` lambda -- and, with one operand, quote
- `?` cond -- test/result pairs, then a final else. it is dyadic like every other
  sigil, so infix `(t ? a)` is the one-armed form and an else arm is the prefix
  `(? t a b)`, which is n-ary
- `:` letrec\*/sequence -- binds in source order, the last form is the result

the reader is structural and knows no operator tables -- just tokens, parens, strings,
and five value sigils:

- `'` quote (desugars to one-operand `\`)
- `` ` `` list -- the element-eval ctor: every element is evaluated, so quote the
  literal positions
- `@` array, `#` hash/box, `~` twin gem (complex, `~(re im)`; a bare `~x` lifts a gem,
  conjugates a twin)

the two that nest worst have a bracket spelling: `[a b]` is `` `(a b) `` and `{k v}`
is `#(k v)`, so a list of lists reads `[[a b] [c d]]`. the opener carries the wrap; the
three closers `)` `]` `}` are one and only end the open form, so a crossed pair reads
in silence -- matching them is [libra](crew/libra/)'s job (`make lint`). a constructor
sigil over an open form *renames* that wrap instead of stacking on it, so any delimiter
serves any wrap: `@[1 2 3]` is `@(1 2 3)`. the constructors are exactly `` ` `` `#` `@`
`~`; `'` is not one -- it takes the next datum whatever it is, so `'[a b]` is the form
`(list a b)` quoted.

### operators

a sigil -- an all-punctuation symbol -- is a plain symbol until the compiler resolves
it, at compile time, against two tables. **the valence law** picks which: *glued is
monadic, spaced is dyadic*, and it holds everywhere, head position included, so
`(<x = y)` reads `(= (cap x) y)`. only the three special forms keep the whole list at
head, so minified `(:(co ..)` still reads `(: (co ..))`.

**an undeclared sigil is infix at band 95 -- above every declared row -- so the table
holds only the exceptions.** one table, `operators`, keyed by ARITY -- the operands a
sigil takes from the form around it: 0 glued, 2 spaced, -1 spaced with no bound. the
declared bands, loosest to tightest: the binder `:`, weak apply `$` and the lambda `\`,
cond `?`, cons `><`, logical, comparison, additive, multiplicative, `**`. arithmetic
folds left (`1 - 2 - 3` is -4), everything else right. `grip` is the live door onto
the table: it takes a lane, a nom and a band, answers the row it replaced, and a refused
shape rolls back and scares.

a glued run factors greedily, longest prefix first -- `!!` double-negates, `<>` is
`cap` of `cup` -- while a spaced sigil is one whole name and never splits (`!=`, `&&`,
`>>=`). `+` and `-` are runs like every other punctuation; the one exception is that a
digit or `.` after them starts a NUMERAL, so `-3` stays a number while `--5` is `-(-5)`.
wrapping a sigil in parens hands it back as a plain curried function: `3 = (+) 1 2`.
quote interiors are data, so operators under `'` stay plain symbols.

the monadic vocabulary: `<x >x` cap and cup (`<>x ><x <<x >>x` the compounds, by
factorization), `+l` the net -- the true sum -- and `*l` the product, `|x` abs, `-x`
neg, `/x` reciprocal, `%x` frac, `?x` the iverson bracket, `$x` sat, `!x` nil?, `.x`
print-and-return. a glued sigil binds tightest: `$"ab" + 2` is `(+ (saturate "ab") 2)`,
i.e. 197. the numerals carry the power family (`-1 x = 1 / x`, `(1 / 2) x = sqrt x`,
`n x = x ** n`); words cover the rest (`abs int gcd // << >> ^ sine cosine log`), and
general folds stay words (`foldl f z l`).

### true things

paste any of these at the repl, which reads a line as one expression -- each is a
fixed point of `libra infix`, the max-infix min-paren spelling:

```
1 = 0 5                      ; 0 is const-1, 1 is the identity
5 = 1 5
8 = 3 2                      ; n x = x ** n
262144 = 2 3 4               ; the tower 4 ** (3 ** 2)
3.0 = (1 / 2) 9              ; (1 / 2) x = sqrt x
12 = foldl (+) 0 '(3 4 5)
24 = foldl (*) 1 '(1 2 3 4)
'(1 2 3) = sort '(3 1 2)
'(2 3 4) = map (+ 1) '(1 2 3)
'(0 1 2) = jot 3
10 = +(jot 5)
'(0 0 1 3 6 10 15 21) = (flip compose jot (map (compose sat jot))) 8
```

that last is the triangular numbers, point-free: `jot` lays out `0 .. n-1`, `sat` sums
each prefix to its charm, and `flip compose` feeds the one into the other.

`5 = () + 5` and `5 = () * 5` -- `()` is the **unit**, the shared identity of `+` and
`*` in every lane; `0` and `1` are its two faces. it rides through every other
arithmetic operator the same way, either side (`5 = 5 - ()` and `5 = () - 5`): the
do-nothing operand, the op never happens.

false is *nothing*: whatever nets `<= 0`. the net is the complex-valued content
measure -- a number its own value, text the sum of its charms, a list the sum of its
elements'. `$` is the one saturating clamp onto that measure and `!!$` is the truth bit
`?` dispatches on, so `!x` is exactly `0 = $x`. see [doc/measures.md](doc/measures.md).

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
- `make test_slow` before committing, `make test_extra` before merging; read the
  summary lines, not the exit code
- `make repl`, `make lint`, `make install` (into `~/.love`, a self-implying nest)
- `make kernel` + `make run` boot the freestanding kernel under qemu; `make uefi`
  builds our own `BOOTX64.EFI`
- `make wasm` the browser image, `make site` the static site
- `make dist` the one-file artifact: `out/dist/love-<arch>` is the default love re-baked
  with the crew warm. `love up URL` clones + builds + installs the whole nest from a
  served `.seed/` tree, and the same binary is multi-call
  (`love seed|cook|kore|mooncc ..`); `love down` uninstalls
- `out/host/love` is the binary. `love file.l` runs a file, `love -e expr` an
  expression, `love -l lib` preloads one; with no program and a terminal on stdin it is
  a repl. `man love`, or [doc/love.md](doc/love.md)

`echo .ev | love` prints the compiler -- `.` prints, `ev` is the self-hosted evaluator,
and what comes out is the lambda the compiler compiled itself into, under nine
kilobytes of the whole back end. that is not a joke.

the shell survives its mistakes: with no help of your own installed it provides one, so
any condition prints `;; a b` -- `;; missing undefined-name`, `;; apcap 3000000` --
answers the zero point, and the session keeps going. multiline entry continues while a
shape is open. scripts and files stay helpless (terminal), per the law.

### the crew

the **inle crew** is the cast of programs and pieces that make up love -- each a tiny
love layer over a handful of host nifs. the runnable ones land on PATH beside `love`
via `make install`; the creatures they wear live on the front page.

- 🌑 **inle** -- the vessel: the freestanding kernel, booting on bare metal with no OS
  under it. [port/inle/](port/inle/)
- 🐇 **bellberry** -- the navigator: the evaluator. [love/ev.l](love/ev.l)
- 🐐 **mow** -- the grass chewer: the two-gen, two-space copying collector. [love.c](love.c)
- 🕷️ **holo** -- the assembler: amd64/arm64/riscv64/thumb, and the linker.
  [crew/holo/](crew/holo/)
- 🍄 **moon** -- the C compiler: `mooncc`, preprocessor to optimizing backend, holo its
  assembler and linker. [crew/moon/](crew/moon/)
- 🐀 **cook** -- the build system: a gnu make clone that reads a real Makefile, g's own
  included. [doc/cook.md](doc/cook.md)
- 🌱 **seed** -- the vcs: a patch-set DAG folded together with the installer.
  [doc/seed.md](doc/seed.md)
- 🦨 **kore** -- the utility skunk: one multi-call coreutils binary, busybox's trick.
  [crew/kore/](crew/kore/)
- 🐕 **bao** -- the shell: an rlwrap clone. raw `love` shrinks to a read/eval/write
  filter and bao is the editor, history and fault-face on top. [love/bao.l](love/bao.l)
- 🐚 **lush** -- the command shell: POSIX, the distro's console and its `/bin/sh`.
  [doc/lush.md](doc/lush.md)
- ⚖ **libra** -- the scale: the .l balance scan, formatter and lsp, one scanner under
  all three. [doc/libra.md](doc/libra.md)
- 🐈 **ain** -- the netcat: an openbsd netcat clone in ~70 lines. [tools/ain.l](tools/ain.l)
- 🦐 **lux** -- the window manager: an xmonad clone. [crew/lux/](crew/lux/)
- 🦑 **quay** -- the terminal emulator: a cuttlefish with 256-color skin that likes
  writing screensavers -- and roguelikes: rove plays live on the front page.
  [crew/quay/](crew/quay/)
- 🪸 **pulchritude** -- the editor: a vi clone. [crew/vi/](crew/vi/)
- 🦇 **thom** -- the SAT bat: a CDCL solver. [crew/sat/](crew/sat/)
- 🔭 **tele** -- the pilot: a pytorch clone, autograd over the celestial numerics.
  [crew/tele/](crew/tele/)
- 🕊 **gwen** -- the synthesist.

the document chain is **lapiz** (the markdown/html/roff lens, which writes the man
pages) -> **papel** (the static site) -> **kiosko** (the static web server). **rune**
does symbolic algebra, **manifest** is the spreadsheet, **tls** the cipher suite in the
word lane.

### under the hood

- one word per value: a fixnum is a tagged odd word, anything else a heap object whose
  first word is its hot -- a live external reference, the wire out of the heap to the
  ap that runs it. the vm is tail-threaded -- aps jump, never return -- over a two-space
  copying heap; `make vmret` proves the no-return claim by disassembling the binary.
- every operation is generic, dispatched on a value's kind through NxN tables (`+`,
  `*`, apply); the kind enum is the type lattice, the total order over all values is the
  enum order, and the lattice is literally the diagonal of the dispatch tables. `sort`
  is one C comparison per chain -- the total order is the comparator.
- love has no global state. nothing lives outside the heap: what C needs it reaches by
  numbered slot, sealed in boot order and GC-traced -- the reader, the church hooks, the
  operator pass, the installed help, the task's stdio. names live in the book, and a
  name not in the book is missing: reading one is a call for help, and helpless it reads
  the zero point.
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
- status rides the 2 pointer tag bits, and the installed help receives every raise as
  `(help a b)` -- the condition, and nothing else. the reader answers its own nothings
  as values rather than raising them.
- `=` is exact, so e^(i\*pi) honestly misses -1 by ~1e-16 -- but the principal log is
  exact (`log -1 = i * pi`, since atan2(0,-1) is pi by IEEE fiat), and sqrt factors its
  angle through sinpi/cospi, so `(1 / 2) -1 = i` on the nose.
- functions compare by alpha-equivalence of their source, and a partial application
  equals its literal lambda: `adder 5 = (\ x (x + 5))`. numbers never equal closures --
  numerals *act* as their lambdas (`1 x = x`), but `=` stays representation-strict.

### where the truth lives

[test/spec.l](test/spec.l) is the spec -- the reference and the test in one, each
section stating its laws in a comment over the asserts that prove them, a real test in
the corpus, so every claim stays green. [proof/rocq/spec.v](proof/rocq/spec.v) is the
machine-checked, axiom-free half. [CLAUDE.md](CLAUDE.md) is the narrative: how to work
here, the traps, the architecture. [doc/](doc/) goes deep, one file per subsystem.

### license

love is free: [0BSD](LICENSE). use it, change it, sell it, ship it, fold it into
anything -- no attribution owed, no notice to keep, nothing asked back. the few files
that came from elsewhere keep their own terms and are rostered in [NOTICE](NOTICE);
none of them is copyleft, and none of them reaches love.
