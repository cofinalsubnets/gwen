# 🌑 love

love is a fully-curried language with an infix, low-paren surface that factors down
to a small parenthesized core. every value is a total one-argument function, and
(almost) every expression has a value. integers are church numerals, so a numeric
list read left-associatively is a reversed exponential tower, and any recursive
operator is iterated by a non-negative integer (a green charm) -- iteration is its
function action. all recursion lives on the heap.

the surface adds sigils -- all-punctuation symbols -- read as infix or prefix
operators by placement: a precedence-and-associativity superset over the
parenthesized core, and the two mix freely. arithmetic folds left, as everywhere
else (`1 - 2 - 3` is -4); the other bands fold right. an infix operator takes its
plain meaning in function position, and wrapping a sigil in parens hands it back
as a plain curried function: `3 = ((+) 1 2)`. the core is applicative, curried, untyped lambda calculus (though
applicative order may vary). it stays internally sound because every operator is
total and every thread yields cooperatively regardless of user program behavior;
one uniform mechanism handles conditions such as OOM.

features
- numeric tower with shaped array broadcasting
- lambdas, macros, closures, multitasking
- freestanding bare metal kernel build
- free portable C with zero dependencies
- self-hosting compiler written in love

love has three special forms plus "operators". the forms are
- `\` lambda
- `?` cond
- `:` let

the reader is structural and knows no operator tables -- just tokens, parens,
strings, and the value surface:
- `'` quote (desugars to one-operand lambda)
- `` ` `` list (the element-eval ctor: every element is evaluated, so quote the literal positions)
- `@` at (array literal)
- `#` hash (hash/box literal)
- `~` twin (twin-gem/complex literal `~(re im)`; a bare `~x` lifts a gem, conjugates a twin gem)

operator sigils are plain symbols until the compiler resolves them, at compile
time, against two tables -- `dyadics` for a spaced sigil, `monadics` for a glued
run (`dyadics` is per-form extensible: pin an entry and the next form compiles
with it):
- spaced, at two: `+ - * / % = < <= > >= | &`
- `?` is dyadic like the rest: infix `(t ? a)` is the one-armed cond, and an
  else arm is the prefix `(? t a b)` -- which is n-ary, `(? t1 a t2 b else)`
- glued: `$` sat (the value's net -- its content measure -- clamped once,
  `max(0, ceil)`; `!!$` is the iverson bracket, the truth bit `?` dispatches on),
  `!` nil? (not), `.` dot (print and return item)
- a glued run factors greedily, longest prefix first: `!!` double-negates, `<>`
  is `cap` of `cup`; a spaced sigil is one whole name and never splits
  (`!=`, `&&`, `>>=`)

and the valence law: every operator is two operators -- GLUED IS MONADIC,
SPACED IS DYADIC -- and this holds everywhere, head position included, so
`(<x = y)` reads `(= (cap x) y)`. spaced heads stay prefix calls and sections
(`(+ 1 2)`, `(1 +)`); only the special forms `:` `?` `\` keep the whole list at
head, so minified `(:(co ..)` still reads `(: (co ..))`.
- `<x >x` cap and cup; `<>x ><x <<x >>x` the compounds, by factorization
- `+l` the net -- the true sum -- and `*l` the product
- `|x` abs, `-x` neg (`+` and `-` are runs like every other punctuation; the one
  exception is that a digit or `.` after them starts a NUMERAL, so `-3` stays a
  number while `--5` is `-(-5)`), `/x` reciprocal, `%x` frac, `?x` the iverson bracket
- `$x !x .x` as ever: sat, not, print
- a glued sigil binds tightest: `$"ab" + 2` is `(+ ($ "ab") 2)`, i.e. 197
the numerals still carry the power family (`-1 x = 1 / x`, `(1 / 2) x =
sqrt x`, `n x = x ** n`); words cover the rest (`abs int gcd // << >> ^
sine cosine log`), and general folds stay words: `(foldl f z l)`.

the plain parenthesized subset is the lassoc core: `?` is still the cond form at
the head of a list, bare punct symbols escape in parens -- `(+)` is `+` as a value
-- and quote interiors are data (operators under `'` stay plain symbols), so these
are true too:
- `12 = foldl (+) 0 '(3 4 5)`
- `24 = foldl (*) 1 '(1 2 3 4)`
- `'(1 2 3) = sort '(3 1 2)`
- `'(2 3 4) = map (+ 1) '(1 2 3)`
- `'(0 1 2) = jot 3`
- `10 = +(jot 5)`
- `5 = () + 5` and `5 = () * 5` -- `()` is the **unit**, the shared identity of `+` and `*` in every lane; `0` and `1` are its two faces (the additive identity it shows in `+`, the multiplicative in `*`). it rides through every other arithmetic operator the same way, either side: `5 = 5 - ()` and `5 = () - 5` (the do-nothing operand -- the op never happens)
- `'(0 0 1 3 6 10 15 21) = (flip compose jot (map (compose sat jot))) 8`

that last is the triangular numbers, point-free: `jot` lays out `0 .. n-1`,
`sat` sums each prefix to its charm, and `flip compose` feeds the one into the
other (the nth is the net of `0 .. n-1`, so it opens on the floor twice).

the spec is [test/spec.l](test/spec.l) -- the reference and the test in one,
each section stating its laws in a comment over the asserts that prove them, a
real test in the corpus, so every claim stays green. the narrative -- how to
work here, the traps, the architecture -- is [CLAUDE.md](CLAUDE.md).


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
- `make` build + test
- `make repl` interactive shell
- `make test_slow` adds the freestanding kernel (qemu) + tool diffs
- `make wasm` build the browser image (wasm/love.js, used by index.html) -- rebuild + stage by hand
- `make dist` the one-file artifact: `out/dist/love-x86_64` is the default love
  re-baked with the crew warm -- `love up URL` clones + builds + installs the
  whole nest from a served `.seed/` tree, and `love seed|cook|kore|kiosko|mooncc ..`
  is the same binary being multi-call (`love down` uninstalls)
- `out/host/love` is the binary -- love is the word (it was `ai` for a while); `out/host/ai` stays a symlink
- `out/host/love file.l` run a file
- `echo .ev | love` print the compiler

that last one is not a joke. `.` prints, `ev` is the self-hosted evaluator,
and what comes out is the lambda the compiler compiled itself into -- about
eight kilobytes of the whole back end.

the shell survives its mistakes: with no help of your own installed it
provides one, so any condition prints `;; a b` -- `;; missing undefined-name`,
`;; apcap 3000000` -- answers the zero point, and the session keeps going. multiline
entry continues while a shape is open; enter cashes any complete buffer;
history is a normal shell's. scripts and files stay helpless (terminal),
per the law.

### cook

love includes a somewhat GNU-compatible `make` clone called `cook` written in pure
love and bootstrapped through normal `make`. `cook` accepts either the traditional
`Makefile` format or its own `Cookfile` DSL.

```
(recipe "hello" '("hello.o" "greet.o")
        '(("cc" "-o" "hello" "hello.o" "greet.o")))
(recipe 'clean '() '(("rm" "-f" "hello" "hello.o" "greet.o")))
(cook (ticket 0))
```

each card is `(recipe item ingredients steps)`: an item is a filename or a
phony symbol, ingredients are the items it needs first, steps are argv lists
(run as subprocesses) or thunks. item ages come from `stat` through the host
`hark` nif, so a phony symbol is ageless and always cooks. the whole kitchen:
cook an item -- check its date, prep its ingredients, follow the recipe's
steps, record what shipped, from the cards; the ticket names what to make
(default: the first card, the standing check).

- `love -l crew/cook/cook.l [ticket]` -- cook a ticket (cook discovers the build file:
  a `Makefile` if present, else a `Cookfile`, else a legacy `Cards.l`; name one
  explicitly to override)
- cook reads a real **Makefile** directly -- a reasonably GNU-make-compatible
  import: `$(VAR)` expansion and substitution refs, the common functions
  (`shell`, `wildcard`, `dir`, `patsubst`, `filter`, ...), `ifeq`/`ifdef`
  conditionals, `include`, `:= ?= +=`, pattern + static-pattern rules,
  order-only prereqs, and the `$@ $< $^ $*` automatics
- `make -f cook.mk test` -- the make-shaped stub: bootstraps the binary, then
  forwards to cook (it names the `Cookfile` explicitly)
- [crew/cook/example/](crew/cook/example/) is a worked C build;
  [crew/cook/cooktest.l](crew/cook/cooktest.l) (`make test_cook`) tests the importer

love builds itself this way too: `love -l crew/cook/cook.l Makefile host` runs g's own
Makefile from scratch, and the cook-built binary passes the whole corpus. cook
runs *on* love, so you need a love to begin; the [crew/cook/Cookfile](crew/cook/Cookfile) is the
curated cross-cutting verbs (`test clean valg vmret bench install`).

### the crew

the **inle crew** is the cast of programs and pieces that make up love. each runnable
member is a tiny love layer over a handful of host nifs; on a hosted system they
`make install` onto PATH beside `love`. the full roster (the creatures they wear live
on the front page):

- 🌑 **inle** -- the vessel: the freestanding kernel, booting on bare metal with no
  OS -- a tty and a repl, nothing under it. `make kernel` builds the image, `make
  run` boots it under qemu. [port/inle/](port/inle/)
- 🔭 **tele** -- the pilot: a pytorch clone, autograd over the celestial numerics.
  [crew/tele/](crew/tele/)
- 🐇 **bellberry** -- the navigator: the evaluator. [love/ev.l](love/ev.l)
- 🕊 **gwen** -- the synthesist.
- 🐈 **ain** -- the netcat: an openbsd netcat clone in ~70 lines -- `ain host port` a
  TCP client, `ain -l port` a server, bytes pumping both ways with no select loop.
  [tools/ain.l](tools/ain.l) + [host/sock.c](host/sock.c)
- 🐕 **bao** -- the shell: an rlwrap clone. raw `love` shrinks to a read/eval/write
  filter and bao is the editor + history + fault-face on top, doubling as a pty
  wrapper. [love/bao.l](love/bao.l) + [host/posix.c](host/posix.c)
- 🐀 **salt** -- the build system: cook, a gnu make clone that reads a real Makefile.
  [crew/cook/](crew/cook/)
- 🪸 **seed** -- the vcs: a patch-set version control folded together with the
  installer -- record/log/diff/sync over a content-addressed patch DAG.
  [crew/seed/](crew/seed/) + [doc/seed.md](doc/seed.md)
- 🐐 **mow** -- the grass chewer: the two-gen, two-space copying garbage collector.
- 🕷️ **holo** -- the assembler: an amd64/arm64 assembler (short for holophrasm), the
  glaze's back end. [crew/holo/](crew/holo/)
- 🍄 **moon** -- the C compiler: mooncc, preprocessor to optimizing backend, holo its
  assembler and linker -- love rebuilds itself gcc/glibc/ld-free. [crew/moon/](crew/moon/)
- 🦨 **kore** -- the coreutils: one multi-call binary, busybox's trick -- diff, grep,
  sed, the fs/line/process tools, nc, make, as, ar, ld, vi. [crew/kore/](crew/kore/)
- 🦇 **thom** -- the SAT bat: a CDCL solver. [crew/sat/](crew/sat/)
- 🦐 **lux** -- the window manager: an xmonad clone, configured by salt
  (`~/.love/etc/lux.l`). [crew/lux/](crew/lux/) + [crew/lux/config.l](crew/lux/config.l)
- 🦑 **quay** -- the terminal emulator: a cuttlefish with 256-color skin that likes
  writing screensavers -- and roguelikes: rove plays live on the front page.
  [crew/quay/](crew/quay/)
- 🪸 **pulchritude** -- the editor: a vi clone. [crew/vi/](crew/vi/)

### under the hood
- one word per value: a fixnum is a tagged odd word, anything else is a heap
  object whose first word is its hot -- a live external reference, the wire out
  of the heap to the ap that runs it. the vm is tail-threaded -- aps jump,
  never return -- over a two-space copying heap; `make vmret` proves the
  no-return claim by disassembling the binary.
- every operation is generic, dispatched on a value's kind through NxN tables
  (`+` `*` apply); the kind enum is the type lattice, the total order over all
  values is the enum order, and the lattice is literally the diagonal of the
  dispatch tables. `sort` is one C comparison per chain -- the total order is
  the comparator.
- no interpreter state lives outside the heap: the book (an ordinary love
  hash) carries the globals, macros, the operator tables, the help function
  and the rng; C finds its own hooks by name, allocation-free. the egg pulls
  every compiler-internal name -- the book itself included -- before the
  image is born. a name not in the book is missing: reading one is a
  call for help, and helpless it reads the zero point, a nameless unit.
- the compiler is written in love. at build time the evaluator sits on the egg
  (the quoted compiler source) twice -- the C bootstrap compiles the compiler,
  which recompiles itself -- and the hatchling bakes into the binary; `born`
  records the hatch time. the same image runs on linux, bare metal
  (x86_64 through three doors, one ELF: its own PVH boot for `qemu -kernel`,
  its own `BOOTX64.EFI` for real hardware, or limine; aarch64 via limine),
  and wasm.
- moon is a C compiler, also written in love: a preprocessor, parser, and an
  optimizing amd64/arm64 backend through the holo assembler. it IS the default
  build now: the host cc compiles only the bootstrap `love0`, which runs mooncc
  over `love.c` and every `host/*.c` and lets holo link the result -- no
  gcc/glibc/ld in the inner loop, and `make test_fixpoint` has the binary
  rebuild ITSELF byte-identically. it builds real third-party C too (gnu tar,
  m4) and is closing on clang -O2 on the code it emits. [crew/moon/](crew/moon/)
- status rides the pointer tag bits, and a global `help` function receives every
  raise as `(help a b)` -- the condition, and nothing else: the reader answers its
  own nothings as values rather than raising them.
- `=` is exact, so e^(i*pi) honestly misses -1 by ~1e-16 -- but the principal
  log is exact (`(log -1) = i * pi`, since atan2(0,-1) is pi by IEEE fiat),
  and sqrt factors its angle through sinpi/cospi, so `(1 / 2) -1 = i` on the
  nose.
- functions compare by alpha-equivalence of their source, and a partial
  application equals its literal lambda: `(adder 5) = (\ x (+ x 5))`. numbers
  never equal closures -- numerals *act* as their lambdas (`(1 x) = x`), but
  `=` stays representation-strict.

### license

love is free: [0BSD](LICENSE). use it, change it, sell it, ship it, fold it
into anything -- no attribution owed, no notice to keep, nothing asked back.
the few files that came from elsewhere keep their own terms and are rostered
in [NOTICE](NOTICE); none of them is copyleft, and none of them reaches love.
