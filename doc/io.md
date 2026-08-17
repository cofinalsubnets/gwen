# the io substrate

The reader and the port surface. They are one subject: a port, viewed as input, IS a charlist
whose tail is a thunk that reads more, and once that is true the reader has one input type and
the scheduler has one place it waits.

This ORIENTS; the laws live in `test/reader.l`, `test/io.l` and `test/front/io.l`, and every
doubt settles by probing `sound`.

---

## part I — the reader

### two readers, one grammar

* **`p0`** — the bootstrap parser in C (love.c). It reads a **pure lisp subset**: delimiters,
  `;` and `#!` comments, `"…"` with escapes, atoms, `'` quote. Its lexers are charlist-native.
* **`p1`** — `love/p1.l`, the real reader, written in love on top of p0. It carries everything
  else: the operator run, the valence law's `mono` wrap with its head-fusion rule and the
  `:`/`?` suppression, the constructor wraps `` ` `` `#` `@` `~` and their SPLICE rewrites, the
  `~` twin-vs-conj peek, the empty-collection direct-nif rewrite, the comma datum, the
  trailing-`-` shed, the bracket spellings (`[..]` is `` `(..) ``, `{..}` is `#(..)`, the three
  closers are one, and a constructor sigil over any opener renames the wrap -- `@[1 2]` is
  `@(1 2)`), the prime inside names, the number tower and the
  `ieee-inf` named literals.

This is `c0`/`ev` again, one layer down: a tiny C bootstrap and the real thing in love. ⚠ The
egg's corpus is read by p0, so **p1.l itself is the one file held to the pure lisp subset**
(egg.l too, through applyq's driver door). Everything else, prel included, rides p1.

⚠ **prel rides p1**, so an `ai_evals_` that bundles p1's text with a later one puts BOTH on p0 —
`readtext` picks its reader once per call.

### one input: the charlist IS the port

**Laziness is not transparent.** The caller REFLECTS on the tail — a cons is more input, `()` is
the end, anything else is a thunk to force — and nothing forces implicitly.

That is what makes it cheap. A transparent `cup` would have to force, and forcing means CALLING,
an ap frame and a jump, which the `op1` expression-slot macro cannot build; the reflecting
version leaves `cup` and the hot path untouched, and the only new C is what mints a thunk over a
port. But the point is not speed — it is that "charlist" and "port" stop being two ideas.

`flow` is the lift: a port to a lazy charlist, gulping a chunk per force. `trickle` is its
one-byte-at-a-time twin, which stdin takes so a child can inherit a correctly positioned fd.
`slurp` takes the whole thing. The unit of laziness is the **gulp**, not the byte, and the force
lives in the reader rather than in the lift, so the caller decides.

⚠ `slurp` must build its charlist on an ACCUMULATOR, never on the way down. `(link c (rl i))`
makes the stack depth the byte count — merely deep on a file, and quadratic on a socket, because
every would-block parks and a park copies the task's stack.

### the `sound` protocol

`sound` takes text — a port, a string, or a charlist — and answers the read protocol:
`(datum . rest)`, with the rest always a charlist, `()` at a clean end, and the symbol `'torn`
mid-shape.

⚠ **`torn` is a plain interned symbol, not a book name.** `'torn` at a call site and `'torn`
inside p1 are the one value, so it is spelled at each site — a quote, which is cheaper than a
book walk. A bare `torn` is an honest `missing`.

⚠ **The RESIDUE is a charlist whichever door went in.** A string door spreads once per text, not
once per datum, so the two doors meet after the first read. (Making the *walk* take a string is a
different and much larger claim.)

⚠ **The reader does not resume.** On an unfinished shape it discards the partial parse and the
bytes are already consumed. `more` is only a signal that the caller must own the text and
re-parse the concatenation — which every consumer already does: bao re-reads the whole editor
buffer from byte 0 on each Enter, lux carries pending text forward and re-taps it, and
`reads`/`use`/`-l`/cook/kore/salt simply end the stream.

Because the reader can ask for more itself, `reads` and `forms` are a walk rather than a refill
loop, and the old hazard they carried — *re-parse once per drink, never once per byte*, with
`crew/moon/gen.l` a single 360 KB form — cannot arise.

### the laws, and why they came first

A reader replacement is tractable only with an exact-equivalence oracle, and a semantic change
in flight destroys it: every diff becomes a hand triage of "the new reader is broken" against
"right and deliberately different". `test/reader.l` is the conformance suite, and
`test/host/rdiff.l` is the differential — ⚠ it compares p0 against p1 on the **pure lisp
subset**, because a comparison that cannot fail proves nothing.

Two laws worth carrying:

* **`+` and `-` are runs like every other punctuation.** The one exception is that a numeral has
  to start somewhere: a digit (or a `.`, for `-.5`) right after them makes a NUMBER. So `--5` is
  `-(-5)` and `-x` is negate x, symmetric with `!!5` and `!x`. ⚠ The kebab law is not the reason
  `-x` could have been a name — it governs the INSIDE of a name-led token (`old-thing`,
  `nl->sp`), on tokens whose LEADING char is alnum/`_`.
* **A digit-led token carrying a sigil is a RUN, not a name.** `2?<>x` is the run `2?<>` glued to
  `x`, which opfix factors to `(two? (cap (cup x)))`. A digit breaks `p1-run`, so this run is cut
  out of the whole token instead — one past its LAST sigil, digits and all — and the numeral lane
  is asked first, so `1.5` and `0.5` are never runs. ⚠ **p0 does not implement this**, so nothing
  the bootstrap reads (`love/*.l`, `core/*.l`) may spell one; `rdiff.l` cannot catch it, being
  scoped to the pure lisp subset.
* **An integer literal reads the same in every build.** All three bases go through the reader's
  own `ai_big_read_dec`/`_hex`/`_oct`, so a literal is a fixnum, a box or a bignum **by its
  VALUE** and never by what a libc did with an overflow. ⚠ Before that, `0xffffffff80200000`
  read as three different numbers across mooncc+nolibc, system cc+glibc and wasm — the same
  source text — and the tree had voted around it three separate times.

---

## part II — the port surface

### a port's head is three words, and the vtable is its type

Word 1 holds the **vtable**, and nothing else answers what a port is. The descriptor lives in
`struct ai_fio`, reachable only through `ai_io_fd`, which asks the vt first and answers -1 for
every port with no device behind it. The four doors that are not a device are named statics
beside their own `readn`/`writen`.

That the fd was once the type made three things wrong at once: it was the OS handle, the type
tag, and an index into a synthetic-port array. ⚠ A test of `fd >= 0` reads any port whose word 1
happens to be non-negative as a buffered one and dereferences its word 3 — a charlist head — as
a string. The test is `vt == &ai_fd_port_vt`, which only love.c ever writes.

`close` hands the port `ai_closed_vt`, so **the swap IS the close** and the finalizer's "is there
an fd" is the same question everyone else asks.

⚠ **Every vtable address belongs in `image_immortals`.** A port's head carries a binary pointer,
and only an index survives a bake; without the entries an imaged port rides absolute on the
base-delta path. The witness is a live `tap` and `jug` baked and woken in a fresh process.

prel still builds `tap` and `jug`: C hands the two addresses over as book globals (`ci-vt`,
`to-vt`) exactly the way it hands over `lvm_quote`, and `love/egg.l` mops both noms at birth.

⚠ **A jug's backing starts EMPTY, and that is its whole isolation.** `to_writen` fills the
backing string IN PLACE while it has room, so a literal there would be ONE string every jug
shares — two live jugs then wrote over each other until the first one grew, and the second one's
first bytes were whatever the first had put down. spec.l's io section holds the two-jugs law.

### in, out and err are NAMES, and a task seats its own

`(wear (i o e))` re-seats the three for the RUNNING TASK; `(wear ())` hands the console back and
`(worn ())` answers what is worn. It is `seal-hook`'s second dynamic slot (`g->hot_io`, love.h),
carried exactly the way the help is: saved into the task's node, restored on the switch,
inherited at spawn, and **zero — the console — is the steady state**, so an op that is not
routing tests one word. `wear` replaces the whole triple; an element that is not a port keeps
the console, so re-seating stdout alone reads `(() o ())` and a caller who wants to keep an
in it already wears has to name it.

Why a slot rather than a rebind: `in`/`out`/`err` are C addresses in `image_immortals`, so every
holder — the book, a folded compile-time reference, a baked image — has the same pointer, and no
rebinding of the NAME could reach compiled code. And why per task rather than `dup2`: dup2 is the
process's, so two tasks cannot each have their own fd 1.

⚠ **The routing is OP-LEVEL, never identity-level.** `io_route` runs in the port ops' prologues
and rewrites the operand IN PLACE (the ops re-read it across a GC edge, so the routed port has to
be what the stack holds); `id?`, `peek`, `hot?` and the image all still answer the static. They
have to: prel's `tap`/`jug` read the port head by index. The two C-internal writers that name
`ai_stderr` outright (the missing-nom face, the scare face) are not ops and do not route: a
raise says its piece to the real console whatever the task is wearing.

⚠ **A jug is not an fd.** It takes what love says through `out` and nothing a child process
writes — a captured body that may `spawn` still wants a pipe. And a forked child must drop what
its parent wears (lush's `sh-spawn1` does, first thing): its stdio is the fds dup2 just laid.

⚠ **Nothing worn survives a bake.** A worn port names an fd, which means nothing in a new
process, so the wake clears the slot — the parked ring's rule, for the parked ring's reason.

**The end is not a state a port remembers.** There is no `eof_seen` latch — a spent device
answers `-1` to every ask, which love.h's `readn` contract states as an obligation on the
DEVICE, with `test/front/io.l`'s law 3 the witness that reddens if a frontend ever answers the
end once and then goes quiet.

### `readn` is the whole read door

`getc` does not exist. `readn`'s contract is the answer: **>0 = bytes, 0 = nothing waiting right
now, -1 = end**. A fourth answer on `getc` would have been two spellings of one fact in nine
places.

What that shape buys:

* **no frontend spins.** A device that computes "nothing ready" answers 0 rather than looping
  inside a VM op.
* **the would-block branch is the only branch.** It used to need a fault injector to reach; now
  every quiet device on every frontend comes through it, so the whole corpus walks it and
  `test_front` gates the sharp case (ready said go, the device said no).
* **one sentinel, one meaning.** `-1 = EOF` and `-1 = no data` are spelled apart.
* ⚠ **`readn` fills an `unsigned char*`**, so a charlist element lands as its low byte. A raw
  charm handed through would make a `-1` element read as EOF — `(flow (tap '(-1 65)))` must be
  2 long.
* **a NULL vt slot means NO METHOD**, and the dispatcher answers for it (no `readn` reads the
  end). No noop stubs.

⚠ The price: a static port reads one byte per call and used to pay a per-call `O_NONBLOCK`
toggle beside it, so `love < corpus.l` spent **3.8M syscalls** on 953 KB where the same corpus
as a file spent 23K, and ran 1.9× slower for it. Both halves are gone now, and they went
separately, because a door that cannot lend one can still lend the other. What the frontend
takes it gives back at `quit`, at `exec`, and at the end of `main` — `stdin_give`, three sites,
all holding `g`, which is why none of this needs an `atexit` or a global.

**A seekable fd 0 lends its bytes.** Not a buffer on the static — a **borrowed** one: a heap bio
parked in `g->inport`, which `rbio_of` reads *through* the static. `in` keeps its identity, its
one-byte face, and its position; only the device gulps. 3.8M → 23K, an exact match for the file
lane, and what remains of that gap is `trickle`'s per-byte promise, not I/O.

**A pipe lends its blocking bit.** No run is possible — nothing puts a pipe back — but the
*toggle* costs nothing to hoist, because a one-byte read leaves the fd exactly where the reader
is. `O_NONBLOCK` goes on once, the old flags into `g->inflag`, and `fd_readn` skips the dance for
that one fd: 3.8M → 976K, with 2,859,623 `fcntl` becoming 35. Worth ~3% on the corpus, which is
compute-bound, and **0.96 s → 0.65 s** on a load that only reads.

⚠ **A tty gets neither.** A human types, so syscalls-per-byte buys nothing, and a terminal handed
back nonblocking is the one version of this that breaks the user's shell. It is also the only
lane where the flag is genuinely shared with something that will read again.

The run is camp 2's bargain from part III taken exactly where it is free; the bit is the part of
that bargain nobody had to pay in the first place. `test_stdinbuf` runs one program down each
door and diffs — the only way to catch a lane that starts running ahead — and reads the exec'd
child's `/proc/self/fdinfo/0` to prove fd 0 was handed on blocking.

⚠ Ungated: no gate feeds a keystroke to inle, virt, mps2 or teensy — the qemu harness runs
`</dev/null` on purpose, since a non-definite stdin hangs it. Their `readn` is exercised by
dispatch and by review, not by a byte arriving.

### `writen` is the whole write door

The vt is **three slots** — `flush`, `writen`, `readn`. Every `putc` implementation was its own
`writen` at n = 1.

The slot's real job was that **`putc` was the only write path allowed to allocate**, which is
why the bulk lane answered "0 = no room without an alloc" and bounced back through it. `writen`
allocates — it takes the frame BY ADDRESS (`struct ai **`) so a scare rides out — and there is
nothing left to bounce to.

### backpressure: the write run is a buffer, not a queue

A nonblocking write door hands the remainder to `wbuf` instead of waiting, which stops bytes
being lost and also means **`say` never refuses**: write to a socket whose peer has stopped
reading and every call succeeds instantly while the run grows forever.

So a write op that would push the run past its own size (`ai_iobuf`) **drains first, and parks
if the run is still that big**. Three ops carry the guard — `say`, `put`, `putx` — and re-running
is free, because the park sits where nothing has been consumed yet.

What it does NOT bound: a single `say` longer than the buffer still leaves its own tail in the
run, because a park mid-string is not re-runnable. The run holds at most **one buffer plus the
tail of one say**, never an accumulation across ops.

⚠ **The bound is the buffer's own size, not a new number** — `ai_iobuf` is already the tree's
chunk size, and a bound on a BUFFER is not a cap on how many things love can be doing.

⚠ **"Zero pending" is the wrong rule.** Parking whenever the run is non-empty needs a drain
before every write, and `zputc` strokes only when the buffer fills — so a `put` loop becomes one
`write(2)` per byte. The threshold is what keeps the byte lane's batching, and it is why the
drain sits BEHIND the test on `put`/`putx` and in front of it on `say`.

⚠ One shape changes and it is an OS-level deadlock coming back into view: a single task writing
more than a pipe holds, to a pipe only it will read, now waits. Unbounded buffering was the
anomaly that hid it. A peer TASK still drains fine, because this parks rather than blocking.

### who waits — `make waits`

`mk/tools/waits.l` (on the fast gate, beside `vmret`) reads every tracked `.c` file and answers
**who waits**: every call to `ai_sleep`, `ai_wait_fds`, `ai_fd_drain` or love.c's `wait_one`
must name a function on a roster carried in the tool, each with the sentence that earns it.
Reading the roster IS the invariant:

| | |
|---|---|
| `wait_one -> ai_wait_fds` | the scheduler's one-fd shim; an `lvm_` frame may hold no scratch |
| `lvm_yield_sw_mono -> wait_one` / `-> ai_sleep` | the monotask scheduler, with a parked fd and without |
| `yield_sw_wait -> ai_wait_fds` | THE wait: every parked task and the nearest timer, one call |
| `io_close -> ai_fd_drain` | ⚠ the finalizer, the write side's one remaining unbounded wait |
| teensy41 `main -> ai_sleep` | the panic blink after a fatal shell exit |

⚠ **It reads the C as WRITTEN, never the binary, and that is the design.** Every `io_*` and `z*`
function in love.c is `static`, so the compiler inlines them and their names leave the ELF; a
call graph read off the image cannot find the caller it is looking for, and would answer GREEN
forever. `vmret` can disassemble only because `lvm_*` aps are addressed and survive. **A gate
that cannot see its subject reports success**, which is the worst answer a gate gives.

Reading the source also makes one edge free: `Ap(lvm_yield_sw, g)` names `Ap`, never
`lvm_yield_sw`. A tail-jump is not a call — the op returns to the trampoline and the scheduler
waits in *its own* frame — so the shape the arc exists to permit is the shape the rule already
ignores.

⚠ What it does not see: the four hooks by NAME, never blocking in general. A raw `read(2)`, a
fresh primitive, or a hook reached through a function pointer all walk past it. It says *the
named waits are where we put them*, which is the thing that regresses.

### parking, and the nif floor

A nif that would block **parks the task** rather than waiting: the catcher, `connect`'s
handshake, and the resolver, which lives in love rather than C. `cue?` is the dual of the park
law — would `see` answer WITHOUT parking? — generalized off stdin, so a device that is ready and
a device that is spent are asked the same way.

`k_sources_max` is a door rather than a rule: a frontend states its own capacity.

⚠ **A task must not sleep on a quiet fd over a full buffer.** Readiness is a question about the
port, not about the device: bytes already in `rbuf` make the task runnable no matter what the fd
says (the scheduler's side of this is's syscall-free wake pass).

---

## part III — who owns the bytes

A lazy memoized chain over an fd is a COPY with its own position, and nothing in the original
design considered two readers of one stdin.

### the problem, and how it went

`(reads in)` flowed stdin, so a form inside the script that read `in` found nothing — and the
next form still ran, because the colist held it:

```
$ printf '(say out (+ "rest: [" (+ (slurp in) "]")))\n(say out "second form ran")\n' | love
rest: []second form ran            # what it used to do
rest: [(say out "second form ran") # what it does now -- the slurp takes the tail, so
]                                  # the second form is CONSUMED and never runs
```

**Camp 3 is reached.** `reads` (`love/bao.l`) asks whether the port is `in` and, if it is,
`trickle`s it instead — one byte per force, so the reader never runs past the form it is on
and the port's position is the only position there is. `flow` is kept for a port we own alone
(a file, a tap), where running ahead is free.

⚠ That `(id? p in)` test is why `in` is **borrowed through rather than rebound** when a seat
buffers it (part II): `reads` folded its own `in` at egg-compile time, so binding a fresh object
to the name would fail the test and silently go back to gulping.

**It was ONE call site** (`love/cli.l`). The tracked stdin readers look like a migration and are
not: most are kore's `(? (f = "-") (slurp in) (uread f))` idiom, which takes ALL of it and
leaves no residue, and the rest are interactive key decoders that use one byte immediately. A
session-wide ownership protocol to fix one line is the wrong size of answer.

### what our peers do, measured

| camp | who | behaviour | cost |
|---|---|---|---|
| **read the whole program first** | python, node, perl, ruby | script consumed entirely; `stdin` reads empty; every later line still runs | no ownership question, because there is no interleaving |
| **interleave, keep the FD exact** | bash, zsh, tclsh | a command that reads stdin gets the remainder and the shell stops | an `lseek` probe at startup, then seek-back on files and **one `read()` per byte** on pipes, forever |
| **interleave at DATUM granularity, one port** | guile | `(read)` takes exactly the next datum and execution carries on | none |

guile works because the REPL's reader and the user's `(read)` are **the same port with one
position**. Nothing is copied, so nothing can disagree.

⚠ `kore sh` on a piped script does single-byte reads and is correct for a genuine spawned
child — **but that correctness is an accident**: lush does not read stdin at all, it goes
through bao's editor, which reads one byte at a time because it was written for a TTY.

### the semantics we want, and the one thing decided

**Camp 3**: one port, one buffer, one position. A reader takes exactly what it needs; in-process
readers share coherently. love had this before the lift — the culprit was not the colist and not
the reader, it was **the GULP**, which predates both: it takes everything ready and hands back a
head DETACHED from the port. Two positions where there was one.

**Decided: persistence is a BENEFIT, not a bug to trade away.** A charlist is a persistent value;
a port position is ephemeral. guile has only the position — its buffer is not a value. love has
handed the value out, and that is what made p1 clean (`once` exists precisely so forcing twice
is free). So the way out had to keep the persistent value and give the port back its position,
rather than choosing between them — and `trickle` is that: the charlist is still a value, and
the port never runs ahead of it.

What is left is a COST, not a question: trickle mints a `once` per byte, which is now the whole
of the gap between stdin and a file (~2.2 µs/byte over the corpus) — the device is at parity on
the seekable door. A run-at-a-time charlist that stays attached to its port closes it; that is
part IV. Everything else in this part is standing design.

⚠ The pipe additionally keeps camp 2's per-byte *read*, for camp 2's reason: a child must find fd 0
where our reader stopped, and nothing puts a pipe back. But that is one syscall per byte, not four —
the `O_NONBLOCK` toggle beside it was never part of the bargain and no longer runs (part II). Where
bash's row above says "one `read()` per byte on pipes, forever", ours says it without the forever:
part IV's rung 3 is why.

## part IV — closing the gap to a file

Part III left one cost. The goal is **parity with a file argument on both stdin doors**, and the
gap is two independent costs with two independent fixes.

| door | 970 KB corpus | over a file | what the excess is |
|---|---|---|---|
| `love f.l` | 5.59s | — | the work itself |
| `love < f.l` | 7.66s | +2.07s | **all** of it love-level: one `once` — a whole tablet — per byte. The device is already at parity; the borrowed run (part II) sees to that. |
| `cat f.l \| love` | 8.31s | +2.72s | that same 2.07s, **plus** ~0.65s of one-byte `read(2)`: `stdin_take` lends a run to a *seekable* door only, so a pipe has no buffer to read through |

Three instruments agree on the per-byte figure: the table above, ~2.2 µs/byte measured directly,
and moon-diff's ~0.5s of kernel time on the pipe lane.

### why `flow` is not already the answer

`flow` **is** the run-at-a-time port-backed charlist part III asks for, and it landed. It is not
used on `in` for a reason with two halves:

* **it would be wrong.** `chug` hands out a head detached from the port, so `ai_io_pending` stops
  counting those bytes and `stdin_give`'s seek-back comes up short — two positions where there was
  one, which is part III's bug arriving by the other door.
* **it would also be SLOWER.** `chug` guards with `bio_of`, not `rbio_of`, so it refuses the
  borrowed run and counts only the pushback byte: one byte per gulp, with a `see`, an `unsee`, a
  `tally` and a cons loop around each. `flow in` degenerates to worse than `trickle`. ⚠ `slurp`
  gulps the same way and pays the same handicap.

So the fix is not "use flow on in". What it is took three measurements to find, and two of them
refuted the obvious answers — they are below, because each cost a build and none is guessable.

### three measurements, and what they killed

Instruments: `perf -e instructions` on the 971,922-byte corpus, stable to ~0.01% across runs. Wall
clock is reported only where syscalls make it diverge, since `instructions:u` cannot see them.
⚠ Read these off a BAKED love; a relink leaves it unbaked and every figure moves.

**1. The ceiling is FULL PARITY, and the seekable door beats the file.** Flip `chug` to `rbio_of`
(and `ai_io_pending` and `ai_io_read_drain` with it — all three guard separately, so one alone is
incoherent) and let `reads` gulp. The redirect gap goes **+2.327 G → −0.060 G**: 37.420 G against
the file door's 37.480 G, 4157 ms against 4266 ms. The whole gap is recoverable, so parity is the
right target and nothing about the architecture impedes it.

**2. It is not the `once` TABLET.** `once` mints a whole tablet per byte (`love/prel.l`), which
looks like the cost and is not. Swapping it for a `spin 1` cell with `poke` made it **WORSE:
+2.327 G → +2.577 G** — `poke`'s write barrier and `spin`'s memset together cost more than an
empty tablet. The per-byte price is the STRUCTURE, one promise per byte, not the memo's shape.
⚠ Do not re-open this; the gate is green either way, so only the counter tells you.

**3. The pipe must keep trickling, and gulping it is a catastrophe.** `athand` is NULL for the fd
port vt, so a port with no bio yields ONE byte per chug — and `flow` around a one-byte gulp costs a
`see`, an `unsee`, a `tally` and a cons loop for each. Making `reads` gulp everything took the pipe
**+2.469 G → +6.442 G**. So the fork is not "is it `in`?" but **"does this port hold a run?"**:
a seekable stdin has a borrowed one and can gulp, a pipe has none and must drip until rung 3
lends it one.

### ⚠ the constraint is IN-PROCESS readers, not the child

The gulp's failure is not the inherited fd — `unchug` answers that. It is that `reads` holding
bytes in its charlist makes them invisible to the port. `test_stdinbuf`'s own program says it:

```
pipe door:      rest: [(say out "tail form")     # correct: slurp takes the remainder
seekable door:  rest: []tail form                # gulped -- (slurp in) found an EMPTY port
```

That is part III's bug exactly, and it rules out the give-back-the-residue shape this section first
proposed. Unchugging before each eval IS correct, but it means one chug per FORM, re-spreading the
same run every time — order 100 cons per source byte, worse than what it replaces. **Any read-ahead
held as a value is a second position**, which is the one thing camp 3 does not allow.

So `sound` has to take one form off the port without holding read-ahead. It has no such door: the
port door is `(p1-flow x)`, a charlist like the others. The only shape that is both fast and safe
is a POSITION door — read datum `i` out of a text and answer where it ended, so the run is walked
once without being re-spread and nothing is retained across the eval.

⚠ That is the mechanism of the reverted rung 9, and it is NOT what got rung 9 reverted (the walk's
representation was). Reaching for it is a deliberate decision, not a slide back into a known
mistake — see "what must not be repeated" below.

### the rungs

**1 — `unchug`, the give-back. ✅ LANDED.** `b->rpos` already IS the reader's position and
`ai_io_pending` is derived from it, so handing bytes back is a rewind of one word. `(unchug p n)`
is **`unsee` at n bytes**: it un-reads the last `n` this port gave out and answers how many went
back, a short answer being the refusal. It reaches through `rbio_of`, so the run borrowed under a
static counts — which is the entire point, since `stdin_give`'s seek reads `ai_io_pending`.

⚠ It rewinds the POSITION, so what comes back is whatever the run last gave, not a remembered
chug; a refill replaces `rbuf` and resets `rpos`, so bytes are recoverable only until the next
read here. The clamp to `rpos` is what makes a stale ask answer what is really there.

⚠ **`bio_of` would pass every file-port law and still be wrong.** A heap file port *is* a bio, so
the unit laws in `test/io.l` cannot tell the two lookups apart — only an inherited fd can. Hence
the `test_stdinbuf` clause, whose law is the CONTRAST: the same program with and without the
give-back must hand `cat` ten bytes and nine. Injecting `bio_of` collapses both to nine while
`test/io.l` stays green, which is what that clause exists to catch.

Rung 1 buys no speed on its own — it is the door rung 2 spends.

**2 — a COUNTED flowing charlist, and the port positioned absolutely. ✅ LANDED, 70%.** ⚠ REWRITTEN
TWICE. The first draft said "chug a run, sound the text, unchug the residue"; the second aligned
that window to newlines. Both are wrong, and the reason is worth keeping:

> **A finite window cannot carry the reader's state.** `sound` answers a CLEAN END -- not `torn` --
> when text stops inside a `;` comment, so a window ending mid-comment loses the fact, and the next
> window reads the prose as code. The corpus answers `;; missing so`, off an English sentence.
> Aligning windows to newlines fixes one instance and the tail past the last newline is itself a
> part-line, so it fails again one level down. There is no window size that is safe.

What landed instead keeps `flow`'s shape -- a **lazy tail that gulps on demand**, so there is no
boundary to straddle and `torn` and the clean end mean what they say -- and adds only a count:

* every gulp adds its run's length to a tablet slot, so the walk can ask how many bytes have been
  DRAWN and put the port exactly where the walk stands.
* consumed per form comes from walking forward to the residue by identity -- O(consumed), so the
  sum over the stream is the stream. ⚠ never `(tally >r)`: the residue is lazy, so that forces the
  whole rest of the stream. ⚠ and the walk must FORCE at each step -- a cons holds its tail as an
  unforced promise, so a plain `cup` walk stops dead at a gulp boundary and spins on `()`.
* the port is positioned ABSOLUTELY (a delta off `inhand`), because relative does not compose: a
  caller that gave back is behind its own position and must step forward again. That is why
  `unchug` is signed. Give-back-only makes the second step a rewind to the run's start, and the
  reader re-reads the whole stream -- through a green `make test`.
* after each eval the port goes FORWARD again to the walk's end, or the next gulp draws the bytes
  we already hold a second time and the charlist grows with duplicates.
* `inhand` before and after the eval says whether the form itself drew on `p`. If it did, `p` is
  the truth and our charlist is stale: carry the one lookahead byte `p` will not hand over twice,
  and read the rest afresh.

⚠ ONE BYTE SHORT, ON PURPOSE. The byte-exact lane reads the residue's head before it evals
(trickle's cons holds it), so it too runs one ahead. Matching that is what keeps the doors
byte-identical, and rung 2 owes no semantic change.

Measured on one corpus, `perf -e instructions`, both loves:

| door | rung 1 gap | rung 2 gap | absolute |
|---|---|---|---|
| `love f.l` | — | — | +0.009% |
| `love < f.l` | +2,325 M (+50.7% wall) | **+694 M (+1.7% wall** -- 3953 ms vs 3886 ms) | **-4.09%** |
| `cat f.l \| love` | +2,469 M | +2,557 M | +0.23% |

⚠ READ THE ABSOLUTE COLUMN. A GAP is a difference between two ~40 G numbers, so the pipe's grew
3.6% off a 0.23% move in the whole — the size of a layout shift, which adding a nif causes by moving
`nifs[]` and the def table. A gap only means something next to its absolute.

**3 — the pipe gets a run too, and the handoff carries the residue.** ✅ LANDED. The pipe was the
whole of the remaining lane, not a leftover: with no bio it paid both costs. Lending it one meant
answering the inheritance question, and part III had borrowed its answer from bash along with the
framing:

> nothing puts a pipe back

True, and beside the point. The residue does not need to be *undone*, it needs to be *delivered* —
`stdin_hand` forks a pumper, writes the bytes our reader did not take into a fresh pipe, splices
whatever the old fd 0 still brings, and dup2s the read end onto fd 0. bash pays per byte partly
because it has no fork to spare at the handoff; we do, and it costs one process per handoff, only
when a residue exists at all.

⚠ **RUNG 3 CHANGED NO `.l` AT ALL** — 64 lines of `host/main.c` — and measurement 3 is why. Because
`reads` forks on *"does this port hold a run?"* rather than *"is it `in`?"*, and because `start`
primes with a `see` before it asks, lending the pipe a bio routes it into rung 2's counted-flow lane
by itself. The tty asks the same question, gets `0`, and keeps trickling. Getting that predicate
right in rung 2 is what made rung 3 a frontend change.

Exec-replacement was written up here as the one holdout, and it isn't: the pumper is a *separate*
child, so it survives our being overwritten. `host_exec` calls `stdin_hand` and the child reads the
new fd 0 — and a *failed* exec is now better than before too, since love resumes reading the pumped
pipe rather than a drained one.

⚠ THE REAL HOLDOUT is narrower and is not about us at all: a peer holding fd 0 from **before** us
(`cat f | { love a.l; love b.l; }`) has its own descriptor, so no dup2 of ours can reach it. There
is no rewind and no substitution — a pipe's read offset is shared kernel state. So the give-back
splits by who is asking, and the split is the law `stdin_hand` states:

| asker | the door's answer |
|---|---|
| an in-process reader (`(slurp in)`) | exact, always — zgetc drains the run before the device |
| a child we fork or exec | exact — the pumper delivers |
| a peer sharing fd 0 from before us | **past reach on a pipe**, whatever we do |

`lvm_exit` and main's tail therefore call plain `stdin_give`, not `stdin_hand`: at our own exit
nothing of ours is left to pump and the dup2 would be private to a process about to vanish. A
pumper forked there would be pure waste and a stray process.

**What it bought** (972,400-byte corpus, both binaries baked, one sitting):

| door | rung 2 | rung 3 | |
|---|---|---|---|
| `love f.l` | 37,487,781,134 (4.176 s) | 37,489,976,861 (4.144 s) | the work itself |
| `love < f.l` | 38,181,520,575 | 38,180,261,078 (4.205 s) | untouched, as intended |
| `cat f.l \| love` | 40,046,481,350 (6.518 s) | **38,184,042,767 (4.267 s)** | gap +2.559 G → **+0.694 G**, elapsed 1.561× → **1.030×** |

`read(2)` on fd 0 over the corpus: **972,400 → 239**, which is the file door's own count to the
call. And the two stdin doors now land within 4 M instructions of each other — 0.01%, inside the
run-to-run spread — so there is one stdin lane again, not two.

**What is left** is the +697 M both stdin doors carry over the file door (~717 instructions/byte),
which rung 2 also had and neither rung explains. That is its own rung, and it wants `perf` before
it wants a patch.

**4 — the per-byte promise. NOT STARTED; here is the `perf` it asked for.** Re-measured on a
1,021,645-byte corpus, both loves baked:

| door | absolute | over the file door |
|---|---|---|
| `love f.l` | 32,484,675,263 | — |
| `love < f.l` | 33,415,991,237 | +931 M (+2.87%, 912 ins/byte) |
| `cat f.l \| love` | 33,416,129,526 | +931 M — **0.0004% off the redirect** |

So rung 3 holds: there is one stdin lane, not two, and it stays one. The gap per byte is a little
above rung 3's 717 because the corpus grew; it is the same cost.

⚠ **THERE IS NO HOTSPOT, AND THAT IS THE FINDING.** A profile of both doors over the same corpus
puts the same VM ops at the same shares — no io routine appears on the pipe side at all. The whole
+2.87% is spread, and what moves is the shape of the spread:

| what moves | file | pipe |
|---|---|---|
| kernel time | 0.00% | 0.83% |
| `evac_data` + `evac_thread` (the collector COPYING) | 0.50% | 0.87% |
| `lvm_litp`, `lvm_ret`, `lvm_eq`, `lvm_aa`, `lvm_add` | — | each +0.25..0.45 |

The collector copying more, plus a flat tax on every op, is one promise per byte living long enough
to be a major's problem — measurement 2's conclusion arriving by the other instrument. It is not a
routine to optimise, so there is nothing here a patch can aim at: closing it means not building the
structure, which is the position door part IV already named, and the deliberate reach that section
flags. Do not read this rung as cheap because no hotspot was found.

### what must not be repeated

Rung 9 (`65359706`) did a version of this, went green on `test_slow` at −5.2% instructions, and was
**reverted** (`0306fdea`). `435631a5` landed the narrow string door instead and says why: *"this is
a door and not the reverted rung 9, which tried to make the WALK take a string."*

* ⚠ **the residue stays a charlist, and the charlist door stays open.** Rungs 1–3 change who walks
  a chunk, never what a charlist IS. Rung 9 changed the representation; that is what broke.
* ⚠ **`crew/moon/lex.l`'s `dec2flo` rides `sound`.** Break that door and every mooncc float
  constant becomes an infinity, four layers down, wearing mooncc's face.
* ⚠ **`love/p1.l` carries its OWN `flow`** — the pure-lisp reader that runs before prel exists. A
  change to prel's copy that skips p1's is a difference between the two readers.
* ⚠ **the kernel's heap is budgeted.** K_TEST explodes the baked corpus under a tap; one cons per
  byte OOM'd it once, and `make test` cannot see that.
* ⚠ **`in` is never rebound** — `reads` folded its `(id? p in)` at egg-compile time.
* ⚠ **the tty keeps neither optimization.** A terminal handed back nonblocking breaks the user's
  next shell line — and with no run, `reads` keeps trickling, which is what a prompt wants anyway.

The gates that pin it: `test_stdinbuf` (one program down each door, diffed, *plus* the handoff asked
of the pipe output directly and a 200,000-byte splice past the first gulp), `test_stdincorpus` (the
whole corpus down all three doors on both loves), `test/io.l`, `test/host/parked.l`, `test_filemode`,
`test_clay` (the registry is generated), `test_fixpoint` (a new nif rebuilds the egg), and
`test_kernel` for the heap.

⚠ Each of the four `test_stdinbuf` handoff laws was proven load-bearing by injection, not by
reading. Dropping the delivery reddens the diff; delivering the residue and skipping the splice
reports `4079 of 200000` — which is also the measurement that says the residue is exactly one gulp.
