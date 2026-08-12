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

`tools/waits.l` (on the fast gate, beside `vmret`) reads every tracked `.c` file and answers
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
says (the scheduler's side of this is doc/sched.md's syscall-free wake pass).

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

So the fix is not "use flow on in". It is to let `chug` see the borrowed run, and to give the port
back the bytes the reader did not use.

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

**2 — `chug` reads through the borrowed run, and `reads` walks a chunk as TEXT.** With rung 1 in
hand, `chug`'s guard becomes `rbio_of`. Then `reads` on `in` chugs a run, `sound`s the text (the
string door that landed in `435631a5`), and `unchug`s the residue. The per-byte `once` is gone —
the cost becomes one chug, one sound and one rewind per FORM. This takes `love < f.l` to parity.

**3 — the pipe gets a run too, and the handoff carries the residue.** The pipe is the whole of the
remaining lane, not a leftover: with no bio it pays both costs. Lending it one means answering the
inheritance question, and part III borrowed its answer from bash along with the framing:

> nothing puts a pipe back

True, and beside the point. The residue does not need to be *undone*, it needs to be *delivered*.
**Every love spawn is a `fork`** (host/posix.c, host/main.c) — the parent survives, so it can hand
the child a fresh pipe and pump `residue ++ rest` into it. bash pays per byte partly because it
declined to build that; we already own the scheduler and the port machinery.

⚠ THE ONE HOLDOUT is exec-replacement (`stdin_give` then `execvp` in host/main.c — the lane behind
`love up`, gdb and the qemu run targets): love overwrites itself, so nothing is left to pump. That
lane keeps a byte-at-a-time stdin, or drains its residue into a memfd and dup2s it — a bounded cost
at the handoff either way. It is narrow, and it is not the ordinary script.

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
  next shell line.

The gates that pin it: `test_stdinbuf` (one program down each door, diffed — a lane that starts
running ahead fails there), `test/io.l`, `test/host/parked.l`, `test_filemode`, `test_clay` (the
registry is generated), `test_fixpoint` (a new nif rebuilds the egg), and `test_kernel` for the heap.
