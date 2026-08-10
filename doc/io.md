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
  trailing-`-` shed, `[`/`{` synonyms, the prime inside names, the number tower and the
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

⚠ The price, measured and accepted: the unbuffered statics pay a per-call `O_NONBLOCK` toggle,
so the syscall count per byte went 2 → 4 while the read count did not move. The `fcntl` pair is
skipped when the fd already says nonblocking; flags are **not** cached for an inherited fd, and
the statics do **not** get a buffer — that would make the repl swallow the line after the one it
is reading, and re-open part III.

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

The live question. A lazy memoized chain over an fd is a COPY with its own position, and nothing
in the design considered two readers of one stdin.

### the problem

`(reads in)` flows stdin, so a form inside the script that reads `in` finds nothing — and the
next form still runs, because the colist held it:

```
$ printf '(say out (+ "rest: [" (+ (slurp in) "]")))\n(say out "second form ran")\n' | love
rest: []second form ran
```

**It is ONE call site** (`love/cli.l`). The tracked stdin readers look like a migration and are
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
readers share coherently. love had this before the lift — the culprit is not the colist and not
the reader, it is **the GULP**, which predates both: it takes everything ready and hands back a
head DETACHED from the port. Two positions where there was one.

**Decided: persistence is a BENEFIT, not a bug to trade away.** A charlist is a persistent value;
a port position is ephemeral. guile has only the position — its buffer is not a value. love has
handed the value out, and that is what made p1 clean (`once` exists precisely so forcing twice
is free). So the way out has to keep the persistent value and give the port back its position,
rather than choosing between them.

That is the open work. Everything else in this document is standing design.
