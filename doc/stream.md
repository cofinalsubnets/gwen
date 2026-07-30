# stream.md — path B: the coinductive byte-stream io rework

A design study (notes only — build in a dedicated core `love.c` session). This is
**bao's path B**: the io/scheduler rework that unblocks
the rlwrap wiring (`edraw` into `wrap`) which was *built and reverted* because it
deadlocks the cooperative scheduler (see `crew/bao.md` §"path B", and the bao memory).

The user's standing direction: **fix this as the real io rework — the coinductive
stream + `select`/`ready?` — not the half-duplex shortcut.** This doc is the design
that session implements.

> **§4 already landed, without the stream** (doc/reader.md rung 6c, 2026-07-30).
> The reader is a pure fold now — `sound : text -> (datum . rest) | () | torn` —
> and the whole more-bit / port-back / help-continuation protocol is deleted, as
> §4 predicted. It took no new C: the residue moved to the CALLER rather than
> into a lookahead cons. So what is left here is the part §4 was never really
> about — laziness, memoization, `ready?`/`select`, and the scheduler/`wrap`
> rework that is path B's actual motivation. When the charlist `sound` walks
> becomes a lazy `source`, neither the reader nor any of its callers change
> again; `reads` (love/bao.l) is the one place a port becomes text, and it is the
> one place that would.
>
> One piece landed early too: **`cue?` is general now** — it answers "would `see`
> park?" for any port, which is `ready?` for the fd case (§5).

## 0. The symptom

`wrap` today is a *transparent* pty pump and works: `stdin -> master` in one task,
`master -> stdout` in another, the two park on different fds and interleave through
the cooperative scheduler (`love/bao.l:43`). The rlwrap upgrade swaps the input side
for a line editor — `feedlines` reads keys via `edraw` and renders the edited line
to `out`, sending to the child only on Enter:

```
(feedlines ip m)
  (: line (edraw ip "")
     (? (id? line eofsym) (put m 4)
        (: _ (each line (\ c (put m c))) _ (put m 10) _ (flush m)
           (feedlines ip m))))
```

Run `feedlines` (reads `in`, writes `out`) in one task and `pump m out` (reads
`m`, writes `out`) in the other and it **hangs after one keystroke.** The *only*
difference from the working transparent pump is that the editor task now writes to
`out` (the render) instead of feeding the master. Single-task `edraw` on `in`
works; a lone `see`-in-a-spawned-task works; the concurrent two-task park is what
breaks. The exact proximate trigger could not be pinned statically (it wants a
debug-build instrument) — but it does not need to be, because the substrate it
rides on is structurally wrong, and path B removes that substrate entirely.

## 1. The diagnosis — four structural defects in the port surface

The `see`/`unsee`/`end?`/`key?` surface is **POSIX wrongly embedded in the
generic core.** Four leaks, each a place where the cooperative model and the
read model disagree:

1. **`ungetc_buf` is invisible to the readiness check.** `lvm_fgetc` (love.c:3168)
   parks the task whenever `!ai_ready(fd)` — *without first consulting the port's
   `ungetc_buf`*. The editor's escape decoder (`besc`/`besc1`, `love/bao.l:69-86`)
   reads bytes ahead; any pushed-back byte is then held in `ungetc_buf` where the
   poll-based readiness test cannot see it. A task can be **parked with the very
   byte it needs already in hand.** In monotask the `while(!ai_ready) ai_wait_fd`
   spin (`lvm_yield_sw_mono`, love.c:2128) is bailed out by the user's next
   keystroke; under two tasks, `yield_sw` hands control away and the byte is never
   reconsidered. (`key?` already gets this right — love.c:3663 tests
   `ungetc_buf != EOF || ai_ready(fd)`; `see` does not. The asymmetry is the bug.)

2. **The `-1` EOF sentinel is un-love.** Absence in love is the zero point `()`, not a
   magic integer; and `-1` collides with byte `0xFF`-as-`putcharm` reasoning and
   with the fact that byte `0` already nets nothing. EOF should ride the *container*
   (the stream is empty), in-band, as a value — which is exactly
   *floor-is-the-runtime*: a stream that bottoms out at `()` hands control back to
   `g`.

3. **fd/poll knowledge is smeared across the generic VM.** `next_wait_fd` is set
   *inside* `lvm_fgetc` (love.c:3172); the scheduler (`yield_sw_wait`/`find_runnable`,
   love.c:2135-2158) walks task nodes pulling `wait_fd` out of slot `N[4]`; `key?`
   hardcodes `ai_stdin` (love.c:3663). The generic tail-threaded VM should know
   *generic-apply + scheduler-yield* and nothing about file descriptors.

4. **The write side never yields.** `put`/`say`/`flush`/`puts`/`putc`
   (love.c:2595-2625) block synchronously in `zputc`/`zflush` and never park. In a
   cooperative scheduler a blocking write to a flow-controlled fd stalls the *whole
   VM* — and with the editor and the pump both writing `out`, plus a child that can
   fill its pty slave when the pump is slow, the write side is a latent
   single-point stall the reader-only model never had to reason about.

The deadlock is what you get when two independent reader tasks meet an inadequate
readiness model: pushed-back bytes the poll can't see (1), an out-of-band EOF that
forces sentinel-loops instead of emptiness tests (2), fd-awareness scattered so no
single place owns "this stream would block" (3), and writes that can stall everyone
(4). Patching any one leaves the others. Path B replaces the surface.

## 2. The boundary principle

> The core knows **generic-apply + scheduler-yield**. "Bytes out of an fd" is a
> **host** concern, presented as a value of an existing kind — the chain (a lazy
> one). fd-awareness meets the scheduler at **exactly one** site: the thunk that
> forces a stream's tail.

## 3. The `source` hot — a coinductive byte-stream

An fd (or an in-memory charlist, or the reader's consume surface) becomes a
`source`: a `hot` that **presents under `cap`/`cup`/`!`/`twop` as a lazy chain of
bytes.** It is a chain whose tail forces on demand and **memoizes** — and that memo
*is* the lookahead, so `ungetc` deletes (defect 1 gone).

```
(source fd)   ; a stream hot over an OS fd          (subsumes `in`, `out`'s read dual)
(sip charlist); the SAME primitive, fed from memory (subsumes today's `sip`)
```

Surface behaviour:

| form        | meaning                                                          |
|-------------|------------------------------------------------------------------|
| `(cap s)`   | the head byte (a charm `0..255`), forcing once if unforced       |
| `(cup s)`   | the tail `source` (forcing memoizes — call it twice, force once)  |
| `(! s)`     | true iff the stream is **at EOF** — test the *stream*, never a byte |
| `(twop s)`  | true iff non-empty (one more byte is available)                  |
| **EOF**     | the empty stream **is `()`** — the blue floor, control back to `g` |

Representation (a small hot, modelled on `ai_io`): `{ap=lvm_source, kind (fd |
mem), state (fd / remaining charlist), head (a forced charm, or an UNFORCED
sentinel), tail (the next source, lazily allocated on first `cup`)}`. The forced
`head` is the one-byte lookahead — there is no separate pushback buffer, no
`eof_seen` flag. EOF is observed by forcing and finding nothing: the source
*becomes* `()`.

## 4. `read` as a pure fold — the more-bit protocol collapses

With lookahead-as-cons-cell, the datum reader becomes a **pure parser**:

```
read : stream -> ()  |  (datum . stream')
```

- **EOF = `()`** is the floor — "nothing more to read" *is* the empty stream.
- **"Incomplete" stops being a control signal.** Today an incomplete read raises a
  condition routed through `help` with the *more* bit, and the reader hands the
  port back to be resumed when more bytes arrive (the more-bit / port-back
  protocol). Under path B, "incomplete" just means **you still hold the tail** —
  block-force it and continue when bytes land. **The stream *is* the continuation.**
  The whole `more`-bit / port-back / help-continuation read protocol deletes.
- **Mid-datum EOF is the genuine error** — an unterminated string or list, the
  stream ending inside a datum, is a real `scare` (not the `()` floor).

This unifies three secretly-one things — `sip` (charlist -> stream), an fd-stream,
and the reader's consume surface — behind one `cup` dispatch. "The same reader
serves data and code" becomes literally one input type.

## 5. The one genuinely-new primitive — `(select ss)` / `(ready? s)`

You cannot block on *two* streams with pure `cap`/`cup` (forcing one commits you to
it), so multiplexing needs one new thing. It is **stream-shaped, not fd-shaped** —
the real replacement for `key?`:

```
(ready? s)     ; true iff (cap s) would NOT block — generalizes key? to any stream
(select ss)    ; given a list of streams, block until >=1 is ready; return the
               ; ready one(s). The stream-level poll.
```

`ready?` generalizes `key?` (`key?` = `(ready? (source 0))`); both project onto the
host's `ai_ready(fd)` (host/main.c:42; kmain.c:234). `select` projects onto
`ai_wait_fds(fds, n, ticks)` (host/main.c:44; kmain.c:241) — gather each stream's
fd, poll, return the ready streams. This is the *only* place fd-awareness
legitimately re-enters, and it is contained in this one primitive plus the tail-
force thunk (§8). Design `select`'s exact return shape (first-ready vs all-ready
mask) when building — a first-ready `source` is the minimal version; an all-ready
list is the general one. Memory streams (`sip`) are always ready.

## 6. The write side — keep `dot`, add `sink`, don't rebuild `put`

Output is barely gummy. Keep `.`/`dot` (the generic tap writer). Add `(sink fd)`
only as the dual *target* — `(sink 1)` is `out`'s writer end — so a pump is
`(s->stream src) ... (sink dst)` symmetric. Do **not** rebuild `put`/`say`.
A write that would block can, if we want fairness, park through the same force/yield
discipline; but the first cut keeps writes synchronous and relies on the reader-
side fix to break the deadlock (the editor and pump both reading via streams is
what unsticks; the render writes are short bursts to a tty, not the stall site in
practice).

## 7. Delete / keep ledger

**Delete (once parity lands):** `lvm_fgetc`'s fd/park logic, `lvm_fungetc`, the
`ungetc_buf` and `eof_seen` fields of `ai_io`, `lvm_feof`, `lvm_key` (becomes
`ready?`), the `-1` EOF sentinel convention, the `more`-bit / port-back read
protocol in the reader and its `help` dispatch.

**Keep:** `ai_io_alloc` / the close-finalizer machinery (a `source` over an fd
reuses it), `ai_ready` / `ai_wait_fds` (the host interface is right — only its
*callers* move), `dot`, the scheduler core (`yield_sw`, task nodes) — only the fd
plumbing it carries thins out, since `wait_fd` is now set by one site.

## 8. Scheduler changes — fd-awareness collapses to one site

Today three places know fds: `lvm_fgetc` (sets `next_wait_fd`), `lvm_key` (reads
`ai_stdin`), and the scheduler's `wait_fd` slot handling. Under path B:

- The **tail-force thunk** of a `source` is the *only* code that sets
  `next_wait_fd` and yields. Forcing `(cup s)` when `s`'s fd is not `ai_ready`
  parks the task on that fd (the existing `next_wait_fd = fd; Ap(lvm_yield_sw)`
  dance, now in one named place).
- `find_runnable` / `yield_sw_wait` (love.c:2135-2158) are **unchanged** — they
  already multiplex N parked fds correctly. The bug was never the scheduler; it was
  the readiness *model* feeding it (defect 1). With lookahead in the source and EOF
  as `()`, a task never parks holding a byte, and `select` lets a task wait on
  several streams deliberately rather than committing to one `see`.

## 9. Migration — staged, each stage gated

Strictly additive first; delete only after parity.

- **Stage 0 — add the surface alongside the old.** `source`/`sip`(re-pointed)/
  `sink`/`ready?`/`select` as new nifs; `see`/`key?`/`in`/`out` untouched. Gate:
  full `make test` green; a `boot/stream.l` smoke (the pure half over a charlist
  producer — see §11) under `make test_hostnif`.
- **Stage 1 — `read` as the pure fold** over a `source`, behind a flag or a new
  name, run side-by-side with the old reader until datum-for-datum parity on the
  corpus + the oracle (`test/oracle.l`). Keep the old read path live.
- **Stage 2 — port bao.** Rewrite `feedlines`/`edraw`/`pump`/`wrap` onto
  `source`/`sink`/`select`. **This is the deadlock acceptance test** (§10): the
  rlwrap wiring that was reverted now runs. Gate: manual interactive (`(wrap (L
  "bash"))`, type, edit, Enter, see echo) + the scriptable pty round-trip in
  `boot/pty.l`.
- **Stage 3 — port the editor/repl/sip surface** in `love/bao.l` (the baked shell
  core, formerly `repl.l`) and the cli onto streams. Heaviest stage (the egg-baked
  editor; coordinate with the core thread — `bao.l` is shared, kernel- and
  corpus-pinned).
- **Stage 4 — delete.** Remove `see`/`unsee`/`end?`/`key?`/`ungetc_buf`/
  `eof_seen` and the more-bit/port-back protocol (§7). Gate: every tier green +
  valg 0/0 + vmret.

## 10. How this dissolves the bao deadlock (acceptance)

Map each defect to its removal:

1. **ungetc invisible** → there is no `ungetc`; lookahead is the source's memoized
   head, and `ready?`/the force thunk consult it. A task never parks holding a byte.
2. **`-1` EOF** → EOF is `()`; the editor loop and the pump test the *stream* for
   emptiness (`!s` / `twop s`), never a byte for `-1`.
3. **scattered fd logic** → one force site sets `next_wait_fd`; the editor task and
   the pump task each block on their own `source`, or on `(select (L kbd master))`
   when they genuinely need to wait on both — a deliberate multiplex, not two blind
   `see`s racing.
4. **blocking writes** → the render and the pump write through `dot`/`sink`; the
   reader-side fix removes the park-with-byte-in-hand stall that was the actual
   hang, and `select` makes the wait explicit so neither task spins or sleeps on
   the wrong fd.

The reverted `feedlines` becomes, in stream form, roughly: read keys off `(source
0)` (holding the tail as lookahead — no `besc` pushback dance), render via `dot`,
and on Enter drain the edited line into `(sink master)`; the pump is `(source
master) -> (sink out)`; if both must be waited on, `(select (L kbd master))`. No
`ungetc`, no `key?`, no `-1`, no per-getc park scatter — the deadlock's substrate
is gone.

## 11. A runnable model to write next (the pure half)

`boot/stream.l` (a design companion like `boot/tag.l`,
UNVERIFIED-by-run until built) should model the **pure half over an in-memory list
producer** — no fds, so it runs anywhere and pins the algebra:

- a `source` over a charlist: `head`/`tail`/force-memoize as a closure/box;
- `read` as the pure `stream -> () | (datum . stream')` fold, with EOF=`()` and a
  mid-datum truncation raising a `scare`;
- `select`/`ready?` modelled trivially (a memory stream is always ready).

This is the artifact that lets the core session land Stage 0/1 against a known-good
reference before any fd touches the picture. (Left unwritten here per the notes-
only scope; it is the first build step.)

## 12. The minimal-fix fallback (recorded, and why it is not the plan)

If the window is too small for the full rework, two surgical `love.c` fixes unblock
the deadlock without the redesign — recorded for completeness; **the user rejected
the half-duplex shortcut and chose the full rework**, so these are a fallback only:

1. **`lvm_fgetc` ungetc-park (love.c:3171).** Mirror `key?`: park only when *both*
   the pushback is empty and the fd is unready —
   `if (getcharm(i->ungetc_buf) == EOF && !ai_ready(getcharm(i->fd))) park`.
2. **Generalize `key?` -> a port-taking `ready?`** (and keep `key?`=stdin for
   the shell core `love/bao.l`): `(ready? p)` probes `p`'s fd via `ai_ready`,
   register both names on one nif.

These remove defect 1 and add the missing readiness surface, but leave the `-1`
sentinel, the scattered fd logic, the more-bit protocol, and the write-side stall —
i.e. they patch the deadlock without paying down the design. Path B is the
principled end-state they are a down-payment on.

## 13. Coordination & risk

- **Core thread owns `love.c`/`love.h`.** bao designs this (this doc); the `source`/
  `select`/scheduler edits land in a dedicated core session. The bao-side ports
  (`bao.l`, the shell core) are bao's.
- **`bao.l` is shared and pinned** (kernel `(shell 0)`, the love0 corpus, the host
  egg). Stage 3 cannot move the editor out — it ports it in place, with the core
  thread.
- **Risks:** interactive testing is a weak gate (mechanics scriptable via `sip`/the
  pty round-trip, full interaction manual); the `read`-as-fold parity (Stage 1)
  must clear the oracle before the old reader deletes; `select`'s return shape is a
  genuine design choice (first-ready vs all-ready). The write-side park (defect 4)
  is deferred — flagged if a blocking-write stall resurfaces after the reader fix.
```
