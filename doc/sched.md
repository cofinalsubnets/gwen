# the scheduler

love's tasks are cooperative: an op tail-jumps to `lvm_yield_sw`, which snapshots the running
task into a fresh heap node and picks the next one. That design is right. What this doc records
is the shape that keeps a **per-switch cost from scaling with the parked-task count** — invisible
at two tasks, dominant at a hundred — and the one rung that is still only planned (preemption on
inle).

It is not the io arc (doc/io.md); the two meet at exactly one place, `ai_wait_fds`.

## two rings

`g->tasks` is the **run ring**; `g->parked` is the **parked ring**. A task parking on an fd
leaves the run ring, and a wake pass moves ready ones back. **Nothing on the run ring is
fd-parked, so an ordinary switch issues no syscall at all** — that is the design in one
sentence.

The split alone does not do it. With every client blocked, a server leaves a SELF-RING behind,
`find_runnable` over a self-ring answers nothing, and the scheduler walks straight back into the
parked ring and pays what it paid before. Two things carry the rest:

- **`sweep_interval`, a second counter.** Handing the cpu to a runnable peer is a ring walk;
  asking the kernel whether a parked peer became runnable is a syscall. One counter pricing
  both is why a fairness knob reads as a latency/throughput trade — every value that makes the
  sweep affordable also starves the run ring. They are separate: `yield_interval` (64 aps) for
  fairness, `sweep_interval` (16 of those) for the parked sweep.
- **`ai_ready_fds`** (love.h) — the readiness question WITHOUT the wait, one ask for the whole
  ring. The host answers it in a single `poll(2)`; the weak default asks `ai_ready` one fd at a
  time, so no frontend had to move and none reads slower than it did.
  ⚠ Unlike `ai_wait_fds`'s block this one is **authoritative** — the default fills every slot,
  so all-zero means "none ready", never "nobody answered".

⚠ **The syscall-free wakes must come FIRST.** A parked task whose PORT already holds bytes —
another task's bulk gulp put them there — is runnable over an fd with nothing left to say. If
that test sits behind the sweep counter, `yield_sw_wait` builds a wait over an fd that will
never fire and `catch` hangs on a task that has finished. `wake_parked`'s `ask` parameter runs
the pass over the parked ring for the syscall-free terms only (deadline come, port buffered)
before any wait is built. test/host/parked.l's second law holds it.

⚠ **Woken tasks splice at the TAIL** (`run_splice_at`), behind the peers already queued rather
than ahead of them, so a park/wake-heavy task cannot jump a compute-bound one every cycle.

⚠ **Migration is two relinks, so it is two barriers** — unsplice from one ring, splice into the
other, and the splice writes BOTH ends (the woken node's own link out and the tail's link in),
so it barriers both.

⚠ **A doubly-linked ring is refused.** With two rings the wake-side unsplice is free (the scan
carries prev) and the park-side walk is over the run ring, which is short by construction — so
the extra word buys only `lvm_wait`. Against that: every relink would need its own `gen_wb`,
doubling the barrier surface in the code most likely to eat the ring on a miss.

**The parked ring does not survive a bake.** `g->parked` is NULL at wake — an fd number means
nothing in a new process, and a baker is single-tasked, so there is never a parked task to
carry. No `root_tag` slot, `nroot` unchanged, no encver bump; it rides the four `gcp` forwarding
sites and nothing else. `sweep_ctr` is free for the same reason: it sits OUTSIDE the traced
`v0..end` span, where nothing serializes it. A root that DOES need the image appends at
`[2 + nv]` so nothing renumbers (the table is 24 slots, `H.nroot` carries the count) and gates
on `test_encver`.

## the wait path uses the readiness it already has

`ai_wait_fds` fills `revents` (`struct ai_wait_fd` in love.h; the host's block IS a `struct
pollfd`, static-asserted in host/main.c). `find_runnable` takes the block, so the post-wait scan
and the my-fd check are both served from it instead of re-asking the kernel one fd at a time.
No new state, no image change — the block is live in the heap gap at that point and
`find_runnable` allocates nothing.

⚠ **Match on the (fd, events) PAIR, never on the fd alone.** Two tasks can park on one fd in
opposite directions and each entry carries only its own task's question. A socket is almost
always writable, so a reader that accepted a `POLLOUT` wake would spin every pass.

⚠ **Any nonzero `revents` is ready.** Only one direction is ever asked for, so poll can add
nothing but its error/hangup/invalid bits on top — and a task parked on a hung-up fd wants
waking to read the end, not sleeping through it.

⚠ **The block is trusted only when SOME entry fired** — authoritative when it has anything to
say. A frontend that fills nothing (playdate sleeps; the mps2/teensy41/virt boards answer off a
device flag and never write the block back) leaves every `revents` at the zero the scheduler
wrote, reads as "nothing to say", and every fd is asked exactly as before. Correct everywhere,
no frontend forced to move, no hang available as a failure mode.

⚠ **The scheduler zeroes `revents` itself, in the fill loop.** The block is raw heap gap; an
unwritten slot would otherwise read as whatever the last allocation left there, and "ready" is
exactly the wrong way to guess.

⚠ `find_runnable` is `ai_inline` and lands on `lvm_yield_sw`'s frame: pointer parameter, never
array scratch. `make vmret` catches the slip.

⚠ The fd block is sized by the **count**, never a constant — the block rides the uncommitted
heap gap, so counting first retires the cap by construction. When the heap is at its fullest and
there is no gap to lay it in, the sweep asks the old way rather than skipping, which would leave
a ready peer parked.

## `scoop` — collecting finished tasks without a roster

`scoop` (love.c, beside `lvm_donep`) walks the run ring once, unsplices the first task whose
`Ip` is `lvm_task_exit`, and answers `(pid . retval)`, or `()` when none have finished. It is
the task-side twin of `glean` (host/posix.c), which harvests one finished CHILD: love could
collect the OS's children without naming them and not its own.

An app that keeps a roster of live pids and asks `landed?` about each one per accept pays two
multipliers that are roughly equal at the sizes that hurt: the list walk is linear (~330 ns of
interpreted `filter` per live session, paid whether or not anything finished) and `landed?` is
quadratic on top, because `lvm_donep` searches a ring per call. Making the ask cheap fixes only
half — the fix has to stop asking per session.

⚠ **A bounded or rotating collector is a TRAP.** Inspect k pids per accept and the un-reaped
dormant tasks linger on the RUN ring, which `find_runnable` walks on every switch — it trades a
linear cost on the accept path for a linear cost on a hotter one. Collecting *nothing* measures
faster than a rotating collector, and `scoop` beats collecting nothing, because the tasks it
collects stop being scanned.

⚠ **Presence rides the pair, never the net.** A session task returns `()` every time — it is run
for its effect — so the retval alone cannot say whether anything was collected. `two?` is the
test. A drain that read the net would stop on the first such task with the ring still full and
look like it had finished. This is a law in both test/task.l and spec.l rather than a comment.

**No third ring is needed.** A dormant task already sits on the run ring with its retval at
`node[6]`, and `lvm_wait` already does find-then-unsplice — `scoop` is that with the pid
discovered instead of given. With a drain per accept the run ring stays ~2 in steady state, so
`find_runnable` stops walking dormant nodes for free.

## the listener backlog

`ai_listen_backlog` (host/sock.c) is **512**. A backlog of one overflows on a second
simultaneous arrival, the kernel drops the SYN, and the client waits out an exponential retry
that reads as our latency.

⚠ **512 and not SOMAXCONN, and the reason is a law.** test/host/nifpark.l law 5 uses a FULL
ACCEPT QUEUE as its instrument — it is the only way to stall a connect offline, and so the only
way to reach the write-direction park at all. SOMAXCONN puts that out of reach; 512 is the
measured floor for a flat arrival curve at 400 clients and is fillable in about 20 ms. The
number lives in two places on purpose and both say so.

⚠ **The backlog is a constant, not an operand.** `listen` is 1-ary across the tree and out of
it, and a second operand would turn every `(listen port)` into a closure — truthy, so every
"did it listen?" test would read the failure as a success.

## the anatomy of one switch

⚠ Function names are the reference; this region moves, so `git grep -n` the name.

- `find_runnable` walks the ring and, for each parked node, asks readiness. It runs twice per
  wait cycle: once in `lvm_yield_sw` before the wait, once at the foot of `yield_sw_wait`.
- `yield_sw_wait` walks to count the fds and to fill the block, then waits once.
- `lvm_yield_sw` walks for the predecessor of the running node, because the ring is singly
  linked.
- the switch allocates a node and memcpys the outgoing task's stack in, then memmoves the
  incoming one's back out.

Two ops that look constant are not: `lvm_donep` (`landed?`) walks the ring per call, and
`task_live` walks it once per catch-parked task inside `find_runnable` — O(n·k) in catchers.
⚠ `task_live` searches the **parked** ring too, because a caught task blocked on an fd is live
and a catcher told otherwise stops waiting.

## measuring this

⚠ **`make out/host/love` leaves the binary UNBAKED** — naming the target directly skips the
`bake` stamp that only `make host`/`make test` reach. The content is right; the STARTUP is
1.10 s against 0.03 s, an egg boot every run. That alone wrecks a timing measurement, and it
looks like nothing.

⚠ **When a measurement disagrees with the source, verify the BINARY before believing either** —
`objdump -d out/host/love --disassemble=<fn>`, or read the value back at runtime (`ss -ltn`'s
Send-Q is the live `listen` backlog).

⚠ **Ablate before you optimise.** Deleting a suspected cost and re-measuring settles in one run
what paragraphs of reasoning get wrong: the residual slope after the ring split was the *app*
asking the scheduler a question per session per accept, not the parked sweep's `poll(n)`.

⚠ The client-load harness stays OUT of the gate: it takes minutes, and a test that crawls is a
bug announcing itself, never a bench to wait out.

⚠ Every number this design was tuned against came from a SEQUENTIAL client, which is the
friendliest case the fairness cost has. Re-sweep against a concurrent one.

## open

- **`yield_interval` (64) and `sweep_interval` (16) have never been swept together.** 64 has
  been the fairness interval since multitasking landed; 16 was chosen to put the parked sweep at
  roughly a measured rate and never tuned. `sweep_interval` trades wake latency for a
  compute-bound task's throughput and nothing else reads it.
- **epoll / kqueue behind `ai_wait_fds`** — register on park, deregister on wake, so the wait
  returns the ready set instead of rebuilding an n-entry block. Not worth building on today's
  measurements: the curve is flat across 400 held clients, and the sweep is not the visible
  cost. host/posix.c already carries Darwin branches, and the poll path stays as the fallback.
- **inle's one-shot timer** — `ai_wait_fds` re-polls every source on every tick
  (port/inle/kmain.c); its own comment names the fix.
- **wake catchers at exit.** An exiting task knows its own pid and can hand a waiting catcher
  straight back to the run ring, retiring `task_live` and the `lvm_wait` clause in
  `find_runnable` together.
- **free prev in `lvm_wait`.** The run-ring unsplice still walks a full lap for the
  predecessor; `parked_find` already hands its caller the prev, which is the shape to copy.

## preemption on inle — planned, not built

**The timer must not switch tasks. It sets a flag the next `YieldCheck` honors.**

Switching inside the ISR cannot work, for three load-bearing reasons:

1. **the snapshot allocates** — `lvm_yield_sw` can reach `ai_please`, a collection, and memcpys
   the task stack into a fresh node. No GC from an interrupt.
2. **`g` is coherent only at Pack/Unpack boundaries.** Mid-op, Sp/Ip/Hp are locals and `g`'s
   copies are stale; a stale `g->hp` wrecks the barrier.
3. **ring mutations are two steps.** An interrupt between the relink and its `gen_wb` leaves an
   un-barriered old→young edge.

Safepoint preemption avoids all three, and the tree is ready for it:

- **safepoints are already dense** — `YieldCheck` sits on every application op.
- **the long-primitive problem has a house answer** — the bignum ops chunk their work, persist
  state and re-dispatch through `YieldCheck`: the exact discipline preemption needs, with a
  precedent to copy.
- **the ISR exists and does one thing** — bump `kticks` (port/inle/mkvec.l). The addition is one
  store, and mkvec.l LAYS that assembly from a love loop, so it is a lay change and not a `.S`
  edit (there are no `.S` files).

The shape: `++g->yield_ctr >= yield_interval || g->preempt`, with `preempt` a plain scalar
declared beside `next_wait_fd` in love.h — **outside the traced `v0..end` span, so no image
change and no encver bump.**

⚠ **The scheduler change is the small half.** Latency is bounded by safepoint DISTANCE, not by
the tick: a task inside a long non-allocating primitive has no safepoint, and those are the
tail. The real work is auditing unbounded primitives and chunking them on the bignum pattern.

⚠ **This is preemptive SCHEDULING, not isolation.** One heap, one `g`, one stack discipline
shared by every task; a task that faults in C still takes the kernel down. Protection domains
are a different project.

⚠ Preemption raises the switch rate, which multiplies whatever per-switch linear cost is left —
which is why the ring split had to land first.

*gates:* `test_kernel`, `test_kernel_arm64`, `test_vec`, plus a corpus test that proves a task
with no yield of its own still loses the cpu.

## what this design does not touch

- **the snapshot itself.** `lvm_yield_sw` memcpys the stack in and out on every switch, so a
  switch is O(stack depth) plus one node of garbage. Per-task stack segments would make it a
  pointer swap — the deepest change available, and the reason switching is not O(1). Not
  proposed: session stacks are shallow, and nothing has measured it as the cost.
- **fairness policy.** Round-robin is what the ring gives, and no workload has asked for
  priorities.
