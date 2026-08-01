# the scheduler -- THE PLAN

> **status, 2026-08-01.** rungs 0, 1 and 2 are BUILT. the measurements taken while
> building 0 and 1 moved the diagnosis -- the cost was not on the wait path but on the
> **pre-wait scan** -- and rung 2 removed it: **at 200 parked clients, 305 -> 3837 req/s,
> and the zero-parked column did not move.** the target had been 1680, which is what a
> `yield_interval` of 4096 bought while costing 15% at zero parked; rung 2 beat it 2.3x
> and cost nothing. **rung 3 is now a question, not a plan** -- see its section.

love's tasks are cooperative: an op tail-jumps to `lvm_yield_sw` (love.c:~2854), which
snapshots the running task into a fresh heap node and picks the next one off a single
circular ring. that design is right and this plan does not replace it. what it does is
retire the **per-switch linear costs**, which are invisible at two tasks and dominant at
a hundred -- and then, on inle, put a timer behind the yield so the switch stops needing
the task's permission.

the arc touches the host and the kernel equally. it is NOT the io arc (doc/io.md): the
two meet at exactly one place, `ai_wait_fds`, and rung 1 below changes that contract, so
that rung is cross-filed there.

## why -- the measurement

kiosko twirls a task per client, so it is the tree's honest scheduler benchmark. clients
connected and held open, then ordinary sequential requests timed against them
(2026-08-01, x86_64 host, `out/host/love`):

| parked clients | service rate | per request |
|---|---|---|
| 0 | 8600 req/s | 0.12 ms |
| 10 | 3984 req/s | 0.25 ms |
| 25 | 2608 req/s | 0.38 ms |
| 50 | 2127 req/s | 0.47 ms |
| 100 | 875 req/s | 1.14 ms |

nothing breaks along the way -- at 50, 100 and 200 held-then-released every request came
back `200`, none empty, none stalled. it is a rate collapse, not a failure, and the shape
is the giveaway: a tenfold client count costs tenfold per request.

⚠ **the measured wall is not the scheduler yet.** `listen(fd, 1)` (host/sock.c) gave
the listener an accept queue of one, so *connecting* 100 clients took 29.7s -- 1-second
TCP SYN retries, with `TcpExtListenOverflows` climbing on every run and `ss -ltn` showing
Send-Q 1. rung 0 exists because until that is gone, none of the rest is visible end to end.

## what the measurements actually said

rung 0 landed and the connect storm went away entirely (0.01s to connect 400 clients,
where it had timed out at four minutes). the service curve barely moved, which confirmed
the two ceilings were independent -- and left the scheduler's own cost showing clean, and
superlinear: 200 parked costs 2.70 ms/req, 400 costs 16.78.

then two probes changed the plan.

**one: `yield_interval` is worth more than rung 1, and it has never been tuned.** it has
been 64 since multitasking landed. holding everything else fixed and rebuilding per value:

| `yield_interval` | 0 parked | 100 parked | 200 parked |
|---|---|---|---|
| **64** (today) | 5886 req/s | 854 | 305 |
| 256 | 5795 | 2164 | 1058 |
| 1024 | 5137 | 3489 | 1622 |
| 4096 | 5009 | 3563 | 1680 |

**two: the wait path is cold.** counting from inside `ai_wait_fds`, 600 requests against
100 parked clients enter the multi-fd wait **fewer than 25 times**. nearly every
scheduling decision is settled by the PRE-wait `find_runnable`, which has no poll behind
it and pays one `poll(2)` per parked task.

those two say the same thing from opposite directions: **the cost is the O(n) syscall scan
on the fairness-yield path.** a bigger `yield_interval` helps only because it runs that
scan less often, and it charges latency for it -- note the 0-parked column falling as the
slice grows, which is the accept loop waiting longer for its turn. that is a trade, not a
fix. rung 2 removes the scan instead, and should get the throughput without the latency.

⚠ **the yield_interval numbers are NOT a recommendation to change it.** 64 -> 256 looks
nearly free (2% at 0 parked, 2.5-3.5x under load) and may well be worth taking, but the
fairness cost was measured only against a SEQUENTIAL client, which is the friendliest case
there is. re-measure it after rung 2, when the scan it is compensating for is gone.

⚠ **a stale object cost three measurements before this was caught**, and the cause was
found afterwards: the tree had no `.DELETE_ON_ERROR`, so a failed recipe left its
half-written target behind with a fresh mtime and the next make called it up to date.
FIXED (`d8df2376`). the habit that caught it stays, because it costs nothing: when a
measurement disagrees with the source, VERIFY THE BINARY before believing either --
`objdump -d out/host/love --disassemble=<fn>`, or read the value back at runtime
(`ss -ltn`'s Send-Q is the live `listen` backlog).

⚠ **and `make out/host/love` leaves the binary UNBAKED** -- naming the target directly
skips the `--bake` stamp that only `make host`/`make test` reach. the content is right;
the STARTUP is 1.10s against 0.03s, an egg boot every run. that alone will wreck a timing
measurement, and it looks like nothing.

## the anatomy of one switch

with n parked tasks, a scheduling decision walks the ring about **four times** and issues
up to **2n+1 `poll(2)` calls**:

- `find_runnable` (love.c:~2812) walks the ring, and for each parked node asks
  `ai_ready` -- which on the host is one `poll(&p, 1, 0)` syscall (host/main.c:~49). it
  runs **twice** per wait cycle: once in `lvm_yield_sw` before the wait, once at the foot
  of `yield_sw_wait` after.
- `yield_sw_wait` (love.c:~2830) walks twice more -- to count the fds and to fill the
  block -- then one `poll(n)`.
- `lvm_yield_sw` walks a fourth time for the predecessor of the running node, because the
  ring is singly linked.
- and the switch itself allocates a node and memcpys the outgoing task's stack in, then
  memmoves the incoming one's back out.

on top of that, two ops that look constant are not: `lvm_donep` (`back?`) walks the ring
per call, and `task_live` walks it once **per catch-parked task** inside `find_runnable`
-- O(n·k) in catchers.

⚠ **line numbers here are `~` anchors and the function names are the real reference.** this
whole region moves; `git grep -n` the name, never trust the number.

⚠ the syscall counts above are read off the source, not traced; ptrace is unavailable in
this sandbox (`PTRACE_SEIZE: Operation not permitted`, on attach and on launch alike). the
rate tables are measured, and the wait-entry count was got by counting inside
`ai_wait_fds` on a throwaway build.

## the rungs (each green and useful on its own)

### 0. make the improvement visible -- BUILT

the listener's accept queue was ONE, so a second simultaneous arrival overflowed it, the
kernel dropped the SYN, and the client waited out an exponential retry that reads as our
latency. `ai_listen_backlog` (host/sock.c) is **512**. connect phase, before -> after:
1.06s -> 0.00s at 25 clients, 29.7s -> 0.00s at 50, four-minute timeout -> 0.01s at 400.

⚠ **512 and not SOMAXCONN, and the reason is a law.** test/host/nifpark.l law 5 uses a FULL
ACCEPT QUEUE as its instrument -- it is the only way to stall a connect offline, and so the
only way to reach the write-direction park at all. it filled that queue with two arrivals
back when the backlog was 1. SOMAXCONN put it out of reach and the law reddened; 512 is the
measured floor for a flat arrival curve at 400 clients and is fillable in about 20ms. the
number lives in two places on purpose and both say so.

⚠ **the backlog is a constant, not an operand.** `listen` is 1-ary across the tree and out
of it, and a second operand would turn every `(listen port)` into a closure -- truthy, so
every "did it listen?" test would read the failure as a success.

⚠ the harness stays OUT of the gate: it takes minutes, and a test that crawls is a bug
announcing itself, never a bench to wait out.

### 1. use the `revents` already collected -- BUILT, and it is groundwork

**it does not move the end-to-end number, and the measurement above says why**: the
multi-fd wait is entered fewer than 25 times per 600 requests. this rung makes the WAIT
path cheap, and the wait path is cold until rung 2 moves the parked tasks onto it. it is
committed as what it is -- a syscall reduction on a cold path that regresses nothing and
pays once rung 2 lands. the original text follows, corrected where it was wrong.

`ai_wait_fds` fills `revents` (`struct ai_wait_fd` in love.h; the host's block IS a
`struct pollfd`, static-asserted in host/main.c) and the scheduler threw it away and
re-asked the kernel one fd at a time -- **2n+1 syscalls where the wait had already answered
all n.** `find_runnable` takes the block now; the post-wait scan and the my-fd check are
both served from it.

no new state, no image change -- the block is still live in the heap gap at that point
and `find_runnable` allocates nothing.

⚠ **match on the (fd, events) PAIR, never on the fd alone.** two tasks can park on one fd
in opposite directions and each entry carries only its own task's question. a socket is
almost always writable, so a reader that accepted a `POLLOUT` wake would spin every pass.

⚠ **any nonzero `revents` is ready.** only one direction is ever asked for, so poll can add
nothing but its error/hangup/invalid bits on top -- and a task parked on a hung-up fd wants
waking to read the end, not sleeping through it.

⚠ **the block is trusted only when SOME entry fired**, and that is what settled the
advisory-vs-contract question the plan left open. **neither was chosen: the answer is
"authoritative when it has anything to say".** a frontend that fills nothing (playdate
sleeps; the mps2/teensy41/virt boards answer off a device flag and never write the block
back) leaves every `revents` at the zero the scheduler wrote, reads as "nothing to say",
and every fd is asked exactly as before -- correct everywhere, no frontend forced to move,
no hang available as a failure mode. inle learned to fill it anyway, being the other
frontend where the parked count grows.

⚠ **the scheduler zeroes `revents` itself, in the fill loop.** the block is raw heap gap;
an unwritten slot would otherwise read as whatever the last allocation left there, and
"ready" is exactly the wrong way to guess.

⚠ `find_runnable` is `ai_inline` and lands on `lvm_yield_sw`'s frame: pointer parameter,
never array scratch. `make vmret` is what catches the slip.

*gates:* `make test` (waits + vmret), `test_hostnif` (test/host/parked.l carries the law),
`test_kernel`, `test_kernel_arm64`.

### 2. two rings: runnable and parked -- BUILT

`g->parked` beside `g->tasks`. a task parking on an fd leaves the run ring; a wake pass
moves ready ones back. **nothing on the run ring is fd-parked, so a switch issues no
syscall at all** -- that is the rung in one sentence.

| parked clients | before | after | |
|---|---|---|---|
| 0 | 8600 req/s | 7690 | flat (0.12 -> 0.13 ms) |
| 10 | 3984 | 8831 | 2.2x |
| 25 | 2608 | 8344 | 3.2x |
| 50 | 2127 | 7002 | 3.3x |
| 100 | 875 | 5939 | 6.8x |
| 200 | 305 | 3837 | **12.6x** |
| 400 | 60 (16.78 ms/req) | 1782 (0.56) | **30x** |

the target was 1680 at 200 parked without paying the 15% at zero that `yield_interval`
4096 charged for it. both held: 2.3x past the target, and the zero-parked column is inside
its own noise (three 2000-request runs: 8547 / 8641 / 8188 against a baseline of 8600).

**the plan expected the split alone to do it, and the split alone does NOT.** with the
parked tasks moved off, a server whose every client is blocked leaves a SELF-RING behind --
and `find_runnable` over a self-ring answers nothing, so the scheduler walked straight back
into the parked ring and paid exactly what it had before. two things were needed on top:

- **`sweep_interval`, a SECOND counter.** handing the cpu to a runnable peer is a ring walk;
  asking the kernel whether a parked peer became runnable is a syscall. `yield_interval` was
  pricing both, and that is the whole reason its sweep read as a latency/throughput trade --
  every value that made the sweep affordable also starved the run ring. they are separate
  counters now: fairness every 64 aps as before, the parked sweep every 16 of those.
- **`ai_ready_fds`** (love.h) -- the readiness question WITHOUT the wait, one ask for the
  whole ring. the host answers it in a single `poll(2)`; the weak default asks `ai_ready`
  one fd at a time, so no frontend had to move and none reads slower than it did.
  ⚠ unlike `ai_wait_fds`'s block this one is AUTHORITATIVE -- the default fills every slot,
  so all-zero means "none ready", never "nobody answered".

⚠ **the trap that reddened the gate, and it is defect 6 wearing new clothes.** a parked task
whose PORT already holds bytes -- another task's bulk gulp put them there -- is runnable
over an fd with nothing left to say. `find_runnable` used to test that before any wait
because the task was on the one ring; with two rings the test moved behind the sweep
counter, so `yield_sw_wait` built a wait over an fd that would never fire and `catch` hung
on a task that had finished. the fix is a pass over the parked ring for the SYSCALL-FREE
terms only (deadline come, port buffered) before a wait is ever built -- `wake_parked`'s
`ask` parameter. **test/host/parked.l's second law caught it on the first run**, which is
exactly the job it was written for.

**splice woken tasks at the TAIL** -- TAKEN. `run_splice_tail` puts a woken task behind
the peers already queued rather than ahead of them, so a park/wake-heavy task cannot jump
a compute-bound one every cycle.

what did NOT ride along, and is still worth taking:

- **dormant tasks go on the parked ring.** an exited-but-uncaught task is still scanned on
  every pass, and kiosko's `reap` (crew/kiosko/kiosko.l:275) maps `back?` over every
  live session while `lvm_donep` walks the whole ring per call -- so one accept costs
  |sessions| x |ring|. that product is the decay the comment above `reap` describes
  (4581 -> 3652 req/s over 2400 requests). ⚠ `lvm_donep` searches BOTH rings now, so the
  product did not shrink, it only moved.
- **wake catchers at exit.** an exiting task knows its own pid and can hand a waiting
  catcher straight back to the run ring, which retires `task_live` and the `lvm_wait`
  clause in `find_runnable` together. ⚠ `task_live` got MORE expensive here, not less: it
  searches the parked ring too, because a caught task blocked on an fd is live and a
  catcher told otherwise stops waiting.
- **free prev in `lvm_wait`.** the run-ring unsplice still walks a full lap for the
  predecessor. `parked_find` already hands its caller the prev, which is the shape to copy.

⚠ **a doubly-linked ring was considered and REFUSED.** with two rings the wake-side
unsplice is free (the scan carries prev) and the park-side walk is over the run ring,
which is short by construction -- so the extra word buys only `lvm_wait`, which rung 2
fixes for nothing. against that: every relink would need its own `gen_wb`, doubling the
barrier surface in the code most likely to eat the ring on a miss. love.c:~2906 records
that failure already ("berth+ink froze in seconds on exactly this").

⚠ **migration is two relinks, so it is two barriers** -- unsplice from one ring, splice
into the other, and `run_splice_tail` writes BOTH ends (the woken node's own link out and
the tail's link in), so it barriers both. this was billed as the highest-risk part and it
cost nothing: `test_gc`, `test_gcstress` and `test_gcheck` were green on the first build.
the hang came from the readiness ORDER instead, which no barrier could have caught.

**the question that was open BEFORE writing code, now ANSWERED: no, a parked ring does not
need to survive a bake.** `g->parked` is NULL at wake -- an fd number means nothing in a
new process, and a baker is single-tasked, so there was never a parked task to carry. no
`root_tag` slot, `nroot` unchanged, **no encver bump** (`test_encver` green). it rides the
four `gcp` forwarding sites and nothing else. `sweep_ctr` is free for the same reason the
plan gives `preempt` in rung 4: it sits OUTSIDE the traced `v0..end` span, where nothing
serializes it. a root that DOES need the image appends at `[2 + nv]` so nothing renumbers
(the table is 24 slots, `H.nroot` carries the count) and gates on `test_encver`.

*gates:* `make test`, `test_gc`, `test_gcstress`, `test_gcheck`, `test_hostnif`,
`test_kernel`, `test_kernel_arm64`, `test_encver`, `vmret`.

### 3. re-measure, then decide -- MEASURED, and the answer is "not yet"

the curve was re-run and it is in rung 2's table. **it is not flat** -- 8600 at zero
parked against 1782 at 400 -- so the arc does not stop here on its own terms. but the
knee moved so far out that the case for rung 3 is now weaker, not stronger: what used to
collapse at 100 clients holds 5939 req/s there, and 400 parked costs 0.56 ms/req where it
cost 16.78.

**what is left to pay for, and it is one thing:** the parked sweep still rebuilds an
n-entry block and asks about every parked fd, on `sweep_interval` and on every block. it
is one syscall instead of n, which is why it stopped mattering -- but it is still O(n)
walk plus O(n) kernel-side scan, and that is what the residual slope is.

**two knobs to try FIRST, both free, before writing an epoll lane:**
- **re-measure `yield_interval` and `sweep_interval` on top of rung 2.** neither has been
  swept since the split, and the old sweep conflated them. ⚠ use a CONCURRENT client this
  time -- every number in this document came from a sequential one, which is the
  friendliest case the fairness cost has.
- **`sweep_interval` is a guess.** 16 was chosen to put the parked sweep at roughly the
  rate the `yield_interval` 1024 row measured, and never tuned. it trades wake latency for
  a compute-bound task's throughput and nothing else reads it.

only if the `poll(n)` rebuild is still visible after that does the readiness set become
worth keeping in the kernel:

- **epoll / kqueue behind `ai_wait_fds`** -- register on park, deregister on wake, so the
  wait returns the ready set instead of rebuilding an n-entry block every time. the Darwin
  objection recorded at doc/io.md's rung 6a was raised against a different question (a
  frontend owning the fd *vector*); host/posix.c already carries Darwin branches, and
  today's poll path stays as the fallback.
- **inle's one-shot timer** -- `ai_wait_fds` re-polls every source on every tick
  (port/inle/kmain.c:~280-286); its own comment names the fix, and rung 2 makes the
  deadline cheap to compute.

### 4. preemption on inle -- planned, and gated on rungs 0-2

**the timer must not switch tasks. it sets a flag the next `YieldCheck` honors.**

switching inside the ISR cannot work here, for three reasons that are each load-bearing:

1. **the snapshot allocates** -- `lvm_yield_sw` can reach `ai_please` (love.c:~2886), a
   collection, and memcpys the task stack into a fresh node. no GC from an interrupt.
2. **`g` is coherent only at Pack/Unpack boundaries.** mid-op, Sp/Ip/Hp are locals and
   `g`'s copies are stale; love.c:~2906 records what a stale `g->hp` does to the barrier.
3. **ring mutations are two steps.** an interrupt between `prev->m = tagthread(...)` and
   its `gen_wb` leaves an un-barriered old->young edge.

safepoint preemption avoids all three, and this tree is unusually ready for it:

- **safepoints are already dense** -- `YieldCheck` (love.c:~781) sits on every application
  op: love.c:~2676, 2685, 2697, 2707, 3072, 3087, 3104.
- **the long-primitive problem already has a house answer.** the bignum ops chunk their
  work, persist state and re-dispatch through `YieldCheck` (love.c:~7565, 7603, 7655,
  7747) -- the exact discipline preemption needs, with a precedent to copy.
- **the ISR exists and does one thing** -- bump `kticks` (port/inle/mkvec.l:102-105). the
  addition is one store, and mkvec.l LAYS that assembly from a love loop, so it is a lay
  change and not a `.S` edit (there are no `.S` files).

the shape: `++g->yield_ctr >= yield_interval || g->preempt`, with `preempt` a plain scalar
declared beside `next_wait_fd` (love.h:~141) -- **outside the traced `v0..end` span, so no
image change and no encver bump.** that is the same dodge defect 6's commit message argued
for: check whether the value can live where nothing traces it before pricing a format bump.

⚠ **the scheduler change is the small half.** latency is bounded by safepoint DISTANCE,
not by the tick: a task inside a long non-allocating primitive has no safepoint, and those
are the tail. the real work of this rung is auditing unbounded primitives and chunking
them on the bignum pattern.

⚠ **this is preemptive SCHEDULING, not isolation.** one heap, one `g`, one stack
discipline shared by every task; a task that faults in C still takes the kernel down.
protection domains are a different project and this design does not reach them.

*gates:* `test_kernel`, `test_kernel_arm64`, `test_vec`, plus a corpus test that proves a
task with no yield of its own still loses the cpu.

## order, and what to do first

**0 and 1 are built.** the prediction that "between them they should move the curve more
than anything else here" was WRONG, and the re-measure is what caught it: 0 removed a TCP
artifact that was masking the scheduler, and 1 optimized a path this workload does not
take. the plan called for buying rung 2 with a number rather than a prediction, and the
number arrived pointing at rung 2 harder than before.

**2 is built, and it was the whole arc.** everything measured said the fairness-yield scan
was the cost, and removing it bought 12.6x at 200 parked for nothing at zero. ⚠ but the
SPLIT alone bought none of it -- the counter that stopped the sweep firing on every
fairness yield, and the one ask that replaced n, are what the number came from. a plan
that says "move them to another ring" and stops there describes a refactor.

**2 before 4, always.** preemption raises the switch rate, which multiplies whatever
per-switch linear cost is left; landing it first would make the split look like it did
not help.

**3 is now a measurement, not a plan** -- two knobs to sweep before anyone writes an epoll
lane, and the case for building it got weaker rather than stronger. see its section.

**4 is the next real rung** if the arc continues, and it is the one the tree is most ready
for.

## what this arc does not touch

- the snapshot itself. `lvm_yield_sw` memcpys the stack in and out on every switch, so a
  switch is O(stack depth) plus one node of garbage. per-task stack segments would make it
  a pointer swap -- the deepest change available, and the reason switching will not be
  O(1) even after rung 2. not proposed: kiosko's session stacks are shallow, and nothing
  has measured it as the cost.
- fairness policy. round-robin is what the ring gives and no workload here has asked for
  priorities.
and one thing it very much DOES touch, having been listed here as noise and measured as a
5x lever:

- `yield_interval` (64 aps, love.c) has never been tuned -- 64 since multitasking landed.
  the sweep is in the measurement section. it is STILL not changed: rung 2 deleted the
  cost it was compensating for, and the honest answer is that its old sweep was measuring
  two knobs at once. `sweep_interval` (16) now carries the half that made it look like a
  lever. both want a fresh sweep on top of rung 2, against a CONCURRENT client.
