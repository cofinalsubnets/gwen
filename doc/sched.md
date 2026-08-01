# the scheduler -- THE PLAN

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

⚠ **the measured wall is not the scheduler yet.** `listen(fd, 1)` (host/sock.c:111) gives
the listener an accept queue of one, so *connecting* 100 clients took 29.7s -- 1-second
TCP SYN retries, with `TcpExtListenOverflows` climbing on every run and `ss -ltn` showing
Send-Q 1. rung 0 exists because until that is gone, none of the rest is visible end to end.

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

⚠ **line numbers here are `~` anchors and the function names are the real reference.** the
scheduler is being actively edited (see the in-flight note under rung 1) and this whole
region moves; `git grep -n` the name, never trust the number.

⚠ the syscall counts above are read off the source, not traced; ptrace was unavailable in
the sandbox this was written in. the rate table is measured.

## the rungs (each green and useful on its own)

### 0. make the improvement visible -- planned

`listen(fd, 1)` -> `SOMAXCONN` (host/sock.c:111). one word, and until it lands every
end-to-end number below is measuring TCP retry backoff instead of the scheduler.

record the baseline curve above in the rung's commit. ⚠ the harness stays OUT of the
gate: it takes minutes, and a test that crawls is a bug announcing itself, never a bench
to wait out.

### 1. use the `revents` already collected -- planned

`ai_wait_fds` fills `revents` (`struct ai_wait_fd` in love.h; the host's block IS a
`struct pollfd`, static-asserted in host/main.c) and the scheduler then throws it away and
re-asks the kernel one fd at a time. hand the block and its count to the post-wait
`find_runnable` and match by fd: **2n+1 syscalls per wait become 1.**

no new state, no image change -- the block is still live in the heap gap at that point
and `find_runnable` allocates nothing.

⚠ **IN FLIGHT, and this rung lands ON TOP of it, not beside it.** as of 2026-08-01 another
session is teaching `connect` to park on its handshake, which is the last open item in the
io arc. it reaches all of this: `ai_ready` grows an `events` operand (a love.h contract
change, so every frontend moves), the task node grows a slot for the park's direction (the
saved stack shifts from `n[5]` to `n[6]`), and **the scheduler now fills `events` per fd
instead of the frontend blanket-setting POLLIN**. that last one is this rung's own premise
arriving early -- the block becomes fully scheduler-owned, and reading `revents` back is
the natural completion of the same change. rebase on it; do not start before it settles.

⚠ and it adds a correctness term to the match: **`revents` must be compared by DIRECTION,
not just by fd.** their own note earns it -- a socket is almost always writable, so the two
directions cannot be OR'd and asked as one, and a reader that accepts a `POLLOUT` wake
would spin on every pass.

⚠ **inle does not fill `revents`, and love.h:~530 currently blesses that** ("a frontend
that does not poll reads `.fd` and ignores the rest"): port/inle/kmain.c:~280-286 returns
on the first ready source without recording which. two ways, and the choice is the rung:

- **advisory** -- `revents` is a positive hint only (nonzero means ready and skips the
  syscall; zero still asks). correct on every frontend unchanged; inle gains nothing.
- **contract** -- `ai_wait_fds` MUST fill `revents`, and inle learns to (~5 lines: record
  which sources answered instead of returning on the first).

**the contract is chosen (revisable), with the advisory read kept as the fallback path**
-- so a frontend that forgets degrades to today's cost rather than to a hang. inle is the
only other polling frontend, so the blast radius is two files.

⚠ `find_runnable` is `ai_inline` and lands on `lvm_yield_sw`'s frame: pointer parameter,
never array scratch. `make vmret` is what catches the slip.

*gates:* `make test` (waits + vmret), `test_hostnif` (test/host/parked.l carries the law),
`test_kernel`, `test_kernel_arm64`.

### 2. two rings: runnable and parked -- planned

`g->parked` beside `g->tasks` (love.h:~133). a task parking on an fd leaves the run ring;
the wake pass moves ready ones back. a switch becomes O(runnable), and the O(parked) scan
runs only when nothing is runnable.

what rides along, because the split is the moment each becomes cheap:

- **dormant tasks go on the parked ring.** an exited-but-uncaught task is scanned on
  every pass today, and kiosko's `reap` (crew/kiosko/kiosko.l:275) maps `back?` over every
  live session while `lvm_donep` walks the whole ring per call -- so one accept costs
  |sessions| x |ring|. that product is the decay the comment above `reap` describes
  (4581 -> 3652 req/s over 2400 requests).
- **wake catchers at exit.** an exiting task knows its own pid and can hand a waiting
  catcher straight back to the run ring, which retires `task_live` (love.c:~2779) and the
  `lvm_wait` clause in `find_runnable` (love.c:~2812) together.
- **free prev in `lvm_wait`.** the unsplice at love.c:~2947 walks a full lap for the
  predecessor; the search loop that found the node could carry it, exactly as `lvm_hush`
  (love.c:~2975) already does. no extra word per node.
- **splice woken tasks at the TAIL.** round-robin falls out of `g->tasks = next` today;
  once wake is an explicit migration, a head splice lets a park/wake-heavy task jump a
  compute-bound peer every cycle.

⚠ **a doubly-linked ring was considered and REFUSED.** with two rings the wake-side
unsplice is free (the scan carries prev) and the park-side walk is over the run ring,
which is short by construction -- so the extra word buys only `lvm_wait`, which rung 2
fixes for nothing. against that: every relink would need its own `gen_wb`, doubling the
barrier surface in the code most likely to eat the ring on a miss. love.c:~2906 records
that failure already ("berth+ink froze in seconds on exactly this").

⚠ **migration is two relinks, so it is two barriers** -- unsplice from one ring, splice
into the other. this is the highest-risk rung in the plan; `test_gcstress` is the gate
that earns its keep here.

**⚠ the open question to settle BEFORE writing code: does a parked ring need to survive a
bake?** `g->tasks` is image-serialized through an explicit root slot (love.c:~5786,
`root_tag[1]`), not through the traced `v0..end` sweep. an fd number is meaningless across
bake/wake and the bakers are single-tasked, so an empty self-ring at wake is very likely
right -- which means **no image slot and no encver bump**, only the four `gcp` forwarding
sites (love.c:~1315, 1340, 1411, 1452). if the answer turns out to be yes, append at
`[2 + nv]` so nothing renumbers (the table is 24 slots, `H.nroot` carries the count,
love.c:~5783-5788) and gate on `test_encver`.

*gates:* `make test`, `test_gc`, `test_gcstress`, `test_gcheck`, `test_hostnif`,
`test_kernel`, `test_kernel_arm64`, `test_encver`, `vmret`.

### 3. re-measure, then decide -- planned

re-run the curve. **if it is flat to 500+ clients, the arc stops here** and rungs 3-4
stay unbuilt. only if the `poll(n)` rebuild is still visible does the readiness set become
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

**0 and 1 together.** a one-word fix plus a contained change to two functions, gated by
tests that already exist, and between them they should move the curve more than anything
else here. then re-measure before committing to 2 -- the split is where the write-barrier
risk lives, and it should be bought by a number rather than by a prediction.

**2 before 4, always.** preemption raises the switch rate, which multiplies whatever
per-switch linear cost is left; landing it first would make the split look like it did
not help.

**3 may never be built,** and that is a success condition rather than a gap.

## what this arc does not touch

- the snapshot itself. `lvm_yield_sw` memcpys the stack in and out on every switch, so a
  switch is O(stack depth) plus one node of garbage. per-task stack segments would make it
  a pointer swap -- the deepest change available, and the reason switching will not be
  O(1) even after rung 2. not proposed: kiosko's session stacks are shallow, and nothing
  has measured it as the cost.
- fairness policy. round-robin is what the ring gives and no workload here has asked for
  priorities.
- `yield_interval` (64 aps, love.c:~780). an adaptive interval is available if rung 3's
  measurement wants it; it is noise beside the linear costs.
