# the scheduler -- THE PLAN

> **status, 2026-08-01.** rungs 0 and 1 are BUILT. the measurements taken while building
> them moved the diagnosis: the cost is not on the wait path, it is on the **pre-wait
> scan**, so **rung 2 is the rung** and rung 1 turned out to be its groundwork rather than
> a win of its own. the "what the measurements actually said" section below is the part to
> read first; the rung texts under it have been corrected to match.

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

⚠ **a stale object cost three measurements before this was caught.** `make host` relinks
but does not re-archive `out/host/liblove.a`, and `make out/host/love` leaves the pre-BAKE
binary -- twice a run, `out/host/love` did not contain the constant the source said it did
(`ss -ltn` showed Send-Q 64 against a source reading 512; `objdump --disassemble=call_listen`
showed the immediate). when a measurement here disagrees with the source, VERIFY THE BINARY
before believing either: `rm -f out/host/moon/love.o out/host/liblove.a out/host/love` then
`make host`, and read the constant back out of the ELF.

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

### 2. two rings: runnable and parked -- planned, and THIS IS THE RUNG

`g->parked` beside `g->tasks` (love.h:~133). a task parking on an fd leaves the run ring;
the wake pass moves ready ones back. a switch becomes O(runnable), and the O(parked) scan
runs only when nothing is runnable.

**this is where the measured cost is.** the fairness yield fires every `yield_interval`
aps and runs `find_runnable` over the whole ring, one `poll(2)` per parked task -- and the
two probes above agree that this, not the wait, is what collapses the rate. moving parked
tasks off the run ring deletes that scan rather than running it less often, which is what
the `yield_interval` lever does at the price of latency.

**the number to beat:** at 200 parked, 305 req/s today. a `yield_interval` of 4096 buys
1680 req/s while costing 15% at 0 parked. rung 2 should reach the first without paying the
second -- and if it does, re-measure `yield_interval` afterwards on top of it, because the
value that is right for a scan-free scheduler is not the one that was right for this one.

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

**0 and 1 are built.** the prediction that "between them they should move the curve more
than anything else here" was WRONG, and the re-measure is what caught it: 0 removed a TCP
artifact that was masking the scheduler, and 1 optimized a path this workload does not
take. the plan called for buying rung 2 with a number rather than a prediction, and the
number arrived pointing at rung 2 harder than before.

**2 next, and it is the whole arc.** everything measured says the fairness-yield scan is
the cost.

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
and one thing it very much DOES touch, having been listed here as noise and measured as a
5x lever:

- `yield_interval` (64 aps, love.c:~774) is a real knob and has never been tuned -- 64 has
  been the value since multitasking landed. the sweep is in the measurement section. it is
  deliberately NOT changed here: it buys throughput with latency, and the thing it is
  compensating for is what rung 2 deletes. retune it on top of rung 2, against a
  CONCURRENT client rather than the sequential one used above, which is the friendliest
  case the fairness cost has.
