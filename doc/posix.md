# posix — a POSIX OS layer for love user programs

A design map. The goal: love user programs (and the love shell) get a real POSIX
environment — files, processes, pipes, signals, sockets — so people can write real
apps. The guiding decision (user, 2026-06-17): **don't reimplement POSIX; expose
the one already under us.** On the host, love is already a Unix process — the OS layer
isn't something to build, it's something to *surface*. And it's **host-agnostic**:
target the POSIX standard, not a kernel, so the same nifs run on Linux, a BSD, or
macOS (apt — `ain` is an OpenBSD `nc` clone).

This is the **ain pattern, generalized.** ain already wraps the socket
syscalls as host nifs in `main.c`; the POSIX layer is "do that for the rest of the
syscall surface." Nothing exotic.

## Three strata

| stratum | what                                   | host needs       | when        |
|---------|----------------------------------------|------------------|-------------|
| **L0**  | host nifs over the real kernel (libc)  | any POSIX host   | near-term ★ |
| **L1**  | same nifs, freestanding / raw syscalls | a kernel ABI     | mid         |
| **L2**  | the freestanding KERNEL grows its own POSIX | bare metal  | inle era   |

**L0 is the recommendation.** L1 and L2 are the same *interface* reached without a
host libc (L1) or without a host at all (L2) — design the surface once at L0 and the
backends swap underneath.

### L0 — ride the host Unix (the killer feature)

love-the-host-process already calls `read`/`write`/`malloc`. L0 widens that to the
POSIX surface as nifs, exactly like ain's `connect`/`listen`/`accept`:

> every host nif is `host_X` (an `ai_noinline` syscall worker) + `lvm_X` (the VM
> tail wrapper) + a `nif_X[]` thread registered via `AI_NIF` in a `host/*.c` file
> (auto-globbed — no love.c/love.h/main.c edit; main.c is core). The fd→port path is
> free: `ai_io_alloc(g,fd)` wraps any fd as a port with a close finalizer, and
> read/write then come free via getc/putc. The general-POSIX nifs wear the
> `posix_` C-symbol prefix (host/posix.c: `lvm_posix_stat` &c); the love names stay
> the plain POSIX words.

The payoff: **the love shell becomes a real shell whose external commands are the
host's programs.** `(exec "ls" '("-l"))`, a `(pipe)` between two `(fork)`ed children,
job control — love runs `ls`/`grep`/`git` and pipes between them. "Use the host Unix as
the shell substrate" — you get a usable Unix without writing one.

Gating mirrors ain: host-only `#ifdef`; the kernel (`kmain.c`) and wasm don't link
`main.c` so they auto-exclude; `prel.l` stays syscall-free; a separate
`make ostest`-style target, not the portable corpus.

### L1 — freestanding on a host ABI

The same nifs, but the binary is libc-free and issues raw syscalls (`syscall`/`svc`)
— converging the host build with the kernel's existing freestanding discipline
(`-ffreestanding -Wall -Wextra -Werror`). One small per-ABI backend (the syscall
instruction + the errno convention) replaces libc; the nif surface above is
unchanged. Drops a dependency, doesn't change the API.

### L2 — the freestanding kernel grows its own POSIX

When there's no Unix under you (the `inle` bare-metal agent — see `crew/inle.md`),
the kernel must *be* the OS. This is the only stratum that's a real OS build: a ramfs
VFS, an in-kernel process model, signal delivery. The `k_sources[]` table in
`kmain.c` is already "vfs-shaped" (its own comment says so, and anticipates ramfs),
so L2 grows from there. Largest effort; post-everything.

## The concept → primitive map

Most POSIX concepts already have a love shape — L0 just wires them to the host:

| POSIX                          | love surface / backing                                   |
|--------------------------------|--------------------------------------------------------|
| process / thread               | **task** — `spawn`/`wait`/`done?`/`chill` (the cooperative scheduler) |
| `fork`/`exec`/`waitpid`/`_exit`| new L0 nifs; `exec` over the host `execve`             |
| file descriptor                | **port** via `ai_io_alloc` + the `k_sources[]` vtable  |
| `open`/`read`/`write`/`close`  | `open` exists; read/write are getc/putc; + `lseek`     |
| `dup2`/`pipe`                  | new L0 nifs (pipe returns a port pair)                 |
| `stat`/`mkdir`/`unlink`/readdir| new L0 nifs (a `stat` nif is already wanted for cook)  |
| `cwd` — `chdir`/`getcwd`       | new L0 nifs                                             |
| signals — `sigaction`/`kill`   | **the condition system**: a delivered signal `raise`s through `help`/`scare`; `(kill pid sig)` is the planned bao nif |
| environment — `getenv`/`environ`/argv | cli.l (#8) already parses argv; add env nifs    |
| exit codes / std streams       | `in`/`out`/`err` ports exist; exit-code plumbing via #8/#10 |
| sockets (BSD)                  | **ain** — `connect`/`listen`/`accept`/`shutdown`/DNS |
| time — `clock_gettime`         | `ai_clock` / `(clock t)` exist                          |
| `select`/`poll`                | `ai_wait_fds` / `ai_ready` exist (the scheduler's core) |

Two mappings are the elegant ones, and both are *already built*:

- **signals → conditions.** POSIX signal delivery is exactly a `raise`: a `SIGINT`
  becomes `(scare 'sigint pid)` routed to the installed `help`; a handler is a `help`
  policy; `welp` is the default disposition. No new mechanism — the condition system
  *is* the signal machinery.
- **fds → ports, select → `ai_wait_fds`.** The cooperative scheduler already blocks
  tasks on fds and wakes the ready one. Two `spawn`ed pumps on two fds interleave with
  no select loop (this is why ain's bidirectional pump is ~free).

## Staging (L0)

1. **fs nifs** — DONE: `stat`/`readdir`/`unlink`/`lseek` (the `posix_` lane in
   host/posix.c, gated in boot/init.l) joined `mkdir`/`chdir`/`cwd`; the
   `open`/read/write/close path predates them. `stat` answers `(size mtime-ms mode)`
   or `()` for absence; `lseek` rides the raw-fd `openfd` lane (ports buffer).
2. **process nifs** — `fork`/`exec`/`waitpid`/`_exit`/`pipe`/`dup2`/`kill`. The core of
   "use the host as a shell."
3. **the shell uses it** — extend the love shell (#8 cli.l → shell.l, #10) to run external
   programs: PATH lookup, `fork`+`exec`, pipelines, redirection, `$?`/exit codes,
   env. *This is the "Linux/BSD as a shell" deliverable.*
4. **signals → conditions + job control** — DONE: `(signal sig disp)` (sigaction
   default/ignore), `sigfd` takes a signal LIST (any signal becomes perceive DATA,
   `(signo . pid)`, re-raisable as `(scare 'sigint pid)` through `help` — the
   conditions mapping, gated in boot/sh.l), `wait` reports stops (256+sig,
   WUNTRACED), and the shell got real job control: per-job process groups +
   tcsetpgrp handoff (`spawnio pg/fg`, `ttyfg`), ^C/^Z to the foreground job only,
   jobs/fg/bg/&. (The pgrp lesson: a stop signal to an ORPHANED group is discarded,
   so in-shell-pgrp children can never ^Z under a nested session.) Task-level
   `chill`/thaw stays separate — tasks are not processes.
5. **env + status polish** — DONE: `(setenv name val)` (a non-string val unsets — the
   absence lane), `(environ _)` (raw "K=V" strings), and the shell expands `$NAME`/`$?`
   in unquoted words and inside `".."` (literal in `'..'`; an unset var is the unit and
   concatenates away, so a bare unset word drops — bash's empty-removal for free). `$?`
   is the last stage's status, a stop folded to 128+sig; `cd` sets it; `export N=V` and
   `exit [n]` (over the `quit` nif) round out the builtins. Gated in boot/sh.l.

**L0 staging complete** (2026-07-04). What remains of L0 is widening, not scaffolding:
`dup2`-as-nif if a program (not the shell) wants it, `rmdir`/`rename` when something
asks, and the shell's own deferred niceties (glued operator lexing, PATH hashing).

Sockets are already covered by ain; fold them in as the network slice.

## Open questions

- **`fork` semantics** — love's heap is a two-space copying GC; `fork` is a host-process
  primitive (copy-on-write at the OS level), orthogonal to the love heap (the child gets
  its own address space from the kernel). Confirm the GC and the forked child coexist
  — likely fine since `fork` copies the whole process, but exec-after-fork is the safe
  pattern; a fork *without* exec (two live love VMs) needs thought.
- **task vs process** — keep them distinct: a **task** is an in-VM green thread
  (`spawn`/`chill`); a **process** is a host pid (`fork`/`kill pid`). Never cross them
  (the `chill` task vs `kill` process split is already the plan).
- **portability spread** — POSIX is the target, but readdir/stat struct layouts and
  errno values differ across Linux/*BSD/mac. Wrap at the call boundary (the `call_X`
  worker normalizes), surface a stable love shape.
- **L0 vs L1 timing** — L0 (libc) ships fastest and is what proves the API. L1
  (freestanding) is a backend swap, do it once the surface settles.

## See also

- `crew/inle.md` — the L2 consumer (a bare-metal agent that needs its own POSIX).
- the ain / `ai_io` plan — the L0 pattern this generalizes (socket nifs in `main.c`).
- todo `#8` (cli.l → shell.l) / `#10` (POSIX shell) — staging step 3 lives there.
