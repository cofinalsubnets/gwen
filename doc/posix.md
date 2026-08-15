# posix — the OS layer for love user programs

love user programs (and lush) get a real POSIX environment — files, processes, pipes, signals,
sockets — so people can write real apps. The design decision: **don't reimplement POSIX; expose
the one already under us.** On the host, love is already a Unix process — the OS layer isn't
something to build, it's something to *surface*. And it is **host-agnostic**: the target is the
POSIX standard, not a kernel, so the same nifs run on Linux, a BSD, or macOS.

This is the **`ain` pattern, generalized.** ain wraps the socket syscalls as host nifs; the
POSIX layer is "do that for the rest of the syscall surface."

## Three strata

| stratum | what | host needs |
|---------|------|------------|
| **L0**  | host nifs over the real kernel (libc) | any POSIX host |
| **L1**  | same nifs, freestanding / raw syscalls | a kernel ABI |
| **L2**  | the freestanding KERNEL grows its own POSIX | bare metal |

L0 is what ships. L1 and L2 are the same *interface* reached without a host libc (L1) or
without a host at all (L2) — the surface is designed once at L0 and the backends swap
underneath.

### L0 — ride the host Unix

love-the-host-process already calls `read`/`write`/`malloc`. L0 widens that to the POSIX
surface as nifs:

> every host nif is `host_X` (an `ai_noinline` syscall worker) + `lvm_X` (the VM
> tail wrapper) + a `nif_X[]` thread registered via `AI_NIF` in a `host/*.c` file
> (auto-globbed — no love.c/love.h/main.c edit; main.c is core). The fd→port path is
> free: `ai_io_alloc(g,fd)` wraps any fd as a port with a close finalizer, and
> read/write then come free via getc/putc. The general-POSIX nifs wear the
> `posix_` C-symbol prefix (host/posix.c: `lvm_posix_stat` &c); the love names stay
> the plain POSIX words.

The payoff: **lush is a real shell whose external commands are the host's programs** — love
runs `ls`/`grep`/`git` and pipes between them.

Gating mirrors ain: host-only `#ifdef`; the kernel (`kmain.c`) and wasm don't link `main.c` so
they auto-exclude; `prel.l` stays syscall-free; the smokes ride `test_hostnif`, not the portable
corpus.

### L1 — freestanding on a host ABI

The same nifs, but the binary is libc-free and issues raw syscalls (`syscall`/`svc`)
— converging the host build with the kernel's existing freestanding discipline
(`-ffreestanding -Wall -Wextra -Werror`). One small per-ABI backend (the syscall
instruction + the errno convention) replaces libc; the nif surface above is
unchanged. Drops a dependency, doesn't change the API.

### L2 — the freestanding kernel grows its own POSIX

When there's no Unix under you (the `inle` bare-metal kernel, `free/`), the kernel must
*be* the OS. This is the only stratum that's a real OS build: a ramfs VFS, an in-kernel process
model, signal delivery. The `k_sources[]` table in `kmain.c` is already vfs-shaped, so L2 grows
from there. **The rung-by-rung plan is `doc/inle.md`** — one address space, tasks as the
processes, this surface answered against a ramfs.

## The concept → primitive map

| POSIX                          | love surface / backing                                   |
|--------------------------------|--------------------------------------------------------|
| process / thread               | **task** — `spawn`/`wait`/`done?`/`chill` (the cooperative scheduler) |
| `fork`/`exec`/`waitpid`/`_exit`| `fork` `exec` `wait` `quit` (host/posix.c)             |
| file descriptor                | **port** via `ai_io_alloc` + the `k_sources[]` vtable  |
| `open`/`read`/`write`/`close`  | `open`/`close` + getc/putc; `lseek` over the raw-fd `openfd` lane |
| `dup2`/`pipe`                  | `dup` `dup2` `pipe` (a pair of fds)                    |
| `stat`/`mkdir`/`unlink`/readdir| `stat` `mkdir` `rmdir` `unlink` `readdir` `rename` `symlink` `readlink` `hardlink` `chmod` `chown` `utime` `umask` |
| `cwd` — `chdir`/`getcwd`       | `chdir` `cwd`                                           |
| signals — `sigaction`/`kill`   | **the condition system**: `signal`, `sigfd`/`sigtake`, `still` |
| environment                    | `getenv` `setenv` `environ`; cli.l parses argv          |
| exit codes / std streams       | `in`/`out`/`err` ports; `quit`                          |
| sockets (BSD)                  | **ain** — `connect`/`listen`/`accept`/`shutdown`/DNS (host/sock.c) |
| time — `clock_gettime`         | `ai_clock` / `(clock t)`                                |
| `select`/`poll`                | `ai_wait_fds` / `ai_ready` (the scheduler's core)       |

Two mappings are the elegant ones:

- **signals → conditions.** POSIX signal delivery is exactly a `raise`: a `SIGINT`
  becomes `(scare 'sigint pid)` routed to the installed `help`; a handler is a `help`
  policy; `welp` is the default disposition. No new mechanism — the condition system
  *is* the signal machinery. `sigfd` takes a signal LIST and turns any of them into perceive
  DATA, `(signo . pid)`, re-raisable through `help`.
- **fds → ports, select → `ai_wait_fds`.** The cooperative scheduler already blocks
  tasks on fds and wakes the ready one. Two `spawn`ed pumps on two fds interleave with
  no select loop (this is why ain's bidirectional pump is ~free).

## Conventions

An effect op answers `()` on success | a POSITIVE errno | EINVAL on misuse; a value op answers
the value | `()`. `stat` answers `(size mtime-ms mode ns)` — ns the whole mtime in nanoseconds,
one charm, cook's build-grade resolution — or `()` for absence. `spawn` answers a pid or a
negative errno, and a child that cannot exec `_exit(127)`s. `setenv` with a non-string value
unsets (the absence lane). Wrap at the call boundary — readdir/stat struct layouts and errno
values differ across Linux/*BSD/mac, so the `call_X` worker normalizes and love sees a stable
shape.

The shell's job control rides this: per-job process groups + tcsetpgrp handoff (`spawnio`
pg/fg, `ttyfg`), ^C/^Z to the foreground job only, jobs/fg/bg/&. ⚠ a stop signal to an
ORPHANED group is discarded, so in-shell-pgrp children can never ^Z under a nested session.
Task-level `chill`/thaw stays separate — **tasks are not processes**: a task is an in-VM green
thread (`spawn`/`chill`), a process is a host pid (`fork`/`still`). Never cross them.

## Open

- **`fork` semantics without exec.** love's heap is a two-space copying GC; `fork` is a
  host-process primitive (copy-on-write at the OS level), orthogonal to the love heap — the
  child gets its own address space from the kernel. exec-after-fork is the safe pattern; a fork
  *without* exec (two live love VMs) needs thought.
- **Widening, as something asks**: the L0 surface grows a call at a time rather than by
  scaffolding.
