# svm — a guest under inle

The spike behind doc/inle.md's open question: **can a guest run under inle, and does the exit land
back in ordinary C?** Both answers are yes, and the second is the one that mattered.

`(svm-run ())` builds one VMCB, enters a six-byte real-mode guest, and takes its CPUID intercept.
The gate (`test/kernel/svm.l`) reads three numbers back and they separate three different
successes:

| law | what a failure would have meant |
|---|---|
| exit code `114` (0x72) | not `-1` — the entry passed every consistency check |
| guest rax `4660` (0x1234) | a guest instruction **retired**; only the guest could have put it there |
| guest rip `3` | the intercept fired at the CPUID's own address, one instruction in |

Green on three lanes: x86_64 under KVM (nested SVM on AMD silicon), x86_64 under TCG (qemu's
`qemu64` model carries `svm`, so a box without `/dev/kvm` still runs the laws), and aarch64, where
the nom is not in the book and the file skips with a word.

## why this was the cheap half

`vmrun` returns to **the instruction after itself**. So the exit handler is straight-line C in
`k_svm_spike` — no host-RIP entry point, no stack to build, and nothing the love VM above has to
know about. That is the whole reason the spike is ~160 lines: the property being tested turned out
to be free on this vendor.

It also cost nothing structural. `free/x86_64/*.c` is wildcarded by `kernel.mk`, so the file
joined the build with no makefile edit; the guest's pages ride a love string's own bytes (blk.c's
DMA trick, safe for the same reason — nothing allocates between the carve and the `vmrun`, so the
collector cannot move the VMCB out from under the CPU); and there is no new global anywhere.

## the ⚠ list — five things that each silently answer −1, and one that kills the machine

Written down because every one of them presents as something other than what it is:

* **The VMRUN intercept must be set** in the guest's own VMCB (word 4, bit 0). Clear, and `vmrun`
  exits INVALID with nothing else to say.
* **The ASID must be non-zero.** Zero is the host's.
* **The guest's EFER.SVME must be set** in the save area — the guest's, not just the host's.
* **The IOPM and MSRPM base addresses must be valid physical addresses**, whether or not the
  matching intercepts are on. Hence the five otherwise-pointless pages.
* **RFLAGS bit 1 reads 1, always.**
* **`#VMEXIT` clears GIF.** Between the exit and the `stgi`, the machine takes no interrupt at
  all. Leave the `stgi` out and the timer never ticks again — a deaf machine wearing a hang's
  face, and it would be diagnosed as anything but.

And one the hardware does not warn about: `#VMEXIT` restores rax, rsp, rip, rflags, the segments
and the control registers — **and no other GPR**. rbx/rcx/rdx/rsi/rdi/r8–r15 come back holding
whatever the guest left in them. This guest writes exactly one register and it is rax, which is
why an inline asm is honest here; a guest that runs real code wants a save/restore stub around
`vmrun` instead.

## what it does not show

One vCPU, no nested paging (guest paging is off, so guest-physical *is* host-physical), no device
model, no interrupt injection, and a guest that retires one instruction. Nothing here says a Linux
kernel would boot — only that the floor it would stand on holds.

## the seam, and where the Intel twin goes

Five instructions joined the tree, all of them `0f 01 /3` and all register-contracted like the
port I/O already there — `vmrun`, `vmload`, `vmsave`, `stgi`, `clgi`. Three places knew about
them, which is the checklist for the next one:

* `crew/holo/holo.l`'s `ir-arity` — **an op absent here is `bad-op` at bake time**, before the
  emitter table is ever consulted. This is the row that is easy to forget.
* `crew/holo/x64.l`'s emitter table, and `test/holo/golden.l` to freeze the bytes.
* `free/x86_64/asmops.h`, both dialects, plus a call in `test/gate/asmops.c` — and
  `asmops.sh`'s x64 filter, which is a **whitelist** of privileged mnemonics: an op missing from
  it reads as "emitted no privileged instruction" rather than as an unknown op.

Two limits of mooncc's asm surface were found the hard way and are worth knowing before writing
the VMX half, which needs `vmread`/`vmwrite` and therefore more operands than anything here:

* **An output may not be pinned to a register that is also an input.** There is no spelling of
  GNU's tied `"0"` constraint on the neutral surface. This is why `k_cpuid` answers ebx/ecx/edx
  and not eax.
* **Only r0–r3 and r5–r10 are nameable** — the frame register and the callee-saved four are
  refused, so the full GPR clobber list cannot be written on that half. It does not need to be:
  an asm function's homing is off, so nothing of mooncc's own lives in a register across the
  statement. `asmops.h` carries the divergence with its reason.

The VMX twin is a second backend behind the same door, not a rewrite of this one — but it will
feel bigger for the two reasons named in doc/inle.md: the VMCS is opaque (every field through
`vmread`/`vmwrite` with an encoding table, where the VMCB is plain stores at documented offsets),
and the exit needs a host-RIP entry point where `vmrun` just came back.

**It landed — doc/vmx.md.** Both predictions held, and it came to three times this file. The
third cost was not predicted here: real mode needs "unrestricted guest", which needs EPT, so the
Intel guest runs 32-bit paged instead and brings a page directory, a TSS and a GDT with it.
