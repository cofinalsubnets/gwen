# vmx — the Intel twin

The other half of doc/svm.md's question, answered on the other vendor. `(vmx-run ())` builds one
VMCS, enters a seven-byte 32-bit guest, and takes its CPUID exit.

Green on an Intel i5-8200Y under nested KVM (`tree.lan`), and skipped with a word everywhere else
— on AMD, and on aarch64 where the nom is not in the book. Four laws, one more than the SVM gate:

| law | what a failure would have meant |
|---|---|
| VM-instruction error `0` | the entry was taken, not refused |
| exit reason `10` | `EXIT_REASON_CPUID` |
| guest rax `4660` (0x1234) | a guest instruction **retired** |
| guest rip `5` | the CPUID's own address, one instruction in |

## it really is three times the file

The SVM spike is ~160 lines; this one is ~300, and every extra line traces to one of three things
the other vendor gave away free.

**The VMCS is opaque.** Forty-odd `vmwrite`s with a field encoding apiece, where the VMCB was
plain stores at documented offsets. This is the bulk of the difference and it is pure typing —
but typing where a single wrong encoding is a silently refused entry.

**A VM exit does not return.** `vmrun` came back to the instruction after itself; `vmlaunch`
resumes at the HOST_RIP in the VMCS, with the HOST_RSP in the VMCS. So the entry is an assembly
block with a label in it (`asmops.h`'s `k_vmlaunch`) rather than a call. And the two outcomes
arrive at the same place by different roads — a **refused** launch falls *through* to the next
instruction, a successful one lands on the label — so a marker register is the only thing that
tells them apart. It is set to 1 before the launch and to 0 on the label.

**Real mode needs "unrestricted guest", which needs EPT.** SVM ran a real-mode guest natively. To
avoid building EPT for a spike, this guest runs in 32-bit *paged* protected mode instead — which
means it brings a page directory (one page of 4 MiB PSE entries mapping 0..4 GiB onto itself),
and a TSS, and a GDT. Which brings the surprise:

## inle had never had a TSS

VMX checks that the host TR selector is non-zero, and the boot GDT is three entries — null, code,
data — with no `ltr` anywhere in the tree. So the spike writes its own GDT: the two boot
descriptors (written from mkboot.l's own constants rather than read back out of the live table,
which would mean dereferencing a GDTR base loaded before paging), plus a 64-bit TSS descriptor at
index 3. ⚠ Every exit reloads GDTR from it, so the boot GDTR is saved with `sgdt` and put back by
hand at the foot — these pages are a love string's bytes, and the collector is free to move them
the moment the nif returns.

## the ⚠ list

The one that actually bit, and cost a full build cycle on the remote box:

* **`vmxon` FAULTS rather than fails.** `#GP` at the instruction itself if the live CR0 does not
  satisfy `IA32_VMX_CR0_FIXED0` — which demands `NE`, a bit inle had never had a reason to set.
  The reconciled CR0 must be **written back**, not merely computed. Computing it and forgetting
  the write is a `#GP` at `vmxon` with rip pointing straight at it, which is at least an honest
  failure: `llvm-objdump` on the kernel ELF named the instruction in one step.

And the ones that answer a refused entry with nothing but a number in VM_INSTRUCTION_ERROR:

* **The VMCS link pointer is ~0, not 0.** Zero is a valid-looking shadow-VMCS pointer.
* **Control words must be reconciled** against the capability MSRs both ways: the low half is the
  bits that must be 1, the high half the bits that may be. Asking for a forbidden bit and failing
  to set a demanded one are the same failure. ⚠ The `TRUE_*` MSRs exist only when
  `IA32_VMX_BASIC` bit 55 says so; reading one on a part that lacks it faults.
* **The guest TR may not be unusable** — it gets a real busy-TSS descriptor even though nothing
  will ever task-switch to it. The guest LDTR, by contrast, must be marked unusable (bit 16).
* **CPUID needs no intercept bit.** It exits unconditionally under VMX — one of the few places
  Intel asks for less than AMD.
* **VMX saves no guest GPR.** The guest's rax is read *at the label*, because by the next
  instruction it is gone. The host's own GPRs are not restored either — rsp and rip are, and
  nothing else — so a guest that writes more than rax wants a save/restore stub around the launch
  rather than an inline. Sharper here than on SVM, where rax and rflags came back.

## what mooncc could not say

The SVM half found two limits of the neutral asm surface (doc/svm.md). VMX found a third, and it
shaped the interface:

* **There is no `"m"` constraint.** `vmxon`, `vmclear` and `vmptrld` take a *memory* operand
  holding a physical address, and mooncc cannot express one — so the neutral half takes the
  address in a register and names a base and a displacement (`lgdt`'s shape, `vmxon %0, 0`), where
  AT&T takes the operand directly. That is why every one of those wrappers takes a `uint64_t *`.

What did work, and was the open question before any of this was written: **a label and a
pc-relative `la` survive inside a mooncc asm template.** The template assembles to
`leaq 0x16(%rip), %rax` and a `jmp` over the landing site, exactly as intended. Without that, the
whole exit-path design would have needed a separate laid function.

## what it does not show

One vCPU, no EPT, no device model, no interrupt injection, and a guest that retires one
instruction. Guest-physical is host-physical here, so the guest's page directory maps it straight
onto the machine's memory — two instructions cannot abuse that, and it is exactly why EPT is the
next rung rather than an optional one.
