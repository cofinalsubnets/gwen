# vmx — the Intel twin

The other half of doc/svm.md's question, answered on the other vendor. `(vmx-run ())` builds one
VMCS, enters a seven-byte 32-bit guest, and takes its CPUID exit.

Green on two Intel parts under nested KVM — an i5-8200Y (`tree.lan`, Amber Lake) and a Celeron
J4125 (`tau.lan`, Gemini Lake) — and skipped with a word on AMD, and on aarch64 where the nom is
not in the book. Four laws, one more than the SVM gate:

| law | what a failure would have meant |
|---|---|
| VM-instruction error `0` | the entry was taken, not refused |
| exit reason `10` | `EXIT_REASON_CPUID` |
| guest rax `4660` (0x1234) | a guest instruction **retired** |
| guest rip `5` | the CPUID's own address, one instruction in |

## it really is three times the file

The SVM spike is ~160 lines; this one is ~360, and every extra line traces to one of three things
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

**Real mode needs "unrestricted guest", which needs EPT.** SVM ran a real-mode guest natively.
This guest runs in 32-bit *paged* protected mode instead — which means it brings a page directory
(one page of 4 MiB PSE entries), and a TSS, and a GDT. Which brings the surprise:

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

## EPT: the guest gets an address space

The guest runs under EPT, so there are **two** translations under every fetch — its own page
directory turns a linear address into a guest-physical one, and the EPT turns that into a
host-physical one. The guest believes its code is at guest-physical `0` and its page directory at
`0x1000`. Neither is true of the machine: both pages live wherever the collector put the love
string they were carved from.

That makes the existing laws prove one thing more than they did. Host physical `0` is the
machine's own low memory, so a sentinel of `0x1234` coming back could only have been executed out
of the page the EPT pointed at — an untranslated fetch would have run whatever lies at the bottom
of RAM, and would not have produced it.

Four levels down to 4 KiB leaves, mapping exactly the two pages the guest can reach. ⚠ A leaf
carries a memory type in bits 5:3 where the upper levels carry only the three permission bits;
write-back is 6, and an EPT leaf with no memory type is a refused entry. The EPTP wants the same
memory type plus a walk length given as `levels - 1`. ⚠ EPT is reached only through the
*secondary* controls, so the primary word must set "activate secondary controls" (bit 31) first —
and the secondary word has no `TRUE_` twin, so `IA32_VMX_PROCBASED_CTLS2` is the whole truth about
what a part will take.

EPT is not a fallback here: `k_vmx_ok` requires it, along with a 4-level walk and write-back. Every
VMX part since Nehalem answers yes, and both test boxes do.

## what mooncc could not say — and now can

The SVM half found two limits of the neutral asm surface (doc/svm.md). VMX found a third, and that
one was **fixed rather than worked around**: mooncc now takes `"m"`, lowering the address of its
operand into a picked register and substituting `rN, 0` — the base and displacement holo spells a
memory operand with. So `vmxon %0` reads as `vmxon r3, 0` on our half and `vmxon (%rax)` on
clang's, from one template with no `#ifdef`; the six memory-operand wrappers in `asmops.h` are
shared lines now. doc/moon-kernel.md carries the details, `crew/moon/law.l` the law.

What worked from the start, and was the open question before any of this was written: **a label
and a pc-relative `la` survive inside a mooncc asm template.** The template assembles to
`leaq 0x16(%rip), %rax` and a `jmp` over the landing site, exactly as intended. Without that the
whole exit-path design would have needed a separate laid function.

## what it does not show

One vCPU, no device model, no interrupt injection, and a guest that retires one instruction. What
EPT buys is the address space, not the isolation story: the two mapped pages are all the guest can
reach, but nothing yet stops a *host* bug from handing it more.
