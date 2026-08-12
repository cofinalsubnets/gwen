# the kernel's own toolchain

The inle kernel (`port/inle/`) is compiled, assembled and linked entirely by us, on both arches
and through all three boot doors. `KCC ?= mooncc` compiles every TU, `port/inle/mkboot.l` and
`mkvec.l` lay the assembly as holo IR, and `crew/holo/link.l` does the link. **Nothing foreign
builds the kernel.**

What stays foreign in this tree is only what is meant to: the DDC leg (an ambient CC
cross-checking love0), the purely-source git-door bootstrap rim, and comparison targets — of
which the clang kernel is one, kept honest by `test_kdiff`. qemu, OVMF and limine are firmware,
not toolchain.

⚠ **Each KCC/KLINK variant has its own odir and its own ELF** (`kccsuf`/`klsuf`). Without that,
`KCC=clang` reuses the mooncc objects and the differential lane silently re-runs the artifact it
is supposed to be checking.

⚠ **mooncc REFUSES a `-m` flag rather than ignoring it** (silently dropping one would be a no-op
wearing a cc face), so the kernel's flags carry no `-m*` soup. Both machine flags were probed
vacuous before they were dropped, and the probes are the argument:

- **`-mno-red-zone`** — gen.l's prologue is `push rbp; mov rbp,rsp; sub rsp,F`, so the frame is
  allocated before a slot is addressed, and every scratch cell is a `sub sp,16` then a store.
  Empirically, across every mooncc-built object, zero functions address a slot below their
  allocated rsp and there are no negative-rsp displacements anywhere. (aarch64 has no red zone —
  AAPCS64 defines none, which is why only x86 carried the flag.)
- **`-mcmodel=kernel`** — mooncc emits `R_X86_64_64`, `PC32` and `PLT32` and nothing else, where
  clang's kernel objects carry thousands of `_32`/`_32S`. 32-bit absolutes are exactly what a
  code model exists to make safe in the top 2 GiB; having none, we need no knob.

## the link

`crew/holo/link.l` carries the kernel layout — the one real linker feature the `.lds` had that
we did not: **per-lane (vaddr, LMA) with multiple PT_LOADs**. `ldkern target entry vbase bias
srcs` is the whole surface; `port/inle/klink.l` is the driver, carrying the four numbers per
arch, which is all `<a>.lds` ever said that we could not. `KLINK=holo` is the default;
`KLINK=lld` puts ld.lld and the .lds back as the comparison lane. The layout lands on lld's
addresses exactly: same vaddrs, same paddrs, same entry.

Deliberately not reproduced: `--gc-sections` (the image carries some dead code, and it is RAM)
and `.rodata` string merging.

What foreign objects ask for that our own never did — all of it live in link.l because the clang
lane must keep working:

* **local symbols as relocation targets.** clang names every constant pool and jump table
  `.LCPI0_0` and relocates against THAT, where obj.l goes through the section symbol. A local
  defined symbol resolves in its own object (base + value) and never reaches the global book.
* **modular field arithmetic.** A 32-bit boot stub says `.boot + 0x80003000`, and that sum WRAPS
  the high-half base off to the physical address the pre-paging code jumps to. Every S+A goes
  through `ld-u64` first; `R_X86_64_32` reads it unsigned, `_32S` folds it signed (the kernel's
  high addresses are 32S's negative half).
* **the aarch64 ABS_LO12_NC family.** holo emits adrp+add, so `ADD_ABS_LO12` was the only LO12 we
  had; clang folds the add into the load and emits `LDST{8,16,32,64,128}_ABS_LO12_NC`, one field
  with a width-scaled imm12.

⚠ **The reader folds a hex literal at or above 2^63 into its negative twin** (`0xffffffff80200000`
reads -2145386496). The same bits, and every emit path takes it — but a LAYOUT divides, and
aligning a negative overshoots by a page with nothing downstream looking wrong. `ldkern` takes
both halves of the address split through `ld-u64` first.

## the inline-asm seam: one header, two spellings

`port/inle/<a>/asmops.h`, one per arch, is **the only place in the kernel that spells an
instruction**. Every asm site is a static inline behind a NAME (`k_rd_ttbr1_el1()`, `k_outb()`,
`k_sp_to_el1h()`, …), and the header says each one twice — holo's neutral template under
`__mooncc__`, the GNU string otherwise. `grep asm` over the kernel's C finds the header and
nothing else.

That is what makes the clang carve-out structural rather than incidental: the clang kernel stays
buildable forever as the differential twin, and `test_kdiff` has something to compare.

**The two dialects agree on more than they disagree.** A bare mnemonic (`cli`, `wfi`, `isb`) and
a `mnemonic op, op` line (`mrs %0, ctr_el0`, `dc cvau, %0`, `at s1e1w, %1`) read the SAME in both
once each compiler has put its own register names into `%0` — so those lines carry no `#ifdef`
at all. The divergences are exactly three, and enumerable:

1. AT&T's operand order and constraint letters (`"a"`/`"Nd"` where the neutral surface pins by
   register name, `"r0"`/`"r2"`);
2. the ops holo NAMES differently (`trap`, `dbrk`, `msri`, `ldcr`/`stcr`, and `lea d,s,0` for
   the SP move);
3. the `#`/`$` on an immediate.

⚠ **A multi-instruction template separates on `\n`, NEVER `;`.** The neutral reader takes `;` as
a comment to end of line, so a `;`-joined template assembles its first instruction and SILENTLY
DROPS the rest — no scare, a short block, and a barrier or an `isb` quietly missing. GNU is happy
with `\n` either way, so `\n` is the form that serves both.

holo's arm64 bitmask-immediate encoder takes bottom-aligned runs only, so `orr x9, x9, #(3<<20)`
materializes through a register in the neutral half — the one op whose two halves differ in
instruction COUNT.

One declared divergence in the gate: `k_divzero` only has to FAULT, and the neutral surface has
no 32-bit divide, so clang's half raises #DE with `divl` and ours with `divq`. Every other op
matches instruction for instruction on both arches.

The law is `test/gate/asmops.sh` (`make test_asmops`) — ⚠ **not in `test_slow`**, so it is one of
the few gates a `test_slow` run will not catch for you: one probe TU calling every
inline, compiled by BOTH compilers and compared op by op — same privileged mnemonics, same
symbolic operands, same order, same function. **The op list is read out of asmops.h itself**, so
adding an op and forgetting the probe fails the gate. It is the only check that can catch one
half of the header drifting from the other.

## the assembly is a lay

There are no `.S` files. `port/inle/mkvec.l` lays the exception/IRQ tail and
`port/inle/mkboot.l` the bring-up, one file per job rather than one per arch, since the scaffold
is shared and only the payload is per-ISA. GAS's `.macro exc_noerr/exc_err` and `.rept` loops are
love loops — the x86 stubs and the aarch64 vector slots **generate** rather than repeat, which is
what makes the error-code split and the table shape *stated* instead of transcribed.

The x86-32 helper table lives in mkboot.l, ~20 encodings, each llvm-mc-checked and each carrying
its AT&T source in the comment — holo lays x64, and teaching it a mode nothing else in the tree
needs would be the wrong shape. The PVH protocol is frozen, so it is write-once.

Three ops and a reloc kind exist for this:

* **`lia`** — the label's ABSOLUTE (linked) address, movabs on x86 and an inline literal on
  aarch64 — because `la` answers where a label IS RUNNING, and the whole difficulty here is that
  the image is linked high and runs low until the MMU comes on.
* **`push`** takes an immediate (the stubs).
* **`ldseg`** reads a segment register, ldcr's twin (archinit reads CS: the three doors leave
  three different selectors).
* **`abs32`** on x86, for the PVH note's `.long pvh32 - KVMA`.

⚠ **aarch64's `lia` wants an `('align 8)` in front of it.** The literal sits at the
instruction's own address + 8, so it is 8-byte aligned only when the site is, and an emitter
cannot know its own address. A 4-mod-8 literal is an unaligned 8-byte load: fine on Normal memory
while SCTLR.A is clear, an **Alignment fault on Device memory** whatever SCTLR says — which is
what the world looks like before the MMU comes on, where a boot stub most wants this.

⚠ **`lay` answers an empty stream for an unregistered backend.** A frontend bakes holo with the
NATIVE backend only and a cross target joins the cat at runtime; without it the object still
writes out whole with a 0-byte `.text`, and the failure surfaces only as a boot that goes
nowhere. `objsecs` checks the backend is registered (`obj-no-backend`), not merely named.

⚠ **An immediate slot will take a SYMBOL and assemble it.** Every other operand refuses a
stranger through `rn` (`badreg`), but an immediate gets arithmetic done to it, and a symbol
survives arithmetic: `(li r6 a-tbl)` inside a QUOTED IR block (where the name never evaluated)
assembles quietly to the wrong constant. Both `li` emitters check now — `badimm`.

## named sections

`objsecs target secs funcs globs weaks locals` takes a LIST of `(name type flags align forms)`,
and `objelf` is a four-element call to it (.text/.data/ai_nifs/.image). The kernel's lays are the
other caller and want names a compiler never emits: `.boot`, `.boot.text`, `.note.pvh`
(SHT_NOTE, which the PVH loader READS), `.rodata`, `.bss` (SHT_NOBITS — it lays its forms for
the labels and the size, then writes no file bytes) and a 2 KiB-aligned `.text.vectors`.

**One section is one LAY**, which is exactly what a section boundary means: a branch inside it
resolves in place, a reference across it relocates.

The two relocation emitters are one, dispatching on the fixup's KIND rather than on the section
— the old split was never the real line, since an executable section carries an abs64 too (the
hop out of the low mapping), and merging closed a hole where a cross-section reference fell
through to an UNDEF.

⚠ On the compiler half, `__attribute__((section(NAME)))` must work **for any name**, not a
known two: the limine door needs `.limine_requests` to BE a section, because a bootloader scans
for it — a request block quietly sitting in `.data` is a kernel that boots with an empty memmap,
and three of them bracket the scan, so both their existence and their order are the protocol's.
A named section on the arm32 writer, which has no section list, SCARES rather than being dropped.

## the privileged vocabulary

The privileged instructions are ordinary holo IR ops, and the text front-end takes them with no
grammar change — which is the seam mooncc's inline asm parses through (`asm-text`).

* **arm64.l**: `mrs`/`msr` over a sysreg table, `msri` (the PSTATE immediate:
  daifset/daifclr/spsel), `dsb`/`dmb` (+ domains) / `isb`, and the SYS family
  `tlbi`/`ic`/`dc`/`at` — an operation NAME then a register, `zr` for the whole-system forms.
  Exception generation `brk`/`hvc`/`smc`/`udf` plus `dbrk`, and `wfi`/`wfe`/`eret`.
* **x64.l**: `cli`/`sti`/`hlt`, `ud2`, `iretq`, `swapgs`, `rdmsr`/`wrmsr`, `cpuid`/`rdtsc`,
  `int n`, `ltr`, `ldcr`/`stcr` (mov from/to crN), `lgdt`/`lidt`/`invlpg`, `in`/`out`.

MRS/MSR and the SYS family are ONE encoding wearing four faces — fixed head, an L bit, and a
16-bit selector op0:op1:CRn:CRm:op2 at [20:5] — so a system register is its selector with op0 =
2 or 3, and a SYS operation is the same shape with op0 = 1. One `sreg` packer under all of it,
and every table row is one instruction.

⚠ **`mov` cannot say SP on arm64.** It is ORR against XZR, and encoding 31 in THAT form reads as
the zero register — so `(mov r9 sp)` would quietly answer zero. Kernel code moves SP by name
(`mov x9, sp` around an `msr spsel, #1`), so `mov` SCARES on sp and points at `(lea d s 0)`,
add-#0, which is what the assembler writes.

`int 3` stays the two-byte `CD 03`. An assembler folds it to `CC`, which is what `trap` already
emits; keeping the long form means the instruction you wrote is the one you get (they part
company under vm86).

Not added, because nothing asks: `dc` maintenance-by-set/way, ASID-scoped tlbi variants,
`sgdt`/`sidt`, the debug registers. One row is one instruction, so each is a two-line change when
a caller appears.

The laws are goldens in `test/holo/golden.l` plus `test/holo/fuzz/sysdiff.py` in `test_holofuzz` — a
byte-exact differential that ASSEMBLES the intended text with llvm-mc and demands the same bytes.
It reads the arm64 op tables out of arm64.l itself, so a row added to holo is checked with no
edit to the harness.

## what the compiler had to grow

Five things the kernel needed from mooncc that a hosted build never asks for. They are recorded
because each was silent in a different way:

* **`__STDC_HOSTED__` is 0 under `-ffreestanding`.** love.c asks the standard's own question to
  choose the W^X mmap arena over the freestanding heap copy; answering 1 unconditionally made
  the kernel take the hosted lane and reach for `sysconf`. It rides in as a `-D` (cpp applies
  those after its predefines), which keeps it out of the flag accumulators. ⚠ NOT the rest of
  the `-nostd*` family: `-nostdlib` alone is a hosted program supplying its own runtime, which
  is exactly what `test_raw` is.
* **`-nostdinc`**, threaded through `incload`. LOUD, never advisory: with the `/usr/include`
  tail on, a header we do not carry resolves to glibc's, and a freestanding build quietly taking
  a hosted declaration is the wrong artifact wearing a green face.
* **`_Static_assert(expr)`**, the one-argument C23 form. The message was only ever the
  diagnostic's wording.
* ⚠ **`sizeof` of a LATER declarator with an inferred `[]` bound.** `char a[]={1}, b[]={2,2};`
  left b INCOMPLETE and `sizeof(b)` answered 0 in SILENCE — the file-scope path parses a later
  declarator's dims before it sees the initializer, so only the first ran the inference. The
  bytes were always laid correctly; only the type was wrong, which is the worse half, since a
  `countof()` over b read zero and every loop over it did nothing.
* **`__attribute__((section(NAME)))` for any name** — above.

`aarch64/builtins.c` is outside the moon lane: it supplies `__clear_cache` and `__udivti3`, the
two calls a foreign compiler's codegen emits and then has to be handed somewhere. Ours emits
neither — it lowers `__builtin___clear_cache` to the dc/ic sequence inline and never reaches for
a 128-bit divide — so its objects reference no such symbol. (Just as well: the file is written in
`__int128`, a type we do not carry.)

## the gates

* **`test_kernel`** / **`test_kernel_arm64`** — the corpus over `qemu -kernel`, both arches.
  ⚠ `test_kernel_arm64` crosses with mooncc's `-t`, not clang's `-target`.
* **`test_uefi`** — the same corpus through our own BOOTX64.EFI (doc/uefi.md).
* **`test_vec`** — the interrupt tail. A green boot exercises most of mkvec's lay already
  (nothing boots without archinit's IDT, and the corpus is FED over the serial line and CLOCKED
  by the timer, so uart_isr and timer_isr run thousands of times per gate). What a green boot
  never touches is the part that runs when something goes wrong, so this gate **makes something
  go wrong**: `(fault n)` raises a real CPU exception and the report is read back. #PF is the
  load-bearing case — an error-code vector that reports `err` AND `cr2`, so a stub on the wrong
  side of the split shifts the frame and misreports all three. The stubs no boot reaches are
  checked against the architecture's own error-code list.
* **`test_asmops`** — the two spellings, op by op.
* **`test_kdiff`** — the clang-vs-mooncc differential, both compilers over both arches. OPT-IN
  (~45 s/arch on top of test_kernel): test_kernel already gates the artifact we ship, and the
  twin's job is to be there when the kernel breaks. Run it when the kernel moves.

The house habit that built all of this and is worth keeping: **write the new lay, assemble the
old one with the foreign tool, compare bytes.** `.rodata` (the ISR and gate-type tables) and
`.boot` (the page tables and the GDT) came out byte-identical that way, the 32-bit prologue
instruction-for-instruction, and the relocations matched addend for addend including the KVMA
fold — and it caught a mistyped `TCR_EL1` constant that reading twice would not have.
