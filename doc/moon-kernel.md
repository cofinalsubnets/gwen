# the kernel onto mooncc -- THE PLAN

the inle kernel (port/inle/) is the LAST clang island: userland, the UEFI
loader (PE32+ via crew/holo/pe.l), the riscv virt lane, the MCU ports
(mps2/teensy/rp2040 ride `mooncc -t thumb*`) and the whole host build are
mooncc already. after this ladder the only foreign compilers left are the
deliberate ones: the DDC leg (an ambient CC cross-checking love0), the
purely-source git-door bootstrap rim, and benchmarking / reverse-engineering
comparison targets. qemu, OVMF and limine are firmware, not toolchain -- they
stay what they are. drafted 2026-07-28; trued up as rungs land.

## what clang does today (the inventory, as drafted)

* **C** (port/inle/kernel.mk:33-56): love.c + am.c, crew/quay/*.c, libc/*.c,
  kmain.c, port/inle/<a>/{arch,pvh,dtb,builtins}.c under
  `$(ai_cflags) -nostdinc -ffreestanding -fno-lto -fno-PIC
  -ffunction-sections -fdata-sections` + per-arch `-mno-red-zone
  -mcmodel=kernel` (x86) / `-mcpu -march` (arm).
* ~~**inline asm**, ~35 sites, GNU spelling~~ -- BOTH spellings since rung 3
  below, in `port/inle/<a>/asmops.h`, and no call site says either. what they
  were: x86 cli/hlt/in/out/mov-cr/int3/ud2 + the CR0/CR4 SSE enable; arm64
  mrs/msr over ~10 sysregs, msr-immediate (daifset/daifclr/spsel), dsb/isb,
  tlbi, ic/dc, `at s1e1w`, brk/udf/hvc/wfi, and register-asm locals
  (`asm("x0")`). mooncc's inline asm is the NEUTRAL template (doc/moon.md's
  inline-asm section) -- an AT&T/ARM front-end was deliberately deferred, and
  this plan keeps it deferred: the kernel is OURS, so the sites moved to the
  neutral surface instead.
* ~~**assembly files**, four~~ -- GONE since rung 4 below: `port/inle/mkboot.l`
  and `port/inle/mkvec.l` lay `boot.o` and `vec.o` as holo IR, and no assembler
  runs in the kernel build at all. what they were: <a>/boot.S (the PVH stub --
  ~50 lines of .code32 before long mode -- and the EL1 MMU stub) and
  <a>/<a>.S (the exception/IRQ vector stubs, GAS .macro loops, iretq/eret,
  context plumbing).
* ~~**the link**~~ -- OURS since rung 2 below; ld.lld and <a>/<a>.lds are the
  `KLINK=lld` comparison lane. what they said: five named PT_LOADs (boot |
  limine_requests | text | rodata | data), every section vaddr HIGH with an
  `AT()` LMA bias (vaddr - KVMA + phys base -- what lets `qemu -kernel` load
  low by p_paddr while limine maps high by p_vaddr), ENTRY by symbol,
  4K max-page-size, kimage_end, DISCARD .note/.eh_frame, --gc-sections.

## the rungs (each green and useful on its own)

### 1. holo grows the SYSTEM vocabulary -- LANDED 2026-07-28

the privileged instructions are ordinary holo IR ops now, and the text
front-end takes them with no grammar change (which is the seam mooncc's
inline asm parses through -- `asm-text` is what rung 3's templates land in).

* arm64.l: `mrs`/`msr` over a ~score-row sysreg table, `msri` (the PSTATE
  immediate: daifset/daifclr/spsel), `dsb`/`dmb` (+ domains) / `isb`, and
  the SYS family `tlbi`/`ic`/`dc`/`at` -- an operation NAME then a register,
  `zr` for the whole-system forms. exception generation `brk`/`hvc`/`smc`/
  `udf` plus `dbrk`, and `wfi`/`wfe`/`eret`.
* x64.l: `cli`/`sti`/`hlt`, `ud2`, `iretq`, `swapgs`, `rdmsr`/`wrmsr`,
  `cpuid`/`rdtsc`, `int n`, `ltr`, `ldcr`/`stcr` (mov from/to crN),
  `lgdt`/`lidt`/`invlpg`. `in`/`out` were already there.
* MRS/MSR and the SYS family turned out to be ONE encoding wearing four
  faces -- fixed head, an L bit, and a 16-bit selector op0:op1:CRn:CRm:op2
  at [20:5] -- so a system register is its selector with op0 = 2 or 3 and a
  SYS operation the same shape with op0 = 1. one `sreg` packer under all of
  it, and every table row is one instruction.
* the laws: goldens in holotest (542 now), plus `crew/holo/fuzz/sysdiff.py`
  in test_holofuzz -- a byte-exact differential that ASSEMBLES the intended
  text with llvm-mc and demands the same bytes. it reads the arm64 op tables
  out of arm64.l itself, so a row added to holo is checked with no edit to
  the harness. 910 encodings, zero discrepancies.

two findings worth carrying:

* ⚠ **`mov` cannot say SP on arm64.** it is ORR against XZR, and encoding 31
  in THAT form reads as the zero register -- so `(mov r9 sp)` would have
  quietly answered zero. kernel code moves SP by name (`mov x9, sp` around
  an `msr spsel, #1`), so this was a live hole: `mov` now SCARES on sp and
  points at `(lea d s 0)`, add-#0, which is what the assembler writes.
* `int 3` stays the two-byte `CD 03`. an assembler folds it to `CC`, which
  is what `trap` already emits; keeping the long form means the instruction
  you wrote is the one you get (they part company under vm86).

not added, because nothing asks: `dc` maintenance-by-set/way, ASID-scoped
tlbi variants, `sgdt`/`sidt`, the debug registers. one row is one
instruction, so each is a two-line change when a caller appears.

### 2. the holo KERNEL LINK lane (vaddr =/= paddr) -- LANDED 2026-07-28

crew/holo/link.l grew a kernel layout -- the one real linker feature the
.lds had that we didn't: per-lane (vaddr, LMA) with multiple PT_LOADs.
`ldkern target entry vbase bias srcs` is the whole surface;
port/inle/klink.l is the driver (it carries the four numbers per arch,
which is all <a>.lds ever said that we could not); `KLINK=holo` is the
default in port/inle/kernel.mk and `KLINK=lld` puts ld.lld and the .lds
back as the comparison lane. all five doors boot the file it writes:
`qemu -kernel` on both arches, our own BOOTX64.EFI, and the limine iso on
both arches -- 3558 tests pass through each. the clang-built objects are
laid unchanged, so this rung carries zero compiler risk, as planned.

what the clang objects asked for that our own never had:

* **local symbols as relocation targets.** clang names every constant pool
  and jump table `.LCPI0_0` and relocates against THAT, where obj.l always
  goes through the section symbol. a local defined symbol now resolves in
  its own object (base + value) and never reaches the global book.
* **modular field arithmetic.** boot.S's 32-bit stub says `.boot +
  0x80003000`, and that sum WRAPS the high-half base off to the physical
  address the pre-paging code jumps to. every S+A now goes through
  `ld-u64` first; R_X86_64_32 reads it unsigned, _32S folds it signed
  (the kernel's high addresses are 32S's negative half -- 2761 sites).
* **the aarch64 ABS_LO12_NC family.** holo emits adrp+add, so ADD_ABS_LO12
  was the only LO12 we had; clang folds the add into the load and emits
  LDST{8,16,32,64,128}_ABS_LO12_NC, one field with a width-scaled imm12.
* ⚠ **the reader folds a hex literal at or above 2^63 into its negative
  twin** (`0xffffffff80200000` reads -2145386496). the same bits, and
  every emit path takes it -- but a LAYOUT divides, and aligning a
  negative overshoots by a page with nothing downstream looking wrong.
  ldkern takes both halves of the address split through `ld-u64` first.

not reproduced, deliberately: --gc-sections (the image carries some dead
code -- .text 0x3b340 against lld's 0x37d80 -- and it is RAM), and
.rodata string merging. the layout otherwise lands on lld's addresses
exactly: same vaddrs, same paddrs, same entry.

### 3. the inline-asm seam: one header, two spellings -- LANDED 2026-07-29

`port/inle/<a>/asmops.h`, one per arch, is the only place in the kernel that
spells an instruction now. every asm site is a static inline behind a NAME
(`k_rd_ttbr1_el1()`, `k_outb()`, `k_sp_to_el1h()`, ...), and the header says
each one twice -- holo's neutral template under `__mooncc__`, today's GNU
string otherwise. all 40 sites across kmain.c, both arch.c's and aarch64's
builtins.c are spelling-free; `grep asm` over the kernel's C finds the header
and nothing else.

this is what makes gwen's carve-out STRUCTURAL rather than incidental: the
clang kernel stays buildable forever as the differential twin, and rung 5's
compiler-vs-compiler `K_TEST` has something to compare.

the moon-side halves of the rung, both small and both loud:

* `__mooncc__`, a new cpp.l predefine -- who is compiling, where every other
  predefine says something about the language or the machine.
* `-nostdinc`, threaded through `incload` (which already refused it -- an
  unknown dash arg -- so kcflags would have died at the flip). LOUD, never
  advisory: with the /usr/include tail still on, a header we do not carry
  resolves to glibc's, and a freestanding build quietly taking a hosted
  declaration is the wrong artifact wearing a green face.

the law is `test/gate/asmops.sh` (`make test_asmops`, in test_all): one probe
TU calling every inline, compiled by BOTH compilers and compared op by op --
same privileged mnemonics, same symbolic operands, same order, same function.
the op list is read out of asmops.h itself, so adding an op and forgetting the
probe fails the gate. rung 5's differential in miniature, and the only check
that can catch one half of the header drifting from the other.

what the rung turned up:

* **the two dialects agree on more than they disagree.** a bare mnemonic
  (`cli`, `wfi`, `isb`) and a `mnemonic op, op` line (`mrs %0, ctr_el0`,
  `dc cvau, %0`, `at s1e1w, %1`) read the SAME in both once each compiler has
  put its own register names into `%0` -- so those lines carry no `#ifdef` at
  all. the divergences are exactly three, and enumerable: AT&T's operand order
  and constraint letters (`"a"`/`"Nd"` where the neutral surface pins by
  register name, `"r0"`/`"r2"`), the ops holo NAMES differently (`trap`,
  `dbrk`, `msri`, `ldcr`/`stcr`, and `lea d,s,0` for the SP move), and the
  `#`/`$` on an immediate.
* ⚠ **a multi-instruction template separates on `\n`, NEVER `;`.** the neutral
  reader takes `;` as a comment to end of line, so a `;`-joined template
  assembles its first instruction and SILENTLY DROPS the rest -- no scare, a
  short block, and a barrier or an isb quietly missing. GNU is happy with `\n`
  either way, so `\n` is the form that serves both, and the three `;`-joined
  blocks the kernel had were rewritten.
* holo's arm64 bitmask-immediate encoder takes bottom-aligned runs only (a
  documented choice with a documented escape hatch), so `orr x9, x9, #(3<<20)`
  materializes through a register in the neutral half. the one op whose two
  halves differ in instruction COUNT; growing the encoder to the full
  replicated-rotated form is rung-1 work, and nothing asks for it yet.
* one declared divergence in the gate: `k_divzero` only has to FAULT, and the
  neutral surface has no 32-bit divide, so clang's half raises #DE with `divl`
  and ours with `divq`. every other op matches instruction for instruction on
  both arches.

### 4. the .S files become LAYS (the mksys precedent) -- LANDED 2026-07-29

the four .S files were exactly mksys.l's class -- register-exact leaves C
cannot say -- and they are holo IR lays beside it now. `port/inle/mkvec.l`
(the exception/IRQ tail) and `port/inle/mkboot.l` (the bring-up), one file per
job rather than one per arch, since the scaffold is shared and only the payload
is per-ISA. the GAS `.macro exc_noerr/exc_err` and `.rept` loops are love loops
-- the 32 x86 stubs and the 16 aarch64 vector slots generate rather than
repeat, which is what makes the error-code split and the table shape *stated*
instead of transcribed. no assembler runs in the kernel build.

what the rung needed under it:

* **holo's object writer grew NAMED SECTIONS.** `objelf` emitted exactly four
  (.text/.data/ai_nifs/.image); the kernel needs .boot, .boot.text, .note.pvh,
  .rodata, .bss and a 2 KiB-aligned .text.vectors. `objsecs` now takes a LIST of
  `(name type flags align forms)` and `objelf` is a four-element call to it.
  one section is one lay, which is what a section boundary means: a branch
  inside it resolves in place, a reference across it relocates. test_fixpoint
  says the refactor is byte-identical for hosted objects.
* **the two relocation emitters became one**, dispatching on the fixup's KIND.
  the old split (text vs data) was never the real line -- an executable section
  carries an abs64 too, which is exactly what the hop out of the low mapping
  is. it also closed a hole: a reference from one section to a label in another
  used to fall through to an UNDEF.
* **three ops and a reloc kind.** `lia` -- the label's ABSOLUTE (linked)
  address, movabs on x86 and an inline literal on aarch64 -- because `la`
  answers where a label IS RUNNING and the whole difficulty here is that the
  image is linked high and runs low until the MMU comes on. `push` takes an
  immediate (the stubs). `ldseg` reads a segment register, ldcr's twin (archinit
  reads CS: the three doors leave three different selectors). `abs32` on x86,
  for the note's `.long pvh32 - KVMA`.
* **the x86-32 helper table lives in mkboot.l**, ~20 encodings, each
  llvm-mc-checked and each carrying its AT&T source in the comment -- holo lays
  x64, and teaching it a mode nothing else in the tree needs would be the wrong
  shape. the PVH protocol is frozen, so it is write-once. it came out
  instruction-for-instruction identical to GAS's.

the differential, run on every piece while it was written: clang assembles the
old .S, we lay ours, compare. `.rodata` (the ISR and gate-type tables) and
`.boot` (the page tables and the GDT) came out BYTE-IDENTICAL, the 32-bit
prologue instruction-for-instruction, and the relocations matched addend for
addend including the KVMA fold. it caught a real bug -- a mistyped TCR_EL1
constant -- which is the argument for doing it that way rather than reading
twice.

what the rung turned up:

* **`lay` answered an empty stream for an unregistered backend.** a frontend
  bakes holo with the NATIVE backend only and a cross target joins the cat at
  runtime; without it the object still wrote out whole, with a 0-byte .text, and
  the failure surfaced only as a boot that went nowhere. `objsecs` checks the
  backend is registered now, not merely named.
* **an immediate slot took a SYMBOL and assembled it.** every other operand
  refuses a stranger through `rn` (badreg), but an immediate gets arithmetic
  done to it, and a symbol survives arithmetic: `(li r6 a-tbl)` inside a QUOTED
  IR block (where the name never evaluated) assembled quietly to the wrong
  constant. both `li` emitters check now -- `badimm`.
* ⚠ **aarch64's `lia` wants an `('align 8)` in front of it.** the literal sits
  at the instruction's own address + 8, so it is 8-byte aligned only when the
  site is, and an emitter cannot know its own address. a 4-mod-8 literal is an
  unaligned 8-byte load: fine on Normal memory while SCTLR.A is clear, an
  Alignment fault on Device memory whatever SCTLR says -- which is what the
  world looks like before the MMU comes on, where a boot stub most wants this.

the gate is `test_vec` (test/gate/vec.sh, in test_all). test_kernel already
runs most of the lay by booting on it -- nothing boots without archinit's IDT,
and the corpus is FED over the serial line and CLOCKED by the timer, so
uart_isr and timer_isr run thousands of times per gate. what a green boot never
touches is the part that runs when something goes wrong, so the gate makes
something go wrong: `(fault n)` raises a real CPU exception and the report is
read back. #PF is the load-bearing case -- an error-code vector that reports
`err` AND `cr2`, so a stub on the wrong side of the split shifts the frame and
misreports all three. the 27 stubs no boot reaches are checked against the
architecture's own error-code list. negative-tested three ways (a vector moved
across the split, the aarch64 IRQ slot moved, the table alignment dropped); the
first prints `rip=2`, the shifted frame itself.

### 5. the flip

* kernel.mk: the moon lane becomes the default -- every TU
  `mooncc -t <arch>` with kernel flags (the -m* soup drops; mooncc's
  codegen is already red-zone-free and abs64+PC32-only, see the probes
  below), objects + lays bound by rung 2's layout. `KCC=clang KLINK=lld`
  stays the opt-in comparison lane, exactly like CC on the host side.
* gates: test_kernel, test_kernel_arm64, test_uefi, the limine iso boot,
  and the clang-vs-mooncc K_TEST differential from rung 3.
* after this: `make test_all` runs no foreign compiler outside the DDC leg.

## probes to run FIRST (each is an hour, and two are load-bearing)

* **red zone**: confirm gen.l NEVER reads/writes below sp (x86 IRQ context
  clobbers the red zone -- this is correctness, not style). expected yes
  (the stack machine pre-decrements); pin it with a vmret-style scan or a
  law, then the -mno-red-zone flag is vacuously satisfied.
* **mcmodel=kernel**: confirm mooncc emits ONLY abs64 + pc-relative
  relocations -- then top-2GiB linking is safe with no code-model knob at
  all. (rung 2 settled the linker's half either way: clang's kernel objects
  carry 2761 32S sites and 12 wrapped _32 ones, and ldkern lays them.)
* **attribute lists**: `__attribute__((used, section(".limine_requests")))`
  (kmain.c:83) -- confirm the parser takes the comma list; `used` is a
  no-op for us (no gc-sections).
* **tables in .boot**: the aarch64 stub's 4K translation tables are .skip
  blocks -- the lay wants a zeros-array op in the boot lane (obj.l may
  have it already via bss-like sections; check before inventing).

## order and size

2 went first (it needed nothing from rung 1 and carried no compiler risk),
and it landed the value early: our linker is under the shipping kernel on
both arches and all three doors. 1 followed, pure ADDITION -- new ops, no
caller yet, nothing in the tree lowered differently. 3 then gave those ops
their first callers and, with them, the differential the flip will lean on.
4 retired the last assembler. what is left is 5 alone: makefile + gates. the
probes above belong before it.

nothing before 5 disturbs the clang COMPILER lanes, and the tree stayed green
the whole climb -- rung 3 changed 40 call sites and rung 4 replaced every
assembly file in the kernel, and the kernel gates have not moved once: 3562
tests, both arches, both linkers, all three doors.

so what clang still does, after four rungs, is COMPILE C. that is the whole
remaining island, and rung 5 is the one line of kernel.mk that steps off it.
