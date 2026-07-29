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
* **inline asm**, ~35 sites, GNU spelling: x86 cli/hlt/in/out/mov-cr/int3/
  ud2 + a cpuid-ish sequence; arm64 mrs/msr over ~10 sysregs, msr-immediate
  (daifset/daifclr/spsel), dsb/isb, tlbi, ic/dc, `at s1e1w`, brk/hvc/wfi,
  and register-asm locals (`asm("x0")`). mooncc's inline asm is the NEUTRAL
  template (doc/moon.md's inline-asm section) -- an AT&T/ARM front-end was
  deliberately deferred, and this plan keeps it deferred: the kernel is OURS,
  so the sites move to the neutral surface instead (rung 3).
* **assembly files**, four: <a>/boot.S (the PVH stub -- ~50 lines of .code32
  before long mode -- and the EL1 MMU stub) and <a>/<a>.S (the exception/IRQ
  vector stubs, GAS .macro loops, iretq/eret, context plumbing).
* ~~**the link**~~ -- OURS since rung 2 below; ld.lld and <a>/<a>.lds are the
  `KLINK=lld` comparison lane. what they said: five named PT_LOADs (boot |
  limine_requests | text | rodata | data), every section vaddr HIGH with an
  `AT()` LMA bias (vaddr - KVMA + phys base -- what lets `qemu -kernel` load
  low by p_paddr while limine maps high by p_vaddr), ENTRY by symbol,
  4K max-page-size, kimage_end, DISCARD .note/.eh_frame, --gc-sections.

## the rungs (each green and useful on its own)

### 1. holo grows the SYSTEM vocabulary

the privileged instructions, as ordinary holo IR ops with laws:

* arm64.l: `mrs`/`msr` (sysreg by name -> its 16-bit encoding; the table is
  the ~dozen the kernel touches, growable), msr-immediate (daifset/daifclr/
  spsel), `dsb`/`isb` (+ domains), `tlbi`, `ic`/`dc`, `at`, `brk`, `hvc`,
  `wfi`/`wfe`, `eret`.
* x64.l: `cli`/`sti`/`hlt`, `in`/`out` (b/w/l forms), `int3`, `ud2`,
  `iretq`, `lgdt`/`lidt`/`ltr`, mov to/from crN, `rdmsr`/`wrmsr`,
  `invlpg`, `swapgs` if the vectors want it.
* law files keep the encodings honest the way holotest already does:
  byte-differential against clang's assembly of the same instruction.

this rung feeds BOTH consumers: the neutral inline-asm templates (rung 3)
and the .S lays (rung 4). nothing downstream starts until its ops exist here.

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

### 3. the inline-asm seam: one header, two spellings

* per-arch `port/inle/<a>/asmops.h`: every asm site becomes a static inline
  (`mrs_ttbr1()`, `outb()`, ...) -- most already are. the HEADER carries
  both spellings behind the mooncc predefine: neutral template for mooncc,
  today's GNU string for clang. the ~35 call sites go spelling-free.
* this is what makes gwen's carve-out STRUCTURAL: the clang kernel stays
  buildable forever as the benchmark / differential twin -- `K_TEST` runs
  on both builds and must agree (arm64check.sh's shape, compiler-vs-compiler
  instead of arch-vs-arch).
* register-asm locals (`asm("x0")` in the psci/semihost calls) become the
  template's `"rN"` pin constraints (mooncc has them; GNU keeps the
  register-asm spelling in the clang half of the header).
* moon.l honors `-nostdinc` (incload drops the /usr/include fallback --
  today a header missing from crew/moon/include would silently pull glibc's).

### 4. the .S files become LAYS (the mksys precedent)

the four .S files are exactly mksys.l's class -- register-exact leaves C
cannot say -- and they port to holo IR lays beside it:

* `mkvec.l`: the exception/vector stubs. the GAS `.macro exc_noerr/exc_err`
  loops become love loops generating the 32 stubs -- strictly nicer than
  assembler macros. iretq/eret/swapgs come from rung 1.
* `mkboot.l`: aarch64's EL1 MMU stub is all A64 -- rung 1 ops + what the
  backend has. x86_64's PVH stub opens with ~50 lines of .code32; holo lays
  x64 only, so the 32-bit prologue takes a SMALL x86-32 helper table inside
  the lay (~15 encodings: mov/or/and r32-imm/r32-r32, lgdt, mov-crN,
  rdmsr/wrmsr, ljmp, the fill loop) -- prefer that over frozen raw bytes;
  fall back to `('raw ..)` words WITH the source as comments only if the
  table fights. the PVH protocol is frozen, so this code is write-once.
* differential: K_TEST behavior on both doors; byte-compare stub-for-stub
  against clang's .o where label layout allows.

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
both arches and all three doors. what is left is 1 -> 3 -> 4 -> 5, with
the probes before rung 1. rung 1 is holo work (~a few hundred lines +
laws), 3 is a mechanical sweep with one new header per arch, 4 is two lay
files (~mksys.l x 2-3 in size), 5 is makefile + gates. nothing before 5
disturbs the clang COMPILER lanes, so the tree stays green the whole climb.
