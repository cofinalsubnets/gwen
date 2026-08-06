#!/bin/sh
# test/gate/thumb.sh -- the 32-bit ARM codegen gates, on the DEVICE CPUs under qemu.
# One procedure, three targets, because they only ever differed in four things: the
# -t flag, the arm-none-eabi CPU/FPU flags, the qemu machine, and which lanes run.
#
#   thumb1    Cortex-M0  (v6-M, no FPU)      qemu -M microbit    -- the RP2040 ISA
#   thumb2    Cortex-M7  (v7E-M, fpv5-d16)   qemu -M mps2-an500  -- Teensy 4.1 / Playdate
#   thumb2sp  Cortex-M4  (v7E-M, fpv4-sp)    qemu -M mps2-an386  -- soft f64 over __aeabi
#
# EVERY lane is the same differential: mooncc compiles the library, arm-none-eabi-gcc
# compiles a harness that calls it and computes the same answers ITS way, ld binds them
# against a gas startup, and the semihosted exit code must be exactly what the harness
# says. gcc is the reference ABI; a disagreement is ours. Where a harness returns 100+n,
# n names the first check that missed.
#
# ⚠ qemu reads </dev/null: -nographic muxes guest serial + monitor onto stdio, so with
# no definite-EOF stdin qemu BLOCKS on the host chardev when this runs without a tty --
# the guest exits via semihosting instantly, but qemu-in-make hangs to the timeout.
# Host I/O, not codegen. Every qemu line here keeps the redirect.
#
# NOT set -e: every lane captures $? to report the exit code it got.
#
# usage: thumb.sh TARGET OUTDIR
set -u

tgt=$1
ho=$2

case $tgt in
  thumb1)   banner=THUMB1   ; cpu="-mcpu=cortex-m0 -mthumb"
            acpu="cortex-m0"; afpu=""            ; mach=microbit   ; prefix=t1 ; vfp=0 ;;
  thumb2)   banner=THUMB2   ; cpu="-mcpu=cortex-m7 -mthumb -mfloat-abi=hard -mfpu=fpv5-d16"
            acpu="cortex-m7"; afpu="fpv5-d16"    ; mach=mps2-an500 ; prefix=t2 ; vfp=1 ;;
  thumb2sp) banner=THUMB2SP ; cpu="-mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16"
            acpu="cortex-m4"; afpu="fpv4-sp-d16" ; mach=mps2-an386 ; prefix=ts ; vfp=1 ;;
  *) echo "thumb.sh: unknown target $tgt" >&2; exit 1 ;;
esac

name=test_$tgt
fail() { echo "FAIL $*" >&2; exit 1; }

for tool in arm-none-eabi-gcc arm-none-eabi-ld qemu-system-arm; do
  command -v $tool > /dev/null 2>&1 || {
    echo "$name: no arm-none-eabi toolchain / qemu-system-arm, skipped"; exit 0; }
done

echo "$banner $ho/$tgt"
d=$ho/$tgt
mkdir -p "$d"
rm -f "$d"/*.o

# the startup: vector table, then bl run, then semihosting SYS_EXIT_EXTENDED carrying
# run()'s answer out as the process exit code. A VFP target must enable CP10/CP11 in
# CPACR first or the first float instruction UsageFaults.
{ echo '.syntax unified'
  echo ".cpu $acpu"
  [ -n "$afpu" ] && echo ".fpu $afpu"
  echo '.thumb'
  echo '.section .vectors,"a"'; echo '.word 0x20004000'; echo '.word _start+1'
  echo '.text'; echo '.thumb_func'; echo '.global _start'; echo '_start:'
  if [ "$vfp" = 1 ]; then
    echo '  ldr r0, =0xE000ED88'; echo '  ldr r1, [r0]'; echo '  orr r1, r1, #0xF00000'
    echo '  str r1, [r0]'; echo '  dsb'; echo '  isb'
  fi
  echo '  bl run'; echo '  ldr r1, =0x20026'; echo '  push {r0}'; echo '  push {r1}'
  echo '  mov r1, sp'; echo '  movs r0, #0x20'; echo '  bkpt 0xAB'; echo '  b .'
} > "$d/start.S"

{ echo 'MEMORY'; echo '{'
  echo '  FLASH (rx) : ORIGIN = 0, LENGTH = 256K'
  echo '  RAM  (rwx) : ORIGIN = 0x20000000, LENGTH = 16K'; echo '}'
  echo 'SECTIONS'; echo '{'
  echo '  .text : { KEEP(*(.vectors)) *(.text*) *(.rodata*) } > FLASH'
  echo '  .data : { *(.data*) } > RAM'
  echo '  .bss : { *(.bss*) } > RAM'; echo '}'
} > "$d/link.ld"

# shellcheck disable=SC2086  # $cpu is a deliberate word list
arm-none-eabi-gcc $cpu -c "$d/start.S" -o "$d/start.o" || fail "as start.S"
lg=$(arm-none-eabi-gcc $cpu -print-libgcc-file-name)

# one differential lane. libgcc rides every link: it is an archive, so ld takes only
# the members actually referenced (the __aeabi soft-float and 64-bit helpers).
lane() { # lane TAG LIBSRC HARNESSSRC MOONFLAGS WANT TIMEOUT MSG TAIL
  tag=$1; libsrc=$2; harsrc=$3; mflags=$4; want=$5; tmo=$6; msg=$7; tail=$8
  lb=$(basename "$libsrc" .c); hb=$(basename "$harsrc" .c)
  "$ho/mooncc" -t "$tgt" $mflags -c "$libsrc" "$d/$tag.lib.o" \
    || fail "mooncc -t $tgt -c $lb"
  arm-none-eabi-gcc $cpu -ffreestanding -O2 -c "$harsrc" -o "$d/$tag.har.o" \
    || fail "gcc $hb"
  arm-none-eabi-ld -T "$d/link.ld" "$d/start.o" "$d/$tag.har.o" "$d/$tag.lib.o" "$lg" \
    -o "$d/$prefix$tag.elf" || fail "ld $tag objects"
  timeout "$tmo" qemu-system-arm -M $mach -semihosting -nographic \
    -kernel "$d/$prefix$tag.elf" < /dev/null
  a=$?
  # ⚠ a WANT must stay clear of 124 (timeout's own code) and of 128+n, where a
  # guest that died by signal lands -- a crashing qemu aborts to 134 and a want
  # of 134 reads as every check passing. Found the hard way: a deliberately
  # thumb-bitless vector entry HardFaulted, qemu dumped core, and this gate said
  # green. The wants below are all under 124 for that reason; say which death it
  # was when one misses, since "got 134" alone never looked like a crash.
  if [ "$a" -ne "$want" ]; then
    [ "$a" -eq 124 ] && fail "$msg (TIMED OUT after ${tmo}s, want $want$tail)"
    [ "$a" -ge 128 ] && fail "$msg (guest DIED by signal $((a - 128)), want $want$tail)"
    fail "$msg (got $a, want $want$tail)"
  fi
}

am=crew/moon/lib/math/am.c
aminc="-Icrew/moon/lib/math -Icrew/moon/include"

# ---- the smoke lane: written here because it IS the target's own feature list ----
if [ "$tgt" = thumb1 ]; then
  # a cross-object BL, the inline v6-M soft divide/rem, a scalar global through the
  # literal-pool `la`, and -- the cross-ABI catch -- a pointer-bearing struct BUILT by
  # gcc (4-byte pointer, x at offset 4) whose field a mooncc function reads back.
  # ..and a NAMED SECTION carrying a table of function pointers -- the vector-table
  # shape every one of these parts boots through, and the only shape where the
  # object writer's own bookkeeping is load-bearing: each entry is an ABS32 into
  # a section that is neither .text nor .data, and each target is a STATIC fn, so
  # it can only bind through a local symbol carrying the THUMB BIT. Lose the bit
  # and the call HardFaults on M0 (there is no ARM state to fall back to); bind it
  # to the section instead and the addend truncates. start.S already KEEPs
  # .vectors at flash base, so ours lands right behind the boot pair.
  { printf 'int acc = 40;\n'
    printf 'int arr[4];\n'
    printf 'struct S { int *p; int x; };\n'
    printf 'int addto(int x){ acc = acc + x; return acc; }\n'
    printf 'int divmod(int a,int b){ return a/b + a%%b; }\n'
    printf 'int sx(struct S *s){ return s->x; }\n'
    printf 'int aset(int i,int v){ arr[i] = v; return 0; }\n'
    printf 'int aget(int i){ return arr[i]; }\n'
    printf 'static int v1(void){ return 3; }\n'
    printf 'static int v2(void){ return 4; }\n'
    printf '__attribute__((section(".vectors"))) int (*const vt[2])(void) = { v1, v2 };\n'
    printf 'int viacall(int i){ return vt[i](); }\n'
  } > "$d/lib.c"
  { echo 'struct S { int *p; int x; };'
    echo 'int addto(int); int divmod(int,int); int sx(struct S*); int aset(int,int); int aget(int);'
    echo 'int viacall(int);'
    echo 'int run(void){ int t = 0; struct S s; s.p = &t; s.x = 16;'
    echo '  int dd = divmod(-17,5); addto(50); aset(3, 12);'
    echo '  return addto(dd) + sx(&s) + aget(3) + viacall(0) + viacall(1); }'
  } > "$d/harness.c"
  lane smoke "$d/lib.c" "$d/harness.c" "" 120 30 "thumb1 -c link+run" \
    " = addto(40+50) then addto(-17/5 + -17%5)=85 + s->x=16 + arr[3]=12 + the .vectors table 3+4; a wrong struct offset misreads s->x, a wrong leax scale/base misreads arr[3], a thumb-bitless vector entry HardFaults"
fi

if [ "$tgt" = thumb2 ]; then
  # thumb2's MOVW/MOVT absolute pair, every binding shape once: a global fn's address,
  # a STATIC fn's (section symbol + thumb bit in the addend), a string literal, a global.
  { printf 'int acc = 40;\n'
    printf 'int f1(void){ return 30; }\n'
    printf 'int f2(void){ return 12; }\n'
    printf 'static int sf(void){ return 5; }\n'
    printf 'int callidx(int i){ int (*a[2])(void) = {f1,f2}; return i ? a[1]() : a[0](); }\n'
    printf 'int callsf(void){ int (*p)(void) = sf; return p(); }\n'
    printf 'char *msg(void){ return "A!"; }\n'
    printf 'int addacc(int x){ acc = acc + x; return acc; }\n'
  } > "$d/lib.c"
  { echo 'int callidx(int); int callsf(void); char *msg(void); int addacc(int);'
    echo 'int run(void){ return callidx(0) + callidx(1) + msg()[1] + addacc(3) + callsf(); }'
  } > "$d/harness.c"
  lane smoke "$d/lib.c" "$d/harness.c" "" 123 30 "thumb2 -c link+run" \
    " = fnptr 30+12 + '!' 33 + addacc 43 + static-fn 5; a missing thumb bit on the static fn faults the BLX, a bad section addend misreads the string"
fi

# ---------------------------------- the shared differential lanes ----------------
case $tgt in
thumb1)
  lane v  test/thumb1/libv.c  test/thumb1/harnessv.c  "" 7  30 "thumb1 varargs" \
    " = every differential check vs gcc; 100+n names the first miss -- see test/thumb1/harnessv.c; the pop-r3/bx epilogue or the r0-r3 push block over lr/fp/r4 is the usual suspect"
  lane p  test/thumb2/lib64.c test/thumb2/harness64.c "" 45 30 "thumb1 64-bit pairs" \
    " = every differential check vs gcc; 100+n names the first miss -- see test/thumb2/harness64.c; the v6-M lanes ride ADCS/SBCS inline + __aeabi_lmul/(u)ldivmod/shift libcalls"
  lane d  test/thumb2/libd.c  test/thumb2/harnessd.c  "" 45 30 "thumb1 soft doubles" \
    " = every differential check vs gcc's base-ABI soft float; 100+n names the first miss -- see test/thumb2/harnessd.c; doubles ride gp pairs at every seam, f0/f1/f15 are frame cells inside a fn (soften6)"
  lane am "$am" test/thumb2/harnessam.c "$aminc" 9 60 "thumb1 am.c" \
    " = the seven transcendentals BIT-IDENTICAL to the host am floor through the shared __aeabi soft float, incl. the Payne-Hanek big-argument reduction"
  lane f  test/thumb1/libf.c  test/thumb1/harnessf.c  "" 7  30 "thumb1 bare floats" \
    " = every differential check vs gcc; 100+n names the first miss -- see test/thumb1/harnessf.c; a bare float is ONE WORD on v6-M (ai_flo_t IS float on a 32-bit love -- the widened-pair mismatch here kept the egg from hatching)"
  lane z  test/thumb1/libzn.c test/thumb1/harnesszn.c "" 9  30 "thumb1 composites" \
    " = every differential check vs gcc; 100+n names the first miss -- see test/thumb1/harnesszn.c; the MEMORY-return (sret) lane and the position-0 16B r0-r3 quad are the featured shapes"
  # the other direction: read one of the objects just written back through the front
  # half of OUR linker (link.l's ld-read). every 64-bit link the tree does proves that
  # reader's ELF64/RELA rows; this is the only thing that proves its ELF32/REL ones.
  # am.lib.o because it carries no compiler-NAMED section -- ld-lane has no lane for
  # one until the 32-bit link itself lands, and refuses rather than guessing.
  { echo "(use 'holo)"
    cat crew/holo/thumb1.l crew/kore/text.l crew/kore/core.l crew/kore/asbook.l \
        crew/holo/elf.l crew/holo/obj.l crew/holo/link.l test/gate/ld32.l
    echo "(ld32-check \"$d/am.lib.o\")"; } | "$ho/love" || fail "ld-read of $d/am.lib.o"
  echo "test_thumb1: mooncc -t thumb1 -c -> ELF32/EM_ARM (R_ARM_THM_CALL + soft divide + la/R_ARM_ABS32 + 32-bit struct layout + leax + AAPCS32 varargs + 64-bit pairs + soft doubles + am.c bit-exact + composites vs gcc), ld binds, runs on qemu Cortex-M0; holo's own ld-read reads the object back" ;;
thumb2)
  lane p  test/thumb2/lib64.c test/thumb2/harness64.c "" 45 30 "thumb2 64-bit pairs" \
    " = every differential check vs gcc; 100+n names the first miss -- see test/thumb2/harness64.c"
  lane d  test/thumb2/libd.c  test/thumb2/harnessd.c  "" 45 30 "thumb2 VFP doubles" \
    " = every differential check vs gcc -mfloat-abi=hard; 100+n names the first miss -- see test/thumb2/harnessd.c"
  lane am "$am" test/thumb2/harnessam.c "$aminc" 9 60 "thumb2 am.c" \
    " = the seven transcendentals BIT-IDENTICAL to the host am floor, incl. the Payne-Hanek big-argument reduction"
  lane z  test/thumb2/libz.c  test/thumb2/harnessz.c "-Icrew/moon/include" 18 30 "thumb2 composites+varargs" \
    " = HFA d-pairs + 8B blob + <=4B int one + the AAPCS32 word walk, gcc<->mooncc both directions; 100+n names the first miss -- see test/thumb2/harnessz.c"
  echo "test_thumb2: mooncc -t thumb2 -c -> ELF32/EM_ARM (la + pairs + VFP + am.c bit-exact + composites/varargs: 45+45+9+18 differential checks), ld binds, runs on qemu Cortex-M7" ;;
thumb2sp)
  lane d  test/thumb2/libd.c  test/thumb2/harnessd.c  "" 45 30 "thumb2sp doubles" \
    "; 100+n names the first miss -- soft f64 vs gcc's __aeabi"
  lane am "$am" test/thumb2/harnessam.c "$aminc" 9 60 "thumb2sp am.c" \
    " = BIT-identical through the shared __aeabi helpers"
  lane z  test/thumb2/libz.c  test/thumb2/harnessz.c "-Icrew/moon/include" 18 30 "thumb2sp composites+varargs" \
    ""
  echo "test_thumb2sp: mooncc -t thumb2sp (soft f64 over __aeabi) -> 45+9+18 differential checks vs gcc on qemu Cortex-M4" ;;
esac
