#!/bin/sh
# test/gate/vec.sh -- the INTERRUPT gate (doc/moon-kernel.md rung 4).
#
# port/inle/mkvec.l lays the exception and IRQ entry points that used to be
# x86_64/x86_64.S and aarch64/aarch64.S. a green `make test_kernel` already
# proves most of that lay by running it: nothing boots without archinit's IDT,
# and the corpus is FED over the serial line and CLOCKED by the timer, so
# uart_isr and timer_isr run thousands of times per gate. what a green boot
# never touches is the part that only runs when something goes wrong --
# the 32 exception stubs, the common tail, and aarch64's fault vector.
#
# so this gate makes something go wrong, on purpose, and reads the report:
#
#   1. THE FAULT PATH, BOOTED. `(fault n)` (kmain.c's nif -> k_fault_trigger in
#      each arch.c) raises a real CPU exception. the kernel dispatches it
#      through the vector table we laid, and prints what it caught. the vectors
#      below are chosen to cover BOTH stub shapes: the CPU pushes an error code
#      for 8, 10..14, 17, 21, 29 and 30 and for nothing else, so those stubs
#      push one word and the other twenty-two push a dummy zero first. #PF is
#      the load-bearing case -- it is an error-code vector AND it reports both
#      `err` and `cr2`, so a stub on the wrong side of that split would shift
#      the whole frame and misreport all three fields.
#   2. THE STUBS THE FAULT PATH CANNOT REACH. five vectors are booted above;
#      the other twenty-seven come off the same loop, and a loop is exactly the
#      thing that goes wrong at one end. so the laid object is read back and
#      the error-code split is checked stub by stub against the architecture's
#      own list -- and on aarch64, that the vector table is 2 KiB-aligned with
#      one entry every 0x80 and exactly one of the sixteen (index 5, current EL
#      with SP_ELx, IRQ) going somewhere different from the rest.
#
# a faulted kernel HALTS by design (k_exception does not return), so each boot
# is stopped as soon as its report lands rather than waited out.
#
# usage: vec.sh ARCH ELF OBJ
set -u
arch=$1; elf=$2; obj=$3
work=${TMPDIR:-/tmp}/vec.$$
mkdir -p "$work"
trap 'rm -rf "$work"' EXIT
# a `while read` on the end of a pipe is its own process, so a failure raised
# in one cannot set a variable here -- it leaves a mark on the filesystem instead.
fail() { echo "FAIL test_vec($arch): $*" >&2; : > "$work/failed"; }
have() { command -v "$1" >/dev/null 2>&1; }

case $arch in
  x86_64)  qemu=qemu-system-x86_64;  mach="-M q35" ;;
  aarch64) qemu=qemu-system-aarch64; mach="-M virt,gic-version=2 -cpu cortex-a72" ;;
  *) echo "FAIL test_vec: unknown arch $arch" >&2; exit 1 ;;
esac

# KVM where the host can back it; without /dev/kvm this falls to TCG and the
# faults report the same. x86_64-on-x86_64 only -- `virt` is asked for
# gic-version=2 above, which an arm host with no v2 backing refuses outright.
if [ "$arch" = x86_64 ] && [ -e /dev/kvm ] && [ "$(uname -m)" = x86_64 ]; then
  mach="$mach -enable-kvm -cpu host"
fi

if ! have "$qemu"; then
  echo "  (vec $arch: fault boots skipped, no $qemu)"
else
  # ⚠ only a NEWLINE-TERMINATED report counts: rip=/err=/cr2= land after the marker,
  # so waking on the marker alone kills qemu mid-line and the gate reads a PREFIX of
  # the address it asked for -- a failure that only shows under load. wc -l counts
  # terminators, so head -n that many is exactly the complete lines.
  said() {
    n=$(tr -d '\0' < "$work/out" | wc -l | tr -d ' ')
    [ "${n:-0}" -gt 0 ] && tr -d '\0' < "$work/out" | head -n "$n" | grep -qa '\*\*\* CPU exception'
  }
  # boot, feed one expression, stop as soon as the report lands whole. the kernel is
  # halted at that point and would otherwise sit until a timeout.
  fault_report() {
    printf '(fault %s)\n' "$1" > "$work/in"
    : > "$work/out"
    # shellcheck disable=SC2086
    $qemu $mach -m 512M -serial stdio -display none -no-reboot \
          -kernel "$elf" < "$work/in" > "$work/out" 2>/dev/null &
    qp=$!
    i=0
    while [ $i -lt 600 ]; do
      said && break
      kill -0 $qp 2>/dev/null || break
      sleep 0.1; i=$((i + 1))
    done
    kill $qp 2>/dev/null
    wait $qp 2>/dev/null
    n=$(tr -d '\0' < "$work/out" | wc -l | tr -d ' ')
    tr -d '\0' < "$work/out" | head -n "${n:-0}" | grep -a '\*\*\* CPU exception' | head -1
  }

  # vector -> every string its report must contain. the x86 set covers a
  # no-error-code stub (0, 3, 6), an error-code stub whose code happens to be
  # zero (13), and the one that reports all three fields (14).
  case $arch in
    x86_64) cases='0:exception 0 (#DE)
3:exception 3 (#BP)
6:exception 6 (#UD)
13:exception 13 (#GP)
14:exception 14 (#PF)|err=2|cr2=600000000000' ;;
    # aarch64 has no error-code split: every fault reaches the sync vector and
    # a_fault reads ESR/ELR/FAR itself. esr 0x2000000 is EC=0 (unknown
    # instruction, the udf), 0x96000044 EC=0x25 (data abort at the current EL).
    aarch64) cases='6:esr=2000000
14:data abort|far=600000000000' ;;
  esac

  echo "$cases" | while IFS=: read -r vec want; do
    [ -n "$vec" ] || continue
    got=$(fault_report "$vec")
    [ -n "$got" ] || { fail "(fault $vec) produced no exception report"; continue; }
    echo "$want" | tr '|' '\n' | while read -r w; do
      case $got in
        *"$w"*) ;;
        *) fail "(fault $vec): report is missing \"$w\""; echo "    got: $got" >&2 ;;
      esac
    done
  done
fi

# -- the static half: the stubs no boot reaches ------------------------------
if ! have llvm-objdump; then
  echo "  (vec $arch: object checks skipped, no llvm-objdump)"
elif [ ! -f "$obj" ]; then
  fail "$obj was never laid"
else
  case $arch in
    x86_64)
      # the ARCHITECTURE's list, not a restatement of mkvec.l: these are the
      # vectors for which the CPU itself pushes an error code (Intel SDM
      # vol.3 6.3.1 -- #DF #TS #NP #SS #GP #PF #AC #CP #VC #SX).
      errset=" 8 10 11 12 13 14 17 21 29 30 "
      d=$(llvm-objdump -d --no-show-raw-insn "$obj")
      v=0
      while [ $v -lt 32 ]; do
        body=$(echo "$d" | awk -v s="<exc_stub_$v>:" '
                 $0 ~ s {on = 1; next} /^[0-9a-f]+ </ {on = 0} on && NF')
        [ -n "$body" ] || { fail "exc_stub_$v is not in $obj"; v=$((v + 1)); continue; }
        n=$(echo "$body" | grep -c 'push')
        case $errset in
          *" $v "*) wantn=1; kind="takes the CPU's error code" ;;
          *)        wantn=2; kind="must push a dummy error code" ;;
        esac
        [ "$n" = "$wantn" ] || fail "exc_stub_$v $kind, so it should push $wantn word(s); it pushes $n"
        # the last stub is followed by the align's nop filler, which the symbol
        # span picks up -- it is padding, never executed, so drop it first.
        echo "$body" | grep -v 'nop' | tail -1 | grep -q 'jmp.*exc_common' ||
          fail "exc_stub_$v does not end in a jump to exc_common"
        v=$((v + 1))
      done
      ;;
    aarch64)
      # VBAR_EL1 ignores the low 11 bits, so a table off its 2 KiB boundary
      # would dispatch into the middle of another entry. sh_addralign is the
      # last column of the section header, and it is the linker's instruction.
      # the index column is "[ 1]" or "[10]", so the name's field NUMBER moves --
      # find the field that IS the name (which also skips .rela.text.vectors).
      al=$(llvm-readelf -S "$obj" |
           awk '{ for (i = 1; i <= NF; i++) if ($i == ".text.vectors") { print $NF; exit } }')
      [ "$al" = 2048 ] ||
        fail ".text.vectors must ask the linker for 2048-byte alignment; it asks for ${al:-nothing}"
      # one entry every 0x80, sixteen of them, and exactly one different.
      br=$(llvm-objdump -d --no-show-raw-insn "$obj" |
           awk '/^ +[0-9a-f]+:/ { off = strtonum("0x" substr($1, 1, length($1) - 1))
                                  if (off < 2048 && off % 128 == 0) print off, $2, $3 }')
      n=$(echo "$br" | grep -c .)
      [ "$n" = 16 ] || fail "the vector table should hold 16 entries at 0x80 spacing; found $n"
      echo "$br" | awk '$2 != "b" { print "slot at " $1 " is not a branch" }' | while read -r m; do fail "$m"; done
      odd=$(echo "$br" | awk '{c[$3]++; t[$3] = t[$3] " " NR} END { for (k in c) if (c[k] == 1) print t[k] }')
      [ "$(echo "$odd" | tr -d ' ')" = 6 ] ||
        fail "exactly one of the sixteen vectors (slot 5, current EL SP_ELx IRQ) must differ; the odd one out is entry$odd"
      ;;
  esac
fi

[ -f "$work/failed" ] && exit 1
echo "test_vec($arch): the fault path reports what it caught, and every stub is on the right side of the error-code split"
exit 0
