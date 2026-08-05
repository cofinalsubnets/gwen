#!/bin/sh
# test/gate/boot.sh -- LOVE ITSELF booting on emulated silicon. Four gates, one
# procedure: build the port with its own make, run the ELF under qemu, and require an
# exact exit code. 42 means the egg hatched on-device and the driver laws held; 98 is a
# fault, 1 is a law that failed. These are the gates that prove the whole runtime --
# not a codegen lane -- survives on a board.
#
#   mps2       Cortex-M7,  all-mooncc thumb2         42
#   mps2_t1    Cortex-M0,  all-mooncc thumb1 (RP2040 ISA)  42
#   mps2_wake  the IMAGE lane: baked on qemu's M7, woken in a DIFFERENT binary  42
#   virt       riscv64 bare metal, our linker + holo start.o  42
#
# ⚠ qemu reads </dev/null: -nographic muxes guest serial + monitor onto stdio, so
# without a definite-EOF stdin qemu BLOCKS on the host chardev when this runs with no
# tty -- the guest exits via semihosting instantly but qemu-in-make hangs to the
# timeout. Host I/O, not the port.
#
# NOT set -e: every gate captures $? to report the code it got.
#
# usage: boot.sh GATE MAKE
set -u

gate=$1
mk=$2

arm='arm-none-eabi-gcc arm-none-eabi-ld qemu-system-arm'
case $gate in
  mps2)      banner="MPS2 out/mps2/love.elf"          ; need=$arm
             elf=out/mps2/love.elf                    ; tmo=300 ; want=42
             qemu="qemu-system-arm -M mps2-an500 -semihosting -nographic"
             why="love-on-M7 boot"
             done_msg="love (all-mooncc thumb2) boots on qemu Cortex-M7 -- egg baked on-device, laws hold, exit 42" ;;
  mps2_t1)   banner="MPS2T1 out/mps2/love-t1.elf"     ; need=$arm
             elf=out/mps2/love-t1.elf                 ; tmo=600 ; want=42
             qemu="qemu-system-arm -M mps2-an500 -semihosting -nographic"
             why="love-on-v6M boot"
             done_msg="love (all-mooncc thumb1/ARMv6-M, the RP2040 ISA) boots -- egg baked on-device, laws hold, exit 42" ;;
  mps2_wake) banner="MPS2WAKE out/mps2/waker.elf"     ; need=$arm
             elf=out/mps2/waker.elf                   ; tmo=300 ; want=42
             qemu="qemu-system-arm -M mps2-an500 -semihosting -nographic"
             why="image wake"
             done_msg="the qemu-baked image WAKES in a different binary -- laws hold, exit 42" ;;
  virt)      banner="VIRT out/virt/love.elf"          ; need=qemu-system-riscv64
             elf=out/virt/love.elf                    ; tmo=300 ; want=42
             qemu="qemu-system-riscv64 -M virt -bios none -nographic"
             why="love-on-virt boot"
             done_msg="love (all-mooncc riscv64, our linker, holo start.o) boots on qemu -M virt -- egg baked on-hart, laws hold, exit 42" ;;
  *) echo "boot.sh: unknown gate $gate" >&2; exit 1 ;;
esac

name=test_$gate
fail() { echo "FAIL $*" >&2; exit 1; }

echo "$banner"
for tool in $need; do
  command -v "$tool" > /dev/null 2>&1 || {
    case $gate in
      virt) echo "$name: no qemu-system-riscv64, skipped" ;;
      *)    echo "$name: no arm-none-eabi toolchain / qemu-system-arm, skipped" ;;
    esac
    exit 0; }
done

case $gate in
  mps2)      $mk -C port/mps2 || fail "mps2 build" ;;
  mps2_t1)   $mk -C port/mps2 mps2t1 || fail "mps2t1 build" ;;
  mps2_wake) $mk -C port/mps2 img ../../out/mps2/waker.elf || fail "mps2 waker build"
             # the image is baked ON qemu, so without qemu at bake time there is
             # nothing to wake -- a skip, not a failure
             test -s out/mps2/love.img || {
               echo "$name: empty image (no qemu at bake), skipped"; exit 0; } ;;
  virt)      $mk -C port/virt || fail "virt build" ;;
esac

# shellcheck disable=SC2086  # $qemu is a deliberate word list
timeout "$tmo" $qemu -kernel "$elf" < /dev/null
a=$?

case $gate in
  mps2_wake) [ "$a" -eq "$want" ] || fail "$why (got $a, want $want)" ;;
  *)         [ "$a" -eq "$want" ] \
               || fail "$why (got $a, want $want = the egg hatched + the driver laws held; 98 = fault, 1 = a law failed)" ;;
esac

echo "$name: $done_msg"
