# port -- the freestanding targets of the love lisp

Every non-host target. Each device port is self-contained (own Makefile,
`R := ../..` back to the repo root); the qemu boot gates live in
test/test.mk and ride `make test_slow`.

## inle/

The freestanding kernel -- the bare-metal build of love, a tty and a repl on
raw hardware: `kmain.c` + `k.h` with per-arch backends `x86_64/` and
`aarch64/` (arch.c + boot `.S` + `.lds`), built by the root Makefile's
`kernel` target. Gates: test_kernel (qemu + OVMF), test_kernel_arm64.

## mps2/

The qemu Cortex-M reference machine (ARM MPS2 boards): love compiled WHOLE by
mooncc boots and bakes its egg on-device. Three faces: thumb2 on the M7
(test_mps2), thumb1 -- the ARMv6-M/RP2040 ISA (test_mps2_t1), and the
build-time image bake that a different binary wakes (test_mps2_wake). No
silicon; this is where the thumb backends verify.

## nucleo446/

ST Nucleo-F446RE (STM32F446RE, Cortex-M4F @ 180 MHz, 128 KB SRAM -- firmware
only, no love). All C through `mooncc -t thumb2sp`: cold boot, 180 MHz PLL
with bounded ready-waits falling back to HSI, USART2 console on the ST-LINK
VCP, and a 28-check on-silicon battery (soft doubles, 64-bit, am math,
composites). `make flash` via st-flash; gate test_nucleo446 boots the
semihosting face on qemu's netduinoplus2 (STM32F405 -- same UART/RCC map).

## playdate/

Panic Playdate: love WAKES on the device build -- all-mooncc `-t thumb2sp`
soft-double pdx (device + simulator, needs PLAYDATE_SDK_PATH and
arm-none-eabi-gcc for link/pack), booting a qemu-baked love-pd.img; cas.l is
the rune workbench face. Simulator-verified; sideloading is the human step.
See playdate/README.md.

## rp2040/

Raspberry Pi Pico (RP2040, Cortex-M0+ @ 125 MHz, 264 KB SRAM -- firmware
only, no love: the baked image is ~544 KB, so this is the toolchain on
silicon, like nucleo446). ARMv6-M is the leanest target mooncc has -- no FPU
and no divide instruction, so every `/`, `%`, float and double is a libcall --
and main.c is a 32-check battery over exactly those. The one port with **no
`.S` anywhere**: the vector table and crt0 are C (rp2040.c), and boot2 -- the
256-byte stage the mask ROM CRC-checks before running -- is laid by mkboot2.l
into a named section. Gate test_rp2040 verifies the boot image (boot2 CRC, SP,
thumb-bit reset entry); qemu has no RP2040 machine, so test_mps2_t1 is where
this ISA actually runs. ⚠ arm-none-eabi-ld still binds it (thumb relocations
are not in crew/holo/link.l), and mk/tools/ (elf2uf2, pad_checksum) is stale
pre-rename code -- pad_checksum's job now belongs to mkboot2.l, so only the
.uf2 packer is still wanted.

## teensy41/

Bare-metal Teensy 4.1 (NXP i.MX RT1062, Cortex-M7 @ 600 MHz, ~1 MB SRAM):
love LIVE ON SILICON -- all-mooncc `-t thumb2` build, XIP boot from the 8 MB
FlexSPI NOR behind the config block + IVT, LPUART6 console on pins 0/1 at
115200, image boot (a build-time qemu bake wakes in ~1 s where the on-device
egg bake took ~55 s). `make` / `make flash` (teensy_loader_cli). See
teensy41/README.md.
