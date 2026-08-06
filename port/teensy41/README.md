# teensy41/

Bare-metal Teensy 4.1 (NXP i.MX RT1062, Cortex-M7) port of love -- no
Teensyduino core, no Arduino. A freestanding thumbv7em hard-float build of the
love runtime booting XIP from the 8 MB FlexSPI NOR flash, with a REPL on a
hardware serial console. The closest analogue in the tree is `rp2040/`, but the
RT1062 is a far roomier part: a 600 MHz M7 with a double-precision FPU and ~1 MB
of on-chip SRAM, so unlike the RP2040 (dropped upstream for "not enough RAM")
the self-hosting double-bake fits comfortably.

## Build

```
cd port/teensy41 && make            # -> $R/out/teensy41/love.hex
make flash                           # teensy_loader_cli --mcu=TEENSY41 -w -v ...
```

Compiled END TO END by **mooncc** (`out/host/mooncc -t thumb2`, built by the
root Makefile): love.c, the am math floor, libc, and the port's own C all go
through the repo's compiler, and `mkboot.l` lays the ROM-facing FlexSPI/IVT/
vector blocks + crt0 + the barrier helpers from holo IR -- there is no `.S`
here and nothing assembles them. `arm-none-eabi-ld` still links against
`teensy41.lds`. No libgcc -- mooncc's thumb2 lanes are self-contained. The
build also verifies the boot image (FCFB tag at 0, IVT at 0x1000, a thumb-bit
entry); the qemu cousin `port/mps2/` proves the same runtime end to end under
emulation (`make test_mps2`). Needs `arm-none-eabi-ld` and `llvm-objcopy` or
`arm-none-eabi-objcopy`.

## Console

LPUART6 on **pin 0 (RX1) / pin 1 (TX1)** at 115200 8N1, over a 3.3 V USB-serial
adapter -- the analogue of the rp2040 port's UART0 console. The REPL line editor
in `love/repl.l` drives it exactly as it drives the kernel's. USB CDC (so the
Teensy's own USB shows up as the console, no adapter) is the obvious next step
and a **TODO** -- bare-metal device USB on the RT1062 is a substantial chunk.

## Memory & boot

The i.MX RT1062 boot ROM reads the **FlexSPI Configuration Block** at flash base
(`0x60000000`), the **Image Vector Table** at `0x60001000`, and the **Boot
Data** that follows, then jumps to our startup. `teensy41.c` supplies all three
plus the crt0 (FPU enable, `.data`/`.bss`, VTOR, clocks, console). We run all
RAM -- `.data`, `.bss`, the love heap pool, and the C stack -- out of the
dedicated 512 KB **OCRAM2** at `0x20200000`, which is mapped at reset
independent of the FlexRAM split, so the scaffold needs no FlexRAM
reconfiguration to have working memory.

## Status: boots + serial console verified on hardware (2026-07-12)

First silicon ride: the board **boots XIP from flash and runs to `main`** (the
FlexSPI config block / IVT / boot-data / crt0 / clocks path is proven), and the
**LPUART6 console works both directions** at 115200 over an RPi Debug Probe UART
bridge. Two register bugs surfaced and were fixed against PJRC's `cores/teensy4`
as the authority (treat every offset here as verify-first -- these are lifted
from the i.MX RT1060 RM):

- **TX pad drive** -- `serial_init` set the pin *mux* but never the pad *control*
  (drive strength). The mux routes the LPUART TX signal to the pad, but a pad at
  its reset-default driver never drove the line: TDRE asserted and bytes clocked
  out with the wire idle. Fixed by writing `SW_PAD_CTL` = `0x10B0` for both pads.
- **RX daisy** -- `IOMUXC_LPUART6_RX_SELECT_INPUT` was at `+0x520`; the real
  register is the SECOND IOMUXC block (`base+0x400`) offset `0x150`, i.e.
  `+0x550`. With the wrong offset the receiver's pad-select stayed at reset
  default, so RX heard nothing while TX (which drives its pad directly) worked.

**Still open -- the egg double-bake does not complete on hardware.** The board
boots and prints the `baking the egg` banner, but the self-hosting double-bake
never reaches the new `; egg hatched -- shell up` marker (>14 min, solid LED, no
scare face). Prime suspects, in order: the ROM-default core clock (see the clock
TODO below), the tight GC budget (`main.c` sets `budget = pool/4` over the 384 KB
OCRAM2 arena -- a live set past the pool likely faults inside the collector,
matching the solid-LED silence), or egg size. Getting to a live REPL means
attacking one of these next. Remaining verify-first surface + TODOs:

- **FlexSPI config block / read LUT** -- verify the exact bytes against the
  on-board QSPI part (Teensy 4.1 ships a Winbond/ISSI 8 MB NOR) before trusting
  a first flash.
- **Clock tree** -- `clocks_init` only brings up the LPUART and GPT1 roots off
  the 24 MHz crystal and leaves the M7 on the ROM's clock; raising the core to
  600 MHz via ARM_PLL is a TODO (it changes neither the console nor the
  GPT1-microsecond timebase math).
- **ITCM/DTCM** -- left empty; moving hot code to ITCM and the stack to DTCM
  (via the FlexRAM `GPR16/GPR17` bank split) is a performance optimisation, not
  a correctness need.
- **GPIO** -- `gpio_*` covers the GPIO2 bank and muxes pin 13 (the LED). A full
  Teensy pin map (all 55 pads -> bank/bit -> ALT5 mux) is a TODO.
- **USB CDC console** -- see above.

See `tools/README.md` for the flashing path.
