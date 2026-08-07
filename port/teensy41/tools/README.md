# teensy41/tools

Unlike `rp2040/tools` (which needs custom love programs -- `pad_checksum.l` to
stamp the boot2 CRC and `elf2uf2.l` to pack the .uf2), the Teensy 4.1 flashing
path needs no bespoke tooling: the i.MX RT1062 boots a plain Intel HEX, and the
PJRC bootloader chip on the board accepts it directly.

## Producing the image

The `Makefile` runs `ocopy.l -O ihex` -- `crew/holo/copy.l`, our own objcopy --
on the linked ELF to make `$R/out/teensy41/love.hex`. The flashable layout
(FlexSPI config block at `0x60000000`, IVT at `0x60001000`, boot data, then
code) is established by `../tlink.l`'s memory map and the blocks `../mkboot.l`
lays, so the flatten is the whole "packer".

## Flashing

Tap the white button on the Teensy 4.1 to enter the bootloader, then:

```
teensy_loader_cli --mcu=TEENSY41 -w -v out/teensy41/love.hex   # or: make flash
```

`teensy_loader_cli` is PJRC's open-source command-line loader
(<https://github.com/PaulStoffregen/teensy_loader_cli>); the Teensy Loader GUI
that ships with Teensyduino flashes the same `.hex`.

## Recovery

If a bad image bricks the console, the bootloader chip is independent of the
i.MX RT1062 -- the button always re-enters it, so a re-flash always recovers the
board. (There is no equivalent to soldering or UF2-mass-storage gymnastics.)
