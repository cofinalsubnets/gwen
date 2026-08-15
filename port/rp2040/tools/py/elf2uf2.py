#!/usr/bin/env python3
# Frozen golden reference for mk/tools/elf2uf2.g -- DO NOT edit to change
# behaviour without updating the .g in the same commit (mk/tools/Makefile gates
# them byte-for-byte). See mk/tools/py/README.md.
#
# Packs a linked RP2040 ELF32 into a flashable UF2 image: walk the PT_LOAD
# segments by LOAD address (p_paddr), lay them into one contiguous 256-aligned
# flash image, and emit one 512-byte UF2 block per 256-byte page.
#
# Usage: elf2uf2.py INPUT.elf OUTPUT.uf2

import struct
import sys

UF2_MAGIC0 = 0x0A324655
UF2_MAGIC1 = 0x9E5D5157
UF2_MAGIC_END = 0x0AB16F30
UF2_FLAG_FAMILY = 0x00002000
RP2040_FAMILY = 0xE48BFF56
FLASH_LO = 0x10000000
FLASH_HI = 0x20000000


def die(msg):
    sys.stderr.write("elf2uf2: " + msg + "\n")
    sys.exit(1)


def main():
    if len(sys.argv) < 3:
        die("usage: elf2uf2.py INPUT.elf OUTPUT.uf2")
    inpath, outpath = sys.argv[1], sys.argv[2]
    s = open(inpath, "rb").read()
    if s[:4] != b"\x7fELF":
        die(inpath + ": not an ELF file")
    if s[4] != 1 or s[5] != 1:
        die(inpath + ": not ELF32 little-endian")
    phoff = struct.unpack_from("<I", s, 28)[0]
    phent = struct.unpack_from("<H", s, 42)[0]
    phnum = struct.unpack_from("<H", s, 44)[0]

    segs = []  # (paddr, offset, filesz)
    for i in range(phnum):
        o = phoff + i * phent
        p_type, p_offset, p_vaddr, p_paddr, p_filesz = struct.unpack_from("<IIIII", s, o)
        if p_type == 1 and p_filesz > 0 and FLASH_LO <= p_paddr < FLASH_HI:
            segs.append((p_paddr, p_offset, p_filesz))
    if not segs:
        die("no flash-resident PT_LOAD segments")

    base = min(pa for pa, _, _ in segs) & ~255
    end = (max(pa + fsz for pa, _, fsz in segs) + 255) & ~255
    img = bytearray(end - base)
    for pa, off, fsz in segs:
        img[pa - base:pa - base + fsz] = s[off:off + fsz]

    nblocks = (end - base) // 256
    out = bytearray()
    for i in range(nblocks):
        out += struct.pack("<IIIIIIII",
                           UF2_MAGIC0, UF2_MAGIC1, UF2_FLAG_FAMILY,
                           base + i * 256, 256, i, nblocks, RP2040_FAMILY)
        out += img[i * 256:i * 256 + 256]
        out += bytes(476 - 256)
        out += struct.pack("<I", UF2_MAGIC_END)

    open(outpath, "wb").write(out)


if __name__ == "__main__":
    main()
