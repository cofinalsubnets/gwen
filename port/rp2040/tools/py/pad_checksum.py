#!/usr/bin/env python3
# Frozen golden reference for mk/tools/pad_checksum.g -- DO NOT edit to change
# behaviour without updating the .g in the same commit (mk/tools/Makefile gates
# them byte-for-byte). See mk/tools/py/README.md.
#
# Stamps the RP2040 second-stage bootloader checksum: read the raw boot2
# payload (<=252 bytes), zero-pad to 252, append the 4-byte CRC-32/MPEG-2 the
# bootrom checks (poly 0x04c11db7, init 0xffffffff, no reflection), and emit a
# .byte array in section .boot2. Mirrors the Pico SDK's own pad_checksum.py.
#
# Usage: pad_checksum.py <boot2.bin>   (writes the .S to stdout)

import sys


def crc32_mpeg2(buf):
    crc = 0xFFFFFFFF
    for b in buf:
        crc ^= b << 24
        for _ in range(8):
            crc = ((crc << 1) ^ 0x04C11DB7) & 0xFFFFFFFF if crc & 0x80000000 \
                else (crc << 1) & 0xFFFFFFFF
    return crc


def main():
    if len(sys.argv) < 2:
        sys.stderr.write("pad_checksum: usage: pad_checksum.py <boot2.bin>\n")
        sys.exit(1)
    inpath = sys.argv[1]
    data = open(inpath, "rb").read()
    if len(data) > 252:
        sys.stderr.write("pad_checksum: boot2 payload exceeds 252 bytes\n")
        sys.exit(1)
    image = bytearray(data) + bytes(252 - len(data))
    image += crc32_mpeg2(image).to_bytes(4, "little")

    out = sys.stdout
    out.write("// Padded and checksummed version of: %s\n" % inpath)
    out.write("\n.cpu cortex-m0plus\n.thumb\n\n.section .boot2, \"ax\"\n\n")
    for j, b in enumerate(image):
        out.write((".byte " if j % 16 == 0 else ", ") + "0x%02x" % b)
        if j % 16 == 15:
            out.write("\n")


if __name__ == "__main__":
    main()
