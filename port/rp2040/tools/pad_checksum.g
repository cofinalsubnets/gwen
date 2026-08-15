#!/usr/bin/env gl
; pad_checksum.g -- stamp the RP2040 second-stage bootloader checksum.
;
; The RP2040 bootrom copies the first 256 bytes of flash into RAM and runs
; them only if the last 4 bytes are a valid CRC of the preceding 252. This
; tool reads the raw boot2 payload (<= 252 bytes, e.g. arch/rp2040/boot2.bin),
; zero-pads it to 252 bytes, computes that CRC, and emits an assembler source
; (a .byte array in section .boot2) for the final link to place at 0x10000000.
;
; CRC is CRC-32/MPEG-2: poly 0x04c11db7, init 0xffffffff, no input/output
; reflection, xorout 0 -- exactly what the bootrom checks (and what the Pico
; SDK's pad_checksum.py emits; arch/rp2040/boot2.bin checksums to 0x7a4eb274).
;
; Usage: gl pad_checksum.g <boot2.bin>   (writes the .S to stdout)
;
; Port of mk/tools/py/pad_checksum.py -- their output must stay byte-identical
; (see mk/tools/Makefile test_pad_checksum + mk/tools/py/README.md).

(: (die m) (: _ (fputs err "pad_checksum: ") _ (fputs err m) _ (fputs err "\n") (exit 1)))

(: (u8 s o) (get 0 o s))

; --- CRC-32/MPEG-2 over the 252-byte padded image -------------------
; byte b folded in MSB-first; pad bytes past the payload read as 0.
(: (crc-byte crc b)
   ((: (f crc k)
       (? (< k 8)
          (f (? (& crc 0x80000000)
                (& (^ (<< crc 1) 0x04c11db7) 0xffffffff)
                (& (<< crc 1) 0xffffffff))
             (+ k 1))
          crc))
    (^ crc (<< b 24)) 0))

(: (crc32 s n)
   ((: (f crc i)
       (? (< i 252) (f (crc-byte crc (? (< i n) (u8 s i) 0)) (+ i 1)) crc))
    0xffffffff 0))

; --- hex byte writer ------------------------------------------------
(: hexd "0123456789abcdef"
   (put-byte p b)
   (: _ (fputs p "0x") _ (fputc p (u8 hexd (& (>> b 4) 15))) (fputc p (u8 hexd (& b 15)))))

; the j-th byte of the 256-byte image: payload, then zero pad, then the
; 4 CRC bytes little-endian.
(: (image-byte s n crc j)
   (? (< j n)   (u8 s j)
      (< j 252) 0
      (& (>> crc (* 8 (- j 252))) 255)))

; --- emit the 16-per-line .byte block -------------------------------
(: (emit-bytes p s n crc)
   ((: (f j)
       (? (< j 256)
          (: _ (fputs p (? (= 0 (mod j 16)) ".byte " ", "))
             _ (put-byte p (image-byte s n crc j))
             _ (? (= 15 (mod j 16)) (fputc p 10) 0)
             (f (+ j 1)))
          0))
    0))

(: args   (B argv)
   inpath (? (twop args) (A args) (die "usage: pad_checksum.g <boot2.bin>"))
   port   (open inpath "r")
   _      (? port 0 (die (scat "cannot open " inpath)))
   s      (slurp port)
   n      (pin s)
   _      (? (<= n 252) 0 (die "boot2 payload exceeds 252 bytes"))
   crc    (crc32 s n)
   _ (fputs out "// Padded and checksummed version of: ") _ (fputs out inpath) _ (fputc out 10)
   _ (fputs out "\n.cpu cortex-m0plus\n.thumb\n\n.section .boot2, \"ax\"\n\n")
   (emit-bytes out s n crc))
