; port/rp2040/mkboot2.l -- lay boot2.o for the Pico: the 256-byte second stage
; the RP2040's mask ROM copies into SRAM and runs to open the QSPI XIP window.
; the ROM runs it ONLY if the last 4 bytes are a valid CRC over the preceding
; 252, so the stamp is the whole point of this file.
;
; the payload (boot2.bin) is the vendor W25Q080 sequence, 240 bytes of it; this
; zero-pads to 252, stamps the CRC, and hands the 256 bytes to holo as ONE
; named section. no gas: `.boot2` is a section name the object writer knows how
; to spell now (objsecs32), which is the same door port/rp2040/rp2040.c's
; vector table rides -- so this port has no .S at all, the only thumb one that
; doesn't. rp2040.lds KEEPs .boot2 at flash base and the table right behind it.
;
; ⚠ CRC-32/MPEG-2, not the zlib one: poly 0x04c11db7, init 0xffffffff, NO input
; or output reflection, xorout 0. boot2.bin checksums to 0x7a4eb274 -- the one
; number that says this file still works.
;
; runs after crew/holo/{elf,obj}.l in a cat with the thumb1 backend spliced into
; the sealed holo module (the mkstart shape; port/rp2040/Makefile).
(: m32 4294967295
   (crc-step c) (: c2 ((c << 1) & m32)
                   (? (0 < (c & 2147483648)) (c2 ^ 79764919) c2))
   (crc-byte c b) (: (go c i) (? (i < 1) c (go (crc-step c) (i - 1)))
                     (go (c ^ ((b & 255) << 24)) 8))
   ; the payload's bytes, then the pad's -- a zero byte still turns the register
   (crc-mpeg2 s n) (: (go c i) (? (n <= i) c (go (crc-byte c (? ((tally s) <= i) 0 (s i))) (i + 1)))
                      (go m32 0))
   (mkboot2 src dst)
    (: r (uread src)
       _ (? (two? r) 0 (scare 'mkboot2-no-payload src))
       s <>r
       _ (? ((tally s) <= 252) 0 (scare 'mkboot2-too-big (tally s)))
       crc (crc-mpeg2 s 252)
       ; 252 payload+pad bytes, then the CRC little-endian: exactly 256
       bs ((: (go i acc) (? (252 <= i) (rev acc) (go (i + 1) ((? ((tally s) <= i) 0 (s i)) >< acc)))
              (go 0 ()))
           + (le32 crc))
       ir `(`('label 'boot2) `('raw bs))
       ; PROGBITS, ALLOC (read-only -- it is flash), on its own 256-byte grain
       (write-bytes dst (objsecs32 'thumb1 `(`(".boot2" 1 2 256 ir)) () '("boot2") ()))))
