#!/usr/bin/env gl
; elf2uf2.g -- pack a linked RP2040 ELF into a flashable UF2 image.
;
; The RP2040 USB mass-storage bootloader accepts UF2: a stream of 512-byte
; blocks, each carrying up to 256 bytes of payload plus a header naming the
; flash address to program. This tool walks the ELF's PT_LOAD segments by
; their LOAD address (p_paddr -- the .data segment lives in RAM at runtime but
; loads from flash), lays them into one contiguous 256-aligned flash image,
; and emits one UF2 block per 256-byte page.
;
; Usage: gl elf2uf2.g INPUT.elf OUTPUT.uf2
;
; Port of mk/tools/py/elf2uf2.py -- output must stay byte-identical (see
; mk/tools/Makefile test_elf2uf2 + mk/tools/py/README.md). ELF32 little-endian
; (the chip is Cortex-M0+); only flash-resident segments are packed.

(: (die m) (: _ (fputs err "elf2uf2: ") _ (fputs err m) _ (fputs err "\n") (exit 1)))

(: (u8 s o)   (get 0 o s)
   (rd16 s o) (| (u8 s o)   (<< (u8 s (+ o 1)) 8))
   (rd32 s o) (| (rd16 s o) (<< (rd16 s (+ o 2)) 16)))

; (wr out o n v): store the low n bytes of v little-endian into buf out at
; offset o; return the next offset.
(: (wr out o n v)
   ((: (f k v) (? (< k n) (: _ (put (+ o k) (& v 255) out) (f (+ k 1) (>> v 8))) (+ o n))) 0 v))

(: (align-up x a)   (& (+ x (- a 1)) (^ (- a 1) -1))
   (align-down x a) (& x (^ (- a 1) -1)))

; --- UF2 constants --------------------------------------------------
(: UF2-MAGIC0   0x0a324655
   UF2-MAGIC1   0x9e5d5157
   UF2-MAGIC-END 0x0ab16f30
   UF2-FLAG-FAMILY 0x00002000      ; familyID present
   RP2040-FAMILY 0xe48bff56
   FLASH-LO 0x10000000  FLASH-HI 0x20000000)

; --- flash PT_LOAD segments -> list of (paddr offset filesz) --------
(: (loads s phoff phent phnum)
   ((: (loop i)
       (? (< i phnum)
          (: o    (+ phoff (* i phent))
             rest (loop (+ i 1))
             pa   (rd32 s (+ o 12))                       ; p_paddr (LOAD addr)
             fsz  (rd32 s (+ o 16))                       ; p_filesz
             (? (&& (= 1 (rd32 s o))                      ; PT_LOAD
                 (&& (< 0 fsz) (&& (<= FLASH-LO pa) (< pa FLASH-HI))))
                (X (L pa (rd32 s (+ o 4)) fsz) rest)
                rest))
          0))
    0))

(: (min-pa segs m) (? (twop segs) (min-pa (B segs) (? (< (get 0 0 (A segs)) m) (get 0 0 (A segs)) m)) m)
   (max-end segs m) (? (twop segs) (max-end (B segs) (: e (+ (get 0 0 (A segs)) (get 0 2 (A segs))) (? (< m e) e m))) m))

; --- emit one UF2 block for page i ----------------------------------
(: (emit-block out bo img isz base i nblocks)
   (: _ (wr out bo 4 UF2-MAGIC0)
      _ (wr out (+ bo 4) 4 UF2-MAGIC1)
      _ (wr out (+ bo 8) 4 UF2-FLAG-FAMILY)
      _ (wr out (+ bo 12) 4 (+ base (* i 256)))          ; targetAddr
      _ (wr out (+ bo 16) 4 256)                         ; payloadSize
      _ (wr out (+ bo 20) 4 i)                           ; blockNo
      _ (wr out (+ bo 24) 4 nblocks)                     ; numBlocks
      _ (wr out (+ bo 28) 4 RP2040-FAMILY)
      _ (bcopy out (+ bo 32) img (* i 256) 256)          ; 256B payload (img is pre-padded)
      (wr out (+ bo 508) 4 UF2-MAGIC-END)))

(: args    (B argv)
   inpath  (? (twop args) (A args) (die "usage: elf2uf2.g INPUT.elf OUTPUT.uf2"))
   outpath (? (twop (B args)) (A (B args)) (die "usage: elf2uf2.g INPUT.elf OUTPUT.uf2"))
   port    (open inpath "r")
   _       (? port 0 (die (scat "cannot open " inpath)))
   s       (slurp port)
   _       (? (&& (= 127 (u8 s 0)) (&& (= 69 (u8 s 1)) (&& (= 76 (u8 s 2)) (= 70 (u8 s 3))))) 0
              (die (scat inpath ": not an ELF file")))
   _       (? (&& (= 1 (u8 s 4)) (= 1 (u8 s 5))) 0 (die (scat inpath ": not ELF32 little-endian")))
   phoff   (rd32 s 28)
   phent   (rd16 s 42)
   phnum   (rd16 s 44)
   segs    (loads s phoff phent phnum)
   _       (? (twop segs) 0 (die "no flash-resident PT_LOAD segments"))
   base    (align-down (min-pa segs (get 0 0 (A segs))) 256)
   end     (align-up (max-end segs 0) 256)
   isz     (- end base)
   img     (bufnew isz)                                  ; zero-filled contiguous flash image
   _       (each segs (\ seg (bcopy img (- (get 0 0 seg) base) s (get 0 1 seg) (get 0 2 seg))))
   nblocks (/ isz 256)
   out     (bufnew (* nblocks 512))
   _       ((: (f i) (? (< i nblocks) (: _ (emit-block out (* i 512) img isz base i nblocks) (f (+ i 1))) 0)) 0)
   op      (open outpath "w")
   _       (? op 0 (die (scat "cannot open " outpath)))
   (fputs op out))
