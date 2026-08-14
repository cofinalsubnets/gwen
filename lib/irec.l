; lib/irec.l -- READ THE RECORD: the machine-form IR `mooncc -fir=` laid in an object or a
; binary, out of ANY file, on ANY machine. `(use 'irec)`:
;
;   (irec-read path)  -> ((name form..) ..) | ()     the record in that file
;   (irec-self ())    -> ((name form..) ..) | ()     ..in the love running this
;   (irec-of rows nm) -> (form..) | ()               one function's, out of a read
;   (irec-sym exe nm) -> a symbol's FILE offset | () the ELF walk, for whoever needs it
;
; ⚠ THIS IS NOT THE JIT, and the split is deliberate: love/glaze/hook.l lights its lane on
; `(from 'splice)`, so a reader that merely wants to LOOK at a binary must not be that module.
; lib/splice.l is one consumer of this one; `tools/ir.l` is the other, and a third is whoever
; opens a binary wondering what the compiler thought it was emitting.
;
; ⚠ CROSS-EVERYTHING, because a record is text and an ELF is a table: the forms in an aarch64
; object are aarch64's, and reading them on x86-64 asks nothing of the machine underneath --
; no disassembler, no per-arch mnemonic table, no objdump that has to have been built with
; the right target list. That is the whole reason this exists as a record rather than as a
; disassembly. What you cannot do cross-machine is RUN what you read: that is splice.l's
; business, and it is in-process by construction.
(:
   (irec-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) s) ()))
   (irec-le s i n) (: (go k a m) (? (k = n) a (go (k + 1) (a + peep s (i + k) 0 * m) (m * 256)))
                      (go 0 0 1))
   ; ⚠ BOTH ELF CLASSES, and it is not optional here: three of mooncc's six targets are
   ; 32-bit ARM and lay ELF32, where every offset below moves. e_ident[4] is the class -- 1
   ; is 32, 2 is 64 -- and the two rows are the same fields at their two spellings:
   ;   shoff shentsz shnum | sh_addr sh_off sh_size sh_link sh_entsz | st_val st_shndx symsz
   irec-e32 '(32 4 46 48   12 4 16 4 20 4 24 4 36 4   4 4 14 16)
   irec-e64 '(40 8 58 60   16 8 24 8 32 8 40 4 56 8   8 8  6 24)
   ; a symbol's FILE offset in an ELF's bytes | (). ⚠ works on a RELOCATABLE object as well as
   ; a linked one: st_value is section-relative there and sh_addr is 0, so the one arithmetic
   ; -- sh_offset + (va - sh_addr) -- covers both without a special case.
   (irec-sym exe nm)
    (: (le i n) (irec-le exe i n)
       (el l i) (? (i < 1) <l (el >l (i - 1)))
       t (? (1 = peep exe 4 0) irec-e32 irec-e64)      ; e_ident[EI_CLASS]
       (f i) (el t i)
       shoff (le (f 0) (f 1)) shentsz (le (f 2) 2) shnum (le (f 3) 2)
       (sh i o w) (le (shoff + ((i * shentsz) + o)) w)
       (shf i k) (sh i (f k) (f (k + 1)))              ; a section header field by row index
       symi (: (go i) (? (i = shnum) -1 (? ((sh i 4 4) = 2) i (go (i + 1)))) (go 0))
       (? (symi < 0) ()
          (: stroff (shf (shf symi 10) 6)              ; sh_offset of the section sh_link names
             symoff (shf symi 6) syment (shf symi 12) nsyms (shf symi 8 // syment)
             (cstr i) (: (go j) (? (peep exe j 0 = 0) (snip exe i j) (go (j + 1))) (go i))
             (go i) (? (i = nsyms) ()
                       (: o (symoff + (i * syment))
                          (? (= nm (cstr (stroff + (le o 4))))
                             (: va (le (o + (f 14)) (f 15)) sx (le (o + (f 16)) 2)
                                ((shf sx 6) + (va - (shf sx 4))))
                             (go (i + 1)))))
             (go 0))))
   ; the record itself: a NUL-terminated readable datum at that offset. ⚠ () is the honest
   ; answer three ways -- no such file, no symtab, no record -- and a caller wanting to tell
   ; them apart asks irec-sym itself.
   (irec-at exe nm)                                  ; the datum at ANY symbol, not just ours
    (: off (irec-sym exe nm)
       (? (nil? off) ()
          (: txt (: (go j) (? (= 0 (peep exe j 0)) (snip exe off j) (go (j + 1))) (go off))
             r (sound txt)
             (? (two? r) <r ()))))
   (irec-bytes exe) (irec-at exe "ai_lvm_ir")
   (irec-read path) (: s (irec-slurp path) (? (string? s) (irec-bytes s) ()))
   ; a symbol's VIRTUAL ADDRESS, the other half of the same walk -- what a live binding wants
   ; (that plus the load bias is where the symbol actually IS), where irec-sym answers where
   ; to READ it. lib/splice.l is the caller, and it is why this walk lives in one place.
   (irec-va exe nm)
    (: (le i n) (irec-le exe i n)
       (el l i) (? (i < 1) <l (el >l (i - 1)))
       t (? (1 = peep exe 4 0) irec-e32 irec-e64)
       (f i) (el t i)
       shoff (le (f 0) (f 1)) shentsz (le (f 2) 2) shnum (le (f 3) 2)
       (sh i o w) (le (shoff + ((i * shentsz) + o)) w)
       (shf i k) (sh i (f k) (f (k + 1)))
       symi (: (go i) (? (i = shnum) -1 (? ((sh i 4 4) = 2) i (go (i + 1)))) (go 0))
       (? (symi < 0) ()
          (: stroff (shf (shf symi 10) 6)
             symoff (shf symi 6) syment (shf symi 12) nsyms (shf symi 8 // syment)
             (cstr i) (: (go j) (? (peep exe j 0 = 0) (snip exe i j) (go (j + 1))) (go i))
             (go i) (? (i = nsyms) ()
                       (? (= nm (cstr (stroff + (le (symoff + (i * syment)) 4))))
                          (le (symoff + (i * syment) + (f 14)) (f 15))
                          (go (i + 1))))
             (go 0))))
   (irec-self _) (irec-read "/proc/self/exe")
   (irec-of rows nm) (: (go l) (@ l (r >< more) (? (id? <r nm) >r (go more)) ())
                        (go rows))
   ; the names a record carries, in the order the compiler laid them
   (irec-names rows) (map cap rows)
   )
