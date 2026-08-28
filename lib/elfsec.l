; lib/elfsec.l -- a named section's bytes out of any ELF, on any machine. `(use 'elfsec)`:
;
;   (elfsec path nm) -> (1 bytes) | ()      the section named nm, or () for no file / no section
;
; the (1 bytes) wrapper is the answer, not the bytes: an empty section is a real section and
; a bare () would call it absent. a section table is a table, so an aarch64 object reads on
; x86-64 asking nothing of the machine underneath -- which is how `.comment`, the producer
; record, is read back with no binutils in the picture.
(:
   (es-slurp p) (: q (open p "r") (q && (: s (slurp q) _ (close q) s)))
   (es-le s i n) (: (go k a m) (? (k = n) a (go (k + 1) (a + peep s (i + k) 0 * m) (m * 256)))
                    (go 0 0 1))
   ; e_ident[4] is the class, 1 is 32 and 2 is 64; the rows are the same fields at their two
   ; spellings -- shoff shentsz shnum | sh_name sh_off sh_size, and e_shstrndx two past shnum.
   es-e32 '(32 4 46 48   0 4 16 4 20 4)
   es-e64 '(40 8 58 60   0 4 24 8 32 8)
   (es-sec exe nm)
    (: t (peep exe 4 0 = 1 ? es-e32 es-e64)
       (le i n) (es-le exe i n)
       (f i) (t i)
       shoff (le (f 0) (f 1)) shentsz (le (f 2) 2) shnum (le (f 3) 2)
       shstrx (le (f 3 + 2) 2)
       (sh i o w) (le (shoff + i * shentsz + o) w)
       (shf i k) (sh i (f k) (f (k + 1)))
       stro (shf shstrx 6)
       (cstr i) (: (go j) (? (peep exe j 0 = 0) (snip exe i j) (go (j + 1))) (go i))
       (go i) (? (i = shnum) ()
                 (nm = cstr (stro + shf i 4))
                  (: o (shf i 6) [(snip exe o (o + shf i 8))])
                 (go (i + 1)))
       (go 0))
   (elfsec path nm) (: s (es-slurp path) (string? s && es-sec s nm))
   )
