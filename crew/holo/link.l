; crew/holo/link.l -- a static ELF LINKER over obj.l's relocatable objects (the
; toolchain ladder, rung 3): read each .o (the section shape obj.l lays --
; .text / .data / ai_nifs + symtab + RELA), resolve every symbol across the set
; (strong over weak; __start_/__stop_ai_nifs synthesized over the packed nif
; region), lay the sections at the classic ET_EXEC base, apply the relocations
; (R_64 an absolute S+A; PC32/PLT32 both the pc-relative S+A-P once the link is
; static), and wrap the bytes in elf.l's one-segment executable header. entry
; is the FIRST object's text -- the driver leads with a crt0 object built by
; objelf itself, so `main` resolves through the same reloc path as everything
; else. our own objects only: a foreign .o (gcc's .eh_frame / .bss / comdat
; zoo) scares off honestly, never drops a section. two targets, like obj.l
; (x86-64 field relocs REPLACE their field; aarch64 ones OR bits into the
; instruction word the placeholder carries).
; loads after crew/holo/obj.l in the mooncc cat -- reads elf-base / elf-machines /
; elf64-hdr / le32 / le64 bare (asbook.l + elf.l re-leak them). moon.l drives it:
; `mooncc a.o b.c -o prog`.
(:
   ; little-endian reads off a byte-indexable text; a u64 field can carry a
   ; signed value (a RELA addend), so sg64 folds the high half back down
   (rle16 s i) ((s i) + (256 * (s (i + 1))))
   (rle32 s i) ((rle16 s i) + (65536 * (rle16 s (i + 2))))
   (rle64 s i) ((rle32 s i) + (4294967296 * (rle32 s (i + 4))))
   (sg64 v) (? (v < 9223372036854775808) v (v - 18446744073709551616))
   (ld-cstr s i) (: (go j) (? ((s j) = 0) (snip s i j) (go (j + 1))) (go i))
   (ld-zeros n) (? (n < 1) () (0 >< (ld-zeros (n - 1))))
   (ld-align a v) (? (a < 2) v (: r (v % a) (? (r = 0) v (v + (a - r)))))
   (ld-fits32 v) (&& (-2147483648 <= v) (v <= 2147483647))
   ; a section tag's base key on the object record
   (ld-key tag) (? (id? tag 'text) 'tbase (id? tag 'data) 'dbase 'nbase)

   ; parse one relocatable object (id names it in scares) -> a tablet
   ;   'id 'src            the name and the raw bytes
   ;   'secmap             section index -> tag ('text | 'data | 'nifs)
   ;   'sects              tag -> (off size align)
   ;   'syms / 'nsym       symbol index -> (name bind typ shndx value)
   ;   'relas              ((tag (off kind symidx addend)..) ..)
   ; layout pins 'tbase/'dbase/'nbase (the load addresses) on it later.
   (ld-read id s mach)
    (: _ (? (&& ((s 0) = 127) (&& ((s 1) = 69) (&& ((s 2) = 76) ((s 3) = 70)))) 0
            (scare 'link-not-elf id))
       _ (? (&& ((rle16 s 16) = 1) ((rle16 s 18) = mach)) 0 (scare 'link-machine id))
       _ (? ((rle16 s 58) = 64) 0 (scare 'link-shentsize id))
       shoff (rle64 s 40) shnum (rle16 s 60) shstrx (rle16 s 62)
       (sh i) (shoff + (i * 64))                       ; the i-th section header
       shstro (rle64 s ((sh shstrx) + 24))
       (sname i) (ld-cstr s (shstro + (rle32 s (sh i))))
       ob (tablet 0) _ (pin ob 'id id) _ (pin ob 'src s)
       secmap (tablet 0) _ (pin ob 'secmap secmap)
       sects (tablet 0) _ (pin ob 'sects sects)
       ; classify: our three PROGBITS by name, symtab/rela by type; anything
       ; else with bytes is a foreign object -- refuse, never drop
       (class i symt rls)
        (? (shnum <= i) (symt >< rls)
           (: h (sh i) ty (rle32 s (h + 4))
              off (rle64 s (h + 24)) sz (rle64 s (h + 32))
              (? (ty = 1)
                  (: nm (sname i)
                     tag (? (nm = ".text") 'text (nm = ".data") 'data
                            (nm = "ai_nifs") 'nifs ())
                     (? !tag (? (0 < sz) (scare 'link-section nm) (class (i + 1) symt rls))
                        (: _ (pin secmap i tag)
                           _ (pin sects tag `(off sz (rle64 s (h + 48))))
                           (class (i + 1) symt rls))))
                 (ty = 2) (class (i + 1) `(off sz (rle32 s (h + 40))) rls)
                 (ty = 4) (class (i + 1) symt (`((rle32 s (h + 44)) off sz) >< rls))
                 (member? ty '(0 3)) (class (i + 1) symt rls)
                 (0 < sz) (scare 'link-section (sname i))
                 (class (i + 1) symt rls))))
       cl (class 1 () ())
       symt <cl
       _ (? (two? symt) 0 (scare 'link-no-symtab id))
       stro (rle64 s ((sh (at symt 2)) + 24))       ; .strtab offset, via symtab's link
       syms (tablet 0) _ (pin ob 'syms syms)
       nsym ((at symt 1) // 24)
       _ (pin ob 'nsym nsym)
       _ (: (go i) (? (nsym <= i) 0
                      (: e ((at symt 0) + (i * 24))
                         info (s (e + 4))
                         _ (pin syms i `((ld-cstr s (stro + (rle32 s e)))
                                          (info >> 4) (info & 15)
                                          (rle16 s (e + 6)) (rle64 s (e + 8))))
                         (go (i + 1))))
            (go 0))
       relas (map (\ r (: tag (peep secmap <r ())
                          _ (? tag 0 (scare 'link-rela-target id))
                          n ((at r 2) // 24)
                          (es i acc) (? (i < 0) acc
                                        (: e ((at r 1) + (i * 24))
                                           (es (i - 1)
                                               (`((rle64 s e) (rle32 s (e + 8))
                                                        (rle32 s (e + 12)) (sg64 (rle64 s (e + 16)))) >< acc))))
                          (tag >< (es (n - 1) ()))))
                  >cl)
       _ (pin ob 'relas relas)
       ob)

   ; assign every section its load address: header first, then all text (each
   ; aligned as its header asks), all data, then the nif region -- 16-aligned
   ; once and packed tight, so the __start_/__stop_ bracket reads it as ONE
   ; array across every object. pins the bases; -> (end nifstart nifend).
   (ldlay objs base)
    (: (one tag cur os)
        (? !(two? os) cur
           (: ob <os se (peep (peep ob 'sects 0) tag ())
              (? !(two? se) (one tag cur >os)
                 (: a (ld-align (at se 2) cur)
                    _ (pin ob (ld-key tag) a)
                    (one tag (a + (at se 1)) >os)))))
       t1 (one 'text (base + 120) objs)
       d1 (one 'data t1 objs)
       n0 (ld-align 16 d1)
       `((one 'nifs n0 objs) n0))

   ; the one symbol book: name -> (addr weak?), every defined global/weak
   ; across the set. a strong def beats any weak; two strongs scare; the
   ; first weak stands until a strong arrives.
   (ldsyms objs)
    (: glo (tablet 0)
       (one ob i)
        (? ((peep ob 'nsym 0) <= i) 0
           (: sy (peep (peep ob 'syms 0) i ())
              bind (at sy 1) shx (at sy 3)
              (? (|| (shx = 0) !(member? bind '(1 2))) (one ob (i + 1))
                 (: nm <sy
                    tag (peep (peep ob 'secmap 0) shx ())
                    _ (? tag 0 (scare 'link-sym-section nm))
                    a ((peep ob (ld-key tag) 0) + (at sy 4))
                    w (bind = 2)
                    old (peep glo nm ())
                    _ (? !(two? old) (pin glo nm `(a w))
                         w 0                                   ; incoming weak: the standing def keeps
                         (at old 1) (pin glo nm `(a w))    ; strong replaces weak
                         (scare 'link-dup nm))
                    (one ob (i + 1))))))
       _ (: (each os) (? !(two? os) 0 (: _ (one <os 1) (each >os)))
            (each objs))
       glo)

   ; resolve a name: the symbol book, or a synthesized bracket -- the nif region
   ; (n0..n1) or, under -pie, the RELATIVE-reloc offset table (r0..r1) that
   ; nolibc's startup walks to slide every abs64 data pointer by the load base.
   ; non-pie leaves r0 == r1 (an empty table), so the startup loop is a no-op.
   (ldres glo n0 n1 r0 r1 nm)
    (: r (peep glo nm ())
       (? (two? r) <r
          (nm = "__start_ai_nifs") n0
          (nm = "__stop_ai_nifs") n1
          (nm = "__start_ai_rela") r0
          (nm = "__stop_ai_rela") r1
          (scare 'link-undef nm)))

   ; patch one object's one section: stream its bytes, splicing each
   ; relocation's field. a section-symbol reloc lands on the object's own
   ; section base; a named one resolves through the book (so a weak def's
   ; callers meet the strong override). entries arrive in ascending offset
   ; order (obj.l's walk order) -- pieces come out as text snips + byte lists.
   (ldpatch ob tag base glo n0 n1 r0 r1 relbox)
    (: s (peep ob 'src 0) se (peep (peep ob 'sects 0) tag ())
       off0 (at se 0) sz (at se 1)
       (ent es) (? !(two? es) ()
                   (id? <<es tag) ><es
                   (ent >es))
       (go pos es acc)
        (? !(two? es) (rev ((snip s pos (off0 + sz)) >< acc))
           (: e <es ro <e kind (at e 1) ad (at e 3)
              _ (? ((pos - off0) <= ro) 0 (scare 'link-unsorted (peep ob 'id 0)))
              sy (peep (peep ob 'syms 0) (at e 2) ())
              sv (? ((at sy 2) = 3)                 ; STT_SECTION: the section's base
                    (: stag (peep (peep ob 'secmap 0) (at sy 3) ())
                       _ (? stag 0 (scare 'link-sym-section (peep ob 'id 0)))
                       (peep ob (ld-key stag) 0))
                    (ldres glo n0 n1 r0 r1 <sy))
              w (? (member? kind '(1 257)) 8 4)
              ; -pie: this abs64 site holds a base-0 offset (base is 0), so record its
              ; load-address for the startup relocator to add the real base to.
              _ (? (&& relbox (member? kind '(1 257)))
                    (pin relbox 0 ((base + ro) >< (peep relbox 0 ()))) 0)
              _ (? (member? kind '(1 2 4 257 275 277 282 283)) 0 (scare 'link-reloc kind))
              _ (? ((ro + w) <= sz) 0 (scare 'link-oob (peep ob 'id 0)))
              ; the aarch64 field kinds patch BITS of the instruction word the
              ; placeholder carries -- read it back and OR the field in
              wd (? (member? kind '(1 257)) 0 (rle32 s (off0 + ro)))
              bs (? (member? kind '(1 257)) (le64 (sv + ad))   ; R_X86_64_64 / R_AARCH64_ABS64: S + A outright
                    (member? kind '(2 4))
                     (: v ((sv + ad) - (base + ro))     ; PC32/PLT32: S + A - P
                        _ (? (ld-fits32 v) 0 (scare 'link-range <sy))
                        (le32 v))
                    (member? kind '(282 283))           ; JUMP26/CALL26: (S+A-P)>>2 into imm26
                     (: v ((sv + ad) - (base + ro))
                        _ (? (&& (-134217728 <= v) (v <= 134217727)) 0 (scare 'link-range <sy))
                        (le32 (wd | ((v // 4) & 67108863))))
                    (kind = 275)                        ; ADR_PREL_PG_HI21: page(S+A)-page(P) into adrp
                     (: pg (((sv + ad) // 4096) - ((base + ro) // 4096))
                        _ (? (&& (-1048576 <= pg) (pg <= 1048575)) 0 (scare 'link-range <sy))
                        (le32 (wd | (((pg & 3) << 29) | (((pg >> 2) & 524287) << 5)))))
                    (le32 (wd | (((sv + ad) % 4096) << 10))))   ; ADD_ABS_LO12_NC: the low 12
              (go (off0 + ro + w) >es
                  (bs >< ((snip s pos (off0 + ro)) >< acc)))))
       (go off0 (ent (peep ob 'relas ())) ()))

   ; count the abs64 (R_X86_64_64 / R_AARCH64_ABS64) data relocations across the
   ; set -- the entry count of the -pie relocation table (8 bytes each).
   (ld-nabs objs)
    (foldl (\ a ob (foldl (\ a2 tr
              (foldl (\ a3 e (? (member? (at e 1) '(1 257)) (a3 + 1) a3)) a2 >tr))
              a (peep ob 'relas ())))
       0 objs)

   ; link a set of objects -- ((id bytes-text)..) -- into a static-exe PIECE
   ; list (texts + byte lists; ld-write lays them down). entry = the first
   ; object's text. one R+X+W segment maps the whole file, elf.l's shape.
   ; pie: emit ET_DYN laid at base 0 (the kernel biases it to a random high
   ; address), collect every abs64 site into a trailing offset table bracketed
   ; by __start_/__stop_ai_rela, and let nolibc's startup add the load base to
   ; each -- the classic static-PIE self-relocation, our own toolchain end to end.
   (ldlink target pie srcs)
    (: mach (peep elf-machines target 0)
       _ (? (0 < mach) 0 (scare 'link-target target))
       objs (map (\ p (ld-read <p <>p mach)) srcs)
       base (? pie 0 elf-base)
       lay (ldlay objs base)
       end <lay n0 <>lay
       nrel (? pie (ld-nabs objs) 0)
       r0 (? pie (ld-align 16 end) end)         ; the reloc table starts 16-aligned, just past the nifs
       r1 (r0 + (8 * nrel))
       relbox (? pie (: t (tablet 0) _ (pin t 0 ()) t) ())    ; abs64 site accumulator (pie only)
       glo (ldsyms objs)
       ; emit in layout order, zero-padding up to each section's base
       (emit tag cur os acc)
        (? !(two? os) (cur >< acc)
           (: ob <os se (peep (peep ob 'sects 0) tag ())
              (? !(two? se) (emit tag cur >os acc)
                 (: b (peep ob (ld-key tag) 0)
                    pad (? (cur < b) `((ld-zeros (b - cur))) ())
                    (emit tag (b + (at se 1)) >os
                          (acc + pad + (ldpatch ob tag b glo n0 end r0 r1 relbox)))))))
       et (emit 'text (base + 120) objs ())
       ed (emit 'data <et objs >et)
       en (emit 'nifs <ed objs >ed)
       tail (? (<en < r0) `((ld-zeros (r0 - <en))) ())      ; nif tail + table alignment in one pad
       rtab (? pie (catall (map (\ o (le64 o)) (rev (peep relbox 0 ())))) ())
       hdr (elf64-hdr (? pie 3 2) base (peep <objs 'tbase 0) (r1 - base) mach)
       (hdr >< (>en + tail + (? pie `(rtab) ()))))

   ; write a piece list to a file, raw (texts pass through; byte lists pack into a
   ; CASK -- the byte array, not a string that happens to hold bytes today; see
   ; doc/proto/widen.md). an explicit left walk: map runs its effects right-to-left.
   (ld-write path ps)
    (: q (open path "w")
       (go l) (? !(two? l) (close q)
                 (: _ (say q (: p <l (? (string? p) p (cask p))))
                    (go >l)))
       (go ps)))
