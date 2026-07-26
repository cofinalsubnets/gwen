; crew/holo/obj.l -- a RELOCATABLE ELF64 object emitter (cc stage 7a). where elf.l
; wraps assembled bytes in a static EXECUTABLE (one RWX segment, all labels
; resolved), this lays a `.o` the SYSTEM linker can consume: separate .text /
; .data sections, a symbol table (each function a global FUNC, each global var a
; global OBJECT, each external reference an UNDEF), and RELA relocations for every
; reference the assembler cannot resolve within a section -- an la into .data
; (R_X86_64_PC32 via the .data section symbol), a call to a symbol defined
; elsewhere (R_X86_64_PLT32), a data pointer to a label (R_X86_64_64). intra-.text
; branches and calls stay pc-relative and resolve internally (both ends ride .text
; and move together). aarch64 rides the same walk with its own reloc vocabulary
; (CALL26/JUMP26, the ADRP/ADD pair, ABS64) -- see the type table and reltext.
; loads after crew/holo/elf.l in the au cat -- reads elf-machines / write-bytes
; bare, and the assembler internals bare too: the cat leads with (use 'holo)
; (crew/kore/asbook.l), so lay/laylax/patch-with/le*/catall resolve through the
; spliced module. moon.l drives it: `mooncc -c foo.c foo.o`.
(:
   ; the ELF relocation TYPES we emit (x86-64)
   r-64    1                                        ; R_X86_64_64     S + A   (an abs64 data pointer)
   r-pc32  2                                        ; R_X86_64_PC32   S + A - P (an la into .data)
   r-plt32 4                                        ; R_X86_64_PLT32  L + A - P (a call to an external fn)
   ; ... and aarch64. field relocs here patch BITS of an instruction word, not a
   ; whole field -- the placeholder bytes carry the base opcode (the fixup's aux).
   ra-abs64  257                                    ; R_AARCH64_ABS64           S + A
   ra-jump26 282                                    ; R_AARCH64_JUMP26          (S+A-P)>>2 into b's imm26
   ra-call26 283                                    ; R_AARCH64_CALL26          ... into bl's imm26
   ra-pghi21 275                                    ; R_AARCH64_ADR_PREL_PG_HI21  page(S+A)-page(P) into adrp
   ra-lo12   277                                    ; R_AARCH64_ADD_ABS_LO12_NC   (S+A) & 0xfff into add

   (o-has? tab k) !(id? (peep tab k 'no) 'no)    ; is key k bound in the tablet?
   (o-strbytes s) (: (go i acc) (? (i < 0) acc (go (i - 1) ((s i) >< acc)))
                     (go ((tally s) - 1) ()))       ; a string -> its byte list. this one
                     ; is a real ENCODE, not a bytes-through-text leak: an ELF string
                     ; table holds NAMES, which are text. when the charm widens this
                     ; becomes an explicit utf-8 encode (doc/proto/widen.md).

   ; walk an assembled section's items (bytes + fixups) -> (bytes relocs). a fixup
   ; is resolved in place when the section may (text-internal, a kind in rset --
   ; the translation-INVARIANT ones: a pc-relative delta moves with the section;
   ; an adrp page delta or an abs-lo12 does NOT and always relocates); otherwise
   ; it becomes a relocation record (off kind label addend) and placeholder bytes
   ; hold its place -- zeros where RELA overwrites the field outright (x64, abs64),
   ; the base opcode word where the reloc patches instruction BITS (aarch64).
   ; the addend is the fixup's aux -- a data pointer to &arr[i]/&s.field bakes the
   ; constant byte offset there, and the RELA carries it (added to the section-sym
   ; addend below), so the linker's S+A lands on the element, not the base.
   (o-walk be items labels wkset internal? rset off bacc racc)
    (? !(two? items) `((rev bacc) (rev racc))
       (: x <items
          (? (fix? x)
              (: lbl (fix-l x) w (fix-w x) k (fix-k x)
                 (? (&& (&& internal? (member? k rset))
                        (&& (o-has? labels lbl) !(o-has? wkset lbl)))   ; a weak target stays a reloc
                     (: tgt (peep labels lbl 0)
                        bs (patch-with be k tgt off w (fix-a x))
                        (o-walk be >items labels wkset internal? rset (off + w)
                                ((rev bs) + bacc) racc))
                    (: fill (? (member? k '(b26 b19 adrp21 addlo12)) (le32 (fix-a x))
                               ; ARM is REL, not RELA: the addend rides IN the field. a surviving thumb
                               ; branch holds its base encoding with the -4 PIPELINE addend baked in (the
                               ; field is pc=instr+4 relative, but the reloc's P is the instr address, so
                               ; the -4 compensates -- gas emits the identical fff7feff for `bl sym`). the
                               ; linker folds S-P into the imm bits and needs the opcode there already.
                               (member? k '(b bcc bl)) (patch-with be k off off w (fix-a x))
                               ; a surviving movw16/movt16 (thumb2 la) holds its base encoding with a
                               ; zero imm16 -- absolute, so no pipeline addend; bakemw below re-encodes
                               ; the section-relative addend into the field when the target binds one.
                               (member? k '(movw16 movt16)) (patch-with be k 0 off w (fix-a x))
                               (o-zeros w))
                       ; carry the byte just BEFORE the field (bacc's head, reversed) -- the
                       ; x64 opcode: E8/E9 (call/jmp) vs a lea's modrm, so reltext can tell a
                       ; control transfer (PLT32) from an address-of (PC32) for an external ref.
                       (o-walk be >items labels wkset internal? rset (off + w)
                               ((rev fill) + bacc) (`(off k lbl (fix-a x) (? (two? bacc) <bacc 0)) >< racc)))))
             (o-walk be >items labels wkset internal? rset (off + 1) (x >< bacc) racc))))
   (o-zeros n) (? (n < 1) () (0 >< (o-zeros (n - 1))))
   ; splice a 4-byte little-endian value into a byte list at OFF (the ARM REL in-place addend: a
   ; pooled abs32 field carries S's section offset there, since REL has no addend column).
   (o-take l n) (? !(two? l) () (n < 1) () (<l >< (o-take >l (n - 1))))
   (o-drop l n) (? (n < 1) l !(two? l) l (o-drop >l (n - 1)))
   ; splice pre-encoded bytes (a re-encoded instruction) in at OFF -- the movw16/movt16 addend bake.
   (o-putbs bytes off bs) (catall `((o-take bytes off) bs (o-drop bytes (off + (tally bs)))))
   (o-put32 bytes off val) (o-putbs bytes off (le32 val))

   ; the externals: labels referenced by a relocation but defined in NO
   ; section -- each needs an UNDEF symbol. unique, first-seen order.
   (o-exts rels tlabels dlabels nlabels)
    (: seen (tablet 0)
       (add lbl acc) (? (|| (o-has? tlabels lbl) (|| (o-has? dlabels lbl) (o-has? nlabels lbl))) acc
                        (o-has? seen lbl) acc
                        (: _ (pin seen lbl 1) (lbl >< acc)))
       (go rs acc) (? !(two? rs) acc (go >rs (add <>><rs acc)))   ; reloc = (off kind label); <>><rs = the label field
       (rev (go rels ())))

   ; a 24-byte symbol-table entry, and a 24-byte RELA entry.
   (o-sym nameoff bind typ shndx value)
    (catall `((le32 nameoff) `(((bind << 4) | typ) 0) (le16 shndx) (le64 value) (le64 0)))
   (o-rela offset symidx typ addend)
    (catall `((le64 offset) (le64 ((symidx * 4294967296) + typ)) (le64 addend)))

   ; a string table from a list of names -> (blob (offset..)); leading NUL, so the
   ; k-th name's offset is where it starts. shared for .strtab and .shstrtab.
   (o-strtab names)
    (: (go ns off bacc offs)
        (? !(two? ns) `((rev bacc) (rev offs))
           (: bs ((o-strbytes <ns) + `(0))
              (go >ns (off + (tally bs)) ((rev bs) + bacc) (off >< offs))))
       (go names 1 `(0) ()))

   ; the index kit both emitters lean on: a name-set tablet (interned -> 1), a list
   ; pinned elem -> i0+i (the symbol-index maps), and map-with-index.
   (o-names ns) (: t (tablet 0) (go ws) (? !(two? ws) t (: _ (pin t (intern <ws) 1) (go >ws))) (go ns))
   (o-idx ls i0) (: t (tablet 0) (go es i) (? !(two? es) t (: _ (pin t <es i) (go >es (i + 1)))) (go ls i0))
   (o-mapi f ls) (: (go l i acc) (? !(two? l) (rev acc) (go >l (i + 1) ((f i <l) >< acc))) (go ls 0 ()))

   ; -- ELF32 / EM_ARM (the thumb backends). the 32-bit cousin of the emitter below:
   ; a 52-byte header, 16-byte symbols, and REL (not RELA) relocations -- ARM's ABI
   ; keeps the addend IN the field, so a surviving thumb branch carries its base
   ; encoding (o-walk fills it) and .rel.text needs no addend column. thumb1 (v6-M)
   ; reaches here for `-c`: .text + function/undef symbols + R_ARM_THM_CALL for every
   ; external `bl`. a DATA relocation (an abs32 pointer) would need the literal-pool
   ; `la` the backend doesn't emit yet, so a surviving data fixup scares LOUD. --
   arm32-targets '(thumb1 thumb2 thumb2sp cortex-m0 cortex-m7 arm)
   (arm32? t) (member? t arm32-targets)
   r-thm-call 10                                    ; R_ARM_THM_CALL   ((S+A)|T) - P  (a thumb BL to a symbol)
   r-thm-jump24 30                                  ; R_ARM_THM_JUMP24 same math      (a thumb2 B.W tail-jump)
   r-arm-abs32 2                                     ; R_ARM_ABS32      S + A           (la's pooled address word)
   r-thm-movw 47                                    ; R_ARM_THM_MOVW_ABS_NC  (S+A)|T, low 16   (thumb2 la)
   r-thm-movt 48                                    ; R_ARM_THM_MOVT_ABS     S+A, high 16
   ; a 16-byte ELF32 symbol entry (name value size info other shndx) and an 8-byte REL entry.
   (o-sym32 nameoff bind typ shndx value)
    (catall `((le32 nameoff) (le32 value) (le32 0) `(((bind << 4) | typ) 0) (le16 shndx)))
   (o-rel32 offset symidx typ) (catall `((le32 offset) (le32 ((symidx * 256) + typ))))
   ; a 40-byte ELF32 section header.
   (sh32 nameoff typ flags off sz link info align entsz)
    (catall `((le32 nameoff) (le32 typ) (le32 flags) (le32 0)
               (le32 off) (le32 sz) (le32 link) (le32 info) (le32 align) (le32 entsz)))
   (objelf32 target textforms dataforms funcnames globnames wknames nifforms)
    (: machine (peep elf-machines target 0)
       _ (? (machine = 0) (scare 'no-elf-machine target) 0)
       be (peep backends target ())
       rset '(b bcc bl)                              ; thumb internal branches are pc-relative -> resolve in place
       wkset (o-names wknames)
       tp (: r laylax (? (lit? r) (r textforms be 0 (tablet 0) wkset) (scare 'holo-bake-stale 'laylax)))
       dp (lay dataforms be 0 () (tablet 0))
       np (lay nifforms be 0 () (tablet 0))
       tlabels >tp dlabels >dp nlabels >np
       tw (o-walk be <tp tlabels wkset 1 rset 0 () ())
       dw (o-walk be <dp dlabels wkset 0 rset 0 () ())
       nw (o-walk be <np nlabels wkset 0 rset 0 () ())
       tbytes0 <tw trel <>tw
       dbytes0 <dw drel <>dw
       nbytes <nw nrel <>nw
       _ (? (two? nrel)                             ; an ai_nifs data pointer is rare on thumb1 -- unsupported for now
             (scare 'obj32-nif-reloc-unsupported target) 0)
       exts (o-exts (trel + drel + nrel) tlabels dlabels nlabels)
       ; STATIC fns whose addresses survive as relocations (a thumb2 la's movw/movt
       ; pair, a data initializer's abs32 fn pointer -- love.c's ap tables): REL's
       ; in-field addend is 16-bit SIGNED, so a deep .text offset cannot ride it --
       ; the movt half truncates and the stored pointer drops its high bits (the
       ; qemu-M7 boot's pc hijack: a cata popped as 0x14ab instead of 0x114ab).
       ; each referenced .text label gets its own LOCAL FUNC symbol instead, thumb
       ; bit in st_value like the globals, addend 0 -- the gcc convention.
       fnset (o-names funcnames)
       locfns (: seen (tablet 0)
                 (add lbl acc) (? (! (o-has? tlabels lbl)) acc
                                  (o-has? fnset lbl) acc
                                  (o-has? seen lbl) acc
                                  (: _ (pin seen lbl 1) (lbl >< acc)))
                 (go rs acc) (? !(two? rs) acc
                                (: r <rs
                                   (go >rs (? (member? <>r '(movw16 movt16 abs32)) (add <>>r acc) acc))))
                 (rev (go drel (go trel ()))))
       ; STATIC DATA whose address rides a movw/movt pair has the SAME truncation:
       ; the split fields each carry the whole signed addend, so a section offset
       ; past 32K composes an address missing its middle bits -- the teensy boot's
       ; silent banner: main.o's 384K pool pushed its string offsets past 0x60000
       ; and every la'd string pointed 384K low. each referenced static-data label
       ; gets a LOCAL OBJECT symbol (st_value = its section offset, addend 0).
       ; abs32 fields stay on the section path -- a 32-bit field carries any offset.
       gset (o-names globnames)
       locdat (: seen (tablet 0)
                 (add lbl acc) (? (! (|| (o-has? dlabels lbl) (o-has? nlabels lbl))) acc
                                  (o-has? gset lbl) acc
                                  (o-has? seen lbl) acc
                                  (: _ (pin seen lbl 1) (lbl >< acc)))
                 (go rs acc) (? !(two? rs) acc
                                (: r <rs
                                   (go >rs (? (member? <>r '(movw16 movt16)) (add <>>r acc) acc))))
                 (rev (go drel (go trel ()))))
       nlocf (tally locfns) nlocd (tally locdat)
       nloc (nlocf + nlocd)
       nfun (tally funcnames) nglob (tally globnames)
       fstart (4 + nloc) gstart (fstart + nfun) estart (gstart + nglob)
       locidx (o-idx (locfns + locdat) 4)
       extidx (o-idx exts estart)
       fnidx (o-idx (map intern funcnames) fstart)
       globidx (o-idx (map intern globnames) gstart)
       ; an abs32 pool target -> (symidx field). a defined global / a function address binds its OWN
       ; symbol (addend rides the field, usually 0); a referenced STATIC fn its LOCAL symbol (above);
       ; a .data/ai_nifs static or string binds the SECTION symbol with its offset in the field
       ; (REL has no addend column); anything else is external.
       (a32loc lbl aux)
        (? (o-has? globidx lbl) `((peep globidx lbl 0) aux)
           (o-has? fnidx lbl)   `((peep fnidx lbl 0) aux)
           (o-has? locidx lbl)  `((peep locidx lbl 0) aux)
           (o-has? dlabels lbl) `(2 ((peep dlabels lbl 0) + aux))
           (o-has? nlabels lbl) `(3 ((peep nlabels lbl 0) + aux))
           `((peep extidx lbl 0) aux))
       ; bake each abs32 field (its addend + any section-relative offset) into the section bytes,
       ; since REL has no addend column. shared by .text (la's pool word) and .data (a pointer global).
       (bakeabs bytes rs)
        (: (go rs bs) (? !(two? rs) bs
                         (: r <rs
                            (go >rs (? (id? <>r 'abs32) (: sl (a32loc <>>r <>>>r) (o-put32 bs <r <>sl)) bs))))
           (go rs bytes))
       ; a movw16/movt16 (thumb2 la) target -> (symidx addend). the shape of a32loc, but the fixup's
       ; aux is Rd (not an addend), so the addend starts at 0; a .data/nif section-bound target
       ; carries its offset as the addend (BOTH fields carry the FULL value -- the linker reads each
       ; field as a whole signed addend, so this only fits under 32K; a static FN never rides here,
       ; it has its local symbol above).
       (mwloc lbl) (a32loc lbl 0)
       ; REL keeps the addend IN the imm16 scatter: re-encode each section-bound movw16/movt16 with
       ; its half of the addend (patch-with does the scatter; the linker adds S on top).
       (bakemw bytes rs)
        (: (go rs bs)
            (? !(two? rs) bs
               (: r <rs
                  (go >rs (? (member? <>r '(movw16 movt16))
                             (: sl (mwloc <>>r)
                                (? (<>sl = 0) bs
                                   (o-putbs bs <r (patch-with be <>r <>sl 0 4 <>>>r))))
                             bs))))
           (go rs bytes))
       tbytes (bakemw (bakeabs tbytes0 trel) trel)
       dbytes (bakeabs dbytes0 drel)
       symnames ((map (\ s (string s)) locfns) + (map (\ s (string s)) locdat)
                 + funcnames + globnames + (map (\ s (string s)) exts))
       str (o-strtab symnames)
       strblob <str stroffs <>str
       ; -- the symbol table. a FUNC symbol's value carries the THUMB BIT (bit 0): a
       ; cross-object R_ARM_THM_CALL folds it into T, so the callee is entered in
       ; Thumb state -- vital on M0 (no ARM state at all). data OBJECT syms stay even.
       ; the referenced-static LOCAL FUNC syms lead (symtab sh_info = first global). --
       symbytes
        (: lsyms (catall (o-mapi (\ i ls (o-sym32 (at stroffs i) 0 2 1
                                             ((peep tlabels ls 0) | 1)))                  ; STB_LOCAL STT_FUNC, +thumb bit
                                 locfns))
           dsyms (catall (o-mapi (\ i ls (? (o-has? dlabels ls)                           ; STB_LOCAL STT_OBJECT in .data/ai_nifs, plain value
                                            (o-sym32 (at stroffs (nlocf + i)) 0 1 2 (peep dlabels ls 0))
                                            (o-sym32 (at stroffs (nlocf + i)) 0 1 3 (peep nlabels ls 0))))
                                 locdat))
           fsyms (catall (o-mapi (\ i fs (o-sym32 (at stroffs (nloc + i))
                                             (? (member? fs wknames) 2 1)                 ; STB_WEAK vs STB_GLOBAL
                                             2 1 ((peep tlabels (intern fs) 0) | 1)))     ; STT_FUNC, +thumb bit
                                 funcnames))
           gsyms (catall (o-mapi (\ i gs (o-sym32 (at stroffs (nloc + (nfun + i))) 1 1 2 (peep dlabels (intern gs) 0)))
                                 globnames))
           esyms (catall (o-mapi (\ i e (o-sym32 (at stroffs (nloc + ((nfun + nglob) + i))) 1 0 0 0))
                                 exts))
           (catall `((o-sym32 0 0 0 0 0)              ; NULL
                      (o-sym32 0 0 3 1 0)              ; .text section symbol
                      (o-sym32 0 0 3 2 0)              ; .data
                      (o-sym32 0 0 3 3 0)              ; ai_nifs
                      lsyms dsyms fsyms gsyms esyms)))
       nsym ((4 + nloc) + (nfun + (nglob + (tally exts))))
       ; -- .rel.text: R_ARM_THM_CALL for each external/weak `bl` (internal ones resolved in place). --
       (fnsym lbl) (? (o-has? wkset lbl) (peep fnidx lbl 0) (peep extidx lbl 0))
       reltext (catall (map (\ r
                  (: off <r k <>r lbl <>>r aux <>>>r
                     (? (id? k 'bl) (o-rel32 off (fnsym lbl) r-thm-call)
                        (id? k 'b)  (o-rel32 off (fnsym lbl) r-thm-jump24)   ; a B.W tail-jump to an external/weak fn
                        (id? k 'abs32) (: sl (a32loc lbl aux) (o-rel32 off <sl r-arm-abs32))
                        (member? k '(movw16 movt16))
                         (: sl (mwloc lbl) (o-rel32 off <sl (? (id? k 'movw16) r-thm-movw r-thm-movt)))
                        (scare 'obj32-text-reloc k))))
                 trel))
       ntrel (tally trel)
       ; -- .rel.data: R_ARM_ABS32 for each data pointer (int *p = &g; a string-literal char *). --
       reldata (catall (map (\ r
                  (: off <r k <>r lbl <>>r aux <>>>r
                     (? (id? k 'abs32) (: sl (a32loc lbl aux) (o-rel32 off <sl r-arm-abs32))
                        (scare 'obj32-data-reloc k))))
                 drel))
       ndrel (tally drel)
       shnames `(".text" ".data" "ai_nifs" ".symtab" ".strtab" ".rel.text" ".rel.data" ".shstrtab")
       shstr (o-strtab shnames)
       shstrblob <shstr shoffs <>shstr
       (shname i) (at shoffs i)
       ; -- file layout: 52-byte header, section contents packed tight, then the SHT. --
       h-sz 52
       t-off h-sz               t-sz (tally tbytes)
       d-off (t-off + t-sz)     d-sz (tally dbytes)
       n-off (d-off + d-sz)     n-sz (tally nbytes)
       sym-off (n-off + n-sz)   sym-sz (nsym * 16)
       str-off (sym-off + sym-sz) str-sz (tally strblob)
       rt-off (str-off + str-sz)  rt-sz (ntrel * 8)
       rd-off (rt-off + rt-sz)    rd-sz (ndrel * 8)
       shs-off (rd-off + rd-sz)   shs-sz (tally shstrblob)
       sht-off (shs-off + shs-sz)
       sht (catall `(
              (o-zeros 40)                                                 ; 0 NULL
              (sh32 (shname 0) 1 6 t-off t-sz 0 0 4 0)                     ; 1 .text  PROGBITS ALLOC|EXEC
              (sh32 (shname 1) 1 3 d-off d-sz 0 0 4 0)                     ; 2 .data  PROGBITS ALLOC|WRITE
              (sh32 (shname 2) 1 3 n-off n-sz 0 0 4 0)                     ; 3 ai_nifs
              (sh32 (shname 3) 2 0 sym-off sym-sz 5 (4 + nloc) 4 16)       ; 4 .symtab link=.strtab info=first-global
              (sh32 (shname 4) 3 0 str-off str-sz 0 0 1 0)                 ; 5 .strtab
              (sh32 (shname 5) 9 0 rt-off rt-sz 4 1 4 8)                   ; 6 .rel.text  SHT_REL link=.symtab info=.text
              (sh32 (shname 6) 9 0 rd-off rd-sz 4 2 4 8)                   ; 7 .rel.data  SHT_REL link=.symtab info=.data
              (sh32 (shname 7) 3 0 shs-off shs-sz 0 0 1 0)))               ; 8 .shstrtab
       hdr (catall `(
              `(127 69 76 70 1 1 1 0 0 0 0 0 0 0 0 0)                     ; e_ident: \x7fELF 32-bit LSB
              (le16 1) (le16 machine) (le32 1)                            ; ET_REL, EM_ARM, version
              (le32 0) (le32 0) (le32 sht-off)                            ; e_entry=0, e_phoff=0, e_shoff
              (le32 83886080) (le16 52) (le16 0)                          ; e_flags=EABI_VER5, e_ehsize=52, e_phentsize=0
              (le16 0) (le16 40) (le16 9)                                 ; e_phnum=0, e_shentsize=40, e_shnum=9
              (le16 8)))                                                  ; e_shstrndx=8
       (catall `(hdr tbytes dbytes nbytes symbytes strblob reltext reldata shstrblob sht)))

   ; build the whole .o image. textforms/dataforms/nifforms are cgen-obj's split
   ; IR (nifforms = the AI_NIF registry entries, laid in their own `ai_nifs`
   ; section so the linker's __start_/__stop_ bracket symbols bound them),
   ; funcnames the DEFINED functions (global FUNC symbols), globnames the global
   ; variables (global OBJECT symbols); statics reach neither list and stay
   ; local labels. -> a byte list.
   (objelf target textforms dataforms funcnames globnames wknames nifforms)
    (? (arm32? target)
       (objelf32 target textforms dataforms funcnames globnames wknames nifforms)
    (: a64 (member? target '(arm64 aarch64))
       _ (? (|| (id? target 'x64) a64) 0 (scare 'obj-target target))
       rset (? a64 '(b26 b19) '(rel32 rel8))         ; the in-place-resolvable kinds (translation-invariant)
       machine (peep elf-machines target 0)
       be (peep backends target ())
       ; a WEAK-defined function's callers must go through a RELOCATION -- an
       ; in-place resolve would pin them to the local body, and the linker could
       ; never redirect to a strong override. built before lay: the relaxer needs
       ; it too (a shortened branch could never survive as a relocation).
       wkset (o-names wknames)
       ; the relaxing layout -- obj.l is tree-fresh in the kore/mooncc cats while the holo
       ; book rides the running binary's BAKE, and the build pairs them structurally (the
       ; wake shims exec the sibling love; crew/build.mk). a bake too old to carry laylax
       ; means a hand-mixed script/binary combo: scare LOUD, never lay quietly long.
       tp (: r laylax
             (? (lit? r) (r textforms be 0 (tablet 0) wkset)
                (scare 'holo-bake-stale 'laylax)))  ; assemble each section at org 0
       dp (lay dataforms be 0 () (tablet 0))
       np (lay nifforms be 0 () (tablet 0))
       tlabels >tp dlabels >dp nlabels >np
       tw (o-walk be <tp tlabels wkset 1 rset 0 () ())
       dw (o-walk be <dp dlabels wkset 0 rset 0 () ())
       nw (o-walk be <np nlabels wkset 0 rset 0 () ())
       tbytes <tw trel <>tw
       dbytes <dw drel <>dw
       nbytes <nw nrel <>nw
       exts (o-exts (trel + drel + nrel) tlabels dlabels nlabels)

       ; -- symbol indices: NULL, .text/.data/ai_nifs section syms, then funcs, globs, exts --
       nfun (tally funcnames) nglob (tally globnames)
       fstart 4 gstart (4 + nfun) estart ((4 + nfun) + nglob)
       extidx (o-idx exts estart)
       fnidx (o-idx (map intern funcnames) fstart)   ; interned fn name -> its symbol index (weak-target relocs bind here)

       ; -- .strtab: every symbol's name (funcs, globs, exts as spellings) --
       symnames (funcnames + globnames + (map (\ s (string s)) exts))
       str (o-strtab symnames)
       strblob <str stroffs <>str

       ; -- the symbol table: locals (NULL + 2 section syms) then the globals --
       ; (one catall per table -- a cat per entry re-copies it)
       symbytes
        (: fsyms (catall (o-mapi (\ i fs (o-sym (at stroffs i)
                                             (? (member? fs wknames) 2 1)   ; STB_WEAK: a strong def elsewhere wins
                                             2 1 (peep tlabels (intern fs) 0)))
                                 funcnames))
           gsyms (catall (o-mapi (\ i gs (o-sym (at stroffs (nfun + i)) 1 1 2 (peep dlabels (intern gs) 0)))
                                 globnames))
           esyms (catall (o-mapi (\ i e (o-sym (at stroffs ((nfun + nglob) + i)) 1 0 0 0))
                                 exts))
           (catall `((o-sym 0 0 0 0 0)                 ; the NULL symbol
                      (o-sym 0 0 3 1 0)                  ; .text section symbol (STT_SECTION)
                      (o-sym 0 0 3 2 0)                  ; .data section symbol
                      (o-sym 0 0 3 3 0)                  ; ai_nifs section symbol
                      fsyms gsyms esyms)))
       nsym (4 + nfun + nglob + (tally exts))

       ; -- the relocations. x64 text: an la into .data -> PC32 via the .data
       ; section symbol (idx 2, addend = data offset - 4); a call to an external
       ; -> PLT32. aarch64 text: a b26 that survived the walk is weak or external
       ; (CALL26/JUMP26 by its opcode, addend 0); an adrp/add pair relocates by
       ; kind (PG_HI21 / ABS_LO12_NC) via the target's section symbol + offset --
       ; including .text targets (a page delta never resolves inside a .o).
       ; data: an abs64 pointer -> the abs64 type via the target's section
       ; symbol + offset, or an external symbol. --
       ; a label -> (symidx base-addend): a weak fn binds its OWN symbol (base 0, so the
       ; linker can redirect), a section-bound label its section symbol + offset, anything
       ; else its UNDEF. the reloc emitters compose their own addend on the base.
       (secsym lbl)
        (? (o-has? wkset lbl)   `((peep fnidx lbl 0) 0)
           (o-has? tlabels lbl) `(1 (peep tlabels lbl 0))
           (o-has? dlabels lbl) `(2 (peep dlabels lbl 0))
           (o-has? nlabels lbl) `(3 (peep nlabels lbl 0))
           `((peep extidx lbl 0) 0))
       reltext
        (? !a64
           (catall (map (\ r
              (: off <r lbl <>>r pv <>>>>r
                 (? (o-has? wkset lbl)                ; a weak-defined fn: bind its OWN symbol (overridable)
                     (o-rela off (peep fnidx lbl 0) r-plt32 -4)
                    (o-has? dlabels lbl)
                     (o-rela off 2 r-pc32 ((peep dlabels lbl 0) - 4))
                    (o-has? nlabels lbl)
                     (o-rela off 3 r-pc32 ((peep nlabels lbl 0) - 4))
                    ; an external ref: a call/jmp (E8/E9 precedes the disp field) -> PLT32;
                    ; an la/lea address-of (its modrm precedes) -> PC32 -- external DATA
                    ; like `stdout` needs the data reloc, not a PLT stub (the -c crash).
                    (o-rela off (peep extidx lbl 0)
                            (? (member? pv '(232 233)) r-plt32 r-pc32) -4))))
              trel))
           (catall (map (\ r
              (: off <r k <>r lbl <>>r aux <>>>r
                 (? (member? k '(adrp21 addlo12))
                     (: sl (secsym lbl)
                        (o-rela off <sl (? (id? k 'adrp21) ra-pghi21 ra-lo12) <>sl))
                    (id? k 'b26)
                     (: ty (? (aux = 2483027968) ra-call26 ra-jump26)   ; 0x94000000 = bl
                        (? (o-has? wkset lbl) (o-rela off (peep fnidx lbl 0) ty 0)
                           (o-rela off (peep extidx lbl 0) ty 0)))
                    (scare 'obj-a64-fix k))))
              trel)))
       ; an abs64 pointer out of a DATA-class section (.data or ai_nifs): via the
       ; target's section symbol + its offset, or an external's own symbol
       rabs (? a64 ra-abs64 r-64)
       (reldat1 rs)
        (catall (map (\ r
           (: off <r lbl <>>r ad <>>>r               ; ad = the constant byte offset (&arr[i])
              sl (secsym lbl)
              (o-rela off <sl rabs (<>sl + ad))))
           rs))
       reldata (reldat1 drel)
       relnif  (reldat1 nrel)
       ntrel (tally trel) ndrel (tally drel) nnrel (tally nrel)

       ; -- the section-name string table --
       shnames `(".text" ".data" "ai_nifs" ".symtab" ".strtab" ".rela.text" ".rela.data" ".rela.ai_nifs" ".shstrtab")
       shstr (o-strtab shnames)
       shstrblob <shstr shoffs <>shstr
       (shname i) (at shoffs i)

       ; -- file layout: header, then section contents packed tight, then the SHT.
       ; the linker reads each section by (sh_offset, sh_size), so no in-file
       ; alignment padding is needed; sh_addralign drives final placement. --
       h-sz 64
       t-off h-sz               t-sz (tally tbytes)
       d-off (t-off + t-sz)     d-sz (tally dbytes)
       n-off (d-off + d-sz)     n-sz (tally nbytes)
       sym-off (n-off + n-sz)   sym-sz (nsym * 24)
       str-off (sym-off + sym-sz) str-sz (tally strblob)
       rt-off (str-off + str-sz)  rt-sz (ntrel * 24)
       rd-off (rt-off + rt-sz)    rd-sz (ndrel * 24)
       rn-off (rd-off + rd-sz)    rn-sz (nnrel * 24)
       shs-off (rn-off + rn-sz)   shs-sz (tally shstrblob)
       sht-off (shs-off + shs-sz)

       ; a 64-byte section header
       (sh nameoff typ flags off sz link info align entsz)
        (catall `((le32 nameoff) (le32 typ) (le64 flags) (le64 0)
                   (le64 off) (le64 sz) (le32 link) (le32 info) (le64 align) (le64 entsz)))
       sht (catall `(
              (o-zeros 64)                                                 ; 0 NULL
              (sh (shname 0) 1 6 t-off t-sz 0 0 16 0)                      ; 1 .text  PROGBITS ALLOC|EXEC
              (sh (shname 1) 1 3 d-off d-sz 0 0 8 0)                       ; 2 .data  PROGBITS ALLOC|WRITE
              (sh (shname 2) 1 3 n-off n-sz 0 0 8 0)                       ; 3 ai_nifs PROGBITS ALLOC|WRITE (__start_/__stop_ bracket it)
              (sh (shname 3) 2 0 sym-off sym-sz 5 4 8 24)                  ; 4 .symtab link=.strtab info=first-global(4)
              (sh (shname 4) 3 0 str-off str-sz 0 0 1 0)                   ; 5 .strtab
              (sh (shname 5) 4 0 rt-off rt-sz 4 1 8 24)                    ; 6 .rela.text link=.symtab info=.text
              (sh (shname 6) 4 0 rd-off rd-sz 4 2 8 24)                    ; 7 .rela.data link=.symtab info=.data
              (sh (shname 7) 4 0 rn-off rn-sz 4 3 8 24)                    ; 8 .rela.ai_nifs link=.symtab info=ai_nifs
              (sh (shname 8) 3 0 shs-off shs-sz 0 0 1 0)))                 ; 9 .shstrtab

       hdr (catall `(
              `(127 69 76 70 2 1 1 0 0 0 0 0 0 0 0 0)                     ; e_ident: \x7fELF 64-bit LSB
              (le16 1) (le16 machine) (le32 1)                            ; ET_REL, e_machine, version
              (le64 0) (le64 0) (le64 sht-off)                            ; e_entry=0, e_phoff=0, e_shoff
              (le32 0) (le16 64) (le16 0)                                 ; e_flags, e_ehsize, e_phentsize=0
              (le16 0) (le16 64) (le16 10)                                ; e_phnum=0, e_shentsize=64, e_shnum=10
              (le16 9)))                                                  ; e_shstrndx=9
       (catall `(hdr tbytes dbytes nbytes symbytes strblob reltext reldata relnif shstrblob sht)))))
