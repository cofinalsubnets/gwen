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
; (CALL26/JUMP26, the ADRP/ADD pair, ABS64) -- see the type table and reltext --
; and riscv64 likewise (CALL_PLT over the auipc pair, JAL, R_RISCV_64).
; loads after crew/holo/elf.l in the au cat -- reads elf-machines / write-bytes
; bare, and the assembler internals bare too: the cat leads with (use 'holo)
; (crew/kore/asbook.l), so lay/laylax/patch-with/le*/catall resolve through the
; spliced module. moon.l drives it: `mooncc -c foo.c foo.o`.
(:
   ; the ELF relocation TYPES we emit (x86-64)
   r-64    1                                        ; R_X86_64_64     S + A   (an abs64 data pointer)
   r-pc32  2                                        ; R_X86_64_PC32   S + A - P (an la into .data)
   r-plt32 4                                        ; R_X86_64_PLT32  L + A - P (a call to an external fn)
   r-32    10                                       ; R_X86_64_32     S + A, TRUNCATED to 4 bytes -- the
                                                    ; kernel's PVH note says `.long pvh32 - KVMA`, a
                                                    ; 64-bit address minus the high-half base, and the
                                                    ; wrapped-down sum is the physical entry qemu jumps
                                                    ; to. the linker checks the fit; nothing hosted emits it.
   ; ... and aarch64. field relocs here patch BITS of an instruction word, not a
   ; whole field -- the placeholder bytes carry the base opcode (the fixup's aux).
   ra-abs64  257                                    ; R_AARCH64_ABS64           S + A
   ra-jump26 282                                    ; R_AARCH64_JUMP26          (S+A-P)>>2 into b's imm26
   ra-call26 283                                    ; R_AARCH64_CALL26          ... into bl's imm26
   ra-pghi21 275                                    ; R_AARCH64_ADR_PREL_PG_HI21  page(S+A)-page(P) into adrp
   ra-lo12   277                                    ; R_AARCH64_ADD_ABS_LO12_NC   (S+A) & 0xfff into add
   ; ... and riscv64. the backend's 8-byte 'pcrel (one fixup across the auipc
   ; pair) maps whole onto CALL_PLT: S+A-P split hi20-into-auipc / lo12-into-the-
   ; second-word -- and jalr/addi/ld share the I-form imm field, so the ONE type
   ; serves call and la alike (the placeholder carries both base words).
   rr-64    2                                       ; R_RISCV_64        S + A  (⚠ collides with R_X86_64_PC32: link.l dispatches by machine)
   rr-jal   17                                      ; R_RISCV_JAL       S + A - P into jal's j-imm
   rr-call  19                                      ; R_RISCV_CALL_PLT  S + A - P across the auipc pair

   (o-has? tab k) !(id? (peep tab k 'no) 'no)    ; is key k bound in the tablet?
   (o-strbytes s) (: (go i acc) (? (i < 0) acc (go (i - 1) (s i >< acc)))
                     (go (tally s - 1) ()))         ; a string -> its byte list. this one
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
                 (? ((internal? && member? k rset) &&
                     o-has? labels lbl && !(o-has? wkset lbl))          ; a weak target stays a reloc
                     (: tgt (peep labels lbl 0)
                        bs (patch-with be k tgt off w (fix-a x))
                        (o-walk be >items labels wkset internal? rset (off + w)
                                (rev bs + bacc) racc))
                    (: fill (? (member? k '(b26 b19 adrp21 addlo12 j21)) (le32 (fix-a x))
                               ; a surviving riscv 'pcrel: the aux packs BOTH base words (auipc low,
                               ; the I-insn high) -- lay them as the 8-byte placeholder CALL_PLT patches
                               (id? k 'pcrel) (le32 (fix-a x % 4294967296) + le32 (fix-a x // 4294967296))
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
                               (rev fill + bacc) (`(off k lbl (fix-a x) (? (two? bacc) <bacc 0)) >< racc)))))
             (o-walk be >items labels wkset internal? rset (off + 1) (x >< bacc) racc))))
   (o-zeros n) (? (n < 1) () (0 >< o-zeros (n - 1)))
   ; splice a 4-byte little-endian value into a byte list at OFF (the ARM REL in-place addend: a
   ; pooled abs32 field carries S's section offset there, since REL has no addend column).
   (o-take l n) (? !(two? l) () (n < 1) () (<l >< o-take >l (n - 1)))
   (o-drop l n) (? (n < 1) l !(two? l) l (o-drop >l (n - 1)))
   ; splice pre-encoded bytes (a re-encoded instruction) in at OFF -- the movw16/movt16 addend bake.
   (o-putbs bytes off bs) (catall `((o-take bytes off) bs (o-drop bytes (off + tally bs))))
   (o-put32 bytes off val) (o-putbs bytes off (le32 val))

   ; a LANE is one laid section: (name type flags align bytes relocs labels).
   ; both emitters build the same shape, so the two questions asked of a whole
   ; object -- where is this label defined, and which labels are defined nowhere
   ; -- are asked once here.
   ; o-where: a label -> (section-index offset), the index 1-based (ELF's shndx,
   ; where 0 is UNDEF), or () for a label no lane defines.
   (o-where lanes lbl)
    (: (go ls i) (? !(two? ls) ()
                    (: lb (at <ls 6)
                       (? (o-has? lb lbl) `(i (peep lb lbl 0)) (go >ls (i + 1)))))
       (go lanes 1))
   ; the externals: labels a relocation names that NO lane defines -- each needs
   ; an UNDEF symbol. unique, first-seen order.
   (o-exts lanes rels)
    (: seen (tablet 0)
       (go rs acc)
        (? !(two? rs) acc
           (: lbl (at <rs 2)
              (? (? (o-where lanes lbl) 1 (o-has? seen lbl)) (go >rs acc)
                 (: _ (pin seen lbl 1) (go >rs (lbl >< acc))))))
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
           (: bs (o-strbytes <ns + '(0))
              (go >ns (off + tally bs) (rev bs + bacc) (off >< offs))))
       (go names 1 '(0) ()))

   ; the index kit both emitters lean on: a name-set tablet (interned -> 1), a list
   ; pinned elem -> i0+i (the symbol-index maps), and map-with-index.
   (o-names ns) (: t (tablet 0) (go ws) (? !(two? ws) t (: _ (pin t (intern <ws) 1) (go >ws))) (go ns))
   (o-idx ls i0) (: t (tablet 0) (go es i) (? !(two? es) t (: _ (pin t <es i) (go >es (i + 1)))) (go ls i0))
   (o-mapi f ls) (: (go l i acc) (? !(two? l) (rev acc) (go >l (i + 1) (f i <l >< acc))) (go ls 0 ()))

   ; -- ELF32 / EM_ARM (the thumb backends). the 32-bit cousin of the emitter below,
   ; and the SAME shape: a list of sections, one lay each. what differs is the wire --
   ; a 52-byte header, 16-byte symbols, and REL (not RELA) relocations, since ARM's
   ; ABI keeps the addend IN the field, so a surviving thumb branch carries its base
   ; encoding (o-walk fills it) and the .rel tables need no addend column. --
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
   (objsecs32 target secs funcnames globnames wknames)
    (: machine (peep elf-machines target 0)
       _ (? (machine = 0) (scare 'no-elf-machine target) 0)
       be (peep backends target ())
       rset '(b bcc bl)                              ; thumb internal branches are pc-relative -> resolve in place
       wkset (o-names wknames)
       nsec (tally secs)
       _ (? (0 < nsec) 0 (scare 'obj-no-sections target))
       (exec? s) (0 < (at s 2 & 4))
       ; one section is one LAY -- the executable ones through the relaxer, the rest
       ; through plain lay. see objsecs below for why that split is the only one.
       lanes (map (\ s
                (: fm (at s 4)
                   p (? (exec? s)
                        (: r laylax
                           (? (lit? r) (r fm be 0 (tablet 0) wkset)
                              (scare 'holo-bake-stale 'laylax)))
                        (lay fm be 0 () (tablet 0)))
                   w (o-walk be <p >p wkset (? (exec? s) 1 ()) rset 0 () ())
                   `((at s 0) (at s 1) (at s 2) (at s 3) <w <>w >p)))
              secs)
       (ln i) (at lanes i)
       (ln-ty l) (at l 1)  (ln-bs l) (at l 4)  (ln-rl l) (at l 5)
       (ln-fsz l) (? (ln-ty l = 8) 0 (tally (ln-bs l)))     ; a NOBITS lane owns no FILE bytes
       (sec-exec? si) (exec? (at secs (si - 1)))            ; by 1-based shndx, as o-where answers
       allrel (foldl (\ a l (a + ln-rl l)) () lanes)
       exts (o-exts lanes allrel)
       fnset (o-names funcnames)
       gset (o-names globnames)
       ; a label defined HERE that a relocation names but no symbol of ours covers:
       ; it would have to bind its SECTION symbol, and REL's in-field addend cannot
       ; carry the offset. a movw/movt half reads its field as a whole signed 16-bit
       ; addend, so a target past 32K composes an address missing its middle bits
       ; (the teensy boot's silent banner: main.o's 384K pool pushed every string
       ; offset past 0x60000 and every la'd pointer landed 384K low; the qemu-M7
       ; pc hijack was the same truncation on a static fn, 0x14ab for 0x114ab).
       ; each gets its OWN local symbol instead, addend 0 -- the gcc convention.
       ; abs32 fields stay on the section path where the label is data: a 32-bit
       ; field carries any offset. a .text target never can -- it needs the thumb bit.
       ; -> ((label shndx offset) ..), in relocation order.
       (o-locs pick kinds)
        (: seen (tablet 0)
           (go rs acc)
            (? !(two? rs) acc
               (: r <rs lbl (at r 2)
                  w (? (member? (at r 1) kinds) (o-where lanes lbl) ())
                  (go >rs (? !(two? w) acc
                             !(pick <w lbl) acc
                             (o-has? seen lbl) acc
                             (: _ (pin seen lbl 1) (`(lbl <w <>w) >< acc))))))
           (rev (go allrel ())))
       ; the BRANCH kinds ride here too: a branch used to resolve in place, since
       ; both ends shared .text -- but a fn that named its own section (love.c's
       ; data sentinels) branches ACROSS one, and REL's in-field addend has no room
       ; for the offset any more than movw16's does. without this the fixup binds
       ; symbol 0 and the link answers `link-undef ""`, its cause a section away.
       locfns (o-locs (\ si lbl (sec-exec? si && !(o-has? fnset lbl)))
                      '(movw16 movt16 abs32 b bcc bl))
       locdat (o-locs (\ si lbl (!(sec-exec? si) && !(o-has? gset lbl)))
                      '(movw16 movt16))
       locs (locfns + locdat)
       ; -- symbol indices: NULL, one section symbol per section, then those LOCALs
       ; (ELF wants every local ahead of the first global -- .symtab's sh_info points
       ; there), then funcs, globs, exts --
       nloc (tally locs) nfun (tally funcnames) nglob (tally globnames)
       fstart ((1 + nsec) + nloc) gstart (fstart + nfun) estart (gstart + nglob)
       locidx (o-idx (map (\ e <e) locs) (1 + nsec))
       extidx (o-idx exts estart)
       fnidx (o-idx (map intern funcnames) fstart)
       globidx (o-idx (map intern globnames) gstart)
       ; an abs32 target -> (symidx field). a defined global / a function address binds
       ; its OWN symbol (the addend rides the field, usually 0); a label with a local
       ; symbol above binds that; anything else defined here binds its SECTION symbol
       ; with its offset in the field (REL has no addend column), and the rest is external.
       (a32loc lbl aux)
        (? (o-has? globidx lbl) `((peep globidx lbl 0) aux)
           (o-has? fnidx lbl)   `((peep fnidx lbl 0) aux)
           (o-has? locidx lbl)  `((peep locidx lbl 0) aux)
           (: w (o-where lanes lbl)
              (? (two? w) `(<w (<>w + aux)) `((peep extidx lbl 0) aux))))
       ; a movw16/movt16 (thumb2 la) target -> (symidx addend). the shape of a32loc, but
       ; the fixup's aux is Rd (not an addend), so the addend starts at 0.
       (mwloc lbl) (a32loc lbl 0)
       ; bake each abs32 field (its addend + any section-relative offset) into the section
       ; bytes, since REL has no addend column..
       (bakeabs bytes rs)
        (: (go rs bs) (? !(two? rs) bs
                         (: r <rs
                            (go >rs (? (id? <>r 'abs32) (: sl (a32loc <>>r <>>>r) (o-put32 bs <r <>sl)) bs))))
           (go rs bytes))
       ; ..and the same for a section-bound movw16/movt16, whose addend rides the imm16
       ; scatter (patch-with does the scatter; the linker adds S on top).
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
       lbytes (map (\ l (: rs (ln-rl l) (bakemw (bakeabs (ln-bs l) rs) rs))) lanes)
       symnames (map (\ e (string <e)) locs
                 + funcnames + globnames + map (\ s (string s)) exts)
       str (o-strtab symnames)
       strblob <str stroffs <>str
       ; -- the symbol table. a FUNC symbol's value carries the THUMB BIT (bit 0): a
       ; cross-object R_ARM_THM_CALL folds it into T, so the callee is entered in
       ; Thumb state -- vital on M0 (no ARM state at all). data OBJECT syms stay even.
       ; a named symbol's SECTION is simply wherever its bytes went, which is what
       ; lets a global live in a section the writer never heard of -- an rp2040
       ; vector table, a bootloader's scanned-for block. --
       (o-place nm) (: w (o-where lanes (intern nm)) (? (two? w) w '(1 0)))
       symbytes
        (: lsyms (catall (o-mapi (\ i e (: si <>e off <>>e
                                           (? (sec-exec? si)
                                              (o-sym32 (at stroffs i) 0 2 si (off | 1))   ; STB_LOCAL STT_FUNC, +thumb bit
                                              (o-sym32 (at stroffs i) 0 1 si off))))      ; STB_LOCAL STT_OBJECT
                                 locs))
           fsyms (catall (o-mapi (\ i fs (: w (o-place fs)
                                            (o-sym32 (at stroffs (nloc + i))
                                                     (? (member? fs wknames) 2 1)         ; STB_WEAK vs STB_GLOBAL
                                                     2 <w (<>w | 1))))                    ; STT_FUNC, +thumb bit
                                 funcnames))
           gsyms (catall (o-mapi (\ i gs (: w (o-place gs)
                                            (o-sym32 (at stroffs (nloc + (nfun + i)))
                                                     (? (member? gs wknames) 2 1) 1 <w <>w)))
                                 globnames))
           ; a weak UNDEF resolves to zero at static link, where a strong one is an
           ; error -- the binding is the whole difference
           esyms (catall (o-mapi (\ i e (o-sym32 (at stroffs (nloc + ((nfun + nglob) + i)))
                                                 (? (o-has? wkset e) 2 1) 0 0 0))
                                 exts))
           ssyms (catall (o-mapi (\ i l (o-sym32 0 0 3 (i + 1) 0)) lanes))
           (catall `((o-sym32 0 0 0 0 0) ssyms lsyms fsyms gsyms esyms)))   ; the NULL symbol leads
       nsym ((((1 + nsec) + nloc) + nfun) + (nglob + tally exts))
       ; -- the relocations, ONE emitter dispatching on the fixup's KIND (the
       ; text/data split the 64-bit twin dropped was never the real line here either). --
       ; a branch's target as a SYMBOL, never a section+offset: the thumb bit rides
       ; the symbol's own value (lsyms bakes it), and REL has no addend column to
       ; carry an offset in. the locidx rung is the cross-section case -- a static
       ; fn a sectioned caller branches to has no other symbol to bind.
       (fnsym lbl) (? (o-has? wkset lbl) (peep fnidx lbl 0)
                      (o-has? locidx lbl) (peep locidx lbl 0)
                      (o-has? fnidx lbl) (peep fnidx lbl 0)
                      (peep extidx lbl 0))
       (o-rels32 rs)
        (catall (map (\ r
           (: off <r k <>r lbl <>>r aux <>>>r
              (? (id? k 'bl) (o-rel32 off (fnsym lbl) r-thm-call)
                 (id? k 'b)  (o-rel32 off (fnsym lbl) r-thm-jump24)   ; a B.W tail-jump to an external/weak fn
                 (id? k 'abs32) (: sl (a32loc lbl aux) (o-rel32 off <sl r-arm-abs32))
                 (member? k '(movw16 movt16))
                  (: sl (mwloc lbl) (o-rel32 off <sl (? (id? k 'movw16) r-thm-movw r-thm-movt)))
                 (scare 'obj32-reloc k))))
           rs))
       rels (map (\ l (o-rels32 (ln-rl l))) lanes)
       (rcnt i) (tally (ln-rl (ln i)))
       ; -- the section-name string table: the sections in order, .symtab and
       ; .strtab, then one .rel.<name> per section, then .shstrtab --
       shnames (map (\ l (at l 0)) lanes
                + '(".symtab" ".strtab")
                + map (\ l (".rel" + at l 0)) lanes
                + '(".shstrtab"))
       shstr (o-strtab shnames)
       shstrblob <shstr shoffs <>shstr
       (shname i) (at shoffs i)
       ; -- file layout: a 52-byte header, section contents packed tight, then the SHT.
       ; the section-header INDICES: 0 NULL, the content sections, .symtab, .strtab,
       ; one .rel per content section, .shstrtab last. --
       i-sym (1 + nsec) i-str (2 + nsec)
       i-shs ((2 * nsec) + 3) nsh ((2 * nsec) + 4)
       h-sz 52
       (o-offs32 i off acc) (? (nsec <= i) `((rev acc) off)
                               (o-offs32 (i + 1) (off + ln-fsz (ln i)) (off >< acc)))
       co (o-offs32 0 h-sz ())    coffs <co
       sym-off <>co               sym-sz (nsym * 16)
       str-off (sym-off + sym-sz) str-sz (tally strblob)
       (o-roffs32 i off acc) (? (nsec <= i) `((rev acc) off)
                                (o-roffs32 (i + 1) (off + (rcnt i * 8)) (off >< acc)))
       ro (o-roffs32 0 (str-off + str-sz) ())   roffs <ro
       shs-off <>ro               shs-sz (tally shstrblob)
       sht-off (shs-off + shs-sz)
       sht (catall (`((o-zeros 40))                                            ; 0 NULL
              + (o-mapi (\ i l (sh32 (shname i) (ln-ty l) (at l 2)             ; 1..nsec the content sections
                                     (at coffs i) (tally (ln-bs l)) 0 0 (at l 3) 0))
                        lanes)
              + `((sh32 (shname nsec) 2 0 sym-off sym-sz i-str ((1 + nsec) + nloc) 4 16)  ; .symtab link=.strtab info=first-global
                  (sh32 (shname (nsec + 1)) 3 0 str-off str-sz 0 0 1 0))                  ; .strtab
              + (o-mapi (\ i l (sh32 (shname ((nsec + 2) + i)) 9 0                        ; one .rel per section, link=.symtab
                                     (at roffs i) (rcnt i * 8) i-sym (i + 1) 4 8))
                        lanes)
              + `((sh32 (shname ((2 * nsec) + 2)) 3 0 shs-off shs-sz 0 0 1 0))))          ; .shstrtab
       hdr (catall `(
              '(127 69 76 70 1 1 1 0 0 0 0 0 0 0 0 0)                     ; e_ident: \x7fELF 32-bit LSB
              (le16 1) (le16 machine) (le32 1)                            ; ET_REL, EM_ARM, version
              (le32 0) (le32 0) (le32 sht-off)                            ; e_entry=0, e_phoff=0, e_shoff
              (le32 83886080) (le16 52) (le16 0)                          ; e_flags=EABI_VER5, e_ehsize=52, e_phentsize=0
              (le16 0) (le16 40) (le16 nsh)                               ; e_phnum=0, e_shentsize=40, e_shnum
              (le16 i-shs)))                                              ; e_shstrndx
       (catall (`(hdr)
                + o-mapi (\ i l (? (ln-ty l = 8) () (at lbytes i))) lanes       ; a NOBITS lane writes nothing
                + `(symbytes strblob)
                + rels
                + `(shstrblob sht))))

   ; .data's placement grain: 8, or the widest ('align n) the stream asks. an
   ; aligned(N) global (mooncc honoring __attribute__((aligned))) pads WITHIN the
   ; section (lay, org 0), so the header must ask the linker for the section's own
   ; start at that N -- or the inner pad aligns nothing.
   (o-dalign forms)
    (: (go l a2) (? !(two? l) a2
                    (: f <l
                       (go >l (? (two? f && id? <f 'align && (at f 1 > a2)) (at f 1) a2))))
       (go forms 8))

   ; build the whole .o image. textforms/dataforms/nifforms/imgforms are cgen-obj's
   ; split IR -- the two named streams are the ones where the ADDRESS is the point
   ; and a symbol alone cannot say it: nifforms are the AI_NIF registry entries,
   ; laid in their own `ai_nifs` section so the linker's __start_/__stop_ bracket
   ; symbols bound them, and imgforms the `.image` bake slot, which link.l lays LAST
   ; of all so `love bake` can grow it at the tail (host/image.c). funcnames the
   ; DEFINED functions (global FUNC symbols), globnames the global variables (global
   ; OBJECT symbols); locnames the STATIC functions -- LOCAL FUNC symbols, so a
   ; linked binary's nm/objdump/gdb (and tools/vmret.l's symbol spans) see them;
   ; static data stays local labels. -> a byte list.
   ; these five sections are what a HOSTED object carries; objsecs below is the
   ; general door, and the kernel's lays go through it for names a compiler never
   ; emits (.boot, .note.pvh, the 2 KiB-aligned vector table).
   ; the five DESCRIPTORS are their own binding because a compiler has a sixth
   ; kind of stream to add to them: a global wearing __attribute__((section(..)))
   ; with a name that is neither ai_nifs nor .image (the kernel's
   ; .limine_requests, which a bootloader SCANS for). moon.l appends those and
   ; calls objsecs itself; everything else takes the five and nothing more.
   ; ⚠ .bss is NOBITS (8): its size rides the header and its zeros ride NOTHING, so
   ; whoever loads the image owes them -- linux over p_memsz, the .lds ports over
   ; their own startup loop. a lay that owns no loader must not use this lane.
   ; ⚠ .rodata is the one data lane at flags 2 (ALLOC, no WRITE). holo maps it writable
   ; anyway -- one segment -- so the flag is what an EXTERNAL linker reads to give it a
   ; read-only home, and it is a promise the stream has to keep: no relocation, ever.
   (objsecs4 textforms dataforms nifforms imgforms bssforms rodataforms)
    `(`(".text"   1 6 16 textforms)
      `(".rodata" 1 2 (o-dalign rodataforms) rodataforms)
      `(".data"   1 3 (o-dalign dataforms) dataforms)
      `(".bss"    8 3 (o-dalign bssforms) bssforms)
      `("ai_nifs" 1 3 8 nifforms)
      `(".image"  1 3 16 imgforms))
   ; a named section a COMPILER emits: PROGBITS on the widest grain its own globals
   ; ask for, allocated, and writable unless ro says the compiler found every global
   ; in it const and reloc-free. the descriptor a hand-written lay needs is its own
   ; business -- it says the flags outright.
   (objsec-named nm forms ro) `(nm 1 (? ro 2 3) (o-dalign forms) forms)
   ; ..and the CODE twin, for a function that named its own section (love.c's data
   ; sentinels). flags 6 = ALLOC|EXECINSTR, which is also what picks objsecs'
   ; internal-resolve lane; a jump out to .text becomes a relocation, as it must.
   ; the GRAIN is the caller's: a run of one-fn sections at grain N tiles at N, which
   ; is how love.h's ai_typ gets a stride out of a linker that was told no such thing.
   (objsec-code nm forms al) `(nm 1 6 al forms)
   ; the arm32 twin of objsecs4: FOUR, because there is no .image lane at 32 bits
   ; (no bake), so a non-empty one is a caller's mistake and not a section. the grain
   ; is 4 throughout rather than objsecs4's 16/8 -- a thumb function is 2-byte
   ; aligned and flash is the scarce thing on these parts, which is also why .bss
   ; rides here: these ports copy .data out of flash and zero .bss themselves
   ; (rp2040.c, teensy41.c, nucleo446.c), so a zero laid in .data is flash spent twice.
   (objsecs3 target textforms dataforms nifforms imgforms bssforms rodataforms)
    (: _ (? (two? imgforms) (scare 'obj-image-arm32 target) 0)
       `(`(".text"   1 6 4 textforms)
         `(".rodata" 1 2 4 rodataforms)
         `(".data"   1 3 4 dataforms)
         `(".bss"    8 3 4 bssforms)
         `("ai_nifs" 1 3 4 nifforms)))

   (objelf target textforms dataforms funcnames globnames wknames nifforms imgforms locnames bssforms rodataforms)
    (? (arm32? target)
       (objsecs32 target (objsecs3 target textforms dataforms nifforms imgforms bssforms rodataforms)
                  funcnames globnames wknames)          ; locnames unrepresented on arm32
       (objsecs target (objsecs4 textforms dataforms nifforms imgforms bssforms rodataforms)
                funcnames globnames wknames locnames))

   ; -- the general object: a LIST of sections, each (name type flags align forms).
   ; one section is one LAY, which is exactly what a section boundary means: a
   ; branch inside it resolves in place, a reference across it becomes a
   ; relocation. SHF_EXECINSTR (flags & 4) picks the internal-resolve lane and the
   ; relaxer; everything else relocates every fixup it carries. a NOBITS section
   ; (type 8) still lays its forms -- that is where its labels and its SIZE come
   ; from -- but writes no file bytes, which is the whole meaning of .bss.
   (objsecs target secs funcnames globnames wknames locnames)
    (: a64 (member? target '(arm64 aarch64))
       rv (member? target '(riscv64 rv64))
       _ (? (id? target 'x64 || a64 || rv) 0 (scare 'obj-target target))
       ; the in-place-resolvable kinds (translation-invariant). riscv's BOTH label
       ; kinds are pure byte deltas (auipc is byte pc-relative by nature -- no page
       ; math), so intra-.text la/call/jmp all resolve inside the .o.
       rset (? a64 '(b26 b19) rv '(j21 pcrel) '(rel32 rel8))
       machine (peep elf-machines target 0)
       ; ⚠ the backend must be REGISTERED, not merely named: a frontend bakes holo
       ; with the NATIVE backend only, and a cross target joins the cat at runtime
       ; ((use 'holo) then the backend's own text). unregistered, `lay` lowers
       ; nothing and answers an empty stream -- so the object writes out whole,
       ; with a 0-byte .text, and the failure only surfaces as a link or a boot
       ; that goes nowhere. one lit? here turns that into a scare at the source.
       be (: b (peep backends target ())
             (? (lit? b) b (scare 'obj-no-backend target)))
       ; a WEAK-defined function's callers must go through a RELOCATION -- an
       ; in-place resolve would pin them to the local body, and the linker could
       ; never redirect to a strong override. built before lay: the relaxer needs
       ; it too (a shortened branch could never survive as a relocation).
       wkset (o-names wknames)
       nsec (tally secs)
       _ (? (0 < nsec) 0 (scare 'obj-no-sections target))
       (exec? s) (0 < (at s 2 & 4))
       ; the relaxing layout -- obj.l is tree-fresh in the kore/mooncc cats while the holo
       ; book rides the running binary's BAKE, and the build pairs them structurally (the
       ; wake shims exec the sibling love; crew/build.mk). a bake too old to carry laylax
       ; means a hand-mixed script/binary combo: scare LOUD, never lay quietly long.
       ; the relaxer is the EXECUTABLE section's -- shortening a branch is only ever
       ; sound where the branch may resolve internally in the first place.
       ; a LANE is one laid section: (name type flags align bytes relocs labels).
       lanes (map (\ s
                (: fm (at s 4)
                   p (? (exec? s)
                        (: r laylax
                           (? (lit? r) (r fm be 0 (tablet 0) wkset)
                              (scare 'holo-bake-stale 'laylax)))   ; assemble each section at org 0
                        (lay fm be 0 () (tablet 0)))
                   w (o-walk be <p >p wkset (? (exec? s) 1 ()) rset 0 () ())
                   `((at s 0) (at s 1) (at s 2) (at s 3) <w <>w >p)))
              secs)
       (ln i) (at lanes i)
       (ln-ty l) (at l 1)  (ln-bs l) (at l 4)  (ln-rl l) (at l 5)
       (ln-fsz l) (? (ln-ty l = 8) 0 (tally (ln-bs l)))     ; a NOBITS lane owns no FILE bytes
       allrel (foldl (\ a l (a + ln-rl l)) () lanes)
       exts (o-exts lanes allrel)

       ; -- symbol indices: NULL, one section symbol per section, then the
       ; LOCAL statics (ELF wants every local ahead of the first global -- .symtab's
       ; sh_info points there), then funcs, globs, exts --
       nloc (tally locnames) nfun (tally funcnames) nglob (tally globnames)
       fstart ((1 + nsec) + nloc) gstart (fstart + nfun) estart (gstart + nglob)
       extidx (o-idx exts estart)
       fnidx (o-idx (map intern funcnames) fstart)   ; interned fn name -> its symbol index (weak-target relocs bind here)
       gidx (o-idx (map intern globnames) gstart)    ; ..and the DATA twin: a weak GLOBAL (musl's __stdout_used)
                                                     ; is referenced through its own symbol, not fnidx's miss (index 0)

       ; -- .strtab: every symbol's name (statics, funcs, globs, exts as spellings) --
       symnames (locnames + funcnames + globnames + map (\ s (string s)) exts)
       str (o-strtab symnames)
       strblob <str stroffs <>str

       ; -- the symbol table: locals (NULL + the section syms) then the globals --
       ; (one catall per table -- a cat per entry re-copies it)
       ; a named symbol's SECTION is simply wherever its bytes went, found by the
       ; same walk the relocations use -- which is what lets a global live in
       ; .image or .rodata or .bss without the writer knowing those names.
       ; the fallback is unreachable for a DEFINED name (funcnames/globnames/
       ; locnames are the defined sets by construction); it keeps a caller that
       ; names one it never laid pointing at the first section, as before.
       (o-place nm) (: w (o-where lanes (intern nm)) (? (two? w) w '(1 0)))
       symbytes
        (: lsyms (catall (o-mapi (\ i ls (: w (o-place ls)                ; STB_LOCAL FUNC
                                            (o-sym (at stroffs i) 0 2 <w <>w)))
                                 locnames))
           fsyms (catall (o-mapi (\ i fs (: w (o-place fs)
                                            (o-sym (at stroffs (nloc + i))
                                             (? (member? fs wknames) 2 1)   ; STB_WEAK: a strong def elsewhere wins
                                             2 <w <>w)))
                                 funcnames))
           ; a DATA symbol honours wknames too: musl's __stdio_exit.c defines
           ; __stdout_used weakly (aliasing a null dummy) so stdout.c's real one
           ; wins -- a hardcoded strong binding made that pair a link-dup
           gsyms (catall (o-mapi (\ i gs (: w (o-place gs)
                                            (o-sym (at stroffs ((nloc + nfun) + i))
                                                   (? (member? gs wknames) 2 1) 1 <w <>w)))
                                 globnames))
           ; an EXTERNAL symbol honours wknames too: a weak undefined reference
           ; resolves to zero at static link (musl's _DYNAMIC), where a strong
           ; one is an error -- the binding is the whole difference
           esyms (catall (o-mapi (\ i e (o-sym (at stroffs (((nloc + nfun) + nglob) + i))
                                               (? (o-has? wkset e) 2 1) 0 0 0))
                                 exts))
           ; one STT_SECTION symbol per section, in section order -- a relocation
           ; against a label defined here binds ITS section's symbol + offset
           ssyms (catall (o-mapi (\ i l (o-sym 0 0 3 (i + 1) 0)) lanes))
           (catall `((o-sym 0 0 0 0 0) ssyms lsyms fsyms gsyms esyms)))   ; the NULL symbol leads
       nsym ((((1 + nsec) + nloc) + nfun) + (nglob + tally exts))

       ; -- the relocations, ONE emitter dispatching on the fixup's KIND. it used
       ; to be two, split by section (text vs data), and the split was never the
       ; real line: an executable section carries an abs64 too (the kernel's
       ; `movabs $khop` / `ldr x9, =khop`, the hop from the low physical mapping
       ; to the high linked one), and a data section's pointer and a text
       ; section's are the same relocation. the kind says which; the section only
       ; ever decided whether a fixup SURVIVED to here.
       ; a label -> (symidx base-addend): a weak fn binds its OWN symbol (base 0, so the
       ; linker can redirect), a section-bound label its section symbol + offset, anything
       ; else its UNDEF. the emitters compose their own addend on the base.
       (secsym lbl)
        (? (o-has? wkset lbl && o-has? fnidx lbl) `((peep fnidx lbl 0) 0)
           (o-has? wkset lbl && o-has? gidx lbl)  `((peep gidx lbl 0) 0)
           (: w (o-where lanes lbl) (? (two? w) w `((peep extidx lbl 0) 0))))
       rabs (? a64 ra-abs64 rv rr-64 r-64)
       (o-rels rs)
        (catall (map (\ r
           (: off (at r 0) k (at r 1) lbl (at r 2) aux (at r 3) pv (at r 4)
              sl (secsym lbl) si <sl sa <>sl
              (? ; an ABSOLUTE pointer at the label -- aux is the constant byte
                 ; offset a &arr[i] / &s.field baked into the field.
                 (id? k 'abs64) (o-rela off si rabs (sa + aux))
                 ; ..and its 4-byte twin, the PVH note's `.long pvh32 - KVMA`
                 (id? k 'abs32) (o-rela off si r-32 (sa + aux))
                 ; x64: the field is measured from its own END, so every addend
                 ; carries the -4. a weak-defined fn binds its own symbol through
                 ; a PLT32 -- section-relative would nail the call to THIS
                 ; object's body and the linker could never redirect it to a
                 ; strong override. so would a weak DATA symbol (musl's
                 ; __stdout_used: bound to the section, the exit-time flush read
                 ; its own null dummy and silently dropped everything buffered).
                 ; for an external the byte BEFORE the field says which it is --
                 ; E8/E9 (call/jmp) is a control transfer and wants PLT32, a
                 ; lea's modrm an address and wants PC32 (external DATA like
                 ; `stdout` has no PLT stub).
                 (id? k 'rel32)
                  (o-rela off si
                          (? (o-has? wkset lbl && o-has? fnidx lbl) r-plt32
                             (o-where lanes lbl) r-pc32
                             (member? pv '(232 233)) r-plt32
                             r-pc32)
                          ((sa + aux) - 4))
                 ; aarch64: an adrp/add pair relocates by kind (a page delta never
                 ; resolves inside a .o, even for a .text target); a b26 that
                 ; survived the walk is weak, external, or in ANOTHER section here.
                 (id? k 'adrp21) (o-rela off si ra-pghi21 sa)
                 (id? k 'addlo12) (o-rela off si ra-lo12 sa)
                 (id? k 'b26) (o-rela off si (? (aux = 2483027968) ra-call26 ra-jump26) sa)   ; 0x94000000 = bl
                 ; riscv: a 'pcrel pair (call/la, CALL_PLT) or a wide branch's
                 ; 'j21 (JAL), both S+A-P through the same door.
                 (id? k 'pcrel) (o-rela off si rr-call sa)
                 (id? k 'j21) (o-rela off si rr-jal sa)
                 (scare 'obj-fix k))))
           rs))
       relas (map (\ l (o-rels (ln-rl l))) lanes)
       (rcnt i) (tally (ln-rl (ln i)))

       ; -- the section-name string table: the sections in order, .symtab and
       ; .strtab, then one .rela.<name> per section, then .shstrtab --
       shnames (map (\ l (at l 0)) lanes
                + '(".symtab" ".strtab")
                + map (\ l (".rela" + at l 0)) lanes
                + '(".shstrtab"))
       shstr (o-strtab shnames)
       shstrblob <shstr shoffs <>shstr
       (shname i) (at shoffs i)

       ; -- file layout: header, then section contents packed tight, then the SHT.
       ; the linker reads each section by (sh_offset, sh_size), so no in-file
       ; alignment padding is needed; sh_addralign drives final placement. --
       ; the section-header INDICES, one naming per shape: 0 NULL, then the
       ; content sections, .symtab, .strtab, one .rela per content section,
       ; .shstrtab last. with the standard four this is exactly the old layout
       ; (symtab 5, strtab 6, relas 7..10, shstrtab 11, shnum 12).
       i-sym (1 + nsec) i-str (2 + nsec) i-rel0 (3 + nsec)
       i-shs ((2 * nsec) + 3) nsh ((2 * nsec) + 4)
       h-sz 64
       ; the content offsets, in section order; a NOBITS section is skipped over
       ; (it owns no file bytes, so its sh_offset is only a placeholder).
       (o-offs i off acc) (? (nsec <= i) `((rev acc) off)
                             (o-offs (i + 1) (off + ln-fsz (ln i)) (off >< acc)))
       co (o-offs 0 h-sz ())    coffs <co
       sym-off <>co             sym-sz (nsym * 24)
       str-off (sym-off + sym-sz) str-sz (tally strblob)
       (o-roffs i off acc) (? (nsec <= i) `((rev acc) off)
                              (o-roffs (i + 1) (off + (rcnt i * 24)) (off >< acc)))
       ro (o-roffs 0 (str-off + str-sz) ())    roffs <ro
       shs-off <>ro             shs-sz (tally shstrblob)
       sht-off (shs-off + shs-sz)

       ; a 64-byte section header
       (sh nameoff typ flags off sz link info align entsz)
        (catall `((le32 nameoff) (le32 typ) (le64 flags) (le64 0)
                   (le64 off) (le64 sz) (le32 link) (le32 info) (le64 align) (le64 entsz)))
       sht (catall (`((o-zeros 64))                                        ; 0 NULL
              + (o-mapi (\ i l (sh (shname i) (ln-ty l) (at l 2)           ; 1..nsec the content sections
                                   (at coffs i) (tally (ln-bs l)) 0 0 (at l 3) 0))
                        lanes)
              + `((sh (shname nsec) 2 0 sym-off sym-sz i-str ((1 + nsec) + nloc) 8 24)   ; .symtab link=.strtab info=first-global
                  (sh (shname (nsec + 1)) 3 0 str-off str-sz 0 0 1 0))                   ; .strtab
              + (o-mapi (\ i l (sh (shname ((nsec + 2) + i)) 4 0                          ; one .rela per section, link=.symtab
                                   (at roffs i) (rcnt i * 24) i-sym (i + 1) 8 24))
                        lanes)
              + `((sh (shname ((2 * nsec) + 2)) 3 0 shs-off shs-sz 0 0 1 0))))            ; .shstrtab

       hdr (catall `(
              '(127 69 76 70 2 1 1 0 0 0 0 0 0 0 0 0)                     ; e_ident: \x7fELF 64-bit LSB
              (le16 1) (le16 machine) (le32 1)                            ; ET_REL, e_machine, version
              (le64 0) (le64 0) (le64 sht-off)                            ; e_entry=0, e_phoff=0, e_shoff
              (le32 0) (le16 64) (le16 0)                                 ; e_flags, e_ehsize, e_phentsize=0
              (le16 0) (le16 64) (le16 nsh)                               ; e_phnum=0, e_shentsize=64, e_shnum
              (le16 i-shs)))                                              ; e_shstrndx
       (catall (`(hdr)
                + map (\ l (? (ln-ty l = 8) () (ln-bs l))) lanes           ; a NOBITS lane writes nothing
                + `(symbytes strblob)
                + relas
                + `(shstrblob sht)))))
