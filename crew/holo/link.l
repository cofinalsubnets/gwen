; crew/holo/link.l -- a static ELF LINKER over obj.l's relocatable objects (the
; toolchain ladder, rung 3): read each .o (the section shape obj.l lays --
; .text / .data / ai_nifs + symtab + RELA), resolve every symbol across the set
; (strong over weak; __start_/__stop_ai_nifs synthesized over the packed nif
; region), lay the sections at the classic ET_EXEC base, apply the relocations
; (R_64 an absolute S+A; PC32/PLT32 both the pc-relative S+A-P once the link is
; static), and wrap the bytes in elf.l's one-segment executable header. entry
; is the FIRST object's text -- the driver leads with a crt0 object built by
; objelf itself, so `main` resolves through the same reloc path as everything
; else. it eats a FOREIGN .o too (gcc's, the differential oracle): .bss rides
; the NOBITS lane, and the sections a static image cannot use are dropped on a
; STATED RULE, never on a hunch -- see `class`. anything else with bytes still
; scares off honestly, never drops silently. the targets are obj.l's
; (x86-64 field relocs REPLACE their field; aarch64 and riscv64 ones OR bits
; into the instruction word(s) the placeholder carries).
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
   ; a section NAME -> its lane, by prefix: gcc splits a lane across many
   ; sections (.text.unlikely, .rodata.str1.1, .data.rel.ro.local) and every
   ; piece of a lane lands in the same run of the image. one RWX segment maps
   ; the whole thing, so .rodata and .data.rel.ro are data that nothing writes.
   ; ai_nifs is ours and exact -- it is a packed array with a bracket, not a lane.
   (ld-pre nm p) (: n (tally p) (&& ((tally nm) >= n) ((snip nm 0 n) = p)))
   (ld-lane nm ty)
    (? (ty = 8) (? (|| (nm = ".bss") (ld-pre nm ".bss.")) 'bss ())
       (|| (nm = ".text") (ld-pre nm ".text.")) 'text
       (|| (nm = ".rodata") (ld-pre nm ".rodata.")) 'rodata
       (|| (nm = ".data") (ld-pre nm ".data.")) 'data
       (nm = "ai_nifs") 'nifs ())

   ; parse one relocatable object (id names it in scares) -> a tablet
   ;   'id 'src            the name and the raw bytes
   ;   'secs               section index -> (lane off size align), the placed ones
   ;   'lanes              lane -> its section indices, ASCENDING (header order)
   ;   'drops              section index -> 1, the ones ruled out (see `class`)
   ;   'syms / 'nsym       symbol index -> (name bind typ shndx value size)
   ;   'relas              ((secidx (off kind symidx addend)..) ..)
   ; layout pins 'base -- section index -> load address -- on it later. keying
   ; the bases by INDEX, not by lane, is what lets a lane hold more than one
   ; section: a symbol reads its own section's base straight off its shndx.
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
       secs (tablet 0) _ (pin ob 'secs secs)
       lanes (tablet 0) _ (pin ob 'lanes lanes)
       drops (tablet 0) _ (pin ob 'drops drops)
       _ (pin ob 'base (tablet 0))
       ; DROPPABLE: a section this image cannot use, ruled out by what it IS,
       ; never by a hunch. empty (nothing to place); no SHF_ALLOC, so the loader
       ; never maps it at all (gcc's .comment, .debug_*, .note.GNU-stack); a NOTE,
       ; which is advisory metadata (.note.gnu.property carries ISA feature bits
       ; a static exe of ours consults nowhere); or .eh_frame, the ONE alloc
       ; section dropped by name -- it is the unwinder's table, and a link with
       ; no unwinder and no exceptions never reads a byte of it. its .rela goes
       ; with it (see the rela walk). the index is recorded so a relocation
       ; POINTING AT a dropped section can be skipped rather than scared at.
       (drop? i ty fl nm sz)
        (? (|| (sz = 0) (|| (0 = (fl & 2)) (|| (ty = 7) (nm = ".eh_frame"))))
           (: _ (pin drops i 1) 1) ())
       ; classify: PROGBITS and NOBITS onto their lane by name, symtab/rela by
       ; type, the droppable set by the rule above; anything ELSE with bytes is a
       ; section we have no story for -- refuse, never drop silently. SHF_TLS
       ; (.tdata/.tbss) is exactly that: a thread-local image needs a PT_TLS the
       ; one segment here cannot express, and letting it ride the data lane would
       ; give every thread the SAME variable -- a wrong answer, quietly.
       (class i symt rls)
        (? (shnum <= i) (symt >< rls)
           (: h (sh i) ty (rle32 s (h + 4)) fl (rle64 s (h + 8))
              off (rle64 s (h + 24)) sz (rle64 s (h + 32)) nm (sname i)
              _ (? (&& (0 < (fl & 1024)) (0 < sz)) (scare 'link-tls nm) 0)
              (? (member? ty '(1 8))
                  ; SHT_NOBITS (8) is the bss lane: it OWNS NO FILE BYTES -- its
                  ; sh_offset is a placeholder -- so it carries only size and
                  ; alignment, and the image gives it address space PAST the last
                  ; byte written (p_memsz over p_filesz, the kernel zeroing the
                  ; difference). that is the whole meaning of .bss.
                  (: lane (ld-lane nm ty)
                     (? lane (: _ (pin secs i `(lane (? (ty = 8) 0 off) sz (rle64 s (h + 48))))
                                _ (pin lanes lane (i >< (peep lanes lane ())))
                                (class (i + 1) symt rls))
                        (drop? i ty fl nm sz) (class (i + 1) symt rls)
                        (scare 'link-section nm)))
                 (ty = 2) (class (i + 1) `(off sz (rle32 s (h + 40))) rls)
                 (ty = 4) (class (i + 1) symt (`((rle32 s (h + 44)) off sz) >< rls))
                 (member? ty '(0 3)) (class (i + 1) symt rls)
                 (drop? i ty fl nm sz) (class (i + 1) symt rls)
                 (scare 'link-section nm))))
       cl (class 1 () ())
       ; the lane lists came out reversed (the walk conses); put each back in
       ; header order, so a lane's pieces lay in the order the object wrote them
       _ (foldl (\ _ l (pin lanes l (rev (peep lanes l ()))))
                0 '(text rodata data nifs bss))
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
                                          (rle16 s (e + 6)) (rle64 s (e + 8))
                                          (rle64 s (e + 16))))
                         (go (i + 1))))
            (go 0))
       ; a .rela whose TARGET was dropped is dropped with it (gcc's
       ; .rela.eh_frame is the whole of this case) -- but a target that is
       ; neither placed nor dropped is a section we never saw, and scares.
       relas (map (\ r (: n ((at r 2) // 24)
                          (es i acc) (? (i < 0) acc
                                        (: e ((at r 1) + (i * 24))
                                           (es (i - 1)
                                               (`((rle64 s e) (rle32 s (e + 8))
                                                        (rle32 s (e + 12)) (sg64 (rle64 s (e + 16)))) >< acc))))
                          (<r >< (es (n - 1) ()))))
                  (filter (\ r (? (two? (peep secs <r ())) 1
                                  (peep drops <r ()) ()
                                  (scare 'link-rela-target id)))
                          >cl))
       _ (pin ob 'relas relas)
       ob)

   ; lay ONE lane across the set in object order -- and, within an object, its
   ; sections in header order -- each aligned as its own header asks. pins the
   ; base per SECTION INDEX; answers the address just past the last.
   (ldlay1 lane cur os)
    (? !(two? os) cur
       (: ob <os bs (peep ob 'base 0)
          (go c is) (? !(two? is) c
                       (: se (peep (peep ob 'secs 0) <is ())
                          a (ld-align (at se 3) c)
                          _ (pin bs <is a)
                          (go (a + (at se 2)) >is)))
          (ldlay1 lane (go cur (peep (peep ob 'lanes 0) lane ())) >os)))

   ; assign every section its load address: header first, then all text (each
   ; aligned as its header asks), rodata, data, then the nif region -- 16-aligned
   ; once and packed tight, so the __start_/__stop_ bracket reads it as ONE
   ; array across every object. the bss lane is NOT here: it owns no file bytes,
   ; so it is laid LAST, past everything written (ldlink, once the -pie table's
   ; size is known). pins the bases; -> (end nifstart).
   (ldlay objs base)
    (: t1 (ldlay1 'text (base + 120) objs)
       o1 (ldlay1 'rodata t1 objs)
       d1 (ldlay1 'data o1 objs)
       n0 (ld-align 16 d1)
       `((ldlay1 'nifs n0 objs) n0))

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
                    _ (? (two? (peep (peep ob 'secs 0) shx ())) 0
                         (scare 'link-sym-section nm))
                    a ((peep (peep ob 'base 0) shx 0) + (at sy 4))
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
   ; rv flags an EM_RISCV link: riscv's reloc numbers live in their own namespace
   ; and R_RISCV_64 is 2 -- the same number as R_X86_64_PC32 -- so the kind
   ; dispatch below switches on the machine, never on the number alone.
   (ldpatch ob ix base glo n0 n1 r0 r1 relbox rv)
    (: s (peep ob 'src 0) se (peep (peep ob 'secs 0) ix ())
       off0 (at se 1) sz (at se 2)
       (ent es) (? !(two? es) ()
                   (<<es = ix) ><es
                   (ent >es))
       (go pos es acc)
        (? !(two? es) (rev ((snip s pos (off0 + sz)) >< acc))
           (: e <es ro <e kind (at e 1) ad (at e 3)
              _ (? ((pos - off0) <= ro) 0 (scare 'link-unsorted (peep ob 'id 0)))
              sy (peep (peep ob 'syms 0) (at e 2) ())
              sv (? ((at sy 2) = 3)                 ; STT_SECTION: the section's base
                    (: _ (? (two? (peep (peep ob 'secs 0) (at sy 3) ())) 0
                            (scare 'link-sym-section (peep ob 'id 0)))
                       (peep (peep ob 'base 0) (at sy 3) 0))
                    ; a WEAK UNDEFINED symbol resolves to ZERO -- ELF says so, and
                    ; C leans on it: musl's __init_tls tests `if (_DYNAMIC)` to
                    ; tell a static link from a dynamic one, and a static link is
                    ; exactly the one where nothing defines it. only an unresolved
                    ; STRONG reference is an error.
                    (&& ((at sy 1) = 2) (&& ((at sy 3) = 0)
                         (! (two? (peep glo <sy ()))))) 0
                    (ldres glo n0 n1 r0 r1 <sy))
              abs64? (? rv (kind = 2) (member? kind '(1 257)))   ; R_RISCV_64 / R_X86_64_64 / R_AARCH64_ABS64
              ; R_X86_64_32 / _32S: S + A written ABSOLUTE into a 4-byte field.
              ; gcc emits these under -fno-pie (`mov $sym, %eax`) where we always
              ; emit the 64-bit or pc-relative form, so they arrive only from a
              ; foreign object. 32 is unsigned, 32S signed -- a truncated address
              ; would point somewhere real and wrong, so each checks its own range.
              abs32? (&& !rv (member? kind '(10 11)))
              w (? abs64? 8 (&& rv (kind = 19)) 8 4)             ; CALL_PLT spans the auipc pair
              ; -pie: this abs64 site holds a base-0 offset (base is 0), so record its
              ; load-address for the startup relocator to add the real base to.
              _ (? (&& relbox abs64?)
                    (pin relbox 0 ((base + ro) >< (peep relbox 0 ()))) 0)
              ; ..and an abs32 site cannot be slid at all: the startup relocator
              ; adds a 64-bit base, and a 32-bit field has nowhere to put it.
              _ (? (&& relbox abs32?) (scare 'link-abs32-pie <sy) 0)
              _ (? (? rv (member? kind '(2 17 19)) (member? kind '(1 2 4 10 11 257 275 277 282 283)))
                   0 (scare 'link-reloc kind))
              _ (? ((ro + w) <= sz) 0 (scare 'link-oob (peep ob 'id 0)))
              ; the aarch64/riscv field kinds patch BITS of the instruction word the
              ; placeholder carries -- read it back and OR the field in
              wd (? abs64? 0 (rle32 s (off0 + ro)))
              bs (? abs64? (le64 (sv + ad))                      ; the abs64 three: S + A outright
                    abs32?                                       ; ..and the two abs32, range-checked
                     (: v (sv + ad)
                        _ (? (? (kind = 10) (&& (0 <= v) (v <= 4294967295)) (ld-fits32 v))
                             0 (scare 'link-range <sy))
                        (le32 v))
                    (&& rv (kind = 17))                          ; R_RISCV_JAL: S+A-P into jal's j-imm
                     (: v ((sv + ad) - (base + ro))
                        _ (? (&& (-1048576 <= v) (v <= 1048575)) 0 (scare 'link-range <sy))
                        u (v & 2097151)
                        (le32 (wd | ((((u >> 20) & 1) << 31) | ((((u >> 1) & 1023) << 21)
                                     | ((((u >> 11) & 1) << 20) | (((u >> 12) & 255) << 12)))))))
                    (&& rv (kind = 19))                          ; R_RISCV_CALL_PLT: S+A-P as hi20 + signed lo12
                     (: v ((sv + ad) - (base + ro))
                        low (v & 4095) lo (? (low > 2047) (low - 4096) low)
                        hi ((v - lo) // 4096)
                        _ (? (&& (-524288 <= hi) (hi <= 524287)) 0 (scare 'link-range <sy))
                        ((le32 (wd | ((hi & 1048575) << 12)))
                         + (le32 ((rle32 s (off0 + (ro + 4))) | ((lo & 4095) << 20)))))
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

   ; a 24-byte Elf64_Sym and a 64-byte Elf64_Shdr. the symbol is obj.l's o-sym
   ; with st_size carried instead of zeroed: a foreign object reports real
   ; function extents, and they are worth keeping once we have somewhere to put them.
   (ld-sym nameoff bind typ shndx value size)
    (catall `((le32 nameoff) `(((bind << 4) | typ) 0) (le16 shndx) (le64 value) (le64 size)))
   (ld-shdr nm ty fl ad off sz lk inf al es)
    (catall `((le32 nm) (le32 ty) (le64 fl) (le64 ad) (le64 off) (le64 sz)
              (le32 lk) (le32 inf) (le64 al) (le64 es)))

   ; THE EXECUTABLE'S SYMBOL TABLE -- what makes `nm`, a labeled `objdump -d` and
   ; a gdb `break main` work on a binary of ours. every DEFINED symbol across the
   ; set, read back out of the objects that carried it: locals FIRST (ELF requires
   ; that, and .symtab's sh_info names the first global), then the globals that WON
   ; the link -- a weak definition a strong one replaced is skipped, because glo
   ; already holds the winner and two entries for one name at two addresses would
   ; be a lie. st_size rides through from the object: ours say 0 (holo lays labels,
   ; not extents), and a size-0 symbol is bounded by the next one, which is what
   ; every reader already does for a stripped-but-symboled binary.
   ; lanesx maps a lane to its section index in the image. -> (bytes strs nlocal)
   (ldstab objs glo lanesx)
    (: (one ob acc)
        (: bs (peep ob 'base 0) secs (peep ob 'secs 0) syms (peep ob 'syms 0)
           (go i a)
            (? ((peep ob 'nsym 0) <= i) a
               (: sy (peep syms i ())
                  nm <sy bind (at sy 1) typ (at sy 2) shx (at sy 3)
                  se (peep secs shx ())
                  ad ((peep bs shx 0) + (at sy 4))
                  won (: r (peep glo nm ()) (? (two? r) (ad = <r) 0))
                  (go (i + 1)
                      (? (|| !(two? se) (|| (0 = (tally nm))
                              (|| !(member? typ '(0 1 2)) !(member? bind '(0 1 2)))))
                          a                             ; undefined, unnamed, a FILE/SECTION marker, or dropped
                         (&& (0 < bind) !won) a         ; a weak def that a strong one replaced
                         (`(nm bind typ (peep lanesx <se 0) ad (at sy 5)) >< a)))))
           (go 1 acc))
       all (foldl (\ a ob (one ob a)) () objs)
       locs (filter (\ e ((at e 1) = 0)) all)
       ord (locs + (filter (\ e (0 < (at e 1))) all))
       str (o-strtab (map (\ e <e) ord))
       ; index 0 is the reserved NULL entry, and it counts as a local
       `(((ld-zeros 24) + (catall (o-mapi (\ i e (ld-sym (at <>str i) (at e 1) (at e 2)
                                                         (at e 3) (at e 4) (at e 5)))
                                          ord)))
         <str (1 + (tally locs))))

   ; count the abs64 (R_X86_64_64 / R_AARCH64_ABS64) data relocations across the
   ; set -- the entry count of the -pie relocation table (8 bytes each).
   (ld-nabs objs rv)
    (foldl (\ a ob (foldl (\ a2 tr
              (foldl (\ a3 e (? (? rv ((at e 1) = 2) (member? (at e 1) '(1 257))) (a3 + 1) a3)) a2 >tr))
              a (peep ob 'relas ())))
       0 objs)

   ; link a set of objects -- ((id bytes-text)..) -- into a static-exe PIECE
   ; list (texts + byte lists; ld-write lays them down). entry = the first
   ; object's text. one R+X+W segment maps the whole file, elf.l's shape.
   ; pie: emit ET_DYN laid at base 0 (the kernel biases it to a random high
   ; address), collect every abs64 site into a trailing offset table bracketed
   ; by __start_/__stop_ai_rela, and let nolibc's startup add the load base to
   ; each -- the classic static-PIE self-relocation, our own toolchain end to end.
   ; org: () takes the default base (pie 0 / elf-base); a number lays the image
   ; THERE -- the bare-metal lane (qemu -M virt wants its RAM base, 0x80000000),
   ; where `qemu -kernel` loads the segment at p_vaddr and enters at e_entry.
   (ldlink target pie org srcs)
    (: mach (peep elf-machines target 0)
       _ (? (0 < mach) 0 (scare 'link-target target))
       objs (map (\ p (ld-read <p <>p mach)) srcs)
       rv (mach = 243)                          ; EM_RISCV: its own reloc-number namespace (see ldpatch)
       base (? (charm? org) org pie 0 elf-base)
       lay (ldlay objs base)
       end <lay n0 <>lay
       nrel (? pie (ld-nabs objs rv) 0)
       r0 (? pie (ld-align 16 end) end)         ; the reloc table starts 16-aligned, just past the nifs
       r1 (r0 + (8 * nrel))
       ; .bss last, past every byte the file carries: the segment's p_memsz runs
       ; out to `bend` where its p_filesz stops at r1, and the kernel hands over
       ; the difference already zeroed -- which is the whole meaning of .bss.
       bend (ldlay1 'bss r1 objs)
       relbox (? pie (: t (tablet 0) _ (pin t 0 ()) t) ())    ; abs64 site accumulator (pie only)
       glo (ldsyms objs)
       ; the ENTRY: ld's rule -- the symbol `_start` when the set defines one,
       ; else the first object's text (mooncc's own crt0 lane, which names its
       ; entry __ai_start and relies on being first). a foreign libc brings its
       ; own _start and must be entered THROUGH it: musl's sets up the stack
       ; image, tls and stdio before main ever runs.
       ent (: e (peep glo "_start" ())
              o1 <objs
              (? (two? e) <e
                 (peep (peep o1 'base 0) <(peep (peep o1 'lanes 0) 'text ()) 0)))
       ; emit in layout order -- lane by lane, object by object, and within an
       ; object its sections in header order -- zero-padding up to each base, so
       ; the file offsets land exactly where ldlay said the addresses would.
       (emit lane cur os acc)
        (? !(two? os) (cur >< acc)
           (: ob <os
              (go c ac is)
               (? !(two? is) (c >< ac)
                  (: se (peep (peep ob 'secs 0) <is ())
                     b (peep (peep ob 'base 0) <is 0)
                     pad (? (c < b) `((ld-zeros (b - c))) ())
                     (go (b + (at se 2))
                         (ac + pad + (ldpatch ob <is b glo n0 end r0 r1 relbox rv))
                         >is)))
              r (go cur acc (peep (peep ob 'lanes 0) lane ()))
              (emit lane <r >os >r)))
       et (emit 'text (base + 120) objs ())
       eo (emit 'rodata <et objs >et)
       ed (emit 'data <eo objs >eo)
       en (emit 'nifs <ed objs >ed)
       tail (? (<en < r0) `((ld-zeros (r0 - <en))) ())      ; nif tail + table alignment in one pad
       rtab (? pie (catall (map (\ o (le64 o)) (rev (peep relbox 0 ())))) ())
       ; -- the section headers and the symbol table, laid PAST p_filesz. nothing
       ; here is SHF_ALLOC, so the kernel maps not a byte of it and the program is
       ; the same program; nm, objdump and gdb read exactly this. we describe the
       ; image as the five lanes we laid it in, so a symbol lands in the section
       ; it actually lives in and `nm` classifies it T/R/D/B without guessing.
       fsz (r1 - base)                                      ; the loaded bytes: p_filesz
       lanesx (tabof `(('text >< 1) ('rodata >< 2) ('data >< 3) ('nifs >< 4) ('bss >< 5)))
       st (ldstab objs glo lanesx)
       shstr (o-strtab '(".text" ".rodata" ".data" "ai_nifs" ".bss"
                         ".symtab" ".strtab" ".shstrtab"))
       (snm k) (at <>shstr k)
       symo (ld-align 8 fsz) symz (tally <st)
       stro (symo + symz) strz (tally <>st)
       ssto (stro + strz) sstz (tally <shstr)
       shto (ld-align 8 (ssto + sstz))
       sht (catall `((ld-shdr 0 0 0 0 0 0 0 0 0 0)                                   ; the reserved null header
                     (ld-shdr (snm 0) 1 6 (base + 120) 120 (<et - (base + 120)) 0 0 16 0)
                     (ld-shdr (snm 1) 1 2 <et (<et - base) (<eo - <et) 0 0 16 0)
                     (ld-shdr (snm 2) 1 3 <eo (<eo - base) (<ed - <eo) 0 0 16 0)
                     (ld-shdr (snm 3) 1 3 <ed (<ed - base) (<en - <ed) 0 0 16 0)
                     (ld-shdr (snm 4) 8 3 r1 fsz (bend - r1) 0 0 16 0)               ; NOBITS: an offset, no bytes
                     (ld-shdr (snm 5) 2 0 0 symo symz 7 (at st 2) 8 24)              ; link=.strtab info=first global
                     (ld-shdr (snm 6) 3 0 0 stro strz 0 0 1 0)
                     (ld-shdr (snm 7) 3 0 0 ssto sstz 0 0 1 0)))
       hdr (elf64-hdr (? pie 3 2) base ent fsz (bend - base) mach `(shto 9 8))
       (hdr >< (>en + tail + (? pie `(rtab) ())
                + `((ld-zeros (symo - fsz)) <st <>st <shstr
                    (ld-zeros (shto - (ssto + sstz))) sht))))

   ; write a piece list to a file, raw (texts pass through; byte lists pack into a
   ; CASK -- the byte array, not a string that happens to hold bytes today; see
   ; doc/proto/widen.md). an explicit left walk: map runs its effects right-to-left.
   (ld-write path ps)
    (: q (open path "w")
       (go l) (? !(two? l) (close q)
                 (: _ (say q (: p <l (? (string? p) p (cask p))))
                    (go >l)))
       (go ps)))
