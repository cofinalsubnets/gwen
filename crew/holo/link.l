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
; `ldkern` is the second door out: the same objects laid for the BARE METAL --
; several page-aligned PT_LOADs, each carrying p_paddr = p_vaddr - bias, so one
; ELF serves a loader that copies physical and a bootloader that maps virtual.
; port/inle/klink.l drives it; the inle kernel rides it on both arches.
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
   ; every relocation's field arithmetic is a linker's: 64-bit MODULAR. hosted
   ; addresses never reach the wrap, but the kernel LEANS on it -- mkboot.l's
   ; 32-bit stub says `.boot + 0x80003000`, and that sum wraps the high-half
   ; base off to the physical address the pre-paging code has to jump to.
   ; % is truncated here, so a negative sum comes back negative: fold it up.
   (ld-u64 v) (: r (v % 18446744073709551616)
                 (? (r < 0) (r + 18446744073709551616) r))
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
       (nm = "ai_nifs") 'nifs
       (nm = ".image") 'image
       ; the KERNEL lanes (ldkern's, port/inle/<a>/<a>.lds's names): the boot
       ; stub's page tables and bring-up ladder, the limine request block --
       ; its three sections bracket a scan, so they lay in header order, which
       ; is the order kmain.c writes them -- and the PVH note qemu reads to
       ; find the 32-bit entry. a hosted object carries none of them.
       (|| (nm = ".boot") (nm = ".boot.text")) 'boot
       (ld-pre nm ".limine_requests") 'limreq
       (nm = ".note.pvh") 'note ())

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
              ; a NOTE the LOADER reads is placed like progbits -- .note.pvh is
              ; how `qemu -kernel` finds the 32-bit entry, so dropping it as
              ; advisory (which every other note is) would strand the kernel.
              (? (|| (member? ty '(1 8)) (&& (ty = 7) (? (ld-lane nm ty) 1 ())))
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
                0 '(text rodata data nifs bss image note boot limreq))
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

   ; how many bytes a lane holds in one object. a lane with a NAME is recorded even
   ; when it is empty (every object carries a zero-size .image), so "does this set
   ; use the lane at all?" has to weigh the bytes, not count the sections.
   (ld-lanesz ob lane)
    (foldl (\ a i (a + (at (peep (peep ob 'secs 0) i ()) 2)))
           0 (peep (peep ob 'lanes 0) lane ()))

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
   ; cx is the resolution context every lane shares -- (glo n0 n1 r0 r1 relbox rv),
   ; the symbol book, the two synthesized brackets, the -pie site box and the
   ; riscv flag -- bundled so ldemit can thread it without a nine-argument tail.
   (ldpatch ob ix base cx)
    (: glo <cx n0 (at cx 1) n1 (at cx 2) r0 (at cx 3) r1 (at cx 4)
       relbox (at cx 5) rv (at cx 6)
       s (peep ob 'src 0) se (peep (peep ob 'secs 0) ix ())
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
                    ; a LOCAL defined symbol is its own object's business: base +
                    ; value, read off the object that carried it. clang names
                    ; every constant pool and jump table with a .L label and
                    ; relocates against THAT, where holo's own objects always go
                    ; through the section symbol. it must never reach the global
                    ; book -- it is not there, and a namesake from another object
                    ; would be a wrong answer where link-undef is an honest one.
                    (&& ((at sy 1) = 0) (&& (0 < (at sy 3)) ((at sy 3) < 65280)))
                     (: _ (? (two? (peep (peep ob 'secs 0) (at sy 3) ())) 0
                             (scare 'link-sym-section <sy))
                        ((peep (peep ob 'base 0) (at sy 3) 0) + (at sy 4)))
                    ; a WEAK UNDEFINED symbol resolves to ZERO -- ELF says so, and
                    ; C leans on it: musl's __init_tls tests `if (_DYNAMIC)` to
                    ; tell a static link from a dynamic one, and a static link is
                    ; exactly the one where nothing defines it. only an unresolved
                    ; STRONG reference is an error.
                    (&& ((at sy 1) = 2) (&& ((at sy 3) = 0)
                         (! (two? (peep glo <sy ()))))) 0
                    (ldres glo n0 n1 r0 r1 <sy))
              sa (ld-u64 (sv + ad))                 ; S + A, wrapped as a linker wraps it
              prel (sg64 (ld-u64 (sa - (base + ro))))            ; ..and S + A - P, signed
              abs64? (? rv (kind = 2) (member? kind '(1 257)))   ; R_RISCV_64 / R_X86_64_64 / R_AARCH64_ABS64
              ; R_X86_64_32 / _32S: S + A written ABSOLUTE into a 4-byte field.
              ; a foreign object's, always -- gcc under -fno-pie and clang under
              ; -mcmodel=kernel, where we emit the 64-bit or pc-relative form.
              abs32? (&& !rv (member? kind '(10 11)))
              w (? abs64? 8 (&& rv (kind = 19)) 8 4)             ; CALL_PLT spans the auipc pair
              ; -pie: this abs64 site holds a base-0 offset (base is 0), so record its
              ; load-address for the startup relocator to add the real base to.
              _ (? (&& relbox abs64?)
                    (pin relbox 0 ((base + ro) >< (peep relbox 0 ()))) 0)
              ; ..and an abs32 site cannot be slid at all: the startup relocator
              ; adds a 64-bit base, and a 32-bit field has nowhere to put it.
              _ (? (&& relbox abs32?) (scare 'link-abs32-pie <sy) 0)
              _ (? (? rv (member? kind '(2 17 19))
                        (member? kind '(1 2 4 10 11 257 275 277 278 282 283 284 285 286 299)))
                   0 (scare 'link-reloc kind))
              _ (? ((ro + w) <= sz) 0 (scare 'link-oob (peep ob 'id 0)))
              ; the aarch64/riscv field kinds patch BITS of the instruction word the
              ; placeholder carries -- read it back and OR the field in
              wd (? abs64? 0 (rle32 s (off0 + ro)))
              bs (? abs64? (le64 sa)                             ; the abs64 three: S + A outright
                    abs32?                                       ; ..and the two abs32, range-checked
                     ; _32 is UNSIGNED and _32S signed, so each reads the wrapped
                     ; sum its own way -- and a truncated address would point
                     ; somewhere real and wrong, so each checks its own range.
                     ; the kernel exercises both: its high-half addresses are
                     ; 32S's negative half, and mkboot.l's wrapped `.boot + bias`
                     ; is a plain unsigned physical address.
                     (: v (? (kind = 10) sa (sg64 sa))
                        _ (? (? (kind = 10) (v <= 4294967295) (ld-fits32 v))
                             0 (scare 'link-range <sy))
                        (le32 v))
                    (&& rv (kind = 17))                          ; R_RISCV_JAL: S+A-P into jal's j-imm
                     (: v prel
                        _ (? (&& (-1048576 <= v) (v <= 1048575)) 0 (scare 'link-range <sy))
                        u (v & 2097151)
                        (le32 (wd | ((((u >> 20) & 1) << 31) | ((((u >> 1) & 1023) << 21)
                                     | ((((u >> 11) & 1) << 20) | (((u >> 12) & 255) << 12)))))))
                    (&& rv (kind = 19))                          ; R_RISCV_CALL_PLT: S+A-P as hi20 + signed lo12
                     (: v prel
                        low (v & 4095) lo (? (low > 2047) (low - 4096) low)
                        hi ((v - lo) // 4096)
                        _ (? (&& (-524288 <= hi) (hi <= 524287)) 0 (scare 'link-range <sy))
                        ((le32 (wd | ((hi & 1048575) << 12)))
                         + (le32 ((rle32 s (off0 + (ro + 4))) | ((lo & 4095) << 20)))))
                    (member? kind '(2 4))
                     (: v prel                          ; PC32/PLT32: S + A - P
                        _ (? (ld-fits32 v) 0 (scare 'link-range <sy))
                        (le32 v))
                    (member? kind '(282 283))           ; JUMP26/CALL26: (S+A-P)>>2 into imm26
                     (: v prel
                        _ (? (&& (-134217728 <= v) (v <= 134217727)) 0 (scare 'link-range <sy))
                        (le32 (wd | ((v // 4) & 67108863))))
                    (kind = 275)                        ; ADR_PREL_PG_HI21: page(S+A)-page(P) into adrp
                     (: pg ((sa // 4096) - ((base + ro) // 4096))
                        _ (? (&& (-1048576 <= pg) (pg <= 1048575)) 0 (scare 'link-range <sy))
                        (le32 (wd | (((pg & 3) << 29) | (((pg >> 2) & 524287) << 5)))))
                    ; the ABS_LO12_NC family -- ADD's and every LDST width's --
                    ; is one field: the low 12 bits of S+A into imm12, SCALED by
                    ; the width the opcode implies (an `ldr x`'s imm12 counts
                    ; 8-byte units). NC = no check: the low bits cannot overflow.
                    ; holo's own aarch64 emits only adrp+add, so the LDST widths
                    ; arrive from a foreign object -- clang folds the add away.
                    (: sc (? (kind = 284) 2 (kind = 285) 4 (kind = 286) 8 (kind = 299) 16 1)
                       (le32 (wd | (((sa % 4096) // sc) << 10)))))
              (go (off0 + ro + w) >es
                  (bs >< ((snip s pos (off0 + ro)) >< acc)))))
       (go off0 (ent (peep ob 'relas ())) ()))

   ; emit ONE lane in layout order -- object by object, and within an object its
   ; sections in header order -- zero-padding up to each base, so the file
   ; offsets land exactly where the layout said the addresses would. threads an
   ; accumulator through, and answers (end . pieces) so the next lane picks up
   ; where this one stopped.
   (ldemit cx lane cur os acc)
    (? !(two? os) (cur >< acc)
       (: ob <os
          (go c ac is)
           (? !(two? is) (c >< ac)
              (: se (peep (peep ob 'secs 0) <is ())
                 b (peep (peep ob 'base 0) <is 0)
                 pad (? (c < b) `((ld-zeros (b - c))) ())
                 (go (b + (at se 2)) (ac + pad + (ldpatch ob <is b cx)) >is)))
          r (go cur acc (peep (peep ob 'lanes 0) lane ()))
          (ldemit cx lane <r >os >r)))

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
                         ; a weak def a strong one replaced: its BYTES stay laid, so it
                         ; keeps a symbol -- demoted to LOCAL, naming the dead copy at its
                         ; own address (glo holds the winner; a global twin would be a lie,
                         ; but an unnamed span makes nm/vmret charge the bytes to whatever
                         ; symbol precedes them)
                         (&& (0 < bind) !won) (`(nm 0 typ (peep lanesx <se 0) ad (at sy 5)) >< a)
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
       ; .image last of all -- past .bss, on a page, so `love --bake` can GROW it at
       ; the tail of the file (host/image.c: the section that ENDS the segment is the
       ; one that can grow without moving anything). It owns file bytes where .bss does
       ; not, so p_filesz has to reach out over the bss gap and the emit writes it as
       ; zeros -- free here, where every object is mooncc's and .bss is empty, and the
       ; price of a foreign .o's real .bss if one ever rides this lane. No .image
       ; section in the set (every link but love's) and the whole lane costs nothing.
       img? (0 < (foldl (\ a ob (a + (ld-lanesz ob 'image))) 0 objs))
       iaddr (? img? (ld-align 4096 bend) r1)
       iend (? img? (ldlay1 'image iaddr objs) r1)
       mend (? img? iend bend)                  ; the last address the segment spans
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
       cx `(glo n0 end r0 r1 relbox rv)
       et (ldemit cx 'text (base + 120) objs ())
       eo (ldemit cx 'rodata <et objs >et)
       ed (ldemit cx 'data <eo objs >eo)
       en (ldemit cx 'nifs <ed objs >ed)
       tail (? (<en < r0) `((ld-zeros (r0 - <en))) ())      ; nif tail + table alignment in one pad
       rtab (? pie (catall (map (\ o (le64 o)) (rev (peep relbox 0 ())))) ())
       ei (? img? (ldemit cx 'image r1 objs ()) `(r1))      ; pads r1 -> iaddr over the bss gap
       ; -- the section headers and the symbol table, laid PAST p_filesz. nothing
       ; here is SHF_ALLOC, so the kernel maps not a byte of it and the program is
       ; the same program; nm, objdump and gdb read exactly this. we describe the
       ; image as the five lanes we laid it in, so a symbol lands in the section
       ; it actually lives in and `nm` classifies it T/R/D/B without guessing.
       ; and EVERY loaded byte gets a covering header (ai_rela names the -pie
       ; table): binutils strip rebuilds a file FROM its sections, so an orphan
       ; gap comes back as zeros -- the relocator's table gone, the stripped
       ; binary a segfault at the first unrebased pointer.
       fsz (iend - base)                                    ; the loaded bytes: p_filesz
       lanesx (tabof `(('text >< 1) ('rodata >< 2) ('data >< 3) ('nifs >< 4) ('bss >< 6) ('image >< 7)))
       st (ldstab objs glo lanesx)
       shstr (o-strtab '(".text" ".rodata" ".data" "ai_nifs" "ai_rela" ".bss" ".image"
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
                     (ld-shdr (snm 4) 1 2 r0 (r0 - base) (r1 - r0) 0 0 16 0)         ; ai_rela, the -pie table (empty non-pie)
                     (ld-shdr (snm 5) 8 3 r1 (r1 - base) (bend - r1) 0 0 16 0)       ; NOBITS: an offset, no bytes
                     (ld-shdr (snm 6) 1 3 iaddr (iaddr - base) (iend - iaddr) 0 0 16 0)  ; .image, the grow slot
                     (ld-shdr (snm 7) 2 0 0 symo symz 9 (at st 2) 8 24)              ; link=.strtab info=first global
                     (ld-shdr (snm 8) 3 0 0 stro strz 0 0 1 0)
                     (ld-shdr (snm 9) 3 0 0 ssto sstz 0 0 1 0)))
       hdr (elf64-hdr (? pie 3 2) base ent fsz (mend - base) mach `(shto 11 10))
       (hdr >< (>en + tail + (? pie `(rtab) ()) + >ei
                + `((ld-zeros (symo - fsz)) <st <>st <shstr
                    (ld-zeros (shto - (ssto + sstz))) sht))))

   ; --- the KERNEL lane: one ELF, three doors ---------------------------------
   ; a freestanding image is laid where it RUNS and loaded where it LANDS: every
   ; PT_LOAD carries p_paddr = p_vaddr - bias, which is what lets `qemu -kernel`
   ; copy by p_paddr with the MMU off while limine (and our own BOOTX64.EFI) maps
   ; by p_vaddr with it on -- one file, and mkboot.l bridges the two. that split,
   ; and the several segments it needs, is the whole of what port/inle/<a>/<a>.lds
   ; says that ldlink's one-segment shape cannot.
   ;
   ; the lanes, each page-aligned and each its own segment: `boot` (the stub's
   ; page tables and bring-up ladder), `limreq` (the limine request block, which
   ; a bootloader scans), text, rodata, then data -- nifs and image ride the data
   ; run, and .bss follows it past p_filesz. .note.pvh is laid at vbase and rides
   ; NO PT_LOAD: nothing maps it, qemu reads it out of the file through PT_NOTE
   ; to find XEN_ELFNOTE_PHYS32_ENTRY, so it lives in the header page where ld
   ; puts it too. `ent` names the entry SYMBOL (kmain on x86_64, a64boot on
   ; aarch64) rather than taking the first object's text, and `kimage_end` is
   ; synthesized just past .bss -- pvh.c and dtb.c read it to hand the heap
   ; every byte of RAM the image does not occupy.
   (ldkern target ent vbase bias srcs)
    (: mach (peep elf-machines target 0)
       _ (? (0 < mach) 0 (scare 'link-target target))
       objs (map (\ p (ld-read <p <>p mach)) srcs)
       (lsz lane) (foldl (\ a ob (a + (ld-lanesz ob lane))) 0 objs)
       ne (ldlay1 'note vbase objs)                 ; the note first, at vbase
       b0 (ld-align 4096 ne)  b1 (ldlay1 'boot b0 objs)
       q0 (ld-align 4096 b1)  q1 (ldlay1 'limreq q0 objs)
       t0 (ld-align 4096 q1)  t1 (ldlay1 'text t0 objs)
       o0 (ld-align 4096 t1)  o1 (ldlay1 'rodata o0 objs)
       d0 (ld-align 4096 o1)  d1 (ldlay1 'data d0 objs)
       n0 (ld-align 16 d1)    n1 (ldlay1 'nifs n0 objs)
       i1 (ldlay1 'image n1 objs)
       kend (ldlay1 'bss i1 objs)                   ; .bss owns address space, no file bytes
       ; p_offset mirrors the address space ONE PAGE IN -- the first page holds
       ; the ELF header, the program headers and the note -- so every p_offset
       ; stays congruent to its p_vaddr modulo the page size, which is what a
       ; mapping loader asks for and what a copying one never notices.
       (fo a) ((a - vbase) + 4096)
       (gap a b) (? (a < b) `((ld-zeros (b - a))) ())
       nsz (lsz 'note)
       segs (filter (\ g (0 < (at g 6)))
              `(`(1 7 (fo b0) b0 (b0 - bias) (b1 - b0) (b1 - b0) 4096)
                `(1 6 (fo q0) q0 (q0 - bias) (q1 - q0) (q1 - q0) 4096)
                `(1 5 (fo t0) t0 (t0 - bias) (t1 - t0) (t1 - t0) 4096)
                `(1 4 (fo o0) o0 (o0 - bias) (o1 - o0) (o1 - o0) 4096)
                `(1 6 (fo d0) d0 (d0 - bias) (i1 - d0) (kend - d0) 4096)))
       hdrsz (64 + (56 * ((tally segs) + (? (0 < nsz) 1 0))))
       phs (? (0 < nsz) (`(4 4 hdrsz vbase vbase nsz nsz 4) >< segs) segs)
       _ (? ((hdrsz + nsz) <= 4096) 0 (scare 'link-kern-headers hdrsz))
       glo (ldsyms objs)
       _ (pin glo "kimage_end" `(kend 0))
       e (peep glo ent ())
       _ (? (two? e) 0 (scare 'link-entry ent))
       cx `(glo n0 n1 n1 n1 () ())                  ; no -pie table here, and no riscv kernel yet
       en (ldemit cx 'note vbase objs ())
       eb (ldemit cx 'boot b0 objs (gap vbase b0))  ; the file resumes at vbase (offset 4096)
       eq (ldemit cx 'limreq q0 objs (>eb + (gap <eb q0)))
       et (ldemit cx 'text t0 objs (>eq + (gap <eq t0)))
       eo (ldemit cx 'rodata o0 objs (>et + (gap <et o0)))
       ed (ldemit cx 'data d0 objs (>eo + (gap <eo d0)))
       ef (ldemit cx 'nifs n0 objs (>ed + (gap <ed n0)))
       eg (ldemit cx 'image n1 objs (>ef + (gap <ef n1)))
       fsz (fo i1)                                  ; the loaded bytes end here
       ; the section headers and symbol table ride past p_filesz, mapped by
       ; nothing -- what `nm`, a labeled objdump and a gdb `break kmain` read.
       lanesx (tabof `(('note >< 1) ('boot >< 2) ('limreq >< 3) ('text >< 4)
                       ('rodata >< 5) ('data >< 6) ('nifs >< 6) ('image >< 6) ('bss >< 7)))
       st (ldstab objs glo lanesx)
       shstr (o-strtab '(".note.pvh" ".boot" ".limine_requests" ".text" ".rodata"
                         ".data" ".bss" ".symtab" ".strtab" ".shstrtab"))
       (snm k) (at <>shstr k)
       symo (ld-align 8 fsz) symz (tally <st)
       stro (symo + symz) strz (tally <>st)
       ssto (stro + strz) sstz (tally <shstr)
       shto (ld-align 8 (ssto + sstz))
       sht (catall `((ld-shdr 0 0 0 0 0 0 0 0 0 0)                                ; the reserved null header
                     (ld-shdr (snm 0) 7 2 vbase hdrsz nsz 0 0 4 0)                ; .note.pvh, in the header page
                     (ld-shdr (snm 1) 1 7 b0 (fo b0) (b1 - b0) 0 0 4096 0)
                     (ld-shdr (snm 2) 1 3 q0 (fo q0) (q1 - q0) 0 0 4096 0)
                     (ld-shdr (snm 3) 1 6 t0 (fo t0) (t1 - t0) 0 0 4096 0)
                     (ld-shdr (snm 4) 1 2 o0 (fo o0) (o1 - o0) 0 0 4096 0)
                     (ld-shdr (snm 5) 1 3 d0 (fo d0) (i1 - d0) 0 0 4096 0)        ; .data, the nifs and image runs with it
                     (ld-shdr (snm 6) 8 3 i1 (fo i1) (kend - i1) 0 0 16 0)        ; NOBITS: an offset, no bytes
                     (ld-shdr (snm 7) 2 0 0 symo symz 9 (at st 2) 8 24)           ; link=.strtab info=first global
                     (ld-shdr (snm 8) 3 0 0 stro strz 0 0 1 0)
                     (ld-shdr (snm 9) 3 0 0 ssto sstz 0 0 1 0)))
       hdr (elf64-hdrn 2 <e mach `(shto 11 10)
                       (map (\ g (elf64-phdr <g (at g 1) (at g 2) (at g 3)
                                             (at g 4) (at g 5) (at g 6) (at g 7)))
                            phs))
       (hdr >< (>en + `((ld-zeros (4096 - (hdrsz + nsz)))) + >eg
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
