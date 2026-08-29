; crew/holo/link.l -- a static ELF LINKER over obj.l's relocatable objects (the
; toolchain ladder, rung 3): read each .o (the section shape obj.l lays -- .text /
; .data / love_nifs + symtab + RELA, or REL at 32 bits -- ld-read takes either ELF
; class off the field table below), resolve every symbol across the set
; (strong over weak; __start_/__stop_love_nifs synthesized over the packed nif
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
; free/klink.l drives it; the inle kernel rides it on both arches. `ldbare32`
; is the third: one ELF32 segment for a part with no MMU at all (an M-profile
; board), where the image lays at the address the core boots from.
; loads after crew/holo/obj.l in the mooncc cat -- reads elf-base / elf-machines /
; elf64-hdr / le32 / le64 bare (asbook.l + elf.l re-leak them). moon.l drives it:
; `mooncc a.o b.c -o prog`.
(module 'holo
(:
   ; little-endian reads off a byte-indexable text; a u64 field can carry a
   ; signed value (a RELA addend), so sg64 folds the high half back down
   (rle16 s i) (s i + 256 * s (i + 1))
   (rle32 s i) (rle16 s i + 65536 * rle16 s (i + 2))
   (rle64 s i) (rle32 s i + 4294967296 * rle32 s (i + 4))
   (sg64 v) (v < 9223372036854775808 ? v (v - 18446744073709551616))
   ; a field of any width, little-endian -- the table below asks for 1, 2, 3, 4 and 8
   (rlew s i n) (n < 1 ? 0 (s i + (256 * rlew s (i + 1) (n - 1))))

   ; --- the FIELD TABLE: where each class keeps what the readers read. ---
   ; ⚠ the two classes are not one shape at two widths. Elf32_Sym REORDERS (st_info
   ; 4 -> 12, st_shndx 6 -> 14, st_value 8 -> 4), a REL's r_info splits at a BYTE
   ; (type low, sym the top three) where a RELA's splits at a word, and a REL has no
   ; addend column at all -- its addend rides IN the field, so `rl-add` is simply
   ; absent at 32 bits and the patcher pulls it per kind. every one of those but the
   ; last is a row here, which is why there is one reader below and not two.
   ; a row is (offset width); a lone scalar rides as a one-element row so presence
   ; is always the `two?`, never a zero.
   (ld-cls c) (foldl (t \ (a . u) \ pin t a u) {} c)
   (ld-row fs nm) (peep fs nm ())
   (ld-n fs nm) (: r (ld-row fs nm) (two? r ? <r (scare 'link-field nm)))
   (ld-f fs s b nm)
    (: r (ld-row fs nm) (two? r ? (rlew s (b + <r) <>r) (scare 'link-field nm)))
   ; ⚠ a table stride is the ONE row a wrong value does not announce: read a
   ; 16-byte symbol at 24 and the walk quietly takes every other one, each of
   ; them well-formed. so ask the section its OWN sh_entsize and hold the table
   ; to it -- the file is the oracle, and it is the only one there is.
   (ld-esz fs s h want sec)
    (ld-f fs s h 'sh-esz != ld-n fs want ? (scare 'link-entsize sec))
   ; the ph- rows are copy.l's, reading a LINKED file where this one reads relocatables --
   ; the one wire shape, so it stays in the one table. and an Elf32_Phdr is no more the
   ; 64-bit one narrowed than the symbol above it: every field but p_type moves.
   ;                     ELF64 / RELA                        ELF32 / REL
   elf64f (ld-cls '((sh-ent 64) (sy-ent 24) (rl-ent 24) (rl-ty 4) (ph-ent 56)
                    (e-entry 24 8) (e-phoff 32 8) (e-phent 54 2) (e-phnum 56 2)
                    (e-shent 58 2) (e-shoff 40 8) (e-shnum 60 2) (e-shstrx 62 2)
                    (ph-type 0 4) (ph-off 8 8) (ph-pa 24 8) (ph-fsz 32 8)
                    (sh-name 0 4) (sh-type 4 4) (sh-flags 8 8) (sh-off 24 8)
                    (sh-size 32 8) (sh-link 40 4) (sh-info 44 4) (sh-align 48 8) (sh-esz 56 8)
                    (sy-name 0 4) (sy-info 4 1) (sy-shndx 6 2) (sy-val 8 8) (sy-size 16 8)
                    (rl-off 0 8) (rl-kind 8 4) (rl-sym 12 4) (rl-add 16 8)))
   elf32f (ld-cls '((sh-ent 40) (sy-ent 16) (rl-ent 8) (rl-ty 9) (ph-ent 32)
                    (e-entry 24 4) (e-phoff 28 4) (e-phent 42 2) (e-phnum 44 2)
                    (e-shent 46 2) (e-shoff 32 4) (e-shnum 48 2) (e-shstrx 50 2)
                    (ph-type 0 4) (ph-off 4 4) (ph-pa 12 4) (ph-fsz 16 4)
                    (sh-name 0 4) (sh-type 4 4) (sh-flags 8 4) (sh-off 16 4)
                    (sh-size 20 4) (sh-link 24 4) (sh-info 28 4) (sh-align 32 4) (sh-esz 36 4)
                    (sy-name 0 4) (sy-info 12 1) (sy-shndx 14 2) (sy-val 4 4) (sy-size 8 4)
                    (rl-off 0 4) (rl-kind 4 1) (rl-sym 5 3)))
   (ld-cstr s i) (: (go j) (s j = 0 ? (snip s i j) (go (j + 1))) (go i))
   (ld-zeros n) (n >= 1 ? (0 . ld-zeros (n - 1)))
   (ld-align a v) (a < 2 ? v (: r (v % a) (r = 0 ? v (v + a - r))))
   (ld-fits32 v) (-2147483648 <= v <= 2147483647)
   ; every relocation's field arithmetic is a linker's: 64-bit MODULAR. nothing
   ; laid here reaches the wrap today, but an image linked above the signed half
   ; would, and % is truncated in this dialect -- a negative sum comes back
   ; negative, so fold it up.
   (ld-u64 v) (: r (v % 18446744073709551616)
               (r < 0 ? (r + 18446744073709551616) r))
   (ld-u32 v) (: r (v % 4294967296) (r < 0 ? (r + 4294967296) r))

   ; --- the ARM field lane. EM_ARM is REL: there is no addend column, so each kind
   ; pulls its own addend OUT of the field, and the thumb bit rides in the SYMBOL --
   ; ldsyms already folded st_value's bit 0 into S, and every branch field shifts the
   ; offset right by at least one, so ((S+A)|T) falls out of S+A with nothing masked.
   ; the instruction formats are the BACKEND's: refix-with hands the field's bytes
   ; back and takes the re-encode, so the J1/J2 scatter and the movw/movt imm16
   ; scatter exist in one copy each, in thumb2.l, and nothing here knows an encoding.
   ; ⚠ and it is the FAMILY's backend, not the link target's -- a REL field's format
   ; is the ABI's, where `patch` is about what a core can execute. one image carries
   ; objects of both cores (mps2's v6-M link takes a v7-M start.o, MOVW/MOVT pair and
   ; all), so the widest arm32 encoder answers for every arm32 link. ldbare32 picks it.
   ; ⚠ the two WORD kinds have no encoding to speak of -- a whole 32-bit field
   ; carrying its own addend -- so they are read and written right here: ABS32 is
   ; ((S+A)|T), REL32 that minus P (gcc's `.word handler - .`, which is how a
   ; libgcc divide reaches __aeabi_ldiv0).
   ; the numbers are obj.l's (r-arm-abs32 / r-thm-call / r-thm-jump24 / r-thm-movw /
   ; r-thm-movt) plus REL32, and a kind outside the set refuses: they collide freely
   ; with the x86-64 and riscv numbers, which is why the machine picks the lane first.
   ldarm-kinds (ld-cls '((2 abs32) (3 rel32) (10 bl) (30 b) (47 movw16) (48 movt16)))
   (ldarm be s off sv kind site)
    (: r (ldarm-kinds kind)
       _ (atom? r ? (scare 'link-reloc kind))
       k <r  sa (sv + rle32 s off)
       (? (k == 'abs32) (le32 (ld-u32 sa))
          (k == 'rel32) (le32 (ld-u32 (sa - site)))
          (refix-with be k (map (i \ s (off + i)) ^4) sv site 4)))

   ; --- ARCHIVES. the classic "!<arch>\n" file, GNU shape -- kore's `ar` writes
   ; exactly this, and reading it is what lets a link name a LIBRARY instead of
   ; every member of it. only the "/" RANLIB INDEX is walked, never the members:
   ; an index entry is a nom and the byte offset of a member's HEADER, which is
   ; the whole reason ranlib exists -- v6-M libgcc.a is 1767 members and a link
   ; wants a dozen. ⚠ so an archive with no index REFUSES rather than falling
   ; back to a full scan: the fallback would be a silent thousand-fold cost, and
   ; `ranlib` (or our own `ar rcs`) is the one-word repair.
   ; a header is 60 bytes of left-justified, space-padded ASCII; the size field
   ; excludes the "\n" a member pads to even with.
   (ld-arfld s i n)
    (: t (snip s i (i + n))
       (go k) (0 < k && t (k - 1) = 32 ? (go (k - 1)) (snip t 0 k))
       (go n))
   (ld-arnum t) (: (go k a) (#t <= k ? a (go (k + 1) (10 * a + t k - 48)))
                   (go 0 0))
   (ld-arsz s off) (ld-arnum (ld-arfld s (off + 48) 10))
   (ld-arnext s off) (: z (ld-arsz s off) (off + 60 + z + (z % 2)))
   (ld-be32 s i) ((((s i << 8) | s (i + 1)) << 16) | ((s (i + 2) << 8) | s (i + 3)))
   (ld-ar? s) (8 <= #s && snip s 0 8 = "!<arch>\n")   ; ⚠ glued ?( is the BIT: && answers () for a false LEFT
   ; nom -> the member header offset that defines it. a nom defined by two members
   ; keeps the FIRST, which is the order a linker resolves in.
   (ld-arsyms id s)
    (: _ (ld-arfld s 8 16 != "/" ? (scare 'link-ar-noindex id))
       d0 68 n (ld-be32 s d0) t {}
       (go i o) (n <= i ? t
                   (: nom (ld-cstr s o)
                      _ (? (o-has? t nom) 0 (pin t nom (ld-be32 s (d0 + 4 + 4 * i))))
                      (go (i + 1) (o + 1 + #nom))))
       (go 0 (d0 + 4 + 4 * n)))
   ; the "//" table, where a name past 15 characters lives (GNU's spelling: the
   ; member is named "/<offset>" and the table's entries end at a "/"). "" when
   ; every name is short enough to sit in its own header.
   (ld-arlong s)
    (: p (ld-arnext s 8)
     (p + 60 <= #s && ld-arfld s p 16 = "//" ?
      (snip s (p + 60) ((p + 60) + ld-arsz s p)) ""))
   (ld-armem s off lt)
    (: nm (ld-arfld s off 16) d0 (off + 60)
       real (nm 0 = 47 ?
             (: o (ld-arnum (snip nm 1 #nm))
                (go k) (k < #lt && lt k != 47 ? (go (k + 1)) k)
              (snip lt o (go o)))
             (: z #nm (nm (z - 1) = 47 ? (snip nm 0 (z - 1)) nm)))
       [real (snip s d0 (d0 + ld-arsz s off))])

   ; a section NAME -> its lane, by prefix: gcc splits a lane across many
   ; sections (.text.unlikely, .rodata.str1.1, .data.rel.ro.local) and every
   ; piece of a lane lands in the same run of the image. one RWX segment maps
   ; the whole thing, so .rodata and .data.rel.ro are data that nothing writes.
   ; love_nifs is ours and exact -- it is a packed array with a bracket, not a lane.
   (ld-pre nm p) (: n #p (#nm >= n && snip nm 0 n = p))
   ; the unwinder's tables: .eh_frame on the 64-bit targets, .ARM.exidx (its own
   ; section TYPE) plus the .ARM.extab it points into on arm -- and gcc splits each
   ; into one section per function, so both are prefix tests. see `drop?`.
   (ld-unwind? nm)
    (ld-pre nm ".eh_frame" || ld-pre nm ".ARM.exidx" || ld-pre nm ".ARM.extab")
   ; the lanes the linker knows by name; anything else allocated is a named lane
   ld-lanes '(text rodata data nifs bss image note boot)
   ; a COMPILER-NAMED section -- one the list below has no word for -- becomes its
   ; OWN lane, keyed by the name. keeping a section apart from .text and .data is
   ; the only reason to name one, so the linker keeps it apart and keeps it whole:
   ; a lane lays as one contiguous run, which is what a program that brackets or
   ; scans its own table is counting on.
   ; ⚠ PROGBITS and NOBITS only. .note.gnu.property is ALLOC and advisory, and the
   ; one note this image places is named below.
   (ld-any nm ty fl) (member? ty '(1 8) && 0 < fl & 2 ? (name nm))
   ; and it travels with the known lane its FLAGS name -- executable with text,
   ; writable with data, neither with rodata, NOBITS with bss. flags, because they
   ; are what the object actually says; a table a port wants at a FIXED address is
   ; the link driver's word to say, and nothing readable here can stand in for it.
   (ld-home lane fl ty)
    (? (member? lane ld-lanes) lane
     (ty = 8) 'bss (0 < fl & 4) 'text (0 < fl & 1) 'data 'rodata)
   (ld-lane nm ty fl)
    (? (ty = 8) (nm = ".bss" || ld-pre nm ".bss." ? 'bss (ld-any nm ty fl))
       (nm = ".text" || ld-pre nm ".text.") 'text
       (nm = ".rodata" || ld-pre nm ".rodata.") 'rodata
       (nm = ".data" || ld-pre nm ".data.") 'data
       (nm = "love_nifs") 'nifs
       (nm = ".love.image") 'image
       ; the KERNEL lanes (ldkern's names): the boot stub's page tables and
       ; bring-up ladder, and the PVH note qemu reads to find the 32-bit entry.
       ; a hosted object carries neither.
       (nm = ".boot" || nm = ".boot.text") 'boot
       (nm = ".note.pvh") 'note
       (ld-any nm ty fl))

   ; parse one relocatable object (id names it in scares) -> a tablet
   ;   'id 'src            the name and the raw bytes
   ;   'secs               section index -> (lane off size align), the placed ones
   ;   'lanes              lane -> its section indices, ASCENDING (header order)
   ;   'drops              section index -> 1, the ones ruled out (see `class`)
   ;   'syms / 'nsym       symbol index -> (name bind typ shndx value size)
   ;   'relas              ((secidx (off kind symidx addend)..) ..), the addend ()
   ;                       on a REL, where it rides in the field instead
   ; layout pins 'base -- section index -> load address -- on it later. keying
   ; the bases by INDEX, not by lane, is what lets a lane hold more than one
   ; section: a symbol reads its own section's base straight off its shndx.
   (ld-read id s mach)
    (: _ (? (s 0 = 127 && s 1 = 69 && s 2 = 76 && s 3 = 70) 0 (scare 'link-not-elf id))
       ; EI_CLASS picks the wire shape; e_type and e_machine sit at 16 and 18 in both
       fs (? (s 4 = 2) elf64f (s 4 = 1) elf32f (scare 'link-elf-class id))
       _ (rle16 s 16 = 1 && rle16 s 18 = mach ? 0 (scare 'link-machine id))
       _ (ld-f fs s 0 'e-shent = ld-n fs 'sh-ent ? 0 (scare 'link-shentsize id))
       shoff (ld-f fs s 0 'e-shoff)
       shnum (ld-f fs s 0 'e-shnum) shstrx (ld-f fs s 0 'e-shstrx)
       (sh i) (shoff + i * ld-n fs 'sh-ent)          ; the i-th section header
       shstro (ld-f fs s (sh shstrx) 'sh-off)
       (sname i) (ld-cstr s (shstro + ld-f fs s (sh i) 'sh-name))
       secs {} lanes {} drops {}
       ob {'id id 'src s 'lanes lanes 'secs secs 'drops drops 'base {}}
       ; DROPPABLE: a section this image cannot use, ruled out by what it IS,
       ; never by a hunch. empty (nothing to place); no SHF_ALLOC, so the loader
       ; never maps it at all (gcc's .comment, .debug_*, .note.GNU-stack); a NOTE,
       ; which is advisory metadata (.note.gnu.property carries ISA feature bits
       ; a static exe of ours consults nowhere); or the UNWINDER's tables, the one
       ; alloc family dropped by name -- a link with no unwinder and no exceptions
       ; never reads a byte of them. their .rel goes with them (see the rela walk).
       ; the index is recorded so a relocation POINTING AT a dropped section can be
       ; skipped rather than scared at.
       (drop? i ty fl nm sz)
        (? ((sz = 0) || (0 = (fl & 2)) || (ty = 7) || ld-unwind? nm)
           (: _ (pin drops i 1) 1) ())
       ; classify: PROGBITS and NOBITS onto their lane by name, symtab/rela by
       ; type, the droppable set by the rule above; anything ELSE with bytes is a
       ; section we have no story for -- refuse, never drop silently. SHF_TLS
       ; (.tdata/.tbss) is exactly that: a thread-local image needs a PT_TLS the
       ; one segment here cannot express, and letting it ride the data lane would
       ; give every thread the SAME variable -- a wrong answer, quietly.
       (class i symt rls)
        (? (shnum <= i) (symt . rls)
           (: h (sh i) ty (ld-f fs s h 'sh-type) fl (ld-f fs s h 'sh-flags)
              off (ld-f fs s h 'sh-off) sz (ld-f fs s h 'sh-size) nm (sname i)
              _ (0 < fl & 1024 && 0 < sz ? (scare 'link-tls nm))
              ; a NOTE the LOADER reads is placed like progbits -- .note.pvh is
              ; how `qemu -kernel` finds the 32-bit entry, so dropping it as
              ; advisory (which every other note is) would strand the kernel.
              (? (member? ty '(1 8) || ty = 7 && ld-lane nm ty fl)
                  ; SHT_NOBITS (8) is the bss lane: it OWNS NO FILE BYTES -- its
                  ; sh_offset is a placeholder -- so it carries only size and
                  ; alignment, and the image gives it address space PAST the last
                  ; byte written (p_memsz over p_filesz, the kernel zeroing the
                  ; difference). that is the whole meaning of .bss.
                  (: lane (ld-lane nm ty fl)
                     (? lane (: _ (pin secs i [lane (? (ty = 8) 0 off) sz
                                               (ld-f fs s h 'sh-align) (ld-home lane fl ty)])
                                _ (pin lanes lane (i . lanes lane))
                                (class (i + 1) symt rls))
                        ; ⚠ .comment is dropped like any non-alloc section and KEPT anyway:
                        ; it is the producer record, and the output's is the union of its
                        ; inputs' (so a foreign .o is credited rather than claimed).
                        (drop? i ty fl nm sz)
                         (: _ (? (nm = ".comment") (pin ob 'comment (snip s off (off + sz))) 0)
                            (class (i + 1) symt rls))
                        (scare 'link-section nm)))
                 (ty = 2) (: _ (ld-esz fs s h 'sy-ent nm)
                             (class (i + 1) [off sz (ld-f fs s h 'sh-link)] rls))
                 (ty = ld-n fs 'rl-ty)
                  (: _ (ld-esz fs s h 'rl-ent nm)
                     (class (i + 1) symt ([(ld-f fs s h 'sh-info) off sz] . rls)))
                 (member? ty '(0 3)) (class (i + 1) symt rls)
                 (drop? i ty fl nm sz) (class (i + 1) symt rls)
                 (scare 'link-section nm))))
       cl (class 1 () ())
       ; the NAMED lanes this object carries, first-seen in HEADER order -- which
       ; is the order they lay in, and the order the C file wrote them
       nmd (: (go i acc)
               (shnum <= i ? (rev acc)
                  (: se (secs i)
                     (go (i + 1) (? !(two? se) acc
                                    (member? <se ld-lanes) acc
                                    (member? <se acc) acc
                                    (<se . acc)))))
              (go 1 ()))
       _ (pin ob 'named nmd)
       ; the lane lists came out reversed (the walk conses); put each back in
       ; header order, so a lane's pieces lay in the order the object wrote them
       _ (foldl (\ _ l (pin lanes l (rev (lanes l)))) 0 (ld-lanes + nmd))
       symt <cl
       _ (atom? symt ? (scare 'link-no-symtab id))
       stro (ld-f fs s (sh (symt 2)) 'sh-off)    ; .strtab offset, via symtab's link
       syms {} _ (pin ob 'syms syms)
       nsym (symt 1 // ld-n fs 'sy-ent)
       _ (pin ob 'nsym nsym)
       _ (: (go i) (? (nsym <= i) 0
                      (: e (symt 0 + i * ld-n fs 'sy-ent)
                         info (ld-f fs s e 'sy-info)
                         _ (pin syms i [(ld-cstr s (stro + ld-f fs s e 'sy-name))
                                         (info >> 4) (info & 15)
                                         (ld-f fs s e 'sy-shndx) (ld-f fs s e 'sy-val)
                                         (ld-f fs s e 'sy-size)])
                         (go (i + 1))))
            (go 0))
       ; a .rela whose TARGET was dropped is dropped with it (gcc's
       ; .rela.eh_frame is the whole of this case) -- but a target that is
       ; neither placed nor dropped is a section we never saw, and scares.
       ; ⚠ the ONE place the absent row shows: a REL has no addend column, so the
       ; slot arrives EMPTY and the patch stage pulls the addend out of the field
       ; per kind. asked by presence off the row, never off a zero addend.
       ad (ld-row fs 'rl-add)
       rent (ld-n fs 'rl-ent)
       relas (map (\ r (: n (r 2 // rent)
                          (es i acc) (? (i < 0) acc
                                        (: e (r 1 + i * rent)
                                           (es (i - 1)
                                               ([(ld-f fs s e 'rl-off) (ld-f fs s e 'rl-kind)
                                                 (ld-f fs s e 'rl-sym)
                                                 (two? ad ? (sg64 (ld-f fs s e 'rl-add)))] . acc))))
                          (<r . es (n - 1) ())))
                  (filter (\ r (? (two? (secs <r)) 1
                                  (drops <r) ()
                                  (scare 'link-rela-target id)))
                          >cl))
       _ (pin ob 'relas relas)
       ob)

   ; every symbol a file names, whatever its e_type -- `kore nm`'s door. ld-read is
   ; ET_REL BY CONTRACT (a linker that accepts a linked file links garbage) and
   ; classifies every section on the way; a symbol dump has neither stake, so this
   ; reads the header, the symbol table and its strings and stops. a row is
   ;   (name bind typ shndx value size sec)
   ; sec being the defining section's (name type flags) -- () when the symbol is
   ; undefined or names one of ELF's reserved indices. ⚠ answers () rather than
   ; scaring when there is no SHT_SYMTAB: a stripped file HAS no symbols, which is
   ; an answer, and .dynsym is a different table this does not read.
   (ld-syms id s)
    (: _ (? (s 0 = 127 && s 1 = 69 && s 2 = 76 && s 3 = 70) 0 (scare 'link-not-elf id))
       fs (? (s 4 = 2) elf64f (s 4 = 1) elf32f (scare 'link-elf-class id))
       shoff (ld-f fs s 0 'e-shoff) shnum (ld-f fs s 0 'e-shnum)
       (sh i) (shoff + i * ld-n fs 'sh-ent)
       shstro (ld-f fs s (sh (ld-f fs s 0 'e-shstrx)) 'sh-off)
       (sname i) (ld-cstr s (shstro + ld-f fs s (sh i) 'sh-name))
       (find i) (? (shnum <= i) () (ld-f fs s (sh i) 'sh-type = 2) [i] (find (i + 1)))
       t (find 1)
       (? !(two? t) ()
          (: h (sh <t)
             _ (ld-esz fs s h 'sy-ent ".symtab")
             ent (ld-n fs 'sy-ent)
             off (ld-f fs s h 'sh-off) n (ld-f fs s h 'sh-size // ent)
             stro (ld-f fs s (sh (ld-f fs s h 'sh-link)) 'sh-off)
             ; index 0 is ELF's null symbol and names nothing -- the walk starts at 1
             (go i acc)
              (? (i < 1) acc
                 (: e (off + i * ent)
                    info (ld-f fs s e 'sy-info) x (ld-f fs s e 'sy-shndx)
                    (go (i - 1)
                        ([(ld-cstr s (stro + ld-f fs s e 'sy-name))
                          (info >> 4) (info & 15) x
                          (ld-f fs s e 'sy-val) (ld-f fs s e 'sy-size)
                          (? (0 < x && x < shnum)
                             [(sname x) (ld-f fs s (sh x) 'sh-type)
                              (ld-f fs s (sh x) 'sh-flags)])]
                         . acc))))
             (go (n - 1) ()))))

   ; -- ARCHIVES. the noms no object defines but the LINKER lays: a driver over any
   ; door here answers for these, so a ledger must not go looking for them in a .a.
   ldsynth '("__start_love_nifs" "__stop_love_nifs" "__start_love_rela" "__stop_love_rela"
             "__start_love_mods" "__stop_love_mods"
             "__data_start__" "__data_end__" "__data_load__"
             "__bss_start__" "__bss_end__" "__load_size__")
   (dusyms id bs mach)                     ; (id bytes) -> (defs strong-undefs), global noms
    (: ob (ld-read id bs mach) sy9 (peep ob 'syms 0)
       (go i ds us)
        (? (peep ob 'nsym 0 <= i) [ds us]
           (: sy (sy9 i) bind (sy 1)
              (? !(member? bind '(1 2)) (go (i + 1) ds us)
                 (0 < sy 3) (go (i + 1) (<sy . ds) us)
                 (bind = 1) (go (i + 1) ds (<sy . us))
                 (go (i + 1) ds us))))    ; a weak undef resolves to 0 (ELF's law): no need
       (go 1 () ()))
   ; --- WRITING one: `ar rcs` as a function, the same shape read back above, and
   ; below dusyms because the index is dusyms' own answer. deterministic
   ; (date/uid/gid 0, mode 644 -- binutils' own -D default) and ALWAYS indexed,
   ; since the reader refuses an archive without one. `ms` is ((name bytes)..) and
   ; the names must be UNIQUE: two members spelling one name leave the second
   ; unreachable through the index, which is the reader's only door. kore's `ar`
   ; and moon's runtime archive are both this.
   (ld-arp w s) (#s < w ? (ld-arp w (s + " ")) s)      ; ar fields are LEFT-justified
   (ld-arb4 v) (string ((v >> 24) & 255) + string ((v >> 16) & 255) + string ((v >> 8) & 255) + string (v & 255))
   (ld-arhdr nm date uid gid mode sz)
    (ld-arp 16 nm + ld-arp 12 date + ld-arp 6 uid + ld-arp 6 gid + ld-arp 8 mode + ld-arp 10 (show sz) + "`\n")
   (ld-arpad s) (#s & 1 ? (s + "\n") s)              ; members lie on even offsets
   (ld-arelf? b) (20 <= #b && b 0 = 127 && b 1 = 69 && b 2 = 76 && b 3 = 70)
   ; a member's defined globals, read by the LEDGER's own eye -- so what the index
   ; promises and what a pull then finds cannot part. a non-ELF member defines nothing.
   (ld-ardefs f b) (? (ld-arelf? b) <(dusyms f b (b 18 + (256 * b 19))) ())
   (arbytes ms)                                               ; ms: ((name bytes)..) -> the archive
    (: names (map cap ms)
       datas (map caup ms)
       ; the long-name table: parallel to names, each entry (offset) | ()
       lofs (: (go l off acc) (? !(two? l) (rev acc)
                                 (: ln #<l
                                    (? (ln < 16) (go >l off (() . acc))
                                       (go >l (off + ln + 2) ([off] . acc)))))
               (go names 0 ()))
       ltab (: (go ns ls) (? !(two? ns) ""
                             (two? <ls ? (<ns + "/\n" + go >ns >ls) (go >ns >ls)))
               (go names lofs))
       syml (map (m \ ld-ardefs <m <>m) ms)
       nsym (foldl (+) 0 (map tally syml))
       strt (foldl (+) "" (map (ss \ foldl (a \ s \ a + s + string 0) "" ss) syml))
       ; the index pads to even with a NUL that COUNTS in its size field (GNU's
       ; spelling -- members pad with "\n" excluded from theirs)
       isz (: z (4 + 4 * nsym + #strt) (z + (& z 1)))
       ; member header offsets: magic, index hdr + data, "//" (when any), then each
       m0 (8 + 60 + isz)
       m1 (? #ltab (m0 + 60 + #ltab + (#ltab & 1)) m0)
       offs (: (go ds off acc) (? !(two? ds) (rev acc)
                                (go >ds (off + 60 + (: z #<ds (z + (z & 1)))) (off . acc)))
               (go datas m1 ()))
       idx (: raw ((ld-arb4 nsym
                    + (: (go os ss) (? !(two? os) ""
                                       (foldl (\ a _ (a + ld-arb4 <os)) "" <ss + go >os >ss))
                         (go offs syml)))
                   + strt)
              (#raw & 1 ? (raw + string 0) raw))
       body (: (go ns ls ds) (? !(two? ns) ""
                                (: nf (two? <ls ? ("/" + show <<ls) (<ns + "/"))
                                 (ld-arhdr nf "0" "0" "0" "644" #<ds + ld-arpad <ds + go >ns >ls >ds)))
               (go names lofs datas))
       ((("!<arch>\n" + ld-arhdr "/" "0" "0" "0" "0" isz + idx)
         + (? #ltab (ld-arhdr "//" "" "" "" "" #ltab + ld-arpad ltab) ""))
        + body))
   ; THE LEDGER a set of objects leaves: every nom it defines (plus the ones the
   ; linker synthesizes rather than any object defining them), and the strong
   ; undefineds still owed. -> (dt owed), dt a tablet the caller goes on pinning into.
   ; a __start_X/__stop_X pair is the linker's to answer for ANY named lane
   ; (ldres's generic rule), so the ledger leaves every such name to it -- a
   ; bracket over a lane that never lays still scares, at the link.
   (ld-bracket? nm)
    (: z #nm (8 < z && snip nm 0 8 = "__start_" ||
              7 < z && snip nm 0 7 = "__stop_"))
   (ldledger srcs mach)
    (: duos (map (\ p (dusyms <p <>p mach)) srcs)
       dt (tablet 0)
       _ (map (nm \ pin dt nm 1) ldsynth)
       _ (map (du \ map (nm \ pin dt nm 1) <du) duos)
       [dt (foldl (\ a du (foldl (\ a2 nm (? (dt nm || member? nm a2
                                              || ld-bracket? nm) a2 (nm . a2)))
                                 a <>du))
                  () duos)])
   ; an ARCHIVE resolves BY NEED and BY INDEX: for each nom the link still owes, the
   ; ranlib table names the member that defines it, so a 1767-member libgcc costs a
   ; dozen ELF reads and not 1767. a pulled member's own undefineds join the owe and
   ; the round repeats, which is how one member drags in another.
   ; -> (picked-srcs owed); `dt` grows in place, as the ledger's owner expects.
   (arpull path s dt owed mach)
    (: idx (ld-arsyms path s) lt (ld-arlong s) seen {}
       (go u picked)
        (: hit (foldl (\ a nm (two? a ? a
                                 (: o (idx nm)
                                    (? !(o-has? idx nm) a (o-has? seen o) a [o]))))
                      () u)
           (? !(two? hit) [picked u]
              (: off <hit
                 _ (pin seen off 1)
                 m (ld-armem s off lt)
                 id (path + "(" + <m + ")")
                 du (dusyms id <>m mach)
                 _ (map (nm \ pin dt nm 1) <du)
                 u2 (foldl (\ a nm (dt nm || member? nm a ? a (nm . a)))
                           (filter (nil? * dt) u) <>du)
                 (go u2 (picked + [[id <>m]])))))
       (go owed ()))
   ; every archive on the line, in COMMAND ORDER and after every object -- a library
   ; is what a link falls back on, never what it leads with. `pairs` are already-read
   ; (path bytes), so the file I/O and its diagnostics stay with whoever has a voice
   ; for them (moon's `cc:` line, a board driver's scare).
   (ldarch srcs pairs tgt)
    (? !(two? pairs) ()
       (: mach (peep elf-machines tgt 0)
          lg (ldledger srcs mach) dt <lg
          (go ps u acc)
           (? !(two? ps) acc
              (: r <ps
                 _ (? (ld-ar? <>r) 0 (scare 'link-not-archive <r))
                 pr (arpull <r <>r dt u mach)
                 (go >ps <>pr (acc + <pr))))
          (go pairs <>lg ())))

   ; lay ONE lane across the set in object order -- and, within an object, its
   ; sections in header order -- each aligned as its own header asks. pins the
   ; base per SECTION INDEX; answers the address just past the last.
   (ldlay1 lane cur os)
    (? !(two? os) cur
       (: ob <os bs (peep ob 'base 0)
          (go c is) (? !(two? is) c
                       (: se (peep (peep ob 'secs 0) <is ())
                          a (ld-align (se 3) c)
                          _ (pin bs <is a)
                          (go (a + se 2) >is)))
          (ldlay1 lane (go cur (peep (peep ob 'lanes 0) lane ())) >os)))

   ; the NAMED lanes of a SET that travel with `home`, deduped, first-seen order
   ; (object order, then header order) -- the one order layout and emit both walk.
   (ld-nlanes objs home)
    (: seen {}
       (nhome ob l) (at (peep (peep ob 'secs 0) <(peep (peep ob 'lanes 0) l ()) ()) 4)
       (go os acc)
        (? !(two? os) (rev acc)
           (: ob <os
              (go >os (foldl (\ a l (? (0 < peep seen l 0 || nhome ob l != home) a
                                       (pin seen l 1, l . a)))
                             acc (ob 'named)))))
       (go objs ()))
   ; lay them, each whole, after the known lane they travel with -> the next address
   (ldlayn home cur objs)
    (foldl (c \ l \ ldlay1 l c objs) cur (ld-nlanes objs home))
   ; and emit them in that SAME order: the bytes and the addresses are one walk
   (ldemitn cx home objs r)
    (foldl (a \ l \ ldemit cx l <a objs >a) r (ld-nlanes objs home))

   ; how many bytes a lane holds in one object. a lane with a NAME is recorded even
   ; when it is empty (every object carries a zero-size .image), so "does this set
   ; use the lane at all?" has to weigh the bytes, not count the sections.
   (ld-lanesz ob lane)
    (foldl (\ a i (a + at (peep (peep ob 'secs 0) i ()) 2))
           0 (peep (peep ob 'lanes 0) lane ()))

   ; assign every section its load address: header first (hdrsz reserves the ELF
   ; header and one phdr per segment), then all text (each aligned as its header
   ; asks), rodata, data, then the nif region -- 16-aligned
   ; once and packed tight, so the __start_/__stop_ bracket reads it as ONE
   ; array across every object. the bss lane is NOT here: it owns no file bytes,
   ; so it is laid LAST, past everything written (ldlink, once the -pie table's
   ; size is known). pins the bases; -> (end nifstart rodatastart datastart).
   ; salign is the grain the three lanes may be cut on: a caller giving each its
   ; own segment asks for a page, so a segment boundary is never shared. 1 packs
   ; them tight, and then each start is just where the lane before it stopped.
   (ldlay objs base hdrsz salign)
    (: t1 (ldlayn 'text (ldlay1 'text (base + hdrsz) objs) objs)
       o0 (ld-align salign t1)
       o1 (ldlayn 'rodata (ldlay1 'rodata o0 objs) objs)
       d0 (ld-align salign o1)
       d1 (ldlayn 'data (ldlay1 'data d0 objs) objs)
       n0 (ld-align 16 d1)
       [(ldlay1 'nifs n0 objs) n0 o0 d0])

   ; the one symbol book: name -> (addr weak?), every defined global/weak
   ; across the set. a strong def beats any weak; two strongs scare; the
   ; first weak stands until a strong arrives.
   (ldsyms objs)
    (: glo {}
       (one ob i)
        (? (peep ob 'nsym 0 <= i) 0
           (: sy (peep (peep ob 'syms 0) i ())
              bind (sy 1) shx (sy 3)
              (? (shx = 0 || !(member? bind '(1 2))) (one ob (i + 1))
                 (: nm <sy
                    _ (? (two? (peep (peep ob 'secs 0) shx ())) 0
                         (scare 'link-sym-section nm))
                    a (peep (peep ob 'base 0) shx 0 + sy 4)
                    w (bind = 2)
                    old (peep glo nm ())
                    _ (? !(two? old) (pin glo nm [a w])
                         w 0                                   ; incoming weak: the standing def keeps
                         (old 1) (pin glo nm [a w])     ; strong replaces weak
                         (scare 'link-dup nm))
                    (one ob (i + 1))))))
       _ (: (each os) (? !(two? os) 0 (one <os 1, each >os))
            (each objs))
       glo)

   ; the extents of every NAMED lane, once laid: lane -> (start end). ldres's
   ; generic bracket reads it -- an empty lane never enters, so a miss stays a miss.
   (ld-extents objs)
    (: xt {}
       _ (map (ob \ map (l \ map (i \ : a (peep (peep ob 'base 0) i 0)
                                        e (a + peep (peep ob 'secs 0) i () 2)
                                        r (xt l)
                                        (pin xt l (? !(two? r) [a e] [(a < <r ? a <r) (<>r < e ? e <>r)])))
                                   (peep (peep ob 'lanes 0) l ()))
                         (ob 'named))
              objs)
       xt)

   ; resolve a name: the symbol book, or a synthesized bracket -- the nif region
   ; (n0..n1), under -pie the RELATIVE-reloc offset table (r0..r1) that
   ; nolibc's startup walks to slide every abs64 data pointer by the load base
   ; (non-pie leaves r0 == r1, an empty table, so the startup loop is a no-op) --
   ; or GNU ld's own generic rule, which the gcc lane already grants: a section
   ; named X brackets as __start_X/__stop_X, read off the laid lane's extent (xt).
   (ldres glo n0 n1 r0 r1 xt nm)
    (: r (glo nm)
       (? (two? r) <r
          (nm = "__start_love_nifs") n0
          (nm = "__stop_love_nifs") n1
          (nm = "__start_love_rela") r0
          (nm = "__stop_love_rela") r1
          (: z #nm
             x (? (8 < z && snip nm 0 8 = "__start_")
                   (: v (xt (name (snip nm 8 z))) (two? v ? [<v]))
                  (7 < z && snip nm 0 7 = "__stop_")
                   (: v (xt (name (snip nm 7 z))) (two? v ? [<>v])))
             (two? x ? <x (scare 'link-undef nm)))))

   ; patch one object's one section: stream its bytes, splicing each
   ; relocation's field. a section-symbol reloc lands on the object's own
   ; section base; a named one resolves through the book (so a weak def's
   ; callers meet the strong override). entries arrive in ascending offset
   ; order (obj.l's walk order) -- pieces come out as text snips + byte lists.
   ; rv flags an EM_RISCV link: riscv's reloc numbers live in their own namespace
   ; and R_RISCV_64 is 2 -- the same number as R_X86_64_PC32 -- so the kind
   ; dispatch below switches on the machine, never on the number alone.
   ; cx is the resolution context every lane shares -- (glo n0 n1 r0 r1 relbox rv
   ; arm? be xt), the symbol book, the two synthesized brackets, the -pie site box,
   ; the riscv flag, the EM_ARM flag, the backend the ARM lane re-encodes through,
   ; and the named-lane extents -- bundled so ldemit can thread it without a
   ; ten-argument tail.
   (ldpatch ob ix base [glo n0 n1 r0 r1 relbox rv arm? be xt])
    (: s (peep ob 'src 0) se (peep (peep ob 'secs 0) ix ())
       off0 (se 1) sz (se 2)
       (ent es) (? !(two? es) ()
                   (<<es = ix) ><es
                   (ent >es))
       (go pos es acc)
        (? !(two? es) (rev (snip s pos (off0 + sz) . acc))
           (: e <es ro <e kind (e 1) ad (e 3)
              _ (pos - off0 <= ro ? 0 (scare 'link-unsorted (peep ob 'id 0)))
              sy (peep (peep ob 'syms 0) (e 2) ())
              sv (? (sy 2 = 3)                   ; STT_SECTION: the section's base
                    (: _ (? (two? (peep (peep ob 'secs 0) (sy 3) ())) 0
                            (scare 'link-sym-section (peep ob 'id 0)))
                       (peep (peep ob 'base 0) (sy 3) 0))
                    ; a LOCAL defined symbol is its own object's business: base +
                    ; value, read off the object that carried it. clang names
                    ; every constant pool and jump table with a .L label and
                    ; relocates against THAT, where holo's own objects always go
                    ; through the section symbol. it must never reach the global
                    ; book -- it is not there, and a namesake from another object
                    ; would be a wrong answer where link-undef is an honest one.
                    (sy 1 = 0 && 0 < sy 3 < 65280)
                     (: _ (? (two? (peep (peep ob 'secs 0) (sy 3) ())) 0
                             (scare 'link-sym-section <sy))
                        (peep (peep ob 'base 0) (sy 3) 0 + sy 4))
                    ; a WEAK UNDEFINED symbol resolves to ZERO -- ELF says so, and
                    ; C leans on it: musl's __init_tls tests `if (_DYNAMIC)` to
                    ; tell a static link from a dynamic one, and a static link is
                    ; exactly the one where nothing defines it. only an unresolved
                    ; STRONG reference is an error.
                    (sy 1 = 2 && sy 3 = 0 && atom? (glo <sy)) 0
                    (ldres glo n0 n1 r0 r1 xt <sy))
              ; the two classes part HERE and nowhere else. an ARM field is 4 bytes
              ; wide whatever the kind, its addend is already in it, and `ad` arrives
              ; () -- so every line below would fold the unit into an address.
              (? arm?
                 (: _ (ro + 4 <= sz ? 0 (scare 'link-oob (peep ob 'id 0)))
                    (go (off0 + ro + 4) >es
                        (ldarm be s (off0 + ro) sv kind (base + ro)
                         . (snip s pos (off0 + ro) . acc))))
                 (: sa (ld-u64 (sv + ad))              ; S + A, wrapped as a linker wraps it
                    prel (sg64 (ld-u64 (sa - (base + ro))))            ; ..and S + A - P, signed
                    abs64? (? rv (kind = 2) (member? kind '(1 257)))   ; R_RISCV_64 / R_X86_64_64 / R_AARCH64_ABS64
                    ; R_X86_64_32 / _32S: S + A written ABSOLUTE into a 4-byte field.
                    ; a foreign object's, always -- gcc under -fno-pie and clang under
                    ; -mcmodel=kernel, where we emit the 64-bit or pc-relative form.
                    abs32? (!rv && member? kind '(10 11))
                    w (? abs64? 8 (rv && kind = 19) 8 4)             ; CALL_PLT spans the auipc pair
                    ; -pie: this abs64 site holds a base-0 offset (base is 0), so record its
                    ; load-address for the startup relocator to add the real base to.
                    _ (relbox && abs64? ? (pin relbox 0 (base + ro . relbox 0)))
                    ; ..and an abs32 site cannot be slid at all: the startup relocator
                    ; adds a 64-bit base, and a 32-bit field has nowhere to put it.
                    _ (relbox && abs32? ? (scare 'link-abs32-pie <sy))
                    _ (? (member? kind $ rv ? '(2 17 19)
                                              '(1 2 4 10 11 257 275 277 278 282 283 284 285 286 299))
                         0 (scare 'link-reloc kind))
                    _ (ro + w <= sz ? 0 (scare 'link-oob (peep ob 'id 0)))
                    ; the aarch64/riscv field kinds patch BITS of the instruction word the
                    ; placeholder carries -- read it back and OR the field in
                    wd (? abs64? 0 (rle32 s (off0 + ro)))
                    bs (? abs64? (le64 sa)                             ; the abs64 three: S + A outright
                          abs32?                                       ; ..and the two abs32, range-checked
                           ; _32 is UNSIGNED and _32S signed, so each reads the wrapped
                           ; sum its own way -- and a truncated address would point
                           ; somewhere real and wrong, so each checks its own range.
                           (: v (kind = 10 ? sa (sg64 sa))
                              _ (kind = 10 && v <= 4294967295 || ld-fits32 v ? 0 (scare 'link-range <sy))
                              (le32 v))
                          (rv && kind = 17)                          ; R_RISCV_JAL: S+A-P into jal's j-imm
                           (: v prel
                              _ (? (-1048576 <= v <= 1048575) 0 (scare 'link-range <sy))
                              u (v & 2097151)
                              (le32 (wd | ((((u >> 20) & 1) << 31) | ((((u >> 1) & 1023) << 21)
                                        | ((((u >> 11) & 1) << 20) | (((u >> 12) & 255) << 12)))))))
                          (rv && kind = 19)                          ; R_RISCV_CALL_PLT: S+A-P as hi20 + signed lo12
                           (: v prel
                              low (v & 4095) lo (low > 2047 ? (low - 4096) low)
                              hi ((v - lo) // 4096)
                              _ (? (-524288 <= hi <= 524287) 0 (scare 'link-range <sy))
                              (le32 (wd | ((hi & 1048575) << 12))
                               + le32 (rle32 s (off0 + ro + 4) | ((lo & 4095) << 20))))
                          (member? kind '(2 4))
                           (: v prel                          ; PC32/PLT32: S + A - P
                              _ (ld-fits32 v ? 0 (scare 'link-range <sy))
                              (le32 v))
                          (member? kind '(282 283))           ; JUMP26/CALL26: (S+A-P)>>2 into imm26
                           (: v prel
                              _ (-134217728 <= v <= 134217727 ? 0 (scare 'link-range <sy))
                              (le32 (wd | (v // 4 & 67108863))))
                          (kind = 275)                        ; ADR_PREL_PG_HI21: page(S+A)-page(P) into adrp
                           (: pg (sa // 4096 - (base + ro) // 4096)
                              _ (-1048576 <= pg <= 1048575 ? 0 (scare 'link-range <sy))
                              (le32 (wd | (((pg & 3) << 29) | (((pg >> 2) & 524287) << 5)))))
                          ; the ABS_LO12_NC family -- ADD's and every LDST width's --
                          ; is one field: the low 12 bits of S+A into imm12, SCALED by
                          ; the width the opcode implies (an `ldr x`'s imm12 counts
                          ; 8-byte units). NC = no check: the low bits cannot overflow.
                          ; holo's own aarch64 emits only adrp+add, so the LDST widths
                          ; arrive from a foreign object -- clang folds the add away.
                          (: sc (kind @ 284 2 285 4 286 8 299 16 1)
                             (le32 (wd | (((sa % 4096) // sc) << 10)))))
                    (go (off0 + ro + w) >es
                        (bs . snip s pos (off0 + ro) . acc))))))
       (go off0 (ent (ob 'relas)) ()))

   ; emit ONE lane in layout order -- object by object, and within an object its
   ; sections in header order -- zero-padding up to each base, so the file
   ; offsets land exactly where the layout said the addresses would. threads an
   ; accumulator through, and answers (end . pieces) so the next lane picks up
   ; where this one stopped.
   (ldemit cx lane cur os acc)
    (? !(two? os) (cur . acc)
       (: ob <os
          (go c ac is)
           (? !(two? is) (c . ac)
              (: se (peep (peep ob 'secs 0) <is ())
                 b (peep (peep ob 'base 0) <is 0)
                 pad (c < b ? [(ld-zeros (b - c))])
                 (go (b + se 2) (ac + pad + ldpatch ob <is b cx) >is)))
          r (go cur acc (peep (peep ob 'lanes 0) lane ()))
          (ldemit cx lane <r >os >r)))

   ; a 24-byte Elf64_Sym and a 64-byte Elf64_Shdr. the symbol is obj.l's o-sym
   ; with st_size carried instead of zeroed: a foreign object reports real
   ; function extents, and they are worth keeping once we have somewhere to put them.
   (ld-sym nameoff bind typ shndx value size)
    (catall [(le32 nameoff) [((bind << 4) | typ) 0] (le16 shndx) (le64 value) (le64 size)])
   (ld-shdr nm ty fl ad off sz lk inf al es)
    (catall [(le32 nm) (le32 ty) (le64 fl) (le64 ad) (le64 off) (le64 sz)
             (le32 lk) (le32 inf) (le64 al) (le64 es)])
   ; the ELF32 twins -- 16 bytes and 40. ⚠ Elf32_Sym REORDERS: st_value and st_size
   ; move AHEAD of st_info/st_other/st_shndx, which is the same fact the field table
   ; up top records at the reading end, so this is its own door rather than a width
   ; knob on the one above. the section header does hold its order, and is a twin.
   (ld-sym32 nameoff bind typ shndx value size)
    (catall [(le32 nameoff) (le32 value) (le32 size) [((bind << 4) | typ) 0] (le16 shndx)])
   (ld-shdr32 nm ty fl ad off sz lk inf al es)
    (catall [(le32 nm) (le32 ty) (le32 fl) (le32 ad) (le32 off) (le32 sz)
             (le32 lk) (le32 inf) (le32 al) (le32 es)])

   ; THE EXECUTABLE'S SYMBOL TABLE -- what makes `nm`, a labeled `objdump -d` and
   ; a gdb `break main` work on a binary of ours. every DEFINED symbol across the
   ; set, read back out of the objects that carried it: locals FIRST (ELF requires
   ; that, and .symtab's sh_info names the first global), then the globals that WON
   ; the link -- a weak definition a strong one replaced is skipped, because glo
   ; already holds the winner and two entries for one name at two addresses would
   ; be a lie. st_size rides through from the object: ours say 0 (holo lays labels,
   ; not extents), and a size-0 symbol is bounded by the next one, which is what
   ; every reader already does for a stripped-but-symboled binary.
   ; lanesx maps a lane to its section index in the image -- by the lane itself
   ; where the caller laid one header per lane, else by the section's HOME, so a
   ; symbol in a compiler-named lane is classified as what it travels with rather
   ; than landing in UNDEF. mksym/esz are the class's entry writer and its
   ; stride -- the WALK is one, and only the wire shape differs. -> (bytes strs nlocal)
   (ldstab objs glo lanesx mksym esz)
    (: (lx se) (? (o-has? lanesx (at se 0)) (peep lanesx (at se 0) 0)
                  (peep lanesx (at se 4) 0))
       (one ob acc)
        (: bs (peep ob 'base 0) secs (peep ob 'secs 0) syms (peep ob 'syms 0)
           (go i a)
            (? (peep ob 'nsym 0 <= i) a
               (: sy (syms i)
                  nm <sy bind (sy 1) typ (sy 2) shx (sy 3)
                  se (secs shx)
                  ad (peep bs shx 0 + sy 4)
                  won (: r (glo nm) (two? r ? (ad = <r) 0))
                  (go (i + 1)
                      (? (atom? se || 0 = #nm ||
                          !(member? typ '(0 1 2)) || !(member? bind '(0 1 2)))
                          a                             ; undefined, unnamed, a FILE/SECTION marker, or dropped
                         ; a weak def a strong one replaced: its BYTES stay laid, so it
                         ; keeps a symbol -- demoted to LOCAL, naming the dead copy at its
                         ; own address (glo holds the winner; a global twin would be a lie,
                         ; but an unnamed span makes nm/vmret charge the bytes to whatever
                         ; symbol precedes them)
                         (0 < bind && !won) ([nm 0 typ (lx se) ad (at sy 5)] . a)
                         ([nm bind typ (lx se) ad (at sy 5)] . a)))))
           (go 1 acc))
       all (foldl (\ a ob (one ob a)) () objs)
       locs (filter (e \ <>e = 0) all)
       ord (locs + filter (e \ 0 < <>e) all)
       str (o-strtab (map cap ord))
       ; index 0 is the reserved NULL entry, and it counts as a local
       [(ld-zeros esz + catall (o-mapi (\ i e (mksym (at <>str i) (at e 1) (at e 2)
                                                        (at e 3) (at e 4) (at e 5)))
                                         ord))
        <str (1 + tally locs)])

   ; count the abs64 (R_X86_64_64 / R_AARCH64_ABS64) data relocations across the
   ; set -- the entry count of the -pie relocation table (8 bytes each).
   (ld-nabs objs rv)
    (foldl (\ a ob (foldl (\ a2 tr
              (foldl (\ a3 e (? (? rv (e 1 = 2) (member? (e 1) '(1 257))) (a3 + 1) a3)) a2 >tr))
              a (peep ob 'relas ())))
       0 objs)

   ; -- .comment, THE PRODUCER RECORD: NUL-separated strings, no SHF_ALLOC, laid
   ; past p_filesz beside the symbol table. `prod` is what wrote this file; the
   ; inputs' own records ride in ahead of it, first-seen order, deduped -- so a
   ; link mixing a foreign .o says both names and neither claims the other's code.
   ; ⚠ a blob need not end in NUL, so the split is BOUNDED, never ld-cstr's walk.
   (ld-cmsplit s)
    (: n #s
       (go i j acc) (? (n <= j) (rev (? (j <= i) acc (snip s i j . acc)))
                       (s j = 0) (go (j + 1) (j + 1) (? (j <= i) acc (snip s i j . acc)))
                       (go i (j + 1) acc))
       (go 0 0 ()))
   (ld-comment objs prod)
    (: (add acc t) (member? t acc ? acc (acc + [t]))
       ins (foldl (\ a ob (a + ld-cmsplit (peep ob 'comment ""))) () objs)
       (catall (map (t \ o-strbytes t + '(0)) (foldl add () (ins + [prod])))))

   ; link a set of objects -- ((id bytes-text)..) -- into a static-exe PIECE
   ; list (texts + byte lists; ld-write lays them down). entry = the first
   ; object's text. three segments map the file -- R+X code, R literals, R+W
   ; data -- plus a fourth for .image when the set carries one (see below).
   ; pie: emit ET_DYN laid at base 0 (the kernel biases it to a random high
   ; address), collect every abs64 site into a trailing offset table bracketed
   ; by __start_/__stop_love_rela, and let nolibc's startup add the load base to
   ; each -- the classic static-PIE self-relocation, our own toolchain end to end.
   ; org: () takes the default base (pie 0 / elf-base); a number lays the image
   ; THERE -- the bare-metal lane (qemu -M virt wants its RAM base, 0x80000000),
   ; where `qemu -kernel` loads the segment at p_vaddr and enters at e_entry.
   ; prod: the producer string for .comment -- what wrote THIS file. BOTH DOORS SAY THE SAME
   ; WORD (`love <base>`): it is one linker, and which front end drove it is not a fact the
   ; file needs. They briefly differed, and the only thing that ever noticed was test_kore,
   ; which then wanted ten lines of objcopy to look past a distinction carrying nothing.
   ; ⚠ THE BASE HALF OF love-version, never the whole id, and the fixpoint is why:
   ; the VCS suffix names the COMMIT that built the compiler, so love1 (built by
   ; love0's mooncc) and love2 (built by love1's) would carry different producer
   ; strings and differ at e_shoff -- a broken fixpoint where the compilers agree
   ; on every byte they EMIT. the base moves with a release, which both generations
   ; share (love0 is stamped $(love_base)+bootstrap for exactly this). the full id
   ; is in love-version, in .rodata, for a reader who wants the commit.
   ; readme: plain text for whoever OPENS this file, laid in `.love.README` beside .comment and
   ; mapped by nothing. `readelf -p .love.README` prints it and `strings` finds it. It is the one
   ; annotation that pays for itself at any size: the source is already in the artifact, so
   ; what a reader lacks is not more data but a page telling them where to stand. "" lays none.
   (ldlink target pie org srcs prod readme)
    (: mach (peep elf-machines target 0)
       _ (? (0 < mach) 0 (scare 'link-target target))
       objs (map (p \ ld-read <p <>p mach) srcs)
       rv (mach = 243)                          ; EM_RISCV: its own reloc-number namespace (see ldpatch)
       base (? (charm? org) org pie 0 elf-base)
       ; W^X gives each lane the rights it needs and no others: read+execute for the
       ; code, read for the literals, read+write for the data -- three segments, and
       ; nothing is ever two of them at once. but only where an OS enforces it. `org`
       ; is the bare-metal lane: the MMU is off, the image is COPIED by p_paddr, and
       ; the port owns its own map, so it keeps the one segment it always had. ⚠ and it
       ; must: the header reserve sits BELOW the text address, so port/virt asks for
       ; 0x80000000 - hdrsz by hand -- one segment more here silently moves the hart's
       ; first instruction. .image, when the set has one, takes a segment too. the
       ; reserve is the ELF header plus one phdr per segment, and every caller reads it
       ; off the count.
       img? (0 < foldl (\ a ob (a + ld-lanesz ob 'image)) 0 objs)
       wx? !(charm? org)
       ; the binary may land on netbsd, whose exec REQUIRES the ident note
       ; (no note -> ENOEXEC; foreign kernels ignore notes). 24 bytes riding the
       ; header reserve, with a PT_NOTE and an alloc section so strip keeps it.
       ; on the ISAs that can meet that kernel: riscv has one today.
       nb? (wx? && (member? mach '(62 183)))
       hdrsz (64 + 56 * ((? wx? 3 1) + (? img? 1 0) + (? nb? 1 0)) + (? nb? 24 0))
       nbo (hdrsz - 24)
       nbnote (? nb? (catall [(le32 7) (le32 4) (le32 1)                 ; namesz descsz NETBSD_TAG
                              '(78 101 116 66 83 68 0 0) (le32 1000000000)]))   ; "NetBSD\0\0", 10.0
                  
       lay (ldlay objs base hdrsz (? wx? 4096 1))
       end <lay n0 <>lay o0 <>>lay d0 <>>>lay
       nrel (? pie (ld-nabs objs rv) 0)
       r0 (? pie (ld-align 16 end) end)         ; the reloc table starts 16-aligned, just past the nifs
       r1 (r0 + 8 * nrel)
       ; .bss last, past every byte the file carries: the segment's p_memsz runs
       ; out to `bend` where its p_filesz stops at r1, and the kernel hands over
       ; the difference already zeroed -- which is the whole meaning of .bss.
       bend (ldlayn 'bss (ldlay1 'bss r1 objs) objs)
       ; .image last of all -- past .bss, on a page, so `love bake` can GROW it at
       ; the tail of the file (src/image.c: the section that ENDS the segment is the
       ; one that can grow without moving anything). ⚠ it takes a SEGMENT of its own,
       ; because .bss sits between it and everything written: one segment would have to
       ; carry that gap as file zeros, and a real .bss makes that the whole of it. two
       ; segments buy the address gap for free -- the file offset stops mirroring the
       ; address. this is the shape ld's --section-start already hands the gcc lane, and
       ; the shape bake_tail already reads. No .image section in the set (every link but
       ; love's) and the whole lane costs nothing: one segment, as before.
       iaddr (? img? (ld-align 4096 bend) r1)
       iend (? img? (ldlay1 'image iaddr objs) r1)
       isz (iend - iaddr)
       ioff (? img? (ld-align 4096 (r1 - base)) (r1 - base))   ; ..its FILE offset, page-congruent to iaddr
       endf (? img? (ioff + isz) (r1 - base))                  ; the file bytes the segments carry
       relbox (? pie {0 ()} ())    ; abs64 site accumulator (pie only)
       glo (ldsyms objs)
       ; the ENTRY: ld's rule -- the symbol `_start` when the set defines one,
       ; else the first object's text (mooncc's own crt0 lane, which names its
       ; entry __ai_start and relies on being first). a foreign libc brings its
       ; own _start and must be entered THROUGH it: musl's sets up the stack
       ; image, tls and stdio before main ever runs.
       ent (: e (glo "_start")
              o1 <objs
              (? (two? e) <e
                 (peep (peep o1 'base 0) <(peep (peep o1 'lanes 0) 'text ()) 0)))
       cx [glo n0 end r0 r1 relbox rv () () (ld-extents objs)]
       et (ldemitn cx 'text objs (ldemit cx 'text (base + hdrsz) objs ()))
       eo (ldemitn cx 'rodata objs (ldemit cx 'rodata <et objs >et))
       ed (ldemitn cx 'data objs (ldemit cx 'data <eo objs >eo))
       en (ldemit cx 'nifs <ed objs >ed)
       tail (<en < r0 ? [(ld-zeros (r0 - <en))])       ; nif tail + table alignment in one pad
       rtab (? pie (catall (map le64 (rev (peep relbox 0 ())))) ())
       ; the image's own pad is a FILE distance now, not an address one: r1's offset up
       ; to ioff. emitting from iaddr leaves ldemit nothing to pad (cur is already the base).
       ipad (? img? [(ld-zeros (ioff - (r1 - base)))] ())
       ei (? img? (ldemit cx 'image iaddr objs ()) [iaddr])
       ; -- the section headers and the symbol table, laid PAST p_filesz. nothing
       ; here is SHF_ALLOC, so the kernel maps not a byte of it and the program is
       ; the same program; nm, objdump and gdb read exactly this. we describe the
       ; image as the five lanes we laid it in, so a symbol lands in the section
       ; it actually lives in and `nm` classifies it T/R/D/B without guessing.
       ; and EVERY loaded byte gets a covering header (love_rela names the -pie
       ; table): binutils strip rebuilds a file FROM its sections, so an orphan
       ; gap comes back as zeros -- the relocator's table gone, the stripped
       ; binary a segfault at the first unrebased pointer.
       lanesx (tabof [('text . 1) ('rodata . 2) ('data . 3) ('nifs . 4) ('bss . 6) ('image . 7)])
       st (ldstab objs glo lanesx ld-sym 24)
       ; ⚠ .comment goes LAST so no index moves: e_shstrndx and .symtab's sh_link
       ; are written down as 10 and 9, and a section spliced in ahead would silently
       ; point both at the wrong table.
       ; ⚠ THE OBJECT SAYS love_nifs AND THE BINARY SAYS .love.nifs -- the split IS the rule.
       ; GNU ld synthesizes __start_X/__stop_X only where X is a C IDENTIFIER and love.h
       ; spells that pair in C, so an OBJECT must carry the undotted name or the gcc-built
       ; love0 drains no nifs at all. ldres defines the pair here by hand off the laid
       ; extent and owes the output name nothing -- the freedom .love.rela already has,
       ; that one being a section our linker invents outright.
       shstr (o-strtab '(".text" ".rodata" ".data" ".love.nifs" ".love.rela" ".bss" ".love.image"
                         ".symtab" ".strtab" ".shstrtab" ".comment" ".love.README" ".note.netbsd.ident"))
       (snm k) (at <>shstr k)
       symo (ld-align 8 endf) symz #<st
       stro (symo + symz) strz #<>st
       ssto (stro + strz) sstz #<shstr
       cmt (ld-comment objs prod) cmto (ssto + sstz) cmtz #cmt
       rdm (string? readme ? (o-strbytes readme))
       rdmo (cmto + cmtz) rdmz #rdm
       nsh ((0 < rdmz ? 13 12) + ?nb?)
       shto (ld-align 8 (rdmo + rdmz))
       sht (catall [(ld-shdr 0 0 0 0 0 0 0 0 0 0)                                    ; the reserved null header
                     ; each lane runs to the page the next one starts on, so a split's pad
                     ; rides a section instead of the orphan gap the note above warns of
                    (ld-shdr (snm 0) 1 6 (base + hdrsz) hdrsz (o0 - (base + hdrsz)) 0 0 16 0)
                    (ld-shdr (snm 1) 1 2 o0 (o0 - base) (d0 - o0) 0 0 16 0)
                    (ld-shdr (snm 2) 1 3 d0 (d0 - base) (<ed - d0) 0 0 16 0)
                    (ld-shdr (snm 3) 1 3 <ed (<ed - base) (<en - <ed) 0 0 16 0)
                    (ld-shdr (snm 4) 1 2 r0 (r0 - base) (r1 - r0) 0 0 16 0)          ; love_rela, the -pie table (empty non-pie)
                    (ld-shdr (snm 5) 8 3 r1 (r1 - base) (bend - r1) 0 0 16 0)        ; NOBITS: an offset, no bytes
                    (ld-shdr (snm 6) 1 3 iaddr ioff isz 0 0 16 0)                    ; .image, the grow slot -- its own segment
                    (ld-shdr (snm 7) 2 0 0 symo symz 9 (at st 2) 8 24)               ; link=.strtab info=first global
                    (ld-shdr (snm 8) 3 0 0 stro strz 0 0 1 0)
                    (ld-shdr (snm 9) 3 0 0 ssto sstz 0 0 1 0)
                    (ld-shdr (snm 10) 1 48 0 cmto cmtz 0 0 1 1)                     ; .comment: MERGE|STRINGS, no alloc
                    (? (0 < rdmz) (ld-shdr (snm 11) 1 32 0 rdmo rdmz 0 0 1 0) ())    ; .love.README: STRINGS, no alloc
                    (? nb? (ld-shdr (snm 12) 7 2 (base + nbo) nbo 24 0 0 4 0) ())])   ; SHT_NOTE, alloc: strip keeps it
       ; W^X: segment one is the header and the code, READ+EXECUTE; segment two the
       ; literals, READ; segment three .data through love_rela, running out to bss in memsz
       ; alone, READ+WRITE; a fourth carries the image when there is one, RW (a heap
       ; snapshot is read and rebased, never entered). each meets the next exactly on a
       ; page -- every lane's header absorbs its own pad -- so no loaded byte belongs to
       ; neither, and no page belongs to two.
       ; ⚠ this holds only because no relocation site lands in text or rodata: the -pie
       ; table is abs64 sites, and mooncc addresses text pc-relative, so every site is in
       ; .data or love_nifs. a lane that ever needs to WRITE code has to say so with its own
       ; segment -- and rodata's promise is cgdata's, made one layer down.
       ; ⚠ the ELF header rides the executable segment: 232 bytes of mostly-zero struct,
       ; the price of not spending a whole page to hold it alone.
       ; ⚠ love_rela is read-only in truth and rides the writable segment anyway -- it is
       ; laid past the nifs, and moving it above d0 would reorder the emit for 13k.
       ; ⚠ bake_tail asks that .image END its segment and that no LOAD live above it --
       ; both hold by construction here.
       phs ((? wx? [(elf64-phdr 1 5 0 base base (o0 - base) (o0 - base) 4096)
                    (elf64-phdr 1 4 (o0 - base) o0 o0 (d0 - o0) (d0 - o0) 4096)
                    (elf64-phdr 1 6 (d0 - base) d0 d0 (r1 - d0) (bend - d0) 4096)]
                 [(elf64-phdr 1 7 0 base base (r1 - base) (bend - base) 4096)])
            + (? img? [(elf64-phdr 1 6 ioff iaddr iaddr isz isz 4096)] ())
            + (? nb? [(elf64-phdr 4 4 nbo (base + nbo) (base + nbo) 24 24 4)] ()))
       hdr (elf64-hdrn (? pie 3 2) ent mach [shto nsh 10] phs)
       ; ⚠ THE BRAND: EI_OSABI = 9 in e_ident[7] on every exe this lane writes for
       ; an ISA that can meet freebsd. The linux loader never reads the byte and
       ; freebsd's imgact refuses without it, so one binary is born acceptable to
       ; every kernel. It lives HERE, where the header is made, and not at a caller:
       ; mooncc branded its own output and kore's `ld` did not, so the two doors
       ; over ONE linker wrote different files. Same ISAs the note rides.
       hdr9 (member? mach '(62 183) ? (ld-brand hdr 7 9) hdr)
       (hdr9 . ((? nb? [nbnote] ()) + >en + tail + (? pie [rtab] ()) + ipad + >ei
                 + [(ld-zeros (symo - endf)) <st <>st <shstr cmt rdm
                    (ld-zeros (shto - (rdmo + rdmz))) sht])))

   ; --- the KERNEL lane: one ELF, three doors ---------------------------------
   ; every PT_LOAD carries p_paddr = p_vaddr - bias, so an image can be laid
   ; where it RUNS and loaded where it LANDS. inle passes bias 0 and takes the
   ; two as one number (free/klink.l), which is what lets `qemu -kernel` copy by
   ; p_paddr with the MMU off and our own BOOTX64.EFI take those same pages with
   ; it on. the several segments it needs are the rest of what a kernel asks for
   ; that ldlink's one-segment shape cannot say.
   ;
   ; the lanes, each page-aligned and each its own segment: `boot` (the stub's
   ; page tables and bring-up ladder), text, rodata, then data -- nifs and image
   ; ride the data run, and .bss follows it past p_filesz. .note.pvh is laid at vbase and rides
   ; NO PT_LOAD: nothing maps it, qemu reads it out of the file through PT_NOTE
   ; to find XEN_ELFNOTE_PHYS32_ENTRY, so it lives in the header page where ld
   ; puts it too. `ent` names the entry SYMBOL (kmain on x86_64, a64boot on
   ; aarch64) rather than taking the first object's text, and `kimage_end` is
   ; synthesized just past .bss -- pvh.c and dtb.c read it to hand the heap
   ; every byte of RAM the image does not occupy.
   (ldkern target ent vbase bias srcs)
    (: mach (peep elf-machines target 0)
       _ (0 >= mach ? (scare 'link-target target))
       objs (map (p \ ld-read <p <>p mach) srcs)
       (lsz lane) (foldl (\ a ob (a + ld-lanesz ob lane)) 0 objs)
       ne (ldlay1 'note vbase objs)                 ; the note first, at vbase
       b0 (ld-align 4096 ne)  b1 (ldlay1 'boot b0 objs)
       t0 (ld-align 4096 b1)  t1 (ldlayn 'text (ldlay1 'text t0 objs) objs)
       o0 (ld-align 4096 t1)  o1 (ldlayn 'rodata (ldlay1 'rodata o0 objs) objs)
       d0 (ld-align 4096 o1)  d1 (ldlayn 'data (ldlay1 'data d0 objs) objs)
       n0 (ld-align 16 d1)    n1 (ldlay1 'nifs n0 objs)
       i1 (ldlay1 'image n1 objs)
       kend (ldlayn 'bss (ldlay1 'bss i1 objs) objs)  ; .bss owns address space, no file bytes
       ; p_offset mirrors the address space ONE PAGE IN -- the first page holds
       ; the ELF header, the program headers and the note -- so every p_offset
       ; stays congruent to its p_vaddr modulo the page size, which is what a
       ; mapping loader asks for and what a copying one never notices.
       (fo a) (a - vbase + 4096)
       (gap a b) (a < b ? [(ld-zeros (b - a))])
       nsz (lsz 'note)
       segs (filter (g \ 0 < g 6)
              [[1 7 (fo b0) b0 (b0 - bias) (b1 - b0) (b1 - b0) 4096]
               [1 5 (fo t0) t0 (t0 - bias) (t1 - t0) (t1 - t0) 4096]
               [1 4 (fo o0) o0 (o0 - bias) (o1 - o0) (o1 - o0) 4096]
               [1 6 (fo d0) d0 (d0 - bias) (i1 - d0) (kend - d0) 4096]])
       hdrsz (64 + 56 * (#segs + (0 < nsz)))
       phs (0 < nsz ? ([4 4 hdrsz vbase vbase nsz nsz 4] . segs) segs)
       _ (hdrsz + nsz > 4096 ? (scare 'link-kern-headers hdrsz))
       glo (ldsyms objs)
       _ (pin glo "kimage_end" [kend 0])
       e (glo ent)
       _ (atom? e ? (scare 'link-entry ent))
       cx [glo n0 n1 n1 n1 () () () () (ld-extents objs)]    ; no -pie table here, and no riscv or arm kernel yet
       en (ldemit cx 'note vbase objs ())
       eb (ldemit cx 'boot b0 objs (gap vbase b0))  ; the file resumes at vbase (offset 4096)
       et (ldemitn cx 'text objs (ldemit cx 'text t0 objs (>eb + gap <eb t0)))
       eo (ldemitn cx 'rodata objs (ldemit cx 'rodata o0 objs (>et + gap <et o0)))
       ed (ldemitn cx 'data objs (ldemit cx 'data d0 objs (>eo + gap <eo d0)))
       ef (ldemit cx 'nifs n0 objs (>ed + gap <ed n0))
       eg (ldemit cx 'image n1 objs (>ef + gap <ef n1))
       fsz (fo i1)                                  ; the loaded bytes end here
       ; the section headers and symbol table ride past p_filesz, mapped by
       ; nothing -- what `nm`, a labeled objdump and a gdb `break kmain` read.
       lanesx (tabof [('note . 1) ('boot . 2) ('text . 3)
                      ('rodata . 4) ('data . 5) ('nifs . 5) ('image . 5) ('bss . 6)])
       st (ldstab objs glo lanesx ld-sym 24)
       shstr (o-strtab '(".note.pvh" ".boot" ".text" ".rodata"
                         ".data" ".bss" ".symtab" ".strtab" ".shstrtab"))
       (snm k) (at <>shstr k)
       symo (ld-align 8 fsz) symz (tally <st)
       stro (symo + symz) strz (tally <>st)
       ssto (stro + strz) sstz (tally <shstr)
       shto (ld-align 8 (ssto + sstz))
       sht (catall [(ld-shdr 0 0 0 0 0 0 0 0 0 0)                                 ; the reserved null header
                    (ld-shdr (snm 0) 7 2 vbase hdrsz nsz 0 0 4 0)                 ; .note.pvh, in the header page
                    (ld-shdr (snm 1) 1 7 b0 (fo b0) (b1 - b0) 0 0 4096 0)
                    (ld-shdr (snm 2) 1 6 t0 (fo t0) (t1 - t0) 0 0 4096 0)
                    (ld-shdr (snm 3) 1 2 o0 (fo o0) (o1 - o0) 0 0 4096 0)
                    (ld-shdr (snm 4) 1 3 d0 (fo d0) (i1 - d0) 0 0 4096 0)         ; .data, the nifs and image runs with it
                    (ld-shdr (snm 5) 8 3 i1 (fo i1) (kend - i1) 0 0 16 0)         ; NOBITS: an offset, no bytes
                    (ld-shdr (snm 6) 2 0 0 symo symz 8 (at st 2) 8 24)            ; link=.strtab info=first global
                    (ld-shdr (snm 7) 3 0 0 stro strz 0 0 1 0)
                    (ld-shdr (snm 8) 3 0 0 ssto sstz 0 0 1 0)])
       hdr (elf64-hdrn 2 <e mach [shto 10 9] (map (foldl id elf64-phdr) phs))
       (hdr . (>en + [(ld-zeros (4096 - (hdrsz + nsz)))] + >eg
                + [(ld-zeros (symo - fsz)) <st <>st <shstr
                   (ld-zeros (shto - (ssto + sstz))) sht])))

   ; --- the BARE-METAL 32-bit door ---------------------------------------------
   ; one ELF32 executable for a part with no MMU, laid where it RUNS -- `qemu
   ; -kernel` and a boot ROM alike copy each segment down by p_paddr.
   ; ⚠ the ELF HEADER is outside every segment. the first run's address is where
   ; the CORE looks (an M7 reads its initial SP and PC out of the vector table
   ; before an instruction runs), so the first laid byte has to BE the first
   ; mapped byte and the header rides file offset 0 with nothing over it -- which
   ; is why nothing here reserves address space for it the way ldlink does. the
   ; file offset stays page-congruent to the address anyway, so a MAPPING loader
   ; could take this image too; nothing on this lane is one yet.
   ;
   ; `runs` is the MAP, as data -- what a linker script says, in one list:
   ;   (at   VADDR lane..)   laid AND loaded there. a gap from the run before is
   ;                         padded in the file, so a header the boot ROM reads at
   ;                         a FIXED offset is just a run of its own.
   ;   (copy VADDR lane..)   laid at VADDR, LOADED after the file bytes so far --
   ;                         `> RAM AT> FLASH`, the classic XIP map, and the
   ;                         port's own startup copies it across at boot.
   ; a lane slot may instead hold `(flags N)`: the segment permissions the BOARD
   ; asserts, in place of the ones ld-segflags reads off the sections -- the PHDRS
   ; block a linker script writes, and it has to exist because no object can answer
   ; for it (a vector table is const to the silicon and still carries the addresses
   ; that make it writable to a compiler). the fused `at` segment takes ONE.
   ; consecutive `at` runs share ONE segment (the file tracks the addresses, gaps
   ; and all); each `copy` run takes its own, p_paddr where its bytes actually sit.
   ; ⚠ once a run is `copy` every later one must be: the file cursor and the
   ; address cursor have parted, and an `at` run after one would load nowhere.
   ; ⚠ `bss` owns address space and no file bytes, so it may only end the LAST run.
   ; ⚠ and every lane the objects actually carry has to appear somewhere in the
   ; map -- a map that forgot one would drop it in silence, which is the one
   ; failure a board reports as a hang.
   ; the five noms a copied map needs are synthesized here (__data_start__ and
   ; friends): the startup cannot compute them, and they are the same names every
   ; MCU linker script writes by hand. moon.l's ldsynth lists them.
   ; ⚠ no -pie: a freestanding board image has nothing to slide.
   ld-lanenames (ld-cls '((text ".text") (rodata ".rodata") (data ".data")
                          (nifs ".love.nifs") (bss ".bss") (image ".love.image")))
   (ld-lanenm l) (: r (peep ld-lanenames l ()) (two? r ? <r (string l)))
   ; a lane's HOME across the set -- what its sections' own flags said it travels
   ; with (ld-home). a lane the linker knows by name is its own home.
   (ld-homeof objs l)
    (? (member? l ld-lanes) l
       (: (go os) (? !(two? os) 'rodata
                     (: is (peep (peep <os 'lanes 0) l ())
                        (two? is ? (peep <os 'secs 0 <is 4) (go >os))))
          (go objs)))
   (ld-secflags h) (? (h == 'text) 6 (member? h '(data bss nifs image)) 3 2)
   (ld-segflags objs ls)                                    ; PF_R plus what the lanes ask
    (foldl (\ a l (: h (ld-homeof objs l)
                    (a | (? ( h == 'text) 1 (member? h '(data bss nifs image)) 2 0))))
           4 ls)
   ; a run row's two halves: the lanes it lays, and the `(flags N)` word a board may
   ; put among them -- a lane is an atom and the word a pair, so neither reads as the
   ; other. ⚠ the assertion OVERRIDES: a lane written at RUN time under a read-only
   ; run is an image that faults wherever permissions are enforced.
   (ld-fword? ('flags . _)) 1
   (ld-runlanes r) (filter (nil? * ld-fword?) >>r)
   (ld-runflags r) (: f (filter ld-fword? >>r)
                      (two? f ? (: v <><f
                                   _ (charm? v && 0 < v <= 7 ? 0 (scare 'link-run-flags v))
                                   v)))
   ; the fused `at` segment takes one assertion across every run that makes it, and
   ; two that disagree are a map with two minds -- named here, not silently joined.
   (ld-onef vs)
    (foldl (\ a v (? !v a !a v (a = v) a (scare 'link-run-flags v))) () vs)
   ; every lane the SET actually puts bytes or address space in, first-seen order
   (ld-uselanes objs)
    (: seen (tablet 0)
       (go os acc)
        (? !(two? os) (rev acc)
           (go >os (foldl (\ a l (? (0 < peep seen l 0) a
                                    !(0 < ld-lanesz <os l) a
                                    (pin seen l 1, l . a)))
                          acc (ld-lanes + <os 'named))))
       (go objs ()))

   (ldbare32 target runs srcs pns)
    (: mach (peep elf-machines target 0)
       _ (0 >= mach  ? (scare 'link-target target))
       _ (atom? runs ? (scare 'link-no-map target))
       ; the re-encoder is the arm32 FAMILY's (see ldarm), not this target's
       be (backends (arm32? target ? 'thumb2 target))
       objs (map (p \ ld-read <p <>p mach) srcs)
       ; a COMPILER-NAMED lane travels with the known lane its FLAGS name (ld-home),
       ; so a board's map never has to have heard of it: expand each run's list in
       ; place, every known lane followed by the named ones that ride with it. the
       ; map stays the board's word about ORDER, this is the linker's about company
       ; -- and everything downstream (the plan, the bytes, the section headers)
       ; reads the expanded runs, so the three walks cannot disagree.
       ;; ..except where the MAP already spoke for one. a table the silicon reads at a
       ;; fixed address (a vector table at flash base, a boot ROM's header block) has
       ;; only its flags to travel by, and those say rodata -- which would lay it after
       ;; the code, where nothing looks. so the board names it, and a named lane is the
       ;; board's word: the expansion supplies the company it did NOT mention.
       (lrf a r) (a + ld-runlanes r)
       spoken (foldl lrf () runs)
       (unspoken n) !(member? n spoken)
       lruns (map (r \ <r . <>r . filter ld-fword? >>r
                                + foldl (a \ l \ a + cons l (filter unspoken (ld-nlanes objs l)))
                                  () (ld-runlanes r))
                  runs)
       maplanes (foldl lrf () lruns)
       ;; a lane lives in ONE place. laid twice it is emitted twice while lpos keeps
       ;; one entry, so the bytes and the section headers part company and the file
       ;; is corrupt rather than wrong -- name it here, where the map is still legible.
       _ (foldl (\ seen l (member? l seen ? (scare 'link-lane-twice l) (l . seen)))
                () maplanes)
       _ (foldl (\ _ l (member? l maplanes ? 0 (scare 'link-unmapped-lane l)))
                0 (ld-uselanes objs))
       org (at <lruns 1)
       foff (4096 + (org % 4096))               ; ..the header's page, and the congruence
       ; -- the LAYOUT pass, run by run and lane by lane in the map's own order.
       ; lpos records each lane's (vaddr fileoff size) as it goes -- the section
       ; headers read it back rather than laying anything a second time (ldlay1
       ; pins bases as it walks, so a second call would be a second opinion).
       ; -> one entry per run: (kind vaddr fileoff filesz memsz lanes flags)
       lpos (tablet 0)
       plan (: (go rs cur fpos copied acc)
                (? !(two? rs) (rev acc)
                   (: r <rs kind <r start (at r 1) ls (ld-runlanes r) fl (ld-runflags r)
                      copy? (kind == 'copy)
                      _ (!copy? && kind !== 'at ? (scare 'link-run-kind kind))
                      _ (copied && !copy? ? (scare 'link-run-after-copy start))
                      _ (member? 'bss ls && (two? >rs || last ls !== 'bss) ?
                         (scare 'link-run-bss start))
                      _ (!copy? && cur > start ? (scare 'link-run-back start))
                      fo (copy? ? fpos (fpos + start - cur))
                      w (foldl (\ a l (: c <a f <>a
                                         n (ldlay1 l c objs)
                                         _ (pin lpos l [c f (n - c)])
                                         [n (? (id? l 'bss) f (f + n - c))]))
                               [start fo] ls)
                      (go >rs (? copy? cur <w) <>w (copied || copy?)
                          ([kind start fo (<>w - fo) (<w - start) ls fl] . acc))))
               (go lruns org foff () ()))
       ; -- the SEGMENTS: the `at` runs fuse into one (the file tracks the
       ; addresses, gaps and all), each `copy` run stands alone with p_paddr where
       ; its bytes actually sit. a row is (vaddr paddr fileoff filesz memsz flags).
       (fladdr f) (org + f - foff)            ; a file offset -> the address it loads at
       ats (filter (r \ <r == 'at) plan)
       cps (filter (r \ <r == 'copy) plan)
       aseg (? (two? ats)
               (: a0 <ats aN (last ats)
                  [[(a0 1) (a0 1) (a0 2)
                    (aN 2 + aN 3 - a0 2)
                    (aN 1 + aN 4 - a0 1)
                    (: af (ld-onef (map (r \ r 6) ats))
                       (? af af (ld-segflags objs (foldl (a \ r \ a + r 5) () ats))))]]))
       segs (aseg + map (\ r [(r 1) (fladdr (r 2)) (r 2) (r 3) (r 4)
                              (r 6 || ld-segflags objs (r 5))]) cps)
       hdrsz (52 + 32 * #segs)
       _ (hdrsz > foff ? (scare 'link-bare-headers hdrsz))
       glo (ldsyms objs)
       ; -- the BOARD's own absolutes, (name value) from the driver: the top of the
       ; stack, a peripheral window, whatever the memory map states and no object can
       ; know. they land before the brackets below, so the linker's word about its own
       ; layout is never one a board can quietly replace.
       _ (foldl (\ _ p (pin glo <p [<>p 0], 0)) 0 pns)
       ; -- the copied region's brackets. a startup that has to move .data cannot
       ; work them out: the load address is the linker's alone to know.
       _ (<cps @ (_ . dv . da . df . dm . _)
          (pins glo "__data_start__" [dv 0]
                    "__data_end__"   [(dv + df) 0]
                    "__data_load__"  [(fladdr da) 0]
                    "__bss_start__"  [(dv + df) 0]
                    "__bss_end__"    [(dv + dm) 0]))
       ; -- how many bytes the file LAYS from org. a boot ROM handed a header with a
       ; length field (the i.MX IVT's boot data) is asking this and nothing else, and
       ; only the layout can answer. the last run's file end, since runs lay in map
       ; order; .bss is not in it, which is right -- a ROM copies what exists.
       _ (: pN (last plan)
          (pin glo "__load_size__" [(pN 2 + pN 3 - foff) 0]))
       ; the entry symbol is a COURTESY -- for objdump, gdb and a reader asking what
       ; runs first. the core never consults e_entry; it takes the vector table.
       ent (: e (glo "_start") (two? e ? <e org))
       nif (: r (lpos 'nifs) (two? r ? r [org org 0]))
       cx [glo <nif (<nif + nif 2) org org () () 1 be (ld-extents objs)]
       ; -- the bytes, run by run, lane by lane, in that same order --
       body (: (go rs fp acc)
                (? !(two? rs) [fp acc]
                   (: r <rs
                      pad (fp < r 2 ? [(ld-zeros (r 2 - fp))])
                      e (foldl (a \ l \ ldemit cx l <a objs >a)
                               [(at r 1)] (filter (!== 'bss) (r 5)))
                      (go >rs (r 2 + r 3) (acc + pad + >e))))
               (go plan foff ()))
       fend <body  pieces <>body
       ; -- the section headers and the symbol table, past every loaded byte: ONE
       ; header per mapped lane, so `nm` classifies a symbol as the lane it really
       ; lives in rather than the lane it travels with.
       shl (filter (l \ member? l (ld-uselanes objs)) maplanes)
       lanesx (: t {} (foldl (\ i l (pin t l i, i + 1)) 1 shl, t))
       st (ldstab objs glo lanesx ld-sym32 16)
       shstr (o-strtab (map ld-lanenm shl + '(".symtab" ".strtab" ".shstrtab")))
       (snm k) (<>shstr k)
       nl (tally shl)
       symo (ld-align 4 fend) symz (tally <st)
       stro (symo + symz) strz #<>st
       ssto (stro + strz) sstz #<shstr
       shto (ld-align 4 (ssto + sstz))
       sht (catall ([(ld-shdr32 0 0 0 0 0 0 0 0 0 0)]                        ; the reserved null header
                    + o-mapi (\ i l (: q (peep lpos l '(0 0 0))
                                       (ld-shdr32 (snm i) (l == 'bss ? 8 1)
                                                  (ld-secflags (ld-homeof objs l))
                                                  <q <>q (at q 2) 0 0 4 0)))
                             shl
                    + [(ld-shdr32 (snm nl) 2 0 0 symo symz (nl + 2) (at st 2) 4 16)    ; link=.strtab
                       (ld-shdr32 (snm (nl + 1)) 3 0 0 stro strz 0 0 1 0)
                       (ld-shdr32 (snm (nl + 2)) 3 0 0 ssto sstz 0 0 1 0)]))
       hdr (elf32-hdrn 2 ent mach [shto (nl + 4) (nl + 3)]
                       (map (g \ elf32-phdr 1 (g 5) (g 2) <g (g 1) (g 3) (g 4) 4096) segs))
       (hdr . ([(ld-zeros (foff - hdrsz))] + pieces
               + [(ld-zeros (symo - fend)) <st <>st <shstr
                  (ld-zeros (shto - (ssto + sstz))) sht])))

   ; one byte of a piece, whichever shape the piece arrived in (the header comes back
   ; as a text from one hdr builder and as a byte list from another)
   (ld-brand p i v)
    (? (string? p) (snip p 0 i + string v + snip p (i + 1) #p)
       (: (go k l) (? (k = i) (v . >l) (<l . (go (k + 1) >l))) (go 0 p)))

   ; write a piece list to a file, raw: texts pass through, byte lists pack into a
   ; CASK. an explicit left walk -- map runs its effects right-to-left.
   ; ⚠ THE PATH IS UNLINKED FIRST, never merely truncated. a file being executed
   ; refuses to open for writing (ETXTBSY), so relinking a binary that is running
   ; -- this tree relinks its own love -- opens nothing and `say` to a () port is
   ; quiet, which lays the new bytes nowhere and leaves the old ones in place. the
   ; unlink is what a linker does for this: the live process keeps the old inode
   ; and the new bytes land on a fresh one. it also means an output that cannot be
   ; opened is ABSENT rather than stale, so the next build makes it again.
   ; -> bytes written | a negative errno, as copyfile answers. the errno is
   ; `open`'s own and passes straight through; the CALLER names itself in the
   ; sentence a reader gets, so a program that could not write its output says so
   ; and stops rather than reporting the build it did not do.
   ; ⚠ test it as (n < 0), never for truth: a zero-byte write is falsy too.
   (ld-write path ps)
    (: _ (unlink path)
       q (open path "w")
       (go l n) (? !(two? l) (: _ (close q) n)
                   (: p <l
                      _ (say q (string? p ? p (cask p)))
                      (go >l (n + #p))))
       (? q (go ps 0) q)))
) ; module 'holo
