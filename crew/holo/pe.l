; crew/holo/pe.l -- the pe32+ container: lay obj.l relocatables into a uefi
; application (subsystem 10) instead of an ELF. same lay/patch heart as
; link.l's ldlink (this file rides after it in a cat -- ld-read/ldlay/ldpatch/
; ldsyms all bound there); only the clothing differs. the whole image is one
; rwx .text section (text/rodata/data/nifs contiguous, bss the zero tail the
; loader grants past SizeOfRawData), plus .reloc: the abs64 sites ldpatch
; collects for the -pie lane, re-encoded as base relocations -- firmware slides
; the image wherever it likes and fixes every pointer itself, no self-reloc.
; entry is the first object's text (the mkefi.l thunk). `target` is ldkern's
; word, and the coff machine below is the only arch-bound thing here: the dir64
; base relocations included, everything else reads the same on both.
;
; two load sections, page-aligned apart, and the split is not decoration:
; firmware protects a loaded image by section, and every section it finds
; executable it also makes read-only. one rwx section therefore comes back as
; ro+X, and the first write to a static takes a permission fault -- which is
; what aarch64 firmware does and x86 firmware happens not to. so .text carries
; text + rodata and no write bit, .data carries data + nifs + bss and no
; execute bit, and the protection lands where it was aimed.
;
; the one address subtlety: ldlay reserves a header inside the load image (an
; ELF's own header rides there); pe headers live below the first section, so
; pelink asks for a reserve of zero and the first text byte lands exactly at
; base = ImageBase + 0x1000.

(module 'holo
(: pe-ibase 65536                        ; the preferred base; .reloc frees the loader from it
   pe-hraw 512                           ; file bytes reserved for every header
   pe-falign 512
   (pe-al a n) ((n + a - 1) & -a)

   (pe-secname s) (map s ^#s + map (_ \ 0) ^(8 - #s))
   (pe-sect nm vsz rva raw off ch)
    (catall [(pe-secname nm) (le32 vsz) (le32 rva) (le32 raw) (le32 off)
             (le32 0) (le32 0) (le16 0) (le16 0) (le32 ch)])

   ;; base relocations: ascending site addresses -> per-page blocks of
   ;; (page-rva u32, block-size u32, entries u16: type 10 = DIR64 high nibble).
   ;; a block with an odd entry count gets a type-0 pad so blocks stay 4-aligned.
   ;; no sites at all still emits one dummy block: an empty-handed .reloc is what
   ;; marks the image relocatable (RELOCS_STRIPPED unset means firmware may slide).
   (pe-relocs sites)
    (? !(two? sites) (catall [(le32 4096) (le32 12) (le16 0) (le16 0)])
       (: (blocks ss acc)
           (? !(two? ss) (rev acc)
              (: pg ((<ss - pe-ibase) & -4096)
                 (grab l es) (two? l && <l - pe-ibase <  pg + 4096 ?
                               (grab >l ((40960 | ((<l - pe-ibase) & 4095)) . es))
                               [l (rev es)])
                 g (grab ss ())
                 es <>g
                 es (#es & 1 ? (es + '(0)) es)
                 (blocks <g (catall [(le32 pg) (le32 (8 + 2 * #es)) (catall (map le16 es))] . acc))))
          (catall (blocks sites ()))))

   ;; the coff machine, elf.l's elf-machines in the other clothing
   pe-machines (tabof [('amd64 . 34404)                          ; 0x8664
                       ('arm64 . 43620)])                       ; 0xaa64

   ;; link a set -- ((id bytes)..), the mkefi thunk first -- into a pe32+ at out.
   (pelink target srcs out)
    (: mach (peep pe-machines target 0)
       emach (peep elf-machines target 0)
       _ (0 < mach ? 0 (scare 'pe-target target))
       objs (map (p \ ld-read <p <>p emach) srcs)
       base (pe-ibase + 4096)
       lay (ldlay objs base 0 4096)       ; the lanes land page-apart: rodata at o0, data at d0
       end (lay 0) n0 (lay 1) o0 (lay 2) d0 (lay 3)
       bend (ldlay1 'bss end objs)
       relbox {0 ()}
       glo (ldsyms objs)
       o1 <objs
       ent (peep (peep o1 'base 0) <(peep (peep o1 'lanes 0) 'text ()) 0)
       cx [glo n0 end end end relbox ()]
       ;; the read-only half. each lane is emitted at the address ldlay gave it,
       ;; so the gap the alignment opened is written out as zeros.
       et (ldemit cx 'text base objs ())
       eo (ldemit cx 'rodata o0 objs (>et + [(ld-zeros (- o0 <et))]))
       ;; ..and the writable half, its own run of bytes from d0.
       ed (ldemit cx 'data d0 objs ())
       en (ldemit cx 'nifs n0 objs (>ed + [(ld-zeros (- n0 <ed))]))
       tfsz (<eo - base)                  ; .text: text + rodata
       traw (pe-al pe-falign tfsz)
       dfsz (<en - d0)                    ; .data: data + nifs, and bss past it
       draw (pe-al pe-falign dfsz)
       text-vsz (d0 - base)
       data-rva (d0 - pe-ibase)
       data-vsz (bend - d0)
       reloc-rva (pe-al 4096 (data-rva + data-vsz))
       reloc (pe-relocs (rev (peep relbox 0 ())))
       rsz #reloc
       rraw (pe-al pe-falign rsz)
       img-sz (reloc-rva + pe-al 4096 rsz)
       opt (catall [
         (le16 523) '(0 0)                               ; pe32+ magic, linker 0.0
         (le32 traw) (le32 draw) (le32 0)
         (le32 (ent - pe-ibase)) (le32 4096)             ; entry rva, base of code
         (le64 pe-ibase)
         (le32 4096) (le32 pe-falign)                    ; section / file alignment
         (le16 0) (le16 0) (le16 0) (le16 0) (le16 0) (le16 0) (le32 0)
         (le32 img-sz) (le32 pe-hraw)
         (le32 0) (le16 10) (le16 0)                     ; subsystem efi_application
         (le64 1048576) (le64 4096) (le64 1048576) (le64 4096)
         (le32 0) (le32 16)
         (le64 0) (le64 0) (le64 0) (le64 0) (le64 0)
         (le32 reloc-rva) (le32 rsz)                     ; dir 5: base relocations
         (le64 0) (le64 0) (le64 0) (le64 0) (le64 0)
         (le64 0) (le64 0) (le64 0) (le64 0) (le64 0)])
       hdr (catall [
         '(77 90) (map (_ \ 0) ^58) (le32 64)       ; mz, e_lfanew = 64
         '(80 69 0 0)
         (le16 mach) (le16 3)                            ; the coff machine, three sections
         (le32 0) (le32 0) (le32 0)
         (le16 #opt) (le16 34)                    ; exe | large_address_aware
         opt
         (pe-sect ".text" tfsz 4096 traw pe-hraw 1610612768)           ; 0x60000020 code|r-x
         (pe-sect ".data" data-vsz data-rva draw (pe-hraw + traw) 3221225536)    ; 0xc0000040 data|rw-
         (pe-sect ".reloc" rsz reloc-rva rraw (pe-hraw + traw + draw) 1107296320)])   ; 0x42000040
       _ (pe-hraw < #hdr ? (scare 'pe-headers-overflow #hdr))
       ;; headers, the read-only half padded to its file size, then the writable
       ;; half padded to its own, then .reloc. `+` folds right, so the chunks
       ;; concatenate in the order written.
       (ld-write out
          ([hdr (ld-zeros (pe-hraw - #hdr))]
           + >eo + [(ld-zeros (traw - tfsz))]
           + >en + [(ld-zeros (draw - dfsz)) reloc (ld-zeros (rraw - rsz))]))))
) ; module 'holo
