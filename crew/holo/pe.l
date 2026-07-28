; crew/holo/pe.l -- the PE32+ container: lay obj.l relocatables into a UEFI
; APPLICATION (subsystem 10) instead of an ELF. same lay/patch heart as
; link.l's ldlink (this file rides after it in a cat -- ld-read/ldlay/ldpatch/
; ldsyms all bound there); only the clothing differs. the whole image is ONE
; RWX .text section (text/rodata/data/nifs contiguous, bss the zero tail the
; loader grants past SizeOfRawData), plus .reloc: the abs64 sites ldpatch
; collects for the -pie lane, re-encoded as base relocations -- firmware slides
; the image wherever it likes and fixes every pointer itself, no self-reloc.
; entry is the FIRST object's text (the mkefi.l thunk -- ms_abi in, sysv out),
; x64 only: UEFI on this machine speaks ms_abi x86-64.
;
; the one address subtlety: ldlay lays text at base+120 (ELF's header rides
; inside the load image); PE headers live BELOW the first section, so pelink
; hands ldlay (base - 120) and the first text byte lands exactly at base =
; ImageBase + 0x1000.

(: pe-ibase 65536                        ; the preferred base; .reloc frees the loader from it
   pe-hraw 512                           ; file bytes reserved for every header
   pe-falign 512
   (pe-al a n) (& (+ n (- a 1)) (- 0 a))

   (pe-secname s) (: b (map s (jot (tally s)))
                     (b + (map (\ _ 0) (jot (8 - (tally s))))))
   (pe-sect nm vsz rva raw off ch)
    (catall `((pe-secname nm) (le32 vsz) (le32 rva) (le32 raw) (le32 off)
              (le32 0) (le32 0) (le16 0) (le16 0) (le32 ch)))

   ;; base relocations: ascending site addresses -> per-page blocks of
   ;; (page-rva u32, block-size u32, entries u16: type 10 = DIR64 high nibble).
   ;; a block with an odd entry count gets a type-0 pad so blocks stay 4-aligned.
   ;; no sites at all still emits one dummy block: an empty-handed .reloc is what
   ;; marks the image relocatable (RELOCS_STRIPPED unset means firmware may slide).
   (pe-relocs sites)
    (? !(two? sites) (catall `((le32 4096) (le32 12) (le16 0) (le16 0)))
       (: (blocks ss acc)
           (? !(two? ss) (rev acc)
              (: pg (& (- <ss pe-ibase) (- 0 4096))
                 (grab l es) (? (&& (two? l) (< (- <l pe-ibase) (+ pg 4096)))
                               (grab >l (link (| 40960 (& (- <l pe-ibase) 4095)) es))
                               `(l (rev es)))
                 g (grab ss ())
                 es <>g
                 es (? (& (tally es) 1) (es + '(0)) es)
                 (blocks <g (link (catall `((le32 pg) (le32 (+ 8 (* 2 (tally es)))) (catall (map le16 es)))) acc))))
          (catall (blocks sites ()))))

   ;; link a set -- ((id bytes)..), the mkefi thunk FIRST -- into a PE32+ at out.
   (pelink srcs out)
    (: objs (map (\ p (ld-read <p <>p 62)) srcs)
       base (+ pe-ibase 4096)
       lay (ldlay objs (- base 120))
       end <lay n0 <>lay
       bend (ldlay1 'bss end objs)
       relbox (: t (tablet 0) _ (pin t 0 ()) t)
       glo (ldsyms objs)
       o1 <objs
       ent (peep (peep o1 'base 0) <(peep (peep o1 'lanes 0) 'text ()) 0)
       (emit lane cur os acc)
        (? !(two? os) (cur >< acc)
           (: ob <os
              (go c ac is)
               (? !(two? is) (c >< ac)
                  (: se (peep (peep ob 'secs 0) <is ())
                     b (peep (peep ob 'base 0) <is 0)
                     pad (? (c < b) `((ld-zeros (b - c))) ())
                     (go (b + (at se 2))
                         (ac + pad + (ldpatch ob <is b glo n0 end end end relbox ()))
                         >is)))
              r (go cur acc (peep (peep ob 'lanes 0) lane ()))
              (emit lane <r >os >r)))
       et (emit 'text base objs ())
       eo (emit 'rodata <et objs >et)
       ed (emit 'data <eo objs >eo)
       en (emit 'nifs <ed objs >ed)
       fsz (- end base)
       craw (pe-al pe-falign fsz)
       text-vsz (- bend base)
       reloc-rva (pe-al 4096 (+ 4096 text-vsz))
       reloc (pe-relocs (rev (peep relbox 0 ())))
       rsz (tally reloc)
       rraw (pe-al pe-falign rsz)
       img-sz (+ reloc-rva (pe-al 4096 rsz))
       opt (catall `(
          (le16 523) `(0 0)                              ; PE32+ magic, linker 0.0
          (le32 craw) (le32 0) (le32 0)
          (le32 (- ent pe-ibase)) (le32 4096)            ; entry RVA, base of code
          (le64 pe-ibase)
          (le32 4096) (le32 pe-falign)                   ; section / file alignment
          (le16 0) (le16 0) (le16 0) (le16 0) (le16 0) (le16 0) (le32 0)
          (le32 img-sz) (le32 pe-hraw)
          (le32 0) (le16 10) (le16 0)                    ; subsystem EFI_APPLICATION
          (le64 1048576) (le64 4096) (le64 1048576) (le64 4096)
          (le32 0) (le32 16)
          (le64 0) (le64 0) (le64 0) (le64 0) (le64 0)
          (le32 reloc-rva) (le32 rsz)                    ; dir 5: base relocations
          (le64 0) (le64 0) (le64 0) (le64 0) (le64 0)
          (le64 0) (le64 0) (le64 0) (le64 0) (le64 0)))
       hdr (catall `(
          `(77 90) (map (\ _ 0) (jot 58)) (le32 64)      ; MZ, e_lfanew = 64
          `(80 69 0 0)
          (le16 34404) (le16 2)                          ; 0x8664, two sections
          (le32 0) (le32 0) (le32 0)
          (le16 (tally opt)) (le16 34)                   ; EXE | LARGE_ADDRESS_AWARE
          opt
          (pe-sect ".text" text-vsz 4096 craw pe-hraw 3758096416)      ; 0xE0000020 code|rwx
          (pe-sect ".reloc" rsz reloc-rva rraw (+ pe-hraw craw) 1107296320)))  ; 0x42000040
       _ (? (< pe-hraw (tally hdr)) (scare 'pe-headers-overflow (tally hdr)) 0)
       (ld-write out
          (`(hdr (ld-zeros (- pe-hraw (tally hdr)))) + (>en
           + `((ld-zeros (- craw fsz)) reloc (ld-zeros (- rraw rsz))))))))
