; free/klink.l -- the kernel link, ours. holo's ldkern (crew/holo/link.l)
; under the freestanding image, in place of ld.lld + the per-arch .lds. cat it
; after crew/holo/{elf,obj,link}.l and run the result:
;
;   love <cat> OUT ARCH OBJ.o..
;
; what the linker script used to carry is exactly the four numbers below: the
; ELF target, the entry SYMBOL, the virtual base the image is laid at, and the
; bias between where it runs and where it loads (p_vaddr - p_paddr). every
; other line of <a>.lds -- the five PT_LOADs, the page alignments, kimage_end,
; the .note/.eh_frame discards -- is ldkern's own shape, one lane per arch.
;
; the bias is the whole trick: x86_64 lays the image in the top 2 GiB and loads
; it at the same offset into low RAM (bias = KVMA), while aarch64's virt board
; starts its RAM at 0x40000000, so the same high vaddr loads there instead
; (bias = KVMA - 0x40000000). one ELF serves `qemu -kernel` (copies by p_paddr,
; MMU off), limine and our own BOOTX64.EFI (map by p_vaddr, MMU on).

(: (kparams arch)
    (? (arch = "x86_64")  `('x64   "kmain"   0xffffffff80200000 0xffffffff80000000)
       (arch = "aarch64") `('arm64 "a64boot" 0xffffffff80200000 0xffffffff40000000)
       (scare 'klink-arch arch))
   ; presence out of band: an unreadable object and an EMPTY one are the same
   ; value, and an empty .o here would link a kernel with a silent hole in it.
   (kread p) (: r (uread p) (? (two? r) `(p <>r) (scare 'klink-open p)))
   (: as >argv
      out <as arch <>as objs >>as
      _ (? (two? objs) 0 (scare 'klink-no-objects arch))
      k (kparams arch)
      _ (ld-write out (ldkern <k <>k <>>k (at k 3) (map kread objs)))
      0))
