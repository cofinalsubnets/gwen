; free/klink.l -- the kernel link, ours. holo's ldkern (crew/holo/link.l) under
; the freestanding image. cat it after crew/holo/{elf,obj,link}.l and run the result:
;
;   love <cat> OUT ARCH OBJ.o..
;
; why a kernel image needs a bias at all -- one ELF, three doors -- is link.l's
; kernel lane.

(: (kparams arch)         ; ldkern's target, entry, vaddr, bias; vaddr - bias = where it loads
    (? (arch = "x86_64")  ['x64   "kmain"   0xffffffff80200000 0xffffffff80000000]  ; -> 0x200000
       (arch = "aarch64") ['arm64 "a64boot" 0xffffffff80200000 0xffffffff40000000]  ; -> 0x40200000, qemu -M virt's RAM base + 2M
       (scare 'klink-arch arch))
   ; presence out of band: an unreadable object and an EMPTY one are the same
   ; value, and an empty .o here would link a kernel with a silent hole in it.
   (kread p) (: r (uread p) (two? r ? [p <>r] (scare 'klink-open p)))
   (: as >argv
      out <as arch <>as objs >>as
    (atom? objs ? (scare 'klink-no-objects arch),
     kparams arch @ (a b c d) (ld-write out * ldkern a b c d $ map kread objs),
     0)))
