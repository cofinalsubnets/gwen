; mk/tools/mkrt.l -- lay the compiled nolibc runtime into an ELF object, one
; raw archive per hosted ISA, so a bare `love cc` links without compiling
; 197 members first (moon.l's rtcarried; host/src.c answers runtime-gz).
;
;   love wake out/host/mooncc0.image mk/tools/mkrt.l OUT.o [ARCH]
;
; the object holds, per name in {id, x64, arm64, riscv64}, mksrc.l's pair
; shape 8-aligned:
;
;   ai_rtgz_<nm>_len   an 8-byte little-endian length
;   ai_rtgz_<nm>       the bytes, right behind it (a raw .a; `id` is
;                      moon.l's rtcid of the tree slice, plain text)
;
; host/src.c declares every symbol WEAK, so a love carrying no runtime (love0)
; answers nothing and the compile-from-source lane stands. ⚠ determinism is
; load-bearing: arbytes and rtcid are all pure of the machine, so the
; seed's fixpoint carries this object unchanged.
; ⚠ this RIDES THE MOONCC IMAGE (wake), not the mksys cat: the archives ARE
; mooncc compiles, so the whole compiler must be aboard; ARCH names the object
; format (the LINK's arch), and a cross backend joins from its tree text the
; way mksrc.l joins one.

(: (rt-die m) (: _ (say err ("mkrt: " + m + "\n")) (quit 1))
   (rt-rdev cl)
    (: r (sound cl)
       (? (two? r) (: _ (ev 'ev (cap r)) (rt-rdev (cup r)))
          (id? r 'torn) (rt-die "torn backend text")
          0))
   (rt-be arch)
    (? (lit? (peep (from 'holo 'backends) arch ())) 0
       (: f ("crew/holo/" + (arch @ 'arm64 "arm64" 'riscv64 "riscv" "x64") + ".l")
          q (open f "r")
          (? q (: _ (rt-rdev (flow q)) (close q))
             (rt-die ("no backend aboard and no text at " + f)))))
   (rt-bytes s i acc) (? (i < 0) acc (rt-bytes s (i - 1) ((s i) >< acc)))
   (rt-le8 v i acc) (? (i = 8) acc (rt-le8 (v >> 8) (i + 1) (acc + `((v & 255)))))
   (rt-zeros k acc) (? (k < 1) acc (rt-zeros (k - 1) (0 >< acc)))
   ; one member: label len, label bytes, zero-padded so the NEXT len lands
   ; 8-aligned (a riscv core may trap an unaligned word; the pad is free)
   (rt-forms nm s)
    (: n (tally s)
       pad ((8 - (n & 7)) & 7)
       `(`('label (name ("ai_rtgz_" + nm + "_len")))
         `('raw (rt-le8 n 0 ()))
         `('label (name ("ai_rtgz_" + nm)))
         `('raw (rt-bytes s (n - 1) (rt-zeros pad ())))))

   (mkrt arch dst)
    ; every ARCHIVE arch joins, not just the object's: the image bakes the
    ; native backend only, and each ISA's members ride its own obj writer
    (: _ (rt-be 'x64) _ (rt-be 'arm64) _ (rt-be 'riscv64) _ (rt-be arch)
       rtcid (from 'moon 'rtcid)
       rtarch (from 'moon 'rtarch)
       objelf (from 'holo 'objelf)
       cid (rtcid "crew/moon/")
       _ (? (string? cid) 0 (rt-die "no crew/moon/ slice to hash -- run from the tree root"))
       ; THROUGH rtarch, so the runtime cache serves a repeat build (~instant)
       ; and only a real slice or compiler change recompiles the members --
       ; without this every seed and rebake paid three full runtime compiles.
       mcid (from 'moon 'mcid)
       id (mcid "crew/moon/")
       cd (? (string? id) ((from 'moon 'mcdir) ()) ())
       ; RAW, not deflated: ~1.5 MB across the three ISAs, and rtcarried reads
       ; the bytes straight -- nothing to inflate on the bare `love cc` path.
       (one tgt) (: a (rtarch cd id "crew/moon/" tgt 'linux ())
                    _ (? (string? a) 0 (rt-die (show tgt + ": no archive (a member refused?)")))
                    _ (say out ("MKRT\t" + show tgt + " " + show (tally a) + "\n"))
                    a)
       forms ((rt-forms "id" cid)
              + (rt-forms "x64" (one 'x64))
              + (rt-forms "arm64" (one 'arm64))
              + (rt-forms "riscv64" (one 'riscv64)))
       xs '("ai_rtgz_id" "ai_rtgz_id_len"
            "ai_rtgz_x64" "ai_rtgz_x64_len"
            "ai_rtgz_arm64" "ai_rtgz_arm64_len"
            "ai_rtgz_riscv64" "ai_rtgz_riscv64_len")
       ob (objelf arch () () () xs () () () () () forms)
       _ ((from 'holo 'write-bytes) dst ob)     ; ob is a BYTE LIST, not a string
       (say out ("MKRT\t" + dst + "\n")))

   (mkrt-main av)
    (: n (tally av)
       (? (1 <= n) (mkrt (? (2 <= n) (name <>av) 'x64) <av)
          (rt-die "usage: love wake out/host/mooncc0.image mk/tools/mkrt.l OUT.o [ARCH]")))

   (mkrt-main >argv))
