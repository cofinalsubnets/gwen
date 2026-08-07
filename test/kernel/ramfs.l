; The ramfs, inle's rung 0 (doc/inle.md). The initrd is .rodata -- tools/lcatfs.l
; bakes one {path, bytes, len} row per file into out/lib/kfs.h -- and reads come
; straight off it; the FIRST write copies the blob into the kernel heap and the file
; reads from the copy ever after. So: a baked path opens and reads, an unbaked one
; answers the zero point for a read (a write CREATES since rung 2), and a write
; is visible to the NEXT open, because the copy is per FILE and not per fd.
;
; KERNEL ONLY -- kernel.mk's kt, never $t: on the host `open` is the real
; filesystem and these laws would write the tree. The scratch is a real baked row on
; purpose: lib/json.l is a file the boot never loads, and the write laws put its
; bytes back, so nothing downstream in the corpus sees a clobbered tree.
;
; ⚠ the `:` HAS a body -- the assert itself -- so the helpers stay local; and each
; law is its own assert ARGUMENT, since assert maps over its args and a `do` would
; report only the last one.

(: f "lib/json.l"
   ; slurp a path, closing behind -- () when it would not open, so presence rides
   ; `port?` and never the emptiness of what was read.
   (rd p) (: q (open p "r") (? (port? q) (: s (slurp q) _ (close q) s) ()))
   (wr p s m) (: q (open p m) (? (port? q) (: _ (say q s) _ (close q) 1) 0))
   orig (rd f)
 (assert
   ; a baked path opens, and carries the bytes lcatfs put in the row
   (port? (open f "r"))
   (< 0 (tally orig))
   ; ... and reads the same on the second open: a file does not drain
   (= orig (rd f))
   ; an unbaked path is ABSENT for a read; a write CREATES it now (rung 2 --
   ; wfs.l holds those laws), so here it is unmade the moment it is proved
   (! (port? (open "lib/no-such-thing.l" "r")))
   (: q (open "lib/no-such-thing.l" "w")
      (&& (port? q) (: _ (close q) (id? (unlink "lib/no-such-thing.l") ()))))
   ; the write copies the blob out of .rodata, and the NEXT open sees it
   (wr f "hi" "w")
   (= "hi" (rd f))
   ; ... and append lands past what is there, not over it
   (wr f "!" "a")
   (= "hi!" (rd f))
   ; a truncate drops the copy's bytes without losing the row
   (wr f "" "w")
   (= "" (rd f))
   ; a write bigger than the baked blob grows the copy
   (wr f (+ orig orig) "w")
   (= (+ orig orig) (rd f))
   ; put it back, and the tree reads as the bake left it
   (wr f orig "w")
   (= orig (rd f))))

; THE PAYOFF, and the end-to-end proof the rung is real: prel's module walk is gated
; on the presence of `open` (love/prel.l's fsopen, by peep), so with the row in
; defs[] `use` resolves lib/<x>.l off the ramfs with zero prel change -- json is in
; no baked ai_libs table on this seat, only in the initrd. ⚠ it also reads the
; scratch back through the COMPILER, so it is the restore above proving itself byte
; for byte; it has to stay after those laws.
(use 'json)
(assert (member? 'json-read (names ())))
