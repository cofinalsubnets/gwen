; mk/tools/mksrc.l -- lay the source tarball into an ELF object, so the artifact can
; hand out its own source with nothing on the box.
;
;   love mk/tools/mksrc.l ARCHIVE.tar.gz OUT.o [ARCH]     arch defaults to x64
;
; the object holds two symbols in .rodata and nothing else:
;
;   ai_srcgz_len   an 8-byte little-endian length, at offset 0 (so it is 8-aligned)
;   ai_srcgz       the .tar.gz bytes, right behind it
;
; host/src.c declares both WEAK and reads them, so an ordinary `love` links with no
; blob at all and answers nothing when asked -- only the dist link supplies one.
;
; ⚠ NO LINKER WORK BUYS THIS. `__start_/__stop_` brackets exist for a region packed
; across MANY objects (the nif lane), and link.l's ldsynth is a fixed list of them; a
; blob is ONE object of known size, so ordinary globals land in the symbol book like
; any other and nothing in the linker has to learn a new name.
(use 'holo)

(: (mksrc-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) `(1 (s + ""))) ()))
   (mksrc-die m) (: _ (say err ("mksrc: " + m + "\n")) (quit 1))

   ; text -> the byte list `raw` takes. Built from the tail so the walk is a tail call
   ; and the list comes out in order; ~0.25 s over a 3.6 MB archive.
   (mksrc-bytes s i acc) (? (i < 0) acc (mksrc-bytes s (i - 1) ((s i) >< acc)))
   ; a little-endian 8-byte word, the width host/src.c reads it at
   (mksrc-le8 v i acc) (? (i = 8) acc (mksrc-le8 (v >> 8) (i + 1) (acc + `((v & 255)))))

   (mksrc arch src dst)
    (: g (mksrc-slurp src)
       _ (? (two? g) 0 (mksrc-die ("cannot read " + src)))
       s <>g
       n (tally s)
       _ (? (0 < n) 0 (mksrc-die (src + " is empty")))
       forms `('(label ai_srcgz_len) `('raw (mksrc-le8 n 0 ()))
               '(label ai_srcgz)     `('raw (mksrc-bytes s (n - 1) ())))
       ob (objelf arch () () () '("ai_srcgz" "ai_srcgz_len") () () () () () forms)
       _ (write-bytes dst ob)
       (say out ("MKSRC\t" + dst + " (" + (show n) + " bytes)\n")))

   ; ⚠ `argv` is a LIST and its head is this script's own path, so the arguments
   ; start at the tail -- mk/tools/tgz.l says the same thing at its own foot.
   ; ⚠ and the arch arrives as TEXT: `name` interns it, so the answer is the very
   ; symbol the backend table is keyed by. `mint` makes a FRESH one instead --
   ; equal-looking, `id?` to nothing, and objsecs refuses it as an unknown target one
   ; call later.
   (mksrc-main av)
    (: n (tally av)
       (? (2 <= n) (mksrc (? (3 <= n) (name <>>av) 'x64) <av <>av)
          (mksrc-die "usage: mksrc ARCHIVE.tar.gz OUT.o [ARCH]")))

   (mksrc-main >argv))
