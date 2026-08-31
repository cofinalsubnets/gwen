; tools/mksrc.l -- lay the source tarball into an ELF object, so the artifact can
; hand out its own source with nothing on the box.
;
;   love tools/mksrc.l archive.tar.gz out.o [arch]     arch defaults to x64
;
; the object holds two symbols in .rodata and nothing else:
;
;   ai_srcgz_len   an 8-byte little-endian length, at offset 0 (so it is 8-aligned)
;   ai_srcgz       the .tar.gz bytes, right behind it
;
; src/src.c declares both weak and reads them, so an ordinary `love` links with no
; blob at all and answers nothing when asked -- only the dist link supplies one.
;
; no linker work buys this. `__start_/__stop_` brackets exist for a region packed
; across many objects (the nif lane), and link.l's ldsynth is a fixed list of them; a
; blob is one object of known size, so ordinary globals land in the symbol book like
; any other and nothing in the linker has to learn a new name.
; no (use 'holo) here: that resolves off a nest (~/.love), which a fresh
; seed tree does not have. make hands us holo the way it hands mksys its own:
; -l $(ho)/.mksys-cat.l (kore text/core/asbook + holo elf/obj), the lane
; sys.o already rides.

(? (lit? (from 'holo 'objelf)) (use 'holo) 0) ; splice the assembler when this love is baked; the guard below says so when not

(: (mksrc-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) [1 (s + "")]) ()))
   (mksrc-die m) (: _ (say err ("mksrc: " + m + "\n")) (quit 1))

   ; text -> the byte list `raw` takes. Built from the tail so the walk is a tail call
   ; and the list comes out in order; ~0.25 s over a 3.6 MB archive.
   (mksrc-bytes s i acc) (? (i < 0) acc (mksrc-bytes s (i - 1) ((s i) >< acc)))
   ; a little-endian 8-byte word, the width src/src.c reads it at
   (mksrc-le8 v i acc) (? (i = 8) acc (mksrc-le8 (v >> 8) (i + 1) (acc + [(v & 255)])))

   ; a cross arch's backend joins at runtime (obj.l's law: registered, not
   ; merely named -- the baked holo carries the native one only). the text
   ; rides the tree beside us; make runs this from the root. and an unbaked
   ; love has no holo at all -- say so, instead of a bare ;; note.
   (mksrc-be arch)
    (? (lit? (from 'holo 'objelf)) (mksrc-be2 arch)
       (mksrc-die "no holo aboard -- this love is unbaked (make host bakes it)"))
   (mksrc-be2 arch)
    (? (lit? (peep backends arch ())) 0
       ; the backend file is the canonical arch word now -- one spelling, no map
       (: f ("crew/holo/" + string arch + ".l")
          q (open f "r")
          (? q (: _ (mksrc-rdev (flow q)) (close q))
             (mksrc-die ("no backend aboard and no text at " + f)))))
   (mksrc-rdev cl)
    (: r (sound cl)
       (? (two? r) (: _ (ev 'ev (cap r)) (mksrc-rdev (cup r)))
          (id? r 'torn) (mksrc-die "torn backend text")
          0))

   (mksrc arch src dst)
    (: _ (mksrc-be arch)
       g (mksrc-slurp src)
       _ (? (two? g) 0 (mksrc-die ("cannot read " + src)))
       s <>g
       n (tally s)
       _ (? (0 < n) 0 (mksrc-die (src + " is empty")))
       forms ['(label ai_srcgz_len) ['raw (mksrc-le8 n 0 ())]
              '(label ai_srcgz)     ['raw (mksrc-bytes s (n - 1) ())]]
       ob (objelf arch () () () '("ai_srcgz" "ai_srcgz_len") () () () () () forms)
       _ (write-bytes dst ob)
       (say out ("MKSRC\t" + dst + " (" + (show n) + " bytes)\n")))

   ; `argv` is a list and its head is this script's own path, so the arguments
   ; start at the tail -- tools/tgz.l says the same thing at its own foot.
   ; and the arch arrives as TEXT: `name` interns it, so the answer is the very
   ; symbol the backend table is keyed by. `mint` makes a fresh one instead --
   ; equal-looking, `id?` to nothing, and objsecs refuses it as an unknown target one
   ; call later.
   (mksrc-main av)
    (: n (tally av)
       (? (2 <= n) (mksrc (? (3 <= n) (name <>>av) 'amd64) <av <>av)
          (mksrc-die "usage: mksrc ARCHIVE.tar.gz OUT.o [ARCH]")))

   (mksrc-main >argv))
