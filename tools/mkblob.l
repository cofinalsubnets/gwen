; tools/mkblob.l -- lay any file into an ELF object under a name of the caller's
; choosing, so a build can carry bytes no source tree holds.
;
;   love tools/mkblob.l IN OUT.o SYM [ARCH]            arch defaults to x64
;
; the object holds two symbols in .rodata and nothing else:
;
;   <SYM>_len   an 8-byte little-endian length, at offset 0 (so it is 8-aligned)
;   <SYM>       the bytes, right behind it
;
; tools/mksrc.l is the same lay under two fixed names, for the one blob every
; artifact carries; this is the door for the rest. src/doom.c reads a pair.
;
; ⚠ NO (use 'holo) here, for mksrc's reason: it resolves off a NEST (~/.love),
; which a fresh seed tree does not have. make hands us holo the way it hands
; mksrc its own -- -l $(ho)/.mksys-cat.l.

(? (lit? (from 'holo 'objelf)) (use 'holo) 0) ; splice the assembler when this love is baked

(: (mkblob-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) [1 (s + "")]) ()))
   (mkblob-die m) (: _ (say err ("mkblob: " + m + "\n")) (quit 1))

   ; text -> the byte list `raw` takes, built from the tail so the walk is a tail call
   (mkblob-bytes s i acc) (? (i < 0) acc (mkblob-bytes s (i - 1) ((s i) >< acc)))
   ; a little-endian 8-byte word, the width the C side reads it at
   (mkblob-le8 v i acc) (? (i = 8) acc (mkblob-le8 (v >> 8) (i + 1) (acc + [(v & 255)])))

   ; a cross arch's backend JOINS at runtime (obj.l's law: registered, not merely
   ; named), and an unbaked love has no holo at all -- say so.
   (mkblob-be arch)
    (? (lit? (from 'holo 'objelf)) (mkblob-be2 arch)
       (mkblob-die "no holo aboard -- this love is unbaked (make host bakes it)"))
   (mkblob-be2 arch)
    (? (lit? (peep backends arch ())) 0
       ; the backend file IS the canonical arch word now -- one spelling, no map
       (: f ("crew/holo/" + string arch + ".l")
          q (open f "r")
          (? q (: _ (mkblob-rdev (flow q)) (close q))
             (mkblob-die ("no backend aboard and no text at " + f)))))
   (mkblob-rdev cl)
    (: r (sound cl)
       (? (two? r) (: _ (ev 'ev (cap r)) (mkblob-rdev (cup r)))
          (id? r 'torn) (mkblob-die "torn backend text")
          0))

   (mkblob arch src dst sym)
    (: _ (mkblob-be arch)
       g (mkblob-slurp src)
       _ (? (two? g) 0 (mkblob-die ("cannot read " + src)))
       s <>g
       n (tally s)
       _ (? (0 < n) 0 (mkblob-die (src + " is empty")))
       lsym (sym + "_len")
       forms [['label (name lsym)] ['raw (mkblob-le8 n 0 ())]
              ['label (name sym)]  ['raw (mkblob-bytes s (n - 1) ())]]
       ob (objelf arch () () () [sym lsym] () () () () () forms)
       _ (write-bytes dst ob)
       (say out ("MKBLOB\t" + dst + " (" + (show n) + " bytes)\n")))

   ; ⚠ `argv` is a LIST whose head is this script's own path, so the arguments
   ; start at the tail. ⚠ the arch arrives as TEXT and `name` interns it, so the
   ; answer is the very symbol the backend table is keyed by.
   (mkblob-main av)
    (: n (tally av)
       (? (3 <= n) (mkblob (? (4 <= n) (name (av 3)) 'amd64) <av <>av <>>av)
          (mkblob-die "usage: mkblob IN OUT.o SYM [ARCH]")))

   (mkblob-main >argv))
