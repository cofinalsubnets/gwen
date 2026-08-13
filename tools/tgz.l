; tools/tgz.l -- make and take apart a .tar.gz with nothing but love.
;
;   love tools/tgz.l c ARCHIVE.tar.gz DIR    pack DIR (its contents, paths relative)
;   love tools/tgz.l x ARCHIVE.tar.gz [DIR]  unpack into DIR (default ".")
;   love tools/tgz.l t ARCHIVE.tar.gz        list, one path a line
;
; the verbs are tar's own letters because the muscle memory is worth more than a
; better name. `z` is not among them: this door is always gzip'd, since a .tar.gz
; is the one thing it exists to make.
;
; ⚠ this writes what lib/gz.l can write, which is the FIXED Huffman code -- a valid
; DEFLATE stream every gunzip reads, and about 24% larger than `gzip -9` on source.
; For a download that is a real cost and it is the honest number; see lib/gz.l.
(use 'tar)
(use 'gz)

(: (tgz-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) `(1 (s + ""))) ()))
   (tgz-blurt p s) (: q (open p "w") (? q (: _ (say q s) _ (close q) 1) ()))
   (tgz-die m) (: _ (say err ("tgz: " + m + "\n")) (quit 1))

   (tgz-c arch dir)
    (: g (tar-gather dir "")
       _ (? (! g) (tgz-die ("cannot read " + dir)) 0)
       a (tar-pack (<(>g)))
       _ (? (! a) (tgz-die "cannot pack (a path too long for ustar?)") 0)
       z (gz-zip a "" 0)
       _ (? (tgz-blurt arch z) 0 (tgz-die ("cannot write " + arch)))
       _ (say out ("tgz: " + (show (tally (<(>g)))) + " entries, "
                   + (show (tally a)) + " -> " + (show (tally z)) + " bytes -> " + arch + "\n"))
       0)

   ; ⚠ every read here is guarded separately so the message NAMES which step failed:
   ; "not a gzip" and "not a tar" are different repairs, and one "cannot read" for
   ; both would send you to the wrong one.
   (tgz-open arch)                          ; -> entries, or quits
    (: b (tgz-slurp arch)
       _ (? (! b) (tgz-die ("cannot read " + arch)) 0)
       u (gz-unzip (<(>b)))
       _ (? (! u) (tgz-die (arch + ": not a gzip stream, or its crc32 disagrees")) 0)
       r (tar-unpack (<(>u)))
       _ (? (! r) (tgz-die (arch + ": not a tar archive, or it is truncated")) 0)
       (<(>r)))

   (tgz-x arch dir)
    (: es (tgz-open arch)
       w (tar-scatter dir es)
       _ (? (! w) (tgz-die ("cannot write under " + dir)) 0)
       _ (say out ("tgz: " + (show (<(>w))) + " entries -> " + dir + "\n"))
       0)

   (tgz-t arch)
    (: es (tgz-open arch)
       _ (each es (\ e (say out ((peep e 'nom "") + "\n"))))
       0)

   (tgz-main av)
    (: n (tally av)
       v (? n (<av) "")
       (? (! n) (tgz-die "usage: tgz c|x|t ARCHIVE.tar.gz [DIR]")
          ((v = "c") & (3 <= n)) (tgz-c (<(>av)) (<(>(>av))))
          ((v = "x") & (2 <= n)) (tgz-x (<(>av)) (? (3 <= n) (<(>(>av))) "."))
          ((v = "t") & (2 <= n)) (tgz-t (<(>av)))
          (tgz-die "usage: tgz c|x|t ARCHIVE.tar.gz [DIR]")))

   ; ⚠ `argv` is a LIST, not a function -- (argv ()) would apply it and answer the
   ; church exponent of nothing. Its head is the script's own path, so the verbs
   ; start at the tail.
   (tgz-main >argv))
