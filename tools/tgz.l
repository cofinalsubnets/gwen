; tools/tgz.l -- take apart a .tar.gz with nothing but love.
;
;   love tools/tgz.l x ARCHIVE.tar.gz [DIR]  unpack into DIR (default ".")
;   love tools/tgz.l t ARCHIVE.tar.gz        list, one path a line
;
; the verbs are tar's own letters because the muscle memory is worth more than a
; better name. the PACK direction lives in tools/selfpack.l, the one cutter.
(use 'tar)
(use 'gz)

(: (tgz-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) `(1 (s + ""))) ()))
   (tgz-die m) (: _ (say err ("tgz: " + m + "\n")) (quit 1))

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
       v (? n <av "")
       (? (! n) (tgz-die "usage: tgz x|t ARCHIVE.tar.gz [DIR]")
          ((v = "x") && (2 <= n)) (tgz-x (<(>av)) (? (3 <= n) (<(>(>av))) "."))
          ((v = "t") && (2 <= n)) (tgz-t (<(>av)))
          (tgz-die "usage: tgz x|t ARCHIVE.tar.gz [DIR]")))

   ; ⚠ `argv` is a LIST, not a function -- (argv ()) would apply it and answer the
   ; church exponent of nothing. Its head is the script's own path, so the verbs
   ; start at the tail.
   (tgz-main >argv))
