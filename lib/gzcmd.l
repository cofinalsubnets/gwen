; lib/gzcmd.l -- `love gzip`, `love gunzip` and `love zcat`: GNU's flag spelling over
; lib/gz.l's two doors. gz.l is the CODEC; this file is the face -- the options, the
; suffix rules, and the in-place replace that carries the mode and the mtime across.
;
;   gzip [-cdfklnNqrtv] [-1..-9] [-S SUF] [FILE..]    FILE.gz written, FILE removed
;   gunzip [..]  = gzip -d          zcat [..] = gzip -dc
;   -c stdout   -d decompress   -f overwrite an existing output   -k keep the input
;   -l list     -n write no name or time into the header   -N take both back out of it
;   -q quiet    -r walk directories   -t test only   -v say what happened   -S SUF
;
; no FILE (or a bare `-`) is the FILTER: stdin -> stdout, which is what a pipe wants.
; a status like GNU's: 0 clean, 1 something failed, 2 a warning (a name refused).
;
; ⚠ ONE MEMBER PER FILE. gz-unzip reads the trailer off the tail, so a legal
; concatenation (`cat a.gz b.gz`) is refused whole rather than half-read -- GNU reads
; every member, and when something here needs that it belongs in gz.l, not in a face.
; ⚠ THE LEVELS ARE TAKEN AND CHANGE NOTHING: the coder costs every block three ways
; and writes the cheapest, which is one level. -9 in a script is a wish, not an error.
; ⚠ -l's ratio is the DEFLATE PAYLOAD's and not the file's -- GNU takes the header and
; the trailer out before it divides, which is why a small file lists a ratio its bytes
; on disk do not show. The (totals) row is OURS: GNU's own adds the first file's whole
; size to the others' payloads, and reproducing that arithmetic is not worth a line.
; the wrap and the ask: gz rides module 'gz, so the codec's names have to be asked for,
; and the ask has to land in a layer of our own -- unwrapped, riding the dist cat, `use`
; would put gz's names straight back on the cat's shared layer. this file read them off
; that layer before, which worked only while nobody moved a file.
(module 'gzcmd
(use 'gz)

(: gzc-usage "usage: gzip [-cdfklnNqrtv19] [-S SUF] [FILE..]   (gunzip = -d, zcat = -dc)"
   gzc-help "gzip -- the RFC 1952 container in love (lib/gz.l is the codec)
usage: gzip [-cdfklnNqrtv19] [-S SUF] [FILE..]
  -c  write on stdout, keep the files      -d  decompress (gunzip)
  -f  overwrite an existing output         -k  keep the input file
  -l  list a member's sizes and ratio      -n  no name or time in the header
  -N  take the name AND the time back      -q  say nothing on a warning
  -r  walk the named directories           -S  the suffix (.gz unsaid)
  -t  test the members, write nothing      -v  say what happened, on err
  -1 .. -9  taken, and they change nothing: the coder has one level
no file, or a bare -, is the filter: stdin -> stdout"

   ; --- the small floor ------------------------------------------------------
   (gzc-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) [1 (s + "")]) ()))
   (gzc-blurt p s) (: q (open p "w") (? q (: _ (say q s) _ (close q) 1) ()))
   (gzc-moan t m) (? (peep t 'q 0) 0
                     (: _ (say err ("gzip: " + m)) _ (put err 10) (flush err)))
   (gzc-tell t m) (? (peep t 'v 0) (: _ (say err m) _ (put err 10) (flush err)) 0)
   (gzc-pad w s) (? (tally s < w) (gzc-pad w (" " + s)) s)
   (gzc-ends? s x) (: n (tally s) m (tally x) ((m <= n) && (snip s (n - m) n = x)))
   (gzc-base p) (: n (tally p)
                   (go k r) (? (k < n) (go (k + 1) (? (p k = 47) (k + 1) r)) (snip p r n))
                   (go 0 0))
   (gzc-dir p) (: n (tally p)
                  (go k r) (? (k < n) (go (k + 1) (? (p k = 47) (k + 1) r)) r)
                  (go 0 0))                          ; the head INCLUDING its slash, "" for none
   (gzc-dirp? q) (: st (stat q) ((two? st) && (16384 = (& 61440 <>>st))))
   (gzc-worse a b) (? ((a = 1) || (b = 1)) 1 (? ((a = 2) || (b = 2)) 2 0))

   ; --- the member's own header, which -l and -N read and gz-unzip does not tell --
   ; every optional field must be STEPPED OVER even though only FNAME is wanted.
   (gzc-hdr s)                         ; -> (1 stamp name body-seat) | ()
    (: n (tally s)
       (skip i) (? (n <= i) () ((s i) = 0) (i + 1) (skip (i + 1)))
       (? ((n < 18) || !(((s 0 = 31) && (s 1 = 139)) && (s 2 = 8))) ()
          (: flg (s 3)
             stamp (((s 4) + ((s 5) << 8)) + (((s 6) << 16) + ((s 7) << 24)))
             i1 (? (flg & 4) ((10 + 2) + ((s 10) + ((s 11) << 8))) 10)
             r2 (? (flg & 8) (: e (skip i1) (? e [1 (snip s i1 (e - 1)) e] ())) [1 "" i1])
             (? !r2 ()
                (: nom <>r2
                   i3 (? (flg & 16) (skip <>>r2) <>>r2)
                   (? !i3 ()
                      (: i4 (? (flg & 2) (i3 + 2) i3)
                         (? (n < (i4 + 8)) () [1 stamp nom i4]))))))))
   (gzc-isize s) (: n (tally s)
                    (((s (n - 4)) + ((s (n - 3)) << 8))
                     + (((s (n - 2)) << 16) + ((s (n - 1)) << 24))))
   (gzc-pay z) (: h (gzc-hdr z) (? h ((tally z) - <>>>h - 8) 0))   ; the deflate bytes alone

   ; --- the ratio, GNU's `%5.1f%%` ------------------------------------------
   ; ⚠ the tenth is ROUNDED, not truncated: an older gzip truncated, this one rounds,
   ; and a hundred random files were asked which. a tie lands half away from zero,
   ; where C rounds half to even -- a difference an exact .05 could reach and a real
   ; pair of sizes essentially never does.
   (gzc-ratio num den)
    (? (den = 0) "  0.0%"
       (: neg (num < 0) a (? neg (0 - num) num)
          v (// ((a * 1000) + (// den 2)) den)
          txt ((show (// v 10)) + "." + (show (% v 10)) + "%")
          (gzc-pad 6 (? neg ("-" + txt) txt))))

   ; --- -l, the listing ------------------------------------------------------
   gzc-lhead "         compressed        uncompressed  ratio uncompressed_name"
   (gzc-lrow fs un pay nom)
    (((gzc-pad 19 (show fs)) + " " + (gzc-pad 19 (show un)))
     + (" " + (gzc-ratio (un - pay) un) + " " + nom))
   (gzc-list t fs)
    (: suf (peep t 'suf ".gz")
       _ (say out (gzc-lhead + "\n"))
       (one p acc)                     ; acc: (status compressed uncompressed payload)
        (: r (gzc-slurp p)
           (? !r (: _ (gzc-moan t (p + ": No such file or directory"))
                    [(gzc-worse <acc 1) <>acc <>>acc <>>>acc])
              (: s <>r h (gzc-hdr s)
                 (? !h (: _ (gzc-moan t (p + ": not in gzip format"))
                          [(gzc-worse <acc 1) <>acc <>>acc <>>>acc])
                    (: fs2 (tally s) un (gzc-isize s) pay (fs2 - <>>>h - 8)
                       ; ⚠ the name column is the PATH as it was typed, stem and all --
                       ; only -N's stored name arrives without a directory to keep
                       nom (? ((peep t 'N 0) && (0 < tally <>>h)) (gzc-base <>>h)
                              (: st (gzc-stem p suf) (? st <>st p)))
                       _ (say out ((gzc-lrow fs2 un pay nom) + "\n"))
                       [<acc (<>acc + fs2) (<>>acc + un) (<>>>acc + pay)])))))
       a (foldl (\ acc p (one p acc)) '(0 0 0 0) fs)
       _ (? (two? >fs) (say out ((gzc-lrow <>a <>>a <>>>a "(totals)") + "\n")) 0)
       _ (flush out)
       <a)

   ; --- the suffix rules -----------------------------------------------------
   ; .gz (or -S's) comes off; .tgz answers .tar, which is the one rename gzip does.
   (gzc-stem p suf)                    ; -> (1 stem) | ()
    (: n (tally p)
       (? (gzc-ends? p suf) [1 (snip p 0 (n - (tally suf)))]
          (gzc-ends? p ".tgz") [1 ((snip p 0 (n - 4)) + ".tar")]
          ()))

   ; --- one file, compressed -------------------------------------------------
   (gzc-zip1 t p)
    (: suf (peep t 'suf ".gz")
       r (gzc-slurp p)
       (? !r (: _ (gzc-moan t (p + ": No such file or directory")) 1)
          ((gzc-ends? p suf) && !(peep t 'c 0))
           (: _ (gzc-moan t (p + " already has " + suf + " suffix -- unchanged")) 0)
          (: s <>r
             st (stat p)
             ms (? (two? st) <>st 0)
             n? (peep t 'n 0)
             z (gz-zip s (? n? "" (gzc-base p)) (? n? 0 (// ms 1000)))
             (? (peep t 'c 0) (: _ (say out z) _ (flush out) 0)
                (: dst (p + suf)
                   (? ((two? (lstat dst)) && !(peep t 'f 0))
                      (: _ (gzc-moan t (dst + " already exists;\tnot overwritten")) 2)
                      !(gzc-blurt dst z)
                       (: _ (gzc-moan t ("cannot write " + dst)) 1)
                      (: _ (? (two? st) (: _ (chmod dst (& <>>st 4095)) (utime dst ms)) 0)
                         _ (? (peep t 'k 0) 0 (unlink p))
                         _ (gzc-tell t (p + ":\t" + (gzc-ratio ((tally s) - (gzc-pay z)) (tally s))
                                          + (? (peep t 'k 0) " -- created " " -- replaced with ")
                                          + dst))
                         0)))))))

   ; --- one file, decompressed (and -t, which is the same read and no write) --
   (gzc-unzip1 t p)
    (: suf (peep t 'suf ".gz")
       stem0 (gzc-stem p suf)
       ; ⚠ THE SUFFIX IS ASKED BEFORE THE BYTES ARE. A name with nothing to strip is a
       ; warning and not an error, so a `gunzip *` over a mixed directory walks on --
       ; reading first would call every plain file "not in gzip format" instead
       (? ((!(peep t 'c 0) && !(peep t 't 0)) && (!stem0 && !(peep t 'N 0)))
          (: _ (gzc-moan t (p + ": unknown suffix -- ignored")) 2)
       (: r (gzc-slurp p)
       (? !r (: _ (gzc-moan t (p + ": No such file or directory")) 1)
          (: s <>r
             h (gzc-hdr s)
             u (? h (gz-unzip s) ())
             (? !h (: _ (gzc-moan t (p + ": not in gzip format")) 1)
                !u (: _ (gzc-moan t (p + ": invalid compressed data--crc error")) 1)
                (peep t 't 0) (: _ (gzc-tell t (p + ":\t OK")) 0)
                (peep t 'c 0) (: _ (say out <>u) _ (flush out) 0)
                (: stem stem0
                   N? ((peep t 'N 0) && (0 < tally <>>h))
                   (? (!stem && !N?)
                      (: _ (gzc-moan t (p + ": unknown suffix -- ignored")) 2)
                      (: dst (? N? ((gzc-dir p) + (gzc-base <>>h)) <>stem)
                         st (stat p)
                         (? ((two? (lstat dst)) && !(peep t 'f 0))
                            (: _ (gzc-moan t (dst + " already exists;\tnot overwritten")) 2)
                            !(gzc-blurt dst <>u)
                             (: _ (gzc-moan t ("cannot write " + dst)) 1)
                            (: _ (? (two? st) (chmod dst (& <>>st 4095)) 0)
                               ; ⚠ WITHOUT -N the OUTPUT KEEPS THE .gz's OWN mtime and not
                               ; the one in the header: gzip restores the stored stamp only
                               ; when asked, which is what -N means on this side
                               _ (? N? (utime dst (<>h * 1000))
                                    (? (two? st) (utime dst <>st) 0))
                               _ (? (peep t 'k 0) 0 (unlink p))
                               _ (gzc-tell t (p + ":\t" + (gzc-ratio ((tally <>u) - (gzc-pay s))
                                                                     (tally <>u))
                                                + (? (peep t 'k 0) " -- created "
                                                     " -- replaced with ")
                                                + dst))
                               0)))))))))))

   ; --- the walk -------------------------------------------------------------
   (gzc-one t p) (? (peep t 'd 0) (gzc-unzip1 t p) (gzc-zip1 t p))
   (gzc-walk t p)                      ; -r: a directory is its files, sorted
    (? !(gzc-dirp? p) (gzc-one t p)
       (? (peep t 'r 0)
          (foldl (\ acc c (gzc-worse acc (gzc-walk t ((p + "/") + c)))) 0 (sort (readdir p)))
          (: _ (gzc-moan t (p + " is a directory -- ignored")) 2)))

   ; --- the filter: no file named, so stdin -> stdout -------------------------
   (gzc-filter t)
    (: s (slurp in)
       (? (peep t 'd 0)
          (: u (gz-unzip (s + ""))
             (? !u (: _ (gzc-moan t "stdin: not in gzip format") 1)
                (peep t 't 0) 0
                (: _ (say out <>u) _ (flush out) 0)))
          (: _ (say out (gz-zip (s + "") "" 0)) _ (flush out) 0)))

   ; --- the options ----------------------------------------------------------
   ; clustered shorts, the long spellings beside them, and -S's value glued or next.
   ; ⚠ a DIGIT is a level and not a flag: it is taken and dropped, because a script
   ; that says -9 is asking for the best this coder has and that is what it gets.
   (gzc-long t w)                      ; -> 1 taken | 'val -S wants a word | ()
    (: n (tally w)
       (eq k) (: m (tally k) (((m + 1) <= n) && (snip w 0 (m + 1) = (k + "="))))
       (? ((w = "stdout") || (w = "to-stdout")) (pin t 'c 1)
          ((w = "decompress") || (w = "uncompress")) (pin t 'd 1)
          (w = "force") (pin t 'f 1)
          (w = "keep") (pin t 'k 1)
          (w = "list") (pin t 'l 1)
          (w = "no-name") (pin t 'n 1)
          (w = "name") (pin t 'N 1)
          (w = "quiet") (pin t 'q 1)
          (w = "recursive") (pin t 'r 1)
          (w = "test") (pin t 't 1)
          (w = "verbose") (pin t 'v 1)
          ((w = "fast") || (w = "best")) 1
          (w = "help") (pin t 'h 1)
          (w = "version") (pin t 'V 1)
          (eq "suffix") (: _ (pin t 'suf (snip w 7 n)) 1)
          (w = "suffix") 'val
          ()))
   (gzc-opts as t)                     ; -> (1 files) | ()  -- () is a bad option
    (: (go a fs)
        (? !(two? a) [1 (rev fs)]
           (: w <a n (tally w)
              (? (w = "--") [1 ((rev fs) + >a)]
                 ((n < 2) || !(w 0 = 45)) (go >a (w >< fs))     ; a name, and "-" is one
                 (w 1 = 45)
                  (: r (gzc-long t (snip w 2 n))
                     (? !r (: _ (gzc-moan t ("unknown option " + w)) ())
                        (id? r 'val) (? (two? >a) (: _ (pin t 'suf <>a) (go >>a fs))
                                        (: _ (gzc-moan t "--suffix wants a suffix") ()))
                        (go >a fs)))
                 (: (cl k rest)
                     (? (n <= k) (go rest fs)
                        (: c (w k)
                           (? ((48 <= c) && (c <= 57)) (cl (k + 1) rest)   ; a level: taken, dropped
                              (c = 83)                                    ; -S, glued or next
                               (: glue (snip w (k + 1) n)
                                  (? (0 < tally glue) (: _ (pin t 'suf glue) (go rest fs))
                                     (two? rest) (: _ (pin t 'suf <rest) (go >rest fs))
                                     (: _ (gzc-moan t "-S wants a suffix") ())))
                              (member? c '(99 100 102 104 107 108 110 113 114 116 118))
                               (: _ (pin t (intern (string c)) 1) (cl (k + 1) rest))
                              (c = 78) (: _ (pin t 'N 1) (cl (k + 1) rest))
                              (c = 86) (: _ (pin t 'V 1) (cl (k + 1) rest))
                              (: _ (gzc-moan t ("unknown option -" + (string c))) ()))))
                    (cl 1 >a)))))
       (go as ()))

   (gzc-run as t0)
    (: t t0
       o (gzc-opts as t)
       (? !o 1
          (peep t 'h 0) (: _ (say out (gzc-help + "\n")) _ (flush out) 0)
          (peep t 'V 0) (: _ (say out "gzip (love) -- lib/gz.l\n") _ (flush out) 0)
          (: fs (filter (\ p !(p = "-")) <>o)
             _ (? (peep t 'l 0) 0 (? (peep t 't 0) (pin t 'd 1) 0))
             (? !(two? fs) (? (peep t 'l 0)
                              (: _ (gzc-moan t "-l wants a file") 1)
                              (gzc-filter t))
                (peep t 'l 0) (gzc-list t fs)
                (foldl (\ acc p (gzc-worse acc (gzc-walk t p))) 0 fs)))))

   (gzip-main as) (gzc-run as (tablet 0))
   (gunzip-main as) (: t (tablet 0) _ (pin t 'd 1) (gzc-run as t))
   (zcat-main as) (: t (tablet 0) _ (pin t 'd 1) _ (pin t 'c 1) (gzc-run as t))

   (pins (from 'verbs 'tab)
    'gzip   gzip-main
    'gunzip gunzip-main
    'zcat   zcat-main))

) ; module 'gzcmd
