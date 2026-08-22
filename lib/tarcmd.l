; lib/tarcmd.l -- `love tar`, tar's own flag spelling over lib/tar.l + lib/gz.l.
;
;   love tar czf A.tar.gz DIR     pack DIR's contents (paths relative)
;   love tar xzf A.tar.gz [DIR]   unpack into DIR (default ".")
;   love tar tzf A.tar.gz         list, one path a line
;
; tools/tgz.l is the same two engines behind c|x|t; this is the spelling a hand types.
; ⚠ `z` IS READ, not assumed: tgz's door is always gzip because a .tar.gz is the one
; thing it exists to make, but a `tar cf` that quietly gzipped would be an archive
; whose name lies about its bytes. Without z the ustar rides plain.
;
; ⚠ our DEFLATE costs every block three ways -- stored, fixed, and a code built from
; the block's own frequencies -- and lands a few percent above `gzip -9`. The honest
; numbers live in lib/gz.l.
; the wrap: `use` SPLICES a module's names into the current layer, so these two asks
; have to land in a layer of our own -- unwrapped, riding the dist cat, they would put
; tar's and gz's names straight back on the cat's shared layer, which is the leak the
; module wraps over there just took out. the ask is explicit either way: this file rode
; that leak before and reads their names by the door now.
(module 'tarcmd
(use 'gz)
(use 'tar)

(: (tarc-die m) (: _ (say err ("tar: " + m + "\n")) (quit 1))
   (tarc-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) `(1 (s + ""))) ()))
   (tarc-blurt p s) (: q (open p "w") (? q (: _ (say q s) _ (close q) 1) ()))
   (tarc-has? fl c) (: n (tally fl)
                       (go k) (? (n <= k) 0 (fl k = c) 1 (go (k + 1)))
                       (go 0))

   (tarc-c arch dir z?)
    (: g (tar-gather dir "")
       _ (? !g (tarc-die ("cannot read " + dir)) 0)
       a (tar-pack <>g)
       _ (? !a (tarc-die "cannot pack (a path too long for ustar?)") 0)
       b (? z? (gz-zip a "" 0) a)
       _ (? (tarc-blurt arch b) 0 (tarc-die ("cannot write " + arch)))
       0)

   ; ⚠ each read is guarded on its own so the message NAMES the step: "not a gzip" and
   ; "not a tar" are different repairs, and one "cannot read" sends you to the wrong one.
   (tarc-open arch z?)
    (: b (tarc-slurp arch)
       _ (? !b (tarc-die ("cannot read " + arch)) 0)
       u (? z? (gz-unzip <>b) `(1 <>b))
       _ (? !u (tarc-die (arch + ": not a gzip stream, or its crc32 disagrees")) 0)
       r (tar-unpack <>u)
       _ (? !r (tarc-die (arch + ": not a tar archive, or it is truncated")) 0)
       <>r)

   (tarc-x arch dir z?)
    (: es (tarc-open arch z?)
       w (tar-scatter dir es)
       _ (? !w (tarc-die ("cannot write under " + dir)) 0)
       0)

   (tarc-t arch z?)
    (: es (tarc-open arch z?)
       _ (each es (\ e (say out (peep e 'nom "" + "\n"))))
       0)

   usage "usage: tar czf|xzf|tzf ARCHIVE [DIR]"
   (tar-main av)
    (: n (tally av)
       fl (? n <av "")
       z? (tarc-has? fl 122)                       ; z
       a (? (2 <= n) <>av "")
       d (? (3 <= n) <>>av ".")
       (? (n < 2) (tarc-die usage)
          (tarc-has? fl 99) (? (n < 3) (tarc-die usage) (tarc-c a d z?))     ; c
          (tarc-has? fl 120) (tarc-x a d z?)                                 ; x
          (tarc-has? fl 116) (tarc-t a z?)                                   ; t
          (tarc-die usage)))

   T (from 'verbs 'tab)
   _ (pins T 'tar tar-main))

) ; module 'tarcmd
