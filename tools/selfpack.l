; tools/selfpack.l -- cut the source archive from the tree itself.
;
;   love tools/selfpack.l ARCHIVE.tar.gz TOP [STAMP [DROP..]]
;
; the ONE cutter, every world: the tree on disk is the source, checkout and
; seed-laid tree alike. pack it under TOP (love-<ver>) -- out/ bin/ dl/ and
; everything HIDDEN at the root (.git .sb .claude .cache .gitignore ..) are
; the machine's and the checkout's, never the source's, doc/misc is the design
; record rather than the product, and each DROP nom is left out by name -- with a sorted walk and every mtime pinned to STAMP, so
; the same files answer the same bytes wherever they sit. test_distboot
; holds a re-cut to the byte.
;
; AND THE ONE STAMP: the sha of the leveled tar is the tree's content id, kept
; in ARCHIVE.sha. run every make (a force rule); the gzip is paid only when the
; tree's content actually moved -- a touch settles, a delete or rename is seen.
; the walk PRUNES before it reads (sp-keep?), and the coder is host/deflate.c
; through gz.l's door, so a full cut is seconds -- on a runner that carries the
; nifs; love0 carries none, which is why the recipe prefers the tree's own love.
;
; ⚠ AND THE QUIET RUN NEVER BUILDS THE ARCHIVE. Every make asks the same question
; -- has the tree moved? -- and the answer used to cost a full cut: read every
; file, pack ten megabytes, hash them, drop them. The thin pass (tar-thin? +
; tar-hash) answers it as a STREAM instead: stat the tree, then hash the archive
; those entries DESCRIBE, a bufferful at a time, holding neither the bodies nor
; the tar. Same id, same rule -- content, never an mtime -- for 175 MB against
; 293 MB and about half the time. The cut below still reads each file once, and
; it is the cut that writes the sidecar, so what .sha names is always the archive
; that was written and never the pass that only looked.
(use 'tar)
(use 'gz)

(: (sp-die m) (say err ("selfpack: " + m + "\n"), quit 1)
   (sp-read p) (: q (open p "r") (? q (: s (slurp q) _ (close q) `(1 (s + ""))) ()))
   (sp-write p s) (: q (open p "w")
                     _ (? q 0 (sp-die ("cannot write " + p)))
                     _ (say q s)
                     (close q))
   sp-skip '("out" "bin" "dl")
   ; ..and the ones matched WHOLE, for a directory that sits inside one the archive
   ; keeps. doc/misc is the tree's design record and its sketches; doc/ itself stays,
   ; because the man pages are written in doc/{love,cook,lush}.md and `make install`
   ; in an unpacked release generates all three from them.
   sp-inner '("doc/misc")
   (sp-top nom) (: n (tally nom)
                   (go i) (? (n <= i) nom (nom i = 47) (snip nom 0 i) (go (i + 1)))
                   (go 0))
   ; hidden at the root = the machine's, not the source's
   (sp-hid? nom) (: seg (sp-top nom) (tally seg && (peep seg 0 0 = 46)))
   ; keep a nom unless its top segment is a build product or hidden, or the whole
   ; nom is one of sp-inner or a DROP. handed to the WALK, not run after it: what
   ; the archive will not carry is never read (out/ and dl/ are gigabytes on a
   ; build box, and a rejected directory is never descended into at all).
   (sp-keep? drops nom)
    (! (member? (sp-top nom) sp-skip || sp-hid? nom
        || member? nom sp-inner || member? nom drops))
   ; re-home the kept entries under TOP -- the walk is already sorted, so order
   ; is preserved.
   (sp-take es top acc)
    (? !(two? es) (rev acc)
       (: e <es
          _ (pin e 'nom (top + "/" + (tar-get e 'nom "")))
          (sp-take >es top (e >< acc))))

   ; the tree root leads the walk; TOP goes in behind it wearing the root's mode,
   ; and the rest is re-homed under TOP. shared, so the thin pass and the cut level
   ; the same entries the same way and their digests are comparable.
   (sp-lev es top stamp)
    (: root <es
       te (tar-entry top 'dir "")
       _ (pin te 'mode (tar-get root 'mode 493))
       (tar-level (root >< (te >< (sp-take >es top ()))) stamp))

   ; the cut that reads: pack, hash, and write when the content actually moved.
   (sp-cut arch side top stamp drops)
    (: g (tar-gather? (sp-keep? drops) "." "")
       _ (? g 0 (sp-die "cannot read the tree"))
       lev (sp-lev <>g top stamp)
       a (tar-pack lev)
       _ (? a 0 (sp-die "cannot pack (a path too long for ustar?)"))
       h (sha256 a)
       old (sp-read side)
       have (sp-read arch)
       fresh (two? old && two? have && (= (<(>old)) h))
       ; an archive with no sidecar (a seed-laid tree, a fresh checkout of out/):
       ; adopt it when its content IS this content -- no rewrite, so its mtime
       ; holds and nothing downstream re-links.
       adopt (? (fresh || !(two? have)) 0
                (: u (gz-unzip (<(>have))) (two? u && (= (<(>u)) a))))
       (? fresh 0
          adopt (sp-write side h)
          (: z (gz-zip a "" 0)
             _ (sp-write arch z)
             _ (sp-write side h)
             _ (say out ("selfpack: " + show (tally lev) + " entries -> " + arch + "\n"))
             0)))

   (sp-main av)
    (: n (tally av)
       _ (? (2 <= n) 0 (sp-die "usage: selfpack ARCHIVE.tar.gz TOP [STAMP [DROP..]]"))
       arch <av
       top (<(>av))
       stamp (? (3 <= n) (<(sound (<(>(>av))))) 0)
       drops (? (3 <= n) (>(>(>av))) ())
       side (arch + ".sha")
       ; THE THIN PASS: stat the tree, hash the archive it describes, and settle the
       ; run when that id is the one the sidecar already names and the archive is
       ; still there. `stat` and not a read -- the question is whether the file
       ; EXISTS, and its three megabytes have nothing to say about that.
       ; ⚠ a () is not an answer. a torn read (a file that moved under the walk) or
       ; a nom too long for ustar answers one, and both fall through to the cut,
       ; which reads each file once and so cannot disagree with itself.
       t (tar-thin? (sp-keep? drops) "." "")
       _ (? t 0 (sp-die "cannot read the tree"))
       hq (tar-hash (sp-lev <>t top stamp))
       old (sp-read side)
       quiet ((string? hq) && (two? old) && (= (<(>old)) hq) && (two? (stat arch)))
       (? quiet 0 (sp-cut arch side top stamp drops)))

   (sp-main >argv))
