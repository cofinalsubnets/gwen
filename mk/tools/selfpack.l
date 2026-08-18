; mk/tools/selfpack.l -- cut the source archive from the tree itself.
;
;   love mk/tools/selfpack.l ARCHIVE.tar.gz TOP [STAMP [DROP..]]
;
; the ONE cutter, every world: the tree on disk is the source, checkout and
; seed-laid tree alike. pack it under TOP (love-<ver>) -- out/ bin/ dl/ and
; everything HIDDEN at the root (.git .sb .claude .cache .gitignore ..) are
; the machine's and the checkout's, never the source's, and each DROP nom is
; left out by name -- with a sorted walk and every mtime pinned to STAMP, so
; the same files answer the same bytes wherever they sit. test_distboot
; holds a re-cut to the byte.
;
; AND THE ONE STAMP: the sha of the leveled tar is the tree's content id, kept
; in ARCHIVE.sha. run every make (a force rule); the gzip is paid only when the
; tree's content actually moved -- a touch settles, a delete or rename is seen.
; the walk PRUNES before it reads (tar-gather? + sp-keep?), and the coder is
; host/deflate.c through gz.l's door, so a full cut is seconds -- on a runner
; that carries the nifs; love0 carries none, which is why the recipe prefers
; the tree's own love.
(use 'tar)
(use 'gz)

(: (sp-die m) (say err ("selfpack: " + m + "\n"), quit 1)
   (sp-read p) (: q (open p "r") (? q (: s (slurp q) _ (close q) `(1 (s + ""))) ()))
   (sp-write p s) (: q (open p "w")
                     _ (? q 0 (sp-die ("cannot write " + p)))
                     _ (say q s)
                     (close q))
   sp-skip '("out" "bin" "dl")
   (sp-top nom) (: n (tally nom)
                   (go i) (? (n <= i) nom (nom i = 47) (snip nom 0 i) (go (i + 1)))
                   (go 0))
   ; hidden at the root = the machine's, not the source's
   (sp-hid? nom) (: seg (sp-top nom) (tally seg && (peep seg 0 0 = 46)))
   ; keep a nom unless its top segment is a build product or hidden, or the
   ; whole nom is a DROP. handed to the WALK, not run after it: what the archive
   ; will not carry is never read (out/ and dl/ are gigabytes on a build box).
   (sp-keep? drops nom)
    (! (member? (sp-top nom) sp-skip || sp-hid? nom || member? nom drops))
   ; re-home the kept entries under TOP -- the walk is already sorted, so order
   ; is preserved.
   (sp-take es top acc)
    (? !(two? es) (rev acc)
       (: e <es
          _ (pin e 'nom (top + "/" + (tar-get e 'nom "")))
          (sp-take >es top (e >< acc))))

   (sp-main av)
    (: n (tally av)
       _ (? (2 <= n) 0 (sp-die "usage: selfpack ARCHIVE.tar.gz TOP [STAMP [DROP..]]"))
       arch <av
       top (<(>av))
       stamp (? (3 <= n) (<(sound (<(>(>av))))) 0)
       drops (? (3 <= n) (>(>(>av))) ())
       g (tar-gather? (sp-keep? drops) "." "")
       _ (? g 0 (sp-die "cannot read the tree"))
       es <>g
       root <es                             ; the "." entry leads the walk
       te (tar-entry top 'dir "")           ; TOP itself, wearing the tree root's mode
       _ (pin te 'mode (tar-get root 'mode 493))
       body (sp-take >es top ())
       lev (tar-level (root >< (te >< body)) stamp)
       a (tar-pack lev)
       _ (? a 0 (sp-die "cannot pack (a path too long for ustar?)"))
       h (sha256 a)
       side (arch + ".sha")
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

   (sp-main >argv))
