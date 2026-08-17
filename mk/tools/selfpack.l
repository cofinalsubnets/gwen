; mk/tools/selfpack.l -- re-cut the source archive from an extracted tree.
;
;   love mk/tools/selfpack.l ARCHIVE.tar.gz TOP [STAMP]
;
; a checkout cuts its archive from the git index; an extracted tree has no
; index, but it IS the staged content laid back on disk. so pack the tree
; itself under TOP (love-<ver>) -- out/ bin/ dl/ .git/ .sb/ are the tree's
; products, never the archive's -- with the same gather, the same sort and
; the same stamp that cut the original, so the same files answer the same
; bytes. test_distboot's binary compare is what holds that to the byte.
(use 'tar)
(use 'gz)

(: (sp-die m) (say err ("selfpack: " + m + "\n"), quit 1)
   sp-skip '("out" "bin" "dl" ".git" ".sb")
   (sp-top nom) (: n (tally nom)
                   (go i) (? (n <= i) nom (nom i = 47) (snip nom 0 i) (go (i + 1)))
                   (go 0))
   ; keep an entry unless its top segment is a build product, and re-home the
   ; kept ones under TOP -- the walk is already sorted, so order is preserved.
   (sp-take es top acc)
    (? !(two? es) (rev acc)
       (: e <es
          nom (tar-get e 'nom "")
          (? (member? (sp-top nom) sp-skip) (sp-take >es top acc)
             (: _ (pin e 'nom (top + "/" + nom))
                (sp-take >es top (e >< acc))))))

   (sp-main av)
    (: n (tally av)
       _ (? (2 <= n) 0 (sp-die "usage: selfpack ARCHIVE.tar.gz TOP [STAMP]"))
       arch <av
       top (<(>av))
       stamp (? (3 <= n) (<(sound (<(>(>av))))) 0)
       g (tar-gather "." "")
       _ (? g 0 (sp-die "cannot read the tree"))
       es <>g
       root <es                             ; the "." entry the stage cut also leads with
       te (tar-entry top 'dir "")           ; TOP itself, wearing the tree root's mode
       _ (pin te 'mode (tar-get root 'mode 493))
       body (sp-take >es top ())
       lev (tar-level (root >< (te >< body)) stamp)
       a (tar-pack lev)
       _ (? a 0 (sp-die "cannot pack (a path too long for ustar?)"))
       z (gz-zip a "" 0)
       q (open arch "w")
       _ (? q 0 (sp-die ("cannot write " + arch)))
       _ (say q z)
       _ (close q)
       _ (say out ("selfpack: " + show (tally lev) + " entries -> " + arch + "\n"))
       0)

   (sp-main >argv))
