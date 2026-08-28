; tools/selfpack.l -- cut the source archive from the tree itself.
;
;   love tools/selfpack.l ARCHIVE.tar.gz TOP [STAMP [DROP..]]
;
; the ONE cutter, every world: the tree on disk is the source, checkout and
; seed-laid tree alike. pack it under TOP (love-<ver>) -- everything sb ignores
; (.sb/ignore + its built-ins, so the vcs walk and this cut read ONE list and a
; generated file named there leaves both), plus this cut's own structural skips:
; bin/ dl/, everything HIDDEN at the root (.git .sb .claude .cache .gitignore ..)
; is the machine's and the checkout's and never the source's, doc/misc is the
; design record rather than the product, and each DROP nom is left out by name
; -- with a sorted walk and every mtime pinned to STAMP, so
; the same files answer the same bytes wherever they sit. test_distboot
; holds a re-cut to the byte.
;
; AND THE ONE STAMP: the sha of the leveled tar is the tree's content id, kept
; in ARCHIVE.sha. run every make (a force rule); the gzip is paid only when the
; tree's content actually moved -- a touch settles, a delete or rename is seen.
; the walk PRUNES before it reads (sp-keep?), and the coder is src/deflate.c
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
; named as PATHS, not module names, because love0 has no image to carry them: it is
; gcc-built from the love/*.l headers alone, so `(use 'tar)` under it has no registered
; tablet to find and the loader has no business inventing one. each file's own
; (module 'nm ..) head registers it as it loads, which is all a later `use` needs.
; the order is the dependency order: kore's text+diff floor under sb, dns under sb's http.
;
; a path, never `-l`: `-l FILE` whose basename names a verb SEATS it (love/verbs.l's
; seatof -- the installed shebang's lane), so `-l crew/sb/sb.l` runs sb instead of
; loading it. `use` on a string touches no command line.
(use "lib/tar.l")
(use "lib/gz.l")
(use "crew/kore/text.l")
(use "crew/kore/diff.l")
(use "crew/sb/merge.l")
(use "lib/dns.l")
(use "crew/sb/http.l")
(use "crew/sb/sb.l")   ; for the ignore law only -- .sb/ignore is sb's file

(: (sp-die m) (say err ("selfpack: " + m + "\n"), quit 1)
   (sp-read p) (: q (open p "r") (? q (: s (slurp q) _ (close q) [1 (s + "")]) ()))
   (sp-write p s) (: q (open p "w")
                     _ (? q 0 (sp-die ("cannot write " + p)))
                     _ (say q s)
                     (close q))
   ; THE RULES, AND SB OWNS THEM: .sb/ignore plus sb's own built-ins, so the vcs
   ; walk and the dist cut cannot disagree about what this tree is -- one law, one
   ; file, and a generated file named there drops out of BOTH. then the cut's own
   ; structural skips, which no ignore file may take away: `.*` is hidden at the
   ; ROOT, the machine's and the checkout's and never the source's; doc/misc is
   ; the design record rather than the product (doc/ itself stays -- the man pages
   ; are written in doc/{love,cook,lush}.md and `make install` in an unpacked
   ; release generates all three from them); and each DROP nom by name.
   sp-own '("bin" "dl" "doc/misc" ".*")
   sp-ign? (from 'sb 'ign?)
   (sp-rules drops) (((from 'sb 'igs) 0) + sp-own + drops)
   ; handed to the WALK, not run after it: what the archive will not carry is never
   ; read (out/ and dl/ are gigabytes on a build box, and a rejected directory is
   ; never descended into at all).
   (sp-keep? rules nom) (! (sp-ign? nom rules))
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
   (sp-cut arch side top stamp rules)
    (: g (tar-gather? (sp-keep? rules) "." "")
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
       ; ⚠ TOP joins the drops: a tree never packs an unpacked copy of itself.
       ; a release untarred at the root (a field test's leftover) otherwise
       ; rides the next blob, and the artifact swallows a whole second love.
       drops (top >< (? (3 <= n) (>(>(>av))) ()))
       side (arch + ".sha")
       ; THE THIN PASS: stat the tree, hash the archive it describes, and settle the
       ; run when that id is the one the sidecar already names and the archive is
       ; still there. `stat` and not a read -- the question is whether the file
       ; EXISTS, and its three megabytes have nothing to say about that.
       ; ⚠ a () is not an answer. a torn read (a file that moved under the walk) or
       ; a nom too long for ustar answers one, and both fall through to the cut,
       ; which reads each file once and so cannot disagree with itself.
       rules (sp-rules drops)
       t (tar-thin? (sp-keep? rules) "." "")
       _ (? t 0 (sp-die "cannot read the tree"))
       hq (tar-hash (sp-lev <>t top stamp))
       old (sp-read side)
       quiet ((string? hq) && (two? old) && (= (<(>old)) hq) && (two? (stat arch)))
       (? quiet 0 (sp-cut arch side top stamp rules)))

   (sp-main >argv))
