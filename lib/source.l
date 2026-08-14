; lib/source.l -- `love source [DIR]`, the artifact handing out its own source.
;
; the seed carries the source tarball in .rodata (tools/mksrc.l lays it,
; host/src.c answers it), so there is no second download and no `tar xf`: the binary
; you ran IS the source distribution. doc/dist.md.
;
;   love source [DIR]    lay ./love-<ver>/ and build in it
;   love seed   [DIR]    lay it, BUILD it, and refuse unless the seed it produces is
;                        this binary byte for byte -- the whole fixpoint, one word
;
; ⚠ IT LAYS bin/love TOO, and that is not a convenience -- it is the point. ./Makefile
; takes a bin/love beside the tree as the C compiler ($(origin CC), bundled_love), so
; the unpacked tree builds with the binary that carried it and touches no ambient cc.
; Source alone would unpack fine and then fall back to whatever compiler the box had,
; which is exactly the claim the seed exists to make good on.
;
; ⚠ the top directory comes from THE ARCHIVE, never from love-version: the tarball's
; own name is what the paths inside it say, and deriving it twice is how the two come
; to disagree on a version whose id ends up dirty.
(: (src-die m) (: _ (say err ("source: " + m + "\n")) (quit 1))
   (src-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) `(1 (s + ""))) ()))

   ; the leading path component of an entry -- everything before the first "/"
   (src-head nom) (: n (tally nom)
                     (go i) (? (n <= i) nom (nom i = 47) (snip nom 0 i) (go (i + 1)))
                     (go 0))
   ; ⚠ the archive's OWN ROOT rides first, as "./" -- tar-gather walks the directory it
   ; was handed and that directory is an entry too. So the top is the first component
   ; that names something, not the first entry; taking entry one lands every path at "."
   ; and lays bin/love beside the tree instead of inside it.
   (src-top es) (? (atom? es) ""
                   (: c (src-head (tar-get <es 'nom ""))
                      (? ((c = ".") || (c = "")) (src-top >es) c)))

   ; ⚠ THE ARCHIVE ITSELF RIDES ALONG, at the path $(dist_source) names. An extracted
   ; tree has no .git, and the stage is `git checkout-index` -- so it cannot RE-CUT the
   ; tarball, and `make dist` there dies "not a git repository". Laying the very bytes we
   ; carried means the rebuilt seed binary embeds the SAME archive, so it comes out
   ; byte-identical by construction rather than by a re-pack that has to coincide.
   (src-lay-tgz root top gz)
    (: _ (mkdir (root + "/out") 493)
       _ (mkdir (root + "/out/dist") 493)
       p (root + "/out/dist/" + top + ".tar.gz")
       q (open p "w")
       _ (? q 0 (src-die ("cannot write " + p)))
       _ (say q gz)
       _ (close q)
       p)

   ; the running binary, laid where the unpacked tree's `make` looks for it
   (src-lay-love root)
    (: exe (readlink "/proc/self/exe")
       _ (? (string? exe) 0 (src-die "cannot read my own path"))
       b (src-slurp exe)
       _ (? (two? b) 0 (src-die ("cannot read " + exe)))
       _ (mkdir (root + "/bin") 493)
       p (root + "/bin/love")
       q (open p "w")
       _ (? q 0 (src-die ("cannot write " + p)))
       _ (say q <>b)
       _ (close q)
       _ (chmod p 493)
       p)

   ; the lay itself, answering (1 root files) -- `seed` builds on exactly this, so the
   ; two verbs cannot drift on where the tree lands or what it contains.
   (src-lay dir)
    (: gz (source-gz "x")
       ; ⚠ `string?`, not `!`: an artifact with no source and one carrying an empty
       ; archive are both nil by net, and only the KIND tells them apart (host/src.c).
       _ (? (string? gz) 0 (src-die "this love carries no source"))
       u (gz-unzip gz)
       _ (? (two? u) 0 (src-die "the embedded archive did not inflate"))
       t (tar-unpack <>u)
       _ (? (two? t) 0 (src-die "the embedded archive is torn"))
       es <>t
       top (src-top es)
       _ (? (0 < tally top) 0 (src-die "the embedded archive names no top directory"))
       w (tar-scatter dir es)
       _ (? (two? w) 0 (src-die ("cannot write under " + dir)))
       root (? (dir = ".") top (dir + "/" + top))
       _ (src-lay-love root)
       _ (src-lay-tgz root top gz)
       [1 root <>w])

   (source-main as)
    (: r (src-lay (? (two? as) <as "."))
       _ (puts ("source: " + show <>>r + " files in " + <>r
                + ", with bin/love and the archive -- `cd " + <>r + " && make`\n"))
       0)

   ; --- seed: the whole fixpoint, run by the artifact ON ITSELF -------------------
   ; `love seed [DIR]` lays the source, builds a new seed with the binary that carried
   ; it, and REFUSES unless what comes out is this binary byte for byte. That is the
   ; release claim -- doc/dist.md's circle, test/gate/distboot.sh's fourth leg -- said as
   ; one word, so somebody holding one file can check it on their own machine.
   ; ⚠ THE COMPILER IS US. CC is set to this binary's own mooncc verb unless the caller
   ; named one, so the build reaches for no ambient toolchain -- which is the half of the
   ; claim a merely-green build would not prove.
   ; ⚠ and it is a REFUSAL, not a report: a fixpoint that failed and exited 0 would be a
   ; worse artifact than one that never claimed it.
   (seed-arch _) (? (love-arch = "arm64") "aarch64" (love-arch = "x64") "x86_64" love-arch)
   (seed-main as)
    (: r (src-lay (? (two? as) <as "."))
       root <>r
       exe (readlink "/proc/self/exe")
       _ (? (string? exe) 0 (src-die "cannot find my own path"))
       here (cwd 0)
       ; ⚠ charm?, never a truth test: chdir answers () on success and the ERRNO on
       ; failure, and an errno is negative -- so it is RED and a truth test reads the
       ; failure as the success. The kind is the only thing that tells them apart.
       _ (? (charm? (chdir root)) (src-die ("cannot enter " + root)) 0)
       _ (? (getenv "CC") 0 (setenv "CC" (exe + " mooncc")))
       _ (puts ("seed: " + show <>>r + " files in " + root
                + " -- building, and the toolchain is this binary\n"))
       s (herald [exe "make" "dist"])
       st (? (two? s) <s s)
       _ (chdir here)
       _ (? (0 = st) 0 (src-die ("the build failed (exit " + show st + ")")))
       out (root + "/out/dist/love-" + seed-arch ())
       a (src-slurp exe)
       _ (? (two? a) 0 (src-die ("cannot read " + exe)))
       ha (sha256 <>a)
       b (src-slurp out)
       _ (? (two? b) 0 (src-die ("cannot read " + out)))
       hb (sha256 <>b)
       (? (ha = hb)
          (: _ (puts ("seed: FIXPOINT -- " + out + "\nseed: is this binary, byte for byte ("
                      + (snip ha 0 16) + "..)\n")) 0)
          (: _ (say err ("seed: FIXPOINT FAILED -- what it built is NOT this binary\n"
                         + "seed:   running " + ha + "\n"
                         + "seed:   built   " + hb + "\n"))
             (quit 1))))

   ; ⚠ one function, two words: `src` is the short spelling and nothing more, so a
   ; reader meeting either finds the same row rather than a wrapper to chase.
   T (from 'verbs 'tab)
   _ (pins T
      'source source-main
      'src    source-main
      'seed   seed-main))
