; lib/source.l -- `love source [DIR]`, the artifact handing out its own source.
;
; the full artifact carries the lean tarball in .rodata (tools/mksrc.l lays it,
; host/src.c answers it), so there is no second download and no `tar xf`: the binary
; you ran IS the source distribution. doc/dist.md.
;
;   love source          lay ./love-<ver>/ and build in it
;   love source DIR      the same under DIR
;
; ⚠ IT LAYS bin/love TOO, and that is not a convenience -- it is the point. ./Makefile
; takes a bin/love beside the tree as the C compiler ($(origin CC), bundled_love), so
; the unpacked tree builds with the binary that carried it and touches no ambient cc.
; Source alone would unpack fine and then fall back to whatever compiler the box had,
; which is exactly the claim the full artifact exists to make good on.
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

   (source-main as)
    (: dir (? (two? as) <as ".")
       gz (source-gz "x")
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
       _ (puts ("source: " + show <>w + " files in " + root
                + ", with bin/love -- `cd " + root + " && make`\n"))
       0)

   ; ⚠ one function, two words: `src` is the short spelling and nothing more, so a
   ; reader meeting either finds the same row rather than a wrapper to chase.
   T (from 'verbs 'tab)
   _ (pins T
      'source source-main
      'src    source-main))
