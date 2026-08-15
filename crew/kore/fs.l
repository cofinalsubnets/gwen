; crew/kore/fs.l -- the fs tools of kore: ls, cp, mv, rm, mkdir, rmdir, ln,
; touch, pwd, chmod. every tool a <name>-main dispatched by kore (crew/kore/kore.l),
; same as core.l -- a toolbox, no seat, kore is its door. loads after core.l in the
; cat and rides its u-floor (udie, uread, ubase); the effects ride host/posix.c's
; posix_ lane (stat readdir unlink mkdir rename symlink hardlink rmdir
; chmod utime cwd), whose convention shapes every loop here: an effect op nets
; truthy exactly when something went wrong. a body-less top-level `:` leaking
; uoct (lawed in law.l) and the mains.
(:
   ; octal text -> a charm ("755" is 493), chmod's one argument shape
   (uoct s)
    (: n (tally s)
       (go i acc) (? (i < n) (go (i + 1) ((* 8 acc) + (s i - 48))) acc)
       (go 0 0))
   ; is the path a directory (stat follows links, like the tools want)
   (udirp q) (: st (stat q) (two? st && (16384 = (& 61440 <>>st))))
   ; a file destination: into a directory the source's basename lands beside
   (udest src dst) (? (udirp dst) (dst + "/" + ubase src "") dst)
   ; copy bytes + permission bits (cp's heart; mv's cross-device leg)
   ; the bytes, by whichever door this love has. ⚠ KORE RUNS IN TWO WORLDS AND THEY DO NOT
   ; CARRY THE SAME NIFS: free/kernel.mk bakes this whole cat verbatim (korecat) and the
   ; kernel compiles no host/*.c, so `copyfile` is hosted-only and the slurp stays as the
   ; freestanding leg. ⚠ ASKED AT THE CALL, never bound at the top: the kernel's image is
   ; baked by a HOST love, so a load-time answer would freeze this world's nif set into
   ; that one's.
   (ucopy-bytes src dst)
    (? (member? 'copyfile (names ()))
       (0 <= copyfile src dst)
       (: r (uread src)
          (? (two? r) (: q (open dst "w")
                         (? q (: _ (say q <>r) _ (close q) 1) 0))
             0)))
   ; ⚠ the stat leads so the two failures stay tellable apart: one errno covers either end,
   ; and `cp: cannot open X` vs `cannot create Y` is the whole diagnostic. it is the same
   ; stat the mode comes from, so it costs nothing it did not already spend.
   (ucopy who src dst)
    (: st (stat src)
       _ (? (two? st) () (udie 1 (who + ": cannot open " + src)))
       _ (? (ucopy-bytes src dst) () (udie 1 (who + ": cannot create " + dst)))
       _ (chmod dst (& 4095 <>>st))
       1)

   ; -- pwd: where we stand --
   (pwd-main _) (: _ (say out (cwd ())) _ (put out 10) _ (flush out) 0)

   ; -- ls: a directory's entries, sorted, dotfiles held back (-a shows them);
   ; a file argument answers its own name --
   (ls-main as)
    (: a? (two? as && (<as = "-a"))
       fs (? a? >as as)
       (one p ok)
        (? !(two? (stat p))
           (: _ (say err ("ls: cannot access " + p)) _ (put err 10) 0)
           !(udirp p) (: _ (say out p) _ (put out 10) ok)
           (: (go m) (? !(two? m) ok
                        (: _ (? (!a? && (<m 0 = 46)) ()
                                (: _ (say out <m) (put out 10)))
                           (go >m)))
              (go (sort (readdir p)))))
       (go a ok) (? (two? a) (go >a (one <a ok))
                    (: _ (flush out) (? ok 0 1)))
       (go (? (two? fs) fs '(".")) 1))

   ; -- cp and mv: a directory destination pulls the basename in beside --
   ; ⚠ -f is TAKEN AND IGNORED, and that is not a silent lie: -f means "do not prompt",
   ; and nothing here ever prompts, so forced IS the behaviour. -p is the other kind --
   ; it changes what lands, so it is honoured. (ucopy already carries the mode; the
   ; mtime is the half that was missing.) A recipe saying `mv -f` fails without this.
   (cpf-split as)                              ; -> (p? rest) off a leading -f/-p cluster
    (: (fl a p) (? !(two? a) `(p a)
                    (member? <a '("-f" "-p" "-fp" "-pf"))
                     (fl >a (? (member? <a '("-p" "-fp" "-pf")) 1 p))
                    `(p a))
       (fl as 0))
   (cp-main as)
    (: q (cpf-split as) p? <q rest <>q
       (? !(two? >rest) (udie 2 "usage: cp [-f] [-p] SRC DST")
          (: src <rest dst (udest src <>rest)
             _ (ucopy "cp" src dst)
             s (? p? (stat src) ())
             _ (? (two? s) (utime dst <>s) 0)
             0)))
   (mv-main as)
    (: rest (<>(cpf-split as))
       (? !(two? >rest) (udie 2 "usage: mv [-f] SRC DST")
          (: src <rest dst (udest src <>rest)
             e (rename src dst)
             (? !e 0
                (e = 18) (: _ (ucopy "mv" src dst) _ (unlink src) 0)   ; EXDEV: the long way
                (udie 1 ("mv: cannot move " + src))))))

   ; -- rm: unlink, -r walks a tree bottom-up, -f keeps quiet about misses --
   (rm-main as)
    (: (fl a r f) (? !(two? a) `(r f a)
                     (<a = "-r") (fl >a 1 f)
                     (<a = "-f") (fl >a r 1)
                     ((<a = "-rf") || (<a = "-fr")) (fl >a 1 1)
                     `(r f a))
       p (fl as 0 0)
       r? <p f? <>p fs <>>p
       (zap q)
        (? (udirp q)
           (: (go m ok) (? !(two? m) ok
                           (: k (zap (q + "/" + <m))
                              (go >m (? k ok 0))))
              ok (go (readdir q) 1)
              e (rmdir q)
              (ok && !e))
           !(unlink q))
       (one q ok)
        (: hit (? r? (zap q) !(unlink q))
           (? hit ok
              f? ok
              (: _ (say err ("rm: cannot remove " + q)) _ (put err 10) 0)))
       (go a ok) (? (two? a) (go >a (one <a ok)) (? ok 0 1))
       (? (two? fs) (go fs 1)
          f? 0
          (udie 2 "usage: rm [-r] [-f] FILE..")))

   ; -- mkdir and rmdir: -p lays every prefix on the way down --
   (mkdir-main as)
    (: p? (two? as && (<as = "-p"))
       fs (? p? >as as)
       (mkp q)                          ; a tolerant mkdir at every slash, then the whole
        (: n (tally q)
           (go k) (? (n <= k) (mkdir q 493)
                     (: _ (? ((0 < k) && (q k = 47)) (mkdir (snip q 0 k) 493) ())
                        (go (k + 1))))
           (go 1))
       (one q ok)
        (? p? (: _ (mkp q)
                 (? (udirp q) ok
                    (: _ (say err ("mkdir: cannot create " + q)) _ (put err 10) 0)))
           (: e (mkdir q 493)
              (? !e ok
                 (: _ (say err ("mkdir: cannot create " + q)) _ (put err 10) 0))))
       (go a ok) (? (two? a) (go >a (one <a ok)) (? ok 0 1))
       (? (two? fs) (go fs 1) (udie 2 "usage: mkdir [-p] DIR..")))
   (rmdir-main as)
    (: (go a ok)
        (? (two? a)
           (: e (rmdir <a)
              (go >a
                  (? !e ok (: _ (say err ("rmdir: cannot remove " + <a)) _ (put err 10) 0))))
           (? ok 0 1))
       (? (two? as) (go as 1) (udie 2 "usage: rmdir DIR..")))

   ; -- ln: two names for one thing (-s the pointing kind, -f moves the old name aside) --
   (ln-main as)
    (: (fl a s f rest) (? !(two? a) `(s f rest)
                          (<a = "-s") (fl >a 1 f rest)
                          (<a = "-f") (fl >a s 1 rest)
                          ((<a = "-sf") || (<a = "-fs")) (fl >a 1 1 rest)
                          `(s f (rest + a)))
       p (fl as 0 0 ())
       s? <p f? <>p rest <>>p
       _ (? (two? >rest) () (udie 2 "usage: ln [-s] [-f] TARGET LINKNAME"))
       _ (? f? (unlink <>rest) ())
       e (? s? (symlink <rest <>rest) (hardlink <rest <>rest))
       (? !e 0 (udie 1 ("ln: cannot link " + <>rest))))

   ; -- touch: be there, and be now --
   (touch-main as)
    (: (one q ok)
        (: _ (? (two? (stat q)) () (: p (open q "a") (? p (close p) ())))
           e (utime q ())
           (? !e ok (: _ (say err ("touch: cannot touch " + q)) _ (put err 10) 0)))
       (go a ok) (? (two? a) (go >a (one <a ok)) (? ok 0 1))
       (? (two? as) (go as 1) (udie 2 "usage: touch FILE..")))

   ; -- chmod: the octal bits onto every named path --
   (chmod-main as)
    (? !(two? >as) (udie 2 "usage: chmod OCTAL FILE..")
       (: m (uoct <as)
          (go a ok)
           (? (two? a)
              (: e (chmod <a m)
                 (go >a
                     (? !e ok (: _ (say err ("chmod: cannot chmod " + <a)) _ (put err 10) 0))))
              (? ok 0 1))
          (go >as 1)))

   ; -- install: copy with the mode set in the same breath (what the build's
   ; place-a-file steps say). -d makes directories (with parents, like -p);
   ; -D lays the destination's parents first; -m OCTAL sets the mode (else 755);
   ; -s (strip) is ACCEPTED AND NOTED -- no stripper rides kore yet, so the copy
   ; keeps its symbol table (a fatter install, never a wrong one) --
   (imkp q)                        ; a tolerant mkdir at every slash (parents only)
    (: n (tally q)
       (go k) (? (k < n) (: _ (? ((0 < k) && (q k = 47)) (mkdir (snip q 0 k) 493) ())
                            (go (k + 1)))
                 ())
       (go 1))
   (install-main as)
    (: (fl a d D m s fs) (? !(two? a) `(d D m s fs)
                            (<a = "-d") (fl >a 1 D m s fs)
                            (<a = "-D") (fl >a d 1 m s fs)
                            ((<a = "-m") && two? >a) (fl >>a d D (uoct <>a) s fs)
                            (<a = "-s") (fl >a d D m 1 fs)
                            (fl >a d D m s (fs + `(<a))))
       p (fl as 0 0 493 0 ())
       d? <p D? <>p m <>>p s? <>>>p fs <>>>>p
       _ (? s? (: _ (say err "install: -s noted, not stripped (no kore stripper yet)") (put err 10)) ())
       (? d? (: (go a ok)
                 (? (two? a)
                    (: _ (imkp <a) e (mkdir <a m)
                       ok2 (? (udirp <a) (: _ (? !e (chmod <a m) ()) ok)
                              (: _ (say err ("install: cannot create " + <a)) _ (put err 10) 0))
                       (go >a ok2))
                    (? ok 0 1))
                 (? (two? fs) (go fs 1) (udie 2 "usage: install -d DIR..")))
          !(two? >fs) (udie 2 "usage: install [-D] [-m OCTAL] [-s] SRC DST")
          (: src <fs dst (udest <fs <>fs)
             _ (? D? (imkp dst) ())
             _ (ucopy "install" src dst)
             _ (chmod dst m)
             0)))

   ; -- readlink: what a symbolic link says (-f chases to an absolute, clean path) --
   (rl-abs q) (? ((0 < tally q) && (q 0 = 47)) q (cwd () + "/" + q))
   (rl-clean q)                    ; lexically: no // . .. (.. pops a segment)
    (: n (tally q)
       (seg i st acc) (? (i < n)
                         (? (q i = 47)
                            (seg (i + 1) (i + 1) (? (st < i) (snip q st i >< acc) acc))
                            (seg (i + 1) st acc))
                         (rev (? (st < i) (snip q st i >< acc) acc)))
       (fold l acc) (? !(two? l) (rev acc)
                       (<l = ".") (fold >l acc)
                       (<l = "..") (fold >l (? (two? acc) >acc acc))
                       (fold >l (<l >< acc)))
       ("/" + ujoinc 47 (fold (seg 0 0 ()) ())))
   (rl-chase q n)                  ; follow link upon link (n bounds a cycle)
    (: t (readlink q)
       (? !(two? (stat q)) ()
          !(string? t) q
          (0 = n) ()
          (rl-chase (? (t 0 = 47) t (rl-clean (rl-abs q + "/../" + t))) (n - 1))))
   (readlink-main as)
    (: f? (two? as && (<as = "-f"))
       fs (? f? >as as)
       _ (? (two? fs) () (udie 2 "usage: readlink [-f] PATH"))
       q <fs
       r (? f? (: c (rl-chase (rl-clean (rl-abs q)) 40) (? (string? c) c ()))
            (: t (readlink q) (? (string? t) t ())))
       (? (string? r) (: _ (say out r) _ (put out 10) _ (flush out) 0) 1))

   ; -- cmp: are two files the same bytes (-s says nothing, the exit code answers) --
   (cmp-main as)
    (: s? (two? as && (<as = "-s"))
       fs (? s? >as as)
       _ (? (two? >fs) () (udie 2 "usage: cmp [-s] A B"))
       ra (uread <fs) rb (uread <>fs)
       (? (!(two? ra) || !(two? rb))
          (: _ (? s? () (: _ (say err ("cmp: cannot read " + (? (two? ra) <>fs <fs))) (put err 10)))
             2)
          (<>ra = <>rb) 0
          (: _ (? s? () (: _ (say out ((<fs + " ") + (<>fs + " differ")))
                           _ (put out 10) (flush out)))
             1))))
