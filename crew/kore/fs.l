; crew/kore/fs.l -- the fs tools of kore: ls, cp, mv, rm, mkdir, rmdir, ln,
; touch, pwd, chmod. every tool a <name>-main dispatched by kore (crew/kore/kore.l),
; same as core.l -- a toolbox, no seat, kore is its door. loads after core.l in the
; cat and rides its u-floor (udie, uread, ubase); the effects ride host/posix.c's
; posix_ lane (stat readdir unlink mkdir rename symlink hardlink rmdir
; chmod utime cwd), whose convention shapes every loop here: an effect op nets
; truthy exactly when something went wrong. a body-less top-level `:` leaking
; uoct (lawed in law.l) and the mains.
(module 'kore
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

   ; -- realpath: a path with every symlink, every . and every .. taken out of it.
   ; the walk resolves COMPONENT BY COMPONENT, so a last name that does not exist yet
   ; still answers -- GNU's default face. -e wants the whole path to be there, -m
   ; allows any of it to be missing, -s takes the links as they lie --
   (rp-parts p) (filter (\ w (0 < tally w)) (usplit 47 p))
   (rp-canon q n)                      ; the resolved path, or () past n links
    (: (walk acc parts d)
        (? !(two? parts) `(1 acc)
           (0 = d) ()
           (: p <parts
              (? (p = ".") (walk acc >parts d)
                 (p = "..") (walk (rl-clean (acc + "/..")) >parts d)
                 (: cand (? (acc = "/") ("/" + p) (acc + "/" + p))
                    t (readlink cand)
                    (? !(string? t) (walk cand >parts d)
                       (walk (? (t 0 = 47) "/" acc) (rp-parts t + >parts) (d - 1)))))))
       (walk "/" (rp-parts (rl-abs q)) n))
   (realpath-main as)
    (: t (tablet 0)
       o (uopts as t () ())
       fs <>>o
       _ (? (two? fs) () (udie 1 "usage: realpath [-e|-m|-s] PATH.."))
       e? (peep t 101 0) m? (peep t 109 0) s? (peep t 115 0)
       (one q ok)
        (: r (? s? `(1 (rl-clean (rl-abs q))) (rp-canon q 40))
           (? !(two? r)
              (: _ (say err ("realpath: " + q + ": too many levels of symbolic links"))
                 _ (put err 10) 0)
              (: p <>r
                 want (? m? () (? e? p (udir p)))    ; what has to be there to answer
                 (? ((id? want ()) || (two? (stat want)))
                    (: _ (say out p) _ (put out 10) ok)
                    (: _ (say err ("realpath: " + q + ": No such file or directory"))
                       _ (put err 10) 0)))))
       (go a ok) (? (two? a) (go >a (one <a ok)) (: _ (flush out) (? ok 0 1)))
       (go fs 1))

   ; -- link and unlink: the two syscalls said plainly, no face on them (ln and rm
   ; are where the faces live) --
   (link-main as)
    (? !(two? >as) (udie 1 "usage: link FILE NEW")
       (: e (hardlink <as <>as)
          (? !e 0 (udie 1 ("link: cannot link " + <>as + " to " + <as)))))
   (unlink-main as)
    (? !(two? as) (udie 1 "usage: unlink FILE")
       (: e (unlink <as)
          (? !e 0 (udie 1 ("unlink: cannot unlink " + <as)))))

   ; ---- the reporting tools: stat, du -- and chown, mktemp ---------------------
   ; ⚠ THESE READ THE STAT TAIL. host/posix.c's stat answers
   ; (size mtime mode ns uid gid nlink blocks ino), append-only, and the KERNEL's own
   ; stat (free/kmain.c) answers the first four alone -- an image tree has no ownership
   ; to tell about. so the tail is asked by TALLY and a world that does not carry it
   ; says so rather than reading a 0 someone might believe.
   (ust-at st k) (? (tally st <= k) () (st k))
   ; /etc/passwd and /etc/group, read as the files they are (no NSS, no getpwnam)
   (uidname path id)                   ; a numeric id -> its name, or the number back
    (: r (uread path)
       (? !(two? r) (show id)
          (: (go ls) (? !(two? ls) (show id)
                        (: f (usplit 58 <ls)
                           (? ((3 <= tally f) && (uatoi (f 2) = id)) (f 0) (go >ls))))
             (go (lines <>r)))))
   (uidof path nm)                     ; a name -> (1 id), or () when the file has none
    (: r (uread path)
       (? !(two? r) ()
          (: (go ls) (? !(two? ls) ()
                        (: f (usplit 58 <ls)
                           (? ((3 <= tally f) && (f 0 = nm)) `(1 (uatoi (f 2)))
                              (go >ls))))
             (go (lines <>r)))))
   ; every group a user belongs to: the primary first, then the /etc/group rows naming
   ; them, ASCENDING -- which is the order the kernel keeps its credential list in, and
   ; so the order `id` prints
   (ugroups nm gid)
    (: r (uread "/etc/group")
       (go ls acc)
        (? !(two? ls) acc
           (: f (usplit 58 <ls)
              g (? (4 <= tally f) (uatoi (f 2)) -1)
              ms (? (4 <= tally f) (usplit 44 (f 3)) ())
              (go >ls (? ((member? nm ms) && !(g = gid)) (g >< acc) acc))))
       rest (? (two? r) (sort (go (lines <>r) ())) ())
       (gid >< rest))

   ; -- stat: the file's own facts, by -c FORMAT. %n %N %s %b %B %f %a %A %F %u %U %g %G
   ; %h %i %Y %y, the \n \t \\ escapes, and %% . -L follows a symbolic link; the bare
   ; face does not. ⚠ THERE IS NO DEFAULT BLOCK. GNU's is four lines of access, change
   ; and birth times and a device number, and this stat carries none of them -- printing
   ; a block with the modify time standing in three times over would be a fabrication,
   ; so the tool asks for -c instead of inventing one.
   (ust-kindc m) (: k (& m 61440)
                    (? (k = 32768) "-" (k = 16384) "d" (k = 40960) "l" (k = 8192) "c"
                       (k = 24576) "b" (k = 4096) "p" (k = 49152) "s" "?"))
   (ust-kind m sz) (: k (& m 61440)
                      (? (k = 32768) (? sz "regular file" "regular empty file")
                         (k = 16384) "directory" (k = 40960) "symbolic link"
                         (k = 8192) "character special file"
                         (k = 24576) "block special file"
                         (k = 4096) "fifo" (k = 49152) "socket" "weird file"))
   (ust-perm m)                        ; the ls face: drwxr-sr-t, the three set bits told
    (: (tri sh sb ch)
        (: b (% (// m (sh 2)) 8)
           x (0 < (& b 1))
           s (0 < (& m sb))
           (((? (0 < (& b 4)) "r" "-") + (? (0 < (& b 2)) "w" "-"))
            + (? s (? x ch (string ((ch 0) - 32))) (? x "x" "-"))))
       ((ust-kindc m) + ((tri 6 2048 "s") + ((tri 3 1024 "s") + (tri 0 512 "t")))))
   (ustamp ns)                         ; %y: 2026-08-17 13:32:41.073610912 +0000
    (: s (// ns 1000000000) frac (% ns 1000000000)
       ymd (ucivil (// s 86400)) secs (% s 86400)
       (p2 v) (uzpad 2 (show v))
       ((((show <ymd) + "-" + (p2 <>ymd) + "-" + (p2 <>>ymd)) + " ")
        + (((p2 (// secs 3600)) + ":" + (p2 (// (% secs 3600) 60)) + ":" + (p2 (% secs 60)))
           + ("." + (uzpad 9 (show frac)) + " +0000"))))
   ; ⚠ -c DOES NOT READ ESCAPES -- `stat -c 'a\tb'` prints the backslash and the t, and
   ; adds a newline of its own; --printf reads them and adds none. GNU's split, and the
   ; only way a format can carry a literal backslash at all.
   (ust-expand fmt p st esc?)
    (: n (tally fmt) o (jug 0)
       sz <st md <>>st ns (st 3)
       uid (ust-at st 4) gid (ust-at st 5) nlk (ust-at st 6)
       blk (ust-at st 7) ino (ust-at st 8)
       (num v) (? (charm? v) (show v) "?")
       (go k)
        (? (n <= k) (slurp o)
           (: c (fmt k)
              (? ((esc? && (c = 92)) && ((k + 1) < n))
                 (: d (fmt (k + 1))
                    _ (? (d = 110) (put o 10) (d = 116) (put o 9) (d = 92) (put o 92)
                         (: _ (put o 92) (put o d)))
                    (go (k + 2)))
                 !((c = 37) && ((k + 1) < n)) (: _ (put o c) (go (k + 1)))
                 (: d (fmt (k + 1))
                    _ (? (d = 37) (put o 37)
                         (d = 110) (say o p)
                         (d = 78) (say o ("'" + p + "'"))
                         (d = 115) (say o (show sz))
                         (d = 98) (say o (num blk))
                         (d = 66) (say o "512")
                         (d = 102) (say o (uradix md 16))
                         (d = 97) (say o (uradix (& md 4095) 8))
                         (d = 65) (say o (ust-perm md))
                         (d = 70) (say o (ust-kind md sz))
                         (d = 117) (say o (num uid))
                         (d = 85) (say o (? (charm? uid) (uidname "/etc/passwd" uid) "?"))
                         (d = 103) (say o (num gid))
                         (d = 71) (say o (? (charm? gid) (uidname "/etc/group" gid) "?"))
                         (d = 104) (say o (num nlk))
                         (d = 105) (say o (num ino))
                         (d = 89) (say o (show (// ns 1000000000)))
                         (d = 121) (say o (ustamp ns))
                         (: _ (put o 37) (put o d)))
                    (go (k + 2))))))
       (go 0))
   (stat-main as)
    (: (long? w n s) ((n < tally w) && (snip w 0 n = s))
       (fl a fmt L pf fs)
        (? !(two? a) `(fmt L pf (rev fs))
           (<a = "-L") (fl >a fmt 1 pf fs)
           (<a = "-c") (? (two? >a) (fl >>a <>a L 0 fs) (udie 2 "stat: -c wants a format"))
           ((2 < tally <a) && (snip <a 0 2 = "-c")) (fl >a (snip <a 2 (tally <a)) L 0 fs)
           (long? <a 9 "--format=") (fl >a (snip <a 9 (tally <a)) L 0 fs)
           (long? <a 9 "--printf=") (fl >a (snip <a 9 (tally <a)) L 1 fs)
           (fl >a fmt L pf (<a >< fs)))
       p (fl as () 0 0 ())
       fmt <p L? <>p pf? <>>p fs (p 3)
       _ (? ((string? fmt) && two? fs) ()
            (udie 2 "usage: stat [-L] -c FORMAT | --printf=FORMAT   FILE.."))
       (one q ok)
        (: st (? L? (stat q) (lstat q))
           (? !(two? st)
              (: _ (say err ("stat: cannot stat '" + q + "': No such file or directory"))
                 _ (put err 10) 0)
              (: _ (say out (ust-expand fmt q st pf?)) _ (? pf? 0 (put out 10)) ok)))
       (go a ok) (? (two? a) (go >a (one <a ok)) (: _ (flush out) (? ok 0 1)))
       (go fs 1))

   ; -- du: what a tree costs on the disk. the count is st_blocks (512 each), which is
   ; ALLOCATION and not size -- a sparse file costs less than it measures and a tiny one
   ; costs a whole block -- and the report is in 1K units, rounded up, unless -b asks for
   ; apparent bytes or -h for the three-figure human face. -a counts files too, -s only
   ; the totals, -c a grand total, -d N stops descending. ⚠ a hard link is counted ONCE
   ; per run, keyed by inode alone: this stat carries no device, so a walk crossing two
   ; filesystems could collide two inodes -- rare, and named here rather than hidden.
   ; ⚠ THE WALK SORTS EACH DIRECTORY, like find's, so the same tree reports twice alike.
   (uhuman n)                          ; GNU's -h: three significant figures, rounded UP
    (: units '("K" "M" "G" "T" "P")
       (pick k) (? ((4 < k) || (n < ((* 10 (k + 1)) 2))) k (pick (k + 1)))
       k (? (n < 1024) 0 (pick 1))
       d ((* 10 k) 2)
       (? (k = 0) (show n)
          (: tenths (// ((* n 10) + (d - 1)) d)
             whole (// (n + (d - 1)) d)
             u (units (k - 1))
             (? (tenths < 100) ((show (// tenths 10)) + "." + (show (% tenths 10)) + u)
                ((show whole) + u)))))
   (du-main as)
    (: (fl a t fs) (? !(two? a) `(t (rev fs))
                      (<a = "-d") (? (two? >a) (: _ (pin t 'd (uatoi <>a)) (fl >>a t fs))
                                     (udie 2 "du: -d wants a depth"))
                      ((tally <a < 2) || !(<a 0 = 45)) (fl >a t (<a >< fs))
                      (: w <a
                         (cl k) (? (tally w <= k) (fl >a t fs)
                                   (: c (w k)
                                      (? (c = 97) (: _ (pin t 'a 1) (cl (k + 1)))
                                         (c = 115) (: _ (pin t 's 1) (cl (k + 1)))
                                         (c = 99) (: _ (pin t 'c 1) (cl (k + 1)))
                                         (c = 98) (: _ (pin t 'b 1) (cl (k + 1)))
                                         (c = 104) (: _ (pin t 'h 1) (cl (k + 1)))
                                         (c = 107) (cl (k + 1))       ; -k IS the default
                                         (c = 100) (: _ (pin t 'd (uatoi (snip w (k + 1) (tally w))))
                                                      (fl >a t fs))
                                         (udie 2 ("du: unknown option -" + string c)))))
                         (cl 1)))
       t (tablet 0)
       p (fl as t ())
       fs (? (two? <>p) <>p '("."))
       a? (peep t 'a 0) s? (peep t 's 0) c? (peep t 'c 0)
       b? (peep t 'b 0) h? (peep t 'h 0) maxd (peep t 'd ())
       seen (tablet 0)
       (fmt n) (? h? (uhuman n) (? b? (show n) (show (// (n + 1023) 1024))))
       (emit n q) (: _ (say out (fmt n)) _ (put out 9) _ (say out q) (put out 10))
       ; ⚠ -b's apparent size counts a FILE's st_size and a directory's not at all --
       ; GNU's rule, and not a guess: an empty directory whose st_size is 40 reports 0.
       (own st dir?)
        (: nlk (ust-at st 6) ino (ust-at st 8)
           ct (? b? (? dir? 0 <st) (* 512 (? (charm? (ust-at st 7)) (ust-at st 7) 0)))
           (? ((charm? nlk) && (1 < nlk))
              (? (peep seen ino 0) 0 (: _ (pin seen ino 1) ct))
              ct))
       (walk q depth top?)
        (: st (lstat q)
           (? !(two? st)
              (: _ (say err ("du: cannot access '" + q + "'")) _ (put err 10)
                 _ (pin t 'bad 1) 0)
              (: dir? (16384 = (& 61440 <>>st))
                 kids (? dir?
                         (foldl (\ acc c (acc + walk ((q + "/") + c) (depth + 1) 0))
                                0 (sort (readdir q)))
                         0)
                 tot ((own st dir?) + kids)
                 deep? (? (charm? maxd) (maxd < depth) 0)
                 _ (? (((dir? || a?) || top?) && (!s? && !deep?)) (emit tot q) ())
                 tot)))
       (go a acc) (? !(two? a) acc
                     (: n (walk <a 0 1)
                        _ (? s? (emit n <a) ())
                        (go >a (acc + n))))
       total (go fs 0)
       _ (? c? (emit total "total") ())
       _ (flush out)
       (? (peep t 'bad 0) 1 0))

   ; -- chown: OWNER[:GROUP] onto every named path, -R down the tree. a name is looked
   ; up in /etc/passwd / /etc/group, a numeral is taken as itself, and an empty side is
   ; -1, which the kernel reads as "leave this one alone" --
   (uowner spec)                       ; "u:g" / "u" / ":g" -> (uid gid), -1 for absent
    (: ps (usplit 58 spec)
       (side w path) (? !(0 < tally w) -1
                        (: r (uidof path w)
                           (? (two? r) <>r
                              (: c (w 0) (? ((48 <= c) && (c <= 57)) (uatoi w)
                                             (udie 1 ("chown: invalid user: '" + spec + "'")))))))
       `((side <ps "/etc/passwd") (? (two? >ps) (side <>ps "/etc/group") -1)))
   (chown-main as)
    (: r? (two? as && (<as = "-R"))
       rest (? r? >as as)
       _ (? (two? >rest) () (udie 2 "usage: chown [-R] OWNER[:GROUP] FILE.."))
       ug (uowner <rest)
       uid <ug gid <>ug
       (one q ok)
        (: e (chown q uid gid)
           ok2 (? !e ok
                  (: _ (say err ("chown: cannot chown '" + q + "'")) _ (put err 10) 0))
           (? (r? && (udirp q))
              (foldl (\ acc c (one ((q + "/") + c) acc)) ok2 (readdir q))
              ok2))
       (go a ok) (? (two? a) (go >a (one <a ok)) (? ok 0 1))
       (go >rest 1))

   ; -- mktemp: a name nobody else has, MADE and not merely proposed. the trailing run
   ; of X's in the template becomes random alphanumerics and the name is claimed with an
   ; exclusive create (openfd's mode 3) or an exclusive mkdir under -d, so the answer is
   ; a fact by the time it is printed. -u drops the file again (the unsafe face, and
   ; GNU calls it that too), -p DIR / -t put the name under a directory --
   umtchars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
   (umtname tmpl)                      ; the X run filled with fresh charms
    (: n (tally tmpl)
       (e k) (? ((0 < k) && (tmpl (k - 1) = 88)) (e (k - 1)) k)
       en (e n)
       o (jug 0)
       _ (say o (snip tmpl 0 en))
       (go k) (? (n <= k) () (: _ (put o (umtchars (rand 62))) (go (k + 1))))
       _ (go en)
       (slurp o))
   (mktemp-main as)
    (: (fl a d u q dir fs)
        (? !(two? a) `(d u q dir (rev fs))
           (<a = "-d") (fl >a 1 u q dir fs)
           (<a = "-u") (fl >a d 1 q dir fs)
           (<a = "-q") (fl >a d u 1 dir fs)
           (<a = "-t") (fl >a d u q (? dir dir (getenv "TMPDIR")) fs)
           (<a = "-p") (? (two? >a) (fl >>a d u q <>a fs) (udie 1 "mktemp: -p wants a directory"))
           ((2 < tally <a) && (snip <a 0 2 = "-p")) (fl >a d u q (snip <a 2 (tally <a)) fs)
           (fl >a d u q dir (<a >< fs)))
       p (fl as 0 0 0 () ())
       d? <p u? <>p q? <>>p pdir (p 3) fs (p 4)
       tm (? (string? pdir) pdir (: e (getenv "TMPDIR") (? (string? e) e "/tmp")))
       named (two? fs)
       tmpl (? named <fs "tmp.XXXXXXXXXX")
       base (? (named && !(string? pdir)) tmpl ((? (0 < tally tm) tm ".") + "/" + tmpl))
       (try n)
        (? (n < 1) (? q? 1 (udie 1 "mktemp: could not make a name nobody had"))
           (: nm (umtname base)
              fd (? d? -1 (openfd nm 3))
              got (? d? !(mkdir nm 448) (0 <= fd))
              _ (? (0 <= fd) (fdclose fd) ())
              (? !got (try (n - 1))
                 (: _ (? u? (? d? (rmdir nm) (unlink nm)) ())
                    _ (say out nm) _ (put out 10) _ (flush out) 0))))
       (try 100))

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
) ; module 'kore
