; crew/kore/core.l -- the line tools, the coreutils batch of kore: cat, echo, head,
; tail, wc, sort, uniq, tee, seq, yes, true, false, basename, dirname -- and the field
; tools: cut, tr, nl, rev. every tool is a
; <name>-main (the args after its name -> conversation on the ports -> the exit status,
; ANSWERED as a charm; a leave from deep inside rides udie, and urun is the door both
; come back through), dispatched by the multi-call `kore` (kore.l holds the registry). this
; file is a TOOLBOX, not a tool: no seat of its own, kore is its door. it rides text.l's
; lines/unlines and loads right after text.l in the cat, so the capture law folds the
; real bridge here before cook.l's later `lines` leak can shadow it. a body-less
; top-level `:` -- the mains leak; everything else stays local to its define. the u-floor it
; stands on is crew/kore/u.l, which loads immediately before it (and which the mooncc cat takes
; ALONE -- see the note there). the GNU faces we keep exact
; (the smokes in `make test_kore` hold them to the byte): wc pads every field to the
; digit width of the byte TOTAL, uniq -c wears width 7, head/tail banner many files
; with ==> name <== and a blank line between.
(:
   ; -- cat: files (or -) onto out, verbatim --
   (cat-main as) (ueach "cat" puts as)

   ; -- echo: the args back; -n holds the newline --
   (echo-main as)
    (? (two? as && (<as = "-n"))
       (: _ (say out (ujoin >as)) _ (flush out) 0)
       (: _ (say out (ujoin as)) _ (put out 10) _ (flush out) 0))

   ; -- head and tail: one driver, two clips; -n N (10 the default) --
   (uclip who clip as)
    (: nr (? (two? as && (<as = "-n"))
             (? (two? >as) `((uatoi <>as) >>as)
                (udie 2 (who + ": -n wants a count")))
             `(10 as))
       n <nr fs <>nr
       many (two? fs && two? >fs)
       (one f first ok)
        (: r (? (f = "-") `(1 (slurp in)) (uread f))
           (? (two? r)
              (: _ (? many (say out (uhdr first (? (f = "-") "standard input" f))) ())
                 _ (say out (clip n <>r))
                 ok)
              (: _ (say err (who + ": cannot open " + f)) _ (put err 10) 0)))
       (go a first ok) (? (two? a) (go >a 0 (one <a first ok))
                          (: _ (flush out) (? ok 0 1)))
       (? (two? fs) (go fs 1 1)
          (: _ (say out (clip n (slurp in))) _ (flush out) 0)))
   (head-main as) (uclip "head" uhead as)
   (tail-main as) (uclip "tail" utail as)

   ; -- wc: lines, words, bytes (flags pick; bare wc reads all three) --
   (wc-main as)
    (: (fl a l w c) (? !(two? a) `(l w c a)
                       (<a = "-l") (fl >a 1 w c)
                       (<a = "-w") (fl >a l 1 c)
                       (<a = "-c") (fl >a l w 1)
                       `(l w c a))
       p (fl as 0 0 0)
       none !(<p || <>p || <>>p)
       l? (? none 1 <p) w? (? none 1 <>p) c? (? none 1 <>>p)
       fs <>>>p
       (c3 x) <>>x
       ; read every input up front: a row is (name (l w b)), name () for stdin
       (rd a acc ok)
        (? !(two? a) `((rev acc) ok)
           (: f <a
              r (? (f = "-") `(1 (slurp in)) (uread f))
              (? (two? r) (rd >a (`(f (ucount <>r)) >< acc) ok)
                 (: _ (say err ("wc: cannot open " + f)) _ (put err 10)
                    (rd >a acc 0)))))
       ro (? (two? fs) (rd fs () 1) `(`(`(() (ucount (slurp in)))) 1))
       rows <ro ok <>ro
       (sum sel a acc) (? (two? a) (sum sel >a (acc + sel <><a)) acc)
       tl (sum cap rows 0) tw (sum caup rows 0) tb (sum c3 rows 0)
       wdt (tally (show tb))                        ; the GNU face: pad to the byte total
       (fmt cnt) (: a (? c? `((upad wdt (show (c3 cnt)))) ())
                    a (? w? (upad wdt (show <>cnt) >< a) a)
                    a (? l? (upad wdt (show <cnt) >< a) a)
                    (ujoin a))
       (emit rs) (? !(two? rs) ()
                    (: row <rs
                       _ (say out (fmt <>row))
                       _ (? !<row () (say out (" " + <row)))
                       _ (put out 10)
                       (emit >rs)))
       _ (emit rows)
       _ (? (two? rows && two? >rows)
            (: _ (say out (fmt `(tl tw tb))) _ (say out " total") (put out 10)) ())
       _ (flush out) (? ok 0 1))

   ; -- sort: the prel sort IS byte order (the LC_ALL=C face); -r reverses, -u keeps firsts --
   (sort-main as)
    (: (fl a r u) (? !(two? a) `(r u a)
                     (<a = "-r") (fl >a 1 u)
                     (<a = "-u") (fl >a r 1)
                     `(r u a))
       p (fl as 0 0)
       r? <p u? <>p fs <>>p
       (rd a acc) (? !(two? a) acc
                     (: f <a
                        t (? (f = "-") (slurp in)
                             (: x (uread f) (? (two? x) <>x
                                               (udie 2 ("sort: cannot open " + f)))))
                        (rd >a (acc + lines t))))
       (ded a) (? !(two? a) ()
                  !(two? >a) a
                  (<a = <>a) (ded >a)
                  (<a >< ded >a))
       s1 (sort (? (two? fs) (rd fs ()) (lines (slurp in))))
       s2 (? u? (ded s1) s1)
       _ (say out (unlines (? r? (rev s2) s2)))
       _ (flush out) 0)

   ; -- uniq: adjacent runs to their firsts; -c wears the run count, width 7 --
   (uniq-main as)
    (: c? (two? as && (<as = "-c"))
       fs (? c? >as as)
       t (? (two? fs)
            (: x (uread <fs) (? (two? x) <>x
                                     (udie 2 ("uniq: cannot open " + <fs))))
            (slurp in))
       (emit line cnt) (: _ (? c? (say out (upad 7 (show cnt) + " ")) ())
                          _ (say out line) (put out 10))
       (go a prev cnt)
        (? !(two? a) (? (0 < cnt) (emit prev cnt) ())
           (cnt = 0) (go >a <a 1)
           (<a = prev) (go >a prev (cnt + 1))
           (: _ (emit prev cnt) (go >a <a 1)))
       _ (go (lines t) () 0)
       _ (flush out) 0)

   ; -- tee: stdin onto out AND every file (-a appends) --
   (tee-main as)
    (: a? (two? as && (<as = "-a"))
       fs (? a? >as as)
       t (slurp in)
       (one f ok) (: q (open f (? a? "a" "w"))
                     (? q (: _ (say q t) _ (close q) ok)
                        (: _ (say err ("tee: cannot open " + f)) _ (put err 10) 0)))
       (go a ok) (? (two? a) (go >a (one <a ok)) ok)
       ok (go fs 1)
       _ (puts t) _ (flush out) (? ok 0 1))

   ; -- seq: FIRST INCR LAST, one a line (one and two args fill in the 1s) --
   (seq-main as)
    (: n (tally as)
       trip (? (n = 1) `(1 1 (uatoi <as))
               (n = 2) `((uatoi <as) 1 (uatoi <>as))
               (n = 3) `((uatoi <as) (uatoi <>as) (uatoi <>>as))
               (udie 2 "usage: seq [FIRST [INCR]] LAST"))
       a <trip d <>trip z <>>trip
       _ (? (d = 0) (udie 2 "seq: increment must not be 0") ())
       (go i) (? (? (0 < d) (i <= z) (z <= i))
                 (: _ (say out (show i)) _ (put out 10) (go (i + d)))
                 ())
       _ (go a) _ (flush out) 0)

   ; -- yes: the affirmative, forever (args override the y) --
   (yes-main as)
    (: w (? (two? as) (ujoin as) "y")
       (go _) (: _ (say out w) _ (put out 10) _ (flush out) (go ()))
       (go ()))

   ; -- true and false: the two exit codes, nothing else --
   (true-main _) 0
   (false-main _) 1

   ; -- test / [ : the POSIX condition. ⚠ IT MUST EXIST AS A PROGRAM. cook's in-image
   ; lane execs a recipe's words DIRECTLY, so `test` is no shell builtin there: a
   ; `.. > $t && test -s $t && mv $t $@` recipe looks it up on PATH, and a missing one
   ; fails the link with the tmp file already swept -- which is exactly how a 0-byte
   ; lcat header reads as a broken bootstrap. 0 true, 1 false, 2 a bad expression.
   t-fops '("-e" "-f" "-d" "-s" "-r" "-w" "-x")
   t-nops '("-eq" "-ne" "-lt" "-le" "-gt" "-ge")
   (t-int s)                                   ; "-12" -> (1 -12); anything else -> ()
    (: n (tally s)
       neg (? (0 < n) (s 0 = 45) 0)
       st (? neg 1 0)
       (go k acc) (? (n <= k) `(1 acc)
                     (: c (s k) (? ((c < 48) || (57 < c)) ()
                                   (go (k + 1) (acc * 10 + c - 48)))))
       (? (n <= st) () (: r (go st 0)
                          (? (two? r) `(1 (? neg (0 - <>r) <>r)) ()))))
   ; the stat tuple is (size mtime mode ..); 61440 masks the kind, 32768 a file, 16384 a dir
   (t-file op p)                               ; a file test, or 'no if op is not one
    (: s (stat p) m (? (two? s) <>>s 0)
       (? !(member? op t-fops) 'no
          !(two? s) 0
          (op = "-e") 1
          (op = "-f") (32768 = (& m 61440))
          (op = "-d") (16384 = (& m 61440))
          (op = "-s") (0 < <s)
          (op = "-r") (0 < (& m 292))
          (op = "-w") (0 < (& m 146))
          (0 < (& m 73))))
   (t-not v) (? (id? v 'no) 'no !v)
   (t-eval as)                                 ; a BIT, or 'no for a malformed expression
    (: n (tally as)
       (? (0 = n) 0
          ((1 < n) && (<as = "!")) (t-not (t-eval >as))
          (1 = n) (0 < tally <as)
          (2 = n) (: op <as p <>as r (t-file op p)
                     (? !(id? r 'no) r
                        (op = "-z") (0 = tally p)
                        (op = "-n") (0 < tally p)
                        'no))
          (3 = n) (: a <as op <>as b <>>as
                     (? (op = "=") (a = b)
                        (op = "!=") !(a = b)
                        !(member? op t-nops) 'no
                        (: x (t-int a) y (t-int b)
                           (? (!(two? x) || !(two? y)) 'no
                              (: p <>x q <>y
                                 (? (op = "-eq") (p = q) (op = "-ne") !(p = q)
                                    (op = "-lt") (p < q)  (op = "-le") (p <= q)
                                    (op = "-gt") (q < p)  (q <= p)))))))
          'no))
   (test-main as) (: v (t-eval as)
                     (? (id? v 'no) (udie 2 "test: bad expression") (? v 0 1)))
   (t-clip l) (? !(two? >l) () (<l >< t-clip >l))    ; every element but the last
   (bracket-main as)                           ; `[` is test that insists on its bracket
    (: n (tally as)
       (? ((0 = n) || !(as (n - 1) = "]")) (udie 2 "[: missing ]")
          (test-main (t-clip as))))

   ; -- uname: -s -n -r -m, or -a for all four. ⚠ THE MACHINE IS love-arch RESPELLED --
   ; holo says x64 where uname says x86_64, and a build naming an artifact off the wrong
   ; one names it for an arch nothing else calls. The other three come off /proc, so a
   ; seat without one answers "unknown" rather than a guess.
   (u-1line p d)                               ; a proc file's first line, or the default
    (: r (uread p)
       (? !(two? r) d
          (: t <>r n (tally t)
             (go k) (? (n <= k) k (t k < 33) k (go (k + 1)))
             e (go 0)
             (? (0 = e) d (snip t 0 e)))))
   (u-mach _) (? (love-arch = "x64") "x86_64" (love-arch = "arm64") "aarch64" love-arch)
   (u-join ws) (? !(two? ws) "" !(two? >ws) <ws (<ws + " " + u-join >ws))
   (uname-main as)
    (: fl (? (two? as) as '("-s"))
       (want f) (? (member? "-a" fl) 1 (member? f fl))
       ws ((? (want "-s") `((u-1line "/proc/sys/kernel/ostype" "Linux")) ())
           + (? (want "-n") `((u-1line "/proc/sys/kernel/hostname" "unknown")) ())
           + (? (want "-r") `((u-1line "/proc/sys/kernel/osrelease" "unknown")) ())
           + (? (want "-m") `((u-mach ())) ()))
       (? !(two? ws) (udie 2 "usage: uname [-snrma]")
          (: _ (say out (u-join ws)) _ (put out 10) _ (flush out) 0)))

   ; -- printf: the escapes, then %s %d %c %%. The format is REUSED while args remain
   ; (POSIX), and an exhausted %s is "" / %d is 0 rather than a stop. No field widths:
   ; nothing in this tree spends one, and a half-implemented %-5.2f would be a lie.
   ; ⚠ (1 charm) or (), never a bare charm: `\0` IS 0, so a charm answer cannot say
   ; "not an escape" -- presence has to ride out of band or \0 reads as an unknown.
   (pf-esc d) (? (d = 110) '(1 10) (d = 116) '(1 9) (d = 114) '(1 13) (d = 92) '(1 92)
                 (d = 97) '(1 7) (d = 98) '(1 8) (d = 102) '(1 12) (d = 118) '(1 11)
                 (d = 48) '(1 0) ())
   ; ⚠ the escape is resolved HERE, in the format walk, and not per character: `\` and
   ; `n` are two charms, so a helper handed one at a time can never see a pair -- which
   ; is exactly how `printf '%s\n'` printed a literal backslash-n.
   (pf-one f as)                               ; one pass of the format -> the args left
    (: n (tally f)
       (go k l) (? (n <= k) l
                   (: c (f k)
                      (? ((c = 92) && (k + 1 < n))
                         (: e (pf-esc (f (k + 1)))
                            (? (two? e) (: _ (put out <>e) (go (k + 2) l))
                               (: _ (put out 92) (go (k + 1) l))))
                         !((c = 37) && (k + 1 < n)) (: _ (put out c) (go (k + 1) l))
                         (: d (f (k + 1))
                            (? (d = 37) (: _ (put out 37) (go (k + 2) l))
                               (d = 115) (: _ (say out (? (two? l) <l "")) (go (k + 2) >l))
                               ((d = 100) || (d = 105))
                                (: v (? (two? l) (t-int <l) ())
                                   _ (say out (show (? (two? v) <>v 0)))
                                   (go (k + 2) >l))
                               (d = 99) (: w (? (two? l) <l "")
                                            _ (? (0 < tally w) (put out (w 0)) 0)
                                            (go (k + 2) >l))
                               (: _ (put out 37) (go (k + 1) l)))))))
       (go 0 as))
   (printf-main as)
    (? !(two? as) (udie 2 "usage: printf FORMAT [ARG..]")
       (: f <as
          (go l) (: r (pf-one f l)                 ; POSIX: repeat while the args advance
                    (? ((two? r) && !(r = l)) (go r) 0))
          _ (go >as)
          _ (flush out) 0))


   ; -- basename and dirname: the two path clips --
   (basename-main as)
    (? !(two? as) (udie 2 "usage: basename NAME [SUFFIX]")
       (: _ (say out (ubase <as (? (two? >as) <>as "")))
          _ (put out 10) _ (flush out) 0))
   (dirname-main as)
    (? !(two? as) (udie 2 "usage: dirname NAME")
       (: _ (say out (udir <as)) _ (put out 10) _ (flush out) 0))

   ; -- cut: -f LIST [-d C] [-s] over fields, -c LIST over charms (flags glue or not) --
   (cut-main as)
    (: (fl a d f c s?)
        (? !(two? a) `(d f c s? a)
           (: h <a
              (? (h = "-s") (fl >a d f c 1)
                 (h = "-d") (? (two? >a) (fl >>a (<>a 0) f c s?)
                               (udie 2 "cut: -d wants a delimiter"))
                 (h = "-f") (? (two? >a) (fl >>a d <>a c s?)
                               (udie 2 "cut: -f wants a list"))
                 (h = "-c") (? (two? >a) (fl >>a d f <>a s?)
                               (udie 2 "cut: -c wants a list"))
                 ((2 < tally h) && (snip h 0 2 = "-d")) (fl >a (h 2) f c s?)
                 ((2 < tally h) && (snip h 0 2 = "-f")) (fl >a d (snip h 2 (tally h)) c s?)
                 ((2 < tally h) && (snip h 0 2 = "-c")) (fl >a d f (snip h 2 (tally h)) s?)
                 `(d f c s? a))))
       p (fl as 9 () () 0)
       d <p fspec <>p cspec <>>p s? <>>>p
       fs <>>>>p
       _ (? (fspec || cspec) () (udie 2 "usage: cut -f LIST [-d C] [-s] | cut -c LIST"))
       spec (uspec (? fspec fspec cspec))
       (fieldline ln)
        (: ps (usplit d ln)
           (? !(two? >ps) (? s? () (: _ (say out ln) (put out 10)))
              (: (go m k acc) (? !(two? m) (rev acc)
                                 (go >m (k + 1) (? (upick spec k) (<m >< acc) acc)))
                 _ (say out (ujoinc d (go ps 1 ())))
                 (put out 10))))
       (charline ln)
        (: n (tally ln) o (jug 0)
           (go k) (? (n < k) () (: _ (? (upick spec k) (put o (ln (k - 1))) ()) (go (k + 1))))
           _ (go 1)
           _ (say out (slurp o)) (put out 10))
       (dot t) (: (go m) (? (two? m) (: _ ((? fspec fieldline charline) <m) (go >m)) ())
                  (go (lines t)))
       (ueach "cut" dot fs))

   ; -- tr: SET1 SET2 translates (SET2 pads with its last charm), -d deletes, -s
   ; squeezes same-charm runs; stdin only, like the real one --
   (tr-main as)
    (: d? (two? as && (<as = "-d"))
       s? (two? as && (<as = "-s"))
       rest (? (d? || s?) >as as)
       _ (? (two? rest) () (udie 2 "usage: tr SET1 SET2 | tr -d SET1 | tr -s SET1"))
       one (uset <rest)
       two (? (two? >rest) (uset <>rest) ())
       (mem ls) (: t (tablet 0)
                   (go m) (? (two? m) (: _ (pin t <m 1) (go >m)) t)
                   (go ls))
       (tmap a b) (: t (tablet 0)
                     (go m w last) (? !(two? m) t
                                      (: v (? (two? w) <w last)
                                         _ (pin t <m v)
                                         (go >m (? (two? w) >w ()) v)))
                     (go a b 0))
       src (slurp in)
       n (tally src)
       o (jug 0)
       _ (? d? (: dt (mem one)
                  (go k) (? (n <= k) ()
                            (: c (src k) _ (? (peep dt c 0) () (put o c)) (go (k + 1))))
                  (go 0))
            s? (: st (mem one)
                  (go k prev) (? (n <= k) ()
                                 (: c (src k)
                                    _ (? ((c = prev) && peep st c 0) () (put o c))
                                    (go (k + 1) c)))
                  (go 0 -1))
            !two (udie 2 "tr: SET2 wanted")
            (: mt (tmap one two)
               (go k) (? (n <= k) () (: c (src k) _ (put o (peep mt c c)) (go (k + 1))))
               (go 0)))
       _ (puts (slurp o)) _ (flush out) 0)

   ; -- nl: number the lines that hold ink (pad 6, a tab after); a blank line wears
   ; seven spaces and no number, and the count carries across inputs --
   (nl-main as)
    (: t #('i 1)
       (dot txt)
        (: (go m) (? !(two? m) ()
                     (: ln <m
                        _ (? !ln (: _ (say out "       ") (put out 10))
                             (: i (peep t 'i 1)
                                _ (say out (upad 6 (show i))) _ (put out 9)
                                _ (say out ln) _ (put out 10)
                                (pin t 'i (i + 1))))
                        (go >m)))
           (go (lines txt)))
       (ueach "nl" dot as))

   ; -- rev: every line back to front --
   (rev-main as)
    (: (dot txt) (: (go m) (? !(two? m) ()
                              (: _ (say out (urev <m)) _ (put out 10) (go >m)))
                    (go (lines txt)))
       (ueach "rev" dot as)))
