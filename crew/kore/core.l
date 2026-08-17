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
(module 'kore
(:
   ; -- cat: files (or -) onto out, verbatim --
   (cat-main as) (ueach "cat" puts as)

   ; -- echo: the args back; -n holds the newline --
   (echo-main as)
    (? (two? as && (<as = "-n"))
       (: _ (say out (ujoin >as)) _ (flush out) 0)
       (: _ (say out (ujoin as)) _ (put out 10) _ (flush out) 0))

   ; -- head and tail: one driver, two clips; -n N (10 the default) --
   ; ⚠ `-5` is the OBSOLESCENT spelling and every script still writes it, so it is not
   ; optional: a bare -DIGITS is the count. `-` alone stays the stdin name, and a flag
   ; that is neither reaches uread as a filename, exactly as it did before.
   (digits? w) (: n (tally w)
                  (go k) (? (n <= k) (1 < n)
                            (47 < (w k) < 58) (go (k + 1))
                            0)
                  (go 1))
   (uclip who clip as)
    (: nr (? (two? as && (<as = "-n"))
             (? (two? >as) `((uatoi <>as) >>as)
                (udie 2 (who + ": -n wants a count")))
             (two? as && ((<as 0) = 45) && (digits? <as))
             `((uatoi (snip <as 1 (tally <as))) >as)
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
    (: (dot txt) (: e (nl-end? txt)                     ; the last line keeps the source's end
                    (go m) (? !(two? m) ()
                              (: _ (say out (urev <m))
                                 _ (? (e || two? >m) (put out 10) 0)
                                 (go >m)))
                    (go (lines txt)))
       (ueach "rev" dot as))

   ; ---- the calendar floor (stat's %y and date both read it) --------------------
   ; Howard Hinnant's exact integer civil-from-days and its inverse, shifted to a March
   ; year so the leap day lands at the END and no month table is needed. ⚠ love's // is
   ; a FLOOR division, which is what these want -- the sign dance the C originals carry
   ; is only there because C truncates. lawed in law.l over the round trip.
   ; ⚠ THERE IS NO TIME ZONE HERE and there will not be one without a tz database: every
   ; face over these is UTC, exactly as nolibc's localtime IS its gmtime.
   (ucivil z0)                         ; days since 1970-01-01 -> (year month day)
    (: z (z0 + 719468)
       era (// z 146097)
       doe (z - (* era 146097))
       yoe (// (((doe - (// doe 1460)) + (// doe 36524)) - (// doe 146096)) 365)
       y ((* era 400) + yoe)
       doy (doe - (((* 365 yoe) + (// yoe 4)) - (// yoe 100)))
       mp (// ((* 5 doy) + 2) 153)
       d ((doy - (// ((* 153 mp) + 2) 5)) + 1)
       m (mp + (? (mp < 10) 3 -9))
       `((? (m <= 2) (y + 1) y) m d))
   (udays y0 m d)                      ; (year month day) -> days since 1970-01-01
    (: y (? (m <= 2) (y0 - 1) y0)
       era (// y 400)
       yoe (y - (* era 400))
       doy ((// ((* 153 (m + (? (2 < m) -3 9))) + 2) 5) + (d - 1))
       doe ((((* yoe 365) + (// yoe 4)) - (// yoe 100)) + doy)
       (((* era 146097) + doe) - 719468))

   ; ---- the record tools: paste, comm, join, split, od -------------------------

   ; -d's LIST -> the delimiter TEXTS. texts and not charms because POSIX's \0 is the
   ; EMPTY delimiter, and no charm can say "nothing here"
   (udels s)
    (: n (tally s)
       (esc c) (? (c = 110) "\n" (c = 116) "\t" (c = 114) "\r" (c = 48) ""
                  (c = 92) "\\" (string c))
       (go k acc) (? (n <= k) (rev acc)
                     ((s k = 92) && ((k + 1) < n)) (go (k + 2) (esc (s (k + 1)) >< acc))
                     (go (k + 1) (string (s k) >< acc)))
       r (go 0 ())
       (? (two? r) r '("")))
   ; an input's lines, or a leave naming the file (the record tools read whole files:
   ; they walk several at once, so ueach's one-at-a-time cat walk is not their shape)
   (urows who f)
    (: r (? (f = "-") `(1 (slurp in)) (uread f))
       (? (two? r) (lines <>r) (udie 1 (who + ": cannot open " + f))))

   ; -- paste: the files' lines side by side, a row at a time; the delimiters cycle
   ; between the columns and start over each row (a tab when unsaid). -s lays a whole
   ; file ALONG one row instead, which is the transpose and not a variant --
   (paste-main as)
    (: (fl a d s?)
        (? !(two? a) `(d s? a)
           (<a = "-s") (fl >a d 1)
           (<a = "-d") (? (two? >a) (fl >>a (udels <>a) s?)
                          (udie 2 "paste: -d wants a list"))
           ((2 < tally <a) && (snip <a 0 2 = "-d"))
            (fl >a (udels (snip <a 2 (tally <a))) s?)
           `(d s? a))
       p (fl as '("\t") 0)
       ds <p s? <>p
       fs (? (two? <>>p) <>>p '("-"))
       (jn cells)
        (: o (jug 0)
           ; ⚠ the list ADVANCES with the separator it spends, never with the cell:
           ; the first cell spends none, so an early advance shifts the whole cycle
           (go m left first)
            (? !(two? m) (slurp o)
               first (: _ (say o <m) (go >m left 0))
               (: _ (say o <left) _ (say o <m)
                  (go >m (? (two? >left) >left ds) 0)))
           (go cells ds 1))
       (emit t) (: _ (say out t) (put out 10))
       (some lss) (foldl (\ a l (? (two? l) 1 a)) 0 lss)
       (rows lss) (? (some lss)
                     (: _ (emit (jn (map (\ l (? (two? l) <l "")) lss)))
                        (rows (map (\ l (? (two? l) >l ())) lss)))
                     ())
       lss (map (\ f (urows "paste" f)) fs)
       _ (? s? (foldl (\ _ l (emit (jn l))) 0 lss) (rows lss))
       _ (flush out) 0)

   ; -- comm: two SORTED files merged into three columns -- only in the first, only in
   ; the second, in both -- each column behind a tab per column standing before it.
   ; -1/-2/-3 drop a column and the tabs close up behind it, GNU's own arithmetic --
   (comm-main as)
    (: (fl a s1 s2 s3)
        (? !(two? a) `(s1 s2 s3 a)
           ((tally <a < 2) || !(<a 0 = 45)) `(s1 s2 s3 a)
           (: w <a
              (cl k b1 b2 b3)
               (? (tally w <= k) (fl >a b1 b2 b3)
                  (: c (w k)
                     (? (c = 49) (cl (k + 1) 1 b2 b3)
                        (c = 50) (cl (k + 1) b1 1 b3)
                        (c = 51) (cl (k + 1) b1 b2 1)
                        (udie 2 ("comm: unknown option -" + string c)))))
              (cl 1 s1 s2 s3)))
       p (fl as 0 0 0)
       s1 <p s2 <>p s3 <>>p fs (at p 3)
       _ (? (two? >fs) () (udie 2 "usage: comm [-123] FILE1 FILE2"))
       (pre k) ((? (!s1 && (1 < k)) "\t" "") + (? (!s2 && (2 < k)) "\t" ""))
       (emit k ln) (? (? (k = 1) s1 (k = 2) s2 s3) ()
                      (: _ (say out (pre k)) _ (say out ln) (put out 10)))
       (go a b) (? (!(two? a) && !(two? b)) ()
                   !(two? a) (: _ (emit 2 <b) (go a >b))
                   !(two? b) (: _ (emit 1 <a) (go >a b))
                   (<a = <b) (: _ (emit 3 <a) (go >a >b))
                   (<a < <b) (: _ (emit 1 <a) (go >a b))
                   (: _ (emit 2 <b) (go a >b)))
       _ (go (urows "comm" <fs) (urows "comm" <>fs))
       _ (flush out) 0)

   ; -- split: a file cut into pieces named PREFIX + a rolling suffix. -l N lines (1000
   ; unsaid), -b N bytes (a K/M/G suffix multiplies), -a N the suffix width, -d digits
   ; for letters. an empty input writes NO file, like GNU --
   (usfx n w d?)                       ; the n'th suffix, w wide: aa ab .. or 00 01 ..
    (: b (? d? 10 26)
       (go k v acc) (? (k < 1) acc
                       (go (k - 1) (v // b) (string ((? d? 48 97) + (v % b)) + acc)))
       (go w n ""))
   (usize s)                           ; a byte count, an optional K/M/G multiplying
    (: n (tally s)
       c (? (0 < n) (s (n - 1)) 0)
       m (? ((c = 107) || (c = 75)) 1024 ((c = 109) || (c = 77)) 1048576
            ((c = 103) || (c = 71)) 1073741824 0)
       (? m (* m (uatoi (snip s 0 (n - 1)))) (uatoi s)))
   (split-main as)
    (: (fl a ln by sw d? fs)
        (: (val w) (snip w 2 (tally w))
           (long? w) ((2 < tally w) && (snip w 0 2 = "-l"))
           (? !(two? a) `(ln by sw d? (rev fs))
              (<a = "-l") (? (two? >a) (fl >>a (uatoi <>a) () sw d? fs)
                             (udie 2 "split: -l wants a count"))
              (<a = "-b") (? (two? >a) (fl >>a () (usize <>a) sw d? fs)
                             (udie 2 "split: -b wants a size"))
              (<a = "-a") (? (two? >a) (fl >>a ln by (uatoi <>a) d? fs)
                             (udie 2 "split: -a wants a width"))
              (<a = "-d") (fl >a ln by sw 1 fs)
              (long? <a) (fl >a (uatoi (val <a)) () sw d? fs)
              ((2 < tally <a) && (snip <a 0 2 = "-b")) (fl >a () (usize (val <a)) sw d? fs)
              ((2 < tally <a) && (snip <a 0 2 = "-a")) (fl >a ln by (uatoi (val <a)) d? fs)
              (fl >a ln by sw d? (<a >< fs))))
       p (fl as () () 2 0 ())
       ln <p by <>p sw <>>p d? (at p 3) fs (at p 4)
       nl (? (charm? ln) ln 1000)
       src (? (two? fs) <fs "-")
       pre (? ((two? fs) && two? >fs) <>fs "x")
       t (? (src = "-") (slurp in)
            (: r (uread src) (? (two? r) <>r (udie 1 ("split: cannot open " + src)))))
       end? (nl-end? t)
       (wr i txt) (: nm (pre + usfx i sw d?)
                     (? (uwrite nm txt) () (udie 1 ("split: cannot write " + nm))))
       (bygo i k) (? (tally t <= k) 0
                     (: e (? ((k + by) < tally t) (k + by) (tally t))
                        _ (wr i (snip t k e))
                        (bygo (i + 1) e)))
       ; ⚠ unlines terminates every line, so the LAST piece of a source that did not
       ; end in a newline has to give the newline back -- the piece is where the
       ; source's own end lands, and GNU keeps it there
       (lngo i ls) (? !(two? ls) 0
                      (: rest (drop nl ls)
                         txt (unlines (take nl ls))
                         _ (wr i (? ((two? rest) || end?) txt (chomp txt)))
                         (lngo (i + 1) rest)))
       _ (? (charm? by) (bygo 0 0) (lngo 0 (lines t)))
       0)

   ; the shape every option walk past this point rides: a word is -X and the value is
   ; either GLUED to it or the next word. ⚠ asked by TALLY: a text is no link, so a
   ; `two?` here reads every glued value as absent (the bug that ate `grep -m2`).
   (uopt w)                            ; "-tx1" -> ("t" "x1"), a bare word -> ()
    (: n (tally w)
       (? ((1 < n) && (w 0 = 45)) `((snip w 1 2) (snip w 2 n)) ()))

   ; -- join: two files SORTED on their join field, merged into one. a row is the key,
   ; then the first file's other fields, then the second's; a key repeated on either
   ; side makes the whole cross product, which is what a relational join owes. -1/-2
   ; pick the field, -t the separator (runs of blanks when unsaid, and then the output
   ; rides one space), -a keeps a side's unpairable rows in their sorted place, -v keeps
   ; ONLY those (and drops the paired rows entirely). out of dialect, deliberately: -o
   ; (an output template is its own little language), -e, -i.
   (ujfs sep ln)                       ; a row's fields: -t's charm, or runs of blanks
    (? (charm? sep) (usplit sep ln)
       (: n (tally ln)
          (bl? c) ((c = 32) || (c = 9))
          (go i st acc)
           (? (n <= i) (rev (? (st < 0) acc (snip ln st i >< acc)))
              (bl? (ln i)) (go (i + 1) -1 (? (st < 0) acc (snip ln st i >< acc)))
              (go (i + 1) (? (st < 0) i st) acc))
          (go 0 -1 ())))
   (unth l k) (? ((k < 1) || !(two? l)) () (k = 1) <l (unth >l (k - 1)))
   (join-main as)
    (: t #('f1 1 'f2 1 'sep () 'a1 0 'a2 0 'v1 0 'v2 0)
       (fl a fs)
        (? !(two? a) (rev fs)
           (: w <a
              o (uopt w)
              takes (? o (member? <o '("1" "2" "t" "a" "v")) 0)
              glue (? o <>o "")
              got (? (0 < tally glue) glue (? (two? >a) <>a ()))
              rest (? (0 < tally glue) >a (? (two? >a) >>a >a))
              (? !takes (fl >a (w >< fs))
                 !(string? got) (udie 2 ("join: " + w + " wants a value"))
                 (: k <o
                    _ (? (k = "1") (pin t 'f1 (uatoi got))
                         (k = "2") (pin t 'f2 (uatoi got))
                         (k = "t") (pin t 'sep (got 0))
                         (k = "a") (pin t (? (got = "2") 'a2 'a1) 1)
                         (pin t (? (got = "2") 'v2 'v1) 1))
                    (fl rest fs)))))
       fs (fl as ())
       _ (? (two? >fs) ()
            (udie 2 "usage: join [-1 F] [-2 F] [-t C] [-a 1|2] [-v 1|2] FILE1 FILE2"))
       sep (peep t 'sep ())
       jsep (? (charm? sep) sep 32)
       n1 (peep t 'f1 1) n2 (peep t 'f2 1)
       pair? !((peep t 'v1 0) || (peep t 'v2 0))
       un1 ((peep t 'a1 0) || (peep t 'v1 0))
       un2 ((peep t 'a2 0) || (peep t 'v2 0))
       ; a side is its rows GROUPED by key: ((key (fields..)..)..), the run of one key
       (grp sel lns)
        (: rows (map (\ ln (: f (ujfs sep ln) k (unth f sel)
                              [(? (string? k) k "") f]))
                     lns)
           (go rs) (? !(two? rs) ()
                      (: k <<rs
                         (span m acc) (? ((two? m) && (<<m = k)) (span >m (<><m >< acc))
                                         [(rev acc) m])
                         r (span rs ())
                         ([k <r] >< go <>r)))
           (go rows))
       (drop1 sel f) (: (go l i acc) (? !(two? l) (rev acc)
                                        (go >l (i + 1) (? (i = sel) acc (<l >< acc))))
                        (go f 1 ()))
       (row ps) (: _ (say out (ujoinc jsep ps)) (put out 10))
       (one k sel g) (foldl (\ _ f (row (k >< drop1 sel f))) 0 <>g)
       (cross g1 g2)
        (foldl (\ _ f1 (foldl (\ _ f2 (row ((<g1 >< drop1 n1 f1) + drop1 n2 f2)))
                              0 <>g2))
               0 <>g1)
       (go g1 g2)
        (? (!(two? g1) && !(two? g2)) ()
           !(two? g1) (: _ (? un2 (one <<g2 n2 <g2) ()) (go g1 >g2))
           !(two? g2) (: _ (? un1 (one <<g1 n1 <g1) ()) (go >g1 g2))
           (<<g1 = <<g2) (: _ (? pair? (cross <g1 <g2) ()) (go >g1 >g2))
           (<<g1 < <<g2) (: _ (? un1 (one <<g1 n1 <g1) ()) (go >g1 g2))
           (: _ (? un2 (one <<g2 n2 <g2) ()) (go g1 >g2)))
       _ (go (grp n1 (urows "join" <fs)) (grp n2 (urows "join" <>fs)))
       _ (flush out) 0)

   ; -- od: the byte dump, GNU's columns to the space. -A d|o|x|n picks the address
   ; radix (octal, 7 wide, unsaid; -An drops the column AND the closing offset), -t
   ; TYPE the reading, -N a limit, -j a skip, -v every line even where it repeats
   ; (a repeat is one `*` otherwise). the short spellings -a -b -c -d -o -x -s -i are
   ; the -t's GNU gives them. ⚠ ONE -t per run, the last given winning: GNU's
   ; several-at-once lane re-widens every column to the widest type in the set, which
   ; is a whole layout of its own and not another row.
   (uzpad w s) (? (tally s < w) (uzpad w ("0" + s)) s)
   (uradix n b) (: o (jug 0) _ (putbn o n b) (slurp o))
   uodnames '("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel" "bs" "ht" "nl" "vt"
              "ff" "cr" "so" "si" "dle" "dc1" "dc2" "dc3" "dc4" "nak" "syn" "etb"
              "can" "em" "sub" "esc" "fs" "gs" "rs" "us" "sp")
   (uodname c)                         ; -ta's name, the high bit masked off like GNU's
    (: b (& c 127) (? (b < 33) (at uodnames b) (b = 127) "del" (string b)))
   (uodesc c)                          ; -tc's escape; anything else is three octal
    (? (c = 0) "\\0" (c = 7) "\\a" (c = 8) "\\b" (c = 9) "\\t" (c = 10) "\\n"
       (c = 11) "\\v" (c = 12) "\\f" (c = 13) "\\r"
       ((31 < c) && (c < 127)) (string c)
       (uzpad 3 (uradix c 8)))
   (uodtype s)                         ; a -t word -> (kind size width) | ()
    (: k (? (0 < tally s) (s 0) 0)
       z (? (1 < tally s) (uatoi (snip s 1 (tally s))) 0)
       (? (k = 99) '(c 1 3)
          (k = 97) '(a 1 3)
          (k = 120) (? (z = 1) '(x 1 2) (z = 4) '(x 4 8) (z = 2) '(x 2 4) (z = 0) '(x 2 4) ())
          (k = 111) (? (z = 1) '(o 1 3) (z = 4) '(o 4 11) (z = 2) '(o 2 6) (z = 0) '(o 2 6) ())
          (k = 100) (? (z = 1) '(d 1 4) (z = 4) '(d 4 11) (z = 2) '(d 2 6) (z = 0) '(d 2 6) ())
          (k = 117) (? (z = 1) '(u 1 3) (z = 4) '(u 4 10) (z = 2) '(u 2 5) (z = 0) '(u 2 5) ())
          ()))
   (od-main as)
    (: t #('ad 111 'ty '(o 2 6) 'lim () 'skip 0 'v 0)
       (fl a fs)
        (? !(two? a) (rev fs)
           (: w <a
              o (uopt w)
              k (? o <o ())
              glue (? o <>o "")
              takes (? o (member? k '("A" "t" "N" "j")) 0)
              got (? (0 < tally glue) glue (? (two? >a) <>a ()))
              rest (? (0 < tally glue) >a (? (two? >a) >>a >a))
              short (? o (? (k = "c") (uodtype "c") (k = "a") (uodtype "a")
                            (k = "b") (uodtype "o1") (k = "x") (uodtype "x2")
                            (k = "o") (uodtype "o2") (k = "d") (uodtype "u2")
                            (k = "s") (uodtype "d2") (k = "i") (uodtype "d4") ())
                       ())
              (? takes
                 (? !(string? got) (udie 2 ("od: " + w + " wants a value"))
                    (: _ (? (k = "A") (pin t 'ad (got 0))
                            (k = "N") (pin t 'lim (usize got))
                            (k = "j") (pin t 'skip (usize got))
                            (: y (uodtype got)
                               (? y (pin t 'ty y) (udie 2 ("od: unknown type " + got)))))
                       (fl rest fs)))
                 !o (fl >a (w >< fs))
                 short (: _ (pin t 'ty short) (fl >a fs))
                 (k = "v") (: _ (pin t 'v 1) (fl >a fs))
                 (udie 2 ("od: unknown option " + w)))))
       fs (fl as ())
       ad (peep t 'ad 111) ty (peep t 'ty '(o 2 6))
       lim (peep t 'lim ()) skip (peep t 'skip 0) v? (peep t 'v 0)
       kd <ty sz <>ty wd <>>ty
       byte? ((id? kd 'c) || (id? kd 'a))
       step (? byte? 1 sz)
       (rd f) (? (f = "-") (slurp in)
                 (: r (uread f) (? (two? r) <>r (udie 1 ("od: cannot open " + f)))))
       ; the named files are ONE stream, which is what makes the offsets run through
       whole (foldl (\ acc f (acc + rd f)) "" (? (two? fs) fs '("-")))
       _ (? (tally whole < skip) (udie 1 "od: cannot skip past end of combined input") ())
       hi (: e (? (charm? lim) (skip + lim) (tally whole))
             (? (e < tally whole) e (tally whole)))
       src (? (skip < hi) (snip whole skip hi) "")
       nb (tally src)
       (adr k) (? (ad = 110) ""
                  (ad = 100) (uzpad 7 (show k))
                  (ad = 120) (uzpad 6 (uradix k 16))
                  (uzpad 7 (uradix k 8)))
       (val k)                          ; the little-endian item at k, short reads zeroed
        (: (go i acc) (? (sz <= i) acc
                         (go (i + 1) (acc + (* (? ((k + i) < nb) (src (k + i)) 0)
                                               ((* 8 i) 2)))))
           (go 0 0))
       (cell k)
        (? (id? kd 'c) (upad 3 (uodesc (src k)))
           (id? kd 'a) (upad 3 (uodname (src k)))
           (: raw (val k)
              v (? ((id? kd 'd) && !(raw < (((* 8 sz) - 1) 2))) (raw - ((* 8 sz) 2)) raw)
              (? (id? kd 'x) (uzpad wd (uradix v 16))
                 (id? kd 'o) (uzpad wd (uradix v 8))
                 (upad wd (show v)))))
       (rowof k cnt)
        (: items (? byte? cnt (// ((cnt + sz) - 1) sz))
           o (jug 0)
           (go i) (? (items <= i) (slurp o)
                     (: _ (put o 32) _ (say o (cell (k + (* i step)))) (go (i + 1))))
           (go 0))
       (walk k prev star)
        (? (nb <= k) ()
           (: cnt (? ((k + 16) < nb) 16 (nb - k))
              blk (snip src k (k + cnt))
              (? ((!v? && (cnt = 16)) && (blk = prev))
                 (: _ (? star () (: _ (say out "*") (put out 10))) (walk (k + 16) blk 1))
                 (: _ (say out (adr (skip + k))) _ (say out (rowof k cnt)) _ (put out 10)
                    (walk (k + 16) blk 0)))))
       _ (walk 0 () 0)
       _ (? (ad = 110) () (: _ (say out (adr (skip + nb))) (put out 10)))
       _ (flush out) 0))
) ; module 'kore
