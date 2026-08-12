; crew/kore/core.l -- the line tools, the coreutils batch of kore: cat, echo, head,
; tail, wc, sort, uniq, tee, seq, yes, true, false, basename, dirname -- and the field
; tools: cut, tr, nl, rev. every tool is a
; <name>-main (the args after its name -> conversation on the ports -> the exit status,
; ANSWERED as a charm; a leave from deep inside rides udie, and urun is the door both
; come back through), dispatched by the multi-call `kore` (kore.l holds the registry). this
; file is a TOOLBOX, not a tool: no seat of its own, kore is its door. it rides text.l's
; lines/unlines and loads right after text.l in the cat, so the capture law folds the
; real bridge here before cook.l's later `lines` leak can shadow it. a body-less
; top-level `:` -- the u-floor (shared helpers, the unified layer's prefix) and the
; mains leak; everything else stays local to its define. the GNU faces we keep exact
; (the smokes in `make test_kore` hold them to the byte): wc pads every field to the
; digit width of the byte TOTAL, uniq -c wears width 7, head/tail banner many files
; with ==> name <== and a blank line between.
(:
   ; -- the u-floor --
   ; decimal text -> a charm (a leading - reads negative)
   (uatoi s)
    (: n (tally s)
       (go i acc) (? (i < n) (go (i + 1) ((* 10 acc) + (s i - 48))) acc)
       (? ((0 < n) && (s 0 = 45)) (0 - go 1 0) (go 0 0)))
   ; a path -> (1 text) when it opens, () when it does not (kore.l's read-text shape)
   (uread p) (: q (open p "r") (? q (: t (slurp q) _ (close q) `(1 t)) ()))
   ; the exit door, two faces: a main ANSWERS its status as a charm, and a leave from
   ; deep inside scares 'leave carrying it. urun is the driver both come back through,
   ; so a caller in the SAME IMAGE gets a charm instead of a dead process -- the seat
   ; quits with what it answers. ⚠ nothing unwinds through a scare (spec.l's control
   ; section), so a port a tool holds at the leave is lost, exactly as `quit` lost it.
   (uleave c) (scare 'leave c)
   (urun f as)
    (: (out! _) (: _ (flush out) (flush err))
       (trap (\ _ (: r (f as) _ (out! ()) (? (charm? r) r 0))) ()
             (\ a b (: _ (out! ())
                       (? (id? a 'leave) b
                          (: _ (say err ";; ") _ (print err a) _ (say err " ")
                             _ (print err b) _ (put err 10) _ (flush err) 1))))))
   ; complain on err and leave through the exit door with code c
   (udie c m) (: _ (say err m) _ (put err 10) (uleave c))
   ; left-pad with spaces to width w (a wider count never shrinks)
   (upad w s) (? (tally s < w) (upad w (" " + s)) s)
   ; join texts with single spaces
   (ujoin a) (? !(two? a) ""
                !(two? >a) <a
                (<a + " " + ujoin >a))
   ; the many-file banner head and tail share: none before the first, a blank between
   (uhdr first f) (? first ("==> " + f + " <==\n") ("\n==> " + f + " <==\n"))
   ; the two pure line clips (lawed in law.l)
   (uhead n t) (: ls (lines t) (unlines (? (n < tally ls) (take n ls) ls)))
   (utail n t) (: ls (lines t) k (tally ls) (unlines (? (n < k) (drop (k - n) ls) ls)))
   ; the wc counter: text -> (lines words bytes), a word a maximal ink run
   (ucount t)
    (: n (tally t)
       (ws? c) ((c = 32) || ((8 < c) && (c < 14)))
       (go k inw l w) (? (n <= k) `(l w n)
                         (: c (t k)
                            (? (ws? c) (go (k + 1) 0 (? (c = 10) (l + 1) l) w)
                               (go (k + 1) 1 l (? inw w (w + 1))))))
       (go 0 0 0 0))
   ; the two pure path clips (basename NAME [SUFFIX] / dirname NAME; lawed in law.l)
   (ubase p sfx)
    (: (e k) (? ((0 < k) && (p (k - 1) = 47)) (e (k - 1)) k)
       e0 (e (tally p))
       (? (e0 = 0) (? (0 < tally p) "/" "")
          (: (b k) (? ((0 < k) && !(p (k - 1) = 47)) (b (k - 1)) k)
             s (snip p (b e0) e0)
             m (tally sfx) n (tally s)
             (? ((0 < m) && (m < n) && (snip s (n - m) n = sfx))
                (snip s 0 (n - m)) s))))
   (udir p)
    (: (e k) (? ((0 < k) && (p (k - 1) = 47)) (e (k - 1)) k)
       e0 (e (tally p))
       (? (e0 = 0) (? (0 < tally p) "/" ".")
          (: (b k) (? ((0 < k) && !(p (k - 1) = 47)) (b (k - 1)) k)
             b0 (b e0)
             (? (b0 = 0) "."
                (: z (e b0) (? (z = 0) "/" (snip p 0 z)))))))
   ; split at charm c, every piece kept -- "a:" is ("a" ""), "" is ("") (cut's face,
   ; where lines would drop the empty tail)
   (usplit c s)
    (: n (tally s)
       (go from k acc) (? (n <= k) (rev (snip s from k >< acc))
                          (s k = c) (go (k + 1) (k + 1) (snip s from k >< acc))
                          (go from (k + 1) acc))
       (go 0 0 ()))
   ; join with charm c between
   (ujoinc c ls)
    (: o (jug 0)
       (go m first) (? !(two? m) (slurp o)
                       (: _ (? first () (put o c)) _ (say o <m) (go >m 0)))
       (go ls 1))
   ; cut's LIST -> ((lo hi)..): N, N-M, N- (open above), -M (from 1)
   (uspec s)
    (: (one p)
        (: n (tally p)
           (dash k) (? (k < n) (? (p k = 45) k (dash (k + 1))) -1)
           i (dash 0)
           (? (i < 0) (: v (uatoi p) `(v v))
              (: lo (? (i = 0) 1 (uatoi (snip p 0 i)))
                 hi (? ((i + 1) = n) 536870911 (uatoi (snip p (i + 1) n)))
                 `(lo hi))))
       (map one (usplit 44 s)))
   ; does 1-based seat k sit in the spec
   (upick spec k)
    (? !(two? spec) 0
       (: r <spec
          (? (!(k < <r) && !(<>r < k)) 1 (upick >spec k))))
   ; tr's SET -> the charm list: a-z ranges expand, \n \t \r \0 \\ escape, a dash at
   ; either end stays literal
   (uset s)
    (: n (tally s)
       (esc c) (? (c = 110) 10 (c = 116) 9 (c = 114) 13 (c = 48) 0 c)
       (span a b acc) (? (b < a) acc (span a (b - 1) (b >< acc)))
       ; the everyday POSIX classes read as their ranges ([:lower:] [:upper:] [:digit:])
       (cls? k nm) (: m (tally nm)
                      (((k + m) <= n) && (nm = snip s k (k + m))))
       (go k acc)
        (? (n <= k) (rev acc)
           (: c (s k)
              (? (cls? k "[:lower:]") (go (k + 9) (rev (span 97 122 ()) + acc))
                 (cls? k "[:upper:]") (go (k + 9) (rev (span 65 90 ()) + acc))
                 (cls? k "[:digit:]") (go (k + 9) (rev (span 48 57 ()) + acc))
                 ((c = 92) && ((k + 1) < n)) (go (k + 2) (esc (s (k + 1)) >< acc))
                 (((k + 2) < n) && (s (k + 1) = 45) && !(s (k + 2) < c))
                  (go (k + 3) (rev (span c (s (k + 2)) ()) + acc))
                 (go (k + 1) (c >< acc)))))
       (go 0 ()))
   ; a text's charms, back to front
   (urev s)
    (: o (jug 0)
       (go k) (? (k < 0) (slurp o) (: _ (put o (s k)) (go (k - 1))))
       (go (tally s - 1)))
   ; the cat walk every whole-input tool rides: dot over each input text (args or
   ; stdin, - reads stdin), a miss complains and the exit code remembers
   (ueach who dot as)
    (: (one f ok) (? (f = "-") (: _ (dot (slurp in)) ok)
                     (: r (uread f)
                        (? (two? r) (: _ (dot <>r) ok)
                           (: _ (say err (who + ": cannot open " + f)) _ (put err 10) 0))))
       (go a ok) (? (two? a) (go >a (one <a ok)) (: _ (flush out) (? ok 0 1)))
       (? (two? as) (go as 1) (: _ (dot (slurp in)) _ (flush out) 0)))

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
