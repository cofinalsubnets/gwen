; crew/kore/u.l -- THE U-FLOOR: the shared helpers every kore tool stands on, and the only
; part of kore that anything outside kore uses. `uread` answers the (1 text) presence wrapper,
; `udie` says its piece and leaves with a status, `urun` is the door a deep leave comes back
; through, `uleave` raises it; the rest is text and argument plumbing.
;
; ⚠ IT IS ITS OWN FILE BECAUSE OF THE COMPILER. mooncc's cat needs exactly five of these
; (ubase udie uread urun usplit) and nothing else in kore -- but core.l held all 53 names, so
; the whole toolbox sat in `moonfiles`, and editing the `ls` applet re-baked mooncc0.image and
; recompiled love.c and every host/*.c behind it. ~95 s to move a printf. The floor moved out;
; the applets are no longer the compiler's business.
;
; a body-less top-level `:` -- these names LEAK on purpose, which is what makes them a floor.
(:
   ; -- the u-floor --
   ; decimal text -> a charm (a leading - reads negative)
   (uatoi s)
    (: n (tally s)
       (go i acc) (? (i < n) (go (i + 1) ((* 10 acc) + (s i - 48))) acc)
       (? ((0 < n) && (s 0 = 45)) (0 - go 1 0) (go 0 0)))
   ; a path -> (1 text) when it opens, () when it does not (kore.l's read-text shape)
   (uread p) (: q (open p "r") (? q (: t (slurp q) _ (close q) [1 t]) ()))
   ; a path + text -> 1 when it lands, () when it will not open (uread's mirror)
   (uwrite p t) (: q (open p "w") (? q (: _ (say q t) _ (close q) 1) ()))
   ; -- the option walk, shared by every applet taking clustered flags -----------
   ; ⚠ CLUSTERING IS NOT OPTIONAL: this tree's own scripts write -qF, -qiw, -vE,
   ; -oE and -ne, so a walk that reads only whole words takes `-qF` for a pattern
   ; and a script for a file. a plain flag pins 1 on t under its own charm; `vals`
   ; names the charms taking a VALUE, glued (-m3) or as the next word, and those
   ; pin the (1 v) wrapper -- ⚠ never the bare string, since `two?` is false for
   ; one and a bare "" would read as absent. charm 101 (-e) is the repeatable one
   ; and stacks into a list instead, that being the option every tool here spells
   ; more than once. answers (t stacked rest), or () when a value is missing.
   (uopts as t vals eps)
    (? !(two? as) [t (rev eps) ()]
       (: a <as
          (? (a = "--") [t (rev eps) >as]
             (!(2 <= tally a) || !(a 0 = 45)) [t (rev eps) as]
             (: (cl k rest es)
                 (? (tally a <= k) (uopts rest t vals es)
                    (: c (a k)
                       (? !(member? c vals) (: _ (pin t c 1) (cl (k + 1) rest es))
                          (: glued (snip a (k + 1) (tally a))
                             got (? (two? glued) 1 (two? rest))
                             v (? (two? glued) glued (? (two? rest) <rest ()))
                             rest2 (? (two? glued) rest (? (two? rest) >rest rest))
                             (? !got ()
                                (: _ (? (c = 101) () (pin t c [1 v]))
                                   (cl (tally a) rest2 (? (c = 101) (v >< es) es))))))))
                (cl 1 >as eps)))))
   ; a value option's text, or () -- the presence read that pairs with uopts
   (uoptv t c) (: r (peep t c ()) (? (two? r) <>r ()))
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
       (? (two? as) (go as 1) (: _ (dot (slurp in)) _ (flush out) 0))))
