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
(module 'kore
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
                          ; ⚠ a GLUED value is asked by TALLY and never by `two?`: a text
                          ; is no link, so `two? "2"` is 0 and a -m2 read as a bare -m
                          ; with nothing behind it -- silently, since the miss then took
                          ; the next word (the FILE) for the count
                          (: glued (snip a (k + 1) (tally a))
                             got (? (0 < tally glued) 1 (two? rest))
                             v (? (0 < tally glued) glued (? (two? rest) <rest ()))
                             rest2 (? (0 < tally glued) rest (? (two? rest) >rest rest))
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
   ; left-pad with spaces to width w (a wider count never shrinks), and its mirror --
   ; the columns that count from the left (ps's TTY, free's label) want the other one
   (upad w s) (? (tally s < w) (upad w (" " + s)) s)
   (urpad w s) (? (tally s < w) (urpad w (s + " ")) s)
   ; join texts with single spaces
   (ujoin a) (? !(two? a) ""
                !(two? >a) <a
                (<a + " " + ujoin >a))
   ; the many-file banner head and tail share: none before the first, a blank between
   (uhdr first f) (? first ("==> " + f + " <==\n") ("\n==> " + f + " <==\n"))
   ; the two pure line clips (lawed in law.l). ⚠ each chomps only when the SOURCE's last
   ; line actually rode through: head that cut before it ends on a line the text itself
   ; terminated, so it keeps its newline whatever the far end of the input looked like.
   (uhead n t) (: ls (lines t) k (tally ls) cut (n < k)
                  o (unlines (? cut (take n ls) ls))
                  (? (cut || (nl-end? t)) o (chomp o)))
   (utail n t) (: ls (lines t) k (tally ls)
                  o (unlines (? (n < k) (drop (k - n) ls) ls))
                  (? (nl-end? t) o (chomp o)))
   ; the wc counter as a FOLD, so a gulp can be counted and dropped: st is
   ; [inw lines words bytes]. ⚠ `inw` HAS TO RIDE BETWEEN GULPS -- a word is a
   ; maximal ink run and one straddling the boundary counts twice without it.
   (ucount-go st s)
    (: n (tally s)
       (ws? c) ((c = 32) || ((8 < c) && (c < 14)))
       (go k inw l w) (? (n <= k) [inw l w]
                         (: c (s k)
                            (? (ws? c) (go (k + 1) 0 (? (c = 10) (l + 1) l) w)
                               (go (k + 1) 1 l (? inw w (w + 1))))))
       r (go 0 <st <>st <>>st)
       [<r <>r <>>r (<>>>st + n)])
   ; the word state is a per-byte machine; a wc that was not asked for words has no
   ; reason to run it, and one that wants only bytes has none to look at them at all
   (ucount-b st s) [<st <>st <>>st (<>>>st + tally s)]
   (ucount-l st s)
    (: n (tally s)
       (go k l) (? (n <= k) l (go (k + 1) (? ((s k) = 10) (l + 1) l)))
       [<st (go 0 <>st) <>>st (<>>>st + n)])
   (ucount-for l? w?) (? w? ucount-go (? l? ucount-l ucount-b))
   ucount0 [0 0 0 0]
   (ucount-say st) [<>st <>>st <>>>st]      ; the state's (lines words bytes) face
   ; text -> (lines words bytes), a word a maximal ink run (lawed in law.l)
   (ucount t) (ucount-say (ucount-go ucount0 t))
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
           (? (i < 0) (: v (uatoi p) [v v])
              (: lo (? (i = 0) 1 (uatoi (snip p 0 i)))
                 hi (? ((i + 1) = n) 536870911 (uatoi (snip p (i + 1) n)))
                 [lo hi])))
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
   ; --- reading an input WITHOUT holding it ----------------------------------
   ; uread + `lines` keeps four whole copies of the input alive at once -- `rev` of
   ; 50 MB wanted 1.09 GB of live set. These hand it over a gulp or a line at a time.
   ; ⚠ see/unsee/chug AND NOT `swig`: kore runs in two worlds (see ucopy-bytes) and
   ; the kernel image compiles no host/*.c. The gulp door is core's, so no probe.
   ; 'p is the unread cursor into the held bytes, 'm how far the newline scan has
   ; looked. the read-through prefix is dropped when a fill wants the room and not
   ; per line, so a line costs its own length instead of the tail behind it
   (urd q) (: t (tablet 0)
              _ (pin t 'q q) _ (pin t 'h "") _ (pin t 'p 0) _ (pin t 'm 0)
              _ (pin t 'e 0)
              t)
   (urd-drop t)                                  ; let the read-through bytes go
    (: p (peep t 'p 0)
       (? (p = 0) 0
          (: h (peep t 'h "")
             _ (pin t 'h (snip h p (tally h)))
             _ (pin t 'm ((peep t 'm 0) - p))
             (pin t 'p 0))))
   ; one gulp onto the held bytes; 1 when something arrived, 0 at the end. ⚠ the
   ; see/unsee pair is what terminates it -- `see` touches the device (chug never
   ; does) and the byte it answers is one chug must count, so a gulp is never empty.
   (urd-fill t)
    (: q (peep t 'q ())
       c (see q)
       (? (c = -1) (: _ (pin t 'e 1) 0)
          (: _ (unsee q c)
             _ (urd-drop t)
             _ (pin t 'h ((peep t 'h "") + (chug q)))
             1)))
   ; the next line -> [line term?], () when spent. term? is whether the SOURCE ended
   ; it with a newline -- what `nl-end?` answers on the whole-text road.
   ; ⚠ the scan mark is not an optimisation: without it the held bytes are rescanned
   ; from the front every gulp, so an input with no newline costs its length squared.
   (urd-line t)
    (: h (peep t 'h "") n (tally h) p (peep t 'p 0) m (peep t 'm 0)
       (idx k) (? (n <= k) -1 ((h k) = 10) k (idx (k + 1)))
       i (idx (m < p ? p m))
       (? (0 <= i) (: _ (pin t 'p (i + 1)) _ (pin t 'm (i + 1)) [(snip h p i) 1])
          (peep t 'e 0)
           (? (p < n) (: _ (pin t 'h "") _ (pin t 'p 0) _ (pin t 'm 0)
                         [(snip h p n) 0])
              ())
          (: _ (pin t 'm n) _ (urd-fill t) (urd-line t))))
   ; the gulp just read, handed over and let go; "" when the input is spent
   (urd-gulp t)
    (? (urd-fill t)
       (: h (peep t 'h "") p (peep t 'p 0)
          _ (pin t 'h "") _ (pin t 'p 0) _ (pin t 'm 0)
          (? (p = 0) h (snip h p (tally h))))
       "")

   ; --- the operand walk -----------------------------------------------------
   ; each name in turn, a miss complains and costs the status without stopping the
   ; run. ⚠ no operands at all IS one operand, and it is stdin.
   (uwalk one as)
    (: (go a ok) (? (two? a) (go >a (one <a ok)) (: _ (flush out) (? ok 0 1)))
       (? (two? as) (go as 1) (: _ (one "-" 1) _ (flush out) 0)))
   ; an operand open for reading -> [port close?] | (). ⚠ "-" is stdin and is NOT
   ; closed after: a later operand may be it again. `miss` says the complaint, because
   ; grep and sed each owe GNU a different sentence for the same failure.
   (uin miss f)
    (? (f = "-") [in 0]
       (: q (open f "r") (? q [q 1] (: _ (miss f) ()))))
   (umiss who) (\ f (: _ (say err (who + ": cannot open " + f)) _ (put err 10) 0))
   ; THE DRIVER the rest ride: body gets (name first? reader) per operand and answers
   ; truth to keep the status. first? is 1 on the leading one, all `uhdr` needs.
   (ureachm miss body as)
    (: t (tablet 0) _ (pin t 'f 1)
       (uwalk (\ f ok
                (: p (uin miss f)
                   (? (! p) 0
                      (: fst (peep t 'f 1)
                         _ (pin t 'f 0)
                         v (body f fst (urd <p))
                         _ (? <>p (close <p) 0)
                         (? v ok 0)))))
              as))
   (ureach who body as) (ureachm (umiss who) body as)
   ; dot gets the bytes a bufferful at a time -- for tools that do not care where
   ; the lines fall (cat)
   (uchunks who dot as)
    (ureach who
            (\ f fst rd (: (go _) (: h (urd-gulp rd) (? (tally h) (: _ (dot h) (go 0)) 1))
                           (go 0)))
            as)
   ; --- the operands AS ONE STREAM -------------------------------------------
   ; sed's joined lane: the line numbers and `$` run through the files, so the read
   ; has to cross them. ⚠ ONE LINE OF LOOKAHEAD, which is the whole reason this is
   ; not `ureach` -- `$` is "no further line anywhere", and nothing shorter answers it.
   ; ⚠ 'bad is read off the box AFTER the walk: a miss cannot stop a joined stream,
   ; it only costs the status, and by then the reader has passed it.
   (urdj-box miss fs)
    (: t (tablet 0)
       _ (pin t 'm miss) _ (pin t 'fs (? (two? fs) fs '("-")))
       _ (pin t 'rd ()) _ (pin t 'cl ()) _ (pin t 'bad 0) _ (pin t 'h "")
       _ (pin t 'hp 0)
       t)
   ; open the next operand; 1 when one did, 0 when the list is spent
   (urdj-step t)
    (: _ (? (peep t 'cl ()) (: _ (close (peep t 'cl ())) (pin t 'cl ())) 0)
       fs (peep t 'fs ())
       (? (! (two? fs)) 0
          (: _ (pin t 'fs >fs)
             p (uin (peep t 'm ()) <fs)
             (? (! p) (: _ (pin t 'bad 1) (urdj-step t))
                (: _ (pin t 'rd (urd <p))
                   _ (? <>p (pin t 'cl <p) 0)
                   1)))))
   ; the next line ignoring the lookahead, crossing operands as it runs out of them
   (urdj-raw t)
    (: rd (peep t 'rd ())
       l (? rd (urd-line rd) ())
       (? (two? l) l (? (urdj-step t) (urdj-raw t) ())))
   (urdj-line t)                            ; -> [line term? last?] | ()
    (: pk (peep t 'pk ())
       (? (! (two? pk)) ()
          (: nx (urdj-raw t)
             _ (pin t 'pk nx)
             [<pk <>pk (? (two? nx) 0 1)])))
   ; the LINE face: primed, so `last?` is answerable from the first line on
   (urdj miss fs) (: t (urdj-box miss fs) _ (pin t 'pk (urdj-raw t)) t)
   ; the BYTE face: unprimed, because priming would eat a line. od dumps ROWS and its
   ; offsets run through the operands, which is the same join and not the same unit.
   (urdb miss fs) (urdj-box miss fs)
   ; up to k bytes off the joined stream, "" when it is spent. 'hp is the unread
   ; cursor: od takes its rows in small bites, and re-copying the held tail behind
   ; each one costs the gulp squared
   (urdj-drop t)
    (: p (peep t 'hp 0)
       (? (p = 0) 0
          (: h (peep t 'h "")
             _ (pin t 'h (snip h p (tally h)))
             (pin t 'hp 0))))
   (urdj-take t k)
    (: (fill _) (? (k <= ((tally (peep t 'h "")) - (peep t 'hp 0)))  1
                   (: _ (urdj-drop t)
                      rd (peep t 'rd ())
                      g (? rd (urd-gulp rd) "")
                      (? (tally g) (: _ (pin t 'h ((peep t 'h "") + g)) (fill 0))
                         (? (urdj-step t) (fill 0) 0))))
       _ (fill 0)
       h (peep t 'h "")
       p (peep t 'hp 0)
       a ((tally h) - p)
       m (? (a < k) a k)
       _ (pin t 'hp (p + m))
       (snip h p (p + m)))

   ; dot gets (line term?), one line at a time -- every tool whose whole-text body was
   ; `(go (lines t))`. ⚠ dot answering 0 STOPS this operand, so a tool can read no
   ; further than it has to.
   (ulines who dot as)
    (ureach who
            (\ f fst rd (: (go _) (: l (urd-line rd)
                                     (? (! (two? l)) 1 (? (dot <l <>l) (go 0) 1)))
                           (go 0)))
            as)

   ; whitespace-split a text into words (the xargs face, quote-blind)
   (uwords t)
    (: n (tally t)
       (go i st acc)
        (? (n <= i) (rev (? (st < 0) acc (snip t st i >< acc)))
           (: c (t i)
              w ((c = 32) || (c = 9) || (c = 10) || (c = 13))
              (? w (go (i + 1) -1 (? (st < 0) acc (snip t st i >< acc)))
                 (go (i + 1) (? (st < 0) i st) acc))))
       (go 0 -1 ())))
) ; module 'kore
