; crew/kore/less.l -- the pager: one screenful of a text at a time, with the keys
; less answers to. `less` and `more` are the SAME door -- the toolbox holds one
; pager and those are the two spellings a script reaches for, so both land here
; and both behave like less (backward motion, no exit at the end unless -e).
;
; the split is crew/vi/'s, for the same reason: the engine is pure -- a state
; tablet stepped one BYTE at a time (pgstep) and rendered to one escape frame
; (pgframe), no port and no tty in it -- and the face below reads keys, performs
; the file requests the engine leaves, and blits. the laws drive pgstep with byte
; lists and read the tablet back.
;
; keys: SPACE f ^F ^V forward a screen, b ^B back one, RETURN j e ^N ^E down a
; line, k y ^Y ^P up one, d ^D half down, u ^U half up, g < top, G > end, p %
; a percentage, a COUNT prefix on any of them (5j, 100g, 50p), / and ? search
; (ERE, over love/re.l), n and N repeat forward and reversed, = ^G the position,
; : then n or p or q the file walk and the exit, h the help, q Q ZZ quit.
; flags: -N line numbers, -i case-blind search, -s runs of blank lines squeezed
; to one, -e quit at the end rather than hold there; +N opens at line N and
; +/PAT at its first match.
;
; the text is HELD: a pager scrolls back, so the reading floor the line tools
; ride is no help here. long lines WRAP, which is less's default -- a display row
; is a fragment carrying the source line it came off, so the counts and the
; percentage stay the source's. a tab steps to the next 8th column, a control
; byte shows as ^X, and a utf-8 tail byte is no column of its own.
; out of scope, documented: horizontal scroll (-S), marks, and following a
; growing file (+F) -- the input is read once.
(use 're)

(module 'kore
(:
   pg-ere (from 're 'reere) pg-fold (from 're 'refold) pg-find (from 're 'refind)

   pg-help
    "  kore less -- the pager (`more` is the same door)

  SPACE  f  ^F  ^V     forward one screenful      N SPACE   forward N lines
  b  ^B                back one screenful         N b       back N lines
  RETURN  j  e  ^N     forward one line           N j       forward N lines
  k  y  ^Y  ^P         back one line              N k       back N lines
  d  ^D                forward half a screen      u  ^U     back half a screen
  g  <                 the first line             N g       line N
  G  >                 the last line              N G       line N
  p  %                 the start                  N p       N percent in

  /PATTERN             search forward (ERE)       n         the next match
  ?PATTERN             search backward            N         the match the other way

  =  ^G                where you are              h         this page
  :n  :p               the next / previous file   :q  q  Q  quit

  -N line numbers   -i case-blind search   -s blank runs squeezed   -e quit at the end
"

   ; -- the display: tabs, columns, and the wrap ---------------------------------
   (pgsp k) (? (k < 1) "" (" " + pgsp (k - 1)))
   ; a source line -> what it looks like on a screen: tabs to the next 8th column,
   ; a control byte as ^X, everything else its own bytes
   (pgtabs ln)
    (: n #ln
       o (jug 0)
       (go k c)
        (? (n <= k) (slurp o)
           (: b (ln k)
              (? (b = 9) (: p (8 - c % 8) _ (say o (pgsp p)) (go (k + 1) (c + p)))
                 (b = 127) (: _ (say o "^?") (go (k + 1) (c + 2)))
                 (b < 32) (: _ (say o ("^" + string (b + 64))) (go (k + 1) (c + 2)))
                 (: _ (put o b) (go (k + 1) (c + 1))))))
       (go 0 0))
   ; the byte index where column k of s falls -- a utf-8 tail byte is no column.
   ; the count is taken at the LEAD byte and the stop is BEFORE it, or the cut
   ; lands in the middle of the character that would have overflowed
   (pgspan s k)
    (: n #s
       (go i c) (? (n <= i) i
                   (: c2 (? (s i & 192 = 128) c (c + 1))
                      (? (k < c2) i (go (i + 1) c2))))
       (go 0 0))
   (pgclip w s) (: k (pgspan s w) (? (k < #s) (snip s 0 k) s))
   ; the STATUS line clips from the LEFT, the way vi's does: its tail is what
   ; carries the news -- the line span, the percent, the end of the / being typed
   ; -- and a deep path would otherwise push every word of it off the screen
   (pgclipl w s) (: k #s (? (w < k) ("<" + snip s ((k - w) + 1) k) s))
   ; a line -> its fragments at width w, at least one, so a blank line is a row
   (pgwrap w ln)
    (: (go s acc) (: k (pgspan s w)
                     (? (#s <= k) (rev (s >< acc))
                        (go (snip s k #s) (snip s 0 k >< acc))))
       (go (pgtabs ln) ()))
   ; the display rows: [fragment line first?], source order
   (pgrows w ls)
    (: (frag fs i acc first) (? !(two? fs) acc
                                (frag >fs i ([<fs i first] >< acc) ()))
       (go l i acc) (? !(two? l) (rev acc)
                       (go >l (i + 1) (frag (pgwrap w <l) i acc 1)))
       (go ls 0 ()))
   ; line -> the first row it owns. walked in order, so the first sighting wins
   (pglrow rw)
    (: t (tablet 0)
       (go r i last) (? !(two? r) t
                        (: ln <>(<r)
                           _ (? (ln = last) 0 (pin t ln i))
                           (go >r (i + 1) ln)))
       (go rw 0 -1))
   ; the k-th element, or () -- a list does not index by application here
   (pgat l i) (? !(two? l) () (i < 1) <l (pgat >l (i - 1)))
   ; the rows from 'top, walked on from the last answer when the view did not move
   ; back. without it every frame re-walks the buffer from its head, which is a
   ; whole 50k-row list per KEYSTROKE once you are near the end of one
   (pgfrom st)
    (: top (st 'top) ct (st 'ct)
       l (? ((0 <= ct) && (ct <= top)) (drop (top - ct) (st 'cr)) (drop top (st 'rw)))
       _ (pins st 'ct top 'cr l)
       l)

   ; -- the state ---------------------------------------------------------------
   (pgflag st c) (peep (st 'opt) c 0)
   (pgnew opt w h)
    (: st (tablet 0)
       _ (pins st 'opt opt 'w w 'h h 'ls () 'rw () 'lr (tablet 0) 'nr 0 'nl 0)
       _ (pins st 'top 0 'file "" 'files () 'fi 0 'msg "" 'cnt "" 'mode 'n)
       _ (pins st 'cmd "" 'lead 47 'pat "" 'nds () 'dir 1 'zz () 'quit () 'doe () 'sav () 'ct -1 'cr ())
       st)
   (pgwidth st) (: w (st 'w - (? (pgflag st 78) 8 0)) (? (w < 1) 1 w))
   (pgbody st) (: b (st 'h - 1) (? (b < 1) 1 b))
   (pgmax st) (: m (st 'nr - pgbody st) (? (m < 0) 0 m))
   ; blank runs to one blank (-s)
   (pgsqueeze ls)
    (: (go l was acc) (? !(two? l) (rev acc)
                         (: b !(0 < #(<l))
                            (? (b && was) (go >l b acc) (go >l b (<l >< acc)))))
       (go ls 0 ()))
   (pgwrapall st)
    (: rw (pgrows (pgwidth st) (st 'ls))
       _ (pins st 'rw rw 'nr #rw 'lr (pglrow rw) 'ct -1 'cr ())
       st)
   ; a text's lines into the buffer under a name, at the top
   (pgload st ls nm)
    (: l2 (? (pgflag st 115) (pgsqueeze ls) ls)
       _ (pins st 'ls l2 'nl #l2 'file nm)
       _ (pgwrapall st)
       _ (pin st 'top 0)
       st)
   (pgmsg st m) (: _ (pin st 'msg m) st)

   ; -- the motions -------------------------------------------------------------
   (pggo st k) (: m (pgmax st) _ (pin st 'top (? (k < 0) 0 (m < k) m k)) st)
   (pgtopline st) (: r <(pgfrom st) (? (two? r) <>r 0))
   ; the row a source line starts on, clamped to the buffer
   (pgline! st i)
    (: n (st 'nl)
       k (? (i < 0) 0 (n <= i) (? (n < 1) 0 (n - 1)) i)
       (pggo st (peep (st 'lr) k 0)))
   (pgpct st n) (: k ((? (n < 0) 0 (100 < n) 100 n) * (st 'nl)) (k // 100))
   ; forward: at the end already, -e is the door out
   (pgdown st k)
    (: t (st 'top) m (pgmax st)
       (? ((t = m) && (pgflag st 101)) (pgquit st) (pggo st (t + k))))
   (pgup st k) (pggo st (st 'top - k))

   ; -- the search --------------------------------------------------------------
   ; a pattern -> (nodes) or (), so an empty pattern is still a presence
   (pgnds st p)
    (: r (pg-ere p)
       (? !(two? r) () [(? (pgflag st 105) (pg-fold <>r) <>r)]))
   ; the nearest source line from i in direction d that the nodes match, or -1
   (pgseek st nds i d)
    (: ls (st 'ls)
       (hit ln) (two? (pg-find nds ln 0))
       (fwd l k) (? !(two? l) -1 (hit <l) k (fwd >l (k + 1)))
       (bak l k best) (? (!(two? l) || (i < k)) best
                         (bak >l (k + 1) (? (hit <l) k best)))
       (? (i < 0) (? (0 < d) (fwd ls 0) -1)
          (0 < d) (fwd (drop i ls) i)
          (bak ls 0 -1)))
   ; the stored pattern, one step on in direction d
   (pghunt st d)
    (: w (st 'nds)
       (? !(two? w) (pgmsg st "no previous pattern")
          (: _ (pin st 'dir d)
             i (pgseek st <w (pgtopline st + d) d)
             (? (i < 0) (pgmsg st ("pattern not found: " + st 'pat)) (pgline! st i)))))
   (pgsearch st p d)
    (: w (? (0 < #p) (pgnds st p) (st 'nds))
       (? ((0 < #p) && !(two? w)) (pgmsg st ("bad pattern: " + p))
          !(two? w) (pgmsg st "no previous pattern")
          (: _ (pin st 'nds w)
             _ (? (0 < #p) (pin st 'pat p) 0)
             (pghunt st d))))

   ; -- the help page, and the file walk ----------------------------------------
   (pghelp st)
    (? (two? (st 'sav)) st
       (: _ (pin st 'sav [(st 'ls) (st 'file) (st 'top)])
          (pgload st (lines pg-help) "HELP")))
   ; back out of the help -- 1 when there was a page to go back to
   (pgback st)
    (: s (st 'sav)
       (? !(two? s) ()
          (: _ (pin st 'sav ())
             _ (pgload st <s <>s)
             _ (pggo st <>>s)
             1)))
   (pgquit st) (? (pgback st) st (pin st 'quit 1))
   ; the operand walk: the engine names the file, the face reads it
   (pgfile st d)
    (: fs (st 'files) k (st 'fi + d)
       (? (k < 0) (pgmsg st "no previous file")
          (#fs <= k) (pgmsg st "no next file")
          (: _ (pin st 'fi k) _ (pin st 'doe [(pgat fs k)]) st)))

   ; -- the step ----------------------------------------------------------------
   (pgcnt st d) (: c (st 'cnt) (? (0 < #c) (uatoi c) d))
   (pgask st ld) (: _ (pins st 'mode 'c 'lead ld 'cmd "") st)
   ; the : commands are one letter, typed straight after the colon
   (pgcolon st c)
    (: _ (pin st 'mode 'n)
       (? (c = 110) (pgfile st 1)
          (c = 112) (pgfile st -1)
          ((c = 113) || (c = 81)) (pin st 'quit 1)
          (c = 27) st
          (pgmsg st ("no : command " + string c))))
   ; the typed line: RETURN runs it, ESC drops it, a backspace off the front does too
   (pgtyped st c)
    (: cm (st 'cmd)
       (? ((c = 13) || (c = 10))
           (: _ (pins st 'mode 'n 'cmd "")
              (? (st 'lead = 63) (pgsearch st cm -1) (pgsearch st cm 1)))
          (c = 27) (: _ (pins st 'mode 'n 'cmd "") st)
          ((c = 8) || (c = 127))
           (? (0 < #cm) (: _ (pin st 'cmd (snip cm 0 (#cm - 1))) st)
              (: _ (pins st 'mode 'n 'cmd "") st))
          (c < 32) st
          (: _ (pin st 'cmd (cm + string c)) st)))
   ; one key in normal mode; a digit stacks a count and everything else spends it
   (pgnorm st c)
    (: _ (pin st 'msg "")
       b (pgbody st)
       half (: k (b // 2) (? (k < 1) 1 k))
       n (pgcnt st 0)
       zz (st 'zz)
       (? (48 <= c <= 57) (: _ (pin st 'cnt (st 'cnt + string c)) st)
          (: _ (pins st 'cnt "" 'zz ())
             (? ((c = 32) || (c = 102) || (c = 6) || (c = 22)) (pgdown st (? n n b))
                ((c = 13) || (c = 10) || (c = 106) || (c = 14) || (c = 101) || (c = 5))
                 (pgdown st (? n n 1))
                ((c = 98) || (c = 2)) (pgup st (? n n b))
                ((c = 107) || (c = 121) || (c = 25) || (c = 16)) (pgup st (? n n 1))
                ((c = 100) || (c = 4)) (pgdown st (? n n half))
                ((c = 117) || (c = 21)) (pgup st (? n n half))
                ((c = 103) || (c = 60)) (pgline! st ((? n n 1) - 1))
                ((c = 71) || (c = 62)) (pgline! st ((? n n (st 'nl)) - 1))
                ((c = 112) || (c = 37)) (pgline! st (pgpct st n))
                (c = 47) (pgask st 47)
                (c = 63) (pgask st 63)
                (c = 110) (pghunt st (st 'dir))
                (c = 78) (pghunt st (0 - st 'dir))
                (c = 58) (: _ (pin st 'mode 'x) st)
                ((c = 61) || (c = 7)) (pgmsg st (pgstat st))
                ((c = 104) || (c = 72)) (pghelp st)
                ((c = 113) || (c = 81)) (pgquit st)
                (c = 90) (? zz (pgquit st) (: _ (pin st 'zz 1) st))
                st))))
   (pgstep st c)
    (? (st 'mode = 'c) (pgtyped st c)
       (st 'mode = 'x) (pgcolon st c)
       (pgnorm st c))

   ; -- the frame ---------------------------------------------------------------
   ; the last visible source line -- the tail of the window, or the buffer's end
   (pglast st)
    (: b (pgbody st) top (st 'top) k (top + b)
       r (pgat (pgfrom st) (? (st 'nr < k) ((st 'nr - top) - 1) (b - 1)))
       (? (two? r) (<>r + 1) (st 'nl)))
   (pgstat st)
    (: b (pgbody st) nl (st 'nl)
       end? (st 'nr <= (st 'top + b))
       l2 (pglast st)
       pc (? (nl < 1) 100 (l2 * 100 // nl))
       ((? (0 < tally (st 'file)) (st 'file) "-") + "  " + show (pgtopline st + 1)
        + "-" + show l2 + "/" + show nl + "  " + show pc + "%"
        + (? end? "  (END)" "")))
   ; the matches in a fragment, in standout. a match spanning a wrap shows on both
   ; halves, which is what painting the FRAGMENT rather than the line buys
   (pgpaint e nds s)
    (: n #s
       (go i acc)
        (? (n <= i) (acc + snip s i n)
           (: m (pg-find nds s i)
              (? !(two? m) (acc + snip s i n)
                 (: a <>m z <>>m
                    (? (z <= a) (acc + snip s i n)
                       (go z (acc + snip s i a + e + "[7m" + snip s a z + e + "[27m")))))))
       (go 0 ""))
   (pgframe st)
    (: e (string 27)
       w (st 'w) b (pgbody st)
       nds (: v (st 'nds) (? (two? v) <v ()))
       (lay r) (: fr <r
                  tx (? (two? nds) (pgpaint e nds fr) fr)
                  (? (pgflag st 78)
                     ((? <>>r (upad 7 (show (<>r + 1))) "       ") + " " + tx) tx))
       (rows l k acc)
        (? (b <= k) acc
           (rows (? (two? l) >l l) (k + 1)
                 (acc + (? (two? l) (lay <l) "~") + e + "[K" + "\r\n")))
       ask (? (st 'mode = 'c) (string (st 'lead) + st 'cmd)
              (st 'mode = 'x) ":"
              (0 < tally (st 'msg)) (st 'msg)
              (pgstat st))
       line (pgclipl w ask)
       (e + "[H" + rows (pgfrom st) 0 ""
          + e + "[7m" + line + e + "[0m" + e + "[K"
          + e + "[" + show (st 'h) + ";" + show (tally line + 1) + "H"))

   ; -- the face: keys off a port, frames onto out -------------------------------
   ; not a terminal: pour the operands through, which is what more and less both
   ; do when nothing is going to read a screen
   (pgpour as) (uchunks "less" puts as)
   ; less's start command: +N opens at line N, +/PAT at its first match
   (pgstart st p)
    (? !(0 < #p) st
       (p 0 = 47) (pgsearch st (snip p 1 #p) 1)
       (pgline! st (uatoi p - 1)))
   ; one key, the arrow and page keys decoded to their plain twins. any other byte
   ; after ESC is delivered AFTER the bare 27 (a one-byte pushback), so ESC q reads
   ; as the two keys it is. answers (byte next-pushback)
   (pgkey kq pend)
    (? (0 <= pend) [pend -1]
       (: c (see kq)
          (? !(c = 27) [c -1]
             (: c2 (see kq)
                (? !(c2 = 91) [27 c2]
                   (: (tail d) (: c3 (see kq)
                                  (? (48 <= c3 <= 57) (tail c3) [d c3]))
                      r (tail -1)
                      d <r f <>r
                      [(? (f = 65) 107 (f = 66) 106 (f = 72) 103 (f = 70) 71
                          ((f = 126) && (d = 53)) 98
                          ((f = 126) && (d = 54)) 32
                          ((f = 126) && (d = 49)) 103
                          ((f = 126) && (d = 52)) 71
                          27) -1]))))))
   ; winsize's (rows . cols) as [h w]. a fresh pty answers 0x0 before anyone has
   ; sized it, and that is a terminal with no size yet, not a missing one
   (pgwin ws) [(? (0 < <ws) <ws 24) (? (0 < >ws) >ws 80)]
   ; the window may move under us; the top LINE is what survives a rewrap
   (pgresize st)
    (: ws (winsize 0)
       (? !(two? ws) 0
          (: hw (pgwin ws)
             (? ((<hw = st 'h) && (<>hw = st 'w)) 0
                (: ln (pgtopline st)
                   _ (pins st 'h <hw 'w <>hw)
                   _ (pgwrapall st)
                   _ (pgline! st ln)
                   1)))))
   ; the loop. fs is the operand list (() when the text came off stdin), kq the
   ; keyboard port -- `in` when an operand is the text, /dev/tty when stdin was
   (pgride fs ls nm kq w h opt plus)
    (: st (pgnew opt w h)
       _ (pin st 'files fs)
       _ (pgload st ls nm)
       _ (pgstart st plus)
       e (string 27)
       _ (raw 1)
       _ (puts (e + "[?1049h"))
       (fin _) (: _ (puts (e + "[0m" + e + "[?1049l")) _ (flush out) _ (raw 0) 0)
       (doe _)
        (: rq (st 'doe)
           (? !(two? rq) 0
              (: _ (pin st 'doe ())
                 nm2 <rq
                 r (uread nm2)
                 (? (two? r) (: _ (pgload st (lines <>r) nm2) 0)
                    (: _ (pgmsg st (nm2 + ": cannot open")) 0)))))
       (go pend)
        (: _ (pgresize st)
           _ (puts (pgframe st)) _ (flush out)
           kr (pgkey kq pend)
           c <kr
           (? (c < 0) (fin ())
              (: _ (pgstep st c)
                 _ (doe ())
                 (? (st 'quit) (fin ()) (go <>kr)))))
       (go -1))
   ; the flags kore's pager knows, and the complaint for anything else
   (pgopts as)
    (: t (tablet 0)
       o (uopts as t () ())
       _ (? (two? o) () (udie 2 "less: bad option"))
       _ (foldl (\ a c (? (member? c '(78 105 115 101)) a
                          (udie 2 ("less: unknown option -" + string c))))
                0 (keys t))
       [t <>>o])
   (less-main as)
    (: r (pgopts as)
       t <r
       rest <>r
       ; +N / +/PAT sits ahead of the names, which is where less takes it
       plus? ((two? rest) && (1 < #(<rest)) && (<rest 0 = 43))
       plus (? plus? (snip <rest 1 #(<rest)) "")
       fs (? plus? >rest rest)
       ws (winsize 0)
       hw (? (two? ws) (pgwin ws) [24 80])
       (? !(two? ws) (pgpour fs)
          (two? fs)
           (: f <fs
              rd (uread f)
              _ (? (two? rd) () (udie 1 ("less: cannot open " + f)))
              (pgride fs (lines <>rd) f in <>hw <hw t plus))
          ; no operand, so stdin is the TEXT -- and fd 0 is the only door `raw`
          ; has, so the pipe moves aside and /dev/tty takes fd 0. no controlling
          ; terminal, no keys: pour it through instead. ⚠ stdin being the terminal
          ; means there is no text at all, and `raw` is the isatty we have: it
          ; answers () for a terminal on fd 0 and an errno for anything else
          (: tty0? !(raw 1)
             _ (raw 0)
             _ (? tty0? (udie 2 "usage: less [-Nise] [+N|+/PAT] [FILE..]") 0)
             dfd (dup 0)
             tfd (openfd "/dev/tty" 0)
             (? ((tfd < 0) || (dfd < 0))
                 (: _ (? (0 <= dfd) (fdclose dfd) 0) (pgpour ()))
                (: _ (dup2 tfd 0)
                   _ (fdclose tfd)
                   q (fdopen dfd)
                   txt (slurp q)
                   _ (close q)
                   (pgride () (lines txt) "" (fdopen 0) <>hw <hw t plus)))))))
) ; module 'kore
