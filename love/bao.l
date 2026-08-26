; bao -- the shell core: the read-eval-print loop, its multi-line line editor,
; the stream shell (read/reads), the floor handler (welp), and the port-driven
; line reader (edraw/edln/edlnc) that lush drives.

(module '(bao read reads forms shell welp edraw bao edln edlnc)
(: (read s)
   (: r (sound (? (hot? s) (flow s) s))
      (? (two? r) (ev <r) 0)))
; reads: the stream shell -- reads and evals every form of a port, the value the last
; form's; ev late-binds ((ev 'ev r)) so one definition drives both of love0's passes.
; the two endings differ: a clean end comes between forms, `torn` inside one -- and torn
; is a scare, the reader having drunk for itself and found the end.
(: (reads p)
   (: (walk cl z) (sound cl @ 'torn (scare 'unfinished-form p)
                              (a >< b) (walk b (ev 'ev a))
                              z)
      ; the shared lane, for `in`: gulp for speed, hold nothing across an eval. box slot 0 counts
      ; every byte drawn off p, so the walk can put the port where it belongs. no window on purpose:
      ; a straddling `;` comment would end one cleanly and the next reads the prose as code.
      box (tablet 0)
      (drew u) (peep box 0 0)
      ; absolute, not relative -- p is already behind us, so a plain (unchug p rem) re-reads the run
      (drop n) (unchug p (n - inhand p))     ; the port to n pending; we hold the rest
      ; flow, plus the count: one chug per gulp, the run laid down as plain conses.
      (feed u)
       (see p @ -1 () c
        (: _ (unsee p c)
           t (chug p)
           _ (pin box 0 (drew () + tally t))
           (go k l) (? (0 >= k) l (: j (k - 1) (go j (t j >< l))))
         (go (tally t) (once (\ u (feed ()))))))
      ; consumed, forwards and by identity. never (tally >r) -- the residue is lazy, so that
      ; forces the whole rest; and force at every step, or a plain `cup` walk stops dead at a
      ; gulp boundary and spins on () forever. `once` memoizes, so re-forcing is free.
      (frc x) (lit? x ? (x ()) x)
      (upto a b n) (: aa (frc a) bb (frc b)
                      (? (id? aa bb) n
                         (two? aa)   (upto (cup aa) bb (n + 1))
                         n))
      ; with no window both endings are honest -- they can only mean the stream itself ended
      (step cl walked z)
       (sound cl @
         'torn (scare 'unfinished-form p)
         (a >< b) (: k (upto cl b 0)
                     w2 (walked + k)
                     rem (drew () - w2)
                     ; one byte short on purpose -- the byte-exact lane runs one ahead, and test_stdincorpus diffs it
                     _ (drop (0 < rem ? (rem - 1) 0))
                     h (inhand p)
                     v (ev 'ev a)
                     (h = inhand p ? (drop 0, step b w2 v)
                                     (start (two? b ? [<b]) v)))
         (drop 0, z))
      (start pre z) (: c (see p)
                       (? (= c -1) (? (two? pre) (walk pre z) z)
                          (: _ (unsee p c)
                             (? (nil? (inhand p)) (walk (cat pre (trickle p)) z)
                                (: _ (pin box 0 (tally pre))
                                   (step (cat pre (feed ())) 0 z))))))
      ; never rebind `in` -- this identity folded at egg-compile, so a fresh port fails it in silence
      (? (id? p in) (start () 0) (walk (flow p) 0))))
(: (forms p)
   ((: (g cl acc) (sound cl @ 'torn (scare 'unfinished-form p)
                              (a >< b) (g b (a >< acc))
                              (rev acc)))
    (flow p) ()))

(: m (nom 0) eofsym (nom 0)

   (want n) (member? n (names ()) ? (ev n) (\ _ ()))
   raw (want 'raw)


   (rdall cl acc)
    (sound cl @ 'torn m                            ; unfinished shape -> more
                (a >< b) (rdall b (a >< acc))
                (rev acc))                         ; a clean end -> the datums, in order
   (parseall cl)
    (rdall cl () @ ^m  m                           ; unfinished shape
                   [a] [a]                         ; one datum: as-is (leading monadic preserved)
                   ds  (two? ds ? [ds]))           ; many: the application; a blank line: ()

   ; --- editor ---

   ; intersperse a list of charlists with newlines, in order
   (joinln ls)
    (ls @ (a >< b) (two? b ? (a + (10 >< joinln b)) a))

   ; flatten the editor state into one charlist suitable for parseall
   (flatten u l r d)
     (joinln (revcat u (revcat l r >< d)))

   ; walk xs forward taking n chars into acc (reversed); when done
   ; call k with (rev (take n xs)) and (drop n xs). stops cleanly at
   ; end-of-xs even if n exceeds the list length.
   (splitat k acc n xs)
     (? (two? xs && 0 < n) (splitat k (<xs >< acc) (n - 1) >xs)
        (k acc xs))

   ; map non-printables to space; otherwise pass through
   (prc c) (putc $ 32 <= c < 127 ? c 32)

   ; line editor buffer zipper arguments
   ; u up (reversed), l left (reversed), r right, d down
   (edleft k u l r d)
     (? (two? l) (k u >l (<l >< r) d)
        (two? u) (k >u (rev <u) () (revcat l r >< d))
        (k u l r d))
   (edright k u l r d)
     (? (two? r) (k u (<r >< l) >r d)
        (two? d) (k (revcat l r >< u) () <d >d)
        (k u l r d))
   (edbsp k u l r d)
     (? (two? l) (k u >l r d)
        (two? u) (k >u (rev <u) r d)
        (k u l r d))
   (eddel k u l r d)
     (? (two? r) (k u l >r d)
        (two? d) (k u l <d >d)
        (k u l r d))
   (edhome k u l r d) (k u () (revcat l r) d)
   (edend  k u l r d) (k u (revcat r l) () d)

   ; edtop / edbot jump across the whole buffer: cursor at the start of
   ; the topmost line / end of the bottommost line. on a single-line
   ; buffer they degenerate to edhome / edend.
   (edtop k u l r d)
     (? (two? u) (: ru (rev u)
                  (k () () <ru (>ru + (revcat l r >< d))))
        (k u () (revcat l r) d))
   (edbot k u l r d)
     (? (two? d) (: rd (rev d)
                  (k (>rd + (revcat l r >< u)) (revcat <rd ()) () ()))
        (k u (revcat r l) () d))
   (edup k u l r d)
     (? (two? u) (splitat (\ lz rr (k >u lz rr (revcat l r >< d)))
                          () (tally l) <u)
        (k u l r d))
   (eddown k u l r d)
     (? (two? d) (splitat (\ lz rr (k (revcat l r >< u) lz rr >d))
                          () (tally l) <d)
        (k u l r d))

   ; --- history zipper ---

   ; pack the buffer's four halves into one slot for history storage.
   (mkframe u l r d) (u >< l >< r >< d)

   ; if cur holds a pristine recall, restore it at its slot in hu;
   ; called before any mutating edit so the original survives.
   (detach hu cur) (two? cur ? (cur >< hu) hu)

   (emptybuf u l r d) (!u && !l && !r && !d)
   (parses u l r d)   (two? (parseall (flatten u l r d)))

   ; up/down through the history zipper. on a normal pop, push the
   ; current buffer onto the other side (skipping if empty); if the
   ; other side is empty (hu or hd) but we're on a real recall (cur
   ; non-nil) and the buffer parses, push and scroll into a fresh
   ; empty line.
   (uphist k u l r d hu hd cur)
     (? (two? hu)
        (: e <hu
           nhd (? (emptybuf u l r d) hd (mkframe u l r d >< hd))
           (k <e <>e <>>e >>>e >hu nhd e))
        (two? cur && parses u l r d)
        (: nhd (mkframe u l r d >< hd)
           (k () () () () hu nhd ()))
        (k u l r d hu hd cur))
   (downhist k u l r d hu hd cur)
     (? (two? hd)
        (: e <hd
           nhu (? (emptybuf u l r d) hu (mkframe u l r d >< hu))
           (k <e <>e <>>e >>>e nhu >hd e))
        (two? cur && parses u l r d)
        (: nhu (mkframe u l r d >< hu)
           (k () () () () nhu hd ()))
        (k u l r d hu hd cur))

   ; 0 is the miss, read with id? -- an event code nets red, so truth cannot tell hit from miss
   arrow    {68 -1 67 -2 65 -8 66 -9 72 -5 70 -6}    ; ESC [ F -- the plain finals
   modarrow {68 -1 67 -2 65 -8 66 -9 72 -10 70 -11}  ; ESC [ 1 ; M F -- modified home/end jump the buffer
   evk      {-1 -7 4 -7 3 -12 13 10 10 10 8 -3 127 -3 1 -5 5 -6}   ; the raw control bytes
   (edesc1 c) (? (c = 126) -5
                 (c = 59)  (getc 0, peep modarrow (getc 0) 0))
   (edesc x)
    (? !(91 = getc 0) 0
     (: c (getc 0) e (peep arrow c 0)
      (? !(id? e 0) e
       (c @ 49 (edesc1 (getc 0))
            55 (getc 0, -5)
            (52 || 56) (getc 0, -6)
            51 (getc 0, -4)
            0))))

   (edev x)
    (: c (getc 0) e (peep evk c 0)
     (? !(id? e 0) e
        (c = 27) (edesc 0)
        (32 <= c < 127 ? c)))

   ; redraw the buffer in place. pra is rows above the cursor from the previous
   ; render -- move up that many first, so we land back on the prompt's row. all
   ; motion is relative, so terminal scrolling never desyncs us. pl is the prompt's
   ; column; the topmost line shares it, later lines start at column 0.
   (csi n f) (putc 27, putc 91, putn n, putc f)   ; ESC [ n f -- the one cursor verb
   (edrender pl pra u l r d)
     (: _ (0 < pra ? (csi pra 65),
           putc 13,
           0 < pl ? (csi pl 67),
           putc 27, putc 91, putc 74, ; clear to end of screen
           each (rev u) (\ ln (each ln prc, putc 10)),
           each (revcat l r) prc,
           each d (\ ln (putc 10, each ln prc)))
        ld (tally d)
        col ((two? u ? 0 pl) + tally l)
        (0 < ld ? (csi ld 65),
         putc 13,
         0 < col ? (csi col 67),
         flush out))

   ps1 "> "
   ; the helm glows in the crew's cyan (256-colour 117 ~ #7dcfff), reset after.
   ; SGR is zero-width to both the host terminal and the inle console (crew/quay/quay.c),
   ; so `pl` below stays (tally ps1) and the cursor math is undisturbed.
   ps1c (string 27 + "[38;5;117m" + ps1 + string 27 + "[39m")

   ; print the prompt, then dispatch events until ^D or a submitted buffer.
   ; returns eofsym on ^D, else (vs . nhu) so the repl can thread history.
   (edline hu hd)
     (: pr ps1
        _ (puts ps1c)
        pl (tally pr)
        (loop pra u l r d hu hd cur)
          (: _ (edrender pl pra u l r d)
             npra (tally u)
             (kloop uu lz rr dd) (loop npra uu lz rr dd hu hd cur)
             (kedit uu lz rr dd) (loop npra uu lz rr dd (detach hu cur) hd ())
             (khist uu lz rr dd nhu nhd ncur) (loop npra uu lz rr dd nhu nhd ncur)
             ; the dispatch stays a chain, not a table: every arm runs a different
             ; continuation over the live buffer, so a table would hold closures
             ; built per keystroke. the tables above work because their arms are constants.
             (edev 0 @
              -7 eofsym
              ; enter cashes the buffer as long as it parses complete
              10 (parseall (flatten u l r d) @
                  ^m (loop npra (rev l >< u) () r d (detach hu cur) hd ())
                  vs (two? d ? (csi (tally d) 66) (),
                      putc 10,
                      [vs (!vs ? (revcat hd (detach hu cur))
                           (take 100 (mkframe u l r d >< revcat hd (detach hu cur)))) ]))
              (c && 0 < c) (loop npra u (c >< l) r d (detach hu cur) hd ())
              -1 (edleft  kloop u l r d)
              -2 (edright kloop u l r d)
              -3 (edbsp   kedit u l r d)
              -4 (eddel   kedit u l r d)
              -5 (edhome  kloop u l r d)
              -6 (edend   kloop u l r d)
              -8 (two? u ? (edup    kloop u l r d)
                           (uphist   khist u l r d hu hd cur))
              -9 (two? d ? (eddown  kloop u l r d)
                           (downhist khist u l r d hu hd cur))
              -10 (edtop kloop u l r d)
              -11 (edbot kloop u l r d)
              -12 (puts "^C\n", emptybuf u l r d ? eofsym (edline hu hd))
              (loop npra u l r d hu hd cur)))
        (loop 0 () () () () hu hd ()))

   ; evaluate a single parsed datum cooperatively.
   (do_eval v)
     (: pid (twirl ev v)
        (poll _)
          (? (landed? pid) (putx (catch pid), -1)   ; the answer
             (cue? 0) 
             (getc 0 @ 3 (puts "^C", freeze pid, -1) (poll ()))
             (rest 0, poll ()))
        (poll ()))

   ; the read-eval-print loop proper. the host and kernel frontends start the
   ; shell via (shell 0), which calls (repl 0 0).
   (repl hu hd)
    (edline hu hd @ ^eofsym 0
                    (vs >< nhu >< nhd) (each vs (\ v (do_eval v, putc 10)), repl nhu nhd))

   ; no-op () returning scare handler
   (welp a b) ()

   ; the face a raise wears -- ;; a b
   (hface a b) (say err ";; ", print err a, say err " ", print err b, put err 10)
   (shell-help a b) (hface a b, welp a b)
   ; the welp after the quit is not dead -- a frontend with no quit nif folds it, and welp answers
   (file-help a b) (hface a b, quit 1, welp a b)

   _ (!(heard ()) ? (hear file-help))
   (shell _)
    (hear shell-help,             ; the prompt survives every raise; a script must not
     raw 1,
     puts $ ";3 love " love-version "\n",
     repl 0 0)

   (besc1 ip c)
    (c @ 126 -5                               ; a line reader never jumps the buffer:
         59  (see ip, peep arrow (see ip) 0)) ;   the plain finals
   (besc ip) (see ip != 91 ? 0
    (: c (see ip) e (peep arrow c 0)
     (? !(id? e 0) e
      (c @ 49        (besc1 ip (see ip))
           55        (see ip, -5)
           (52 || 56) (see ip, -6)
           51        (see ip, -4)
           0))))
   ; the `see` comes first and exactly once -- evk is indexed by the byte, so peeping before
   ; reading has nothing to peep with
   (bev ip) (: c (see ip) e (peep evk c 0)
               (? !(id? e 0) e
                  (c @ 27 (besc ip)
                       9  -13          ; Tab -> the completion event (edlnc's; others ignore)
                       (32 <= c < 127 ? c))))

   ; (edraw ip pr) -- read one line off port `ip`, with in-line editing
   (edraw ip pr)
     (: pl (tally (puts pr))
        (loop l r)
          (: _ (edrender pl 0 () l r ())
             (km uu ll rr dd) (loop ll rr)         ; motion: edleft/.. give (k u l r d), keep l r
           (bev ip @
            -7 eofsym
            10 (putc 10, revcat l r)
            (c && 0 < c) (loop (c >< l) r)            ; insert a printable
            -1 (edleft  km () l r ())
            -2 (edright km () l r ())
            -3 (edbsp   km () l r ())
            -4 (eddel   km () l r ())
            -5 (edhome  km () l r ())
            -6 (edend   km () l r ())
            (loop l r)))
        (loop () ()))

   ; the shell line reader: edraw + command history
   (edlnc ip pr hist comp)
     (: pl (tally (puts pr)) hn (tally hist)
        (hnth k) <(drop k hist)                       ; hist[k], 0-based (k < hn)
        (loop l r i draft)
          (: _ (edrender pl 0 () l r ())
             (km uu ll rr dd) (loop ll rr i draft)     ; motion: index + draft kept
             (recall ni)                               ; jump to view index ni (0 = the draft)
               (: nd (i = 0 ? (revcat l r) draft)    ; save the live draft once, the instant up leaves it
                  ln (ni = 0 ? nd (hnth (ni - 1)))
                  (loop (rev ln) () ni nd))

             (bev ip @ -7 'eof
                10 (: _ (putc 10)
                            ln (revcat l r)
                            nh (? !ln hist (take 100 (ln >< hist)))
                            (ln >< nh))
                (c && 0 < c) (loop (c >< l) r i draft)
                -12 (: _ (puts "^C") _ (putc 10) (() >< hist))   ; ^C: abandon the line, hist untouched
                -13
                  (? !comp (loop l r i draft)
                     (: t (comp (revcat l ()))
                        (? !(two? t) (loop l r i draft)
                           (: l2 (foldl (\ a ch (ch >< a)) l <t)
                              cs >t
                              _ (? (two? cs && two? >cs)
                                   (putc 10,
                                    puts $ foldl (\ a s (? (tally a) (a + "  " + s) s)) "" cs,
                                    putc 10))
                              (loop l2 r i draft)))))
                -1 (edleft  km () l r ())
                -2 (edright km () l r ())
                -3 (edbsp   km () l r ())
                -4 (eddel   km () l r ())
                -5 (edhome  km () l r ())
                -6 (edend   km () l r ())
                -8 (recall (? (i < hn) (i + 1) i))   ; up: older
                -9 (recall (? (0 < i) (i - 1) i))    ; down: newer
                (loop l r i draft)))
        (loop () () 0 ()))
   (edln ip pr hist) (edlnc ip pr hist ())

   ; (bao a) -- the interactive entry the host frontend launches. `(bao 0)` is
   ; exactly the baked shell (an arg is taken: love has no nullary calls).
   ; the test door -- test/host/baotest.l writes against these, so the names stay closure-private
   (bao a) (a @ 'parts
              {'edleft edleft 'edright edright 'edbsp edbsp 'eddel eddel
               'edhome edhome 'edend edend 'edtop edtop 'edbot edbot
               'edup edup 'eddown eddown 'uphist uphist 'downhist downhist
               'detach detach 'emptybuf emptybuf 'mkframe mkframe
               'parses parses 'joinln joinln 'flatten flatten
               'splitat splitat 'parseall parseall}
              (shell ()))


)
) ; module bao
