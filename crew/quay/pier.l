; pier -- berth steps ashore: the terminal emulator in its own X window.
;
; the same moor as berth (a child on a pty, bytes through quay's cb screen),
; but the painter blits the grid into a 32bpp cask framebuffer -- 8x16
; moderndos glyphs, faces and 256 colours, one C blit per cell -- and ships
; it to the server with chunked PutImage over lux's own wire codec.
; keys come back as KeyPress events, translated keysym -> terminal bytes;
; a ConfigureNotify (lux retiling us) opens a fresh grid and tells the
; pty; WM_DELETE or the child's exit steps away cleanly. an xterm, love-native
; from the socket up.
;
; run: out/host/love -l crew/lux/core.l -l crew/lux/wire.l -l crew/quay/berth.l -l crew/quay/pier.l \
;        -e '(pier (L "sh") "/tmp/.X11-unix/X0" (xauth-cookie "/home/gwen/.Xauthority" "0") ())'
;      the fourth arg is a font atlas: () sails with the builtin moderndos
;      8x16; load crew/quay/psf.l and pass (psf-gz "/usr/share/kbd/console-
;      fonts/ter-132n.psf.gz") to berth terminus 16x32 (any PSF fits).
;      (auth () suits an -ac or Xvfb server.)
;
; wants: wire.l (the codec), berth.l (moor/waitfor), host/cb.c (screen/blit).
; host-only, never in the corpus; boot/pier.l proves it against a headless
; Xvfb, reading the painted window back with GetImage.


(: (px-min a b) (? (< a b) a b))

(: (px-upto n f) ((: (go i) (? (< i n) (: _ (f i) (go (+ i 1))) ())) 0))


 ; --- the new requests wire.l never needed: draw ---

(: (px-mkwin s win parent x y w h) (: _ (x-w8 s 1) _ (x-w8 s 0) _ (x-w16 s 10) ; ground, keys+expose+structure
     _ (x-w32 s win) _ (x-w32 s parent)
     _ (x-w16 s x) _ (x-w16 s y) _ (x-w16 s w) _ (x-w16 s h)
     _ (x-w16 s 0) _ (x-w16 s 1) _ (x-w32 s 0)
     _ (x-w32 s 2050)                         ; CWBackPixel | CWEventMask
     _ (x-w32 s 0)
     (x-w32 s 163841)))
                        ; KeyPress|Exposure|StructureNotify

(: (px-gc s gc win) (: _ (x-w8 s 55) _ (x-w8 s 0) _ (x-w16 s 4) _ (x-w32 s gc) _ (x-w32 s win) (x-w32 s 0)))

(: (px-title s win t) (: n (tally t)
     _ (x-w8 s 18) _ (x-w8 s 0) _ (x-w16 s (+ 6 (/ (+ n (x-pad4 n)) 4)))
     _ (x-w32 s win) _ (x-w32 s 39) _ (x-w32 s 31) _ (x-w8 s 8)
     _ (x-w8 s 0) _ (x-w16 s 0) _ (x-w32 s n) (x-wpad s t)))

(: (px-protocols s win) (: prot (x-intern s "WM_PROTOCOLS") del (x-intern s "WM_DELETE_WINDOW")
     _ (x-w8 s 18) _ (x-w8 s 0) _ (x-w16 s 7)
     _ (x-w32 s win) _ (x-w32 s prot) _ (x-w32 s 4) _ (x-w8 s 32)
     _ (x-w8 s 0) _ (x-w16 s 0) _ (x-w32 s 1) _ (x-w32 s del)
     del))


 ; PutImage, chunked: the request length is 16 bits of 4-byte WORDS, so a
 ; band must fit 262112 data bytes NO MATTER THE WIDTH -- a lux retiling us
 ; to a wide monitor makes rows fat, and one over-long request corrupts the
 ; whole stream (the server reads pixels as opcodes: garble, then silence)
 ; ships pyn PIXEL rows starting at py0; each band pours into a scratch
 ; cask so only the shipped bytes ever copy (never the whole framebuffer)

(: (px-ship s win gc depth fb wpix py0 pyn) (: rowb (* wpix 4)
     band (: v (// 262112 rowb) (? (< v 1) 1 (px-min 64 v)))
     (go y left) (? (< 0 left)
               (: n (px-min band left)
                  bytes (* n rowb)
                  t (cask bytes)
                  _ (pour t 0 fb (* y rowb) bytes)
                  _ (x-w8 s 72) _ (x-w8 s 2) _ (x-w16 s (+ 6 (/ bytes 4)))
                  _ (x-w32 s win) _ (x-w32 s gc)
                  _ (x-w16 s wpix) _ (x-w16 s n)
                  _ (x-w16 s 0) _ (x-w16 s y)
                  _ (x-w8 s 0) _ (x-w8 s depth) _ (x-w16 s 0)
                  _ (x-flush s)                  ; the header leaves first;
                  _ (say s t)                    ; the payload goes say's bulk lane
                  (go (+ y n) (- left n)))
               (x-flush s))
     (go py0 pyn)))


 ; --- keyboard: the full keycode -> (sym shifted-sym) table, once ---

(: (px-keysyms s mink maxk) (: nkc (+ 1 (- maxk mink))
     _ (x-w8 s 101) _ (x-w8 s 0) _ (x-w16 s 2)
     _ (x-w8 s mink) _ (x-w8 s nkc) _ (x-w16 s 0) _ (x-flush s)
     hdr (x-reply s) kpc (nthl hdr 1)
     km (tablet 256)
     _ (px-upto nkc (\ k (: s0 (x-r32 s)
                            s1 (? (< 1 kpc) (x-r32 s) 0)
                            _ (x-skip s (* 4 (? (< 2 kpc) (- kpc 2) 0)))
                            (pin km (+ mink k) (L s0 s1)))))
     km))


 ; a KeyPress -> the bytes a terminal sends. shift picks the second column;
 ; ctrl folds letters onto C0; the specials speak escape; latin-1 goes out
 ; as utf-8 (berth's encoder). anything else is silence.

(: (px-keybytes km kc st) (: p (peep km kc ())
     (? (! (two? p)) ""
      (: sym (? (&& (& 1 st) (< 0 (caup p))) (caup p) (cap p))
         (? (&& (<= 32 sym) (< sym 127))
            (? (& 4 st) ("" + (& 31 sym)) ("" + sym))
            (&& (<= 160 sym) (< sym 256)) (utf8 sym)
            (= sym 65293) "\r"       (= sym 65288) "\x7f"
            (= sym 65289) "\t"       (= sym 65307) "\e"
            (= sym 65361) "\e[D"     (= sym 65362) "\e[A"
            (= sym 65363) "\e[C"     (= sym 65364) "\e[B"
            (= sym 65360) "\e[H"     (= sym 65367) "\e[F"
            (= sym 65365) "\e[5~"    (= sym 65366) "\e[6~"
            (= sym 65535) "\e[3~"    (= sym 65379) "\e[2~"
            (= sym 65421) "\r"
            "")))))


 ; --- the painter, wet-driven: the screen names its dirty rows (the cb
 ; marks every write; wet reads-and-clears), the cursor's row and its
 ; last berth join them, blitrow renders each in one C call, and each
 ; contiguous run ships as its own PutImage band. a keystroke repaints one
 ; row of pixels, not a monitor's worth -- that is the whole speed story.
 ; geometry rides boxes so a resize swaps grid and framebuffer under us;
 ; a torn read paints one odd frame (bounds-checked) and the next is true.

(: (px-dirty sc all) (: r (gaze sc 1)
     (bits d base i acc) (? (< i 32)
                            (bits d base (+ i 1)
                                  (? (& 1 (>> d i)) (link (+ base i) acc) acc))
                            acc)
     (go k acc) (? (< k 8) (go (+ k 1) (bits (wet sc k) (* 32 k) 0 acc)) acc)
     ds (go 0 ())
     (? all (jot r) ds)))

(: (px-paint s win gc depth bx fx rig all lc) (: sc (bx -> () 0) fb (fx -> () 0)
     r (gaze sc 1) c (gaze sc 2) w (gaze sc 0) f (gaze sc 3)
     gw (caup rig) gh (caup (cup rig))
     wpix (* c gw)
     cur (? (& 1 f) w -1)
     seen (tablet 8)
     _ (map (\ x (? (< x r) (pin seen x 1) 0)) (px-dirty sc all))
     _ (? (< -1 cur) (: v (// cur c) (? (< v r) (pin seen v 1) 0)) 0)
     _ (? (&& (< -1 lc) (! (= lc cur)))
          (: v (// lc c) (? (< v r) (pin seen v 1) 0))
          0)
     ds (sort (keys seen))
     _ (? (two? ds)
          (: _ (map (\ row (blitrow fb wpix sc row cur (cap rig))) ds)
             ; ship each contiguous run of rows as one band
             (run a b rest) (? (two? rest)
                              (: x (cap rest)
                                 (? (= x (+ b 1)) (run a x (cup rest))
                                    (: _ (px-ship s win gc depth fb wpix
                                                  (* a gh) (* (+ (- b a) 1) gh))
                                       (run x x (cup rest)))))
                              (px-ship s win gc depth fb wpix
                                       (* a gh) (* (+ (- b a) 1) gh)))
             (run (cap ds) (cap ds) (cup ds)))
          0)
     cur))

(: (px-paintloop s win gc depth bx fx rig dirty lc) (: d (dirty -> () 0)
     lc2 (? d (: _ (pin dirty () 0)
                 (px-paint s win gc depth bx fx rig (= d 2) lc))
             lc)
     _ (rest 8)                       ; a keystroke's echo shows within ~10ms
     (px-paintloop s win gc depth bx fx rig dirty lc2)))


 ; --- resize: lux retiled us. a fresh grid and framebuffer to fit,
 ; and the pty learns its new size (SIGWINCH is the kernel's errand) ---

(: (px-resize bx fx m dirty rig wp hp) (: gw (caup rig) gh (caup (cup rig))
     r (: v (// hp gh) (? (< 1 v) v 1))
     c (: v (// wp gw) (? (< 1 v) v 2))
     sc (bx -> () 0)
     (? (&& (= r (gaze sc 1)) (= c (gaze sc 2))) ()
      (: _ (bx <- () (screen (cask (screen () r c)) r c))
         _ (fx <- () (cask (* (* c gw) (* (* r gh) 4))))
         _ (setwinsize m r c)
         _ (pin dirty () 1)
         ()))))


 ; --- the shore watch: X events in the main task; berth's moor pumps the
 ; child in another and SEALS the socket read when the child hangs up, so
 ; x-next wakes with () and the watch steps away ---

(: (px-watch s m km del bx fx rig dirty) (: e (x-next s)
     (? (! (two? e)) ()
      (: code (ev-code e)
         (? (= code 2)
            (: b (px-keybytes km (ev-detail e) (ev-state e))
               _ (? (tally b) (: _ (say m b) (flush m)) 0)
               (px-watch s m km del bx fx rig dirty))
            (= code 12)
            (: _ (pin dirty () 2) (px-watch s m km del bx fx rig dirty))   ; expose: repaint ALL
            (= code 22)
            (: raw (ev-raw e)
               _ (px-resize bx fx m dirty rig (u16 raw 20) (u16 raw 22))
               (px-watch s m km del bx fx rig dirty))
            (= code 33)
            (? (= del (u32 (ev-raw e) 12)) () (px-watch s m km del bx fx rig dirty))
            (px-watch s m km del bx fx rig dirty))))))


 ; --- pier: tie argv up at the shore. returns the child's status pair,
 ; or a nom naming what refused ---

(: (pier argv dpy auth ft) (: s (x-connect dpy)
     (? (! (lit? s)) 'no-socket
      (: sc (x-setup s auth)
         (? (! (two? sc)) (: _ (close s) 'refused)
          (: win (scr-idbase sc) gc (+ 1 (scr-idbase sc))
             depth (scr-depth sc)
             ft2 (? ft ft (font (cask (font () 0)) 0))   ; () sails with moderndos
             gw (peep ft2 0 8) gh (peep ft2 1 16)
             rig (L ft2 gw gh)
             r 24 c 80
             _ (px-mkwin s win (scr-root sc) 0 0 (* c gw) (* r gh))
             _ (px-gc s gc win)
             _ (px-title s win "berth")
             del (px-protocols s win)
             _ (x-map s win)
             _ (x-flush s)
             km (px-keysyms s (scr-mink sc) (scr-maxk sc))
             _ (setenv "TERM" "xterm-256color")   ; the dialect we actually speak
             md (tether argv)
             (? (! (two? md)) (: _ (close s) md)
              (: pid (cap md) m (cup md)
                 s0 (screen (cask (screen () r c)) r c)
                 bx #s0
                 f0 (cask (* (* c gw) (* (* r gh) 4)))
                 fx #f0
                 dirty #1
                 _ (setwinsize m r c)
                 up (twirl (\ _ (: _ (moor bx m dirty) (seal s 0))) 0)
                 pnt (twirl (\ _ (px-paintloop s win gc depth bx fx rig dirty -1)) 0)
                 _ (px-watch s m km del bx fx rig dirty)
                 _ (freeze pnt)
                 _ (? (landed? up) (catch up) (: _ (still pid 15) (freeze up)))
                 _ (close s)
                 _ (close m)
                 (waitfor pid)))))))))
