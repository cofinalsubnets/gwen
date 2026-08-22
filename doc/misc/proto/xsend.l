; xsend -- the fake typist for the wm spike: XTEST FakeInput key chords, sent
; as REAL device-level events so they travel the server's grab machinery into
; whoever holds the GrabKey (the wm), exactly like fingers would. Speaks the
; same wire codec as doc/misc/proto/{x11,wm}.l over the unix display socket.
; Types mod1+j (focus down) then mod1+Return (swap master), then leaves.
;   run: out/host/love doc/misc/proto/xsend.l    (against `Xephyr :7 -ac`, wm running)
(: s (connectu "/tmp/.X11-unix/X7")
   _ (? s 0 (: _ (say err "xsend: no display socket -- Xephyr up?\n") (quit 1)))
   (line m) (: _ (say out m) _ (say out "\n") (flush out))
   (w8 v)   (put s v)
   (w16 v)  (: _ (put s (& 255 v)) (put s (& 255 (>> v 8))))
   (w32 v)  (: _ (w16 (& 65535 v)) (w16 (& 65535 (>> v 16))))
   (r8 u)   (see s)
   (r16 u)  (: a (see s) b (see s) (| a (<< b 8)))
   (r32 u)  (: a (r16 ()) b (r16 ()) (| a (<< b 16)))
   (skip n) (: (go i) (? (< i n) (: _ (see s) (go (+ i 1))) 0) (go 0))
   (pad4 n) (% (- 4 (% n 4)) 4)

   ; handshake + setup: only min/max keycodes matter, drain the rest
   _ (w8 108) _ (w8 0) _ (w16 11) _ (w16 0) _ (w16 0) _ (w16 0) _ (w16 0)
   _ (flush s)
   st (r8 ())
   _ (? (= st 1) 0 (: _ (say err "xsend: setup refused\n") (quit 1)))
   _ (r8 ()) _ (r16 ()) _ (r16 ())
   alen (r16 ())
   _ (r32 ()) _ (r32 ()) _ (r32 ()) _ (r32 ())
   vlen (r16 ()) _ (r16 ())
   _ (r8 ()) nfmt (r8 ())
   _ (skip 4)
   minkc (r8 ()) maxkc (r8 ())
   _ (skip 4)
   _ (skip (- (* 4 alen) 32))                       ; vendor + formats + screens, unread wholesale

   ; the keycodes: Alt_L (0xFFE9), j, Return
   nkc (+ 1 (- maxkc minkc))
   _ (w8 101) _ (w8 0) _ (w16 2) _ (w8 minkc) _ (w8 nkc) _ (w16 0)
   _ (flush s)
   _ (r8 ()) kpc (r8 ()) _ (r16 ()) _ (r32 ()) _ (skip 24)
   km (tablet 8)
   _ (: (go i) (? (< i (* nkc kpc))
                  (: sym (r32 ())
                     _ (? (&& (|| (= sym 65513) (|| (= sym 106) (= sym 65293)))
                             (! (peep km sym 0)))
                          (pin km sym (+ minkc (// i kpc))) 0)
                     (go (+ i 1)))
                  0)
        (go 0))
   kalt (peep km 65513 0) kj (peep km 106 0) kret (peep km 65293 0)
   _ (? (&& kalt (&& kj kret)) 0 (: _ (say err "xsend: keycodes missing\n") (quit 1)))

   ; QueryExtension "XTEST" -> the extension's major opcode
   _ (w8 98) _ (w8 0) _ (w16 4) _ (w16 5) _ (w16 0)
   _ (: (go i) (? (< i 5) (: _ (put s ("XTEST" i)) (go (+ i 1))) 0) (go 0))
   _ (w8 0) _ (w8 0) _ (w8 0)
   _ (flush s)
   _ (r8 ()) _ (r8 ()) _ (r16 ()) _ (r32 ())
   here (r8 ()) xop (r8 ()) _ (skip 22)
   _ (? here 0 (: _ (say err "xsend: no XTEST on this server\n") (quit 1)))

   ; FakeInput (minor 2): type 2 = KeyPress, 3 = KeyRelease, at the core device
   (fake t kc) (: _ (w8 xop) _ (w8 2) _ (w16 9)
                  _ (w8 t) _ (w8 kc) _ (w16 0)
                  _ (w32 0) _ (w32 0)               ; time CurrentTime, root None
                  _ (w32 0) _ (w32 0)
                  _ (w16 0) _ (w16 0)               ; rootX rootY
                  _ (w32 0) _ (w16 0) _ (w8 0)
                  (w8 0))
   (chord kc) (: _ (fake 2 kalt) _ (fake 2 kc) _ (fake 3 kc) _ (fake 3 kalt) (flush s))
   _ (line (+ "xsend: typing mod1+j, mod1+Return (alt=" (+ (show kalt) ")")))
   _ (chord kj) _ (rest 500)
   _ (chord kret) _ (rest 500)
   _ (close s)
   (line "xsend: done"))
