; x11 -- an X11 wire-protocol codec spike (option A of the xmonad-in-ai port).
; no Xlib, no XCB: raw little-endian bytes over TCP to a nested server started
; as `Xephyr :7 -ac -listen tcp` (display :7 = port 6007; -ac drops the auth
; cookie so the setup carries an empty auth block). proves the round trip a
; window manager needs: setup handshake parsed, InternAtom request/reply,
; CreateWindow + MapWindow encoded, the Expose event decoded coming back.
;   run: out/host/love doc/proto/x11.l
; the wire layout is X Window System Protocol ch. 8-9 (setup), the per-request
; encodings from the protocol appendix. everything is one body-having `:` --
; local names, ain-style (tools/ain.l is the socket idiom source).
(: s (connect "127.0.0.1" 6007)
   _ (? s 0 (: _ (say err "x11: cannot connect to :7 -- Xephyr up?\n") (quit 1)))
   (line m) (: _ (say out m) _ (say out "\n") (flush out))

   ; --- little-endian writers / readers over the socket port ---
   (w8 v)   (put s v)
   (w16 v)  (: _ (put s (& 255 v)) (put s (& 255 (>> v 8))))
   (w32 v)  (: _ (w16 (& 65535 v)) (w16 (& 65535 (>> v 16))))
   (wstr t) (: (go i) (? (< i (tally t)) (: _ (put s (t i)) (go (+ i 1))) 0) (go 0))
   (wzero k) (: (go i) (? (< i k) (: _ (put s 0) (go (+ i 1))) 0) (go 0))
   (r8 u)   (see s)
   (r16 u)  (: a (see s) b (see s) (| a (<< b 8)))
   (r32 u)  (: a (r16 ()) b (r16 ()) (| a (<< b 16)))
   (skip n) (: (go i) (? (< i n) (: _ (see s) (go (+ i 1))) 0) (go 0))
   (rstr n) (: (go i acc) (? (< i n) (go (+ i 1) (+ acc (see s))) acc) (go 0 ""))
   (pad4 n) (% (- 4 (% n 4)) 4)          ; X rounds strings up to 4 bytes

   ; --- the setup handshake: 12 bytes, 'l' = little-endian, empty auth ---
   _ (w8 108) _ (w8 0)                   ; byte order 'l', pad
   _ (w16 11) _ (w16 0)                  ; protocol 11.0
   _ (w16 0) _ (w16 0) _ (w16 0)         ; auth name len, auth data len, pad
   _ (flush s)
   st (r8 ())
   _ (? (= st 1) 0
        (: rlen (r8 ()) _ (skip 6) why (rstr rlen)
           _ (say err (+ "x11: setup refused: " (+ why "\n"))) (quit 1)))
   _ (r8 ())                             ; pad
   maj (r16 ()) mnr (r16 ())
   alen (r16 ())                         ; additional data, 4-byte units
   _ (r32 ())                            ; release number
   idbase (r32 ()) idmask (r32 ())
   _ (r32 ())                            ; motion buffer size
   vlen (r16 ()) _ (r16 ())              ; vendor len, max request len
   nscr (r8 ()) nfmt (r8 ())
   _ (skip 10)                           ; byte/bit orders, scanline, keycodes, pad
   vendor (rstr vlen) _ (skip (pad4 vlen))
   _ (skip (* 8 nfmt))                   ; pixmap formats
   root (r32 ()) _ (r32 ())              ; screen 0: root window, default colormap
   white (r32 ()) _ (r32 ())             ; white pixel, black pixel
   _ (r32 ())                            ; current input masks
   wpx (r16 ()) hpx (r16 ())
   ; drain the rest of the setup blob (mm sizes, visuals/depths, screens 1..):
   ; total is 4*alen past the 8-byte header; consumed = 32 fixed + vendor+pad
   ; + 8*nfmt + the 24 screen-0 bytes read above
   _ (skip (- (* 4 alen) (+ 32 (+ (+ vlen (pad4 vlen)) (+ (* 8 nfmt) 24)))))
   _ (line (+ "setup ok: X protocol " (+ (show maj) (+ "." (show mnr)))))
   _ (line (+ "vendor: " vendor))
   _ (line (+ "screens: " (+ (show nscr) (+ "  screen0 " (+ (show wpx) (+ "x" (show hpx)))))))
   _ (line (+ "root window: " (show root)))
   _ (line (+ "id base: " (+ (show idbase) (+ " mask: " (show idmask)))))

   ; --- InternAtom round-trip: opcode 16, the simplest request-with-reply ---
   nm "WM_PROTOCOLS"
   n (tally nm)
   _ (w8 16) _ (w8 0)                    ; opcode, only-if-exists = 0
   _ (w16 (+ 2 (/ (+ n (pad4 n)) 4)))    ; request length in 4-byte units
   _ (w16 n) _ (w16 0)                   ; name length, pad
   _ (wstr nm) _ (wzero (pad4 n))
   _ (flush s)
   b0 (r8 ())
   _ (? (= b0 1) 0
        (: code (r8 ()) _ (skip 30)
           _ (say err (+ "x11: InternAtom X error, code " (+ (show code) "\n"))) (quit 1)))
   _ (r8 ()) _ (r16 ()) _ (r32 ())       ; pad, sequence, reply length
   atom (r32 ()) _ (skip 20)
   _ (line (+ "InternAtom WM_PROTOCOLS -> atom " (show atom)))

   ; --- CreateWindow (opcode 1) + MapWindow (opcode 8) ---
   wid (| idbase 1)
   _ (w8 1) _ (w8 0) _ (w16 10)          ; opcode, depth CopyFromParent, 8 fixed + 2 values
   _ (w32 wid) _ (w32 root)
   _ (w16 20) _ (w16 20) _ (w16 300) _ (w16 200)   ; x y w h
   _ (w16 2) _ (w16 1)                   ; border width, class InputOutput
   _ (w32 0)                             ; visual CopyFromParent
   _ (w32 2050)                          ; value mask CWBackPixel|CWEventMask (0x0802)
   _ (w32 white) _ (w32 32768)           ; background = white, events = Exposure
   _ (w8 8) _ (w8 0) _ (w16 2) _ (w32 wid)
   _ (flush s)
   _ (line (+ "created + mapped window " (show wid)))

   ; --- the event loop: 32-byte messages; wait for our Expose ---
   (msg u) (: b0 (r8 ())
              (? (= b0 0)
                 (: code (r8 ()) _ (skip 30)
                    _ (say err (+ "x11: X error, code " (+ (show code) "\n"))) (quit 1))
                 (: code (& 127 b0)
                    (? (= code 12)
                       (: _ (r8 ()) _ (r16 ())        ; pad, sequence
                          w (r32 ())
                          ex (r16 ()) ey (r16 ()) ew (r16 ()) eh (r16 ())
                          _ (skip 16)                  ; count + pad
                          (line (+ "Expose on " (+ (show w) (+ ": "
                                (+ (show ew) (+ "x" (+ (show eh) (+ " at "
                                (+ (show ex) (+ "," (show ey))))))))))))
                       (: _ (skip 31)
                          _ (line (+ "event code " (+ (show code) " (skipped)")))
                          (msg ()))))))
   _ (msg ())
   _ (line "holding the window 8s -- probe it: xwininfo -display :7 -tree -root")
   _ (rest 8000)
   _ (close s)
   (line "x11 spike done"))
