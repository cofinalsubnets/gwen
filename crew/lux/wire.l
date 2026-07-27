; crew/lux/wire.l -- the X11 wire protocol: connect, the setup handshake, request
; writers, the substructure redirect, keyboard mapping/grabs, and the event
; decoder. Host-only (needs the connectu nif, host/lux.c). Every op takes the
; socket port `s` EXPLICITLY -- so these leak as globals with no closure over a
; free `s` and no nif shadow (the codec's byte writers call the real `put`/`get`
; nifs, never a WM helper). A body-LESS `:` -- all of it leaks to the driver.
;
; The screen record `(x-setup s)` returns: (root wpx hpx minkc maxkc idbase).
; The event record `(x-next s)` returns: (code detail window state) -- enough for
; MapRequest/DestroyNotify/ConfigureRequest/MapNotify/KeyPress; () at EOF.
(:
   ; --- byte IO over the socket (little-endian) ---
   (x-w8 s v)  (put s v)
   (x-w16 s v) (: _ (put s (& 255 v)) (put s (& 255 (>> v 8))))
   (x-w32 s v) (: _ (x-w16 s (& 65535 v)) (x-w16 s (& 65535 (>> v 16))))
   (x-flush s) (flush s)
   (x-r8 s)  (see s)
   (x-r16 s) (: a (see s) b (see s) (| a (<< b 8)))
   (x-r32 s) (: a (x-r16 s) b (x-r16 s) (| a (<< b 16)))
   (x-skip s n) (: (go i) (? (< i n) (: _ (see s) (go (+ i 1))) 0) (go 0))
   (x-rstr s n) (: (go i acc) (? (< i n) (go (+ i 1) (+ acc (see s))) acc) (go 0 ""))
   (x-pad4 n) (% (- 4 (% n 4)) 4)
   (x-wname s t) (: (go i) (? (< i (tally t)) (: _ (put s (t i)) (go (+ i 1))) 0) (go 0))
   (x-rbytes s n) (: (go i acc) (? (< i n) (go (+ i 1) (link (see s) acc)) (rev acc)) (go 0 ()))   ; n raw bytes -> a list
   (u16 l off) (| (nthl l off) (<< (nthl l (+ off 1)) 8))                        ; little-endian reads out of a byte list
   (u32 l off) (| (u16 l off) (<< (u16 l (+ off 2)) 16))

   ; --- the reply/event demux. With SubstructureNotify selected, a request's
   ;     reply arrives INTERLEAVED with events on the one socket -- byte0 marks
   ;     which: 0 error, 1 reply, >=2 event. x-reply stashes events (in evq, a
   ;     FIFO) while it waits for a reply; x-next drains evq before the socket. ---
   evq (tablet 1)
   _ (pin evq 0 ())
   (x-reply s) (: pkt (x-rbytes s 32)                                            ; the next 32-byte REPLY header, events stashed; an ERROR
                  (? (|| (= (cap pkt) 1) (= (& 127 (cap pkt)) 0)) pkt            ; packet ALSO answers the wait (the window died mid-query --
                     (: _ (pin evq 0 (cat (peep evq 0 ()) (L pkt))) (x-reply s))))   ; blocking forever on it would hang the whole WM)

   ; --- connect + the setup handshake -> the screen record | (). auth is ()
   ;     (an -ac server) or (name data) -- the MIT-MAGIC-COOKIE-1 pair a real
   ;     server demands; xauth-cookie reads it out of ~/.Xauthority. ---
   (x-connect path) (connectu path)
   (x-wpad s t) (: _ (x-wname s t)                                               ; a string + its 4-byte padding
                   (: (go i) (? (< i (x-pad4 (tally t))) (: _ (put s 0) (go (+ i 1))) 0) (go 0)))
   (x-setup s auth)
    (: an (? auth (tally (cap auth)) 0) ad (? auth (tally (caup auth)) 0)
       _ (x-w8 s 108) _ (x-w8 s 0) _ (x-w16 s 11) _ (x-w16 s 0)
       _ (x-w16 s an) _ (x-w16 s ad) _ (x-w16 s 0)
       _ (? auth (: _ (x-wpad s (cap auth)) (x-wpad s (caup auth))) 0)
       _ (x-flush s)
       st (x-r8 s)
       (? (! (= st 1)) ()                                                       ; refused
          (: _ (x-r8 s) _ (x-r16 s) _ (x-r16 s)
             alen (x-r16 s)
             _ (x-r32 s) idbase (x-r32 s) _ (x-r32 s) _ (x-r32 s)
             vlen (x-r16 s) _ (x-r16 s)
             _ (x-r8 s) nfmt (x-r8 s)
             _ (x-skip s 4)
             minkc (x-r8 s) maxkc (x-r8 s)
             _ (x-skip s 4)
             _ (x-skip s (+ vlen (x-pad4 vlen)))
             _ (x-skip s (* 8 nfmt))
             root (x-r32 s) cmap (x-r32 s) _ (x-r32 s) _ (x-r32 s) _ (x-r32 s)
             wpx (x-r16 s) hpx (x-r16 s)
             _ (x-skip s 8)                                                      ; mm sizes, min/max installed maps
             vis (x-r32 s) _ (x-r8 s) _ (x-r8 s)                                 ; root visual; backing-stores, save-unders
             depth (x-r8 s) _ (x-r8 s)                                           ; root depth (a drawing client's PutImage needs it)
             _ (x-skip s (- (* 4 alen) (+ 32 (+ (+ vlen (x-pad4 vlen)) (+ (* 8 nfmt) 40)))))
             (L root wpx hpx minkc maxkc idbase cmap vis depth))))
   (scr-root sc) (cap sc)  (scr-w sc) (caup sc)  (scr-h sc) (caup (cup sc))
   (scr-mink sc) (nthl sc 3)  (scr-maxk sc) (nthl sc 4)  (scr-idbase sc) (nthl sc 5)
   (scr-cmap sc) (nthl sc 6)  (scr-visual sc) (nthl sc 7)  (scr-depth sc) (nthl sc 8)

   ; --- requests ---
   (x-cwa-events s win mask)                                                     ; ChangeWindowAttributes: CWEventMask
    (: _ (x-w8 s 2) _ (x-w8 s 0) _ (x-w16 s 4) _ (x-w32 s win) _ (x-w32 s 2048) (x-w32 s mask))
   (x-configure s win x y w h)                                                   ; ConfigureWindow: x y w h (border-width is x-border's alone)
    (: _ (x-w8 s 12) _ (x-w8 s 0) _ (x-w16 s 7) _ (x-w32 s win) _ (x-w16 s 15) _ (x-w16 s 0)
       _ (x-w32 s x) _ (x-w32 s y) _ (x-w32 s w) (x-w32 s h))
   (x-map s win)   (: _ (x-w8 s 8) _ (x-w8 s 0) _ (x-w16 s 2) (x-w32 s win))
   (x-unmap s win) (: _ (x-w8 s 10) _ (x-w8 s 0) _ (x-w16 s 2) (x-w32 s win))
   (x-focus s win) (: _ (x-w8 s 42) _ (x-w8 s 1) _ (x-w16 s 3) _ (x-w32 s win) (x-w32 s 0))   ; SetInputFocus, PointerRoot revert
   (x-border s win px) (: _ (x-w8 s 12) _ (x-w8 s 0) _ (x-w16 s 4) _ (x-w32 s win) _ (x-w16 s 16) _ (x-w16 s 0) (x-w32 s px))   ; ConfigureWindow border-width

   ; --- InternAtom (name -> atom), used as the redirect sync point ---
   (x-intern s nm)
    (: n (tally nm)
       _ (x-w8 s 16) _ (x-w8 s 0) _ (x-w16 s (+ 2 (/ (+ n (x-pad4 n)) 4)))
       _ (x-w16 s n) _ (x-w16 s 0) _ (x-wname s nm)
       _ (: (go i) (? (< i (x-pad4 n)) (: _ (put s 0) (go (+ i 1))) 0) (go 0))
       _ (x-flush s)
       hdr (x-reply s) (? (= (cap hdr) 1) (u32 hdr 8) ()))                      ; the atom; () when the round-trip answered with an error

   ; --- GetProperty (opcode 20) -> (format nitems bytes); decoders on top ---
   (x-getprop s win prop)
    (: _ (x-w8 s 20) _ (x-w8 s 0) _ (x-w16 s 6) _ (x-w32 s win) _ (x-w32 s prop)
       _ (x-w32 s 0) _ (x-w32 s 0) _ (x-w32 s 1024) _ (x-flush s)
       hdr (x-reply s)
       (? (! (= (cap hdr) 1)) (L 0 0 ())                                        ; the window died: no property
          (: rlen (u32 hdr 4)
             bytes (x-rbytes s (* rlen 4))
             (L (nthl hdr 1) (u32 hdr 12) bytes))))
   (x-atoms-of bytes) (? (< (tally bytes) 4) ()                                  ; group 4 LE bytes -> a 32-bit atom/window list
                         (link (u32 bytes 0) (x-atoms-of (drop 4 bytes))))
   (x-getprop-atoms s win prop) (x-atoms-of (nthl (x-getprop s win prop) 2))
   (x-has-atom s win propnm valnm) (: at (x-getprop-atoms s win (x-intern s propnm)) va (x-intern s valnm)   ; isInProperty
                                      (any (\ a (= a va)) at))
   (x-getclass s win)                                                            ; WM_CLASS (atom 67) = "instance\0class\0"; the class is the 2nd
    (: bytes (nthl (x-getprop s win 67) 2)
       (: (go l seen acc) (? (! (two? l)) acc                                    ; walk past the first NUL, then collect to the next
                             (= (cap l) 0) (? seen acc (go (cup l) 1 acc))
                             (go (cup l) seen (? seen (+ acc (cap l)) acc)))
          (go bytes 0 "")))
   (x-transient s win) (: a (x-getprop-atoms s win 68) (? (two? a) (cap a) ()))  ; WM_TRANSIENT_FOR (atom 68) -> the parent window | ()

   ; --- GetGeometry (opcode 14) -> the window's (x y w h) ---
   (x-getgeom s win)
    (: _ (x-w8 s 14) _ (x-w8 s 0) _ (x-w16 s 2) _ (x-w32 s win) _ (x-flush s)
       hdr (x-reply s)
       (? (! (= (cap hdr) 1)) (L 0 0 1 1)                                       ; the window died: a harmless placeholder rect
          (L (u16 hdr 12) (u16 hdr 14) (u16 hdr 16) (u16 hdr 18))))
   (x-raise s win) (: _ (x-w8 s 12) _ (x-w8 s 0) _ (x-w16 s 4) _ (x-w32 s win)   ; ConfigureWindow CWStackMode=Above
                      _ (x-w16 s 64) _ (x-w16 s 0) (x-w32 s 0))
   (x-lower s win) (: _ (x-w8 s 12) _ (x-w8 s 0) _ (x-w16 s 4) _ (x-w32 s win)   ; ConfigureWindow CWStackMode=Below (a desktop stays under)
                      _ (x-w16 s 64) _ (x-w16 s 0) (x-w32 s 1))
   (x-kill s win) (: _ (x-w8 s 113) _ (x-w8 s 0) _ (x-w16 s 2) (x-w32 s win))    ; KillClient: the LAST resort -- it severs the whole
                                                                                 ; connection, every window that client owns dies with it
   (x-wmdel s win)                                                               ; ICCCM close: SendEvent a WM_DELETE_WINDOW ClientMessage --
    (: pr (x-intern s "WM_PROTOCOLS") dl (x-intern s "WM_DELETE_WINDOW")         ; the client shuts ONE window, gracefully
       _ (x-w8 s 25) _ (x-w8 s 0) _ (x-w16 s 11) _ (x-w32 s win) _ (x-w32 s 0)
       _ (x-w8 s 33) _ (x-w8 s 32) _ (x-w16 s 0) _ (x-w32 s win) _ (x-w32 s pr)
       _ (x-w32 s dl) _ (x-w32 s 0) _ (x-w32 s 0) _ (x-w32 s 0) (x-w32 s 0))
   (x-confnotify s win)                                                          ; ICCCM 4.1.5: a ConfigureRequest the layout overrules still
    (: _ (x-w8 s 14) _ (x-w8 s 0) _ (x-w16 s 2) _ (x-w32 s win) _ (x-flush s)    ; gets an ANSWER -- a synthetic ConfigureNotify carrying the
       hdr (x-reply s)                                                           ; window's REAL geometry (VTE sizes its character grid off
       (? (! (= (cap hdr) 1)) 0                                                  ; what it believes the window is; silence = a garbled grid)
          (: gx (u16 hdr 12) gy (u16 hdr 14) gw (u16 hdr 16) gh (u16 hdr 18) bw (u16 hdr 20)
             _ (x-w8 s 25) _ (x-w8 s 0) _ (x-w16 s 11) _ (x-w32 s win) _ (x-w32 s 131072)   ; SendEvent, mask StructureNotify
             _ (x-w8 s 22) _ (x-w8 s 0) _ (x-w16 s 0) _ (x-w32 s win) _ (x-w32 s win)       ; ConfigureNotify: event, window,
             _ (x-w32 s 0) _ (x-w16 s gx) _ (x-w16 s gy) _ (x-w16 s gw) _ (x-w16 s gh)      ; above-sibling None, the geometry,
             _ (x-w16 s bw) _ (x-w16 s 0) (x-w32 s 0))))                                    ; border, override-redirect 0, pad
   (x-delprop s win prop) (: _ (x-w8 s 19) _ (x-w8 s 0) _ (x-w16 s 3) _ (x-w32 s win) (x-w32 s prop))   ; DeleteProperty
   (x-grabbtn s win)                                                             ; GrabButton 1/2/3, AnyModifier, pointer SYNC: a click on a
    (: (one b) (: _ (x-w8 s 28) _ (x-w8 s 0) _ (x-w16 s 6) _ (x-w32 s win)       ; managed window FREEZES until x-replay hands it back to the
                  _ (x-w16 s 4) _ (x-w8 s 0) _ (x-w8 s 1) _ (x-w32 s 0)          ; app unstolen (click-to-focus). buttons 4/5 (the wheel) stay
                  _ (x-w32 s 0) _ (x-w8 s b) _ (x-w8 s 0) (x-w16 s 32768))       ; UNGRABBED -- scroll never rides through the WM
       _ (one 1) _ (one 2) (one 3))
   (x-replay s) (: _ (x-w8 s 35) _ (x-w8 s 2) _ (x-w16 s 2) (x-w32 s 0))         ; AllowEvents ReplayPointer, CurrentTime
   (x-allow-async s) (: _ (x-w8 s 35) _ (x-w8 s 1) _ (x-w16 s 2) (x-w32 s 0))    ; AllowEvents AsyncPointer: thaw a sync grab WITHOUT replaying
   (x-grabptr s win)                                                             ; GrabPointer, async, ButtonRelease(8)|PointerMotion(64) -- the drag catch
    (: _ (x-w8 s 26) _ (x-w8 s 0) _ (x-w16 s 6) _ (x-w32 s win)
       _ (x-w16 s 72) _ (x-w8 s 1) _ (x-w8 s 1) _ (x-w32 s 0) _ (x-w32 s 0) _ (x-w32 s 0) _ (x-flush s)
       (x-reply s))                                                              ; a reply (the grab status) -- read it to keep the demux synced
   (x-ungrabptr s) (: _ (x-w8 s 27) _ (x-w8 s 0) _ (x-w16 s 2) (x-w32 s 0))      ; UngrabPointer, CurrentTime

   ; --- QueryTree (opcode 15) + GetWindowAttributes (opcode 3): at startup we
   ;     ADOPT the windows that mapped before the redirect took hold (closing the
   ;     race) and any survivors of a restart-in-place. ---
   (x-querytree s win)
    (: _ (x-w8 s 15) _ (x-w8 s 0) _ (x-w16 s 2) _ (x-w32 s win) _ (x-flush s)
       hdr (x-reply s)
       (? (! (= (cap hdr) 1)) ()
          (: n (u16 hdr 16) bytes (x-rbytes s (* (u32 hdr 4) 4))
             (take n (x-atoms-of bytes)))))
   (x-attrs s win)                                                               ; -> (map-state override-redirect); map-state 2 = IsViewable
    (: _ (x-w8 s 3) _ (x-w8 s 0) _ (x-w16 s 2) _ (x-w32 s win) _ (x-flush s)
       hdr (x-reply s)
       (? (! (= (cap hdr) 1)) (L 0 1)                                           ; the window died: reads as unmanageable
          (: _ (x-rbytes s (* (u32 hdr 4) 4))                                   ; drain the trailing reply data
             (L (nthl hdr 26) (nthl hdr 27)))))
   (x-manageable? s win) (: a (x-attrs s win) (&& (= (cap a) 2) (= (caup a) 0))) ; viewable and not override-redirect

   ; --- become the WM: SubstructureRedirect|Notify on the root, synced by an
   ;     intern round-trip. 1 = we hold it, 0 = another WM -- the Access error
   ;     arrives BEFORE the intern reply, so x-reply answers the intern wait
   ;     with the ERROR packet: refusal reads as the intern's () ---
   (x-redirect s root)
    (: _ (x-cwa-events s root 1572864)
       (? (x-intern s "WM_STATE") 1 0))

   ; --- keyboard: GetKeyboardMapping -> a keysym->keycode map; GrabKey ---
   (x-keymap s mink maxk syms)                                                   ; syms = the keysyms we want located
    (: nkc (+ 1 (- maxk mink))
       _ (x-w8 s 101) _ (x-w8 s 0) _ (x-w16 s 2) _ (x-w8 s mink) _ (x-w8 s nkc) _ (x-w16 s 0) _ (x-flush s)
       hdr (x-reply s) kpc (nthl hdr 1)                                          ; via the demux -- an event mid-wait must not desync us
       km (tablet 8)
       _ (: (go i) (? (< i (* nkc kpc))
                      (: sym (x-r32 s)
                         _ (? (&& (any (\ x (= x sym)) syms) (! (peep km sym 0)))
                              (pin km sym (+ mink (// i kpc))) 0)
                         (go (+ i 1))) 0) (go 0))
       km)
   (x-grab s root mods kc) (? (< kc 1) 0
                              (: _ (x-w8 s 33) _ (x-w8 s 1) _ (x-w16 s 4) _ (x-w32 s root)
                                 _ (x-w16 s mods) _ (x-w8 s kc) _ (x-w8 s 1) _ (x-w8 s 1)
                                 _ (x-w8 s 0) _ (x-w8 s 0) (x-w8 s 0)))
   (x-ungrabkeys s root) (: _ (x-w8 s 34) _ (x-w8 s 0) _ (x-w16 s 3)             ; UngrabKey AnyKey/AnyModifier: a regrab starts clean
                            _ (x-w32 s root) _ (x-w16 s 32768) (x-w16 s 0))

   ; --- AllocNamedColor (85): a color NAME -> a pixel (the config's border colors) ---
   (x-color s cmap nm)
    (: n (tally nm)
       _ (x-w8 s 85) _ (x-w8 s 0) _ (x-w16 s (+ 3 (/ (+ n (x-pad4 n)) 4)))
       _ (x-w32 s cmap) _ (x-w16 s n) _ (x-w16 s 0) _ (x-wpad s nm) _ (x-flush s)
       hdr (x-reply s) (u32 hdr 8))
   (x-borderpix s win px)                                                        ; ChangeWindowAttributes CWBorderPixel
    (: _ (x-w8 s 2) _ (x-w8 s 0) _ (x-w16 s 4) _ (x-w32 s win) _ (x-w32 s 8) (x-w32 s px))

   ; --- the root cursor (setDefaultCursor xC_left_ptr): OpenFont "cursor" (45),
   ;     CreateGlyphCursor (94, glyph 68/69, black on white), CWA CWCursor ---
   (x-cursor s root fid cid)
    (: _ (x-w8 s 45) _ (x-w8 s 0) _ (x-w16 s 5) _ (x-w32 s fid)
       _ (x-w16 s 6) _ (x-w16 s 0) _ (x-wpad s "cursor")
       _ (x-w8 s 94) _ (x-w8 s 0) _ (x-w16 s 8) _ (x-w32 s cid) _ (x-w32 s fid) _ (x-w32 s fid)
       _ (x-w16 s 68) _ (x-w16 s 69)
       _ (x-w16 s 0) _ (x-w16 s 0) _ (x-w16 s 0)
       _ (x-w16 s 65535) _ (x-w16 s 65535) _ (x-w16 s 65535)
       _ (x-w8 s 46) _ (x-w8 s 0) _ (x-w16 s 2) _ (x-w32 s fid)                  ; CloseFont: the cursor keeps its glyphs
       _ (x-w8 s 2) _ (x-w8 s 0) _ (x-w16 s 4) _ (x-w32 s root) _ (x-w32 s 16384) (x-w32 s cid))

   ; --- ~/.Xauthority: big-endian length-prefixed records; find the display's
   ;     MIT-MAGIC-COOKIE-1 -> (name data), or (). The parser is PURE (bytes in
   ;     a string); xauth-cookie is the host loader over open/slurp. ---
   (xauth-parse b disp)
    (: n (tally b)
       (fld i) (: ln (| (<< (b i) 8) (b (+ i 1))) e (+ (+ i 2) ln) (L (snip b (+ i 2) e) e))
       (go i) (? (< (- n i) 10) ()                                               ; too short for another record
                 (: ad (fld (+ i 2)) nu (fld (caup ad)) nm (fld (caup nu)) da (fld (caup nm))
                    (? (&& (= (cap nu) disp) (= (cap nm) "MIT-MAGIC-COOKIE-1")) (L (cap nm) (cap da))
                       (go (caup da)))))
       (go 0))
   (xauth-cookie path disp)
    (: p (open path "r")
       (? p (: b (slurp p) _ (close p) (xauth-parse b disp)) ()))

   ; --- the event decoder: one 32-byte event -> (code detail window state) | ()
   ;     code 0 = X error (window is garbage). window at byte 8, state at byte 28,
   ;     detail (keycode) at byte 1 -- enough for every event the WM handles. ---
   (x-decode pkt) (L (& 127 (cap pkt)) (nthl pkt 1) (u32 pkt 8) (u16 pkt 28) pkt)   ; code, detail(keycode), window@8, state@28, + the raw bytes
   (x-next s)                                                                    ; a stashed event first, else the socket
    (: q (peep evq 0 ())
       (? (two? q) (: _ (pin evq 0 (cup q)) (x-decode (cap q)))
          (: b0 (see s)
             (? (= b0 -1) ()
                (: pkt (link b0 (x-rbytes s 31))
                   (? (= b0 1) (: _ (x-skip s (* 4 (u32 pkt 4))) (x-next s))     ; an orphaned reply (its wait ended on an error): drain + skip
                      (x-decode pkt)))))))
   (ev-code e) (cap e)  (ev-detail e) (caup e)  (ev-window e) (nthl e 2)  (ev-state e) (nthl e 3)
   (ev-raw e) (nthl e 4))                                                        ; the 32 raw bytes (ConfigureRequest reads its geometry here)
