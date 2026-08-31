; crew/lux/wire.l -- the x11 wire protocol: connect, the setup handshake, request
; writers, the substructure redirect, keyboard mapping/grabs, and the event
; decoder. Host-only (needs the connectu nif, src/sock.c). Every op takes the
; socket port `s` explicitly -- so these leak as globals with no closure over a
; free `s` and no nif shadow (the codec's byte writers call the real `put`/`get`
; nifs, never a wm helper). a body-less `:` -- all of it leaks to the driver.
;
; The screen record `(x-setup s)` returns: (root wpx hpx minkc maxkc idbase).
; The event record `(x-next s)` returns: (code detail window state) -- enough for
; MapRequest/DestroyNotify/ConfigureRequest/MapNotify/KeyPress; () at EOF.
(:
   ; --- byte IO over the socket (little-endian) ---
   (x-w8 s v)  (put s v)
   (x-w16 s v) (put s (255 & v), put s (255 & (v >> 8)))
   (x-w32 s v) (x-w16 s (65535 & v), x-w16 s (65535 & (v >> 16)))
   x-flush flush
   x-r8 see
   (x-r16 s) (: a (see s) b (see s) (a | (b << 8)))
   (x-r32 s) (: a (x-r16 s) b (x-r16 s) (a | (b << 16)))
   (x-skip s n) (: (go i) (i < n ? (see s, go (i + 1))) (go 0))
   (x-rstr s n) (: (go i acc) (i >= n ? acc (go (i + 1) (acc + string (see s)))) (go 0 ""))
   (x-pad4 n) ((4 - (n % 4)) % 4)
   (x-wname s t) (: (go i) (i < #t ? (put s (t i), go (i + 1))) (go 0))
   (x-rbytes s n) (: (go i acc) (i >= n ? (rev acc) (go (i + 1) (see s >< acc))) (go 0 ()))       ; n raw bytes -> a list
   (u16 l off) (l off | (l (off + 1) << 8))                            ; little-endian reads out of a byte list
   (u32 l off) (u16 l off | (u16 l (off + 2) << 16))

   ; --- the reply/event demux. With SubstructureNotify selected, a request's
   ;     reply arrives interleaved with events on the one socket -- byte0 marks
   ;     which: 0 error, 1 reply, >=2 event. x-reply stashes events (in evq, a
   ;     fifo) while it waits for a reply; x-next drains evq before the socket. ---
   evq {0 ()}
   (x-reply s) (: pkt (x-rbytes s 32)                                            ; the next 32-byte reply header, events stashed; an error
                (<pkt = 1 || (127 & <pkt) = 0 ? pkt                              ; packet also answers the wait (the window died mid-query --
                 (pin evq 0 (evq 0 + [pkt]), x-reply s)))                        ; blocking forever on it would hang the whole wm)

   ; --- connect + the setup handshake -> the screen record | (). auth is ()
   ;     (an -ac server) or (name data) -- the mit-magic-cookie-1 pair a real
   ;     server demands; xauth-cookie reads it out of ~/.Xauthority. ---
   (x-connect path) (connectu path)
   (x-wpad s t)                                                                  ; a string + its 4-byte padding
    (x-wname s t, (: (go i) (i < x-pad4 #t ? (put s 0, go (i + 1))) (go 0)))
   (x-setup s auth)
    (: an #<auth ad #<>auth
       _ (x-w8 s 108, x-w8 s 0, x-w16 s 11, x-w16 s 0,
          x-w16 s an, x-w16 s ad, x-w16 s 0,
          auth @ (a b) (x-wpad s a, x-wpad s b),
          x-flush s)
       st (x-r8 s)
       (? !(st = 1) ()                                                           ; refused
          (: _ (x-r8 s) _ (x-r16 s) _ (x-r16 s)
             alen (x-r16 s)
             _ (x-r32 s) idbase (x-r32 s) _ (x-r32 s) _ (x-r32 s)
             vlen (x-r16 s) _ (x-r16 s)
             _ (x-r8 s) nfmt (x-r8 s)
             _ (x-skip s 4)
             minkc (x-r8 s) maxkc (x-r8 s)
             _ (x-skip s 4)
             _ (x-skip s (vlen + x-pad4 vlen))
             _ (x-skip s (8 * nfmt))
             root (x-r32 s) cmap (x-r32 s) _ (x-r32 s) _ (x-r32 s) _ (x-r32 s)
             wpx (x-r16 s) hpx (x-r16 s)
             _ (x-skip s 8)                                                      ; mm sizes, min/max installed maps
             vis (x-r32 s) _ (x-r8 s) _ (x-r8 s)                                 ; root visual; backing-stores, save-unders
             depth (x-r8 s) _ (x-r8 s)                                           ; root depth (a drawing client's PutImage needs it)
             _ (x-skip s ((4 * alen) - (32 + ((vlen + x-pad4 vlen) + ((8 * nfmt) + 40)))))
             [root wpx hpx minkc maxkc idbase cmap vis depth])))
   (scr-root sc) <sc  (scr-w sc) <>sc  (scr-h sc) <>>sc
   (scr-mink sc) (sc 3)  (scr-maxk sc) (sc 4)  (scr-idbase sc) (sc 5)
   (scr-cmap sc) (sc 6)  (scr-visual sc) (sc 7)  (scr-depth sc) (sc 8)

   ; --- requests ---
   (x-cwa-events s win mask)                                                     ; ChangeWindowAttributes: cweventMask
    (x-w8 s 2, x-w8 s 0, x-w16 s 4, x-w32 s win, x-w32 s 2048, x-w32 s mask)
   (x-configure s win x y w h)                                                   ; ConfigureWindow: x y w h (border-width is x-border's alone)
    (x-w8 s 12, x-w8 s 0, x-w16 s 7, x-w32 s win, x-w16 s 15, x-w16 s 0,
     x-w32 s x, x-w32 s y, x-w32 s w, x-w32 s h)
   (x-map s win)   (x-w8 s 8, x-w8 s 0, x-w16 s 2, x-w32 s win)
   (x-unmap s win) (x-w8 s 10, x-w8 s 0, x-w16 s 2, x-w32 s win)
   (x-focus s win) (x-w8 s 42, x-w8 s 1, x-w16 s 3, x-w32 s win, x-w32 s 0)      ; SetInputFocus, PointerRoot revert
   (x-border s win px)                                                           ; ConfigureWindow border-width
    (x-w8 s 12, x-w8 s 0, x-w16 s 4, x-w32 s win, x-w16 s 16, x-w16 s 0, x-w32 s px)

   ; --- InternAtom (name -> atom), used as the redirect sync point ---
   (x-intern s nm)
    (: n #nm
       _ (x-w8 s 16, x-w8 s 0, x-w16 s (2 + (n + x-pad4 n) / 4),
          x-w16 s n, x-w16 s 0, x-wname s nm,
          (: (go i) (i < x-pad4 n ? (put s 0, go (i + 1))) (go 0)),
          x-flush s)
       hdr (x-reply s) (<hdr = 1 ? (u32 hdr 8)))                                 ; the atom; () when the round-trip answered with an error

   ; --- GetProperty (opcode 20) -> (format nitems bytes); decoders on top ---
   (x-getprop s win prop)
    (: _ (x-w8 s 20, x-w8 s 0, x-w16 s 6, x-w32 s win, x-w32 s prop,
          x-w32 s 0, x-w32 s 0, x-w32 s 1024, x-flush s)
       hdr (x-reply s)
       (? !(<hdr = 1) [0 0 ()]                                                   ; the window died: no property
          (: rlen (u32 hdr 4)
             bytes (x-rbytes s (rlen * 4))
             [(hdr 1) (u32 hdr 12) bytes])))
   (x-atoms-of bytes) (? (#bytes < 4) ()                                         ; group 4 le bytes -> a 32-bit atom/window list
                         (u32 bytes 0 >< x-atoms-of (drop 4 bytes)))
   (x-getprop-atoms s win prop) (x-atoms-of ((x-getprop s win prop) 2))
   (x-has-atom s win propnm valnm) (: at (x-getprop-atoms s win (x-intern s propnm)) va (x-intern s valnm)   ; isInProperty
                                      (any (a \ a = va) at))
   (x-getclass s win)                                                            ; wm_class (atom 67) = "instance\0class\0"; the class is the 2nd
    (: bytes ((x-getprop s win 67) 2)
       (go l seen acc) (? !(two? l) acc                                          ; walk past the first NUL, then collect to the next
                          (<l = 0) (seen ? acc (go >l 1 acc))
                          (go >l seen (seen ? (acc + string <l) acc)))
       (go bytes 0 ""))
   (x-transient s win) (: a (x-getprop-atoms s win 68) (two? a ? <a))            ; wm_transient_for (atom 68) -> the parent window | ()

   ; --- GetGeometry (opcode 14) -> the window's (x y w h) ---
   (x-getgeom s win)
    (: _ (x-w8 s 14, x-w8 s 0, x-w16 s 2, x-w32 s win, x-flush s)
       hdr (x-reply s)
       (? !(<hdr = 1) '(0 0 1 1)                                                 ; the window died: a harmless placeholder rect
          [(u16 hdr 12) (u16 hdr 14) (u16 hdr 16) (u16 hdr 18)]))
   (x-raise s win) (x-w8 s 12, x-w8 s 0, x-w16 s 4, x-w32 s win,                 ; ConfigureWindow cwstackMode=Above
                    x-w16 s 64, x-w16 s 0, x-w32 s 0)
   (x-lower s win) (x-w8 s 12, x-w8 s 0, x-w16 s 4, x-w32 s win,                 ; ConfigureWindow cwstackMode=Below (a desktop stays under)
                    x-w16 s 64, x-w16 s 0, x-w32 s 1)
   (x-kill s win) (x-w8 s 113, x-w8 s 0, x-w16 s 2, x-w32 s win)                 ; KillClient: the last resort -- it severs the whole
                                                                                 ; connection, every window that client owns dies with it
   (x-wmdel s win)                                                               ; ICCCM close: SendEvent a wm_delete_window ClientMessage --
    (: pr (x-intern s "WM_PROTOCOLS") dl (x-intern s "WM_DELETE_WINDOW")         ; the client shuts one window, gracefully
       (x-w8 s 25, x-w8 s 0, x-w16 s 11, x-w32 s win, x-w32 s 0,
        x-w8 s 33, x-w8 s 32, x-w16 s 0, x-w32 s win, x-w32 s pr,
        x-w32 s dl, x-w32 s 0, x-w32 s 0, x-w32 s 0, x-w32 s 0))
   (x-confnotify s win)                                                          ; ICCCM 4.1.5: a ConfigureRequest the layout overrules still
    (: _ (x-w8 s 14, x-w8 s 0, x-w16 s 2, x-w32 s win, x-flush s)                ; gets an answer -- a synthetic ConfigureNotify carrying the
       hdr (x-reply s)                                                           ; window's real geometry (VTE sizes its character grid off
       (? !(<hdr = 1) 0                                                          ; what it believes the window is; silence = a garbled grid)
          (: gx (u16 hdr 12) gy (u16 hdr 14) gw (u16 hdr 16) gh (u16 hdr 18) bw (u16 hdr 20)
             (x-w8 s 25, x-w8 s 0, x-w16 s 11, x-w32 s win, x-w32 s 131072,      ; SendEvent, mask StructureNotify
              x-w8 s 22, x-w8 s 0, x-w16 s 0, x-w32 s win, x-w32 s win,          ; ConfigureNotify: event, window,
              x-w32 s 0, x-w16 s gx, x-w16 s gy, x-w16 s gw, x-w16 s gh,         ; above-sibling None, the geometry,
              x-w16 s bw, x-w16 s 0, x-w32 s 0))))                               ; border, override-redirect 0, pad
   (x-delprop s win prop) (x-w8 s 19, x-w8 s 0, x-w16 s 3, x-w32 s win, x-w32 s prop)   ; DeleteProperty
   (x-grabbtn s win)                                                             ; GrabButton 1/2/3, AnyModifier, pointer sync: a click on a
    (: (one b) (x-w8 s 28, x-w8 s 0, x-w16 s 6, x-w32 s win,                     ; managed window freezes until x-replay hands it back to the
                x-w16 s 4, x-w8 s 0, x-w8 s 1, x-w32 s 0,                        ; app unstolen (click-to-focus). buttons 4/5 (the wheel) stay
                x-w32 s 0, x-w8 s b, x-w8 s 0, x-w16 s 32768)                    ; ungrabbed -- scroll never rides through the wm
       (one 1, one 2, one 3))
   (x-replay s) (x-w8 s 35, x-w8 s 2, x-w16 s 2, x-w32 s 0)                      ; AllowEvents ReplayPointer, CurrentTime
   (x-allow-async s) (x-w8 s 35, x-w8 s 1, x-w16 s 2, x-w32 s 0)                 ; AllowEvents AsyncPointer: thaw a sync grab without replaying
   (x-grabptr s win)                                                             ; GrabPointer, async, ButtonRelease(8)|PointerMotion(64) -- the drag catch
    (: _ (x-w8 s 26, x-w8 s 0, x-w16 s 6, x-w32 s win,
          x-w16 s 72, x-w8 s 1, x-w8 s 1, x-w32 s 0, x-w32 s 0, x-w32 s 0, x-flush s)
       (x-reply s))                                                              ; a reply (the grab status) -- read it to keep the demux synced
   (x-ungrabptr s) (x-w8 s 27, x-w8 s 0, x-w16 s 2, x-w32 s 0)                   ; UngrabPointer, CurrentTime

   ; --- QueryTree (opcode 15) + GetWindowAttributes (opcode 3): at startup we
   ;     adopt the windows that mapped before the redirect took hold (closing the
   ;     race) and any survivors of a restart-in-place. ---
   (x-querytree s win)
    (: _ (x-w8 s 15, x-w8 s 0, x-w16 s 2, x-w32 s win, x-flush s)
       hdr (x-reply s)
       ; presence rides out of band: (1 windows) on success, () when the query
       ; failed. a root with no children is a real answer and so is a garbled
       ; reply, and the bare list cannot tell them apart -- which is how a
       ; desynced connection read as "there are no windows", the startup sweep
       ; adopted nothing, and lux ran on for good with an empty client list. the
       ; house law (CLAUDE.md): carry presence in a wrapper tested with two?,
       ; never in the value's own emptiness. QueryTree on the root cannot fail
       ; for any legitimate reason -- the root id is always valid -- so this ()
       ; is precisely a garble detector, and it used to throw the detection away.
       (? !(<hdr = 1) ()
          (: n (u16 hdr 16) bytes (x-rbytes s (u32 hdr 4 * 4))
             [1 (take n (x-atoms-of bytes))])))
   (x-attrs s win)                                                               ; -> (map-state override-redirect); map-state 2 = IsViewable
    (: _ (x-w8 s 3, x-w8 s 0, x-w16 s 2, x-w32 s win, x-flush s)
       hdr (x-reply s)
       (? !(<hdr = 1) '(0 1)                                                    ; the window died: reads as unmanageable
          (: _ (x-rbytes s (u32 hdr 4 * 4))                                     ; drain the trailing reply data
             [(hdr 26) (hdr 27)])))
   (x-manageable? s win) (: a (x-attrs s win) (<a = 2 && <>a = 0))               ; viewable and not override-redirect

   ; --- become the wm: SubstructureRedirect|Notify on the root, synced by an
   ;     intern round-trip. 1 = we hold it, 0 = another wm -- the Access error
   ;     arrives before the intern reply, so x-reply answers the intern wait
   ;     with the error packet: refusal reads as the intern's () ---
   (x-redirect s root)
    (: _ (x-cwa-events s root 1572864)
       (x-intern s "WM_STATE" ? 1 0))

   ; --- keyboard: GetKeyboardMapping -> a keysym->keycode map; GrabKey ---
   (x-keymap s mink maxk syms)                                                   ; syms = the keysyms we want located
    (: nkc (1 + (maxk - mink))
       _ (x-w8 s 101, x-w8 s 0, x-w16 s 2, x-w8 s mink, x-w8 s nkc, x-w16 s 0, x-flush s)
       hdr (x-reply s)                                                           ; via the demux -- an event mid-wait must not desync us
       ; ..and neither must an error. x-reply answers the wait on an error
       ; packet too (a blocking wait would hang the whole wm), and on one, byte 1
       ; is the error code -- so reading nkc*kpc keysyms off the stream would eat
       ; nkc*code words of the next reply and desync the connection for good.
       ; every other reply reader in this file guards on (<hdr = 1); this was
       ; the one that did not, and it is the only unguarded one that consumes
       ; variable-length data off an unvalidated byte. a desync here is silent and
       ; permanent: the next QueryTree reads as garbage, x-querytree answers the
       ; empty list, and lux's startup sweep adopts nothing while printing X
       ; errors whose codes (0, 159) are not even valid -- the tell that the bytes
       ; are misaligned rather than the server complaining. 0 keysyms degrades to
       ; "no chords", the inert value this file answers everywhere else. --
       kpc (<hdr = 1 ? (hdr 1) 0)
       km (tablet 8)
       _ (: (go i) (i < nkc * kpc ?
                    (: sym (x-r32 s)
                       _ (? (any (x \ x = sym) syms && !(peep km sym 0))
                            (pin km sym (mink + (i // kpc))))
                       (go (i + 1))))
            (go 0))
       km)
   (x-grab s root mods kc) (kc < 1 ? 0
                            (x-w8 s 33, x-w8 s 1, x-w16 s 4, x-w32 s root,
                             x-w16 s mods, x-w8 s kc, x-w8 s 1, x-w8 s 1,
                             x-w8 s 0, x-w8 s 0, x-w8 s 0))
   (x-ungrabkeys s root) (x-w8 s 34, x-w8 s 0, x-w16 s 3,                        ; UngrabKey AnyKey/AnyModifier: a regrab starts clean
                          x-w32 s root, x-w16 s 32768, x-w16 s 0)

   ; --- AllocNamedColor (85): a color NAME -> a pixel (the config's border colors) ---
   (x-color s cmap nm)
    (: n #nm
       _ (x-w8 s 85, x-w8 s 0, x-w16 s (3 + (n + x-pad4 n) / 4),
          x-w32 s cmap, x-w16 s n, x-w16 s 0, x-wpad s nm, x-flush s)
       hdr (x-reply s) (<hdr = 1 ? (u32 hdr 8) 0))                               ; an error answers the wait: pixel 0, not a garbage id
   (x-borderpix s win px)                                                        ; ChangeWindowAttributes cwborderPixel
    (x-w8 s 2, x-w8 s 0, x-w16 s 4, x-w32 s win, x-w32 s 8, x-w32 s px)

   ; --- the root cursor (setDefaultCursor xC_left_ptr): OpenFont "cursor" (45),
   ;     CreateGlyphCursor (94, glyph 68/69, black on white), cwa cwcursor ---
   (x-cursor s root fid cid)
    (x-w8 s 45, x-w8 s 0, x-w16 s 5, x-w32 s fid,
     x-w16 s 6, x-w16 s 0, x-wpad s "cursor",
     x-w8 s 94, x-w8 s 0, x-w16 s 8, x-w32 s cid, x-w32 s fid, x-w32 s fid,
     x-w16 s 68, x-w16 s 69,
     x-w16 s 0, x-w16 s 0, x-w16 s 0,
     x-w16 s 65535, x-w16 s 65535, x-w16 s 65535,
     x-w8 s 46, x-w8 s 0, x-w16 s 2, x-w32 s fid,                                ; CloseFont: the cursor keeps its glyphs
     x-w8 s 2, x-w8 s 0, x-w16 s 4, x-w32 s root, x-w32 s 16384, x-w32 s cid)

   ; --- ~/.Xauthority: big-endian length-prefixed records; find the display's
   ;     mit-magic-cookie-1 -> (name data), or (). The parser is pure (bytes in
   ;     a string); xauth-cookie is the host loader over open/slurp. ---
   (xauth-parse b disp)
    (: n #b
       (fld i) (: ln (b i << 8 | b (i + 1)) e (i + 2 + ln) [(snip b (i + 2) e) e])
       (go i) (? (n - i < 10) ()                                                 ; too short for another record
                 (: ad (fld (i + 2)) nu (fld <>ad) nm (fld <>nu) da (fld <>nm)
                    (<nu = disp && <nm = "MIT-MAGIC-COOKIE-1" ? [<nm <da]
                     (go <>da))))
       (go 0))
   (xauth-cookie path disp)
    (: p (open path "r")
       (p ? (: b (slurp p) _ (close p) (xauth-parse b disp))))

   ; --- the event decoder: one 32-byte event -> (code detail window state) | ()
   ;     code 0 = X error (window is garbage). window at byte 8, state at byte 28,
   ;     detail (keycode) at byte 1 -- enough for every event the wm handles. ---
   (x-decode pkt) [(127 & <pkt) (pkt 1) (u32 pkt 8) (u16 pkt 28) pkt]         ; code, detail(keycode), window@8, state@28, + the raw bytes
   (x-next s)                                                                    ; a stashed event first, else the socket
    (: q (evq 0)
       (two? q ? (pin evq 0 >q, x-decode <q)
        (: b0 (see s)
           (b0 = -1 ? ()
            (: pkt (b0 >< x-rbytes s 31)
               (b0 = 1 ? (x-skip s (4 * u32 pkt 4), x-next s)                    ; an orphaned reply (its wait ended on an error): drain + skip
                (x-decode pkt)))))))
   (ev-code e) <e  (ev-detail e) <>e  (ev-window e) (e 2)  (ev-state e) (e 3)
   (ev-raw e) (e 4))                                                        ; the 32 raw bytes (ConfigureRequest reads its geometry here)
