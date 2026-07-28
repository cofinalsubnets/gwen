; crew/haven/drm.l -- the metal's vocabulary: DRM/KMS spoken in love over two
; nifs (host/mem.c: a generic ioctl with the struct in a cask, and an offset
; mmap for dumb buffers). enumeration needs no master and runs beside a live
; X server; the modeset half (drm-master, drm-set) needs the card free -- a
; vt of one's own, or vkms. loads after crew/haven/wire.l (the cask u32s).
;
;   (drm-open k)            -> the card's port | ()   (k a card number)
;   (drm-res p)             -> (crtcs connectors encoders) as id lists | ()
;   (drm-conn p id)         -> (connection encoder mmw mmh modes) | ()
;                              a mode = (w h hz raw68) -- raw rides to drm-set
;   (drm-encoder p id)      -> (type crtc possible-crtcs) | ()
;   (drm-dumb p w h)        -> (handle pitch size) | ()
;   (drm-fb p w h pitch hd) -> fb-id | ()
;   (drm-map p hd size)     -> ptr charm | () -- brush it with mapout
;   (drm-set p crtc fb conn raw68) -> 0 | -errno      (the modeset)
;   (drm-master p) (drm-unmaster p) -> 0 | -errno
;
; connection: 1 connected, 2 disconnected, 3 unknown. all layouts x86_64.
; a body-LESS `:` -- the helpers leak; none shadows a nif.
(:
   ; --- the _IOC math: dir 3 = read+write, type 'd' = 0x64 ---
   (drm-req sz nr) (| (<< 3 30) (| (<< sz 16) (| (<< 100 8) nr)))
   (drm-pin64 b i v) (: _ (wl-pin32 b i (& 4294967295 v)) (wl-pin32 b (+ i 4) (>> v 32)))
   (drm-u64 b i) (| (wl-u32 b i) (<< (wl-u32 b (+ i 4)) 32))

   ; --- a scratch mapping for the pointer-carrying calls: (f ptr) runs with
   ;     n bytes mapped, then the scratch unmaps -- the ids pour back through
   ;     mapin into casks ---
   (drm-scratch n f)
     (: fd (memfd n)
        (? (< fd 0) ()
           (: m (mapfd fd n)
              (? (< m 1) (: _ (fdclose fd) ())
                 (: r (f m)
                    _ (unmap m n)
                    _ (fdclose fd)
                    r)))))
   ; n u32 ids off a mapping at ptr+off, as a list
   (drm-ids m off n)
     (: b (cask (* n 4))
        _ (mapin b 0 m off (* n 4))
        (go i acc) (? (< i 0) acc (go (- i 1) (link (wl-u32 b (* i 4)) acc)))
        (go (- n 1) ()))

   (drm-open k)
     (: p (open (+ "/dev/dri/card" (show k)) "r")
        (? (lit? p) p ()))

   ; --- GETRESOURCES 0xA0: the two-call dance -- counts first, then the
   ;     arrays land in scratch ---
   (drm-res p)
     (: b (cask 64)
        r (ioctl p (drm-req 64 160) b)
        (? (< r 0) ()
           (: nc (wl-u32 b 36) nn (wl-u32 b 40) ne (wl-u32 b 44)
              n (+ nc (+ nn ne))
              (? (< n 1) (L () () ())
                 (drm-scratch (* n 4)
                   (\ m (: _ (drm-pin64 b 8 m)                    ; crtc ids
                           _ (drm-pin64 b 16 (+ m (* nc 4)))      ; connector ids
                           _ (drm-pin64 b 24 (+ m (* (+ nc nn) 4)))   ; encoder ids
                           _ (wl-pin32 b 32 0)                    ; no fbs asked
                           r2 (ioctl p (drm-req 64 160) b)
                           (? (< r2 0) ()
                              (L (drm-ids m 0 nc)
                                 (drm-ids m (* nc 4) nn)
                                 (drm-ids m (* (+ nc nn) 4) ne))))))))))

   ; --- GETCONNECTOR 0xA7: modes are 68-byte records; each rides back whole
   ;     (drm-set wants the raw bytes verbatim) ---
   (drm-conn p id)
     (: b (cask 80)
        _ (wl-pin32 b 48 id)
        r (ioctl p (drm-req 80 167) b)
        (? (< r 0) ()
           (: nm (wl-u32 b 32) np (wl-u32 b 36) ne (wl-u32 b 40)
              sz (+ (* nm 68) (+ (* np 12) (* ne 4)))
              (? (< nm 1)
                 (L (wl-u32 b 60) (wl-u32 b 44) (wl-u32 b 64) (wl-u32 b 68) ())
                 (drm-scratch (? (< sz 68) 68 sz)
                   (\ m (: _ (drm-pin64 b 8 m)                    ; modes
                           _ (drm-pin64 b 0 (+ m (* nm 68)))      ; encoders
                           _ (drm-pin64 b 16 (+ m (+ (* nm 68) (* ne 4))))   ; props
                           _ (drm-pin64 b 24 (+ m (+ (* nm 68) (+ (* ne 4) (* np 4)))))
                           r2 (ioctl p (drm-req 80 167) b)
                           (? (< r2 0) ()
                              (: (mode i) (: raw (cask 68)
                                             _ (mapin raw 0 m (* i 68) 68)
                                             (L (| (peep raw 4 0) (<< (peep raw 5 0) 8))
                                                (| (peep raw 14 0) (<< (peep raw 15 0) 8))
                                                (wl-u32 raw 24)
                                                raw))
                                 (go i acc) (? (< i 0) acc (go (- i 1) (link (mode i) acc)))
                                 (L (wl-u32 b 60) (wl-u32 b 44) (wl-u32 b 64) (wl-u32 b 68)
                                    (go (- nm 1) ())))))))))))

   ; --- GETENCODER 0xA6 ---
   (drm-encoder p id)
     (: b (cask 20)
        _ (wl-pin32 b 0 id)
        r (ioctl p (drm-req 20 166) b)
        (? (< r 0) () (L (wl-u32 b 4) (wl-u32 b 8) (wl-u32 b 12))))

   ; --- the dumb buffer: CREATE 0xB2, ADDFB 0xAE, MAP 0xB3 + mapfdo ---
   (drm-dumb p w h)
     (: b (cask 32)
        _ (wl-pin32 b 0 h) _ (wl-pin32 b 4 w) _ (wl-pin32 b 8 32)
        r (ioctl p (drm-req 32 178) b)
        (? (< r 0) () (L (wl-u32 b 16) (wl-u32 b 20) (drm-u64 b 24))))
   (drm-fb p w h pitch hd)
     (: b (cask 28)
        _ (wl-pin32 b 4 w) _ (wl-pin32 b 8 h) _ (wl-pin32 b 12 pitch)
        _ (wl-pin32 b 16 32) _ (wl-pin32 b 20 24) _ (wl-pin32 b 24 hd)
        r (ioctl p (drm-req 28 174) b)
        (? (< r 0) () (wl-u32 b 0)))
   (drm-map p hd size)
     (: b (cask 16)
        _ (wl-pin32 b 0 hd)
        r (ioctl p (drm-req 16 179) b)
        (? (< r 0) ()
           (: m (mapfdo p size (drm-u64 b 8))
              (? (< m 1) () m))))

   ; --- SETCRTC 0xA2: one connector, one mode, the fb on the crtc ---
   (drm-set p crtc fb conn raw68)
     (: b (cask 104)
        (drm-scratch 4
          (\ m (: c4 (cask 4)
                  _ (wl-pin32 c4 0 conn)
                  _ (mapout m 0 c4 0 4)
                  _ (drm-pin64 b 0 m)
                  _ (wl-pin32 b 8 1)                     ; one connector
                  _ (wl-pin32 b 12 crtc)
                  _ (wl-pin32 b 16 fb)
                  _ (wl-pin32 b 32 1)                    ; mode_valid
                  (go i) (? (< i 68) (: _ (pin b (+ 36 i) (peep raw68 i 0)) (go (+ i 1))) 0)
                  _ (go 0)
                  (ioctl p (drm-req 104 162) b)))))

   ; --- the partings: RMFB 0xAF, DESTROY_DUMB 0xB4 ---
   (drm-rmfb p id) (: b (cask 4) _ (wl-pin32 b 0 id) (ioctl p (drm-req 4 175) b))
   (drm-rmdumb p hd) (: b (cask 4) _ (wl-pin32 b 0 hd) (ioctl p (drm-req 4 180) b))

   ; --- master: the modeset ticket. 0x1E/0x1F, no argument ---
   (drm-master p) (ioctl p 25630 0)
   (drm-unmaster p) (ioctl p 25631 0))
