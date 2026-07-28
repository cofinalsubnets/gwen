; crew/haven/wire.l -- the wayland wire: every message is u32 WORDS, little-
; endian -- a header (the object id, then size<<16 | opcode) and the args
; flattened to words (a string carries its length, NUL included, and pads to
; the word). shared by both shores: haven parses requests and ships events,
; skiff the mirror image. the fds never touch these bytes -- they ride the
; socket as SCM_RIGHTS (wl-send / wl-recv, host/sock.c) and queue up beside
; the byte stream, popped in arrival order.
;
; a body-LESS `:` -- all of it leaks; no name here shadows a nif.
(:
   ; --- u32s in a cask, little-endian ---
   (wl-u32 b i) (| (peep b i 0)
                   (| (<< (peep b (+ i 1) 0) 8)
                      (| (<< (peep b (+ i 2) 0) 16) (<< (peep b (+ i 3) 0) 24))))
   (wl-i32 b i) (: v (wl-u32 b i) (? (< v 2147483648) v (- v 4294967296)))
   (wl-pin32 b i v) (: _ (pin b i (& 255 v))
                       _ (pin b (+ i 1) (& 255 (>> v 8)))
                       _ (pin b (+ i 2) (& 255 (>> v 16)))
                       (pin b (+ i 3) (& 255 (>> v 24))))
   (wl-pad4 n) (& 3 (- 0 n))

   ; --- a string arg, both directions ---
   (wl-rstr b i) (: n (wl-u32 b i)                 ; the length COUNTS the NUL
                    (go j acc) (? (< j (- n 1)) (go (+ j 1) (+ acc (peep b (+ (+ i 4) j) 0))) acc)
                    (? (< n 2) "" (go 0 "")))
   (wl-rskip b i) (: n (wl-u32 b i) (+ i (+ 4 (+ n (wl-pad4 n)))))
   (wl-strw t) (: n (tally t)                      ; a string as its WORDS
                  (byt j) (? (< j n) (t j) 0)
                  (go k acc) (? (< k (+ n 1))
                                (go (+ k 4)
                                    (link (| (byt k)
                                             (| (<< (byt (+ k 1)) 8)
                                                (| (<< (byt (+ k 2)) 16) (<< (byt (+ k 3)) 24))))
                                          acc))
                                (rev acc))
                  (link (+ n 1) (go 0 ())))

   ; --- build one message at offset o; the new offset comes back ---
   (wl-msg b o obj op ws) (: sz (+ 8 (* 4 (tally ws)))
                             _ (wl-pin32 b o obj)
                             _ (wl-pin32 b (+ o 4) (| op (<< sz 16)))
                             (go i l) (? (two? l) (: _ (wl-pin32 b i (cap l)) (go (+ i 4) (cup l))) i)
                             (go (+ o 8) ws))
   ; one message, shipped whole: the words, plus the fds that ride its first byte
   (wl-ship c obj op ws fds) (: n (+ 8 (* 4 (tally ws)))
                                b (cask n)
                                _ (wl-msg b 0 obj op ws)
                                (wl-send c b n fds))

   ; --- walk the COMPLETE messages in b's first n bytes: (f obj op off sz)
   ;     per message, the payload at off; the consumed count comes back ---
   (wl-take b n f) (: (go i) (? (< (+ i 8) (+ n 1))
                                (: w (wl-u32 b (+ i 4)) sz (>> w 16)
                                   (? (< sz 8) i
                                      (< n (+ i sz)) i
                                      (: _ (f (wl-u32 b i) (& 65535 w) (+ i 8) sz)
                                         (go (+ i sz)))))
                                i)
                      (go 0))

   ; --- the pump: one recvmsg into the scratch, splice onto the carry, take
   ;     what parses, keep the tail for the next splice. st holds the seam:
   ;     slot 0 the carry fill, slot 1 the fd queue. answers the bytes taken
   ;     in (1 or more), -1 for nothing-there, 0 at eof ---
   (wl-pump c k sc st f) (: r (wl-recv c sc)
                            (? (! (two? r)) -1
                               (: n (cap r)
                                  (? (< n 1) 0
                                     (: _ (? (two? (cup r)) (pin st 1 (cat (peep st 1 ()) (cup r))) 0)
                                        fill (+ (peep st 0 0) n)
                                        _ (pour k (- fill n) sc 0 n)
                                        used (wl-take k fill f)
                                        left (- fill used)
                                        _ (? (< 0 used)
                                             (? (< 0 left)
                                                (: _ (pour sc 0 k used left) (pour k 0 sc 0 left))
                                                0)
                                             0)
                                        _ (pin st 0 left)
                                        n)))))
   (wl-popfd st) (: q (peep st 1 ()) (? (two? q) (: _ (pin st 1 (cup q)) (cap q)) -1)))
