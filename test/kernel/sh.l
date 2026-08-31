; the rung-4 gate (doc/misc/inle.md): a lush pipeline of kore tools on the kernel.
; the engine parts (job..eval, in cat order) load just before this file, so
; sh-line is the shell -- the same door test/host/sh.l reads on the host. the
; tail redirects onto the rung-2 tree and is read back; the tree is restored.

(use 'lush) ; the engine parts above just registered module 'lush

(: (rd p) (: q (open p "r") (? (port? q) (: s (slurp q) _ (close q) s) ()))
   ; wc pads its counts to the byte total's width, so read the number, not the face
   (numof s) ((: (go i acc) (? (<= (tally s) i) acc
                  (: c (s i) (? (&& (<= 48 c) (<= c 57))
                                (go (+ i 1) (+ (* 10 acc) (- c 48)))
                                (go (+ i 1) acc)))))
              0 0)

   ; the doc's own spelling: kore ls | kore wc -l, the line count = the tree's
   r1 (sh-line "kore ls lib | kore wc -l > /tmp/l-sh-pipe")
   s1 (rd "/tmp/l-sh-pipe")
   _  (unlink "/tmp/l-sh-pipe")

   ; and a byte-exact tail, where wc's padding is width one
   r2 (sh-line "kore echo hi | kore wc -c > /tmp/l-sh-wc")
   s2 (rd "/tmp/l-sh-wc")
   _  (unlink "/tmp/l-sh-wc")

 (assert (= r1 0) (string? s1) (= (numof s1) (tally (readdir "lib")))
         (= r2 0) (= s2 "3\n")))
