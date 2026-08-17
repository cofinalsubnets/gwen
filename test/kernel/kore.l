; kore's fs tools on the rung-2 tree -- doc/inle.md's rung-2 gate: ls, cp, mv, rm,
; mkdir, touch, pwd smoke on the K_TEST kernel. crew/kore/{text,core,fs}.l load
; just before this file (kernel.mk's kt), with kore0.l's identity `quit` pinned
; ahead of them, so a main ANSWERS its exit code instead of leaving; every effect
; is then proved on the tree itself, and unmade at the foot. (The tools print
; their usual output to the console mid-corpus; that is the smoke, not a mess.)

(use 'kore) ; the applets ride module 'kore -- the cat's kore files above just registered it

(: (rd p) (: q (open p "r") (? (port? q) (: s (slurp q) _ (close q) s) ()))
   (wr p s) (: q (open p "w") (? (port? q) (: _ (say q s) _ (close q) 1) 0))
   d "/tmp/l-kore"
 (assert
   (= 0 (pwd-main ()))                          ; prints the seat, answers 0
   (= 0 (mkdir-main `("-p" (d + "/a/b"))))
   (udirp (d + "/a/b"))
   (wr (d + "/f") "hi kore")
   (= 0 (ls-main `(d)))
   (= 1 (ls-main `("/tmp/l-kore-ghost")))       ; a miss says so and exits 1
   (= 0 (cp-main `((d + "/f") (d + "/a"))))     ; a dir destination pulls the name in
   (= "hi kore" (rd (d + "/a/f")))
   (= 0 (touch-main `((d + "/t"))))             ; be there, and be now
   (two? (stat (d + "/t")))
   (= 0 (mv-main `((d + "/t") (d + "/t2"))))
   (id? (stat (d + "/t")) ())
   (two? (stat (d + "/t2")))
   (= 0 (rm-main `((d + "/t2"))))
   (id? (stat (d + "/t2")) ())
   (= 0 (rm-main `("-r" d)))                    ; the tree walks out bottom-up
   (id? (stat d) ())))
