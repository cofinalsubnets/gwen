; --- test/host/init.l: smoke test for the init (pid1 supervisor) nifs (src/posix.c) ---
; spawn/glean/wait/still link into `love` but not love0 (which bakes the corpus), so this
; runs standalone against the built binary, like test/host/pty.l -- not in test/*.l. Run
; (assert comes from test/00-init.l):
;   cat test/00-init.l test/host/init.l | out/host/love
; the full pid-1 story -- mounting proc, reaping a reparented orphan -- needs love to
; actually be pid 1: `make init-container` (a pid+user+mount namespace). Here we
; cover the glean nif's three distinct returns and the spawn/wait pair.

; glean is waitpid(-1, WNOHANG), with three returns the supervisor tells apart:
;   (pid . status)  a child was reaped
;   ()              none pending (children alive) -- the real zero point, not charm 0
;   -echild         no children remain -- a negative charm (nil?-true), distinct from ()
(: (glean-until n)                                   ; glean, paced, to a (pid . status) pair (or steps out)
     (: ev (glean 0) (? (two? ev) ev (< n 1) () (: _ (rest 2) (glean-until (- n 1)))))
   _ (assert
       ; spawn is non-blocking: a real live pid, the child runs on
       (: p (spawn ["sleep" "5"])
          (&& (charm? p) (< 0 p)
              ; a child alive, none exited -> glean is the real () (not charm 0), nil?-true
              (id? (glean 0) ()) (nil? (glean 0)) !(charm? (glean 0))
              ; kill it (bao's `still`), then glean the zombie -> (pid . 128+sig)
              (: _ (still p 9) r (glean-until 200)
                 (&& (two? r) (= (cap r) p) (= (cup r) 137)))))
       ; no children left -> -echild: a negative charm, nil?-true, but not the () above
       (charm? (glean 0)) (< (glean 0) 0) (nil? (glean 0)) !(id? (glean 0) ())
       ; the blocking foreground wait: spawn then wait -> the child's exit status
       (= 0 (wait (spawn ["true"])))
       (= 3 (wait (spawn ["sh" "-c" "exit 3"])))
       ; the "()"-returns are the real zero point now (the ai_nil -> () flip), not charm 0:
       ; (id? x ()) holds and (charm? x) is false -- charm 0 would fail both.
       (id? (close -1) ()) !(charm? (close -1))               ; close nothing -> the real ()
       (id? (getenv "LOVE_NO_SUCH_ENV_VAR_XYZZY") ())             ; getenv absent -> the real ()
       !(charm? (getenv "LOVE_NO_SUCH_ENV_VAR_XYZZY"))))


; --- the fs nifs: stat readdir unlink lseek (the posix_ lane in src/posix.c) -----
(: d "out/.fs-nif-probe" f (+ d "/x")
   _ (mkdir d 493)                                  ; EEXIST from a prior run is fine
   _ (unlink f)                                     ; a clean slate, tolerated if absent
   ; write 5 bytes through the port lane, then measure with stat
   _ (: p (open f "w") _ (say p "hello") _ (flush p) (close p))
   st (stat f)
   (assert
    ; stat: (size mtime mode) -- 5 bytes, an mtime on the (clock t) ms scale, a file mode
    (two? st) (= 5 (cap st))
    (< 1600000000000 (caup st))                     ; ms, not seconds (a 2020s stamp)
    (= 32768 (& 61440 (cauup st)))                  ; s_ifmt bits say regular file
    ; a directory wears the dir bits; a missing path is nothing, the real ()
    (= 16384 (& 61440 (cauup (stat d))))
    (id? (stat (+ f "-absent")) ()) !(charm? (stat (+ f "-absent")))
    ; readdir: our one entry, as a string; a missing dir is ()
    (: l (readdir d) (&& (two? l) (= "x" (cap l)) !(two? (cup l))))
    (id? (readdir (+ d "-absent")) ())
    ; lseek rides the raw openfd lane: end is the size, set returns the offset
    (: fd (openfd f 0)
       (&& (= 5 (lseek fd 0 2)) (= 1 (lseek fd 1 0)) (= 3 (lseek fd 2 1))
           (id? (close fd) ())))
    (< (lseek -1 0 0) 0)                            ; a bad fd is a negative charm
    ; unlink: () ok, then the file really is gone, and a re-unlink says so (ENOENT, POSITIVE)
    (id? (unlink f) ())
    (id? (stat f) ()) (id? (readdir d) ())
    (: e (unlink f) (&& (charm? e) (< 0 e)))))

(test_fin ())
(puts "init: ok") (putc 10)
