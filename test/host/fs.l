; --- test/host/fs.l: smoke test for the fs nifs (host/posix.c) ---
; rename/symlink/readlink/chmod/chown/utime/umask link into `love` AND love0 now
; (love0 carries the whole host/*.c glob since the self-host promotion); this
; still runs standalone against the built binary, like test/host/init.l --
; not in test/*.l. Run (assert comes from test/00-init.l):
;   cat test/00-init.l test/host/fs.l | out/host/love
; the conventions under test are init.c's posix_ lane: an effect op answers ()
; ok | a POSITIVE errno | EINVAL misuse; a value op answers the value | ().

(: d "out/.fs-nif-smoke"
   a (+ d "/a") b (+ d "/b") l (+ d "/l")
   _ (mkdir d 493)                                  ; EEXIST from a prior run is fine
   _ (unlink a) _ (unlink b) _ (unlink l)           ; a clean slate, tolerated if absent
   _ (: p (open a "w") _ (say p "five!") (close p))

   ; rename: the file moves whole -- old gone, new wears the same size
   _ (assert
       (id? (rename a b) ())                        ; ok is the REAL (), not charm 0
       (id? (stat a) ()) (= 5 (cap (stat b)))
       (< 0 (rename a (+ d "/x")))                  ; a missing source is a live errno
       (= 22 (rename 7 b)))                         ; EINVAL misuse: a non-string path

   ; symlink + readlink: path becomes a link TO target -- and the target string
   ; resolves relative to the LINK's directory, so a link beside its file names
   ; it bare. readlink answers that string verbatim; on a NON-link (or a miss)
   ; it is nothing. stat FOLLOWS the link, so through l it sees the file whole.
   _ (assert
       (id? (symlink "b" l) ())
       (= (readlink l) "b")
       (= 5 (cap (stat l))) (= 32768 (& 61440 (cauup (stat l))))
       (id? (readlink b) ()) (id? (readlink (+ d "/nope")) ()))

   ; chmod: the permission bits land exactly (0600 = 384); utime: the mtime
   ; lands on the stat scale (MILLISECONDS), and a non-charm ms reads "now"
   _ (assert
       (id? (chmod b 384) ())
       (= 384 (- (cauup (stat b)) 32768))
       (id? (utime b 1600000000000) ())
       (= 1600000000000 (caup (stat b)))
       (id? (utime b ()) ())
       (< 1600000000000 (caup (stat b)))            ; "now" is later than the pinned past
       (< 0 (chmod (+ d "/nope") 384))              ; a miss is a live errno
       (= 22 (chmod b "rw")))                       ; EINVAL misuse: a non-charm mode

   ; chown: a no-op self-chown succeeds (-1 -1 leaves both ids alone); misuse EINVALs
   _ (assert
       (id? (chown b -1 -1) ())
       (= 22 (chown b "x" 0)))

   ; umask: always answers the PREVIOUS mask, so set-read-restore round-trips
   _ (: m0 (umask 18)
        (assert (charm? m0) (= 18 (umask m0)) (= m0 (umask m0))))

   ; rmdir: the empty-directory unlink -- full is a live errno, empty goes
   _ (: e (+ d "/sub")
        _ (mkdir e 493)
        (assert
          (< 0 (rmdir d))                           ; d holds b and l still: not empty
          (id? (rmdir e) ())
          (id? (stat e) ())
          (< 0 (rmdir e))                           ; already gone: a live errno
          (= 22 (rmdir 7))))

   ; hardlink: two names, one file -- content shows through both, and the
   ; survivor keeps it when the first name unlinks
   _ (: h (+ d "/h")
        _ (unlink h)
        (assert
          (id? (hardlink b h) ())
          (= 5 (cap (stat h)))
          (< 0 (hardlink b h))                      ; the name is taken: EEXIST
          (: _ (unlink b) (= 5 (cap (stat h))))
          (id? (unlink h) ())))

   ; copyfile: a VALUE op, so it answers the byte count and encodes errno negative.
   ; ⚠ an empty source answers 0 -- a falsy success, which is why every caller tests
   ; the SIGN and never the truth.
   _ (: c (+ d "/c") z (+ d "/z")
        _ (: p (open c "w") _ (say p "five!") (close p))
        _ (: p (open z "w") (close p))
        (assert
          (= 5 (copyfile c (+ d "/c2")))
          (= 5 (cap (stat (+ d "/c2"))))
          (= 0 (copyfile z (+ d "/z2")))            ; empty in, empty out, and 0 is the count
          (= -2 (copyfile (+ d "/nope") (+ d "/c3")))
          (= -1 (copyfile 7 c))                     ; misuse: a non-string path
          (id? (unlink c) ()) (id? (unlink z) ())
          (id? (unlink (+ d "/c2")) ()) (id? (unlink (+ d "/z2")) ())))

   ; selfpath: the running binary's own path, the seat every library walk starts
   ; from. no cwd and no argv here -- an ABSOLUTE path at a real regular file is
   ; the whole contract, and it is what the per-OS doors have to agree on.
   _ (: s (selfpath 0)
        (assert
          (string? s)
          (= 47 (peep s 0 0))                       ; absolute, whatever the cwd was
          (= 32768 (& 61440 (cauup (stat s))))      ; S_IFREG: a file, not the /proc link
          (< 0 (cap (stat s)))))

   _ (unlink l) _ (unlink b)
   _ (test_fin ())
   _ (say out "fs: ok") (put out 10))
