; The writable tree, inle's rung 2 (doc/misc/inle.md): create, mkdir, rmdir, unlink,
; rename, chdir/cwd -- plus chmod and utime, which kore's cp and touch ride -- and
; the environment (getenv setenv environ), a tablet in the kernel's boot text. The
; shapes are doc/misc/posix.md's and the host's exactly: an effect answers () | a
; positive errno (the host's numbers) | 22 on misuse; a value answers the value |
; (); chdir wears the host's negative lane. test/host/fs.l holds sibling laws
; against the real tree.
;
; kernel only -- kernel.mk's kt, never $t: on the host these paths are the real
; filesystem. Everything made here is unmade at the foot, so the corpus behind
; reads the tree as this file found it.
;
; the `:` has a body -- the assert itself -- so the helpers stay local; and each
; law is its own assert argument, since assert maps over its args and a `do` would
; report only the last one.

(: (rd p) (: q (open p "r") (? (port? q) (: s (slurp q) _ (close q) s) ()))
   (wr p s) (: q (open p "w") (? (port? q) (: _ (say q s) _ (close q) 1) 0))
   d "/tmp/l-wfs"
   f (d + "/a")
   e (d + "/e")
 (assert
   ; -- create: `w` under a live directory mints the file; `a` does too --
   (id? (mkdir d 493) ())
   (wr f "five!")
   (= "five!" (rd f))
   (= 5 <(stat f))
   (< 1600000000000 <>(stat f))                 ; born wearing the clock's date
   (= 32768 (& 61440 <>>(stat f)))
   (member? "a" (readdir d))
   (: q (open (d + "/b") "a") (? (port? q) (: _ (close q) 1) 0))
   (two? (stat (d + "/b")))
   (! (port? (open "/no-such-dir/x" "w")))      ; an absent parent refuses
   ; -- unlink: the entry leaves the tree; absence and misuse answer the host's way --
   (id? (unlink (d + "/b")) ())
   (id? (stat (d + "/b")) ())
   (< 0 (unlink (d + "/b")))                    ; gone is a live errno
   (= 21 (unlink d))                            ; a directory refuses: eisdir
   (= 22 (unlink 7))                            ; misuse: EINVAL
   ; -- mkdir/rmdir: a directory is a thing now, empty and stattable --
   (id? (mkdir e 493) ())
   (= 16384 (& 61440 <>>(stat e)))
   (id? (readdir e) ())                         ; empty, told from absent by stat
   (= 17 (mkdir e 493))                         ; again: EEXIST
   (= 17 (mkdir "lib" 493))                     ; a baked prefix is a directory too
   (= 20 (mkdir (f + "/x") 493))                ; under a file: enotdir
   (= 2 (mkdir "/hole/x" 493))                  ; under a hole: ENOENT
   (= 22 (mkdir 7 493))
   (wr (e + "/k") "y")
   (= 39 (rmdir e))                             ; occupied: enotempty
   (id? (unlink (e + "/k")) ())
   (id? (rmdir e) ())
   (= 2 (rmdir e))                              ; and gone is ENOENT
   (= 20 (rmdir f))                             ; a file was never a directory
   ; -- rename: a file moves whole; a target file is replaced under it --
   (id? (rename f (d + "/z")) ())
   (id? (stat f) ())
   (= 5 <(stat (d + "/z")))
   (< 0 (rename (d + "/ghost") f))              ; a missing source is a live errno
   (= 22 (rename 7 f))
   (wr f "yo")
   (id? (rename (d + "/z") f) ())
   (= "five!" (rd f))
   ; a directory carries its children, and never moves into itself
   (id? (mkdir (d + "/dd") 493) ())
   (wr (d + "/dd/x") "1")
   (id? (rename (d + "/dd") (d + "/ee")) ())
   (= "1" (rd (d + "/ee/x")))
   (id? (stat (d + "/dd")) ())
   (= 22 (rename d (d + "/in")))
   ; -- chdir/cwd: the cwd is a kernel string; "/" is the boot seat --
   (= "/" (cwd 0))
   (id? (chdir "lib") ())
   (= "/lib" (cwd 0))
   (< 0 (tally (rd "json.l")))                  ; a relative path reads off the cwd
   (member? "json.l" (readdir "."))
   (id? (chdir "..") ())
   (= "/" (cwd 0))
   (< (chdir f) 0)                              ; a file is no seat (the negative lane)
   (< (chdir "/nope") 0)
   (= -1 (chdir 7))
   ; -- chmod and utime: the bits and the date land, and stat reads them back --
   (id? (chmod f 384) ())
   (= 384 (- <>>(stat f) 32768))
   (id? (utime f 1600000000000) ())
   (= 1600000000000 <>(stat f))
   (id? (utime f ()) ())
   (< 1600000000000 <>(stat f))
   (= 22 (chmod 7 384))
   (= 2 (chmod (d + "/ghost") 384))
   ; -- the environment: a tablet behind the host's three doors --
   (id? (getenv "WFS") ())                      ; unset is the real ()
   (id? (setenv "WFS" "1") ())
   (= "1" (getenv "WFS"))
   (id? (setenv "WFS" "2") ())                  ; a re-set replaces: still one row
   (= "2" (getenv "WFS"))
   (member? "WFS=2" (environ 0))
   (id? (setenv "WFS2" "x") ())
   (= 2 (tally (environ 0)))
   (id? (setenv "WFS2" ()) ())                  ; a non-string value unsets
   (id? (getenv "WFS2") ())
   (= 1 (tally (environ 0)))
   (= 22 (setenv 7 "x"))
   (id? (getenv 7) ())
   ; -- and the tree (and the tablet) go back the way they came --
   (id? (setenv "WFS" ()) ())
   (id? (unlink f) ())
   (id? (unlink (d + "/ee/x")) ())
   (id? (rmdir (d + "/ee")) ())
   (id? (rmdir d) ())
   (id? (stat d) ())))
