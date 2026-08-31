; The file nifs, inle's rung 1 (doc/misc/inle.md): stat, readdir, lseek and the raw-fd
; lane under them -- and the wall clock, which is what makes an mtime a date. The
; shapes are doc/misc/posix.md's and the host's, exactly: (size ms mode ns) and a list of
; entry strings, because kore reads these and a divergence would be silent where an
; absence is loud (test/host/init.l holds the same laws against the real tree).
;
; the initrd is flat -- rows for "lib/json.l" and none for "lib" -- so a directory
; is a prefix some path lies under, and the entries are the distinct next components
; of those paths. That is why the root lists one name and not thirteen.
;
; kernel only -- kernel.mk's kt, never $t: on the host these paths are the real tree.
; It runs after ramfs.l, which owns lib/json.l's bytes; the write laws here take
; lib/dns.l instead (a file the boot never loads) and put it back.
;
; the `:` has a body -- the assert itself -- so the helpers stay local; and each
; law is its own assert argument, since assert maps over its args and a `do` would
; report only the last one.

(: f "lib/json.l" d "lib" g "lib/dns.l"
   (rd p) (: q (open p "r") s (slurp q) _ (close q) s)
   (wr p s) (: q (open p "w") _ (say q s) (close q))
   st (stat f)
   sz (tally (rd f))
   orig (rd g)
   was (stat g)
   _ (wr g "abc")
   now (stat g)
   _ (wr g orig)
   back (stat g)
 (assert
   ; stat answers the four fields, and the size is the bytes a read hands over
   (two? st)
   (= sz <st)
   ; the mtime is the source's, baked by lcatfs: milliseconds, and a 2020s stamp
   (< 1600000000000 <>st)
   (= 32768 (& 61440 <>>st))                     ; s_ifmt bits say regular file
   (= (* 1000000 <>st) <>>>st)                   ; ns is that same date whole
   ; a directory wears the dir bits and the newest date under it
   (= 16384 (& 61440 <>>(stat d)))
   (< 1600000000000 <>(stat d))
   ; absence is the real (), never an empty tray of fields
   (id? (stat "lib/no-such-thing.l") ())
   (! (charm? (stat "lib/no-such-thing.l")))
   ; readdir hands over the baked names as strings
   (member? "json.l" (readdir d))
   (string? (cap (readdir d)))
   ; "" and "." are both the root, as they are on the host -- and it holds two
   ; entries: lib, deduped across all thirteen rows that lie under it, and tmp,
   ; the scratch the rung-2 tree seeds at boot
   (member? "lib" (readdir "."))
   (member? "lib" (readdir ""))
   (member? "tmp" (readdir ""))
   (= 2 (tally (readdir "")))
   ; a file is no directory, and neither is a prefix nothing lies under
   (id? (readdir f) ())
   (id? (readdir "lib/nope") ())
   ; lseek rides the raw openfd lane: end is the size, set and cur the arithmetic
   (: fd (openfd f 0)
      (&& (= sz (lseek fd 0 2)) (= 1 (lseek fd 1 0)) (= 3 (lseek fd 2 1))
          (id? (fdclose fd) ())))
   (< (lseek -1 0 0) 0)                          ; a bad fd is negative, never a seek
   (< (openfd "lib/no-such-thing.l" 0) 0)        ; and so is an absent path
   (id? (fdclose -1) ())                         ; closing nothing is the zero point
   ; a write moves the size, and the date with it: stat reads the copy's stamp once
   ; there is a copy, the bake's until then
   (= 3 <now)
   (<= <>was <>now)
   (= (tally orig) <back)
   ; the wall clock, which every line above leans on: the door's date if it left
   ; one, else the machine's own rtc (mc146818 on x86_64, PL031 on aarch64). It reads as
   ; milliseconds since the epoch here exactly as it does on the host -- an uptime
   ; in ticks is what it used to be, and would fail this by a factor of a thousand.
   (< 1600000000000 (clock 0))))
