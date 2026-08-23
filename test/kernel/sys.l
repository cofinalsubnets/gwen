; the syscall seam (free/sys.c): nolibc's write() reaching a k_sources row
; through __ai_sys, with nothing hosted underneath. kernel corpus only
; (kernel.mk's kt) -- no other seat can ask this question, since on a host the
; same __ai_sys is a mksys lay issuing the machine's own trap.
;
; (syswrite fd str) is the gate's one instrument, K_TEST-only (kmain.c): it
; calls write(2) and nothing else, so what it proves is the whole path.

; the bytes land in a pipe, and the pipe reads them back -- the syscall wrote to
; a row, not to a console that would only prove the console
(: pp (pipe 0) rfd (cap pp) wfd (cup pp)
   n (syswrite wfd "through the door")
   _ (close (fdopen wfd))                       ; the last writer leaves; the read ends
   rp (fdopen rfd) s (slurp rp) _ (close rp)
 (assert (= n 16) (= s "through the door")))

; a write of nothing is nothing, and still not a refusal
(: pp (pipe 0) wfd (cup pp)
   n (syswrite wfd "")
   _ (close (fdopen wfd)) _ (close (fdopen (cap pp)))
 (assert (= n 0)))

; a non-string is misuse and answers -1, where a short count would be a lie
(: pp (pipe 0) wfd (cup pp)
   n (syswrite wfd 7)
   _ (close (fdopen wfd)) _ (close (fdopen (cap pp)))
 (assert (= n -1)))

; ⚠ a row that is not there is the VOID, not an error: k_row_write answers what
; it took off an absent row, which is what keeps a closed seat's output from
; failing a task that cannot know. -1 here would make every such write a fault.
(: n (syswrite 4096 "into the void")
 (assert (= n 13)))

; --- the rows themselves, through (syscall "name" ..) ------------------------
; the instrument takes the NAME: close is 3 on x86_64 and 57 on aarch64, so a
; test that spelled a number would pass on one seat and mean nothing on the
; other. a name no row answers to is (), which is how a MISSING row is told
; from a row that refused. an argument spells itself by kind: a charm is the
; integer, a string passes its bytes, a cask lends its bytes as an output
; buffer -- anything else is misuse and answers -1.

; every row this seat carries is reachable, and a stranger is not
(assert (= () (syscall "no-such-row" 0 0 0 0))
        (charm? (syscall "close" 4096 0 0 0)))

; close: EBADF off a row that was never opened, and () tells that from unknown
(assert (= -9 (syscall "close" 4096 0 0 0)))

; close: a real pipe end closes clean, and closing it twice is EBADF the second
; time -- the row is gone, which is the whole point of answering an errno
(: pp (pipe 0) rfd (cap pp) wfd (cup pp)
   a (syscall "close" wfd 0 0 0)
   b (syscall "close" wfd 0 0 0)
   _ (syscall "close" rfd 0 0 0)
 (assert (= 0 a) (= -9 b)))

; lseek: a pipe has a row but no seek -- ESPIPE, which is exactly the distinction
; the love door could not make (its lvm_lseek answers a bare -1 for both)
(: pp (pipe 0) rfd (cap pp) wfd (cup pp)
   r (syscall "lseek" rfd 0 0 0)
   _ (fdclose wfd) _ (fdclose rfd)
 (assert (= -29 r)))

; lseek: EBADF beats ESPIPE -- no row at all is a different answer from a row
; that will not seek, and the order of the two checks is what says so
(assert (= -9 (syscall "lseek" 4096 0 0 0)))

; lseek on a ramfs file: SET, CUR and END all land, and END is the size --
; lib/fat.l is baked, so its length is a fact this seat can check against read
(: fd (openfd "lib/fat.l" 0)
   z (syscall "lseek" fd 0 2 0)                 ; END -> the size
   a (syscall "lseek" fd 0 0 0)                 ; SET 0 -> 0
   b (syscall "lseek" fd 10 1 0)                ; CUR +10 -> 10
   c (syscall "lseek" fd 5 1 0)                 ; CUR +5  -> 15
   _ (fdclose fd)
 (assert (< 0 z) (= 0 a) (= 10 b) (= 15 c)))

; lseek: a seek before the start is EINVAL and moves nothing; an unknown whence
; is EINVAL too, where the love door quietly read it as SEEK_SET
(: fd (openfd "lib/fat.l" 0)
   _ (syscall "lseek" fd 8 0 0)
   r (syscall "lseek" fd (- 0 99) 1 0)          ; 8 - 99 < 0
   w (syscall "lseek" fd 0 7 0)                 ; no such whence
   at (syscall "lseek" fd 0 1 0)                ; unmoved
   _ (fdclose fd)
 (assert (= -22 r) (= -22 w) (= 8 at)))

; --- pointer arguments -------------------------------------------------------

; a string argument passes its bytes -- core keeps a NUL behind them, so the
; pointer is exactly what C expects -- and write takes its count from its own
; argument, so a corner of the string can go through
(: pp (pipe 0) rfd (cap pp) wfd (cup pp)
   n (syscall "write" wfd "marshal" 7 0)
   m (syscall "write" wfd "marshal" 4 0)
   _ (close (fdopen wfd))
   rp (fdopen rfd) s (slurp rp) _ (close rp)
 (assert (= n 7) (= m 4) (= s "marshalmars")))

; a cask argument lends its bytes: read fills it in place, and the count says
; how much of it to believe
(: pp (pipe 0) rfd (cap pp) wfd (cup pp)
   _ (syswrite wfd "abcdef")
   _ (close (fdopen wfd))
   b (cask 16)
   n (syscall "read" rfd b 16 0)
   z (syscall "read" rfd b 16 0)                ; writer gone -> EOF, 0
   _ (fdclose rfd)
 (assert (= n 6) (= "abcdef" (snip b 0 n)) (= z 0)))

; a short buffer takes its count and no more; the rest waits in the row
(: pp (pipe 0) rfd (cap pp) wfd (cup pp)
   _ (syswrite wfd "abcdef")
   b (cask 4)
   n (syscall "read" rfd b 4 0)
   s1 (snip b 0 4)                              ; read the corner before the next fill
   m (syscall "read" rfd b 4 0)
   _ (fdclose wfd) _ (fdclose rfd)
 (assert (= n 4) (= "abcd" s1) (= m 2) (= "ef" (snip b 0 2))))

; read off a ramfs file: the same bytes the love door reads
(: fd (openfd "lib/fat.l" 0)
   b (cask 32)
   n (syscall "read" fd b 32 0)
   _ (fdclose fd)
   p (open "lib/fat.l" "r") s (slurp p) _ (close p)
 (assert (= n 32) (= (snip s 0 32) (snip b 0 32))))

; an argument that is none of the three spellings is misuse, answered before
; the door is asked -- the row must not see a 0 it was never given
(: pp (pipe 0) rfd (cap pp) wfd (cup pp)
   r (syscall "write" wfd 'nope 4 0)
   _ (fdclose wfd) _ (fdclose rfd)
 (assert (= -1 r)))
