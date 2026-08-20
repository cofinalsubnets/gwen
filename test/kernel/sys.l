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
