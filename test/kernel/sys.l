; the syscall seam (src/sys.c): nolibc's write() reaching a k_sources row
; through __ai_sys, with nothing hosted underneath. kernel corpus only
; (kernel.mk's kt) -- no other seat can ask this question, since on a host the
; same __ai_sys is a mksys lay issuing the machine's own trap.
;
; (syswrite fd str) is the gate's one instrument, k_test-only (kmain.c): it
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

; a row that is not there is the void, not an error: k_row_write answers what
; it took off an absent row, which is what keeps a closed seat's output from
; failing a task that cannot know. -1 here would make every such write a fault.
(: n (syswrite 4096 "into the void")
 (assert (= n 13)))

; --- the rows themselves, through (syscall "name" ..) ------------------------
; the instrument takes the NAME: close is 3 on x86_64 and 57 on aarch64, so a
; test that spelled a number would pass on one seat and mean nothing on the
; other. a name no row answers to is (), which is how a missing row is told
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

; lseek: a pipe has a row but no seek -- espipe, which is exactly the distinction
; the love door could not make (its lvm_lseek answers a bare -1 for both)
(: pp (pipe 0) rfd (cap pp) wfd (cup pp)
   r (syscall "lseek" rfd 0 0 0)
   _ (fdclose wfd) _ (fdclose rfd)
 (assert (= -29 r)))

; lseek: EBADF beats espipe -- no row at all is a different answer from a row
; that will not seek, and the order of the two checks is what says so
(assert (= -9 (syscall "lseek" 4096 0 0 0)))

; lseek on a ramfs file: set, cur and end all land, and end is the size --
; lib/fat.l is baked, so its length is a fact this seat can check against read
(: fd (openfd "lib/fat.l" 0)
   z (syscall "lseek" fd 0 2 0)                 ; end -> the size
   a (syscall "lseek" fd 0 0 0)                 ; set 0 -> 0
   b (syscall "lseek" fd 10 1 0)                ; cur +10 -> 10
   c (syscall "lseek" fd 5 1 0)                 ; cur +5  -> 15
   _ (fdclose fd)
 (assert (< 0 z) (= 0 a) (= 10 b) (= 15 c)))

; lseek: a seek before the start is EINVAL and moves nothing; an unknown whence
; is EINVAL too, where the love door quietly read it as seek_set
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

; --- the *at path family -----------------------------------------------------
; nolibc's path members all issue the *at numbers with at_fdcwd (-100), so the
; dispatch honors exactly that; an absolute path ignores its dirfd (POSIX's own
; rule) and any other seat refuses enotsup. a NULL path is efault, not a crash.

; openat reads: o_rdonly is 0, the fd hands over the same bytes the love door
; slurps, and close by number retires it
(: at (- 0 100)
   fd (syscall "openat" at "lib/dns.l" 0 0)
   b (cask 16)
   n (syscall "read" fd b 16 0)
   c (syscall "close" fd 0 0 0)
   p (open "lib/dns.l" "r") s (slurp p) _ (close p)
 (assert (<= 0 fd) (= n 16) (= (snip b 0 16) (snip s 0 16)) (= 0 c)))

; openat refusals, each with its own reason -- absence, writing a directory,
; the rdwr door the ramfs does not have, a stranger dirfd, and the NULL path
(: at (- 0 100)
   fd (syscall "openat" 5 "/lib/dns.l" 0 0)
   _ (syscall "close" fd 0 0 0)
 (assert (= -2 (syscall "openat" at "lib/no-such" 0 0))
         (= -21 (syscall "openat" at "lib" 1 0))
         (= -22 (syscall "openat" at "lib/dns.l" 2 0))
         (= -95 (syscall "openat" 5 "lib/dns.l" 0 0))
         (<= 0 fd)
         (= -14 (syscall "openat" at 0 0 0))))

; openat creates (O_WRONLY|O_CREAT = 65), append (O_WRONLY|O_APPEND = 1025)
; extends, and unlinkat takes the file back out
(: at (- 0 100)
   fd (syscall "openat" at "lib/sys-t1" 65 438)
   _ (syscall "write" fd "half" 4 0)
   _ (syscall "close" fd 0 0 0)
   fa (syscall "openat" at "lib/sys-t1" 1025 0)
   _ (syscall "write" fa "moon" 4 0)
   _ (syscall "close" fa 0 0 0)
   p (open "lib/sys-t1" "r") s (slurp p) _ (close p)
   u (syscall "unlinkat" at "lib/sys-t1" 0 0)
   g (syscall "openat" at "lib/sys-t1" 0 0)
 (assert (= s "halfmoon") (= 0 u) (= -2 g)))

; newfstatat fills the kernel's own struct stat: size at 48 and mtime at 88
; land on both arches; st_mode moves (16 aarch64, 24 x86_64) but the other
; arch's word there carries no type bits, so the union picks the true one
(: at (- 0 100)
   ; + is binary: (+ a b c) applies the sum -- these chain infix instead
   (le4 v i) ((v i) + ((v (+ i 1)) << 8) + ((v (+ i 2)) << 16) + ((v (+ i 3)) << 24))
   (le8 v i) ((le4 v i) + ((le4 v (+ i 4)) << 32))
   b (cask 144)
   r (syscall "newfstatat" at "lib/dns.l" b 0)
   v (string b)                                 ; a string applies as byte index
   st (stat "lib/dns.l")
   md (| (& 61440 (le4 v 16)) (& 61440 (le4 v 24)))
   ms (+ (* 1000 (le8 v 88)) (// (le4 v 96) 1000000))
   d (cask 144)
   rd (syscall "newfstatat" at "lib" d 0)
   w (string d)
   dd (| (& 61440 (le4 w 16)) (& 61440 (le4 w 24)))
 (assert (= 0 r) (= <st (le8 v 48)) (= 32768 md) (= <>st ms)
         (= 0 rd) (= 16384 dd)
         (= -2 (syscall "newfstatat" at "lib/no-such" b 0))))

; mkdirat mints a directory, EEXIST the second time; unlinkat wants the
; at_removedir bit (512) to take one, refuses a full one enotempty, and
; without the bit a directory is eisdir
(: at (- 0 100)
   a1 (syscall "mkdirat" at "lib/sys-d" 493 0)
   a2 (syscall "mkdirat" at "lib/sys-d" 493 0)
   fd (syscall "openat" at "lib/sys-d/f" 65 438)
   _ (syscall "close" fd 0 0 0)
   nf (syscall "unlinkat" at "lib/sys-d" 512 0)
   wf (syscall "unlinkat" at "lib/sys-d" 0 0)
   _ (syscall "unlinkat" at "lib/sys-d/f" 0 0)
   ok (syscall "unlinkat" at "lib/sys-d" 512 0)
   gone (syscall "newfstatat" at "lib/sys-d" (cask 144) 0)
 (assert (= 0 a1) (= -17 a2) (= -39 nf) (= -21 wf) (= 0 ok) (= -2 gone)))

; renameat moves a file whole -- the old name gone, the bytes under the new --
; and a missing source says so
(: at (- 0 100)
   fd (syscall "openat" at "lib/sys-a" 65 438)
   _ (syscall "write" fd "ren" 3 0)
   _ (syscall "close" fd 0 0 0)
   r (syscall "renameat" at "lib/sys-a" at "lib/sys-b")
   old (syscall "openat" at "lib/sys-a" 0 0)
   p (open "lib/sys-b" "r") s (slurp p) _ (close p)
   _ (syscall "unlinkat" at "lib/sys-b" 0 0)
   miss (syscall "renameat" at "lib/sys-a" at "lib/sys-b")
 (assert (= 0 r) (= -2 old) (= s "ren") (= -2 miss)))

; chdir moves the one seat and getcwd spells it back, length NUL-counted; a
; file refuses enotdir, absence ENOENT, a tight buffer erange -- and the seat
; goes home after, because every other test spells relative paths
(: b (cask 64)
   nd (syscall "chdir" "lib/dns.l" 0 0 0)
   miss (syscall "chdir" "lib/no-such" 0 0 0)
   r (syscall "chdir" "lib" 0 0 0)
   n (syscall "getcwd" b 64 0 0)
   seat (snip b 0 4)                            ; read the corner before the next fill
   tight (syscall "getcwd" (cask 64) 2 0 0)
   home (syscall "chdir" "/" 0 0 0)
   hn (syscall "getcwd" b 64 0 0)
 (assert (= -20 nd) (= -2 miss) (= 0 r) (= n 5) (= "/lib" seat)
         (= -34 tight) (= 0 home) (= hn 2) (= "/" (snip b 0 1))
         (= -14 (syscall "chdir" 0 0 0 0)) (= -14 (syscall "getcwd" 0 64 0 0))))

; fchmodat wears the new bits where the love stat reads them, and puts the
; old ones back
(: at (- 0 100)
   was (stat "lib/dns.l")
   r (syscall "fchmodat" at "lib/dns.l" 384 0)
   now (stat "lib/dns.l")
   _ (syscall "fchmodat" at "lib/dns.l" (& 4095 <>>was) 0)
   back (stat "lib/dns.l")
 (assert (= 0 r) (= 384 (& 4095 <>>now)) (= <>>was <>>back)
         (= -2 (syscall "fchmodat" at "lib/no-such" 384 0))))

; utimensat: the cask spells two timespecs and the second is the mtime -- an
; explicit 1700000000s lands whole; utime_omit (nsec 2^30-2) moves nothing;
; NULL says now, which is also what puts the date back in the living present
(: at (- 0 100)
   ts (cask [0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0
             0 241 83 101 0 0 0 0  0 0 0 0 0 0 0 0])
   r (syscall "utimensat" at "lib/dns.l" ts 0)
   md (stat "lib/dns.l")
   om (cask [0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0
             9 9 9 9 0 0 0 0  254 255 255 63 0 0 0 0])
   ro (syscall "utimensat" at "lib/dns.l" om 0)
   still (stat "lib/dns.l")
   t0 (clock 0)
   rn (syscall "utimensat" at "lib/dns.l" 0 0)
   nw (stat "lib/dns.l")
   t1 (clock 0)
 (assert (= 0 r) (= 1700000000000 <>md)
         (= 0 ro) (= 1700000000000 <>still)
         (= 0 rn) (<= t0 <>nw t1)
         (= -2 (syscall "utimensat" at "lib/no-such" 0 0))))

; --- the fd family -----------------------------------------------------------

; pipe2 fills the caller's int pair, and the ends behave as the rows they are;
; a flag word carries nothing here (nothing execs), and a NULL pair is efault
(: b (cask 8)
   r (syscall "pipe2" b 0 0 0)
   v (string b)
   rfd ((v 0) + ((v 1) << 8) + ((v 2) << 16) + ((v 3) << 24))
   wfd ((v 4) + ((v 5) << 8) + ((v 6) << 16) + ((v 7) << 24))
   n (syscall "write" wfd "fdfam" 5 0)
   d (cask 8)
   m (syscall "read" rfd d 8 0)
   _ (syscall "close" wfd 0 0 0)
   _ (syscall "close" rfd 0 0 0)
 (assert (= 0 r) (= n 5) (= m 5) (= "fdfam" (snip d 0 5))
         (= -22 (syscall "pipe2" b 1 0 0))
         (= -14 (syscall "pipe2" 0 0 0 0))))

; dup3 lands the alias in the named slot; a write through either twin reaches
; the one queue. src == dst is dup3's own refusal, a dead src EBADF
(: pp (pipe 0) rfd (cap pp) wfd (cup pp)
   r (syscall "dup3" wfd 40 0 0)
   _ (syscall "write" 40 "one" 3 0)
   _ (syscall "write" wfd "two" 3 0)
   _ (syscall "close" 40 0 0 0)
   _ (syscall "close" wfd 0 0 0)
   d (cask 16)
   n (syscall "read" rfd d 16 0)
   _ (syscall "close" rfd 0 0 0)
 (assert (= 40 r) (= n 6) (= "onetwo" (snip d 0 6))
         (= -22 (syscall "dup3" rfd rfd 0 0))
         (= -9 (syscall "dup3" 4096 41 0 0))))

; fcntl: F_DUPFD takes the lowest row at or past its floor; the flag words
; answer 0 for a live row (this seat keeps none), and a stranger cmd refuses
(: pp (pipe 0) rfd (cap pp) wfd (cup pp)
   d (syscall "fcntl" rfd 0 30 0)
   g0 (syscall "fcntl" rfd 1 0 0)
   bad (syscall "fcntl" rfd 99 0 0)
   _ (syscall "close" d 0 0 0)
   _ (syscall "close" rfd 0 0 0) _ (syscall "close" wfd 0 0 0)
 (assert (<= 30 d) (= 0 g0) (= -22 bad)
         (= -9 (syscall "fcntl" 4096 1 0 0))))

; fstat answers the row's kind: a ramfs fd its entry, a pipe end a fifo, a
; directory row its tree, and a dead row EBADF
(: at (- 0 100)
   (le4 s i) ((s i) + ((s (+ i 1)) << 8) + ((s (+ i 2)) << 16) + ((s (+ i 3)) << 24))
   fd (openfd "lib/fat.l" 0)
   b (cask 144)
   r (syscall "fstat" fd b 0 0)
   v (string b)
   st (stat "lib/fat.l")
   md (| (& 61440 (le4 v 16)) (& 61440 (le4 v 24)))
   sz ((le4 v 48) + ((le4 v 52) << 32))
   _ (fdclose fd)
   pp (pipe 0) rfd (cap pp) wfd (cup pp)
   p2 (cask 144)
   pr (syscall "fstat" rfd p2 0 0)
   w2 (string p2)
   pm (| (& 61440 (le4 w2 16)) (& 61440 (le4 w2 24)))
   _ (fdclose rfd) _ (fdclose wfd)
   dfd (syscall "openat" at "lib" 0 0)
   d3 (cask 144)
   dr (syscall "fstat" dfd d3 0 0)
   w3 (string d3)
   dm (| (& 61440 (le4 w3 16)) (& 61440 (le4 w3 24)))
   _ (syscall "close" dfd 0 0 0)
 (assert (= 0 r) (= 32768 md) (= <st sz)
         (= 0 pr) (= 4096 pm)
         (= 0 dr) (= 16384 dm)
         (= -9 (syscall "fstat" 4096 b 0 0))))

; a directory opens read-only (POSIX's own rule) as a dents row: read(2) on it
; is eisdir, and getdents64 walks exactly the names readdir answers -- each
; record 8-aligned, reclen at 16, the name behind 19 with its NUL. the cursor
; is the row's, so a second call says done, and a buffer too small for even
; one record refuses
(: at (- 0 100)
   fd (syscall "openat" at "lib" 0 0)
   b (cask 4096)
   n (syscall "getdents64" fd b 4096 0)
   v (string b)
   (le2 s i) ((s i) + ((s (+ i 1)) << 8))
   (walk o k) (? (<= n o) k (walk (+ o (le2 v (+ o 16))) (+ k 1)))
   z (syscall "getdents64" fd b 4096 0)
   rr (syscall "read" fd (cask 8) 8 0)
   _ (syscall "close" fd 0 0 0)
   f2 (syscall "openat" at "lib" 0 0)
   tiny (syscall "getdents64" f2 (cask 8) 8 0)
   _ (syscall "close" f2 0 0 0)
 (assert (<= 0 fd) (< 0 n) (= (walk 0 0) (tally (readdir "lib")))
         (= 0 z) (= -21 rr) (= -22 tiny)
         (= -20 (syscall "getdents64" 1 b 4096 0))))

; getpid: inle is one process and love tasks are its threads, so the syscall
; answers the machine's constant -- the task pid stays (getpid _)'s, a nif
; with g, which is where that answer lives
(assert (= 1 (syscall "getpid" 0 0 0 0)))

; clock_gettime: the wall clock only, a timespec whose seconds agree with
; (clock 0)'s scale -- ai_clock is one body over this arm now, so the law is
; that both readings sit within a beat of each other
(: b (cask 16)
   r (syscall "clock_gettime" 0 b 0 0)
   v (string b)
   (le8 s i o) (? (= 8 o) 0 (((s (+ i o)) << (8 * o)) + (le8 s i (+ o 1))))
   sec (le8 v 0 0)
   ms (clock 0)
 (assert (= 0 r) (<= sec (// ms 1000) (+ sec 1))
         (= -22 (syscall "clock_gettime" 1 b 0 0))
         (= -14 (syscall "clock_gettime" 0 0 0 0))))
