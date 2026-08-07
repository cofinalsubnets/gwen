; rung-4 laws (doc/inle.md): the pipe pair over one kernel-heap queue, its EOF
; protocol (a reader parks while a writer lives, reads the end at the last
; close), dup/dup2 as row aliases, and the spawn/wait shim over tasks -- a
; spawned tool's stdio seated under the folded ports. kernel corpus only
; (kernel.mk's kt), after the kore subset: the spawn laws drive echo-main and
; wc-main as processes. (their console output mid-corpus is the smoke, not a
; mess -- test/kernel/kore.l's precedent.)

; the pair, the bytes, the end
(: pp (pipe 0) rfd (cap pp) wfd (cup pp)
   wp (fdopen wfd) rp (fdopen rfd)
   _ (say wp "hello pipe") _ (close wp)         ; the run drains at the close; the last writer leaves
   s (slurp rp) _ (close rp)
 (assert (two? pp) (charm? rfd) (charm? wfd)
         (port? wp) (port? rp)
         (= s "hello pipe")))

; a parked reader wakes on the write, and ends on the close
(: pp (pipe 0) rp (fdopen (cap pp))
   p (twirl (\ _ (slurp rp)) 0)
   _ (rest 0)                                   ; let the reader park on the quiet pipe
   wp (fdopen (cup pp))
   _ (say wp "wake") _ (close wp)
   r (catch p) _ (close rp)
 (assert (= r "wake")))

; dup is a row alias: the end comes at the LAST writer's close, not the first
(: pp (pipe 0) rfd (cap pp) wfd (cup pp)
   w2 (dup wfd)
   wp (fdopen wfd)
   _ (say wp "one") _ (close wp)
   rp (fdopen rfd)
   p (twirl (\ _ (slurp rp)) 0)
   _ (rest 0)
   w2p (fdopen w2)
   _ (say w2p ", two") _ (close w2p)
   r (catch p) _ (close rp)
 (assert (charm? w2) (< 2 w2) (= r "one, two")))

; dup2 lands the alias IN the named slot, closing what sat there
(: pa (pipe 0) pb (pipe 0)
   e (dup2 (cup pa) (cup pb))                   ; b's write slot now feeds a
   wp (fdopen (cup pb)) _ (say wp "crossed") _ (close wp)
   _ (fdclose (cup pa))                         ; a's own write end steps out too
   sb (slurp (fdopen (cap pb)))                 ; b lost its one writer at the dup2
   sa (slurp (fdopen (cap pa)))
 (assert (! e) (= sb "") (= sa "crossed")))

; spawn maps argv onto a love main, the pid is the task pid, wait is catch;
; the corpus itself runs unseated in the main task
(: p (spawn (list "kore" "echo" "rung4"))
 (assert (charm? (getpid 0)) (= 0 (getpid 0))
         (charm? p) (< 0 p) (= 0 (wait p))))

; an unknown program still answers a pid; its exit is 127, the host's face
(: p (spawn (list "no-such-tool"))
 (assert (= 127 (wait p))))

; the seat: a stage's out rides the pipe while the parent keeps the console --
; the parent closes ITS end and still reads the child whole
(: pp (pipe 0) rfd (cap pp) wfd (cup pp)
   p (spawnmap (list "kore" "echo" "seated") (list (link 1 wfd)) (list rfd wfd) (- 0 1) 0)
   _ (fdclose wfd)
   s (slurp (fdopen rfd))
 (assert (= 0 (wait p)) (= s "seated\n")))

; two stages plumbed by hand -- echo | wc -l -- the parent reading the tail;
; the seat carries fd 0 as well as fd 1
(: pa (pipe 0) pb (pipe 0)
   p1 (spawnmap (list "kore" "echo" "a b") (list (link 1 (cup pa))) () (- 0 1) 0)
   p2 (spawnmap (list "kore" "wc" "-l")
                (list (link 0 (cap pa)) (link 1 (cup pb))) () (- 0 1) 0)
   _ (fdclose (cup pa)) _ (fdclose (cap pa)) _ (fdclose (cup pb))
   s (slurp (fdopen (cap pb)))
   r1 (wait p1) r2 (wait p2)
 (assert (= 0 r1) (= 0 r2) (= s "1\n")))
