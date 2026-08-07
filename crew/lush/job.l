; --- lush 🐚 job control -- the shell state + the v3 job machinery ---
; NO BRACKETS HERE: the runtime supplies the layer. (use 'lush) assembles the
; parts via lib/lush.l; the catted bin and the distro's /lib/sh.l read them
; bare, in cat order (job eval line main) -- each part a body-less `:` leaking
; its binds, so later parts read earlier ones. the shell story lives in main.l.
;
; v3 (doc/posix.md step 4): JOB CONTROL. the shell IGNORES the tty's job
; signals (installed in (sh _), main.l; spawn's child side resets them to
; default), so ^C/^Z reach only the foreground command. a ^Z stop comes back
; from `wait` as 256+sig (above every exit status) and the job parks in
; `sh-jobs`: n -> (pids state cmd), state 'stopped | 'running. builtins:
; jobs / fg / bg; a trailing & runs the pipeline unwaited as a 'running job.
; signal numbers are the Linux/x86 charms (the repo's home ABI; signalfd is
; Linux-only already). also here: the shared floor (sh-rev sh-nth sh-int
; sh-unwords) and the shell state (sh-jobs sh-status sh-tty).

(: (sh-rev l) (foldl (\ a x (link x a)) () l)
   (sh-nth l i) (? (= i 0) (cap l) (sh-nth (cup l) (- i 1)))

   sig-int 2 sig-quit 3 sig-tstp 20 sig-cont 18 sig-ttou 22 sig-ttin 21
   sh-jobs (tablet 0)
   sh-njob #0
   sh-tty #0                                   ; 1 = interactive: jobs get their own pgrp + the tty
   sh-login #0                                 ; 1 = a login shell (--login, or a -argv0 seat): profiles read
   ; interactive spawns lead a fresh process group (pg 0) and the first stage takes
   ; the terminal (fg 1) -- a job in its OWN group is what makes ^Z real (a stop to
   ; an orphaned group is discarded, and the shell's group is orphaned under a
   ; nested session). the cooked/non-tty lane keeps the legacy in-shell pgrp (-1).
   (sh-pgm _) (? (sh-tty ()) 0 (0 - 1))
   (sh-fgm _) (? (sh-tty ()) 1 0)
   sh-status #0                                ; $?: the last foreground status
   sh-substat #0                               ; the last $( )'s status -- ambient for the
                                               ; same reason sh-noe is: word.l's expansion
                                               ; sits between sh-cmdsub and the reader
   sh-curline #0                               ; the source line under eval (job display)
   sh-vars (tablet 0)                          ; shell variables (name -> value; export promotes)
   sh-funcs (tablet 0)                         ; functions (name -> the PLANNED body)
   sh-nofn (mint 0)                            ; that plan is a closure, so peep names its absence
   sh-pos #0                                   ; the positionals, a list of strings
   sh-arg0 #0                                  ; $0 (the script path, or the shell)
   sh-eflag #0 sh-uflag #0 sh-xflag #0         ; set -e / -u / -x
   sh-lastbg #0                                ; $!: the last background pid
   ; ⚠ AMBIENT on purpose, never a parameter: the guard is a dynamic extent every
   ; re-entry reads -- a function body, `eval`, a dot-file, and the forks under a
   ; pipeline stage and a $( ) body (those two take it by memory copy). a plan
   ; parameter would have to cross word.l's expansion. test/host/sh.l pins it.
   sh-noe #0                                   ; errexit guard depth (conds, && || arms)
   sh-retf #0                                  ; a top-level `return` landed: sh-lines stops its file (dot-return)

   ; --- read one line (cooked): bytes until \n; 'eof at immediate EOF. here on
   ; the shared floor: eval's `.`/read builtins and the loops all drink from it
   (sh-getline port) (: c (see port) (? (< c 0) 'eof (= c 10) "" (+ (+ "" c) (sh-rest port))))
   (sh-rest port)    (: c (see port) (? (< c 0) ""   (= c 10) "" (+ (+ "" c) (sh-rest port))))

   (sh-unwords ws) (foldl (\ a w (? (tally a) (+ (+ a " ") w) w)) "" ws)
   (sh-addjob pids state cmd)
     (: n (+ 1 (sh-njob ())) _ (pin sh-njob () n) _ (pin sh-jobs n (L pids state cmd)) n)
   (sh-jobline n j)
     (+ "[" (+ (show n) (+ "] " (+ (show (sh-nth j 1)) (+ "  " (+ (sh-nth j 2) "\n"))))))
   (sh-stopped? sts) (foldl (\ a x (? a a (< 255 x))) 0 sts)
   ; wait a foreground job; a stop registers + reports it instead of hanging the prompt
   (sh-waitfg pids cmd)
     (: sts (map (\ p (wait p)) pids)
        _   (? (sh-tty ()) (ttyfg 0) 0)                ; the shell takes the terminal back
        _   (pin sh-status ()                          ; $?: the last stage; a stop folds to 128+sig
             (: st (last sts) (? (&& (charm? st) (< 255 st)) (- st 128) st)))
        (? (sh-stopped? sts)
           (: n (sh-addjob pids 'stopped cmd)
              _ (puts (sh-jobline n (sh-jobs n))) 0)
           0))
   ; background bookkeeping: drain glean at each prompt; a reaped pid leaves its job,
   ; an emptied job prints done and goes
   (sh-unpid l p) (? !l () (? (= (cap l) p) (cup l) (link (cap l) (sh-unpid (cup l) p))))
   (sh-bgnote pid)
     (each (keys sh-jobs)
       (\ n (: j (sh-jobs n)
               (? !j 0
                  (: ps (sh-unpid (sh-nth j 0) pid)
                     (? (= ps (sh-nth j 0)) 0
                        !ps (: _ (? (sh-tty ())          ; notices are the tty's; scripts stay quiet
                                    (puts (+ "[" (+ (show n) (+ "] done  " (+ (sh-nth j 2) "\n"))))) 0)
                               (pull sh-jobs n 0))
                        (pin sh-jobs n (L ps (sh-nth j 1) (sh-nth j 2)))))))))
   (sh-reapbg _)
     (: r (glean 0) (? (two? r) (: _ (sh-bgnote (cap r)) (sh-reapbg 0)) 0))
   ; job addressing: "fg 2" by number, bare "fg" the latest
   ; string membership: member? tests IDENTITY (uninterned strings never hit)
   (sh-smem? x l) (? (! (two? l)) 0 (? (= x (cap l)) 1 (sh-smem? x (cup l))))
   (sh-int s) (sh-intl s 0 0)
   (sh-intl s i a) (? (= i (tally s)) a (sh-intl s (+ i 1) (+ (* a 10) (- (s i) 48))))
   (sh-lastjob _) (foldl (\ a k (? (< a k) k a)) 0 (keys sh-jobs))
   (sh-jobn argv) (? (cup argv) (sh-int (caup argv)) (sh-lastjob 0))
   (sh-fg n)
     (: j (sh-jobs n)
        (? !j (: _ (puts "fg: no such job\n") 0)
           (: pids (sh-nth j 0) cmd (sh-nth j 2)
              _ (pull sh-jobs n 0)
              _ (? (sh-tty ()) (ttyfg (cap pids)) 0)   ; the job's group gets the terminal first
              _ (each pids (\ p (still p sig-cont)))
              (sh-waitfg pids cmd))))
   (sh-bg n)
     (: j (sh-jobs n)
        (? !j (: _ (puts "bg: no such job\n") 0)
           (: _ (each (sh-nth j 0) (\ p (still p sig-cont)))
              _ (pin sh-jobs n (L (sh-nth j 0) 'running (sh-nth j 2)))
              _ (puts (sh-jobline n (sh-jobs n)))
              0))))
