; --- lush 🐚 job control -- the shell state + the v3 job machinery ---
; every part rides module 'lush (the wrap): (use 'lush) assembles them via
; lib/lush.l, and the catted bin and the distro's /lib/sh.l register the same
; module in cat order (job eval line main) -- each part a body-less `:` binding
; into the module, so later parts read earlier ones. the shell story is main.l's.
;
; v3 (doc/misc/posix.md step 4): JOB CONTROL. the shell IGNORES the tty's job
; signals (installed in (sh _), main.l; spawn's child side resets them to
; default), so ^C/^Z reach only the foreground command. a ^Z stop comes back
; from `wait` as 256+sig (above every exit status) and the job parks in
; `sh-jobs`: n -> (pids state cmd), state 'stopped | 'running. builtins:
; jobs / fg / bg; a trailing & runs the pipeline unwaited as a 'running job.
; signal numbers are the Linux/x86 charms (the repo's home ABI; the BSDs
; take them through nolibc's permutation). also here: the shared floor
; (sh-rev sh-nth sh-int sh-unwords) and the shell state (sh-jobs sh-status sh-tty).

(module 'lush
(: (sh-rev l) (foldl (\ a x (x >< a)) () l)
   (sh-nth l i) (? (i = 0) <l (sh-nth >l (i - 1)))

   sig-int 2 sig-quit 3 sig-tstp 20 sig-cont 18 sig-ttou 22 sig-ttin 21
   sh-jobs (tablet 0)
   sh-njob {0}
   sh-tty {0}                                  ; 1 = interactive: jobs get their own pgrp + the tty
   sh-login {0}                                ; 1 = a login shell (--login, or a -argv0 seat): profiles read
   ; interactive spawns lead a fresh process group (pg 0) and the first stage takes
   ; the terminal (fg 1) -- a job in its OWN group is what makes ^Z real (a stop to
   ; an orphaned group is discarded, and the shell's group is orphaned under a
   ; nested session). the cooked/non-tty lane keeps the legacy in-shell pgrp (-1).
   (sh-pgm _) (? (sh-tty ()) 0 (0 - 1))
   (sh-fgm _) (? (sh-tty ()) 1 0)
   sh-status {0}                               ; $?: the last foreground status
   sh-substat {0}                              ; the last $( )'s status -- ambient for the
                                               ; same reason sh-noe is: word.l's expansion
                                               ; sits between sh-cmdsub and the reader
   sh-curline {0}                              ; the source line under eval (job display)
   sh-vars (tablet 0)                          ; shell variables (name -> value; export promotes)
   sh-funcs (tablet 0)                         ; functions (name -> the PLANNED body)
   ; --- trap: the action for a condition, and the door it leaves through -------
   ; sh-traps holds CONDITION -> action text. sh-runtrap is the evaluator, pinned
   ; by eval.l at load the way word.l's sh-subeval is -- job compiles first, and
   ; the action is shell source that only eval can read.
   ; ⚠ ONCE. sh-trapped guards the EXIT action against an `exit` inside it, which
   ; would otherwise re-enter sh-leave and run the cleanup a second time.
   ; EXIT leaves through here; a signal condition rides the sigfd lane below.
   sh-traps (tablet 0)
   sh-runtrap {0}
   sh-trapped {0}
   ; --- the signal lane: a trapped condition, DELIVERED ------------------------
   ; sigfd blocks the signals it watches and queues them to an fd, so a caught INT
   ; stops being a death and becomes a datum to read.
   ; ⚠ ARMED ONLY WHILE A SIGNAL TRAP EXISTS. With none -- every script in this
   ; tree but two -- the shell blocks nothing and this lane is one () test on the
   ; way past, so the ordinary command path is the one it always was.
   ; ⚠ AND DRAINED, NEVER PARKED ON. `wait` is a 1ms POLL that yields (read its
   ; nif), so the shell is never asleep inside a syscall and a non-blocking
   ; sigtake between polls sees the signal without a second wait set to merge.
   ; That hands us POSIX's rule for free -- the action runs when the foreground
   ; command COMPLETES -- and ^C reaches the child too, whose own mask sig_dfl_job
   ; cleared, so the wait it is inside ends on its own.
   ; ⚠ THE NUMBERS ARE LINUX'S, like sig-int and sig-tstp above. The low ones are
   ; every kernel's; USR1 USR2 CHLD TSTP are not, and want the ISA/OS table the
   ; day this rides a BSD.
   sh-sigport {0}                              ; the signalfd port, or 0
   sh-signums '(("HUP" 1) ("INT" 2) ("QUIT" 3) ("ABRT" 6) ("USR1" 10) ("USR2" 12)
                ("PIPE" 13) ("ALRM" 14) ("TERM" 15) ("CHLD" 17) ("CONT" 18) ("TSTP" 20))
   (sh-signum nm)
     ((: (g l) (? !(two? l) 0
                  (: e <l (? (<e = nm) <>e (g >l)))))
      sh-signums)
   (sh-signame n)
     ((: (g l) (? !(two? l) ()
                  (: e <l (? (<>e = n) <e (g >l)))))
      sh-signums)
   ; every trapped condition that is a signal -- EXIT leaves by its own door
   (sh-sigconds _) (filter (\ k !(k = "EXIT")) (keys sh-traps))
   ; ⚠ A SIGNAL IGNORED ON ENTRY CANNOT BE TRAPPED (POSIX), and no error is owed
   ; for trying. The everyday way a script gets one is the caller's `&`: a shell
   ; backgrounded without job control starts with INT and QUIT set to SIG_IGN, and
   ; a `trap .. INT` in it must stay off -- the caller has already said this job
   ; does not answer ^C. Snapshotted ONCE at sh-main, before the interactive entry
   ; sets ignores of its own, or the shell would read its own hand as the caller's.
   sh-igno {0}
   (sh-snapign _)
     (pin sh-igno ()
          (filter (\ nm (: n (sh-signum nm) (n && (sigign? n))))
                  (map cap sh-signums)))
   (sh-ignored? nm) (: l (sh-igno ()) (? (two? l) (any (\ k (k = nm)) l) 0))
   ; ⚠ CLEAR, THEN ARM. sigfd's sigprocmask is a SIG_BLOCK and accumulates, and
   ; closing the fd does not unblock: after `trap - INT` the signal would stay
   ; blocked with nothing left reading it, which is a shell that cannot be
   ; interrupted at all. So the mask is emptied first and the live set re-blocked.
   (sh-rearm _)
     (: cs (sh-sigconds 0)
        ns (filter (\ n !(n = 0)) (map sh-signum cs))
        _ (? (sh-sigport ()) (close (sh-sigport ())) 0)
        _ (pin sh-sigport () 0)
        _ (sigclear 0)
        _ (? (two? ns) (: p (sigfd ns) (? p (pin sh-sigport () p) 0)) 0)
        0)
   (sh-disarm _)                               ; a forked child inherits neither
     (: _ (? (sh-sigport ()) (close (sh-sigport ())) 0)
        _ (pin sh-sigport () 0) _ (sigclear 0) 0)
   ; read every queued signal and run what each one traps. the action's own status
   ; does not become $? -- the command's does, exactly as at the EXIT door.
   (sh-fire-sig n)
     (: nm (sh-signame n)
        a (? (string? nm) (peep sh-traps nm "") "")
        (? (tally a)
           (: st (sh-status ()) _ ((sh-runtrap ()) a) _ (pin sh-status () st) 0)
           0))
   (sh-sigdrain _)
     (: p (sh-sigport ())
        (? !p 0
           ((: (g _) (: ev (sigtake p)
                        (? !(two? ev) 0
                           (: _ (sh-fire-sig (cap ev)) (g 0)))))
            0)))
   ; ⚠ THE STATUS IS THE ONE THE SHELL WAS LEAVING WITH. POSIX says the action
   ; runs with $? as it stood, and the shell still exits on that -- `false` under
   ; an EXIT trap is still a 1, however well the cleanup went. Only an explicit
   ; `exit` inside the action changes it, and that one leaves through sh-leave.
   (sh-fire-exit _)
     (: a (peep sh-traps "EXIT" "")
        (? ((0 = (sh-trapped ())) && (tally a))
           (: st (sh-status ())
              _ (pin sh-trapped () 1)
              _ ((sh-runtrap ()) a)
              _ (pin sh-status () st) 0)
           0))
   sh-nofn (mint 0)                            ; that plan is a closure, so peep names its absence
   sh-pos {0}                                  ; the positionals, a list of strings
   sh-arg0 {0}                                 ; $0 (the script path, or the shell)
   sh-eflag {0} sh-uflag {0} sh-xflag {0}      ; set -e / -u / -x
   sh-lastbg {0}                               ; $!: the last background pid
   ; ⚠ AMBIENT on purpose, never a parameter: the guard is a dynamic extent every
   ; re-entry reads -- a function body, `eval`, a dot-file, and the forks under a
   ; pipeline stage and a $( ) body (those two take it by memory copy). a plan
   ; parameter would have to cross word.l's expansion. test/host/sh.l pins it.
   sh-noe {0}                                  ; errexit guard depth (conds, && || arms)
   ; ⚠ ambient for the same reason: the forkless $( ) captures this shell is
   ; INSIDE, innermost first, each a (read-end . drainer) pair -- empty when none,
   ; and that emptiness is the question `exit` asks (inside one it may not reach
   ; `quit`: it folds to ('xit . st), which nothing catches but the capture).
   ; ⚠ A FORK UNDER ONE INHERITS THE DRAINER TASK, and a child draining its
   ; parent's capture eats the bytes and then blocks the whole child on a pipe
   ; only its parent can end. so a fork shuts each one down first thing
   ; (sh-dropcaps), and a `( )` under a capture does not fork at all (sh-subsh).
   sh-subfds {0}
   sh-online {0}                               ; nesting depth of sh-oneline: a line run HERE for a
                                               ; caller in this image, so `exit` ends the line not us
   ; ⚠ THE EXIT DOORS, and there are three: `exit`, errexit (sh-echk) and set -u on
   ; an unset name. all three END THE SHELL -- and which shell that is depends on
   ; where we stand. innermost first: under a $( ) capture it is the capture, and the
   ; pair is what the plan machinery already rides out; under an sh-oneline it is the
   ; LINE, and the caller lives on in this image -- by SCARE, because set -u fires
   ; from a word expansion whose value is a string, with no pair to hand back.
   ; otherwise it is the process, as it always was. one rule said once, so a fourth
   ; door cannot drift away from it.
   (sh-leave st)
     (? (sh-subfds ()) ('xit >< st)
        (0 < (sh-online ())) (scare 'sh-exit st)
        (: _ (sh-fire-exit 0) (quit st)))
   sh-retf {0}                                 ; a top-level `return` landed: sh-lines stops its file (dot-return)

   ; --- read one line (cooked): bytes until \n; 'eof at immediate EOF. here on
   ; the shared floor: eval's `.`/read builtins and the loops all drink from it
   (sh-getline port) (: c (see port) (? (c < 0) 'eof (c = 10) "" ((string c) + (sh-rest port))))
   (sh-rest port)    (: c (see port) (? (c < 0) ""   (c = 10) "" ((string c) + (sh-rest port))))

   (sh-unwords ws) (foldl (\ a w (? (tally a) ((a + " ") + w) w)) "" ws)
   (sh-addjob pids state cmd)
     (: n (1 + (sh-njob ())) _ (pin sh-njob () n) _ (pin sh-jobs n [pids state cmd]) n)
   (sh-jobline n j)
     ("[" + ((show n) + ("] " + ((show (sh-nth j 1)) + ("  " + ((sh-nth j 2) + "\n"))))))
   (sh-stopped? sts) (foldl (\ a x (? a a (255 < x))) 0 sts)
   ; wait a foreground job; a stop registers + reports it instead of hanging the prompt
   (sh-waitfg pids cmd)
     (: sts (map (\ p (wait p)) pids)
        _   (sh-sigdrain 0)                            ; POSIX: after the command, not during
        _   (? (sh-tty ()) (ttyfg 0) 0)                ; the shell takes the terminal back
        _   (pin sh-status ()                          ; $?: the last stage; a stop folds to 128+sig
             (: st (last sts) (? (charm? st && (255 < st)) (st - 128) st)))
        (? (sh-stopped? sts)
           (: n (sh-addjob pids 'stopped cmd)
              _ (puts (sh-jobline n (sh-jobs n))) 0)
           0))
   ; background bookkeeping: drain glean at each prompt; a reaped pid leaves its job,
   ; an emptied job prints done and goes
   (sh-unpid l p) (? !l () (? (<l = p) >l (<l >< sh-unpid >l p)))
   (sh-bgnote pid)
     (each (keys sh-jobs)
       (\ n (: j (sh-jobs n)
               (? !j 0
                  (: ps (sh-unpid (sh-nth j 0) pid)
                     (? (ps = (sh-nth j 0)) 0
                        !ps (: _ (? (sh-tty ())          ; notices are the tty's; scripts stay quiet
                                    (puts ("[" + ((show n) + ("] done  " + ((sh-nth j 2) + "\n"))))) 0)
                               (pull sh-jobs n 0))
                        (pin sh-jobs n [ps (sh-nth j 1) (sh-nth j 2)])))))))
   (sh-reapbg _)
     (: r (glean 0) (? (two? r) (: _ (sh-bgnote <r) (sh-reapbg 0)) 0))
   ; job addressing: "fg 2" by number, bare "fg" the latest
   ; string membership: member? tests IDENTITY (uninterned strings never hit)
   (sh-smem? x l) (? !(two? l) 0 (? (x = <l) 1 (sh-smem? x >l)))
   (sh-int s) (sh-intl s 0 0)
   (sh-intl s i a) (? (i = (tally s)) a (sh-intl s (i + 1) ((a * 10) + ((s i) - 48))))
   (sh-lastjob _) (foldl (\ a k (? (a < k) k a)) 0 (keys sh-jobs))
   (sh-jobn argv) (? >argv (sh-int <>argv) (sh-lastjob 0))
   (sh-fg n)
     (: j (sh-jobs n)
        (? !j (: _ (puts "fg: no such job\n") 0)
           (: pids (sh-nth j 0) cmd (sh-nth j 2)
              _ (pull sh-jobs n 0)
              _ (? (sh-tty ()) (ttyfg <pids) 0)        ; the job's group gets the terminal first
              _ (each pids (\ p (still p sig-cont)))
              (sh-waitfg pids cmd))))
   (sh-bg n)
     (: j (sh-jobs n)
        (? !j (: _ (puts "bg: no such job\n") 0)
           (: _ (each (sh-nth j 0) (\ p (still p sig-cont)))
              _ (pin sh-jobs n [(sh-nth j 0) 'running (sh-nth j 2)])
              _ (puts (sh-jobline n (sh-jobs n)))
              0))))
) ; module 'lush
