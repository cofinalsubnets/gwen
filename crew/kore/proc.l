; crew/kore/proc.l -- the process tools of kore: env, sleep, kill, xargs.
; a toolbox file, no seat -- kore is its door; loads after core.l and rides the
; u-floor. no nifs of its own: environ/getenv/setenv (host/main.c + init.c),
; spawn/wait (init.c's posix lane -- spawn answers the pid or a NEGATIVE errno,
; a child that cannot exec _exit(127)s), still (pty.c's kill), rest (the core
; sleep, milliseconds). a body-less top-level `:` leaking the two pure helpers
; (udur, uwords -- lawed in law.l) and the mains.
(module 'kore
(:
   ; a duration text -> milliseconds | (): N or N.N, an optional s/m/h/d suffix
   ; (seconds the default). 0 is a real duration -- callers test charm?, not truth
   (udur s0)
    (: n0 (tally s0)
       lc (? (0 < n0) (s0 (n0 - 1)) ())
       sfx ((lc = 115) || (lc = 109) || (lc = 104) || (lc = 100))
       mul (? (lc = 109) 60000 (lc = 104) 3600000 (lc = 100) 86400000 1000)
       s (? sfx (snip s0 0 (n0 - 1)) s0)
       n (tally s)
       (dig? c) ((48 <= c) && (c <= 57))
       (ip i acc) (? ((i < n) && dig? (s i)) (ip (i + 1) ((* 10 acc) + (s i - 48))) `(acc i))
       r (ip 0 0)
       whole <r i <>r
       (? ((i = n) && (0 < n)) (* whole mul)
          ((i < n) && (s i = 46))
           (: (fp j acc sc) (? ((j < n) && dig? (s j))
                               (fp (j + 1) ((* 10 acc) + (s j - 48)) (* sc 10))
                               `(acc sc j))
              fr (fp (i + 1) 0 1)
              (? ((<>>fr = n) && ((0 < i) || ((i + 1) < n)))
                 ((* whole mul) + (// (* <fr mul) <>fr))
                 ()))
          ()))
   ; whitespace-split a text into words (the xargs face, quote-blind)
   (uwords t)
    (: n (tally t)
       (go i st acc)
        (? (n <= i) (rev (? (st < 0) acc (snip t st i >< acc)))
           (: c (t i)
              w ((c = 32) || (c = 9) || (c = 10) || (c = 13))
              (? w (go (i + 1) -1 (? (st < 0) acc (snip t st i >< acc)))
                 (go (i + 1) (? (st < 0) i st) acc))))
       (go 0 -1 ()))

   ; -- env: assignments, then print the world or run a command inside it --
   (env-main as)
    (: (eqat s) (: n (tally s)                    ; the first = past position 0, or ()
                   (go k) (? (n <= k) ()
                             (s k = 61) (? (0 < k) k ())
                             (go (k + 1)))
                   (go 1))
       (setp a) (? !(two? a) a
                   (: i (eqat <a)
                      (? i (: s <a
                              _ (setenv (snip s 0 i) (snip s (i + 1) (tally s)))
                              (setp >a))
                         a)))
       rest (setp as)
       (? (two? rest)
          (: p (spawn rest)
             (? (p < 0) (udie 127 ("env: cannot run " + <rest))
                (: w (wait p) _ (flush out) w)))
          (: (pr e) (? !(two? e) (: _ (flush out) 0)
                       (: _ (say out <e) _ (put out 10) (pr >e)))
             (pr (environ 0)))))

   ; -- sleep: the durations, summed --
   (sleep-main as)
    (? !(two? as) (udie 1 "sleep: missing operand")
       (: (tot a acc) (? !(two? a) acc
                         (: m (udur <a)
                            (? (charm? m) (tot >a (acc + m))
                               (udie 1 ("sleep: invalid time interval '" + <a + "'")))))
          t (tot as 0)
          _ (rest t)
          0))

   ; -- kill: a signal (default TERM) onto every named pid --
   (kill-main as)
    (: signames '(("HUP" 1) ("INT" 2) ("QUIT" 3) ("KILL" 9) ("USR1" 10)
                   ("USR2" 12) ("TERM" 15) ("CONT" 18) ("STOP" 19))
       (snl l b) (? !(two? l) ()
                    (<<l = b) <><l
                    (snl >l b))
       (alldig? s) (: n (tally s)
                      (go k) (? (n <= k) (0 < n)
                                ((48 <= s k) && (s k <= 57)) (go (k + 1))
                                ())
                      (go 0))
       (signum s) (: n (tally s)
                     (? ((n < 2) || !(s 0 = 45)) ()
                        (: b (snip s 1 n)
                           (? (alldig? b) (uatoi b) (snl signames b)))))
       sig (? (two? as) (signum <as) ())
       pids (? (charm? sig) >as as)
       sg (? (charm? sig) sig 15)
       _ (? (two? pids) () (udie 1 "usage: kill [-SIG] PID.."))
       (one p ok)
        (? !(alldig? p)
           (: _ (say err ("kill: invalid pid " + p)) _ (put err 10) 0)
           (: r (still (uatoi p) sg)
              (? !r ok
                 (: _ (say err ("kill: cannot kill " + p)) _ (put err 10) 0))))
       (go a ok) (? (two? a) (go >a (one <a ok))
                    (: _ (flush err) (? ok 0 1)))
       (go pids 1))

   ; -- xargs: stdin's words onto the command's tail (echo when none), -n N a
   ; batch at a time. exits 0 clean, 123 when a run failed, 127 when one
   ; could not exec. quote-blind: words split on whitespace alone --
   (xargs-main as)
    (: nflag (two? as && (<as = "-n"))
       _ (? (nflag && !(two? >as)) (udie 1 "usage: xargs [-n N] [CMD ARGS..]") ())
       nn (? nflag (uatoi <>as) ())
       _ (? (nflag && (nn < 1)) (udie 1 "xargs: invalid -n") ())
       rest (? nflag >>as as)
       cmd (? (two? rest) rest '("echo"))
       ws (uwords (slurp in))
       (chunks k l) (? !(two? l) () (take k l >< chunks k (drop k l)))
       runs (? !(two? ws) `(())                      ; no input still runs once, like GNU
               nn (chunks nn ws)
               `(ws))
       (go rs bad)
        (? !(two? rs)
           (: _ (flush out) bad)
           (: p (spawn (cmd + <rs))
              w (? (p < 0) 127 (wait p))
              (go >rs (? (w = 127) 127
                              (w && !(bad = 127)) 123
                              bad))))
       (go runs 0)))
) ; module 'kore
