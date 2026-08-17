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

   ; -- date: the wall clock. ⚠ UTC AND ONLY UTC -- there is no tz database in this tree,
   ; so localtime IS gmtime, the same call nolibc made and for the same reason. -u is
   ; taken and changes nothing; -d @SECONDS and -r FILE name a moment other than now,
   ; which is also the only thing that makes this tool gateable against GNU at all.
   ; +FORMAT, or GNU's C-locale default when none is given.
   udt-wd '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
   udt-wdl '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday")
   udt-mo '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
   udt-mol '("January" "February" "March" "April" "May" "June" "July" "August"
             "September" "October" "November" "December")
   (udatef fmt ms)
    (: s (// ms 1000)
       days (// s 86400) secs (% s 86400)
       ymd (ucivil days)
       Y <ymd M <>ymd D <>>ymd
       hh (// secs 3600) mm (// (% secs 3600) 60) ss (% secs 60)
       wd (% (days + 4) 7)
       yday ((days - (udays Y 1 1)) + 1)
       (p2 v) (uzpad 2 (show v))
       n (tally fmt) o (jug 0)
       (go k)
        (? (n <= k) (slurp o)
           (: c (fmt k)
              (? !((c = 37) && ((k + 1) < n)) (: _ (put o c) (go (k + 1)))
                 (: d (fmt (k + 1))
                    _ (? (d = 37) (put o 37)
                         (d = 89) (say o (show Y))
                         (d = 109) (say o (p2 M))
                         (d = 100) (say o (p2 D))
                         (d = 101) (say o (upad 2 (show D)))
                         (d = 72) (say o (p2 hh))
                         (d = 77) (say o (p2 mm))
                         (d = 83) (say o (p2 ss))
                         (d = 106) (say o (uzpad 3 (show yday)))
                         (d = 121) (say o (p2 (% Y 100)))
                         (d = 67) (say o (p2 (// Y 100)))
                         (d = 70) (say o ((uzpad 4 (show Y)) + "-" + (p2 M) + "-" + (p2 D)))
                         (d = 84) (say o ((p2 hh) + ":" + (p2 mm) + ":" + (p2 ss)))
                         (d = 68) (say o ((p2 M) + "/" + (p2 D) + "/" + (p2 (% Y 100))))
                         (d = 115) (say o (show s))
                         (d = 97) (say o (at udt-wd wd))
                         (d = 65) (say o (at udt-wdl wd))
                         ((d = 98) || (d = 104)) (say o (at udt-mo (M - 1)))
                         (d = 66) (say o (at udt-mol (M - 1)))
                         (d = 112) (say o (? (hh < 12) "AM" "PM"))
                         (d = 73) (say o (p2 (: h (% hh 12) (? h h 12))))
                         (d = 117) (say o (show (? wd wd 7)))
                         (d = 119) (say o (show wd))
                         (d = 90) (say o "UTC")
                         (d = 122) (say o "+0000")
                         (d = 110) (put o 10)
                         (d = 116) (put o 9)
                         (: _ (put o 37) (put o d)))
                    (go (k + 2))))))
       (go 0))
   (udt-when w)                        ; -d's @SECONDS, the one spelling this date parses
    (? ((0 < tally w) && (w 0 = 64)) (* 1000 (uatoi (snip w 1 (tally w))))
       (udie 1 ("date: cannot read the date '" + w + "' (only @SECONDS here)")))
   (date-main as)
    (: (fl a when fmt)
        (? !(two? a) `(when fmt)
           ((<a = "-u") || (<a = "--utc")) (fl >a when fmt)
           (<a = "-d") (? (two? >a) (fl >>a (udt-when <>a) fmt)
                          (udie 1 "date: -d wants a time"))
           (<a = "-r") (? (two? >a)
                          (: st (stat <>a)
                             (? (two? st) (fl >>a <>st fmt)
                                (udie 1 ("date: cannot stat '" + <>a + "'"))))
                          (udie 1 "date: -r wants a file"))
           ((0 < tally <a) && (<a 0 = 43)) (fl >a when (snip <a 1 (tally <a)))
           (udie 1 ("date: cannot set the clock (" + <a + ")")))
       p (fl as () ())
       ms (? (charm? <p) <p (clock 0))
       fmt (? (string? <>p) <>p "%a %b %e %H:%M:%S %Z %Y")
       _ (say out (udatef fmt ms)) _ (put out 10) _ (flush out) 0)

   ; -- id: who this process is. -u/-g/-G the ids, -n their names, and the bare face
   ; GNU's uid=/gid=/groups= line. ⚠ the supplementary groups are read out of
   ; /etc/group -- there is no getgroups here and no NSS anywhere, so a box whose groups
   ; live in a directory service will be told less than the truth. -r is taken and
   ; changes nothing: this love holds only the REAL ids, which is what -r asks for.
   (id-main as)
    (: (fl a t)
        (? !(two? a) t
           ((tally <a < 2) || !(<a 0 = 45)) (udie 1 ("id: unknown argument " + <a))
           (: w <a
              (cl k) (? (tally w <= k) (fl >a t)
                        (: c (w k)
                           (? (member? c '(117 103 71 110 114))
                              (: _ (pin t c 1) (cl (k + 1)))
                              (udie 1 ("id: unknown option -" + string c)))))
              (cl 1)))
       t (fl as (tablet 0))
       uid (getuid 0) gid (getgid 0)
       nm (uidname "/etc/passwd" uid)
       gs (ugroups nm gid)
       n? (peep t 110 0)
       (one g) ((show g) + "(" + (uidname "/etc/group" g) + ")")
       _ (? (peep t 117 0) (say out (? n? nm (show uid)))
            (peep t 103 0) (say out (? n? (uidname "/etc/group" gid) (show gid)))
            (peep t 71 0) (say out (ujoin (map (\ g (? n? (uidname "/etc/group" g) (show g))) gs)))
            (say out ((("uid=" + (show uid) + "(" + nm + ")") + (" gid=" + (one gid)))
                      + (" groups=" + (ujoinc 44 (map one gs))))))
       _ (put out 10) _ (flush out) 0)

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
