; crew/kore/proc.l -- the process and world tools of kore: env, printenv, sleep,
; kill, xargs, date, id, whoami, groups.
; a toolbox file, no seat -- kore is its door; loads after core.l and rides the
; u-floor. no nifs of its own: environ/getenv/setenv (host/main.c + init.c),
; spawn/wait (init.c's posix lane -- spawn answers the pid or a NEGATIVE errno,
; a child that cannot exec _exit(127)s), still (pty.c's kill), rest (the core
; sleep, milliseconds). a body-less top-level `:` leaking one pure helper
; (udur -- lawed in law.l) and the mains.
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
       (ip i acc) (? ((i < n) && dig? (s i)) (ip (i + 1) ((* 10 acc) + (s i - 48))) [acc i])
       r (ip 0 0)
       whole <r i <>r
       (? ((i = n) && (0 < n)) (* whole mul)
          ((i < n) && (s i = 46))
           (: (fp j acc sc) (? ((j < n) && dig? (s j))
                               (fp (j + 1) ((* 10 acc) + (s j - 48)) (* sc 10))
                               [acc sc j])
              fr (fp (i + 1) 0 1)
              (? ((<>>fr = n) && ((0 < i) || ((i + 1) < n)))
                 ((* whole mul) + (// (* <fr mul) <>fr))
                 ()))
          ()))

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

   ; -TERM / -15 / -9 -> the number, or () for anything else. shared with kill, which
   ; is the tool that taught the table; pkill and killall wear the same spelling.
   usignames '(("HUP" 1) ("INT" 2) ("QUIT" 3) ("KILL" 9) ("USR1" 10)
               ("USR2" 12) ("TERM" 15) ("CONT" 18) ("STOP" 19))
   (usigdig? b k) (? ((tally b) <= k) (0 < (tally b))
                     ((48 <= (b k)) && ((b k) <= 57)) (usigdig? b (k + 1))
                     0)
   (usignum w)
    (: n (tally w)
       (snl l b) (? !(two? l) () (<<l = b) <><l (snl >l b))
       (? ((n < 2) || !((w 0) = 45)) ()
          (: b (snip w 1 n)
             (? (usigdig? b 0) (uatoi b) (snl usignames b)))))

   ; -- kill: a signal (default TERM) onto every named pid --
   (kill-main as)
    (: (alldig? s) (usigdig? s 0)
       sig (? (two? as) (usignum <as) ())
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
                         (d = 97) (say o (udt-wd wd))
                         (d = 65) (say o (udt-wdl wd))
                         ((d = 98) || (d = 104)) (say o (udt-mo (M - 1)))
                         (d = 66) (say o (udt-mol (M - 1)))
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
        (? !(two? a) [when fmt]
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


   ; -- printenv: the world, or just the names asked for (a name with no value in it
   ; says nothing and the exit remembers, like GNU's) --
   (printenv-main as)
    (? !(two? as)
       (: (pr e) (? !(two? e) (: _ (flush out) 0)
                    (: _ (say out <e) _ (put out 10) (pr >e)))
          (pr (environ 0)))
       (: (one w ok) (: v (getenv w)
                        (? (string? v) (: _ (say out v) _ (put out 10) ok) 0))
          (go a ok) (? (two? a) (go >a (one <a ok)) (: _ (flush out) (? ok 0 1)))
          (go as 1)))

   ; -- whoami and groups: id's two thin faces (id -un and id -Gn) --
   (whoami-main _)
    (: _ (say out (uidname "/etc/passwd" (getuid 0))) _ (put out 10) _ (flush out) 0)
   (groups-main _)
    (: nm (uidname "/etc/passwd" (getuid 0))
       gs (ugroups nm (getgid 0))
       _ (say out (ujoin (map (\ g (uidname "/etc/group" g)) gs)))
       _ (put out 10) _ (flush out) 0)


   ; ---- the /proc family: ps, free, uptime, pidof, pgrep, pkill, killall, pwdx ----
   ; ⚠ NO NIF GREW FOR ANY OF THESE. /proc is a filesystem, so the whole family is
   ; `uread` and a parser -- which is also why a world without one (the kernel's own
   ; image, free/kmain.c, mounts no procfs) reads as NO PROCESSES rather than as an
   ; error. The tools say what they found; they do not invent a reason.
   ; ⚠ AND NONE OF THEM CAN BE SMOKED BYTE-FOR-BYTE against procps: the process table
   ; moves between two runs, and every number here is a clock. So the PARSERS are lawed
   ; (law.l) and the FACES are smoked as sets -- a pid we made ourselves is in the
   ; listing, a pid we killed is gone -- which is test/gate/kore.sh's own note.

   (uprocs _)                          ; every numeric name under /proc, ascending
    (: r (readdir "/proc")
       (dig? s) (: n (tally s)
                   (go k) (? (n <= k) (0 < n)
                             ((48 <= (s k)) && ((s k) <= 57)) (go (k + 1))
                             0)
                   (go 0))
       (? (two? r) (sort (map uatoi (filter dig? r))) ()))
   ; a stat line -> (pid comm state ppid pgrp session tty_nr ..). ⚠ THE COMM IS TAKEN
   ; BETWEEN THE FIRST "(" AND THE LAST ")" and never by splitting on spaces: a program
   ; may be named `(sd-pam)` or `a b)c`, and a naive split reads its parentheses as
   ; fields -- silently, since every field after it is then off by the same amount.
   (ustatf ln)
    (: n (tally ln)
       (lp k) (? (k < 0) -1 ((ln k) = 41) k (lp (k - 1)))
       (fp k) (? (n <= k) -1 ((ln k) = 40) k (fp (k + 1)))
       a (fp 0) b (lp (n - 1))
       ; ⚠ the pid is taken through uwords and not off the raw slice: `uatoi` reads
       ; every charm it is handed, so the SPACE before the "(" would go in as -16
       (? ((a < 0) || ((b < 0) || (b < a))) ()
          (: h (uwords (snip ln 0 a))
             (? !(two? h) ()
                ((uatoi <h) >< ((snip ln (a + 1) b) >< (uwords (snip ln (b + 1) n))))))))
   (upstat pid)                        ; a pid -> its stat fields | ()
    (: r (uread (("/proc/" + (show pid)) + "/stat"))
       (? (two? r) (ustatf <>r) ()))
   (upself _)                          ; the same for this very process
    (: r (uread "/proc/self/stat") (? (two? r) (ustatf <>r) ()))
   (upcmd pid)                         ; the cmdline, NULs to spaces, or () when empty
    (: r (uread (("/proc/" + (show pid)) + "/cmdline"))
       (? !(two? r) ()
          (: s <>r n (tally s)
             (go k acc) (? (n <= k) acc
                           (go (k + 1) (acc + (? ((s k) = 0) " " (string (s k))))))
             o (go 0 "")
             z (tally o)
             (? (0 = z) () ((o (z - 1)) = 32) (snip o 0 (z - 1)) o))))
   (ubase0 p) (: n (tally p)                       ; argv[0]'s basename, for pidof
                 (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p r n))
                 (go 0 0))

   ; the two clocks. USER_HZ is 100 everywhere Linux runs a userland we could be in.
   (uclk s)                            ; a second count -> HH:MM:SS
    (: (p2 v) (uzpad 2 (show v))
       ((((p2 (// s 3600)) + ":") + ((p2 (// (% s 3600) 60)) + ":")) + (p2 (% s 60))))
   (uhms tk) (uclk (// tk 100))
   ; tty_nr -> the name ps prints. the encoding is Linux's own: major in bits 8..19,
   ; minor split across 0..7 and 20..31, and 0 means no controlling terminal.
   (utty n)
    (? (n = 0) "?"
       (: maj (& (n >> 8) 4095)
          mn ((& n 255) | (& (n >> 12) 1048320))
          (? (maj = 136) ("pts/" + (show mn))
             (maj = 4) (? (mn < 64) ("tty" + (show mn)) ("ttyS" + (show (mn - 64))))
             "?")))
   ; /proc/uptime's seconds -> procps' phrase. under an hour it counts minutes, and a
   ; day arrives with a comma: "up 26 days,  8:01", "up  2:33", "up 33 min"
   (uupsay secs)
    (: d (// secs 86400)
       hh (// (% secs 86400) 3600)
       mm (// (% secs 3600) 60)
       ds (? (0 < d) ((show d) + (? (d = 1) " day, " " days, ")) "")
       (? (0 < (d + hh)) ((ds + (upad 2 (show hh))) + (":" + (uzpad 2 (show mm))))
          ((show mm) + " min")))
   (umeminfo _)                        ; /proc/meminfo -> a tablet, the kB counts by name
    (: r (uread "/proc/meminfo")
       t (tablet 0)
       (one ln) (: n (tally ln)
                   (go k) (? (n <= k) -1 ((ln k) = 58) k (go (k + 1)))
                   i (go 0)
                   (? (i < 0) 0
                      (: ws (uwords (snip ln (i + 1) n))
                         (? (two? ws) (pin t (intern (snip ln 0 i)) (uatoi <ws)) 0))))
       _ (? (two? r) (foldl (\ _ ln (one ln)) 0 (lines <>r)) 0)
       t)

   ; -- ps: the pid, its terminal, its cpu time and its name. -e/-A/a/x take every
   ; process; bare ps takes the ones that are OURS and share this terminal, which is
   ; procps' rule (same euid, same tty) and not a simplification of it -- a bare ps
   ; listing root's daemons would be a different tool.
   ; ⚠ the owner comes off /proc/PID ITSELF, whose directory is owned by the process:
   ; one stat where /proc/PID/status would be a second read and a second parser.
   (upuid p) (: st (stat ("/proc/" + (show p))) (? (5 < (tally st)) (st 4) -1))
   (ps-main as)
    (: all (: (go l) (? !(two? l) 0
                        (: w <l
                           (? (member? w '("-e" "-A" "-ax" "ax" "a" "x" "-a")) 1 (go >l))))
              (go as))
       self (upself 0)
       mine (? self (uatoi (self 6)) 0)
       myuid (getuid 0)
       _ (say out "    PID TTY          TIME CMD") _ (put out 10)
       (one p) (: f (upstat p)
                  (? !f 0
                     (: tty (uatoi (f 6))
                        (? (! (all || ((tty = mine) && ((upuid p) = myuid)))) 0
                           (: _ (say out (upad 7 (show p)))
                              _ (say out (" " + (urpad 8 (utty tty))))
                              _ (say out (" " + (uhms ((uatoi (f 13)) + (uatoi (f 14))))))
                              _ (say out (" " + (f 1)))
                              (put out 10))))))
       _ (foldl (\ _ p (one p)) 0 (uprocs 0))
       _ (flush out) 0)

   ; -- free: /proc/meminfo, procps' six columns. ⚠ USED IS TOTAL MINUS AVAILABLE and
   ; not total minus free: the kernel's own estimate of what a new program could have
   ; is the only honest reading of "used", and it is what procps prints --
   (free-main as)
    (: m (umeminfo 0)
       (g k) (peep m k 0)
       div (? (member? "-g" as) 1048576 (? (member? "-m" as) 1024 1))
       (cell x) (upad 12 (show (// x div)))
       tot (g 'MemTotal) fre (g 'MemFree) av (g 'MemAvailable)
       bc (((g 'Buffers) + (g 'Cached)) + (g 'SReclaimable))
       st (g 'SwapTotal) sf (g 'SwapFree)
       _ (say out "               total        used        free      shared  buff/cache   available")
       _ (put out 10)
       _ (say out ((urpad 8 "Mem:") + (cell tot) + (cell (tot - av)) + (cell fre)
                   + (cell (g 'Shmem)) + (cell bc) + (cell av)))
       _ (put out 10)
       _ (say out ((urpad 8 "Swap:") + (cell st) + (cell (st - sf)) + (cell sf)))
       _ (put out 10)
       _ (flush out) 0)

   ; -- uptime: the clock, how long since boot, and the three load averages. ⚠ THERE
   ; IS NO `N users` FIELD and there will not be one: it comes out of utmp, which this
   ; tree does not keep, and a fabricated 0 is worse than an absent field. ⚠ the time
   ; of day is UTC, for the reason `date` gives --
   (uptime-main as)
    (: r (uread "/proc/uptime")
       secs (? (two? r) (: w (uwords <>r)
                           (? (two? w) (uatoi <(usplit 46 <w)) 0))
              0)
       l (uread "/proc/loadavg")
       ws (? (two? l) (uwords <>l) ())
       ld (? (2 < (tally ws)) ((((ws 0) + ", ") + ((ws 1) + ", ")) + (ws 2))
              "0.00, 0.00, 0.00")
       tod (uclk (% (// (clock 0) 1000) 86400))
       _ (say out (((" " + tod) + (" up " + (uupsay secs))) + (",  load average: " + ld)))
       _ (put out 10) _ (flush out) 0)

   ; -- the four by-name tools. a name is matched against the COMM, which is what the
   ; kernel keeps and is capped at fifteen charms -- `pgrep -f` asks the cmdline
   ; instead, where the whole of a long program name lives --
   (upmatch f? x? pat pid)             ; does this process answer to pat
    (: c (? f? (upcmd pid) (: s (upstat pid) (? s (s 1) ())))
       (? !(string? c) 0
          x? (c = pat)
          (: r (reere pat) (? (two? r) (rehas <>r c) (udie 2 ("bad pattern: " + pat))))))
   (upfind f? x? pat)                  ; the pids answering, ours never among them
    (: me (: s (upself 0) (? s <s -1))
       (filter (\ p (? (p = me) 0 (upmatch f? x? pat p))) (uprocs 0)))
   (pidof-main as)
    (: fs (filter (\ w !((0 < tally w) && ((w 0) = 45))) as)
       _ (? (two? fs) () (udie 1 "usage: pidof NAME.."))
       me (: s (upself 0) (? s <s -1))
       (hit p) (: s (upstat p)
                  c (? s (s 1) ())
                  a (: x (upcmd p) (? (string? x) (ubase0 <(usplit 32 x)) ()))
                  (? (p = me) 0
                     (foldl (\ acc w (? acc 1 (? ((c = w) || (a = w)) 1 0))) 0 fs)))
       ps (filter hit (rev (uprocs 0)))
       _ (? (two? ps) (: _ (say out (ujoin (map show ps))) (put out 10)) 0)
       _ (flush out)
       (? (two? ps) 0 1))
   (pgrep-main as)
    (: t (tablet 0)
       o (uopts as t () ())
       _ (? (two? o) () (udie 2 "usage: pgrep [-f] [-x] PATTERN"))
       rest <>>o
       _ (? (two? rest) () (udie 2 "usage: pgrep [-f] [-x] PATTERN"))
       ps (upfind (peep t 102 0) (peep t 120 0) <rest)
       _ (foldl (\ _ p (: _ (say out (show p)) (put out 10))) 0 ps)
       _ (flush out)
       (? (two? ps) 0 1))
   (pkill-main as)
    (: sig (? (two? as) (usignum <as) ())
       rest (? (charm? sig) >as as)
       t (tablet 0)
       o (uopts rest t () ())
       _ (? (two? o) () (udie 2 "usage: pkill [-SIG] [-f] [-x] PATTERN"))
       ws <>>o
       _ (? (two? ws) () (udie 2 "usage: pkill [-SIG] [-f] [-x] PATTERN"))
       ps (upfind (peep t 102 0) (peep t 120 0) <ws)
       _ (foldl (\ _ p (still p (? (charm? sig) sig 15))) 0 ps)
       (? (two? ps) 0 1))
   (killall-main as)
    (: sig (? (two? as) (usignum <as) ())
       ns (? (charm? sig) >as as)
       _ (? (two? ns) () (udie 1 "usage: killall [-SIG] NAME.."))
       me (: s (upself 0) (? s <s -1))
       (one nm ok)
        (: ps (filter (\ p (? (p = me) 0 (: s (upstat p) (? s ((s 1) = nm) 0)))) (uprocs 0))
           (? (two? ps)
              (: _ (foldl (\ _ p (still p (? (charm? sig) sig 15))) 0 ps) ok)
              (: _ (say err (nm + ": no process found")) _ (put err 10) 0)))
       ok (foldl (\ a nm (one nm a)) 1 ns)
       _ (flush err)
       (? ok 0 1))
   ; -- pwdx: where a process is standing, out of /proc/PID/cwd --
   (pwdx-main as)
    (: _ (? (two? as) () (udie 1 "usage: pwdx PID.."))
       (one w ok)
        (: d (readlink (("/proc/" + w) + "/cwd"))
           (? (string? d) (: _ (say out ((w + ": ") + d)) _ (put out 10) ok)
              (: _ (say err (w + ": No such process")) _ (put err 10) 0)))
       ok (foldl (\ a w (one w a)) 1 as)
       _ (flush out) (? ok 0 1))

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
       runs (? !(two? ws) [()]                       ; no input still runs once, like GNU
               nn (chunks nn ws)
               [ws])
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
