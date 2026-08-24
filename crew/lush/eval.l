; --- lush 🐚 the evaluator -- AST to processes ---
; NO BRACKETS HERE: the runtime supplies the layer (see job.l for the cat law).
;
; (sh-line src) -> ('more . why) | status. redirects become a spawnmap fdmap
; ((childfd . srcfd)..) applied IN ORDER in the child -- POSIX's left-to-right
; law; a heredoc ("<h") is a pipe whose body pours in AFTER the spawn (the
; child drains as we write, so size never deadlocks). the AST is PLANNED into
; closures once (sh-plan) and the plan is what runs, so a loop body, a function
; and a case arm pay their kind tests and their assignment peel once, not per
; run. pipeline stages spawn;
; a stage that is a compound, a builtin, a function or carries assignments
; FORKS instead (POSIX puts it in a subshell anyway) -- the child lays the
; pipe map with dup2 on itself, evals, quits. control flow threads its
; signals OUT OF BAND as pairs -- ('brk n) ('cnt n) ('ret st) -- loops catch
; brk/cnt, a function call catches ret, everything else passes them up.
; set -e rides sh-eflag with sh-noe guarding conditions and && || arms.

(module 'lush
(: ; export NAME=VAL -> setenv; export NAME promotes the shell var
   (sh-eqpos s i) (? (i = (tally s)) () (? ((s i) = 61) i (sh-eqpos s (i + 1))))
   (sh-export w)
     (: p (sh-eqpos w 0)
        (? !p (setenv w (sh-vval w))
           (setenv (snip w 0 p) (snip w (p + 1) (tally w)))))

   (cmderr msg) (: _ (say err ("lush: " + (msg + "\n"))) _ (pin sh-status () 1) 1)
   ; ⚠ the exec is the CHILD's, and a child that cannot exec has no voice: it can
   ; only _exit(127). so an unfindable word is answered HERE, before the fork, where
   ; there is still a shell to say it -- see sh-simple's spawn arm.
   (sh-saynf w) (say err ("lush: " + (w + ": not found\n")))
   (sh-nofound w) (: _ (sh-saynf w) _ (pin sh-status () 127) (sh-echk 127))

   ; --- redirects -> the fdmap ------------------------------------------------
   ; (sh-rdmap redirs) -> (1 fdmap opens writes) | ('err . msg). opens close in
   ; the child AND in the parent after spawn; writes ((wfd . body)..) are
   ; heredoc feeds -- the parent pours and closes them after the spawn.
   (dfd ion d) (? (two? ion) <ion d)
   (rderr opens msg) (: _ (each opens fdclose) ('err >< msg))
   (sh-rdmap rs) (rdm rs () () ())
   (rdm rs fdmap opens writes)
     (? !rs [1 (sh-rev fdmap) opens writes]
        (: r <rs ion (sh-nth r 0) op (sh-nth r 1)
           (? (op = "<h")
              (: pp (pipe 0)
                 (? (charm? pp) (rderr opens "cannot pipe")
                    (rdm >rs
                         ((dfd ion 0 >< <pp) >< fdmap)
                         (<pp >< opens)
                         ((>pp >< sh-textof (sh-nth r 2)) >< writes))))
              (: tf (sh-fields (sh-nth r 2))
                 (? (!(two? tf) || two? >tf) (rderr opens "ambiguous redirect")
                    (: t <tf
                       (? (op = "<")
                            (: fd (openfd t 0)
                               (? (fd < 0) (rderr opens (t + ": cannot open"))
                                  (rdm >rs ((dfd ion 0 >< fd) >< fdmap) (fd >< opens) writes)))
                          ((op = ">") || (op = ">>"))
                            (: fd (openfd t (? (op = ">") 1 2))
                               (? (fd < 0) (rderr opens (t + ": cannot open"))
                                  (rdm >rs ((dfd ion 1 >< fd) >< fdmap) (fd >< opens) writes)))
                          ((op = ">&") || (op = "<&"))
                            (: d (dfd ion (? (op = "<&") 0 1))
                               (? (t = "-") (rdm >rs ((d >< ()) >< fdmap) opens writes)
                                  (sh-digits? t) (rdm >rs ((d >< sh-int t) >< fdmap) opens writes)
                                  (rderr opens (t + ": bad fd"))))
                          (rderr opens (op + ": unsupported")))))))))
   (sh-hwrite wrs)
     (each wrs (\ e (: p (fdopen <e) _ (say p >e) (close p))))

   ; --- assignments: NAME=value words at the head -------------------------------
   (sh-asnname? s)
     (: n (tally s)
        (? !n 0 !(sh-name1? (s 0)) 0
           (: (go i) (? (i = n) 1 (? (sh-name? (s i)) (go (i + 1)) 0)) (go 1))))
   (sh-asn? segs)
     (? !(two? segs) ()
        (: h <segs
           (? !(id? <h 'lit) ()
              (: t >h p (sh-eqpos t 0)
                 (? !p ()
                    !(sh-asnname? (snip t 0 p)) ()
                    (link (snip t 0 p)
                          (: rest (snip t (p + 1) (tally t))
                             (? (tally rest) (('lit >< rest) >< >segs) >segs))))))))
   (sh-peel ws acc)
     (? (two? ws)
        (: a (sh-asn? <ws)
           (? (two? a) (sh-peel >ws (a >< acc)) (sh-rev acc >< ws)))
        (sh-rev acc >< ws))
   (sh-doasn a) (sh-setvar <a (sh-textof >a))
   (sh-tmpset a)                                ; temporary env around one command
     (: nm <a old (getenv nm)
        _ (setenv nm (sh-textof >a))
        (nm >< old))
   (sh-tmprestore e) (setenv <e >e)             ; () old unsets -- the absence lane

   ; --- the in-image lane: a tool whose main rides THIS image -------------------
   ; a main in this image runs HERE -- no fork, no exec, no image wake -- and answers
   ; its status as a charm (crew/kore/u.l's urun, the door every kore tool and
   ; mooncc leave through). ⚠ we take it ONLY when the thing we would have exec'd IS
   ; the thing we already hold: PATH's winner for the word must be this very binary,
   ; which stat answers through the symlink (size mode mtime-ns -- the distro's shadow
   ; lane is one artifact on PATH wearing every tool's name). an installed twin of our
   ; own name is the SKEW crew/build.mk warns about wearing a friendly face, so it
   ; spawns like anything else; so does a word with a / (an explicit path is a demand).
   ; the verdict is decided once per word and kept: the PATH walk is a session cost.
   ; ⚠ these are NOT builtins -- sh-bi? drives sh-stagex's fork decision, and a stage
   ; that forks where it used to spawn is slower for nothing. this is the simple
   ; foreground command only, and it takes its redirects the way a builtin does.
   sh-imgc (tablet 0)                           ; word -> (1 fn whole?) | 0, decided once
   ; ⚠ AN ALLOW LIST, and a short one on purpose -- the qualifying word is not "is
   ; its main here?" but "is running it here the same thing as running it there?".
   ; three ways that fails, and every one of them is a silent wrong answer:
   ;   * a main that QUITS takes its caller with it. sh-main ends in one, so an
   ;     in-image `sh -c` would end the line that called it -- everything after it
   ;     gone, status 0. cook-main too.
   ;   * a main that keeps STATE one run owns. cook's recipe cards are tablets, so a
   ;     recursive $(MAKE) in here would lay the child's rules over the parent's.
   ;     env-main setenvs into the process it is standing in, which would be ours.
   ;   * a main that BLOCKS. a spawned `cat`, `yes`, `sleep`, `tee` is a child ^C
   ;     kills; in here ^C would kill the shell, and there is no pid for `kill %1`
   ;     to name. that is the job-control half, and it wants tasks before it widens.
   ; `kore` is NOT here either, though kore-main answers -- its own dispatcher reaches
   ; sh-main, cook-main and vi-main, so `kore sh ..` in here would walk straight into the
   ; first failure above through a side door.
   ; SO THE QUALIFYING TEST IS "CANNOT BLOCK, QUITS NOTHING, KEEPS NOTHING": bounded by
   ; its own arguments, waiting on no fd it did not open. That is what the list below is,
   ; and it is most of a build -- 152 of the 249 self-execs one seed-only bootstrap spent.
   ; ⚠ one more class stays out and it is not in the three above: the tools that read
   ; STDIN when handed no file operand (cat head tail wc sort uniq cut tr nl rev grep sed
   ; tee xargs). Those are the next tier and they wait on the stdin story, which is a
   ; question about ^C and a terminal rather than about anything in this file.
   ; MEASURED 2026-08-16, and the number says WAIT. Over a host build -- 228 simple
   ; foreground commands, lush as make's SHELL -- the tier was SEVEN (4 cat, 3 sed)
   ; against the 159 the list below already takes, and every one of the seven named a
   ; file operand, so asking each tool "are these args safe in here?" would have
   ; admitted all of them and bought 3%. ⚠ AND ON A HOST THE LANE IS NOT EVEN LIVE:
   ; cook asks `sh-ours? SHELL` first, /bin/sh is dash, so every recipe line spawns.
   ; The payoff is the DISTRO's, where /bin/sh IS us. Measure there, not here.
   ; ⚠ the checksums are the one row here that reads STDIN when handed no file operand,
   ; and they are in anyway: named a file they are bounded like the rest, and the words
   ; a build spells (`sha256sum dist.tgz`, a -c over a list) always name one.
   sh-imgok '("mooncc" "cc"
              "test" "[" "true" "false" "echo" "printf" "uname" "pwd"
              "basename" "dirname" "readlink" "seq" "ls" "cmp" "diff"
              "mkdir" "rmdir" "rm" "ln" "touch" "chmod" "mv" "cp" "install"
              "objcopy" "cksum" "md5sum" "sha256sum")
   ; nm/ar/ld/as are reachable now (module 'kore exports every <word>-main) but stay
   ; off the list on the qualifying test alone: nothing measured wants them in-image.
   ; the words whose main is not spelled <word>-main. kore-main wants the WHOLE argv
   ; (it reads its own program seat); the rest take the args after the name.
   sh-imgalias '(("mooncc" "moon-run" "moon") ("cc" "moon-run" "moon") ("kore" "kore-main")
                 ("[" "bracket-main") ("objcopy" "objcopy" "holo"))
   (sh-pathdirs _)
     (: p ("" + (getenv "PATH")) n (tally p)
        (go from k acc) (? (n <= k) (sh-rev (snip p from k >< acc))
                           ((p k) = 58) (go (k + 1) (k + 1) (snip p from k >< acc))
                           (go from (k + 1) acc))
        (go 0 0 ()))
   (sh-slashed? w) (: n (tally w)
                      (go k) (? (n <= k) 0 ((w k) = 47) 1 (go (k + 1)))
                      (go 0))
   ; ⚠ asked at the call: the kernel bakes this file and carries no host nif, where a
   ; top-level bind would fold the BAKING love's own seat in and call every tool ours.
   (sh-seat _) (? (member? 'selfpath (names ())) (selfpath 0) ())
   (sh-self? p) (: a (stat p) ((two? a) && (a = (stat (sh-seat ())))))
   (sh-whichp w)                                ; PATH's winner for w -> (1 path), or ()
     (? (sh-slashed? w) (? (two? (stat w)) [1 w] ())
        (: (go l) (? !(two? l) ()
                     (: d (? (tally <l) <l ".") p (d + ("/" + w))
                        (? (two? (stat p)) [1 p] (go >l))))
           (go (sh-pathdirs ()))))
   (sh-onpath? w)                               ; is PATH's winner for w this binary
     (: (go l) (? !(two? l) 0
                  (: d (? (tally <l) <l ".") p (d + ("/" + w))
                     (? (two? (stat p)) (sh-self? p) (go >l))))
        (go (sh-pathdirs ())))
   ; would spawning this word land on the binary we are already running? the one
   ; policy question behind every in-image lane -- lush's own, and cook's.
   (sh-ours? w) (? (sh-slashed? w) (sh-self? w) (sh-onpath? w))
   (sh-imgask w)
     (: al (sh-assoc w sh-imgalias)
        mn (? (two? al) <>al (w + "-main"))
        md (? ((two? al) && (two? >>al)) (from (intern <>>al)) ())   ; the row's module, or ()
        nm (intern mn)
        f0 (? (tablet? md) (md nm)                    ; the alias row named the module
              (member? nm (names ())) (ev nm)         ; a bare global
              (from 'kore nm))                        ; else the applets' home module
        (? (&& (sh-smem? w sh-imgok) !(sh-slashed? w) !(nil? f0) (sh-ours? w))
           [1 (sh-imgwrap f0 mn) (mn = "kore-main")]
           0))
   ; ⚠ THE APPLET IS WRAPPED, because udie RAISES rather than exiting: `(scare 'leave c)`
   ; is what the verb table's own `urun` catches, and the in-image lane reaches the applet
   ; BARE. Unwrapped, one `test` on a bad expression escapes as an unhandled raise and
   ; takes the whole shell down instead of failing one command. moon-run wants no wrap --
   ; it answers its status by construction, which is why it was the safe single entry.
   ; urun lives in module 'kore now; a miss absorbs to (), which keeps the no-wrap lane.
   (sh-imgwrap f mn) (: ur (from 'kore 'urun)
                        (? ((mn = "moon-run") || !ur) f (\ as (ur f as))))
   (sh-assoc w l) (? !(two? l) 0 (w = <<l) <l (sh-assoc w >l))
   (sh-imgfn w) (: v (peep sh-imgc w 'no)
                   (? !(id? v 'no) v
                      (: f (sh-imgask w) _ (pin sh-imgc w f) f)))

   ; --- builtins: a charm status, a control pair (brk/cnt/ret), or 'nope --------
   sh-binames '("exit" "cd" "export" "pwd" "jobs" "fg" "bg" ":" "true" "false" "command"
                "read" "set" "shift" "unset" "eval" "." "wait" "local"
                "break" "continue" "return" "ev" "trap")
   (sh-bi? nm) (sh-smem? nm sh-binames)
   (sh-readb args)
     (: args2 (? (two? args && (<args = "-r")) >args args)
        nms (? (two? args2) args2 ["REPLY"])
        ln (sh-getline in)
        (? (id? ln 'eof) 1 (: _ (sh-readset nms ln 0 (tally ln)) 0)))
   (sh-readset nms ln i n)
     (: i2 (skifs ln i n)
        (? !(two? >nms) (sh-setvar <nms (sh-rtrim (snip ln i2 n)))
           (: j (chunk ln i2 n)
              _ (sh-setvar <nms (snip ln i2 j))
              (sh-readset >nms ln j n))))
   (sh-rtrim s)
     (: (go k) (? ((0 < k) && sh-ifs? (s (k - 1))) (go (k - 1)) (snip s 0 k))
        (go (tally s)))
   (sh-setb args)
     (? !(two? args) 0
        (: a <args
           (? (a = "--") (: _ (pin sh-pos () >args) 0)
              ((1 < (tally a)) && (((a 0) = 45) || ((a 0) = 43)))
                (: on (? ((a 0) = 45) 1 0)
                   (go i) (? (i = (tally a)) (sh-setb >args)
                             (: b (a i)
                                _ (? (b = 101) (pin sh-eflag () on)
                                     (b = 117) (pin sh-uflag () on)
                                     (b = 120) (pin sh-xflag () on)
                                     0)
                                (go (i + 1))))
                   (go 1))
              (: _ (pin sh-pos () args) 0))))
   (sh-dotb args)
     (? !(two? args) (cmderr ". wants a file")
        (: f <args
           (? (two? (stat f))
              (: _ (pin sh-retf () 0)            ; a stale interactive `return` must not clip the file
                 p (open f "r") _ (sh-lines p "") _ (close p) (sh-status ()))
              (cmderr (f + ": not found")))))
   ; trap [ACTION] CONDITION.. -- POSIX. No operands lists what is set, in a form
   ; that reads back in. `-` as the action restores the default, '' ignores, and
   ; both are spelled by REMOVING the entry: nothing here delivers a signal, so an
   ; ignored one and a defaulted one behave alike (see job.l's note).
   ; ⚠ a CONDITION ALONE is the reset form -- `trap INT` is not "trap with an
   ; empty action", it is `trap - INT`. A first word that names a condition is
   ; therefore never an action, which is also why EXIT and 0 both spell the same
   ; row: sh-cond normalises before anything is stored.
   (sh-cond w)
     (: u (sh-upper w)
        v (? ((tally u) > 3 && (snip u 0 3) = "SIG") (snip u 3 (tally u)) u)
        (? (v = "0") "EXIT" v))
   (sh-upper s)                                 ; ⚠ an INDEX walk: foldl does not take a string
     ((: (g i acc)
         (? (i = tally s) acc
            (: b (s i) (g (1 + i) (acc + string (? (97 <= b < 123) (b - 32) b))))))
      0 "")
   (sh-cond? w) (: c (sh-cond w)
                   (any (\ k (k = c)) '("EXIT" "INT" "TERM" "HUP" "QUIT" "USR1" "USR2"
                                        "ALRM" "PIPE" "CHLD" "TSTP" "CONT" "ABRT")))
   ; ⚠ EVERY LANE BELOW ENDS AT sh-rearm: the set of watched signals is fixed when
   ; the fd is made, so adding OR dropping a condition means building a new one.
   (sh-trapb args) (: r (sh-trapb2 args) _ (sh-rearm 0) r)
   (sh-trapb2 args)
     (? !(two? args)
        (: _ (each (keys sh-traps)
                   (\ k (puts ("trap -- '" + (peep sh-traps k "") + "' " + k + "\n"))))
           0)
        (: a2 (? (<args = "--") >args args)
           (? !(two? a2) 0
              (sh-cond? <a2) (: _ (each a2 (\ w (pull sh-traps (sh-cond w) 0))) 0)
              (<a2 = "-") (: _ (each >a2 (\ w (pull sh-traps (sh-cond w) 0))) 0)
              (: act <a2
                 conds >a2
                 bad (filter (\ w !(sh-cond? w)) conds)
                 (? (two? bad)
                    (: _ (say err ("lush: trap: " + <bad + ": bad condition\n")) 1)
                    ; ⚠ a condition IGNORED ON ENTRY is skipped, silently: POSIX says
                    ; it cannot be trapped and that no error need be reported. The
                    ; reset lanes are not filtered -- `trap - INT` on one is already
                    ; a no-op, and refusing it would be an error POSIX does not want.
                    (: _ (each conds
                               (\ w (: c (sh-cond w)
                                       (? (sh-ignored? c) 0
                                          (tally act) (pin sh-traps c act)
                                          (pull sh-traps c 0)))))
                       0))))))
   (sh-localb args)
     (: _ (each args (\ w (: p (sh-eqpos w 0)
                             (? !p (pin sh-vars w "")
                                (pin sh-vars (snip w 0 p) (snip w (p + 1) (tally w)))))))
        0)
   (sh-waitb args)                              ; wait [pid..] -> the LAST one's status
     (? (two? args)
        (foldl (\ a p (: st (wait (sh-int p))
                         _ (sh-bgnote (sh-int p))
                         (? (charm? st && (st < 0)) 127 st)))
               0 args)
        (: _ (each (keys sh-jobs)
                   (\ nn (: j (sh-jobs nn)
                            (? !j 0 (: _ (each (sh-nth j 0) (\ p (wait p)))
                                       (pull sh-jobs nn 0))))))
           0))
   ; `ev EXPR..` -- the arguments join as ONE love form (the `eval` builtin's
   ; join), its answer says on out, and its TRUTH BIT is the status: positive is
   ; true, so `ev 1 + 2` is a calculator and `if ev '3 > 2'` is a test -- the one
   ; thing ${( )} cannot do, since an expansion is a word and not a command. -n
   ; says nothing and answers the status alone (cli.l's -e/-n pair, same letters).
   ; the form runs in THIS image, so a binding it makes is still there next line.
   ; ⚠ NOT SPELLED `love`. A builtin shadows its PATH twin, and the interpreter
   ; and this lane spell their arguments differently -- `love -e "1 + 2"` would
   ; read `-e` as a DATUM and fold it into the form, answering 3.0 for 3. The two
   ; want different command lines and isolated processes, so the word `love` stays
   ; the binary's and spawns. `ev` is love's own name for the evaluator (love/ev.l).
   (sh-loveb args)
     (: q ((two? args) && (<args = "-n"))
        src (sh-unwords (? q >args args))
        (? !(tally src) 0
           (: r (sh-loveval src)
              (? (id? <r 'err) (sh-lovesay >r)
                 (: v >r
                    _ (? q 0 (puts ((sh-lovetext v) + "\n")))
                    (? v 0 1))))))
   (sh-builtin argv)
     (: cmd <argv
        (? (cmd = "exit")
             (: st (? >argv (sh-int <>argv) (sh-status ()))
                (sh-leave st))
           (cmd = "cd")
             ; ⚠ CHARM?, NEVER A TRUTH TEST. chdir answers () on success and the ERRNO on
             ; failure -- and an errno is negative, so it is RED, so `!r` reads a failure as
             ; a success and `cd nowhere` returned 0 with no word said. `cd build && make`
             ; then ran make wherever you already were. The KIND is what separates them.
             (: t (? >argv <>argv ("" + (getenv "HOME")))
                r (chdir t)
                (? (charm? r) (: _ (say err ("lush: cd: " + (t + "\n"))) 1) 0))
           (cmd = "trap") (sh-trapb >argv)
           (cmd = "export") (: _ (each >argv sh-export) 0)
           ; `command -v W` -- the probe every configure-shaped script opens with, and a
           ; SPECIAL BUILTIN, so it was forking and answering ENOENT: 12 of them in one
           ; build, each reading as "the tool is absent". Absent is the honest answer for
           ; ccache and objdump, which is why nothing looked broken -- but a probe that is
           ; wrong by accident is wrong. A builtin answers its own name; a PATH word its path.
           (cmd = "command")
             (: a >argv
                (? ((two? a) && (<a = "-v"))
                   (: w (? (two? >a) <>a "")
                      (? (w = "") 1
                         (sh-bi? w) (: _ (puts (w + "\n")) 0)
                         (: p (sh-whichp w) (? (two? p) (: _ (puts (<>p + "\n")) 0) 1))))
                   !(two? a) 1
                   (sh-simple2 a ())))                 ; `command W ..` -- run W, no function lookup
           (cmd = "pwd")  (: _ (puts ((cwd 0) + "\n")) 0)
           (cmd = "jobs") (: _ (each (sort (keys sh-jobs))
                                     (\ n (puts (sh-jobline n (sh-jobs n))))) 0)
           (cmd = "fg")   (: _ (sh-fg (sh-jobn argv)) (sh-status ()))
           (cmd = "bg")   (sh-bg (sh-jobn argv))
           (cmd = ":")    0
           (cmd = "true") 0
           (cmd = "false") 1
           (cmd = "read") (sh-readb >argv)
           (cmd = "set")  (sh-setb >argv)
           (cmd = "shift") (: n (? >argv (sh-int <>argv) 1)
                              _ (pin sh-pos () (drop n (sh-pos ()))) 0)
           (cmd = "unset") (: _ (each >argv
                                      (\ nm (: _ (pull sh-vars nm 0) (setenv nm ())))) 0)
           (cmd = "eval") (: c (sh-unwords >argv)
                             (? (tally c)
                                (: r (sh-line c) (? (two? r) (cmderr "eval: unexpected eof") r))
                                0))
           (cmd = "ev")   (sh-loveb >argv)
           (cmd = ".")    (sh-dotb >argv)
           (cmd = "local") (sh-localb >argv)
           (cmd = "wait") (sh-waitb >argv)
           (cmd = "break") ('brk >< (? >argv (sh-int <>argv) 1))
           (cmd = "continue") ('cnt >< (? >argv (sh-int <>argv) 1))
           (cmd = "return") ('ret >< (? >argv (sh-int <>argv) (sh-status ())))
           'nope))

   ; --- the function book -------------------------------------------------------
   ; a body is a PLAN (a closure), so two? cannot tell one from absence -- peep
   ; names the miss with a mint no stored value can pose as (spec.l's total test)
   (sh-fnof nm) (peep sh-funcs nm sh-nofn)
   (sh-fn? nm) !(id? sh-nofn (sh-fnof nm))

   ; --- stage expansion + the spawn engines ------------------------------------
   (sh-makepipes n) (? (n < 1) () (pipe 0 >< sh-makepipes (n - 1)))
   (sh-flat ps) (? !ps () (<<ps >< (><ps >< sh-flat >ps)))
   ; a stage: ('x argv fdmap opens writes) spawns; ('a ast () () ()) forks --
   ; compounds, builtins, functions and assignment-bearing commands all fork
   ; (the child re-expands, so its env sees its own assignments)
   (sh-stagex cmds acc)
     (? !cmds [1 (sh-rev acc)]
        (: c <cmds
           (? !(id? <c 'cmd)
              (sh-stagex >cmds ['a c () () () >< acc])
              (: pk (sh-peel (sh-nth c 1) ())
                 argv (catmap sh-fields >pk)
                 (? (|| (two? <pk) !(two? argv)
                        (sh-bi? <argv) (sh-fn? <argv))
                    (sh-stagex >cmds ['a c () () () >< acc])
                    (: rm (sh-rdmap (sh-nth c 2))
                       (? (id? <rm 'err) (: _ (stclean acc) rm)
                          (sh-stagex >cmds
                                     ['x argv (sh-nth rm 1) (sh-nth rm 2) (sh-nth rm 3) >< acc]))))))))
   (stclean acc) (each acc (\ st (each (sh-nth st 3) fdclose)))
   ; spawn one stage: external -> spawnmap; anything else -> fork, the child
   ; lays the pipe map on itself (dup2), closes the plumbing, evals, quits
   ; ⚠ FLUSH FIRST, both lanes: a builtin says through `out` and buffers, a child
   ; writes the fd straight, and when the two share one -- a pipe, a file, a $( )
   ; capture -- whatever we still hold would land after bytes it was said before.
   (sh-spawn1 st base allclose pg fgi)
     (: _ (flush out) _ (flush err)
        (? (id? (sh-nth st 0) 'x)
           ; the stage is spawned either way -- its 127 is the child's own and the
           ; pipeline's status bookkeeping wants a real pid; only the WORD is ours to say
           (: w <(sh-nth st 1)
              _ (? (two? (sh-whichp w)) 0 (sh-saynf w))
              (spawnmap (sh-nth st 1) (base + (sh-nth st 2)) allclose pg fgi))
           (: p (fork 0)
              (? (p = 0)
                 (: _ (wear ())                   ; ⚠ a child wears the CONSOLE: its fds are the dup2 below, and a jug is not one
                    _ (sh-dropcaps ())            ; ...and it is inside no capture of ours
                    _ (each base (\ e (dup2 >e <e)))
                    _ (each allclose (\ fd (? (2 < fd) (fdclose fd) 0)))
                    s (sh-eval (sh-nth st 1))
                    (quit (? (charm? s) s (? (id? <s 'ret) >s 0))))
                 p))))
   ; stage 0 may lead a pgrp + take the tty, the rest join; base map = pipe
   ; wiring, a stage's own redirects land after. lastout >= 0 sends the LAST
   ; stage's stdout there (the cmdsub lane).
   (sh-spawnst sts pipes allclose lastout i last pg fg pids)
     (? !sts (sh-rev pids)
        (: st <sts
           base (+ (? (0 < i) [(0 >< <(sh-nth pipes (i - 1)))] ())
                   (? (i < last) [(1 >< >(sh-nth pipes i))]
                      (? (lastout < 0) () [(1 >< lastout)])))
           pid (sh-spawn1 st base allclose pg (? (i = 0) fg 0))
           (sh-spawnst >sts pipes allclose lastout (i + 1) last
                       (? (pg < 0) pg (? (i = 0) pid pg)) fg (pid >< pids))))
   (sh-echk st)
     (? (&& (charm? st) !(st = 0) (sh-eflag ()) (0 = (sh-noe ())) !(sh-tty ()))
        (sh-leave st) st)
   (sh-pipeline cmds bg)
     (: sx (sh-stagex cmds ())
        (? (id? <sx 'err) (cmderr >sx)
           (: sts (sh-nth sx 1)
              n ((tally sts) - 1)
              pipes (sh-makepipes n)
              pfds (sh-flat pipes)
              ofds (catmap (\ st (sh-nth st 3)) sts)
              wrs (catmap (\ st (sh-nth st 4)) sts)
              allclose (pfds + (ofds + (map cap wrs)))
              pids (sh-spawnst sts pipes allclose (0 - 1) 0 n
                               (sh-pgm 0) (? bg 0 (sh-fgm 0)) ())
              _ (each (pfds + ofds) fdclose)
              _ (sh-hwrite wrs)
              (? bg
                 (: jn (sh-addjob pids 'running ((sh-curline ()) + " &"))
                    _ (pin sh-lastbg () (last pids))      ; $!
                    _ (? (sh-tty ()) (puts ("[" + ((show jn) + ("] " + ((sh-unwords (map show pids)) + "\n"))))) 0)
                    0)
                 (: _ (sh-waitfg pids (sh-curline ())) (sh-echk (sh-status ())))))))
   ; one simple foreground command: assignments, function, builtin, or a spawn.
   ; pk is the peel -- (assignments . words) -- split at PLAN time, it is syntax
   (sh-simple pk rs)
     (: asns <pk
        _ (pin sh-substat () 0)          ; ⚠ ahead of BOTH expansions -- see below
        argv (catmap sh-fields >pk)
        (? !(two? argv)
           ; no command name: POSIX says the status is the LAST $( ) performed,
           ; and the words expand before the assignments do -- so `v=1 $(exit 3)`
           ; is 3 and `v=$(exit 2) $(exit 3)` is 2. none ran leaves the clean 0
           (: _ (each asns sh-doasn)
              st (sh-substat ())
              _ (pin sh-status () st)
              (sh-echk st))
           (: saves (map sh-tmpset asns)
              r (sh-simple2 argv rs)
              _ (each saves sh-tmprestore)
              r)))
   ; run a thunk with redirects laid over OUR OWN fds (dup saves, dup2 swaps,
   ; swap back after) -- functions and builtins take their > < this way
   (sh-withrs rs th)
     (? !(two? rs) (th ())
        (: rm (sh-rdmap rs)
           (? (id? <rm 'err) (cmderr >rm)
              (: fdmap (sh-nth rm 1) opens (sh-nth rm 2) wrs (sh-nth rm 3)
                 _ (sh-hwrite wrs)
                 saves (map (\ e (<e >< dup <e)) fdmap)
                 _ (each fdmap (\ e (? (charm? >e) (dup2 >e <e)
                                       (fdclose <e))))
                 _ (each opens fdclose)
                 s (th ())
                 _ (flush out) _ (flush err)
                 _ (each saves (\ e (? (>e < 0) (fdclose <e)
                                       (: _ (dup2 >e <e) (fdclose >e)))))
                 s))))
   (sh-simple2 argv rs)
     (: _ (? (sh-xflag ()) (say err ("+ " + ((sh-unwords argv) + "\n"))) 0)
        fn (sh-fnof <argv)
        (? !(id? sh-nofn fn) (sh-withrs rs (\ _ (sh-fncall fn argv)))
           (sh-bi? <argv)
             (: b (sh-withrs rs (\ _ (sh-builtin argv)))
                (? (two? b) b                            ; brk/cnt/ret ride out
                   (charm? b) (: _ (pin sh-status () b) (sh-echk b))
                   b))
           (two? (sh-imgfn <argv))                       ; the in-image lane
             (: g (sh-imgfn <argv)
                f (sh-nth g 1)
                s (sh-withrs rs (\ _ (f (? (sh-nth g 2) argv >argv))))
                st (? (charm? s) s 0)
                _ (pin sh-status () st)
                (sh-echk st))
           (: rm (sh-rdmap rs)
              (? (id? <rm 'err) (cmderr >rm)
                 (: opens (sh-nth rm 2) wrs (sh-nth rm 3)
                    _ (flush out) _ (flush err)      ; see sh-spawn1: the child writes the fd straight
                    ; the redirects are already open, which is where a shell answers an
                    ; unfindable word from -- `nope > f` still makes f, then says so
                    pid (? (two? (sh-whichp <argv))
                           (spawnmap argv (sh-nth rm 1) (opens + (map cap wrs))
                                     (sh-pgm 0) (sh-fgm 0))
                           ())
                    _ (each opens fdclose)
                    _ (sh-hwrite wrs)
                    (? !(charm? pid) (sh-nofound <argv)
                       (pid < 0) (cmderr (<argv + ": cannot spawn"))
                       (: _ (sh-waitfg [pid] (sh-unwords argv))
                          (sh-echk (sh-status ())))))))))
   (sh-fncall body argv)
     (: op (sh-pos ())
        _ (pin sh-pos () >argv)
        s (body ())
        _ (pin sh-pos () op)
        (? (two? s && id? <s 'ret)
           (: st >s _ (pin sh-status () st) st)
           s))

   ; --- the AST walk: plan once, then run ---------------------------------------
   ; (sh-plan ast) -> a PLAN, a thunk fired with the unit. every kind test, every
   ; descent and the assignment peel happen HERE, so a loop body, a function and a
   ; case arm pay them once instead of once per run. a plan still ANSWERS its
   ; control pair rather than jumping, so the fd restores between a `break` and
   ; its loop all run -- the reason this is not call-cc.
   (sh-zero _) (: _ (pin sh-status () 0) 0)
   (sh-eval a) ((sh-plan a) ())          ; the raw-AST door: a forked stage, a line
   ; a condition (or an && || arm, or a ! body) runs with errexit off
   (sh-guarded p)
     (: _ (pin sh-noe () (1 + (sh-noe ())))
        s (p ())
        _ (pin sh-noe () ((sh-noe ()) - 1))
        s)
   (sh-plan a)
     (? !a (\ _ (sh-status ()))
        (: k <a
           (? (id? k 'seq)
                (: p (sh-plan (sh-nth a 1)) q (sh-plan (sh-nth a 2))
                   (\ _ (: s (p ()) (? (two? s) s (q ())))))
              (id? k 'and)
                (: p (sh-plan (sh-nth a 1)) q (sh-plan (sh-nth a 2))
                   (\ _ (: s (sh-guarded p) (? (two? s) s (s = 0) (q ()) s))))
              (id? k 'or)
                (: p (sh-plan (sh-nth a 1)) q (sh-plan (sh-nth a 2))
                   (\ _ (: s (sh-guarded p) (? (two? s) s (s = 0) s (q ())))))
              (id? k 'not)
                (: p (sh-plan (sh-nth a 1))
                   (\ _ (: s (sh-guarded p)
                           (? (two? s) s
                              (: ns (? (s = 0) 1 0) _ (pin sh-status () ns) ns)))))
              (id? k 'bg)                        ; a lone command backgrounds as a
                (: b (sh-nth a 1)                ; pipeline of one
                   cmds (? (id? <b 'pipe) (sh-nth b 1) [b])
                   (\ _ (sh-pipeline cmds 1)))
              (id? k 'pipe) (: cmds (sh-nth a 1) (\ _ (sh-pipeline cmds 0)))
              (id? k 'cmd)  (: pk (sh-peel (sh-nth a 1) ()) rs (sh-nth a 2)
                               (\ _ (sh-simple pk rs)))
              (id? k 'if)
                (: cls (map (\ c (sh-plan <c >< sh-plan >c)) (sh-nth a 1))
                   els (? (two? (sh-nth a 2)) (sh-plan (sh-nth a 2)) sh-zero)
                   (\ _ (sh-if cls els)))
              (id? k 'while)
                (: c (sh-plan (sh-nth a 1)) b (sh-plan (sh-nth a 2))
                   (\ _ (sh-wloop 0 c b 0)))
              (id? k 'until)
                (: c (sh-plan (sh-nth a 1)) b (sh-plan (sh-nth a 2))
                   (\ _ (sh-wloop 1 c b 0)))
              (id? k 'for)                       ; the words re-expand each run
                (: nm (sh-nth a 1) ws (sh-nth a 2) b (sh-plan (sh-nth a 3))
                   (\ _ (sh-floop nm (catmap sh-fields ws) b 0)))
              (id? k 'case)                      ; a bodyless arm takes the zero at
                (: w (sh-nth a 1)                ; plan time -- an AST tells its own
                   its (map (\ it (link <it      ; absence by two?, a plan cannot
                                        (? (two? >it) (sh-plan >it) sh-zero)))
                            (sh-nth a 2))
                   (\ _ (sh-citems (sh-textof w) its)))
              (id? k 'group) (sh-plan (sh-nth a 1))
              (id? k 'subsh) (: b (sh-plan (sh-nth a 1)) (\ _ (sh-subsh b)))
              (id? k 'fun)   (: nm (sh-nth a 1) b (sh-plan (sh-nth a 2))
                                (\ _ (: _ (pin sh-funcs nm b) 0)))
              (id? k 'rdr)   (: b (sh-plan (sh-nth a 1)) rs (sh-nth a 2)
                                (\ _ (sh-rdr b rs)))
              (\ _ (cmderr "unknown form")))))
   (sh-if clauses els)
     (? !(two? clauses) (els ())
        (: c <clauses
           s (sh-guarded <c)
           (? (two? s) s
              (s = 0) (>c ())
              (sh-if >clauses els))))
   (sh-wloop inv cond body last)
     (: s (sh-guarded cond)
        (? (two? s) s
           (: go? (? inv !(s = 0) (s = 0))
              (? !go? (: _ (pin sh-status () last) last)
                 (: b (body ())
                    (? (two? b)
                       (: kk <b nn >b
                          (? (id? kk 'brk) (? (1 < nn) ('brk >< (nn - 1)) (sh-zero ()))
                             (id? kk 'cnt) (? (1 < nn) ('cnt >< (nn - 1))
                                              (sh-wloop inv cond body last))
                             b))
                       (sh-wloop inv cond body b)))))))
   (sh-floop nm fs body last)
     (? !(two? fs) (: _ (pin sh-status () last) last)
        (: _ (sh-setvar nm <fs)
           b (body ())
           (? (two? b)
              (: kk <b nn >b
                 (? (id? kk 'brk) (? (1 < nn) ('brk >< (nn - 1)) (sh-zero ()))
                    (id? kk 'cnt) (? (1 < nn) ('cnt >< (nn - 1)) (sh-floop nm >fs body last))
                    b))
              (sh-floop nm >fs body b))))
   (sh-anymatch t pats)
     (? !(two? pats) 0
        (: pm (sh-patmask <pats)
           (? (sh-fmatch <pm >pm t) 1 (sh-anymatch t >pats))))
   (sh-citems t items)
     (? !(two? items) (sh-zero ())
        (: it <items
           (? (sh-anymatch t <it) (>it ())
              (sh-citems t >items))))
   ; ⚠ under a forkless capture a `( )` does NOT fork: the child would inherit
   ; the drainer and stall on it, and the roster sh-subback puts back is already
   ; the subshell's. outside one the fork stands -- it is the reference reading,
   ; and it isolates the fds and the job table this lane cannot.
   (sh-subsh b)
     (? (sh-subfds ()) (sh-subhere b)
        (: _ (flush out) _ (flush err)
           p (fork 0)
           (? (p = 0) (: _ (wear ())         ; the child's stdio is its fds, never a jug this task wears
                         _ (sh-dropcaps ())  ; ...and it is inside no capture of ours
                         s (b ())
                         (quit (? (charm? s) s (? (id? <s 'ret) >s 0))))
              (p < 0) (cmderr "cannot fork")
              (: _ (sh-waitfg [p] (sh-curline ())) (sh-echk (sh-status ()))))))
   ; ⚠ the roster sh-subback puts back IS the subshell's contract, so it has to ride
   ; the raised way out too: a scare under the body would leak the body's cwd, vars,
   ; funcs, positionals, $0, flags and environment into the shell that ran it.
   (sh-subhere b)                            ; a subshell in our own process
     (: snap (sh-subsave ())
        s (mind b () (\ _ (sh-subback snap)))
        st (sh-capstat s)                    ; brk/cnt do not cross a subshell either
        _ (pin sh-status () st)
        (sh-echk st))
   ; a compound's trailing redirect: swap OUR OWN fds (dup saves, dup2 lays),
   ; run in-process (a `done < file` loop keeps its variables), swap back
   (sh-rdr b rs)
     (: rm (sh-rdmap rs)
        (? (id? <rm 'err) (cmderr >rm)
           (: fdmap (sh-nth rm 1) opens (sh-nth rm 2) wrs (sh-nth rm 3)
              _ (sh-hwrite wrs)                       ; a heredoc body fits the pipe
              saves (map (\ e (<e >< dup <e)) fdmap)
              _ (each fdmap (\ e (? (charm? >e) (dup2 >e <e)
                                    (fdclose <e))))
              _ (each opens fdclose)
              s (b ())
              _ (each saves (\ e (? (>e < 0) (fdclose <e)
                                    (: _ (dup2 >e <e) (fdclose >e)))))
              s)))

   ; --- command substitution (word.l's box) -------------------------------------
   ; an all-external pipeline spawns with the LAST stdout on our pipe; drain
   ; through fdopen, THEN reap -- any body size flows, and nothing forks.
   (sh-capsimple a wfd)                        ; -> pids, unwaited
     (: cmds (? (id? <a 'pipe) (sh-nth a 1) [a])
        sx (sh-stagex cmds ())
        (? (id? <sx 'err) (: _ (cmderr >sx) ())
           (: sts (sh-nth sx 1)
              n ((tally sts) - 1)
              pipes (sh-makepipes n)
              pfds (sh-flat pipes)
              ofds (catmap (\ st (sh-nth st 3)) sts)
              wrs (catmap (\ st (sh-nth st 4)) sts)
              allclose (wfd >< (pfds + (ofds + (map cap wrs))))
              pids (sh-spawnst sts pipes allclose wfd 0 n (0 - 1) 0 ())
              _ (each (pfds + ofds) fdclose)
              _ (sh-hwrite wrs)
              pids)))
   (sh-capst sts)                              ; the LAST stage's status, a signal folded down
     (? !(two? sts) 1
        (: st (last sts) (? (charm? st && (255 < st)) (st - 128) st)))
   ; --- which lane takes this body ----------------------------------------------
   ; three, and ONE look picks: the peel and the name are the same question for
   ; all of them, so sh-lane asks once and answers a nom.
   ;  'jug -- a no-effect builtin, so LOVE says the whole of it: a jug takes it
   ;          with no pipe, no fd, no child. ⚠ A JUG IS NOT AN FD, and that is the
   ;          boundary -- it holds what `out` is told and nothing a process writes,
   ;          so a body that could spawn (a function, anything with a body) is out.
   ;  'ext -- every stage a plain external: sh-capsimple already spawns those
   ;          straight onto the pipe, forkless, and nothing needs putting back.
   ;  'sub -- everything else, in-process down a pipe a task drains (sh-capsub).
   ; ⚠ the test is SYNTAX, asked before a single word expands (an expansion runs
   ; $( ) of its own, so it may happen once and only once) -- hence the bare
   ; literal: no ~, no glob, so the name is already its own text. an unreadable
   ; name is 'sub, the lane that holds anything.
   sh-purebi '("pwd" ":" "true" "false")
   (sh-barelit w)                              ; a word that is already its own text, or ()
     (? (!(two? w) || two? >w) ()
        (: h <w
           (? !(id? <h 'lit) ()
              (: t >h
                 (? (!(tally t) || ((t 0) = 126) || sh-anyglob t) () t)))))
   (sh-cmdname a)                              ; a 'cmd's bare name, or () if it has none to read
     (: pk (sh-peel (sh-nth a 1) ())
        (? (two? <pk) ()                       ; assignments want a subshell
           !(two? >pk) ()
           (sh-barelit <>pk)))
   (sh-extcmd? c)                              ; a stage sh-stagex would spawn as it stands
     (? !(id? <c 'cmd) ()
        (: nm (sh-cmdname c) (? !nm () (&& !(sh-bi? nm) !(sh-fn? nm)))))
   (sh-lane a)
     (? (id? <a 'pipe) (? (all sh-extcmd? (sh-nth a 1)) 'ext 'sub)
        !(id? <a 'cmd) 'sub
        (: nm (sh-cmdname a)
           (? !nm 'sub
              (sh-fn? nm) 'sub
              !(sh-bi? nm) 'ext
              (&& !(two? (sh-nth a 2)) (sh-smem? nm sh-purebi)) 'jug   ; a redirect wants an fd
              'sub)))
   ; re-seat STDOUT alone -- `wear` replaces the whole triple, so an in or err this
   ; task already wears has to ride along by name
   (sh-wearout o)
     (: w (worn ())
        (wear [(? (two? w) <w ())
                 o
                 (? (&& (two? w) (two? >w) (two? >>w)) <>>w ())]))
   (sh-capstat st)                             ; a plan's answer read as a $( ) status
     (? (charm? st) st
        ((two? st) && ((id? <st 'ret) || (id? <st 'xit))) >st
        0)
   ; the re-seat is MINDED: a scare under the body leaves this task wearing a jug
   ; nobody drains, and every later word the shell says lands in it instead of the
   ; terminal. one capture pays the mind, not one node -- sh-guarded stays bare.
   (sh-capjug a)
     (: j (jug 0)
        w (worn ())
        _ (sh-wearout j)
        st (mind (\ _ (sh-guarded (\ _ (sh-eval a))))   ; the body's status is the value,
                 () (\ _ (wear w)))                     ; ..never our errexit
        _ (pin sh-substat () (sh-capstat st))
        (slurp j))

   ; --- the forkless capture: the body runs HERE, down a pipe a TASK drains -----
   ; a child process needs an FD, so this lane is a real pipe with our own stdout
   ; dup2'd onto it -- and the DRAINER is what makes that safe. a pipe holds one
   ; buffer and the only reader is the shell, which is busy running the body, so a
   ; body that says more than that would stop forever. a task parks on the read end
   ; and empties it as the body fills it; the two never both stop, because every
   ; wait in this tree parks its TASK and not the process (tools/waits.l).
   ; ⚠ POSIX runs $( ) in a SUBSHELL, and forkless there is nobody to copy the
   ; state -- sh-subsave/sh-subback do it by hand, and that roster IS the contract:
   ; cwd, variables, functions, positionals, $0, the e/u/x flags, the environment.
   ; what a fork would also have taken and this does not: a job started here stays
   ; the shell's, an fd opened here stays open, umask and the handlers stand.
   ; ⚠ a background child inside holds the write end, so the drain sees no eof
   ; until it dies -- the wait `$(sleep 5 &)` has in every shell, and the same one.
   ; ⚠ and a FORK under one of these bodies takes the drainer by copy, so the
   ; child would read its parent's capture: shut every inherited one down before
   ; the child runs a step -- close the read end, then join the task, which now
   ; reads the end at once. closing without joining leaves the fd number free for
   ; the body to reopen, and the drainer would wake onto whatever landed there.
   (sh-dropcaps _)
     (: _ (each (sh-subfds ()) (\ e (: _ (fdclose <e) _ (catch >e) 0)))
        _ (pin sh-subfds () ())
        ; ⚠ and NO TRAP of its parent's: POSIX resets a caught condition in a
        ; subshell, and a `( exit )` that ran the EXIT action would take the
        ; parent's temp dir out from under it while the parent was still working.
        ; sh-disarm drops the parent's signalfd and EMPTIES THE MASK with it --
        ; a subshell that inherited the block would be a subshell ^C cannot reach.
        _ (each (keys sh-traps) (\ k (pull sh-traps k 0)))
        _ (sh-disarm 0)
        0)
   (sh-tabsnap t) (map (\ k (k >< (t k))) (keys t))
   (sh-tabback t sn)
     (: _ (each (keys t) (\ k (pull t k 0)))
        _ (each sn (\ e (pin t <e >e)))
        0)
   (sh-envback old)                            ; unset what appeared, then re-lay the old
     (: now (environ 0)
        (? (now = old) 0
           (: _ (each now (\ e (? (sh-smem? e old) 0
                                  (: p (sh-eqpos e 0)
                                     (setenv (? !p e (snip e 0 p)) ())))))
              _ (each old (\ e (: p (sh-eqpos e 0)
                                  (? !p 0
                                     (setenv (snip e 0 p) (snip e (p + 1) (tally e)))))))
              0)))
   (sh-subsave _)
     [(cwd 0) (sh-tabsnap sh-vars) (sh-tabsnap sh-funcs) (sh-pos ()) (sh-arg0 ())
        [(sh-eflag ()) (sh-uflag ()) (sh-xflag ())] (environ 0)]
   (sh-subback s)
     (: _ (chdir (sh-nth s 0))
        _ (sh-tabback sh-vars (sh-nth s 1))
        _ (sh-tabback sh-funcs (sh-nth s 2))
        _ (pin sh-pos () (sh-nth s 3))
        _ (pin sh-arg0 () (sh-nth s 4))
        f (sh-nth s 5)
        _ (pin sh-eflag () (sh-nth f 0))
        _ (pin sh-uflag () (sh-nth f 1))
        _ (pin sh-xflag () (sh-nth f 2))
        _ (sh-envback (sh-nth s 6))
        0)

   ; --- one line, run the way a subshell would: the door a caller in this image
   ; uses instead of spawning a shell (cook's recipe lines). a make recipe line
   ; runs in ITS OWN shell, so a `cd` in one must not reach the next -- this is the
   ; forkless capture's contract with the capture taken off: the body says its piece
   ; straight onto OUR fds, and sh-subsave/sh-subback put back by hand what a fork
   ; would have copied (cwd, variables, functions, positionals, $0, the e/u/x flags,
   ; the environment). ⚠ the same holes stand: a job started inside stays ours, an
   ; fd opened inside stays open, umask and the handlers hold. `exit` ends the LINE.
   ; answers (1 status) when it ran here, () when the flags ask for something this
   ; door does not serve -- the caller then spawns a shell, exactly as it always did.
   ; ⚠ the restore rides `mind`, so a scare under the line still puts the shell back.
   (sh-oneflags fl)                            ; ("-ec") -> (1 e u x) | () not ours
     (: (go l e u x)
         (? !(two? l) [1 e u x]
            (: w <l n (tally w)
               (? (|| (n < 2) !((w 0) = 45)) ()
                  (: (ch k e2 u2 x2)
                      (? (n <= k) (go >l e2 u2 x2)
                         (: c (w k)
                            (? (c = 99) (ch (k + 1) e2 u2 x2)      ; c, the command marker
                               (c = 101) (ch (k + 1) 1 u2 x2)
                               (c = 117) (ch (k + 1) e2 1 x2)
                               (c = 120) (ch (k + 1) e2 u2 1)
                               ())))                              ; anything else is not ours
                     (ch 1 e u x)))))
        (go fl 0 0 0))
   (sh-oneline fl src)
     (: fs (sh-oneflags fl)
        (? !(two? fs) ()
           (: sv (sh-subsave ())
              _ (pin sh-online () (1 + (sh-online ())))
              _ (pin sh-eflag () (sh-nth fs 1))
              _ (pin sh-uflag () (sh-nth fs 2))
              _ (pin sh-xflag () (sh-nth fs 3))
              ; the body under BOTH: mind puts the shell back either way (the restore
              ; is the resource's, not the catcher's), trap turns sh-leave's scare
              ; into the line's status. any OTHER condition is a real fault -- say it
              ; and let the line fail, so a caller aborts loudly rather than silently.
              r (trap (\ _ (mind (\ _ (sh-line src)) ()
                                 (\ _ (: _ (pin sh-online () ((sh-online ()) - 1))
                                         _ (flush out) _ (flush err)
                                         (sh-subback sv)))))
                      ()
                      (\ a b (? (id? a 'sh-exit) (? (charm? b) b 1)
                                (: _ (say err ";; ") _ (print err a) _ (say err " ")
                                   _ (print err b) _ (put err 10) _ (flush err) 1))))
              [1 (? (two? r) (? (id? <r 'more)
                                   (: _ (say err "lush: unexpected eof in a recipe line")
                                      _ (put err 10) 2)
                                   (? (charm? >r) >r 1))
                      (charm? r) r
                      0)])))
   (sh-capsub a)
     (: pp (pipe 0)
        (? (charm? pp) (: _ (cmderr "cannot pipe") "")
           (: rfd <pp wfd >pp
              d (twirl (\ _ (: p (fdopen rfd) t (slurp p) _ (close p) t)) 0)
              _ (flush out)                    ; the bytes already in hand are the CALLER's
              sv (dup 1)
              _ (dup2 wfd 1)
              caps (sh-subfds ())
              _ (pin sh-subfds () ((rfd >< d) >< caps))
              snap (sh-subsave ())
              ; ⚠ the whole seat is MINDED: nothing here is reclaimed by anything else.
              ; sv and wfd are RAW fds -- fdclose is the only door, and a port's
              ; finalizer never sees them -- our stdout is dup2'd onto the pipe, and the
              ; drain has no other reader. a scare under the body left the shell talking
              ; into a pipe nobody empties, which at the next prompt reads as a dead shell.
              ; the join lands in `got` because it belongs to the teardown: a second catch
              ; answers the zero point, so it can happen in ONE place, and that place has
              ; to be the one both ways out share.
              got (tablet 0)
              st (mind (\ _ (sh-guarded (\ _ (sh-eval a)))) ()
                       (\ _ (: _ (flush out)         ; the body's bytes, before the ends go
                                _ (sh-subback snap)
                                _ (pin sh-subfds () caps)
                                ; fd 1 is a write end TOO (the dup2 above), so it has to be
                                ; put back before the drain can ever read eof
                                _ (dup2 sv 1) _ (fdclose sv)
                                _ (fdclose wfd)      ; the last write end: now eof
                                _ (pin got 0 (catch d))
                                0)))
              _ (pin sh-substat () (sh-capstat st))
              (got 0))))

   ; every lane leaves sh-substat holding this body's status: an assignment with
   ; no command name answers the LAST one performed (POSIX), so the pin is the
   ; only way back -- sh-fields sits between here and the reader. 1 up front is
   ; the floor the three scaring lanes keep.
   (sh-cmdsub text)
     (: _ (pin sh-substat () 1)
        lx (sh-lex text)
        (? (id? <lx 'more) (: _ (cmderr "unterminated $( ) body") "")
           (: pr (sh-parse >lx)
              (? !(id? <pr 'ok) (: _ (cmderr "bad $( ) body") "")
                 !>pr (: _ (pin sh-substat () 0) "")           ; $( ) is a clean nothing
                 (: a >pr ln (sh-lane a)
                    (? (id? ln 'jug) (sh-capjug a)
                       (id? ln 'sub) (sh-capsub a)
                       (: pp (pipe 0)
                          (? (charm? pp) ""
                             (: rfd <pp wfd >pp
                                ; ⚠ GUARDED: the body's status is this
                                ; substitution's value, never the shell's own
                                ; errexit trigger -- aborting it would drop the
                                ; very output the caller asked for.
                                (sh-guarded
                                  (\ _ (: pids (sh-capsimple a wfd)
                                          _ (fdclose wfd)
                                          o (slurp (fdopen rfd))   ; drain, THEN reap
                                          _ (pin sh-substat ()
                                               (sh-capst (map (\ p (wait p)) pids)))
                                          o))))))))))))
   _ (pin sh-subeval () sh-cmdsub)

   ; --- the script lane + the line entries --------------------------------------
   (sh-lines port buf)
     (: line (sh-getline port)
        (? (id? line 'eof)
           (? (tally buf)
              (: _ (say err "lush: unexpected eof\n") _ (pin sh-status () 2) 0)
              0)
           (: full (? (tally buf) (buf + ("\n" + line)) line)
              r (sh-line full)
              ; a signal that arrived while no child was up has nobody to end its
              ; wait, so the line boundary is the other place it is read
              _ (sh-sigdrain 0)
              (? (two? r) (sh-lines port full)
                 (sh-retf ()) (: _ (pin sh-retf () 0) 0)   ; dot-return: stop THIS file
                 (sh-lines port "")))))
   (sh-parse-line src)
     (: lx (sh-lex src)
        (? (id? <lx 'more) lx (sh-parse >lx)))
   (sh-eval-top src a)
     (: _ (pin sh-curline () src)
        s (sh-eval a)
        (? (charm? s) s
           ; a `return` that reached the top: flag it so the READING loop stops
           ; its file (POSIX dot-return; sh-lines clears the flag) -- $? is the
           ; return's own. a stray brk/cnt still swallows.
           (two? s && id? <s 'ret)
             (: _ (pin sh-retf () 1) _ (pin sh-status () >s) >s)
           0))
   (sh-line src)
     (: pr (sh-parse-line src)
        (? (id? <pr 'more) pr
           (id? <pr 'err)
             (: _ (say err ("lush: " + (>pr + "\n"))) _ (pin sh-status () 2) 2)
           (sh-eval-top src >pr))))
) ; module 'lush
