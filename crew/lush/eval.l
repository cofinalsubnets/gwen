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

(: ; export NAME=VAL -> setenv; export NAME promotes the shell var
   (sh-eqpos s i) (? (= i (tally s)) () (? (= (s i) 61) i (sh-eqpos s (+ i 1))))
   (sh-export w)
     (: p (sh-eqpos w 0)
        (? (nil? p) (setenv w (sh-vval w))
           (setenv (snip w 0 p) (snip w (+ p 1) (tally w)))))

   (cmderr msg) (: _ (say err (+ "lush: " (+ msg "\n"))) _ (pin sh-status () 1) 1)

   ; --- redirects -> the fdmap ------------------------------------------------
   ; (sh-rdmap redirs) -> (1 fdmap opens writes) | ('err . msg). opens close in
   ; the child AND in the parent after spawn; writes ((wfd . body)..) are
   ; heredoc feeds -- the parent pours and closes them after the spawn.
   (dfd ion d) (? (two? ion) (cap ion) d)
   (rderr opens msg) (: _ (each opens fdclose) (link 'err msg))
   (sh-rdmap rs) (rdm rs () () ())
   (rdm rs fdmap opens writes)
     (? !rs (L 1 (sh-rev fdmap) opens writes)
        (: r (cap rs) ion (sh-nth r 0) op (sh-nth r 1)
           (? (= op "<h")
              (: pp (pipe 0)
                 (? (charm? pp) (rderr opens "cannot pipe")
                    (rdm (cup rs)
                         (link (link (dfd ion 0) (cap pp)) fdmap)
                         (link (cap pp) opens)
                         (link (link (cup pp) (sh-textof (sh-nth r 2))) writes))))
              (: tf (sh-fields (sh-nth r 2))
                 (? (|| (! (two? tf)) (two? (cup tf))) (rderr opens "ambiguous redirect")
                    (: t (cap tf)
                       (? (= op "<")
                            (: fd (openfd t 0)
                               (? (< fd 0) (rderr opens (+ t ": cannot open"))
                                  (rdm (cup rs) (link (link (dfd ion 0) fd) fdmap) (link fd opens) writes)))
                          (|| (= op ">") (= op ">>"))
                            (: fd (openfd t (? (= op ">") 1 2))
                               (? (< fd 0) (rderr opens (+ t ": cannot open"))
                                  (rdm (cup rs) (link (link (dfd ion 1) fd) fdmap) (link fd opens) writes)))
                          (|| (= op ">&") (= op "<&"))
                            (: d (dfd ion (? (= op "<&") 0 1))
                               (? (= t "-") (rdm (cup rs) (link (link d ()) fdmap) opens writes)
                                  (sh-digits? t) (rdm (cup rs) (link (link d (sh-int t)) fdmap) opens writes)
                                  (rderr opens (+ t ": bad fd"))))
                          (rderr opens (+ op ": unsupported")))))))))
   (sh-hwrite wrs)
     (each wrs (\ e (: p (fdopen (cap e)) _ (say p (cup e)) (close p))))

   ; --- assignments: NAME=value words at the head -------------------------------
   (sh-asnname? s)
     (: n (tally s)
        (? (! n) 0 (! (sh-name1? (s 0))) 0
           (: (go i) (? (= i n) 1 (? (sh-name? (s i)) (go (+ i 1)) 0)) (go 1))))
   (sh-asn? segs)
     (? (! (two? segs)) ()
        (: h (cap segs)
           (? (! (id? (cap h) 'lit)) ()
              (: t (cup h) p (sh-eqpos t 0)
                 (? (nil? p) ()
                    (! (sh-asnname? (snip t 0 p))) ()
                    (link (snip t 0 p)
                          (: rest (snip t (+ p 1) (tally t))
                             (? (tally rest) (link (link 'lit rest) (cup segs)) (cup segs)))))))))
   (sh-peel ws acc)
     (? (two? ws)
        (: a (sh-asn? (cap ws))
           (? (two? a) (sh-peel (cup ws) (link a acc)) (link (sh-rev acc) ws)))
        (link (sh-rev acc) ws))
   (sh-doasn a) (sh-setvar (cap a) (sh-textof (cup a)))
   (sh-tmpset a)                                ; temporary env around one command
     (: nm (cap a) old (getenv nm)
        _ (setenv nm (sh-textof (cup a)))
        (link nm old))
   (sh-tmprestore e) (setenv (cap e) (cup e))   ; () old unsets -- the absence lane

   ; --- builtins: a charm status, a control pair (brk/cnt/ret), or 'nope --------
   sh-binames '("exit" "cd" "export" "pwd" "jobs" "fg" "bg" ":" "true" "false"
                "read" "set" "shift" "unset" "eval" "." "wait" "local"
                "break" "continue" "return")
   (sh-bi? nm) (sh-smem? nm sh-binames)
   (sh-readb args)
     (: args2 (? (&& (two? args) (= (cap args) "-r")) (cup args) args)
        nms (? (two? args2) args2 (L "REPLY"))
        ln (sh-getline in)
        (? (id? ln 'eof) 1 (: _ (sh-readset nms ln 0 (tally ln)) 0)))
   (sh-readset nms ln i n)
     (: i2 (skifs ln i n)
        (? (! (two? (cup nms))) (sh-setvar (cap nms) (sh-rtrim (snip ln i2 n)))
           (: j (chunk ln i2 n)
              _ (sh-setvar (cap nms) (snip ln i2 j))
              (sh-readset (cup nms) ln j n))))
   (sh-rtrim s)
     (: (go k) (? (&& (< 0 k) (sh-ifs? (s (- k 1)))) (go (- k 1)) (snip s 0 k))
        (go (tally s)))
   (sh-setb args)
     (? (! (two? args)) 0
        (: a (cap args)
           (? (= a "--") (: _ (pin sh-pos () (cup args)) 0)
              (&& (< 1 (tally a)) (|| (= (a 0) 45) (= (a 0) 43)))
                (: on (? (= (a 0) 45) 1 0)
                   (go i) (? (= i (tally a)) (sh-setb (cup args))
                             (: b (a i)
                                _ (? (= b 101) (pin sh-eflag () on)
                                     (= b 117) (pin sh-uflag () on)
                                     (= b 120) (pin sh-xflag () on)
                                     0)
                                (go (+ i 1))))
                   (go 1))
              (: _ (pin sh-pos () args) 0))))
   (sh-dotb args)
     (? (! (two? args)) (cmderr ". wants a file")
        (: f (cap args)
           (? (two? (stat f))
              (: _ (pin sh-retf () 0)            ; a stale interactive `return` must not clip the file
                 p (open f "r") _ (sh-lines p "") _ (close p) (sh-status ()))
              (cmderr (+ f ": not found")))))
   (sh-localb args)
     (: _ (each args (\ w (: p (sh-eqpos w 0)
                             (? (nil? p) (pin sh-vars w "")
                                (pin sh-vars (snip w 0 p) (snip w (+ p 1) (tally w)))))))
        0)
   (sh-waitb args)                              ; wait [pid..] -> the LAST one's status
     (? (two? args)
        (foldl (\ a p (: st (wait (sh-int p))
                         _ (sh-bgnote (sh-int p))
                         (? (&& (charm? st) (< st 0)) 127 st)))
               0 args)
        (: _ (each (keys sh-jobs)
                   (\ nn (: j (sh-jobs nn)
                            (? !j 0 (: _ (each (sh-nth j 0) (\ p (wait p)))
                                       (pull sh-jobs nn 0))))))
           0))
   (sh-builtin argv)
     (: cmd (cap argv)
        (? (= cmd "exit") (quit (? (cup argv) (sh-int (caup argv)) (sh-status ())))
           (= cmd "cd")
             (: t (? (cup argv) (caup argv) (+ "" (getenv "HOME")))
                r (chdir t)
                (? (nil? r) 0 (: _ (say err (+ "lush: cd: " (+ t "\n"))) 1)))
           (= cmd "export") (: _ (each (cup argv) sh-export) 0)
           (= cmd "pwd")  (: _ (puts (+ (cwd 0) "\n")) 0)
           (= cmd "jobs") (: _ (each (sort (keys sh-jobs))
                                     (\ n (puts (sh-jobline n (sh-jobs n))))) 0)
           (= cmd "fg")   (: _ (sh-fg (sh-jobn argv)) (sh-status ()))
           (= cmd "bg")   (sh-bg (sh-jobn argv))
           (= cmd ":")    0
           (= cmd "true") 0
           (= cmd "false") 1
           (= cmd "read") (sh-readb (cup argv))
           (= cmd "set")  (sh-setb (cup argv))
           (= cmd "shift") (: n (? (cup argv) (sh-int (caup argv)) 1)
                              _ (pin sh-pos () (drop n (sh-pos ()))) 0)
           (= cmd "unset") (: _ (each (cup argv)
                                      (\ nm (: _ (pull sh-vars nm 0) (setenv nm ())))) 0)
           (= cmd "eval") (: c (sh-unwords (cup argv))
                             (? (tally c)
                                (: r (sh-line c) (? (two? r) (cmderr "eval: unexpected eof") r))
                                0))
           (= cmd ".")    (sh-dotb (cup argv))
           (= cmd "local") (sh-localb (cup argv))
           (= cmd "wait") (sh-waitb (cup argv))
           (= cmd "break") (link 'brk (? (cup argv) (sh-int (caup argv)) 1))
           (= cmd "continue") (link 'cnt (? (cup argv) (sh-int (caup argv)) 1))
           (= cmd "return") (link 'ret (? (cup argv) (sh-int (caup argv)) (sh-status ())))
           'nope))

   ; --- the function book -------------------------------------------------------
   ; a body is a PLAN (a closure), so two? cannot tell one from absence -- peep
   ; names the miss with a mint no stored value can pose as (spec.l's total test)
   (sh-fnof nm) (peep sh-funcs nm sh-nofn)
   (sh-fn? nm) (! (id? sh-nofn (sh-fnof nm)))

   ; --- stage expansion + the spawn engines ------------------------------------
   (sh-makepipes n) (? (< n 1) () (link (pipe 0) (sh-makepipes (- n 1))))
   (sh-flat ps) (? !ps () (link (cap (cap ps)) (link (cup (cap ps)) (sh-flat (cup ps)))))
   ; a stage: ('x argv fdmap opens writes) spawns; ('a ast () () ()) forks --
   ; compounds, builtins, functions and assignment-bearing commands all fork
   ; (the child re-expands, so its env sees its own assignments)
   (sh-stagex cmds acc)
     (? !cmds (L 1 (sh-rev acc))
        (: c (cap cmds)
           (? (! (id? (cap c) 'cmd))
              (sh-stagex (cup cmds) (link (L 'a c () () ()) acc))
              (: pk (sh-peel (sh-nth c 1) ())
                 argv (catmap sh-fields (cup pk))
                 (? (|| (two? (cap pk)) (! (two? argv))
                        (sh-bi? (cap argv)) (sh-fn? (cap argv)))
                    (sh-stagex (cup cmds) (link (L 'a c () () ()) acc))
                    (: rm (sh-rdmap (sh-nth c 2))
                       (? (id? (cap rm) 'err) (: _ (stclean acc) rm)
                          (sh-stagex (cup cmds)
                                     (link (L 'x argv (sh-nth rm 1) (sh-nth rm 2) (sh-nth rm 3)) acc)))))))))
   (stclean acc) (each acc (\ st (each (sh-nth st 3) fdclose)))
   ; spawn one stage: external -> spawnmap; anything else -> fork, the child
   ; lays the pipe map on itself (dup2), closes the plumbing, evals, quits
   (sh-spawn1 st base allclose pg fgi)
     (? (id? (sh-nth st 0) 'x)
        (spawnmap (sh-nth st 1) (+ base (sh-nth st 2)) allclose pg fgi)
        (: _ (flush out) _ (flush err)
           p (fork 0)
           (? (= p 0)
              (: _ (each base (\ e (dup2 (cup e) (cap e))))
                 _ (each allclose (\ fd (? (< 2 fd) (fdclose fd) 0)))
                 s (sh-eval (sh-nth st 1))
                 (quit (? (charm? s) s (? (id? (cap s) 'ret) (cup s) 0))))
              p)))
   ; stage 0 may lead a pgrp + take the tty, the rest join; base map = pipe
   ; wiring, a stage's own redirects land after. lastout >= 0 sends the LAST
   ; stage's stdout there (the cmdsub lane).
   (sh-spawnst sts pipes allclose lastout i last pg fg pids)
     (? !sts (sh-rev pids)
        (: st (cap sts)
           base (+ (? (< 0 i) (L (link 0 (cap (sh-nth pipes (- i 1))))) ())
                   (? (< i last) (L (link 1 (cup (sh-nth pipes i))))
                      (? (< lastout 0) () (L (link 1 lastout)))))
           pid (sh-spawn1 st base allclose pg (? (= i 0) fg 0))
           (sh-spawnst (cup sts) pipes allclose lastout (+ i 1) last
                       (? (< pg 0) pg (? (= i 0) pid pg)) fg (link pid pids))))
   (sh-echk st)
     (? (&& (charm? st) (! (= st 0)) (sh-eflag ()) (= 0 (sh-noe ())) (! (sh-tty ())))
        (quit st) st)
   (sh-pipeline cmds bg)
     (: sx (sh-stagex cmds ())
        (? (id? (cap sx) 'err) (cmderr (cup sx))
           (: sts (sh-nth sx 1)
              n (- (tally sts) 1)
              pipes (sh-makepipes n)
              pfds (sh-flat pipes)
              ofds (catmap (\ st (sh-nth st 3)) sts)
              wrs (catmap (\ st (sh-nth st 4)) sts)
              allclose (+ pfds (+ ofds (map cap wrs)))
              pids (sh-spawnst sts pipes allclose (0 - 1) 0 n
                               (sh-pgm 0) (? bg 0 (sh-fgm 0)) ())
              _ (each (+ pfds ofds) fdclose)
              _ (sh-hwrite wrs)
              (? bg
                 (: jn (sh-addjob pids 'running (+ (sh-curline ()) " &"))
                    _ (pin sh-lastbg () (last pids))      ; $!
                    _ (? (sh-tty ()) (puts (+ "[" (+ (show jn) (+ "] " (+ (sh-unwords (map show pids)) "\n"))))) 0)
                    0)
                 (: _ (sh-waitfg pids (sh-curline ())) (sh-echk (sh-status ())))))))
   ; one simple foreground command: assignments, function, builtin, or a spawn.
   ; pk is the peel -- (assignments . words) -- split at PLAN time, it is syntax
   (sh-simple pk rs)
     (: asns (cap pk)
        _ (pin sh-substat () 0)          ; ⚠ ahead of BOTH expansions -- see below
        argv (catmap sh-fields (cup pk))
        (? (! (two? argv))
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
     (? (! (two? rs)) (th ())
        (: rm (sh-rdmap rs)
           (? (id? (cap rm) 'err) (cmderr (cup rm))
              (: fdmap (sh-nth rm 1) opens (sh-nth rm 2) wrs (sh-nth rm 3)
                 _ (sh-hwrite wrs)
                 saves (map (\ e (link (cap e) (dup (cap e)))) fdmap)
                 _ (each fdmap (\ e (? (charm? (cup e)) (dup2 (cup e) (cap e))
                                       (fdclose (cap e)))))
                 _ (each opens fdclose)
                 s (th ())
                 _ (flush out) _ (flush err)
                 _ (each saves (\ e (? (< (cup e) 0) (fdclose (cap e))
                                       (: _ (dup2 (cup e) (cap e)) (fdclose (cup e))))))
                 s))))
   (sh-simple2 argv rs)
     (: _ (? (sh-xflag ()) (say err (+ "+ " (+ (sh-unwords argv) "\n"))) 0)
        fn (sh-fnof (cap argv))
        (? (! (id? sh-nofn fn)) (sh-withrs rs (\ _ (sh-fncall fn argv)))
           (sh-bi? (cap argv))
             (: b (sh-withrs rs (\ _ (sh-builtin argv)))
                (? (two? b) b                            ; brk/cnt/ret ride out
                   (charm? b) (: _ (pin sh-status () b) (sh-echk b))
                   b))
           (: rm (sh-rdmap rs)
              (? (id? (cap rm) 'err) (cmderr (cup rm))
                 (: opens (sh-nth rm 2) wrs (sh-nth rm 3)
                    pid (spawnmap argv (sh-nth rm 1) (+ opens (map cap wrs))
                                  (sh-pgm 0) (sh-fgm 0))
                    _ (each opens fdclose)
                    _ (sh-hwrite wrs)
                    (? (< pid 0) (cmderr (+ (cap argv) ": cannot spawn"))
                       (: _ (sh-waitfg (L pid) (sh-unwords argv))
                          (sh-echk (sh-status ())))))))))
   (sh-fncall body argv)
     (: op (sh-pos ())
        _ (pin sh-pos () (cup argv))
        s (body ())
        _ (pin sh-pos () op)
        (? (&& (two? s) (id? (cap s) 'ret))
           (: st (cup s) _ (pin sh-status () st) st)
           s))

   ; --- the AST walk: plan once, then run ---------------------------------------
   ; (sh-plan ast) -> a PLAN, a thunk fired with the unit. every kind test, every
   ; descent and the assignment peel happen HERE, so a loop body, a function and a
   ; case arm pay them once instead of once per run. a plan still ANSWERS its
   ; control pair rather than jumping, so the fd restores between a `break` and
   ; its loop all run -- the reason this is not call-cc (doc/honest-face.md).
   (sh-zero _) (: _ (pin sh-status () 0) 0)
   (sh-eval a) ((sh-plan a) ())          ; the raw-AST door: a forked stage, a line
   ; a condition (or an && || arm, or a ! body) runs with errexit off
   (sh-guarded p)
     (: _ (pin sh-noe () (+ 1 (sh-noe ())))
        s (p ())
        _ (pin sh-noe () (- (sh-noe ()) 1))
        s)
   (sh-plan a)
     (? !a (\ _ (sh-status ()))
        (: k (cap a)
           (? (id? k 'seq)
                (: p (sh-plan (sh-nth a 1)) q (sh-plan (sh-nth a 2))
                   (\ _ (: s (p ()) (? (two? s) s (q ())))))
              (id? k 'and)
                (: p (sh-plan (sh-nth a 1)) q (sh-plan (sh-nth a 2))
                   (\ _ (: s (sh-guarded p) (? (two? s) s (= s 0) (q ()) s))))
              (id? k 'or)
                (: p (sh-plan (sh-nth a 1)) q (sh-plan (sh-nth a 2))
                   (\ _ (: s (sh-guarded p) (? (two? s) s (= s 0) s (q ())))))
              (id? k 'not)
                (: p (sh-plan (sh-nth a 1))
                   (\ _ (: s (sh-guarded p)
                           (? (two? s) s
                              (: ns (? (= s 0) 1 0) _ (pin sh-status () ns) ns)))))
              (id? k 'bg)                        ; a lone command backgrounds as a
                (: b (sh-nth a 1)                ; pipeline of one
                   cmds (? (id? (cap b) 'pipe) (sh-nth b 1) (L b))
                   (\ _ (sh-pipeline cmds 1)))
              (id? k 'pipe) (: cmds (sh-nth a 1) (\ _ (sh-pipeline cmds 0)))
              (id? k 'cmd)  (: pk (sh-peel (sh-nth a 1) ()) rs (sh-nth a 2)
                               (\ _ (sh-simple pk rs)))
              (id? k 'if)
                (: cls (map (\ c (link (sh-plan (cap c)) (sh-plan (cup c)))) (sh-nth a 1))
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
                   its (map (\ it (link (cap it) ; absence by two?, a plan cannot
                                        (? (two? (cup it)) (sh-plan (cup it)) sh-zero)))
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
     (? (! (two? clauses)) (els ())
        (: c (cap clauses)
           s (sh-guarded (cap c))
           (? (two? s) s
              (= s 0) ((cup c) ())
              (sh-if (cup clauses) els))))
   (sh-wloop inv cond body last)
     (: s (sh-guarded cond)
        (? (two? s) s
           (: go? (? inv (! (= s 0)) (= s 0))
              (? (! go?) (: _ (pin sh-status () last) last)
                 (: b (body ())
                    (? (two? b)
                       (: kk (cap b) nn (cup b)
                          (? (id? kk 'brk) (? (< 1 nn) (link 'brk (- nn 1)) (sh-zero ()))
                             (id? kk 'cnt) (? (< 1 nn) (link 'cnt (- nn 1))
                                              (sh-wloop inv cond body last))
                             b))
                       (sh-wloop inv cond body b)))))))
   (sh-floop nm fs body last)
     (? (! (two? fs)) (: _ (pin sh-status () last) last)
        (: _ (sh-setvar nm (cap fs))
           b (body ())
           (? (two? b)
              (: kk (cap b) nn (cup b)
                 (? (id? kk 'brk) (? (< 1 nn) (link 'brk (- nn 1)) (sh-zero ()))
                    (id? kk 'cnt) (? (< 1 nn) (link 'cnt (- nn 1)) (sh-floop nm (cup fs) body last))
                    b))
              (sh-floop nm (cup fs) body b))))
   (sh-anymatch t pats)
     (? (! (two? pats)) 0
        (: pm (sh-patmask (cap pats))
           (? (sh-fmatch (cap pm) (cup pm) t) 1 (sh-anymatch t (cup pats)))))
   (sh-citems t items)
     (? (! (two? items)) (sh-zero ())
        (: it (cap items)
           (? (sh-anymatch t (cap it)) ((cup it) ())
              (sh-citems t (cup items)))))
   (sh-subsh b)
     (: _ (flush out) _ (flush err)
        p (fork 0)
        (? (= p 0) (: s (b ())
                      (quit (? (charm? s) s (? (id? (cap s) 'ret) (cup s) 0))))
           (< p 0) (cmderr "cannot fork")
           (: _ (sh-waitfg (L p) (sh-curline ())) (sh-echk (sh-status ())))))
   ; a compound's trailing redirect: swap OUR OWN fds (dup saves, dup2 lays),
   ; run in-process (a `done < file` loop keeps its variables), swap back
   (sh-rdr b rs)
     (: rm (sh-rdmap rs)
        (? (id? (cap rm) 'err) (cmderr (cup rm))
           (: fdmap (sh-nth rm 1) opens (sh-nth rm 2) wrs (sh-nth rm 3)
              _ (sh-hwrite wrs)                       ; a heredoc body fits the pipe
              saves (map (\ e (link (cap e) (dup (cap e)))) fdmap)
              _ (each fdmap (\ e (? (charm? (cup e)) (dup2 (cup e) (cap e))
                                    (fdclose (cap e)))))
              _ (each opens fdclose)
              s (b ())
              _ (each saves (\ e (? (< (cup e) 0) (fdclose (cap e))
                                    (: _ (dup2 (cup e) (cap e)) (fdclose (cup e))))))
              s)))

   ; --- command substitution (word.l's box) -------------------------------------
   ; a single pipeline/command spawns with the LAST stdout on our pipe; drain
   ; through fdopen, THEN reap -- any body size flows. sequencing forms (a;b,
   ; a&&b) reap per step instead (the pipe-buffer caveat rides with them).
   (sh-capsimple a wfd)                        ; -> pids, unwaited
     (: cmds (? (id? (cap a) 'pipe) (sh-nth a 1) (L a))
        sx (sh-stagex cmds ())
        (? (id? (cap sx) 'err) (: _ (cmderr (cup sx)) ())
           (: sts (sh-nth sx 1)
              n (- (tally sts) 1)
              pipes (sh-makepipes n)
              pfds (sh-flat pipes)
              ofds (catmap (\ st (sh-nth st 3)) sts)
              wrs (catmap (\ st (sh-nth st 4)) sts)
              allclose (link wfd (+ pfds (+ ofds (map cap wrs))))
              pids (sh-spawnst sts pipes allclose wfd 0 n (0 - 1) 0 ())
              _ (each (+ pfds ofds) fdclose)
              _ (sh-hwrite wrs)
              pids)))
   (sh-capst sts)                              ; the LAST stage's status, a signal folded down
     (? (! (two? sts)) 1
        (: st (last sts) (? (&& (charm? st) (< 255 st)) (- st 128) st)))
   (sh-cappipe a wfd)                          ; -> status, waited
     (: pids (sh-capsimple a wfd) (sh-capst (map (\ p (wait p)) pids)))
   (sh-capture a wfd)
     (? !a 0
        (: k (cap a)
           (? (id? k 'seq) (: _ (sh-capture (sh-nth a 1) wfd) (sh-capture (sh-nth a 2) wfd))
              (id? k 'and) (: s (sh-capture (sh-nth a 1) wfd)
                              (? (= s 0) (sh-capture (sh-nth a 2) wfd) s))
              (id? k 'or)  (: s (sh-capture (sh-nth a 1) wfd)
                              (? (= s 0) s (sh-capture (sh-nth a 2) wfd)))
              (id? k 'bg)  (sh-capture (sh-nth a 1) wfd)
              (sh-cappipe a wfd))))
   ; every lane leaves sh-substat holding this body's status: an assignment with
   ; no command name answers the LAST one performed (POSIX), so the pin is the
   ; only way back -- sh-fields sits between here and the reader. 1 up front is
   ; the floor the three scaring lanes keep.
   (sh-cmdsub text)
     (: _ (pin sh-substat () 1)
        lx (sh-lex text)
        (? (id? (cap lx) 'more) (: _ (cmderr "unterminated $( ) body") "")
           (: pr (sh-parse (cup lx))
              (? (! (id? (cap pr) 'ok)) (: _ (cmderr "bad $( ) body") "")
                 !(cup pr) (: _ (pin sh-substat () 0) "")      ; $( ) is a clean nothing
                 (: a (cup pr)
                    pp (pipe 0)
                    (? (charm? pp) ""
                       (: rfd (cap pp) wfd (cup pp)
                          seqish (|| (id? (cap a) 'seq) (id? (cap a) 'and)
                                     (id? (cap a) 'or) (id? (cap a) 'bg))
                          ; ⚠ GUARDED, and the guard must be up at the fork (the
                          ; child takes it by copy): the body's status is this
                          ; substitution's value, never the shell's own errexit
                          ; trigger -- aborting it would drop the very output the
                          ; caller asked for. the seq lane reaps per step and so
                          ; never aborted; now both lanes answer the same.
                          (sh-guarded
                            (\ _ (? seqish
                                    (: st (sh-capture a wfd)
                                       _ (pin sh-substat () st)
                                       _ (fdclose wfd)
                                       (slurp (fdopen rfd)))
                                    (: pids (sh-capsimple a wfd)
                                       _ (fdclose wfd)
                                       o (slurp (fdopen rfd))   ; drain, THEN reap
                                       _ (pin sh-substat () (sh-capst (map (\ p (wait p)) pids)))
                                       o)))))))))))
   _ (pin sh-subeval () sh-cmdsub)

   ; --- the script lane + the line entries --------------------------------------
   (sh-lines port buf)
     (: line (sh-getline port)
        (? (id? line 'eof)
           (? (tally buf)
              (: _ (say err "lush: unexpected eof\n") _ (pin sh-status () 2) 0)
              0)
           (: full (? (tally buf) (+ buf (+ "\n" line)) line)
              r (sh-line full)
              (? (two? r) (sh-lines port full)
                 (sh-retf ()) (: _ (pin sh-retf () 0) 0)   ; dot-return: stop THIS file
                 (sh-lines port "")))))
   (sh-parse-line src)
     (: lx (sh-lex src)
        (? (id? (cap lx) 'more) lx (sh-parse (cup lx))))
   (sh-eval-top src a)
     (: _ (pin sh-curline () src)
        s (sh-eval a)
        (? (charm? s) s
           ; a `return` that reached the top: flag it so the READING loop stops
           ; its file (POSIX dot-return; sh-lines clears the flag) -- $? is the
           ; return's own. a stray brk/cnt still swallows.
           (&& (two? s) (id? (cap s) 'ret))
             (: _ (pin sh-retf () 1) _ (pin sh-status () (cup s)) (cup s))
           0))
   (sh-line src)
     (: pr (sh-parse-line src)
        (? (id? (cap pr) 'more) pr
           (id? (cap pr) 'err)
             (: _ (say err (+ "lush: " (+ (cup pr) "\n"))) _ (pin sh-status () 2) 2)
           (sh-eval-top src (cup pr)))))
