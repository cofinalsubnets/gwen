; --- lush 🐚 the evaluator -- AST to processes ---
; NO BRACKETS HERE: the runtime supplies the layer (see job.l for the cat law).
;
; (sh-line src) -> ('more . why) | status: lex + parse + eval one buffer; the
; 'more rides out so the caller can gather a continuation line. redirects
; become a spawnmap fdmap ((childfd . srcfd)..) applied IN ORDER in the child
; -- POSIX's left-to-right law, so `>f 2>&1` sends both to f. pipeline stages
; are spawned processes; a builtin mid-pipeline runs its external namesake for
; now (POSIX puts it in a subshell anyway; the fork nif at rung 3 does it
; honestly). command substitution spawns the body's stages with stdout on a
; pipe and slurps it back through fdopen -- external-only until fork; the
; evaluator lands in word.l's sh-subeval box at load (word compiles first).
; builtins: exit cd export pwd jobs fg bg : -- expanded argv, redirects
; ignored on them (rung 3).

(: ; export NAME=VAL -> setenv; a word without '=' is a quiet no-op (we have no
   ; local-vs-env split to promote from)
   (sh-eqpos s i) (? (= i (tally s)) () (? (= (s i) 61) i (sh-eqpos s (+ i 1))))
   (sh-export w)
     (: p (sh-eqpos w 0)
        (? (nil? p) 0 (setenv (snip w 0 p) (snip w (+ p 1) (tally w)))))

   (cmderr msg) (: _ (say err (+ "lush: " (+ msg "\n"))) _ (pin sh-status () 1) 1)

   ; --- redirects -> the fdmap ------------------------------------------------
   ; (sh-rdmap redirs) -> (1 fdmap openfds) | ('err . msg); entries keep source
   ; order (the C side applies them in order, N>&M reading the map so far)
   (dfd ion d) (? (two? ion) (cap ion) d)
   (rderr opens msg) (: _ (each opens fdclose) (link 'err msg))
   (sh-rdmap rs) (rdm rs () ())
   (rdm rs fdmap opens)
     (? !rs (L 1 (sh-rev fdmap) opens)
        (: r (cap rs) ion (sh-nth r 0) op (sh-nth r 1)
           tf (sh-fields (sh-nth r 2))
           (? (|| !tf (two? (cup tf))) (rderr opens "ambiguous redirect")
              (: t (cap tf)
                 (? (= op "<")
                      (: fd (openfd t 0)
                         (? (< fd 0) (rderr opens (+ t ": cannot open"))
                            (rdm (cup rs) (link (link (dfd ion 0) fd) fdmap) (link fd opens))))
                    (|| (= op ">") (= op ">>"))
                      (: fd (openfd t (? (= op ">") 1 2))
                         (? (< fd 0) (rderr opens (+ t ": cannot open"))
                            (rdm (cup rs) (link (link (dfd ion 1) fd) fdmap) (link fd opens))))
                    (|| (= op ">&") (= op "<&"))
                      (: d (dfd ion (? (= op "<&") 0 1))
                         (? (= t "-") (rdm (cup rs) (link (link d ()) fdmap) opens)
                            (sh-digits? t) (rdm (cup rs) (link (link d (sh-int t)) fdmap) opens)
                            (rderr opens (+ t ": bad fd"))))
                    (rderr opens (+ op ": unsupported")))))))

   ; --- builtins (a charm status, or 'nope: not one of ours) -------------------
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
           (= cmd "fg")   (: _ (sh-fg (sh-jobn argv)) (sh-status ()))   ; the job's exit is $?
           (= cmd "bg")   (sh-bg (sh-jobn argv))
           (= cmd ":")    0
           'nope))

   ; --- stage expansion + the spawn engines ------------------------------------
   (sh-makepipes n) (? (< n 1) () (link (pipe 0) (sh-makepipes (- n 1))))
   (sh-flat ps) (? !ps () (link (cap (cap ps)) (link (cup (cap ps)) (sh-flat (cup ps)))))
   ; every stage expanded first: -> (1 ((argv fdmap opens)..)) | ('err . msg)
   (sh-stagex cmds acc)
     (? !cmds (L 1 (sh-rev acc))
        (: c (cap cmds)
           (? (! (id? (cap c) 'cmd))
              (: _ (stclean acc) (link 'err "only simple commands pipe (rung 3)"))
              (: argv (catmap sh-fields (sh-nth c 1))
                 (? !argv (: _ (stclean acc) (link 'err "empty command in pipeline"))
                    (: rm (sh-rdmap (sh-nth c 2))
                       (? (id? (cap rm) 'err) (: _ (stclean acc) rm)
                          (sh-stagex (cup cmds)
                                     (link (L argv (sh-nth rm 1) (sh-nth rm 2)) acc)))))))))
   (stclean acc) (each acc (\ st (each (sh-nth st 2) fdclose)))
   ; spawn the stages over pipes: stage 0 may lead a pgrp + take the tty, the
   ; rest join (the sh-stages law); base map = pipe wiring, redirects after
   (sh-spawnst sts pipes allclose i last pg fg pids)
     (? !sts (sh-rev pids)
        (: st (cap sts)
           base (+ (? (< 0 i) (L (link 0 (cap (sh-nth pipes (- i 1))))) ())
                   (? (< i last) (L (link 1 (cup (sh-nth pipes i)))) ()))
           pid (spawnmap (sh-nth st 0) (+ base (sh-nth st 1)) allclose pg (? (= i 0) fg 0))
           (sh-spawnst (cup sts) pipes allclose (+ i 1) last
                       (? (< pg 0) pg (? (= i 0) pid pg)) fg (link pid pids))))
   (sh-pipeline cmds bg)
     (: sx (sh-stagex cmds ())
        (? (id? (cap sx) 'err) (cmderr (cup sx))
           (: sts (sh-nth sx 1)
              n (- (tally sts) 1)
              pipes (sh-makepipes n)
              allclose (+ (sh-flat pipes) (catmap (\ st (sh-nth st 2)) sts))
              pids (sh-spawnst sts pipes allclose 0 n (sh-pgm 0) (? bg 0 (sh-fgm 0)) ())
              _ (each allclose fdclose)
              (? bg
                 (: jn (sh-addjob pids 'running (+ (sh-curline ()) " &"))
                    _ (? (sh-tty ()) (puts (+ "[" (+ (show jn) (+ "] " (+ (sh-unwords (map show pids)) "\n"))))) 0)
                    0)
                 (: _ (sh-waitfg pids (sh-curline ())) (sh-status ()))))))
   ; one simple foreground command: builtin, or a single spawnmap + wait
   (sh-simple ws rs)
     (: argv (catmap sh-fields ws)
        (? !argv 0
           (: b (sh-builtin argv)
              (? (charm? b) (: _ (pin sh-status () b) b)
                 (: rm (sh-rdmap rs)
                    (? (id? (cap rm) 'err) (cmderr (cup rm))
                       (: opens (sh-nth rm 2)
                          pid (spawnmap argv (sh-nth rm 1) opens (sh-pgm 0) (sh-fgm 0))
                          _ (each opens fdclose)
                          (? (< pid 0) (cmderr (+ (cap argv) ": cannot spawn"))
                             (: _ (sh-waitfg (L pid) (sh-unwords argv))
                                (sh-status ()))))))))))

   ; --- the AST walk ------------------------------------------------------------
   (sh-eval a)
     (? !a (sh-status ())
        (: k (cap a)
           (? (id? k 'seq) (: _ (sh-eval (sh-nth a 1)) (sh-eval (sh-nth a 2)))
              (id? k 'and) (: s (sh-eval (sh-nth a 1)) (? (= s 0) (sh-eval (sh-nth a 2)) s))
              (id? k 'or)  (: s (sh-eval (sh-nth a 1)) (? (= s 0) s (sh-eval (sh-nth a 2))))
              (id? k 'not) (: s (sh-eval (sh-nth a 1)) ns (? (= s 0) 1 0)
                              _ (pin sh-status () ns) ns)
              (id? k 'bg)  (sh-evalbg (sh-nth a 1))
              (id? k 'pipe) (sh-pipeline (sh-nth a 1) 0)
              (id? k 'cmd) (sh-simple (sh-nth a 1) (sh-nth a 2))
              (cmderr "unknown form"))))
   (sh-evalbg a)
     (: k (cap a)
        (? (id? k 'pipe) (sh-pipeline (sh-nth a 1) 1)
           (id? k 'cmd)  (sh-pipeline (L a) 1)
           (cmderr "background lists land at rung 3")))

   ; --- command substitution (word.l's box) -------------------------------------
   ; spawn the body's stages with the LAST stdout on our pipe, drain through
   ; fdopen, then reap -- drain BEFORE reap, so a body bigger than the pipe
   ; buffer can't deadlock. compound bodies (a;b, a&&b) reap per sequencing
   ; point instead (the 64K caveat rides until fork, rung 3).
   (sh-capspawn sts pipes allclose wfd i last pids)
     (? !sts (sh-rev pids)
        (: st (cap sts)
           base (+ (? (< 0 i) (L (link 0 (cap (sh-nth pipes (- i 1))))) ())
                   (L (link 1 (? (< i last) (cup (sh-nth pipes i)) wfd))))
           pid (spawnmap (sh-nth st 0) (+ base (sh-nth st 1)) allclose (0 - 1) 0)
           (sh-capspawn (cup sts) pipes allclose wfd (+ i 1) last (link pid pids))))
   (sh-capsimple a wfd)                        ; -> pids, unwaited
     (: cmds (? (id? (cap a) 'pipe) (sh-nth a 1) (L a))
        sx (sh-stagex cmds ())
        (? (id? (cap sx) 'err) (: _ (cmderr (cup sx)) ())
           (: sts (sh-nth sx 1)
              n (- (tally sts) 1)
              pipes (sh-makepipes n)
              allclose (+ (sh-flat pipes) (catmap (\ st (sh-nth st 2)) sts))
              pids (sh-capspawn sts pipes allclose wfd 0 n ())
              _ (each allclose fdclose)
              pids)))
   (sh-cappipe a wfd)                          ; -> status, waited
     (: pids (sh-capsimple a wfd)
        sts (map (\ p (wait p)) pids)
        (? !sts 1 (: st (last sts) (? (&& (charm? st) (< 255 st)) (- st 128) st))))
   (sh-capture a wfd)
     (? !a 0
        (: k (cap a)
           (? (id? k 'seq) (: _ (sh-capture (sh-nth a 1) wfd) (sh-capture (sh-nth a 2) wfd))
              (id? k 'and) (: s (sh-capture (sh-nth a 1) wfd)
                              (? (= s 0) (sh-capture (sh-nth a 2) wfd) s))
              (id? k 'or)  (: s (sh-capture (sh-nth a 1) wfd)
                              (? (= s 0) s (sh-capture (sh-nth a 2) wfd)))
              (id? k 'not) (: s (sh-capture (sh-nth a 1) wfd) (? (= s 0) 1 0))
              (id? k 'bg)  (sh-capture (sh-nth a 1) wfd)
              (sh-cappipe a wfd))))
   (sh-cmdsub text)
     (: lx (sh-lex text)
        (? (id? (cap lx) 'more) (: _ (cmderr "unterminated $( ) body") "")
           (: pr (sh-parse (cup lx))
              (? (! (id? (cap pr) 'ok)) (: _ (cmderr "bad $( ) body") "")
                 !(cup pr) ""
                 (: a (cup pr)
                    pp (pipe 0)
                    (? (charm? pp) ""
                       (: rfd (cap pp) wfd (cup pp)
                          simple (|| (id? (cap a) 'pipe) (id? (cap a) 'cmd)
                                     (id? (cap a) 'not))
                          (? simple
                             (: pids (sh-capsimple (? (id? (cap a) 'not) (sh-nth a 1) a) wfd)
                                _ (fdclose wfd)
                                o (slurp (fdopen rfd))
                                _ (each pids (\ p (wait p)))
                                o)
                             (: _ (sh-capture a wfd)
                                _ (fdclose wfd)
                                (slurp (fdopen rfd)))))))))))
   _ (pin sh-subeval () sh-cmdsub)

   ; --- the line entries --------------------------------------------------------
   (sh-parse-line src)
     (: lx (sh-lex src)
        (? (id? (cap lx) 'more) lx (sh-parse (cup lx))))
   (sh-eval-top src a) (: _ (pin sh-curline () src) (sh-eval a))
   (sh-line src)
     (: pr (sh-parse-line src)
        (? (id? (cap pr) 'more) pr
           (id? (cap pr) 'err)
             (: _ (say err (+ "lush: " (+ (cup pr) "\n"))) _ (pin sh-status () 2) 2)
           (sh-eval-top src (cup pr)))))
