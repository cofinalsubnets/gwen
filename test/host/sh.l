; --- test/host/sh.l: lush's gate -- the lexer/parser pure functions + the job-
; control nifs (signal / wait's stop lane / sigfd's signal list: doc/posix.md
; step 4, signals arriving as conditions). the interactive loop needs a pty
; (probed live, the berth pattern); everything beneath it gates headless here.
; the parts read bare in cat order (crew/lush/, the seed model); main.l's seat
; stays quiet under the corpus (no lush on cmdline).
; run: cat test/00-init.l test/host/sh.l | out/host/love
(reads (open "crew/lush/job.l" "r"))
(reads (open "crew/lush/eval.l" "r"))
(reads (open "crew/lush/line.l" "r"))
(reads (open "crew/lush/main.l" "r"))

; the lexer: whitespace split, quotes glue, operators are ordinary words
(assert
 (= '("a" "b" "c") (sh-words "  a b   c "))
 (= '("echo" "x  y") (sh-words "echo 'x  y'"))
 (= '("ab cd") (sh-words "a\"b c\"d"))
 (= '(("a") ("b" "c")) (sh-split '("a" "|" "b" "c")))
 (= '(("cat") "in" "out" 0) (sh-redirs '("cat" "<" "in" ">" "out")))
 (= '(("cat") () "log" 1) (sh-redirs '("cat" ">>" "log")))
 (sh-amp? '("x" "y" "&")) !(sh-amp? '("x" "&y"))
 (= '("x" "y") (sh-drop-amp '("x" "y" "&")))
 (= "a b" (sh-unwords '("a" "b")))
 (= 12 (sh-int "12")))

; (signal sig 1) ignores: a self-INT that would kill us just fizzles; restore after
(: me (getpid 0)
   _ (assert (id? (signal sig-int 1) ()))
   _ (still me sig-int)
   _ (assert 1)                                     ; still here: the ignore held
   _ (assert (id? (signal sig-int 0) ())))

; wait's stop lane: a SIGSTOPped child answers 256+sig (a charm past every exit
; status -- the shell's "park it as a job" signal), then CONT + KILL ends it 137
(: p (spawn (L "sleep" "5"))
   _ (still p 19)                                   ; SIGSTOP
   (assert (= (+ 256 19) (wait p))
           (: _ (still p sig-cont) _ (still p 9) (= 137 (wait p)))))

; sigfd takes a signal LIST now: SIGUSR1 blocked + queued, kill self, take the
; record -- the signal is DATA, (signo . pid)
(: sp (sigfd (L 10))
   me (getpid 0)
   _  (still me 10)
   ev (sigtake sp)
   (assert (hot? sp) (two? ev) (= 10 (cap ev)) (= me (cup ev))))

; ... and a taken signal re-raised through the condition system IS the doc's
; "signals arrive as conditions": scare routes it to the installed help policy
(: help (\ s a b (L 'gotsig a b)))
(assert (= (L 'gotsig 'sigusr1 (getpid 0)) (scare 'sigusr1 (getpid 0))))
(: help 0)

; --- step 5: env nifs + $ expansion + $? -----------------------------------------
(assert (id? (setenv "LOVE_SH_T5" "quay") ())
        (= "quay" (getenv "LOVE_SH_T5"))
        (foldl (\ a e (? a a (= e "LOVE_SH_T5=quay"))) 0 (environ 0))  ; raw K=V shape
        (= '("echo" "quay") (sh-words "echo $LOVE_SH_T5"))
        (= '("xquay") (sh-words "x$LOVE_SH_T5"))                       ; glued expansion
        (= '("pre quay post") (sh-words "\"pre $LOVE_SH_T5 post\""))   ; expands in ".."
        (= '("$LOVE_SH_T5") (sh-words "'$LOVE_SH_T5'"))                  ; literal in '..'
        (= '("a") (sh-words "a $LOVE_SH_UNSET_ZZZ"))                   ; unset word drops away
        (= '("$") (sh-words "$")))                                   ; a lone $ is literal
(assert (id? (setenv "LOVE_SH_T5" ()) ())                              ; absence unsets
        (id? (getenv "LOVE_SH_T5") ()))
; $?: the last foreground status, read back through expansion
(: _ (sh-run (L "sh" "-c" "exit 7"))
   (assert (= '("7") (sh-words "$?"))))
; export, through the shell's own pipeline
(: _ (sh-run (L "export" "LOVE_SH_T6=pier"))
   (assert (= "pier" (getenv "LOVE_SH_T6"))
           (= '("pier") (sh-words "$LOVE_SH_T6"))))

(puts "sh: ok") (putc 10)
