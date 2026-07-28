; --- lush 🐚 the line -- the repl loops over bao's editor ---
; NO BRACKETS HERE: the runtime supplies the layer (see job.l for the cat law).
;
; line editing on a real tty is bao's own editor (`edln`, the baked shell core):
; in-line editing + up/down command history. sh raws the tty itself (edln owns
; echo) and drops back to cooked around each command (programs want a normal
; tty), exactly the readline dance. piped / non-tty input falls back to the
; cooked-tty line reader (no editing, no history). an UNFINISHED line -- open
; quote, trailing \, dangling && -- prompts "> " (PS2) and joins with "\n"
; until the parse closes; ^C at the prompt abandons the buffer (edln's -12,
; folded into a fresh line by edln itself). EOF (^D) ends the shell; both
; loops are tail-recursive (no stack growth across commands).

(: ; --- read one line (cooked tty): bytes until \n; 'eof at immediate EOF.
   (sh-getline port) (: c (see port) (? (< c 0) 'eof (= c 10) "" (+ (+ "" c) (sh-rest port))))
   (sh-rest port)    (: c (see port) (? (< c 0) ""   (= c 10) "" (+ (+ "" c) (sh-rest port))))

   (sh-cooked buf)
     (: _ (? (tally buf) 0 (sh-reapbg 0))
        _ (say out (? (tally buf) "> " (+ (cwd 0) "$ ")))  _ (flush out)
        line (sh-getline in)
        (? (id? line 'eof)
           (: _ (? (tally buf) (say err "lush: unexpected eof\n") 0) (puts "\n"))
           (: full (? (tally buf) (+ buf (+ "\n" line)) line)
              pr (sh-parse-line full)
              (? (id? (cap pr) 'more) (sh-cooked full)
                 (id? (cap pr) 'err)
                   (: _ (say err (+ "lush: " (+ (cup pr) "\n")))
                      _ (pin sh-status () 2) (sh-cooked ""))
                 (: _ (sh-eval-top full (cup pr)) (sh-cooked ""))))))
   ; interactive: edln owns echo, so raw the tty for the prompt and drop to
   ; cooked around each command. edln answers (line . nhist) -- the raw charlist
   ; + the grown history -- or 'eof; thread nhist back so up/down recall.
   (sh-loop buf hist)
     (: _ (? (tally buf) 0 (sh-reapbg 0))
        r (edln in (? (tally buf) "> " (+ (cwd 0) "$ ")) hist)
        (? (id? r 'eof) (: _ (raw 0) (puts "\n"))                 ; cooked, newline, end
           (: line (slurp (tap (cap r))) nhist (cup r)
              full (? (tally buf) (+ buf (+ "\n" line)) line)
              pr (sh-parse-line full)
              (? (id? (cap pr) 'more) (sh-loop full nhist)
                 (id? (cap pr) 'err)
                   (: _ (raw 0)
                      _ (say err (+ "lush: " (+ (cup pr) "\n")))
                      _ (pin sh-status () 2)
                      _ (raw 1) (sh-loop "" nhist))
                 (: _ (raw 0)                                     ; cooked for the command
                    _ (sh-eval-top full (cup pr))
                    _ (raw 1) (sh-loop "" nhist)))))))              ; re-raw for the next prompt
