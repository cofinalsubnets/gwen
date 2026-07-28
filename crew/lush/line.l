; --- lush 🐚 the line -- the repl loops over bao's editor ---
; NO BRACKETS HERE: the runtime supplies the layer (see job.l for the cat law).
;
; line editing on a real tty is bao's own editor (`edln`, the baked shell core):
; in-line editing + up/down command history. sh raws the tty itself (edln owns
; echo) and drops back to cooked around each child (programs want a normal tty),
; exactly the readline dance. piped / non-tty input falls back to the cooked-tty
; line reader (no editing, no history). EOF (^D) ends the shell; both loops are
; tail-recursive (no stack growth across commands). ^C at the PROMPT abandons
; the line (edln's -12); finished background jobs report at the next prompt.

(: ; --- read one line (cooked tty): bytes until \n; 'eof at immediate EOF.
   (sh-getline port) (: c (see port) (? (< c 0) 'eof (= c 10) "" (+ (+ "" c) (sh-rest port))))
   (sh-rest port)    (: c (see port) (? (< c 0) ""   (= c 10) "" (+ (+ "" c) (sh-rest port))))

   (sh-cooked _)
     (: _    (sh-reapbg 0)
        _    (say out (+ (cwd 0) "$ "))  _ (flush out)
        line (sh-getline in)
        (? (id? line 'eof) (puts "\n")
           (: words (sh-words line)
              (? !words (sh-cooked 0)
                 (: r (sh-run words) (? (id? r 'done) 0 (sh-cooked 0)))))))
   ; interactive: edln owns echo, so raw the tty for the prompt and drop to cooked
   ; around each child. edln returns (line . nhist) -- the raw charlist + the grown
   ; history -- or 'eof; thread nhist back in so up/down recall across commands.
   (sh-loop hist)
     (: _ (sh-reapbg 0)
        r (edln in (+ (cwd 0) "$ ") hist)
        (? (id? r 'eof) (: _ (raw 0) (puts "\n"))                 ; cooked, newline, end
           (: line (cap r) nhist (cup r)
              words (sh-words (slurp (tap line)))                 ; charlist -> string for the tokenizer
              (? !words (sh-loop nhist)
                 (: _  (raw 0)                                    ; cooked for the child
                    rr (sh-run words)
                    (? (id? rr 'done) (raw 0)
                       (: _ (raw 1) (sh-loop nhist)))))))))        ; re-raw for the next prompt
