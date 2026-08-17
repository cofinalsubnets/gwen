; --- lush 🐚 the line -- prompt, history, completion, the repl loops ---
; NO BRACKETS HERE: the runtime supplies the layer (see job.l for the cat law).
;
; line editing on a real tty is bao's editor -- `edlnc`, edln + Tab completion:
; sh-complete takes the text left of the cursor, finds the word under it, and
; answers (insert . candidates) -- commands (builtins + a PATH readdir scan)
; in command position, pathnames (the glob walk, ~ unfolded and refolded)
; everywhere else; one candidate also lands its "/" or " ". the prompt is the
; grml feel: last status when nonzero, cwd with ~ shrunk, the git branch when
; a .git sits on the walk up, then $ (or # for root). history PERSISTS:
; ~/.lush_history is append-on-Enter (a crash loses nothing) and the last 100
; lines seed edln's recall at birth. the raw/cooked dance and the "> " (PS2)
; continuation ride as before; lush-help is the interactive FLOOR -- a scare
; drops to cooked, prints ;; a b, welps to the zero point, and the loop
; re-prompts: a crash never eats the terminal (installed in (sh _), main.l).

(module 'lush
(: ; --- the floor: bao's shell-help shape, cooked first so the print lands clean
   (lush-help a b)
     (: _ (raw 0) _ (puts ";; ") _ (putx a) _ (puts " ") _ (putx b) _ (putc 10)
        (welp a b))

   ; --- the prompt --------------------------------------------------------------
   (sh-rfind s b)
     (: (go k) (? (k < 0) (0 - 1) (? ((s k) = b) k (go (k - 1))))
        (go ((tally s) - 1)))
   (sh-homely d)                                ; /home/me/x -> ~/x
     (: h ("" + (getenv "HOME")) hn (tally h)
        (? (&& hn (hn <= (tally d)) (h = (snip d 0 hn)))
           ("~" + (snip d hn (tally d)))
           d))
   (sh-gitup d)                                 ; a .git anywhere up the walk?
     (? (two? (stat (d + "/.git"))) 1
        (: k (sh-rfind d 47)
           (? (k < 1) 0 (sh-gitup (snip d 0 k)))))
   (sh-gitbr _)
     (? (sh-gitup ("" + (cwd 0)))
        (: r (hark ["git" "symbolic-ref" "--short" "-q" "HEAD"])
           (? (0 = <r) (sh-chomp >r) ""))
        "")
   (sh-prompt _)
     (: st (sh-status ())
        br (sh-gitbr 0)
        (+ (? (st = 0) "" ((show st) + " "))
           (+ (sh-homely ("" + (cwd 0)))
              (+ (? (tally br) (" (" + (br + ")")) "")
                 (? (0 = (getuid 0)) " # " " $ ")))))

   ; --- history: ~/.lush_history, oldest first on disk, newest first in hand
   (sh-expl s)                                  ; string -> charlist
     (: n (tally s)
        (go i) (? (i = n) () (s i >< go (i + 1)))
        (go 0))
   (sh-histpath _) (("" + (getenv "HOME")) + "/.lush_history")
   (sh-histload _)
     (: p (sh-histpath 0)
        (? (two? (stat p))
           (: prt (open p "r")
              (go acc)
                (: ln (sh-getline prt)
                   (? (id? ln 'eof) acc
                      (tally ln) (go (sh-expl ln >< acc))
                      (go acc)))
              r (go ())
              _ (close prt)
              (take 100 r))
           ()))
   (sh-histput ln)
     (: o (open (sh-histpath 0) "a")
        (? (hot? o) (: _ (say o (ln + "\n")) _ (flush o) (close o)) 0))

   ; --- completion: (insert . candidates) off the text left of the cursor ------
   sh-builtins '("cd" "exit" "export" "pwd" "jobs" "fg" "bg")
   (sh-prefix? p s) (: n (tally p) ((n <= (tally s)) && (p = (snip s 0 n))))
   (sh-comm2 a s)                               ; longest common prefix of two
     (: n (? ((tally a) < (tally s)) (tally a) (tally s))
        (go i) (? ((i < n) && ((a i) = (s i))) (go (i + 1)) i)
        (snip a 0 (go 0)))
   (sh-common l) (? !(two? l) "" (foldl sh-comm2 <l >l))
   (sh-uniq l)
     (? !(two? l) l
        (? (two? >l && (<l = <>l)) (sh-uniq >l)
           (<l >< sh-uniq >l)))
   (sh-pathdirs s)                              ; "a:b:c" -> ("a" "b" "c")
     (: n (tally s)
        (go i st acc) (? (i = n) (sh-rev (snip s st i >< acc))
                         (? ((s i) = 58) (go (i + 1) (i + 1) (snip s st i >< acc))
                            (go (i + 1) st acc)))
        (go 0 0 ()))
   (sh-cmds w)                                  ; command candidates for prefix w
     (sh-uniq (sort (+ (filter (sh-prefix? w) sh-builtins)
                       (catmap (\ d (filter (sh-prefix? w) (readdir d)))
                               (sh-pathdirs ("" + (getenv "PATH")))))))
   (sh-dirp q) (: st (stat q) (two? st && (16384 = (61440 & (sh-nth st 2)))))
   (sh-unfold w)                                ; ~/x -> $HOME/x (stat wants the real path)
     (? (tally w && ((w 0) = 126)) (("" + (getenv "HOME")) + (snip w 1 (tally w))) w)
   (sh-fileca w)                                ; pathname candidates: glob w*, ~ refolded
     (: h ("" + (getenv "HOME"))
        w2 (sh-unfold w)
        g (sh-glob (w2 + "*") (sh-ones ((tally w2) + 1)))
        (? (w = w2) g
           (map (\ p ("~" + (snip p (tally h) (tally p)))) g)))
   (sh-wdst s)                                  ; where the word under the cursor starts
     (: (go k) (? (k < 0) 0
                  (: b (s k)
                     (? (|| (b = 32) (b = 9) (b = 124) (b = 38) (b = 59) (b = 60) (b = 62))
                        (k + 1) (go (k - 1)))))
        (go ((tally s) - 1)))
   (sh-cmdpos? s st)                            ; only spaces / a separator before it?
     (: (go k) (? (k < 0) 1
                  (: b (s k)
                     (? ((b = 32) || (b = 9)) (go (k - 1))
                        (|| (b = 124) (b = 38) (b = 59)) 1
                        0)))
        (go (st - 1)))
   (sh-complete left)
     (: s (slurp (tap left))
        st (sh-wdst s)
        w (snip s st (tally s))
        (? !(tally w) (() >< ())
           (: cmdp (&& (sh-cmdpos? s st) ((sh-rfind w 47) < 0) !((w 0) = 126))
              cands (? cmdp (sh-cmds w) (sh-fileca w))
              (? !(two? cands) (() >< ())
                 (: com (sh-common cands)
                    ins (? (sh-prefix? w com) (snip com (tally w) (tally com)) "")
                    one !(two? >cands)
                    sfx (? !one ""
                           (? (!cmdp && sh-dirp (sh-unfold <cands)) "/" " "))
                    (sh-expl (ins + sfx) >< cands))))))

   ; --- the loops ---------------------------------------------------------------
   (sh-cooked buf)
     (: _ (? (tally buf) 0 (sh-reapbg 0))
        _ (say out (? (tally buf) "> " ((cwd 0) + "$ ")))  _ (flush out)
        line (sh-getline in)
        (? (id? line 'eof)
           (: _ (? (tally buf) (say err "lush: unexpected eof\n") 0) (puts "\n"))
           (: full (? (tally buf) (buf + ("\n" + line)) line)
              pr (sh-parse-line full)
              (? (id? <pr 'more) (sh-cooked full)
                 (id? <pr 'err)
                   (: _ (say err ("lush: " + (>pr + "\n")))
                      _ (pin sh-status () 2) (sh-cooked ""))
                 (: _ (sh-eval-top full >pr) (sh-cooked ""))))))
   ; interactive: edlnc owns echo, so raw the tty for the prompt and drop to
   ; cooked around each command. edlnc answers (line . nhist) or 'eof; nhist
   ; threads back so up/down recall, and the file gets each line as it lands.
   (sh-loop buf hist)
     (: _ (? (tally buf) 0 (sh-reapbg 0))
        r (edlnc in (? (tally buf) "> " (sh-prompt 0)) hist sh-complete)
        (? (id? r 'eof) (: _ (raw 0) (puts "\n"))                 ; cooked, newline, end
           (: line (slurp (tap <r)) nhist >r
              _ (? (tally line) (sh-histput line) 0)
              full (? (tally buf) (buf + ("\n" + line)) line)
              pr (sh-parse-line full)
              (? (id? <pr 'more) (sh-loop full nhist)
                 (id? <pr 'err)
                   (: _ (raw 0)
                      _ (say err ("lush: " + (>pr + "\n")))
                      _ (pin sh-status () 2)
                      _ (raw 1) (sh-loop "" nhist))
                 (: _ (raw 0)                                     ; cooked for the command
                    _ (sh-eval-top full >pr)
                    _ (raw 1) (sh-loop "" nhist)))))))              ; re-raw for the next prompt
) ; module 'lush
