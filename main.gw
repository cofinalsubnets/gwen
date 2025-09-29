(\ fs (:
   ; else eval expressions from stdin
prog (A fs) args (B fs)
prompt "    "
help (L " [args]"
       " args:"
       "   -h   show this message"
       "   -v   show version"
       "   -r   start repl"
       " file   evaluate file")
(rel _) (: r (read _)
         (? r (, (ev (A r))
                 (rel _))))
(repl p) (: r (, (puts p) (read 0))
           (? r (, (. (ev (A r)))
                   (putc 10)
                   (repl p))))
(putln s) (, (puts s)
            (putc 10))
(go a as) (, (? (= a "-h") (, (puts "usage: ") (puts prog) (each help putln))
               (= a "-v") (, (puts prog) (puts " ") (putln version))
               (= a "-r") (repl prompt)
                          (each (readf a) ev))
            (? as (go (A as) (B as))))

(,
(? args       (go (A args) (B args)) ; if any cli args then process them
  (isatty 0) (repl prompt) ; else if attached to a tty then start a repl
             (rel 0)))))
