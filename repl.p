((: (repl _) 
     (: r (, (puts "    ") (read 0))
        (? r (, (. (ev (A r)))
                (putc 10) (repl 0))))) 0)
