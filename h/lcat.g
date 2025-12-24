(: (go l) (: x (read 0) (? x (, (? l (puts " "))
                                (. (A x))
                                (go x))))
 (go 0))
