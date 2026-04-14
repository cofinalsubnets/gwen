(: eof (sym 0)
  (go l) (: x (read eof)
   (? (= x eof) 0
    (: _ (? l (puts " "))
       _ (. x)
     (go (+ 1 l)))))
 (go 0))
