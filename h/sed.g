(: (sed c) (? (> c -1) (,
          (?  (= c 10) (puts " ")
              (= c 34) (puts "\\\"")
              (putc c))
          (sed (getc c))))
(, (putc 34) (sed (getc 0)) (putc 34)))
