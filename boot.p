(:
   (eachb s f) (: l (slen s) (bb i) (? (= i l) () (, (f (sget s i)) (bb (+ 1 i)))) (bb 0))
   (puts s) (eachb s putc)
   (putsl s) (, (puts s) (putc 10))
   (putxl x) (, (. x) (putc 10))
   (evals x) (? x (, (ev (A x)) (evals (B x))))
   (reads l) (: r (read()) (? r (reads (X (A r) l)) l))
   (evalf f) (evals (readf f))
   (evalfs fs) (? fs (, (evalf (A fs)) (evalfs (B fs))))


 (,
;  (putsl "hi from inside real_prog...")
  (\ fs
   (: prog (A fs) args (B fs)
   (, ;(putsl "hi from inside real_prog_fn...")
      ;(putxl fs) (putc 10)
      (? args (evalfs args)
         (isatty 0) (repl())
         (evals (reads()))))))))
