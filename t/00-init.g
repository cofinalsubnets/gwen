(: test_t0 (clock 0)
 (term-esc string) (, (putc 27) (puts string))
 green-dot "[32m."
 red-x "[31mX"
 reset "[0m"
 (term-text s)
  (, (putc 27)
     (puts s)
     (putc 27)
     (puts reset))

   test-state (new 0)
   (test-set k v) (put k v test-state)
   (test-get k) (get 0 k test-state))
(:: 'assert (\ l
(:
 (report x v) (,
  (test-set 'count (+ 1 (test-get 'count)))
   (? v (term-text green-dot)
    (, (test-set 'fail (X x (test-get 'fail)))
       (term-text red-x))))
 (X ', (map (\ l (L report (L '` l) l)) l)))))
