(: fin (sym 0)
   (li k x) (? (= fin x) (k 0) (li (\ z (k (cons x z)))))
   lis (li id)
   (assert (= '(1 2 3 4 5) (lis 1 2 3 4 5 fin))))
