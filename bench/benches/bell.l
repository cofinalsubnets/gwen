; Bell numbers rendered in base 36, one per line while the rendering's length
; stays <= 280 (a port of test/bell.l / ~/bell.rb) -- a bignum-tower stress:
;     B(n) = sum_{k<n} C(n-1,k) B(k),  C(n,k) = n!/(k!(n-k)!)
; the values blow past 64 bits almost immediately (the last line is a 280-digit
; base-36 number). fresh memo hashes are minted per rep so every iteration
; recomputes from scratch. checksum = total characters across all rendered lines.
(: (bell-run limit)
    (: miss  (nom 0)  facts (tablet 0)  bells (tablet 0)
       digits "0123456789abcdefghijklmnopqrstuvwxyz"  base (tally digits)
       (factloop n acc) (? (< n 2) acc (factloop (- n 1) (* acc n)))
       (fact n)  (: v (peep facts n miss) (? (= v miss) (: r (factloop n 1) _ (pin facts n r) r) v))
       (choose n k) (/ (fact n) (* (fact k) (fact (- n k))))
       (bellsum n k acc) (? (< k n) (bellsum n (+ k 1) (+ acc (* (choose (- n 1) k) (bell k)))) acc)
       (bell n) (: v (peep bells n miss) (? (= v miss) (: r (? (< n 2) 1 (bellsum n 0 0)) _ (pin bells n r) r) v))
       (showloop n acc) (? (< n 1) acc (showloop (// n base) (+ (snip digits (% n base) (+ 1 (% n base))) acc)))
       (show n) (showloop n "")
       (gen i acc) (: b (show (bell i)) (? (<= (tally b) limit) (gen (+ i 1) (+ acc (tally b))) acc))
     (gen 0 0))
 (bench "bell" (\ _ (bell-run 280))))
