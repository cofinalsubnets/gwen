# gwen lisp

gwen is a like simplified version of scheme with less words and
parentheses, however unlike scheme and other lisp dialects, every
value acts as a one argument function. for functions this is done
by currying like in haskell. other objects are interpreted as their
own constant functions.

## 

variable scope is similar to scheme. there are five special forms
which have similar scheme counterparts.

|  scheme  | gwen |
|----------|------|
| `begin`  | `,`  |
| `cond`   | `?`  |
| `let*`   | `:`  |
| `quote`  | <code>&#96;</code> |
| `lambda` | <code>&#92;</code> |

the internal syntax of some forms is simplified compared to scheme.

|  scheme                     | gwen             |
|-----------------------------|------------------|
| `(cond (a b) (c d) (#t e))` | `(? a b c d e)`  |
| `(let* ((a b) (c d)) e)`    | `(: a b c d e)`  |
| `(lambda (a b) c)`          | <code>(&#92; a b c)</code> |

in conditionals only one value is false, `0`.

## example program

```lisp
; self evaluator test from test/meta.gw
(:
 eee '(: ; evaluator
  (meta-eval x) (?
   (symp x) (\ l (l x))
   (not (twop x)) (const x)
   (: x0 (A x) a (B x) (?
    (= x0 '`) (const (A a))
    (= x0 ',) (foldl (\ a b l (, (a l) (b l)))
                     (\ l 0)
                     (map meta-eval a))
    (= x0 '\) (foldr (\ a f l x (f (\ y (? (= y a) x (l y)))))
                     (meta-eval (last a))
                     (init a))
    (= x0 '?) (cond-loop a id)
    (= x0 ':) (let-loop a id)
    ((\ x l (foldl (\ f x (f x)) id (map (\ x (x l)) x))) (map meta-eval x)))))

  (cond-loop a f)
   (? (nilp a) (cond-loop (X 0 0) f)
      (nilp (B a)) (f (meta-eval (A a)))
      (: ant (meta-eval (A a)) con (meta-eval (A (B a)))
       (cond-loop (B (B a)) (\ x (f (\ l (? (ant l) (con l) (x l))))))))

  (let-loop a b m)
   (? (nilp a) (let-loop (X 0 0) b m)
      (nilp (B a)) (meta-eval (A a) (b m))
      (desugar (A a) (A (B a)) (\ k v
       (: t (tnew 0) get (tget 0 t) set (tset t 0)
        (let-loop (B (B a)) (\ l (, (set (meta-eval v (b l))) l))
                            (\ x (? (= x k) (get 0) (m x))))))))

  (desugar k v c)
   (? (twop k) (desugar (A k) (X '\ (append (B k) (X v 0))) c)
               (c k v))
  ;return
  meta-eval)
 meta-eval (ev eee)
 expr '((\ a b (: c (+ a 9) d (+ c b) (* c d))) 4 5)
 G (tget 0 global-namespace)
 (, (assert (= 234 (ev expr)))
    (assert (= 234 (meta-eval expr G)))
    (assert (= 234 (meta-eval eee G expr G)))
    (assert (= 234 (meta-eval eee G eee G expr G)))))
```
