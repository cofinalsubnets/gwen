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
(: ; self evaluator test from test/meta.gw
 evr '(: ; expression for the evaluator
  (meta-eval x) (?
   (symp x) (\ l (l x))
   (not (twop x)) (const x)
   (: x0 (car x) a (cdr x)
    (? (= x0 '`) (const (car a))
       (= x0 ',) (foldl 0 (\ a b l (, (a l) (b l)))
                          (map meta-eval a))
       (= x0 '\) (foldr (meta-eval (last a))
                        (\ a f l x (f (\ y (? (= y a) x (l y)))))
                        (init a))
       (= x0 '?) (cond-loop a id)
       (= x0 ':) (let-loop a id)
       (: y (map meta-eval x)
        (\ l (foldl id id (map (\ x (x l)) y)))))))

  (cond-loop a f) (?
   (nilp a) (cond-loop (cons 0 0) f)
   (nilp (cdr a)) (f (meta-eval (car a)))
   (: ant (meta-eval (car a)) con (meta-eval (cadr a))
    (cond-loop (cddr a) (\ alt (f (\ l (? (ant l) (con l) (alt l))))))))

  (let-loop a b m) (?
   (nilp a) (let-loop (cons 0 0) b m)
   (nilp (cdr a)) (meta-eval (car a) (b m))
   (desugar (car a) (cadr a) (\ k v
    (: t (tnew 0) get (tget 0 t) set (tset t 0)
     (let-loop (cddr a) (\ l (, (set (meta-eval v (b l))) l))
                        (\ x (? (= x k) (get 0) (m x))))))))

  (desugar k v c)
   (? (twop k) (desugar (car k) (cons '\ (append (cdr k) (cons v 0))) c)
               (c k v))
  ;return
  meta-eval)
 meta-eval (ev evr)
 expr '((\ a b (: c (+ a 9) d (+ c b) (* c d))) 4 5)
 G (tget 0 global-namespace)
 (, (assert (= 234 (ev expr)))
    (assert (= 234 (meta-eval expr G)))
    (assert (= 234 (meta-eval evr G expr G)))
    (assert (= 234 (meta-eval evr G evr G expr G)))))
```
