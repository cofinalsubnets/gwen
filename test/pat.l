; test/pat.l -- the laws of `@`, pattern matching. the macro itself is the first half
; of love/post.l (on the book, so the corpus reads `@` bare); this file is what
; keeps it honest. read love/post.l for the grammar -- every tag it documents has
; its clause below, and the sections here follow that order.

(assert
 ; --- a nom binds, `_` swallows, and position is the whole grammar
 (2 = (@ '(1 2) [_a b] b))
 (2 = (@ '(1 2) [_ b] b))
 (1 = (@ '(1 2) [cap cup] cap))            ; the args are read outside: a nom may wear an accessor's name
 ('no = (@ '(1 2 3) [a b] a 'no))          ; [a b] is exactly two
 ('no = (@ '(1) [a b] a 'no))
 (() = (@ '(1 2 3) [a b] a))               ; no clause and no else: the zero point
 (93 = (@ '(3 (30 3)) [a [b c]] (c + (b * a))))
 ; --- a literal matches itself; 'a is the symbol, never a binding
 ('sym = (@ 'a 'a 'sym 'other)) ('other = (@ 'b 'a 'sym 'other))
 (7 = (@ 5 5 7 0)) (7 = (@ "hi" "hi" 7 0)) (0 = (@ "hi" "ho" 7 0))
 ('empty = (@ () [] 'empty 'no)) ('no = (@ 0 [] 'empty 'no))   ; () and 0 are distinct nothings
 ('lst = (@ '(1 2) '(1 2) 'lst 'no))       ; a quoted list is one literal, not a shape
 ; --- clauses, in source order, like ?
 (72 = (@ '(sum 32 40) ['sum a b] (a + b) ['product a b] (a * b) 0))
 (72 = (@ '(product 6 12) ['sum a b] (a + b) ['product a b] (a * b) 0))
 ; --- >< : the cons, and the one law -- a pattern is what the constructor builds
 (1 = (@ '(1 2 3) (a >< t) a))
 ('(2 3) = (@ '(1 2 3) (a >< t) t))
 ('(3) = (@ '(1 2 3) (a >< b >< t) t))     ; ..it chains, so the tail walks further in
 ('no = (@ 5 (a >< t) a 'no))
 ('no = (@ '(1) (a >< b >< t) t 'no))      ; ..and every step still owes a two?
 ('(1 2) = (@ '(1 2) (h >< t) (h >< t)))   ; the law: one text, matching and building
 (id? link ><)                             ; ..and >< is link, but only the sigils tag a pattern
 ; ..so a bracket stays closed: [a b >< t] is ([a b] >< t) is (>< (list a b) t)
 ('yes = (@ '((1 2) 3) [a b >< t] 'yes 'no))
 ('yes = (@ '((1 2) 3) ([a b] >< t) 'yes 'no))
 ('yes = (@ '((1 2) 3) ([a b] . t) 'yes 'no))
 ('no = (@ '(1 2 3) [a b >< t] 'yes 'no))  ; ..1 is not the chain (a b)
 ; ..and "this tag, then more" is the cons chain, which is what says it
 ('at3 = (@ '(lam x b) ('lam >< x >< b >< _) 'at3 'no))
 ('at5 = (@ '(lam x b j k) ('lam >< x >< b >< _) 'at5 'no))
 ('no = (@ '(lam x) ('lam >< x >< b >< _) 'yes 'no))
 ('exact = (@ '(lam x b) ['lam x b] 'exact 'no)) ('no = (@ '(lam x b j) ['lam x b] 'exact 'no))
 (2 = (@ '(1 2 3) (a >< 'b >< t) 0 (a >< b >< t) b))   ; a tail is a pattern like any other
 (6 = (: (sum l) (@ l (h >< t) (h + (sum t)) 0) (sum '(1 2 3))))
 ; --- a repeated nom matches non-linearly
 ('same = (@ '(1 1) [a a] 'same 'diff)) ('diff = (@ '(1 2) [a a] 'same 'diff))
 (7 = (@ '(7 7) [a a] a 0))
 ('pair = (@ '((1 2) (1 2)) [a a] 'pair 'no))
 ('no = (@ '((1 2) (1 3)) [a a] 'pair 'no))
 ('hit = (@ [3 {'f 3}] [n {'f n}] 'hit 'no))      ; ..across shapes, too
 ('no = (@ [3 {'f 4}] [n {'f n}] 'hit 'no))
 ('(1 1 1) = (@ '(1 1 1) [a a a] [a a a] 0))      ; ..and past two
 ('no = (@ '(1 1 2) [a a a] 'yes 'no))
 (2 = (@ '(1 1 2) [a a b] b 0))                    ; ..a fresh nom after a repeat still binds
 ; --- + : the split. the left run's length picks the split point, so nothing searches
 ('(3 4) = (@ '(1 2 3 4) ([a b] + t) t))
 (() = (@ '(1 2) ([a b] + t) t))              ; ..the rest may be empty and still fit
 ('no = (@ '(1) ([a b] + t) t 'no))           ; ..but the run may not come up short
 (10 = (@ '(1 2 3 4) ([a b] + [c d]) (a + b + c + d) 0))   ; ..either side is a pattern
 ('no = (@ '(1 2 3) ([a b] + [c d]) 'four 'no))
 (3 = (@ '(1 2 3 4) ([a b] + (c >< _)) c 0))  ; ..and it composes with the cons
 ('right = (@ 5 ([a] + t) 'no-chain 'right))
 ; ..which is how "this tag, then more" is said -- the loose arity, spelled
 ('fits = (@ '(lam x b) (['lam a b] + _) 'fits 'no))
 ('fits = (@ '(lam x b j k) (['lam a b] + _) 'fits 'no))
 ('no = (@ '(lam x) (['lam a b] + _) 'fits 'no))
 ('exact = (@ '(lam x b) ['lam a b] 'exact 'no)) ('no = (@ '(lam x b j) ['lam a b] 'exact 'no))
 ; --- a map: keys are expressions, values are patterns, the match is by subset
 (9 = (@ {'f 9} {'f x} x 0))
 (0 = (@ {'g 9} {'f x} x 0))
 (9 = (@ {'f 9 'g 1} {'f x} x 0))          ; subset: an extra key is fine
 (12 = (@ {'f 5 'g 7} {'f a 'g b} (a + b) 0))
 ('found = (@ {'f ()} {'f x} 'found 'no))   ; presence, not truth: a pinned () is present
 ('no = (@ 5 {'f x} 'found 'no))
 (: k 'f (9 = (@ {'f 9} {k x} x 0)))       ; ...and a key is an expression, so it can be computed
 ; --- a tray, spelled with the sigil the space just gave back
 (5 = (@ @(2 3) @(a b) (a + b) 0))
 (0 = (@ @(2 3 4) @(a b) (a + b) 0))       ; exactly two
 (0 = (@ '(2 3) @(a b) (a + b) 0))
 (6 = (@ @(1 @(3 4)) @(a @(b c)) (b + c - a) 0))
 ; --- the shapes nest, and a nested literal still binds nothing
 ('hit = (@ ['k {'f 1}] ['k {'f 1}] 'hit 'no))    ; the ` ctor: a quoted {..} is a FORM, not a map
 ('no = (@ ['k {'f 2}] ['k {'f 1}] 'hit 'no))
 ; --- a guard: (? g pat) is the shape and the side condition, (? g) the condition alone.
 ; g is an expression read for truth, never a pattern -- it binds nothing.
 ('up = (@ [1 2] (? (a < b) [a b]) 'up 'down))     ; ..and it sees what its own arm binds
 ('down = (@ [2 1] (? (a < b) [a b]) 'up 'down))
 ('small = (@ 3 (? (3 < 0)) 'neg (? (3 < 10)) 'small 'big))   ; ..alone, it is a conditional arm
 ('big = (@ 99 (? (99 < 0)) 'neg (? (99 < 10)) 'small 'big))
 (0 = (@ 5 (? (5 = 6)) 1 0))                       ; ..a guard that fails falls through, like a shape
 ; the order law: guards ride at the end of the conjunction, so an arm whose shape
 ; failed never runs its guard -- the accessors it reads have all been earned.
 ('no = (@ 5 (? (0 < a) [a]) 'yes 'no))
 ('no = (@ () (? (0 < a) [a b]) 'yes 'no))
 ; ..which is why a positional guard is a placement for the reader, not an earlier test
 (3 = (: lim 5 (@ [1 2] [a (? (a < lim) b)] (a + b) 'no)))    ; ..an outer nom resolves outward
 ('no = (: lim 5 (@ [9 2] [a (? (a < lim) b)] (a + b) 'no)))
 ('y = (@ '(1 1) (? (0 < a) [a a]) 'y 'n))         ; ..it composes with the non-linear nom
 ('n = (@ '(0 0) (? (0 < a) [a a]) 'y 'n))
 ('k = (@ '(2 3) (? (a < b) (? (0 < a) [a b])) 'k 'no))       ; ..and guards stack
 ('no = (@ '(2 1) (? (a < b) (? (0 < a) [a b])) 'k 'no))
 (2 = (@ @(1 2) @(a (? (1 < b) b)) b 0))           ; ..anywhere a pattern goes
 ; --- `&&` is the guard, left to right, and lowers to exactly the `?` above. the tag is
 ; what says expression: (two? x) is a fine untagged pattern as well as a fine call, so
 ; a bare operand could never be read for truth.
 (0 = (@ 5 (x && two? x) 1 0))    (1 = (@ 5 (x && atom? x) 1 0))
 (1 = (@ '(1 2) (x && two? x) 1 0))
 (1 = (@ 5 (c && c > 0) 1 0))     (0 = (@ -5 (c && c > 0) 1 0))
 ('y = (@ 5 (? (0 < c) c) 'y 'n)) ('y = (@ 5 (c && 0 < c) 'y 'n))  ; ..the two spellings
 ; `&&` is right-handed, so a chain arrives nested and is flattened: every
 ; operand past the first is its own guard.
 (1 = (@ 5 (c && 0 < c && c < 10) 1 0))
 (0 = (@ 50 (c && 0 < c && c < 10) 1 0))  (0 = (@ -5 (c && 0 < c && c < 10) 1 0))
 (2 = (@ [1 2] ([a b] && a < b) b 0))     (0 = (@ [2 1] ([a b] && a < b) b 0))
 ; ..a guard over an alternative still stands over every branch
 ('y = (@ 5 ((3 || 5) && 1) 'y 'n))       ('n = (@ 5 ((3 || 5) && 0) 'y 'n))
 ; ..and the shape is earned first, so a guard reading an accessor the shape did not
 ; earn is never run
 (0 = (@ 5 ([a b] && a < b) 1 0))
 (0 = (@ @(1 1) @(a (? (1 < b) b)) b 0))
 ('(3 4) = (@ '(1 2 3 4) (? (a < b) ([a b] + t)) t 'short))     ; ..a split included
 ('short = (@ '(1 2 3 4) (? (b < a) ([a b] + t)) t 'short))
 ; `&` and `|` are bitwise, never tags, so either one in a pattern is an untagged form and
 ; falls to the lax shape -- it fits nothing here, and says so silently. this is the pin.
 ('n = (@ 5 (3 | 5) 'y 'n))       ('n = (@ 5 (c & 0 < c) 'y 'n))
 ; --- alternatives: (p1 || p2) fits when either does.
 ('binder = (@ 'sig ('lam || 'pi || 'sig) 'binder 'other))
 ('other = (@ 'app ('lam || 'pi || 'sig) 'binder 'other))
 ('bit = (@ 1 (0 || 1) 'bit 'big)) ('big = (@ 2 (0 || 1) 'bit 'big))
 ('hit = (@ ['k 'b] ['k ('a || 'b)] 'hit 'no))                 ; ..one that binds nothing nests
 ('no = (@ ['k 'c] ['k ('a || 'b)] 'hit 'no))
 ; ..and one that binds is one clause per branch, so a nom may sit somewhere else in each
 ('x = (@ '(lam x q) (['lam n _] || ['pi n _ _] || ['let n _]) n 'none))
 ('y = (@ '(pi y q r) (['lam n _] || ['pi n _ _] || ['let n _]) n 'none))
 ('none = (@ '(app f) (['lam n _] || ['pi n _ _] || ['let n _]) n 'none))
 ('(2 1) = (@ ['b 1 2] (['a x y] || ['b y x]) [x y] 'n))       ; ..the set is the law, not the order
 ('(1 2) = (@ ['a 1 2] (['a x y] || ['b y x]) [x y] 'n))
 ; ..a guard over an alternative stands over every branch
 (5 = (@ ['a 5] (? (0 < n) (['a n] || ['b _ n])) n 'no))
 (7 = (@ ['b 'q 7] (? (0 < n) (['a n] || ['b _ n])) n 'no))
 ('no = (@ ['a -1] (? (0 < n) (['a n] || ['b _ n])) n 'no))
 ; ..the first branch that matches wins, and supplies every nom: one it does not bind
 ; reads (), it does not fall through to a later branch. so a body never wears bindings
 ; from two shapes at once, and the branches need not bind the same noms.
 ('(1 ()) = (@ ['a 1] (['a x] || ['b y]) [x y] 'n))
 ('(() 1) = (@ ['b 1] (['a x] || ['b y]) [x y] 'n))
 ; ..and one that binds may nest -- same law, one clause, the nom read from the winner
 ('(7 ()) = (@ ['k 7] ['k (a || [a b])] [a b] 'n))
 ('(7 8) = (@ ['k [7 8]] ['k ([a b] || a)] [a b] 'n))
 ('((7 8) ()) = (@ ['k [7 8]] ['k (a || [a b])] [a b] 'n))   ; ..`a` fits first, so `a` it is
 ; --- parens are the form, and exact on the tail the way brackets always were:
 ; (a b c) is three and so is [a b c]. `.` is how a tail says it will take more.
 (4 = (@ '(1 2 3) (a b c) (a + c)))
 ('no = (@ '(1 2 3) (a) 'one 'no))                  ; ..shorter does not fit
 ('no = (@ '(1 2 3) (a b) 'two 'no))
 ('one = (@ '(1 2 3) (a . _) 'one 'no))             ; ..the tail, asked for out loud
 ('two = (@ '(1 2 3) (a . b . _) 'two 'no))         ; ..and `.` is right-handed, so it nests
 ('no = (@ '(1 2) (a b c) 'three 'no))              ; ..fewer never fit either way
 ('no = (@ 5 (a b) 'chain 'no))                     ; ..and a non-chain is not a form
 ('no = (@ '(lam x b extra) ('lam n _) n 'no))      ; ..a term's tag+args, counted
 ('x = (@ '(lam x b extra) ('lam . n . _) n 'no))   ; ..or with room left for the rest
 ('no = (@ '(pi x b) ('lam . n . _) n 'no))
 (3 = (@ '(1 2 3) [a b c] c 0))                     ; ..the bracket beside it reads the same
 (0 = (@ '(1 2 3 4) [a b c] c 0))
 ; ..(X..) is [X..], so `+` is for a tail you want to NAME
 ('(3 4) = (@ '(1 2 3 4) ([a b] + t) t 'no))
 ; ..and `+` is left-handed, so a multi-split arrives as a + whose left is a +: cat is
 ; associative and so is this -- ([a b] + [c] + t) is three runs, the last one named.
 ('(4 5) = (@ '(1 2 3 4 5) ([a b] + [c] + t) t 'no))
 (3 = (@ '(1 2 3 4 5) ([a b] + [c] + t) c 0))
 ('no = (@ '(1 2) ([a b] + [c] + t) t 'no))
 ; --- guards stack without nesting: every operand but the last is a condition
 (4 = (@ '(1 2 3) (? (1 < 2) (2 < 3) (a b c)) (a + c) 'no))
 ('no = (@ '(1 2 3) (? (1 < 2) (3 < 2) (a b c)) (a + c) 'no))
 ; ..and the pair bottoms out consistently: [] is the list at zero and () is the form
 ; at zero, and both are the empty chain and fit nothing else. `_` is the don't-care.
 ('no = (@ 5 () 'any 'no)) ('no = (@ '(1 2) () 'any 'no)) ('any = (@ () () 'any 'no))
 ('any = (@ 5 _ 'any 'no)) ('any = (@ '(1 2) _ 'any 'no)) ('any = (@ () _ 'any 'no))
 ('empty = (@ () [] 'empty 'no)) ('no = (@ '(1) [] 'empty 'no)) ('no = (@ 5 [] 'empty 'no))
 (1 = (@ '(1 2) [a _] a 'no))                      ; .._ is the don't-care in a position
 ; --- and-patterns, the dual of ||: both must fit. `&` says it too.
 ('(lam x b) = (@ '(lam x b) (v @ ['lam p q]) v 'no))   ; ..name the whole, take it apart
 ('b = (@ '(lam x b) (v @ ['lam p q]) q 'no))
 ('no = (@ '(pi x b) (v @ ['lam p q]) v 'no))
 ('(1 2) = (@ '(1 2) (x @ [a b]) x 'no))
 ; ..both branches read the same value, so a nom in both is the ordinary non-linear one
 (3 = (@ '(3 3) ([a _] @ [_ a]) a 'no)) ('no = (@ '(3 4) ([a _] @ [_ a]) a 'no))
 ; --- the as-pattern: (v @ pat) is that same conjunction in haskell's spelling, so every
 ; && law above holds of it. spaced -- glued v@pat is one nom (opfix splits only a leading
 ; sigil) and glued @( is the tray, so the space is what tells the three apart.
 ('(lam x b) = (@ '(lam x b) (v @ ['lam p q]) v 'no))
 ('b = (@ '(lam x b) (v @ ['lam p q]) q 'no))
 ('no = (@ '(pi x b) (v @ ['lam p q]) v 'no))
 (3 = (@ '(3 3) ([a _] @ [_ a]) a 'no))            ; ..non-linear through it too
 ; --- the pin, elixir's ^x: glued ^ names jot, and jot in a pattern is an equality against
 ; x read from outside -- the one way a pattern spends a nom without binding it.
 (: a 1 (9 = (@ '(1 9) (^a b) b 'no)))             ; ..fits when the value equals the outer a
 (: a 1 ('no = (@ '(7 9) (^a b) b 'no)))           ; ..and misses when it does not
 (: a 1 (7 = (@ '(7 9) (a b) a 'no)))              ; ..unpinned, the same nom rebinds instead
 (: a 5 ('both = (@ '(5 5) (x ^a) 'both 'no)))     ; ..so (x ^a) is a pair equal to an outer a
 (: a 5 ('no = (@ '(5 6) (x ^a) 'both 'no)))
 (9 = (@ '(5 9) (^(2 + 3) b) b 'no))               ; ..and what is pinned is any expression
 ('(0 1 2) = ^3)                                   ; ..while outside a pattern ^ is plain jot
 (7 = (3 ^ 4))                                     ; ..and spaced ^ is still xor: the lane is the key
 ; --- a lambda is a form like any other: (\ x b) builds one, so it matches one
 ('form = (@ '(\ x b) (\ q r) 'form 'no))
 ('lit = (@ 'x (\ x) 'lit 'no))                     ; ..at one operand it is still the literal
 ('no = (@ 'y (\ x) 'lit 'no))
 ; ..and the nesting is ordinary -- a form inside a form, a bracket inside a form
 ('deep = (@ '(a (b c) d) (_ (_ 'c) _) 'deep 'no))
 ('no = (@ '(a (b q) d) (_ (_ 'c) _) 'deep 'no))
 )

; --- a pattern in a \ param, which is opfix's doing and not this macro's: a param that is
; not a nom becomes a fresh one and the body takes it apart. (\ [a b] x) is (\ v (@ v [a b] x)),
; so every shape above works there, and a define-sugar binder is a \ like any other.
(assert
 (7 = ((\ [a b] (a + b)) '(3 4)))
 ('(6 20) = (map (\ [a b] (a * b)) '((2 3) (4 5))))      ; ..the everyday one
 ('q = ((\ ['lam x b] x) '(lam q r)))
 (5 = ((\ [a b] [c d] (a + d)) '(1 2) '(3 4)))           ; ..several, in order
 (11 = ((\ x [a b] (x + a)) 10 '(1 2)))                  ; ..mixed with ordinary noms
 (() = ((\ [a b] a) 5))                                  ; ..no fit, no else: the zero point
 ('up = ((\ (? (a < b) [a b]) 'up) '(1 2)))              ; ..guards compose
 (() = ((\ (? (a < b) [a b]) 'up) '(2 1)))
 ('q = ((\ (['lam x] || ['pi x]) x) '(pi q)))            ; ..so do alternatives
 ('z = ((\ ('lam . n . _) n) '(lam z b extra)))          ; ..and a tail
 ; ..an inner param still shadows: the rewrite is a binder, not a splice
 (9 = (((\ [a b] (\ a a)) '(1 2)) 9))
 ; ..a define-sugar binder is one too, at every level of a curried one. position 0 is the
 ; name (or the binder carrying it) and is never a pattern, which is why the two cannot clash.
 (7 = (: (f [a b]) (a + b) (f '(3 4))))
 (11 = (: (g x [a b]) (x + a) (g 10 '(1 2))))
 ('(1 2 3 4) = (: ((k [a b]) [c d]) [a b c d] ((k '(1 2)) '(3 4))))
 (7 = (: ((k a) b) (a + b) ((k 3) 4))))                  ; ..and an all-nom binder is untouched

; there is nothing ungated here any more, because `@` no longer scares at all: every
; pattern is read. a head that names no tag, or names one and misses its shape, is the
; lax form pattern; an alternative binds the union with the winner's values. What used
; to need a deferred-expansion lane to test is now an ordinary law above.
