; crew/kore/bc.l -- bc: the arbitrary-precision calculator language. a toolbox
; file, no seat -- kore is its door, and it stands on the u-floor alone.
;
;   bc [-l] [-q] [FILE..]        the files in turn, then stdin, as POSIX says
;
; A NUMBER IS [v s]: the exact integer v over 10^s. every digit is love's own
; bigint, so nothing rounds and nothing overflows. the scale rules are POSIX's,
; and every one of them TRUNCATES -- which is why love's // (toward zero, never
; floored) is the divide this file wants:
;   + -  max(sa, sb)                  *  min(sa + sb, max(scale, sa, sb))
;   /    scale                        %  max(scale + sb, sa)
;   ^    min(sa * b, max(scale, sa))  sqrt  max(scale, sa)
;
; the dialect is GNU's: POSIX bc plus multi-character names, print, else, halt,
; last (and its `.` spelling), && || !, and the # comment.
;
; out of dialect, deliberately -- each is a rung, not an oversight:
; ⚠ read(). the program and read()'s answer come off ONE stdin, and this reads a
;   whole item before it runs any of it.
; ⚠ -s and -w. both only refuse or complain about what we already accept, and a
;   -w that warns about nothing is worse than no -w at all.
; ⚠ quit is EXECUTED here. GNU's fires when the LEXER reaches it, so GNU leaves
;   on `if (0) quit`; ours does not, and halt is the same statement either way.
(module 'kore
(:
   ; -- numbers ---------------------------------------------------------------
   bn-zero [0 0]
   bn-one [1 0]
   (bn-p10 k) (** 10 k)
   (bn-wide a b) (? (a < b) b a)
   ; rescale to exactly s: zeros on the right, or a truncation toward zero
   (bn-at n s)
    (: d (s - <>n)
       (? (d = 0) n
          (0 < d) [(<n * bn-p10 d) s]
          [(// <n (bn-p10 (0 - d))) s]))
   (bn-zero? n) (<n = 0)
   (bn-int n) <(bn-at n 0)
   (bn-cmp a b)
    (: s (bn-wide <>a <>b)
       x <(bn-at a s) y <(bn-at b s)
       (? (x < y) -1 (y < x) 1 0))
   (bn-add a b) (: s (bn-wide <>a <>b) [(<(bn-at a s) + <(bn-at b s)) s])
   (bn-sub a b) (: s (bn-wide <>a <>b) [(<(bn-at a s) - <(bn-at b s)) s])
   (bn-mul sc a b)
    (: full (<>a + <>b)
       want (bn-wide sc (bn-wide <>a <>b))
       (bn-at [(<a * <b) full] (? (full < want) full want)))
   (bn-div sc a b)
    (: _ (? (<b = 0) (bc-err "Divide by zero") 0)
       num (<a * bn-p10 (sc + <>b))
       den (<b * bn-p10 <>a)
       [(// num den) sc])
   ; a % b is a - (a/b)*b with the quotient taken at scale and the product kept
   ; whole, which is where the result's max(scale + sb, sa) digits come from
   (bn-mod sc a b)
    (: q (bn-div sc a b)
       qb [(<q * <b) (<>q + <>b)]
       s (bn-wide <>a <>qb)
       [(<(bn-at a s) - <(bn-at qb s)) s])
   (bn-pow sc a b)
    (: e (bn-int b)                            ; the exponent's own scale is dropped
       n (abs e)
       (? (e = 0) bn-one
          (0 < e) (: f (<>a * n)
                     want (bn-wide sc <>a)
                     (bn-at [(** <a n) f] (? (f < want) f want)))
          (bn-div sc bn-one [(** <a n) (<>a * n)])))
   (bn-isqrt v)                                ; floor sqrt, newton from above
    (? (v < 2) v
       (: t (show v)
          (go x) (: y (// (x + (// v x)) 2) (? (y < x) (go y) x))
          (go (bn-p10 ((// #t 2) + 1)))))
   ; ⚠ 0 and 1 answer THEMSELVES, scale and all -- GNU's own two short circuits,
   ; and the reason `scale=5; sqrt(1)` is 1 where sqrt(4) is 2.00000
   (bn-sqrt sc a)
    (: _ (? (<a < 0) (bc-err "Square root of a negative number") 0)
       (? (<a = 0) bn-zero
          (bn-cmp a bn-one = 0) bn-one
          (: r (bn-wide sc <>a)
             [(bn-isqrt <(bn-at a (2 * r))) r])))
   ; bc's length: the significant digits, where a whole zero still owns one
   (bn-len n)
    (: v (abs <n) s <>n
       (? (v = 0) (? (s = 0) 1 s)
          (: t (show v) (? (#t < s) s #t))))

   ; -- writing a number ------------------------------------------------------
   bc-figs "0123456789ABCDEF"
   (bc-zpad k t) (? (#t < k) (bc-zpad k ("0" + t)) t)
   (bc-padl k ds) (? (#ds < k) (bc-padl k (0 . ds)) ds)
   ; a fraction of scale s owes the least k with base^k >= 10^s digits -- asked
   ; in whole numbers, never through a log, so no float rounding can shift it
   (bc-fdigits b s)
    (: t (bn-p10 s)
       (go k p) (? (t <= p) k (go (k + 1) (p * b)))
       (go 0 1))
   (bc-basedig b v)                            ; -> the base-b digits, () for 0
    (: (go x acc) (? (x = 0) acc (go (// x b) ((x % b) . acc)))
       (go v ()))
   ; a base over 16 has no charm per digit, so each one is written as a
   ; zero-padded decimal with a space in front -- all but the first of a
   ; FRACTION, which sits straight against the point
   (bc-digs b ds lead)
    (: wide (16 < b)
       w (: t (show (b - 1)) #t)
       o (jug 0)
       (go m first)
        (? !(two? m) (slurp o)
           (: _ (? wide (: _ (? (first && !lead) 0 (put o 32))
                           (say o (bc-zpad w (show <m))))
                   (put o (bc-figs <m)))
              (go >m 0)))
       (go ds 1))
   (bc-show st n)
    (: b (peep st 'obase 10)
       v <n s <>n
       (? (v = 0) "0"
          (: a (abs v)
             p (bn-p10 s)
             ip (bc-digs b (bc-basedig b (// a p)) 1)
             fp (? (s = 0) ""
                   (: k (bc-fdigits b s)
                      fn (// ((a % p) * (** b k)) p)
                      ("." + bc-digs b (bc-padl k (bc-basedig b fn)) 0)))
             ((? (v < 0) "-" "") + ip + fp))))

   ; -- the output column -----------------------------------------------------
   ; ⚠ EVERY byte bc prints goes through here and shares ONE count: 68 columns,
   ; then a backslash and a break. that is why a long string wraps exactly as a
   ; long number does, and why two numbers in one `print` wrap as a single run
   (bc-put st c)
    (? (c = 10) (: _ (pin st 'col 0) (put out 10))
       (: n (peep st 'col 0)
          _ (? (n = 68) (: _ (put out 92) _ (put out 10) (pin st 'col 0)) 0)
          _ (put out c)
          (pin st 'col ((peep st 'col 0) + 1))))
   (bc-say st t)
    (: n #t
       (go k) (? (n <= k) 0 (: _ (bc-put st (t k)) (go (k + 1))))
       (go 0))
   (bc-print st n) (: _ (pin st 'last n) (bc-say st (bc-show st n)))

   ; -- the complaints --------------------------------------------------------
   ; ⚠ `?` SUMS what it is handed, so a signal or a node may not be asked for its
   ; presence with one -- ['ret v] carrying a negative v reads as false, and the
   ; return is silently dropped. presence is a PATTERN here, everywhere
   (bc-nil? ()) 1
   (bc-there? v) !(bc-nil? v)
   (bc-err m) (scare 'bc-err m)
   (bc-syn m) (scare 'bc-syn m)
   (bc-warn m) (: _ (say err ("bc: warning: " + m)) (put err 10))

   ; -- the lexer -------------------------------------------------------------
   (bc-dig? c) (48 <= c <= 57)
   (bc-low? c) (97 <= c <= 122)
   (bc-nmc? c) (bc-low? c || bc-dig? c || c = 95)
   (bc-fig? c) (bc-dig? c || 65 <= c <= 70)
   (bc-dv c) (? (bc-dig? c) (c - 48) (65 <= c <= 70) (c - 55) -1)
   bc-kws '("define" "auto" "break" "continue" "return" "if" "else" "while"
            "for" "quit" "halt" "print" "length" "scale" "sqrt" "ibase"
            "obase" "last" "read")
   bc-ops '("&&" "||" "==" "!=" "<=" ">=" "++" "--" "+=" "-=" "*=" "/=" "%="
            "^=" "+" "-" "*" "/" "%" "^" "=" "<" ">" "!" "(" ")" "[" "]" "{"
            "}" "," ";")
   (bc-op-at s n k)                            ; -> [text next] | ()
    (: (has t) (: m #t (k + m <= n && t = snip s k (k + m)))
       (try (w . u)) (? (has w) [w (k + #w)] (try u))
       (try bc-ops))
   ; GNU's print escapes; a backslash before anything else DROPS both charms
   (bc-esc c) (c @ 110 10  116 9  114 13  97 7  98 8  102 12  113 34  92 92  -1)
   (bc-strlit s n i)                           ; past the quote -> [text next]
    (: o (jug 0)
       (go k) (? (n <= k) (bc-syn "unterminated string")
                 (: c (s k)
                    (? (c = 34) [(slurp o) (k + 1)]
                       (c = 92 && k + 1 < n)
                        (: e (bc-esc (s (k + 1)))
                           _ (? (e < 0) 0 (put o e))
                           (go (k + 2)))
                       (: _ (put o c) (go (k + 1))))))
       (go i))
   ; ⚠ A NUMERAL KEEPS ITS TEXT. its digits are worth what the ibase of the
   ; moment says, and `ibase=16` is a STATEMENT -- so nothing may be converted
   ; before the statement ahead of it has run
   (bc-lex src)
    (: n #src
       (eol j) (? (j < n && src j != 10) (eol (j + 1)) j)
       (blk j) (? (n <= j + 1) (bc-syn "unterminated comment")
                  (src j = 42 && src (j + 1) = 47) (j + 2)
                  (blk (j + 1)))
       (fig j) (? (j < n && (bc-fig? (src j) || src j = 46)) (fig (j + 1)) j)
       (wd j) (? (j < n && bc-nmc? (src j)) (wd (j + 1)) j)
       (go k acc)
        (? (n <= k) (rev (['eof ""] . acc))
           (: c (src k)
              (? (c = 32 || c = 9 || c = 13) (go (k + 1) acc)
                 (c = 35) (go (eol k) acc)
                 (c = 47 && k + 1 < n && src (k + 1) = 42) (go (blk (k + 2)) acc)
                 ; a backslash-newline is a continuation: neither token nor break
                 (c = 92) (go (? (k + 1 < n && src (k + 1) = 10) (k + 2) (k + 1)) acc)
                 (c = 10) (go (k + 1) (['nl ""] . acc))
                 (c = 34) (bc-strlit src n (k + 1) @ [s j] (go j (['str s] . acc)) ())
                 ; a lone dot is `last`; a dot with a figure behind it opens a numeral
                 (c = 46 && !(k + 1 < n && bc-fig? (src (k + 1))))
                  (go (k + 1) (['kw "last"] . acc))
                 (bc-fig? c || c = 46)
                  (: e (fig k) (go e (['num (snip src k e)] . acc)))
                 (bc-low? c)
                  (: e (wd k)
                     w (snip src k e)
                     (go e ((? (member? w bc-kws) ['kw w] ['name w]) . acc)))
                 (bc-op-at src n k @ [w j] (go j (['op w] . acc))
                   (bc-syn ("unexpected character " + string c))))))
       (go 0 ()))
   ; ⚠ a numeral is its own value when it is ONE figure -- `ibase=2; 9` is 9, not
   ; 1. every other numeral clamps a figure at or above the base to base-1
   (bc-numeral t ib)
    (: n #t
       (dot j) (? (n <= j) n (t j = 46) j (dot (j + 1)))
       i (dot 0)
       ip (snip t 0 i)
       fp (? (i < n) (snip t (i + 1) n) "")
       one (n = 1 && 0 <= bc-dv (t 0))
       (dg c) (: d (bc-dv c) (? one d (d < ib) d (ib - 1)))
       (acc s j v) (? (#s <= j) v (acc s (j + 1) ((v * ib) + dg (s j))))
       iv (acc ip 0 0)
       fk #fp
       (? (fk = 0) [iv 0]
          [((iv * bn-p10 fk) + (// ((acc fp 0 0) * bn-p10 fk) (** ib fk))) fk]))

   ; -- the parser ------------------------------------------------------------
   ; every door answers [node rest]; a refusal scares, so no arm carries a
   ; failure shape
   (bc-k? ts k) (two? ts && <<ts == k)
   (bc-op? ts v) (two? ts && <<ts == 'op && <><ts = v)
   (bc-kwd? ts v) (two? ts && <<ts == 'kw && <><ts = v)
   (bc-eof? ts) (? (two? ts) (bc-k? ts 'eof) 1)
   (bc-nl ts) (? (bc-k? ts 'nl) (bc-nl >ts) ts)
   (bc-term ts) (? (bc-k? ts 'nl || bc-op? ts ";") (bc-term >ts) ts)
   (bc-want ts v m) (? (bc-op? ts v) >ts (bc-syn m))
   (bc-lval? e) (<e @ ('var || 'arr || 'reg) 1 0)
   bc-assigns '("=" "+=" "-=" "*=" "/=" "%=" "^=")
   bc-rels '("<" "<=" "==" "!=" ">=" ">")
   bc-regs '("scale" "ibase" "obase" "last")
   (bc-args ts acc)                            ; a call's operands, closed by )
    (? (bc-op? ts ")") [(rev acc) ts]
       (bc-expr ts @ [e r]
         (? (bc-op? r ",") (bc-args (bc-nl >r) (e . acc)) [(rev (e . acc)) r]) ()))
   (bc-prim ts)
    (? !(two? ts) (bc-syn "unexpected end of program")
       (: t <ts k <t v <>t r >ts
          (? (id? k 'num) [['num v] r]
             (id? k 'str) (bc-syn "a string is not an expression")
             (id? k 'name)
              ; ⚠ `b[]` is the whole ARRAY, and only a call has any use for one --
              ; a subscript that is not there is not an empty one
              (? (bc-op? r "[")
                  (? (bc-op? >r "]") [['aref v] >>r]
                     (bc-expr >r @ [e r2]
                       [['arr v e] (bc-want r2 "]" "expected ] after a subscript")] ()))
                 (bc-op? r "(")
                  (bc-args (bc-nl >r) () @ [es r2]
                    [['call v es] (bc-want r2 ")" "expected ) after the arguments")] ())
                 [['var v] r])
             (id? k 'kw)
              ; ⚠ `scale` is BOTH: the register, and the function when a ( follows.
              ; the register arm has to come second or scale(x) never reads
              (? ((member? v '("length" "sqrt")) || ((v = "scale") && bc-op? r "("))
                  (: r2 (bc-want r "(" ("expected ( after " + v))
                     (bc-expr (bc-nl r2) @ [e r3]
                       [[(? (v = "length") 'len (v = "scale") 'scl 'sqrt) e]
                        (bc-want r3 ")" ("expected ) after " + v))]
                       ()))
                 (member? v bc-regs) [['reg (name v)] r]
                 (v = "read") (bc-syn "read() is out of dialect")
                 (bc-syn ("`" + v + "' is not an expression")))
             (bc-op? ts "(")
              (bc-expr (bc-nl r) @ [e r2]
                [['grp e] (bc-want r2 ")" "expected )")] ())
             (bc-op? ts "++" || bc-op? ts "--")
              (bc-prim r @ [e r2]
                (? (bc-lval? e) [['incr 1 (? (v = "++") 1 -1) e] r2]
                   (bc-syn (v + " wants a variable"))) ())
             (bc-syn ("unexpected `" + (? (string? v) v "") + "'")))))
   (bc-post ts)
    (bc-prim ts @ [e r]
      (? ((bc-op? r "++" || bc-op? r "--") && bc-lval? e)
         [['incr 0 (? (<><r = "++") 1 -1) e] >r]
         [e r])
      ())
   ; ⚠ the unary minus binds TIGHTER than the power, which is why -2^2 is 4 and
   ; not -4; the exponent takes a whole neg, which is what makes 2^-2 read
   (bc-neg ts)
    (? (bc-op? ts "-") (bc-neg >ts @ [e r] [['neg e] r] ())
       (bc-post ts))
   (bc-pow ts)
    (bc-neg ts @ [a r]
      (? (bc-op? r "^") (bc-pow >r @ [b r2] [['pow a b] r2] ()) [a r])
      ())
   (bc-mul ts)
    (bc-pow ts @ [a r]
      (: (go v m)
          (? (bc-op? m "*" || bc-op? m "/" || bc-op? m "%")
             (bc-pow >m @ [w m2] (go ['bin <><m v w] m2) ())
             [v m])
         (go a r))
      ())
   (bc-add ts)
    (bc-mul ts @ [a r]
      (: (go v m)
          (? (bc-op? m "+" || bc-op? m "-")
             (bc-mul >m @ [w m2] (go ['bin <><m v w] m2) ())
             [v m])
         (go a r))
      ())
   (bc-rel ts)
    (bc-add ts @ [a r]
      (: (go v m)
          (? (bc-k? m 'op && member? <><m bc-rels)
             (bc-add >m @ [w m2] (go ['cmp <><m v w] m2) ())
             [v m])
         (go a r))
      ())
   (bc-not ts)
    (? (bc-op? ts "!") (bc-not >ts @ [e r] [['not e] r] ())
       (bc-rel ts))
   (bc-and ts)
    (bc-not ts @ [a r]
      (: (go v m)
          (? (bc-op? m "&&") (bc-not >m @ [w m2] (go ['and v w] m2) ()) [v m])
         (go a r))
      ())
   (bc-or ts)
    (bc-and ts @ [a r]
      (: (go v m)
          (? (bc-op? m "||") (bc-and >m @ [w m2] (go ['or v w] m2) ()) [v m])
         (go a r))
      ())
   (bc-expr ts)
    (bc-or ts @ [n r]
      (? ((bc-k? r 'op) && (member? <><r bc-assigns) && bc-lval? n)
         (bc-expr >r @ [v r2] [['asgn <><r n v] r2] ())
         [n r])
      ())

   ; -- statements ------------------------------------------------------------
   (bc-body ts) (bc-stmt (bc-nl ts))           ; the statement a control form owns
   (bc-block ts)                               ; { .. } -> [stmts rest]
    (: r (bc-want ts "{" "expected {")
       (go m acc)
        (: m2 (bc-term m)
           (? (bc-op? m2 "}") [(rev acc) >m2]
              (bc-eof? m2) (bc-syn "unexpected end of program: a { is unclosed")
              (bc-stmt m2 @ [s r2] (go r2 (s . acc)) ())))
       (go r ()))
   (bc-pargs ts acc)                           ; print's list, closed by the line
    (? (bc-k? ts 'str)
       (: it ['say <><ts]
          (? (bc-op? >ts ",") (bc-pargs (bc-nl >>ts) (it . acc))
             [(rev (it . acc)) >ts]))
       (bc-expr ts @ [e r]
         (? (bc-op? r ",") (bc-pargs (bc-nl >r) (e . acc)) [(rev (e . acc)) r])
         ()))
   (bc-fpart ts cl)                            ; one of for's three, maybe empty
    (? (bc-op? ts cl) [() >ts]
       (bc-expr ts @ [e r] [e (bc-want r cl ("expected " + cl + " in for"))] ()))
   (bc-stmt ts)
    (? (bc-op? ts "{") (bc-block ts @ [ss r] [['blk ss] r] ())
       (bc-k? ts 'str) [['say <><ts] >ts]
       (bc-kwd? ts "print") (bc-pargs (bc-nl >ts) () @ [es r] [['print es] r] ())
       (bc-kwd? ts "break") [['brk] >ts]
       (bc-kwd? ts "continue") [['cnt] >ts]
       (bc-kwd? ts "quit") [['quit] >ts]
       (bc-kwd? ts "halt") [['quit] >ts]
       ; POSIX spells it `return (E)`, GNU takes a bare E too -- one expression
       ; walk reads both, and only a statement's end means no value at all
       (bc-kwd? ts "return")
        (: r >ts
           (? ((bc-k? r 'nl) || (bc-op? r ";") || (bc-op? r "}") || bc-eof? r)
              [['ret ()] r]
              (bc-expr r @ [e r2] [['ret e] r2] ())))
       (bc-kwd? ts "if")
        (: r (bc-want >ts "(" "expected ( after if")
           (bc-expr (bc-nl r) @ [c r2]
             (: r3 (bc-want r2 ")" "expected ) after the if condition")
                (bc-body r3 @ [a r4]
                  (: r5 (bc-term r4)
                     (? (bc-kwd? r5 "else") (bc-body >r5 @ [b r6] [['if c a b] r6] ())
                        [['if c a ()] r4]))
                  ()))
             ()))
       (bc-kwd? ts "while")
        (: r (bc-want >ts "(" "expected ( after while")
           (bc-expr (bc-nl r) @ [c r2]
             (: r3 (bc-want r2 ")" "expected ) after the while condition")
                (bc-body r3 @ [b r4] [['while c b] r4] ()))
             ()))
       (bc-kwd? ts "for")
        (: r (bc-want >ts "(" "expected ( after for")
           (bc-fpart (bc-nl r) ";" @ [i r2]
             (bc-fpart (bc-nl r2) ";" @ [c r3]
               (bc-fpart (bc-nl r3) ")" @ [u r4]
                 (bc-body r4 @ [b r5] [['for i c u b] r5] ()) ())
               ())
             ()))
       (bc-op? ts ";") [['nop] >ts]
       (bc-expr ts @ [e r] [['expr e] r] ()))
   (bc-params ts acc)                          ; a define's parameters -> [ps rest]
    (? (bc-op? ts ")") [(rev acc) ts]
       !(bc-k? ts 'name) (bc-syn "expected a parameter name")
       (: nm <><ts
          arr (bc-op? >ts "[")
          r (? arr (bc-want >>ts "]" "expected ] in a parameter") >ts)
          it (? arr ['a nm] ['s nm])
          (? (bc-op? r ",") (bc-params (bc-nl >r) (it . acc)) (bc-params r (it . acc)))))
   (bc-autos ts acc)                           ; the auto lines -> [names rest]
    (: m (bc-term ts)
       (? !(bc-kwd? m "auto") [(rev acc) ts]
          (: (go u a)
              (? !(bc-k? u 'name) (bc-syn "expected an auto name")
                 (: nm <><u
                    arr (bc-op? >u "[")
                    r (? arr (bc-want >>u "]" "expected ] after an auto") >u)
                    it (? arr ['a nm] ['s nm])
                    (? (bc-op? r ",") (go (bc-nl >r) (it . a)) [(it . a) r])))
             (go >m acc @ [a2 r] (bc-autos r a2) ()))))

   ; -- the environment -------------------------------------------------------
   (bc-vars st) (peep st 'v ())
   (bc-arrs st) (peep st 'a ())
   (bc-fns st) (peep st 'f ())
   (bc-sc st) (peep st 'scale 0)
   (bc-arr st nm)
    (: t (bc-arrs st)
       a (peep t nm ())
       (? (bc-there? a) a (: b (tablet 0) _ (pin t nm b) b)))
   (bc-getreg st nm)
    (? (nm == 'last) (peep st 'last bn-zero) [(peep st nm 0) 0])
   (bc-setreg st nm v)
    (: k (bn-int v)
       (? (nm == 'scale)
           (pin st 'scale (? (k < 0) (: _ (bc-warn "negative scale, set to 0") 0) k))
          (nm == 'ibase)
           (pin st 'ibase (? (k < 2) (: _ (bc-warn "ibase too small, set to 2") 2)
                              (16 < k) (: _ (bc-warn "ibase too large, set to 16") 16)
                              k))
          (nm == 'obase)
           (pin st 'obase (? (k < 2) (: _ (bc-warn "obase too small, set to 2") 2) k))
          (pin st 'last v)))
   ; a PLACE is the lvalue with its subscript already counted, so `a[i++] = 1`
   ; moves i once
   (bc-place st lv)
    (<lv @ 'var ['v <>lv]
           'arr ['a <>lv (bn-int (bc-ev st <>>lv))]
           'reg ['r <>lv]
           (bc-err "not a variable"))
   (bc-pget st p)
    (<p @ 'v (peep (bc-vars st) <>p bn-zero)
          'a (peep (bc-arr st <>p) <>>p bn-zero)
          (bc-getreg st <>p))
   (bc-pset st p v)
    (<p @ 'v (pin (bc-vars st) <>p v)
          'a (pin (bc-arr st <>p) <>>p v)
          (bc-setreg st <>p v))

   ; -- expressions -----------------------------------------------------------
   (bc-true? n) !(bn-zero? n)
   (bc-bool b) (? b bn-one bn-zero)
   (bc-ev st nd)
    (: sc (bc-sc st)
       (<nd @ 'num (bc-numeral <>nd (peep st 'ibase 10))
             'grp (bc-ev st <>nd)
             'var (peep (bc-vars st) <>nd bn-zero)
             'arr (peep (bc-arr st <>nd) (bn-int (bc-ev st <>>nd)) bn-zero)
             'reg (bc-getreg st <>nd)
             'neg (: v (bc-ev st <>nd) [(0 - <v) <>v])
             'not (bc-bool !(bc-true? (bc-ev st <>nd)))
             ; bc's && and || are SHORT, and love's are the same two operators --
             ; the right side of a settled one never runs, which a function with a
             ; side effect in it can tell
             'and (bc-bool (bc-true? (bc-ev st <>nd) && bc-true? (bc-ev st <>>nd)))
             'or (bc-bool (bc-true? (bc-ev st <>nd) || bc-true? (bc-ev st <>>nd)))
             'len [(bn-len (bc-ev st <>nd)) 0]
             'scl [<>(bc-ev st <>nd) 0]
             'sqrt (bn-sqrt sc (bc-ev st <>nd))
             'pow (bn-pow sc (bc-ev st <>nd) (bc-ev st <>>nd))
             'bin (: a (bc-ev st <>>nd) b (bc-ev st <>>>nd) o <>nd
                     (? (o = "+") (bn-add a b)
                        (o = "-") (bn-sub a b)
                        (o = "*") (bn-mul sc a b)
                        (o = "/") (bn-div sc a b)
                        (bn-mod sc a b)))
             'cmp (: a (bc-ev st <>>nd) b (bc-ev st <>>>nd)
                     c (bn-cmp a b) o <>nd
                     (bc-bool (? (o = "<") (c < 0) (o = "<=") (c <= 0)
                                 (o = ">") (0 < c) (o = ">=") (0 <= c)
                                 (o = "==") (c = 0) !(c = 0))))
             'incr (: p (bc-place st <>>>nd)
                      old (bc-pget st p)
                      _ (bc-pset st p (bn-add old [<>>nd 0]))
                      (? <>nd (bc-pget st p) old))
             'asgn (: p (bc-place st <>>nd)
                      rhs (bc-ev st <>>>nd)
                      o <>nd
                      cur (? (o = "=") bn-zero (bc-pget st p))
                      _ (bc-pset st p
                          (? (o = "=") rhs
                             (o = "+=") (bn-add cur rhs)
                             (o = "-=") (bn-sub cur rhs)
                             (o = "*=") (bn-mul sc cur rhs)
                             (o = "/=") (bn-div sc cur rhs)
                             (o = "%=") (bn-mod sc cur rhs)
                             (bn-pow sc cur rhs)))
                      (bc-pget st p))
             'call (bc-call st <>nd <>>nd)
             'aref (bc-err "an array has no value of its own")
             (bc-err "internal: an expression shape with no evaluator")))
   ; ⚠ bc's autos are DYNAMICALLY scoped: the old values are stacked on entry and
   ; put back on the way out, which is the whole of what `auto` means. an array
   ; goes in BY VALUE, so a callee's a[] is a copy and the caller's is untouched
   (bc-copy t) (: b (tablet 0) _ (map (\ k (pin b k (peep t k bn-zero))) (keys t)) b)
   (bc-save st ns)
    (map (\ p (? (<p == 's) ['s <>p (peep (bc-vars st) <>p bn-zero)]
                 ['a <>p (bc-copy (bc-arr st <>p))]))
         ns)
   (bc-restore st sv)
    (map (\ p (? (<p == 's) (pin (bc-vars st) <>p <>>p)
                 (pin (bc-arrs st) <>p <>>p)))
         sv)
   (bc-bind st p v)
    (? (<p == 's) (pin (bc-vars st) <>p v) (pin (bc-arrs st) <>p v))
   (bc-call st nm args)
    (: fn (peep (bc-fns st) nm ())
       _ (? (bc-there? fn) 0 (bc-err ("Function " + nm + " not defined.")))
       ps <fn au <>fn body <>>fn
       _ (? (#ps = #args) 0
            (bc-err ("Function " + nm + " called with the wrong count of arguments")))
       ; the operands are counted in the CALLER's frame, before anything is stacked
       (val p a) (? (<p == 'a)
                    (: _ (? (<a == 'aref) 0 (bc-err "an array parameter wants an array"))
                       (bc-copy (bc-arr st <>a)))
                    (bc-ev st a))
       (vals m u acc) (? !(two? m) (rev acc) (vals >m >u ((val <m <u) . acc)))
       vs (vals ps args ())
       sv (bc-save st (ps + au))
       (put1 m u) (? !(two? m) 0 (: _ (bc-bind st <m <u) (put1 >m >u)))
       _ (put1 ps vs)
       _ (map (\ p (bc-bind st p (? (<p == 'a) (tablet 0) bn-zero))) au)
       ; the frame comes back even when the body DIES: nothing unwinds through a
       ; scare here, so the restore is laid on the error path by hand -- without it
       ; a divide by zero leaves the callee's parameters standing as globals
       r (trap (\ _ (bc-runs st body)) ()
               (\ a b (: _ (bc-restore st sv) (scare a b))))
       _ (bc-restore st sv)
       (? (two? r && <r == 'ret) <>r bn-zero))

   ; -- running ---------------------------------------------------------------
   ; a statement answers () to walk on, 'brk, 'cnt, or ['ret v]
   (bc-runs st ss)
    (? !(two? ss) ()
       (: r (bc-run st <ss) (? (bc-there? r) r (bc-runs st >ss))))
   (bc-run st s)
    (<s @ 'nop ()
          'expr (: v (bc-ev st <>s)
                   _ (? (<<>s == 'asgn) 0 (: _ (bc-print st v) (bc-put st 10)))
                   ())
          'say (: _ (bc-say st <>s) ())
          'print (: _ (map (\ i (? (<i == 'say) (bc-say st <>i)
                                   (bc-print st (bc-ev st i))))
                           <>s)
                    ())
          'blk (bc-runs st <>s)
          'if (? (bc-true? (bc-ev st <>s)) (bc-run st <>>s)
                 (bc-there? <>>>s) (bc-run st <>>>s)
                 ())
          'while (: (go _) (? !(bc-true? (bc-ev st <>s)) ()
                              (: r (bc-run st <>>s)
                                 (? (r == 'brk) () (two? r) r (go 0))))
                    (go 0))
          'for (: _ (? (bc-there? <>s) (bc-ev st <>s) 0)
                  (go _) (? !(bc-true? (? (bc-there? <>>s) (bc-ev st <>>s) bn-one)) ()
                            (: r (bc-run st <>>>>s)
                               (? (r == 'brk) () (two? r) r
                                  (: _ (? (bc-there? <>>>s) (bc-ev st <>>>s) 0)
                                     (go 0)))))
                  (go 0))
          'brk 'brk
          'cnt 'cnt
          'ret ['ret (? (bc-there? <>s) (bc-ev st <>s) bn-zero)]
          'quit (: _ (flush out) (uleave 0))
          (bc-err "internal: a statement shape with no runner"))

   ; -- the source walk -------------------------------------------------------
   ; one input item at a time, so a runtime error costs its own statement and the
   ; source walks on -- and so `quit` in a file is never read past
   (bc-guard st f)
    (trap f () (\ a b (? (id? a 'bc-err)
                         (: _ (flush out) _ (say err ("bc: " + b))
                            _ (put err 10) _ (flush err) 0)
                         (scare a b))))
   (bc-define st ts)
    (: r (? (bc-k? >ts 'name) >ts (bc-syn "expected a function name"))
       nm <><r
       r2 (bc-want >r "(" "expected ( after a function name")
       (bc-params (bc-nl r2) () @ [ps r3]
         (: r4 (bc-want r3 ")" "expected ) after the parameters")
            r5 (bc-want (bc-nl r4) "{" "expected { after a definition")
            (bc-autos r5 () @ [au r6]
              (: (go m acc)
                  (: m2 (bc-term m)
                     (? (bc-op? m2 "}")
                         (: _ (pin (bc-fns st) nm [ps au (rev acc)]) >m2)
                        (bc-eof? m2) (bc-syn ("unexpected end of program in " + nm))
                        (bc-stmt m2 @ [s r7] (go r7 (s . acc)) ())))
                 (go r6 ()))
              ()))
         ()))
   (bc-top st m)
    (: m0 (bc-term m)
       (? (!(two? m0) || bc-eof? m0) ()
          (bc-kwd? m0 "define") (bc-define st m0)
          (bc-stmt m0 @ [s r] (: _ (bc-guard st (\ _ (bc-run st s))) r) ())))
   ; a SYNTAX error stops this source where a runtime one does not: nothing past
   ; the bad token has a meaning to walk on to
   (bc-source st src)
    (trap (\ _ (: ts (bc-lex src)
                  (go m) (: r (bc-top st m) (? (two? r) (go r) 0))
                  _ (go ts)
                  (flush out)))
          ()
          (\ a b (? (id? a 'bc-syn)
                    (: _ (flush out) _ (say err ("bc: " + b)) _ (put err 10)
                       _ (flush err) 0)
                    (scare a b))))
   ; is the buffer still mid-item? the LEXER answers it: an unclosed brace, or a
   ; string or comment that ran off the end. this is what makes stdin a prompt
   ; rather than a slurp. asked once per line, over the OPEN text only -- a closed
   ; line costs one line, and only a define pays for its own length twice
   (bc-open? t)
    (: n #t
       (? ((0 < n) && (t (n - 1) = 10) && (1 < n) && (t (n - 2) = 92)) 1
          (trap (\ _ (: ts (bc-lex t)
                        (go m d) (? !(two? m) (0 < d)
                                    (bc-op? m "{") (go >m (d + 1))
                                    (bc-op? m "}") (go >m (d - 1))
                                    (go >m d))
                        (go ts 0)))
                ()
                (\ a b 1))))
   (bc-stdin st)
    (: rd (urd in)
       (go acc)
        (: l (urd-line rd)
           (? !(two? l) (? (0 < #acc) (bc-source st acc) 0)
              (: t (acc + <l + "\n")
                 (? (bc-open? t) (go t) (: _ (bc-source st t) (go ""))))))
       (go ""))

   ; -- the math library, in bc's own language --------------------------------
   ; ⚠ AGREEMENT WITH GNU STOPS AT SCALE 10. above it these are byte-identical over
   ; every argument swept; below it the two part on 29 of 735 sampled calls, each one
   ; a place where GNU's own series ran out of guard digits -- ours is the correctly
   ; truncated value every time, asked of GNU itself at scale 40. the gate takes the
   ; scales where both are right
   bc-mathlib "
scale = 20
define e(x) {
  auto s, f, i, a, d, t, v, m, n
  s = scale; m = 0; f = 0
  if (x < 0) { m = 1; x = -x }
  scale = 0; n = s + 10 + .44 * x
  scale = n
  while (x > 1) { x /= 2; f += 1 }
  v = 1 + x; a = x; d = 1
  for (i = 2; 1; i += 1) {
    a *= x; d *= i; t = a / d
    if (t == 0) break
    v += t
  }
  while (f > 0) { v *= v; f -= 1 }
  scale = s
  if (m) return (1 / v)
  return (v / 1)
}
define l(x) {
  auto s, f, u, m, v, t, i
  s = scale; scale = s + 10; f = 1
  while (x > 2) { x = sqrt(x); f *= 2 }
  while (x < .5) { x = sqrt(x); f *= 2 }
  u = (x - 1) / (x + 1); m = u * u; v = u; t = u
  for (i = 3; 1; i += 2) {
    t *= m; u = t / i
    if (u == 0) break
    v += u
  }
  scale = s
  return (2 * f * v / 1)
}
define a(x) {
  auto s, f, u, m, v, t, i
  if (x < 0) return (-a(-x))
  s = scale; scale = s + 10; f = 1
  while (x > .25) { x = x / (1 + sqrt(1 + x * x)); f *= 2 }
  m = -x * x; t = x; v = x
  for (i = 3; 1; i += 2) {
    t *= m; u = t / i
    if (u == 0) break
    v += u
  }
  scale = s
  return (f * v / 1)
}
define s(x) {
  auto s, q, n, v, t, m, i
  s = scale; scale = s + 10
  q = 8 * a(1)
  scale = 0; n = x / q; scale = s + 10
  x = x - n * q
  m = -x * x; t = x; v = x
  for (i = 3; 1; i += 2) {
    t = t * m / (i * (i - 1))
    if (t == 0) break
    v += t
  }
  scale = s
  return (v / 1)
}
define c(x) {
  auto s, v
  s = scale; scale = s + 10
  v = s(x + 2 * a(1))
  scale = s
  return (v / 1)
}
define j(n, x) {
  auto s, k, t, v, m, f, i
  s = scale
  scale = 0
  n = n / 1
  f = 1
  if (n < 0) { n = -n; if (n % 2 == 1) f = -1 }
  scale = s + 10
  t = 1
  for (i = 1; i <= n; i += 1) { t = t * x / (2 * i) }
  m = -x * x / 4; v = t; k = 1
  while (t != 0) {
    t = t * m / (k * (k + n))
    v += t; k += 1
  }
  scale = s
  return (f * v / 1)
}
"

   ; -- the applet ------------------------------------------------------------
   (bc-state _)
    (: st (tablet 0)
       _ (pin st 'v (tablet 0)) _ (pin st 'a (tablet 0)) _ (pin st 'f (tablet 0))
       _ (pin st 'scale 0) _ (pin st 'ibase 10) _ (pin st 'obase 10)
       _ (pin st 'last bn-zero) _ (pin st 'col 0)
       st)
   (bc-usage _)
    (: _ (say err "usage: bc [-l] [-q] [FILE..]
  -l  the math library (e l a s c j), and scale = 20
  -q  accepted: nothing is printed at start with or without it")
       (put err 10))
   (bc-main as)
    (: st (bc-state 0)
       (opts m fs bad ml)
        (? !(two? m) [fs bad ml]
           (: a <m
              (? (a = "--") [((rev >m) + fs) bad ml]
                 (a = "-l" || a = "--mathlib") (opts >m fs bad 1)
                 (a = "-q" || a = "--quiet") (opts >m fs bad ml)
                 (a = "-lq" || a = "-ql") (opts >m fs bad 1)
                 (a = "-h" || a = "--help") (: _ (bc-usage 0) (uleave 0))
                 ((1 < #a) && (a 0 = 45)) (opts >m fs a ml)
                 (opts >m (a . fs) bad ml))))
       r (opts as () () 0)
       fs (rev <r) bad <>r ml <>>r
       _ (? bad (: _ (say err ("bc: unknown option " + bad)) _ (put err 10)
                   _ (bc-usage 0) (uleave 1))
            0)
       _ (? ml (bc-source st bc-mathlib) 0)
       ; the files in turn, then stdin -- POSIX's own order, and a missing one
       ; costs the status without stopping the walk
       ok (foldl (\ a f (: t (uread f)
                           (? (two? t) (: _ (bc-source st <>t) a)
                              (: _ (say err ("bc: cannot open " + f)) _ (put err 10) 0))))
                 1 fs)
       _ (bc-stdin st)
       _ (flush out)
       (? ok 0 1)))
) ; module 'kore
