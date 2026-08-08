; post -- the third egg stage. prel and ev are sat TWICE (ev compiles itself);
; post is sat ONCE, on the hatchling, after that and before the mop. it is the
; one seat that sees both the finished compiler and the runtime-internal noms
; (peek seek atype, and lamsrc/nifnom below) the mop is about to take off the book -- a
; reference compiled here folds to a value and survives, the way ev's own
; `feels` does. so: love that needs to read the heap raw belongs here.
;
; what lives here is THE PRINTER. it is the reader's mirror image: p0 is a floor
; in C and p1 the real thing in love, and the printer splits the same way -- the
; floor is love.c's facex (the scare face, which runs when the VM has stopped and
; there is nobody left to call), the grammar is all below. `print` `dot` `show`
; `putx` and the `.` sigil are pinned at the foot; prel leaves those four holes
; for us, because a nif folded into prel's own compile could never be replaced.

(: px (:
  ; ⚠ EVERY EAGER BINDING SITS IN THIS OUTER `:`. where boxfix engages (any forward
  ; or self reference -- the printer below is one), a sibling bound to an UNFOLDABLE
  ; CALL is boxed too and its cell never filled, so every read of it raises `missing`:
  ;   (: f (: (a o) (f o) c (tablet 0) (\ o c)))   -> ;; missing c
  cur (peek 0 (+)) ret0 (peek 1 cap) unc (peek 0 (+ 1))
  ; ⚠ THE FLOAT WIDTH IS THE SEAT'S, never this file's: love.c gives a 32-bit seat
  ; (wasm, mps2, teensy41, playdate, rp2040) binary32 for ai_flo_t. so ASK the
  ; arithmetic instead of spelling binary64's numbers -- a hard-coded 2^53 sends
  ; norm's doubling loop past every float32 there is and it never returns.
  p53 (: (go m) (? (m = (1.0 + m)) m (go (m * 2.0))) (go 1.0))   ; where +1 is lost = 2^mantissa
  p52 (p53 / 2.0)
  emin (: (go v e) (: h (v / 2.0) (? (h = 0) e (go h (e - 1)))) (go 1.0 0))   ; the denormal floor
  inf (1.0 / 0.0)
 (:

  ; --- integers: the digits of a non-negative charm/sun/big, most significant first
  (dch n a) (: q (n // 10) d (48 + (n - (q * 10))) (? (0 < q) (dch q (link d a)) (link d a)))
  (dstr n) (string (dch n ()))
  (ndig n) (: q (n // 10) (? (0 < q) (1 + (ndig q)) 1))          ; the count, without building the list
  (pnum o n) (? (n < 0) (: _ (put o 45) (say o (dstr (0 - n)))) (say o (dstr n)))

  ; --- text ---
  (hexd d) (? (d < 10) (48 + d) (87 + d))
  (esc o c) (? (|| (c = 92) (c = 34)) (: _ (put o 92) (put o c))
               (c = 10) (say o "\\n")   (c = 9)  (say o "\\t")
               (c = 13) (say o "\\r")   (c = 27) (say o "\\e")
               (c = 0)  (say o "\\0")
               (c < 32) (: _ (say o "\\x") _ (put o (hexd (c // 16))) (put o (hexd (c - (16 * (c // 16))))))
               (put o c))
  (pstr o s) (: n (tally s)
                (go i) (? (i < n) (: _ (esc o (peep s i 0)) (go (1 + i))) 0)
                _ (put o 34) _ (go 0) (put o 34))

  ; --- gems: the shortest roundtrip, exact steele & white. C had to carry digit
  ; arrays and its own x2/x5 carry walks; here 4m and the midpoints 4m±2 are
  ; bignums at one shared scale and `//` does the rest.
  ; v = m*2^e exactly, m an integer, normalized into [p52,p53) -- but NEVER past the
  ; denormal floor, where the true spacing stops halving and one more step would hand
  ; the search an interval far too narrow.
  (norm m e) (? (p53 <= m) (norm (m / 2.0) (1 + e))
                (&& (m < p52) (emin < e)) (norm (m * 2.0) (e - 1))
                (link (int m) e))
  ; inside the rounding interval? a midpoint counts only for an even mantissa --
  ; round-to-nearest-even is what reads it back.
  (fits c lo hi evn) (&& (? (lo < c) 1 (&& evn (c = lo)))
                        (? (c < hi) 1 (&& evn (c = hi))))
  ; the fewest digits whose rounded form sits inside: pk is the place being cut,
  ; walking DOWN by ten a step, and at each one try the truncation and the bump,
  ; the nearer first (a display nicety). at pk = 1 it terminates, dv itself being
  ; inside by construction.
  (pick dv lo hi evn pk) (? (pk < 1) dv
    (: t ((dv // pk) * pk)
       up (&& (1 < pk) (5 <= ((dv // (pk // 10)) - (10 * (dv // pk)))))
       c1 (? up (t + pk) t) c2 (? up t (t + pk))
       (? (fits c1 lo hi evn) c1 (fits c2 lo hi evn) c2 (pick dv lo hi evn (pk // 10)))))
  ; strip trailing zeros, keeping one digit
  (strip n) (: q (n // 10) (? (&& (0 < q) ((n - (q * 10)) = 0)) (strip q) n))
  (zeros o n) (? (0 < n) (: _ (put o 48) (zeros o (n - 1))) 0)
  (pgem2 o a) (: me (norm a 0) m (cap me) e2 (cup me) sh (e2 - 2)
    up (? (sh < 0) (** 5 (0 - sh)) (** 2 sh))   ; one shared scale: 2^sh, or 5^-sh with the point moved
    flo (&& (m = p52) (emin < e2))                ; at a binade floor the gap below is already half
    dv ((4 * m) * up)
    dlo (((4 * m) - (? flo 1 2)) * up)
    dhi (((4 * m) + 2) * up)
    evn ((m - (2 * (m // 2))) = 0)
    dc (pick dv dlo dhi evn (** 10 ((ndig dv) - 1)))
    lead (ndig dc)
    ss (dstr (strip dc)) sig (tally ss)
    dxp (lead + (? (sh < 0) sh 0))
    (? (|| (1e16 <= a) (a < 1e-4))
       (: e (dxp - 1)
          _ (put o (peep ss 0 0))
          _ (? (1 < sig) (: _ (put o 46) (say o (snip ss 1 sig))) 0)
          _ (put o 101)
          _ (? (e < 0) (put o 45) 0)
          (say o (dstr (? (e < 0) (0 - e) e))))
       (0 < dxp)                                  ; 1 <= v < 1e16: the point sits inside or after
       (: (go i) (? (i < dxp) (: _ (put o (? (i < sig) (peep ss i 0) 48)) (go (1 + i))) 0)
          _ (go 0) _ (put o 46)
          (? (dxp < sig) (say o (snip ss dxp sig)) (put o 48)))   ; integral: force "X.0", never a fixnum face
       (: _ (say o "0.") _ (zeros o (0 - dxp)) (say o ss))))
  (pgem o v) (? !(v <= v) (say o "ieee-nan")
     ; the sign is the BIT, not v < 0: -0.0 < 0 is FALSE, and a comparison there
     ; printed "0.0" -- the one value the roundtrip law lost.
     (: _ (? (|| (v < 0) (&& (v = 0) ((1.0 / v) < 0))) (put o 45) 0) a (abs v)
        (? (a = inf) (say o "ieee-inf") (a = 0) (say o "0.0") (pgem2 o a))))
  (ptwin o x) (: _ (say o "~(") _ (pgem o (re x)) _ (put o 32) _ (pgem o (im x)) (put o 41))

  ; --- chains. a one-operand `\` is quote, and a `(mono (run datum))` is a GLUED
  ; monadic -- print run+datum back the way opfix fused them.
  (plist o x sn) (: (go l) (: _ (px o (cap l) sn)
                              (? (two? (cup l)) (: _ (put o 32) (go (cup l))) 0))
                    _ (put o 40) _ (go x) (put o 41))
  (pchain o x sn) (: h (cap x) t (cup x) one (&& (two? t) !(two? (cup t)))
    (? (&& (id? h '\) one) (: _ (put o 39) (px o (cap t) sn))
       (&& (id? h 'mono) (&& one (: m (cap t) (&& (two? m) (&& (two? (cup m)) !(two? (cuup m)))))))
         (: m (cap t) _ (px o (cap m) sn) (px o (caup m) sn))
       (plist o x sn)))

  ; --- trays. rank 1 is @(…); past that (array '(shape) elem…), since @ has no
  ; shape spelling. an object tray quotes what would otherwise re-evaluate.
  ; a tray is read by COORDINATE, so walk the flat index back into one, row-major
  (coord sh i) (? !(two? (cup sh)) (link i ())
                  (: r (foldl (*) 1 (cup sh)) q (i // r) (link q (coord (cup sh) (i - (q * r))))))
  (ptray o v sn) (: ty (atype v) sh (shape v) n (foldl (*) 1 sh)
    (pel i) (: e (peep v (coord sh i) 0)
               _ (? (&& (ty = 3) (|| (nom? e) (two? e))) (put o 39) 0) (px o e sn))
    (go i f) (? (i < n) (: _ (? f 0 (put o 32)) _ (pel i) (go (1 + i) 0)) 0)
    (gs l f) (? (two? l) (: _ (? f 0 (put o 32)) _ (pnum o (cap l)) (gs (cup l) 0)) 0)
    (? ((rank v) = 1) (: _ (say o "@(") _ (go 0 1) (put o 41))
       (: _ (say o "(array '(") _ (gs sh 1) _ (put o 41)
          (ge i) (? (i < n) (: _ (put o 32) _ (pel i) (ge (1 + i))) 0)
          _ (ge 0) (put o 41))))

  ; --- maps print #(k v …), the empty one #(); both read straight back. a map is
  ; mutable and can hold itself, so the path is carried and matched by IDENTITY.
  (onpath? x l) (? !(two? l) 0 (id? x (cap l)) 1 (onpath? x (cup l)))
  (pmap o m sn) (? (onpath? m sn) (say o "<cycle>")
    (: ks (keys m) s2 (link m sn)
       (go l f) (? (two? l) (: _ (? f 0 (put o 32))
                                _ (px o (cap l) s2) _ (put o 32) _ (px o (peep m (cap l) ()) s2)
                                (go (cup l) 0)) 0)
       (? !(two? ks) (say o "#()") (: _ (say o "#(") _ (go ks 1) (put o 41)))))

  ; --- coins show as (name payload), reparsable when the die's name is a bound ctor
  (pcoin o x sn) (: nm (peep (die-of x) 0 ())
    _ (put o 40) _ (? (nil? nm) (say o "coin") (px o nm sn))
    _ (put o 32) _ (px o (load x) sn) (put o 41))

  ; --- closures. a partial's every unc cell holds its arg at [1] and the link at
  ; [2], so the base value sits at link-2.
  (partp v) (|| (id? unc (peek 0 v)) (&& (id? cur (peek 0 v)) (id? unc (peek 2 v))))
  (unc0 v) (? (id? cur (peek 0 v)) (seek 2 v) v)
  (nargs v) (: (go u n) (: l (peek 2 u) (? (id? unc (peek 0 l)) (go l (1 + n)) n)) (go (unc0 v) 1))
  (fbase v) (: (go u) (: l (peek 2 u) (? (id? unc (peek 0 l)) (go l) (seek -2 l))) (go (unc0 v)))
  (farg v i n) (: (go u w) (? (0 < w) (go (peek 2 u) (w - 1)) (peek 1 u)) (go (unc0 v) ((n - 1) - i)))
  ; --- a lambda's source, de Bruijn renamed: a \-bound variable prints d<level>,
  ; so a-equivalent lambdas print alike and inspect agrees with `=`. free, global
  ; and :-bound names keep their spelling; quoted data is shared verbatim.
  (dnom d) (name (+ "d" (dstr d)))
  (lam? x) (&& (two? x) (&& (id? (cap x) '\) (&& (two? (cup x)) (two? (cuup x)))))
  (qot? x) (&& (two? x) (&& (id? (cap x) '\) (&& (two? (cup x)) !(two? (cuup x)))))
  (canon x sc d) (? !(two? x) (? (nom? x) (: r (assoc x sc) (? r (cup r) x)) x)
                    (qot? x) x
                    (lam? x) (link '\ (cops (cup x) sc d))
                    (link (canon (cap x) sc d) (canon (cup x) sc d)))
  (cops l sc d) (? !(two? (cup l)) (link (canon (cap l) sc d) ())     ; the last operand is the body
                   (: nm (dnom d) (link nm (cops (cup l) (link (link (cap l) nm) sc) (1 + d)))))

  (pfn o v sn) (? (partp v)
    (: n (nargs v)
       (go i) (? (i < n) (: _ (put o 32) _ (px o (farg v i n) sn) (go (1 + i))) 0)
       _ (put o 40) _ (pfn o (fbase v) sn) _ (go 0) (put o 41))
    (: nm (nifnom v)
       (? (string? nm) (say o nm)
          (: s (lamsrc v)
             (? (two? s) (px o (canon s 0 0) sn)
                (: _ (put o 92) (pnum o (abs (dig v)))))))))   ; opaque: a distinguisher, nothing more -- C spelled the ADDRESS here, this is the digest, which at least survives a collection

  (px o x sn) (? (charm? x) (pnum o x)
                (coin? x) (pcoin o x sn)
                (tablet? x) (pmap o x sn)
                (lit? x) (pfn o x sn)
                (id? x ()) (say o "()")
                (two? x) (pchain o x sn)
                (string? x) (pstr o x)
                (nom? x) (? (mint? x) (: _ (say o "(mint ") _ (pnum o (dig x)) (put o 41))
                              (say o (string x)))
                (gem? x) (pgem o x)
                (tray? x) (ptray o x sn)
                (twin? x) (ptwin o x)
                (|| (sun? x) (big? x)) (pnum o x)
                o)
  px)))

; the surface. `dot` takes a string or cask VERBATIM -- the `.` sigil's raw face --
; and everything else in external form; it answers its operand, so it drops into
; any sequence. the monadics table is patched too: opfix folded prel's `.` row to a
; VALUE, so a fresh binding alone would never reach a later file's `.x`.
(: (print o x) (: _ (px o x ()) x)
   (show x) (: o (jug 0) _ (px o x ()) (slurp o))
   (dot x) (: _ (? (|| (string? x) (&& (hot? x) !(port? x))) (say out x) (px out x ())) _ (flush out) x))
(: putx (print out)
   . dot
   _ (pin monadics '. dot))
