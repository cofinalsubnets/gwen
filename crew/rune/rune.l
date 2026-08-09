; rune -- the symbolic algebra engine (a wee CAS): exact multivariate
; polynomials over the integers, plus univariate arithmetic in Q[x]/(m)
; riding the q coin. pure love, no nifs; the gate is test/host/rune.l,
; whose corpus verifies the 2026 jacobian-conjecture disproof whole
; (det J symbolically, the collision certificates in exact arithmetic).
;
; a POLY is a list of monomials, each (c e0 e1 .. ) -- a coefficient (a
; STAR: an integer or a ratio coin, so the ring is Q[x..] with Z[x..]
; sitting inside it structurally) then one exponent per variable -- keys
; lex-descending, no zero coefficients, all polys in one expression
; sharing an arity. the zero polynomial is (): the empty sum IS nothing.
; cnorm keeps coefficients CANONICAL: an integer-valued coin ((q n 1))
; demotes to its integer numerator, so halves that sum whole land an
; integer, = stays structural across Z and Q, and the zero-drop is the
; plain (c = 0) -- a value-zero coin demotes to the fixnum 0 first.
; (! can never test a coefficient: a red one nets nothing.)
;
; a K-ELEMENT of Q[x]/(m), deg m = d, is an ascending list of d q coins.
; the field is given not by m but by XD -- x^d rewritten as an element --
; so reduction folds top powers down with no division anywhere.

; NO BRACKETS HERE: the runtime supplies the layer. a consumer says
; (use 'rune) -- the loader finds this file (lib/rune.l on the host, the
; baked source a device registered) and loads it as one fresh layer, leave
; registers it, the splice serves the bare names; or reaches one member
; through the accessor, (from 'rune 'deriv).

; --- the polynomial ring ---------------------------------------------------
(: (mco m) <m
   (mkey m) >m
   (cnorm c) (coin? c ? (: p (load c) (1 = <>p ? <p c)) c)
   (keylt a b) (? (0 = tally a) 0
                  (<a < <b) 1
                  (<b < <a) 0
                  (keylt >a >b))
   (padd p1 p2) (? (0 = tally p1) p2
                   (0 = tally p2) p1
                   (: mp <p1 mq <p2
                      (? (mkey mp = mkey mq)
                         (: c (cnorm (mco mp + mco mq)) r (padd >p1 >p2)
                            (c = 0 ? r ((c >< mkey mp) >< r)))
                         (keylt (mkey mq) (mkey mp))
                         (mp >< padd >p1 p2)
                         (mq >< padd p1 >p2))))
   (pneg p) (map (\ mm ((0 - mco mm) >< mkey mm)) p)
   (psub p1 p2) (padd p1 (pneg p2))
   (zadd a b) (0 = tally a ? () ((<a + <b) >< zadd >a >b))
   (mmul a b) (cnorm (mco a * mco b) >< zadd (mkey a) (mkey b))
   (mrow a p2) (0 = tally p2 ? () (mmul a <p2 >< mrow a >p2))
   (pmul p1 p2) (0 = tally p1 ? () (padd (mrow <p1 p2) (pmul >p1 p2)))
   (nth l n) (n = 0 ? <l (nth >l (n - 1)))
   (setn l n x) (n = 0 ? (x >< >l) (<l >< setn >l (n - 1) x))
   (deln l n) (n = 0 ? >l (<l >< deln >l (n - 1)))
   (zeros n) (n = 0 ? () (0 >< zeros (n - 1)))
   (pconst n c) (: k (cnorm c) (k = 0 ? () `((k >< zeros n))))
   (pvar n k) `((1 >< setn (zeros n) k 1)))

; --- the derivative and the determinant ------------------------------------
; pd differentiates monomial by monomial (padd of singletons keeps the
; canonical order); pdet expands along the first row, minors by deln.
(: (pd p v) (? (0 = tally p) ()
               (: mm <p e (nth (mkey mm) v) r (pd >p v)
                  (e = 0 ? r
                     (padd `((cnorm (mco mm * e) >< setn (mkey mm) v (e - 1))) r))))
   (pdet rows) (1 = tally rows ? <(<rows)
                  (: (go r0 j sg)
                     (0 = tally r0 ? ()
                        (: t (pmul <r0 (pdet (map (\ r (deln r j)) >rows)))
                           (padd (sg = 1 ? t (pneg t)) (go >r0 (j + 1) (0 - sg)))))
                     (go <rows 0 1))))

; --- gcd & squarefree factorization: euclid in Q[x], answers monic ----------
; UNIVARIATE (arity-1 keys): pmod/pquot are long division's remainder and
; quotient -- the lead quotient monomial (pquo) rides pmul/psub, so
; cancellation is the ring's own, exact -- and pgcd euclids until the zero
; poly, then normalizes monic (unique in Q[x]; units divide out). cinv is
; the exact reciprocal of a coefficient star: a coin through its die, an
; integer through the q ctor. psff is yun's algorithm: the ascending list
; (a1 a2 ..) of monic squarefree factors, monic p = a1^1 * a2^2 * .. --
; empty slots hold the 1 poly, a constant has no factors. both faces load:
; raw monomial lists and poly coins alike; answers are raw.
(: (cinv c) (coin? c ? (-1 c) (q 1 c))
   (pdeg p) <(mkey <p)
   (pmonic p) (? (0 = tally p) ()
                 (: v (cinv (mco <p))
                    (map (\ m (cnorm (v * mco m) >< mkey m)) p)))
   (pquo a b) `((cnorm (mco <a * cinv (mco <b)) >< `((pdeg a - pdeg b))))
   (pmod a b) (? (0 = tally a) ()
                 (pdeg a < pdeg b) a
                 (pmod (psub a (pmul (pquo a b) b)) b))
   (pquot a b) (? (0 = tally a) ()
                  (pdeg a < pdeg b) ()
                  (: t (pquo a b)
                     (padd t (pquot (psub a (pmul t b)) b))))
   (pgcd a b) (: x (load a) y (load b)
                 (? (0 = tally y) (pmonic x) (pgcd y (pmod x y))))
   (psff p) (: f (pmonic (load p))
               (go w y) (? (0 = pdeg w) ()
                           (: z (psub y (pd w 0))
                              a (pgcd w z)
                              (a >< go (pquot w a) (pquot z a))))
               (? ((0 = tally f) || (0 = pdeg f)) ()
                  (: g (pgcd f (pd f 0))
                     (go (pquot f g) (pquot (pd f 0) g))))))

; --- multivariate gcd: Q[x..] as (Q[y..])[x], the primitive PRS -------------
; mgcd recurses on ARITY: view the poly univariate in the FIRST variable
; (lex-descending keys make the lead monomial's e1 the x-degree free), take
; the CONTENT -- the recursive mgcd fold over the x-coefficients (mcof drops
; e1; one variable fewer each level, pgcd the arity-1 floor) -- split it
; off (mdiv, exact lex division), and euclid the primitive halves by
; PSEUDO-remainder (mprem cross-multiplies the lead coefficients, so the
; top term cancels exactly with no coefficient division; each remainder
; re-primitivized). a degree-0 remainder means coprime in x -- the 1 poly.
; the answer is content-gcd times primitive-gcd, lex-lead normalized to 1
; by pmonic. both faces load, answers raw, as with pgcd.
(: (ksub a b) (? (0 = tally a) () ((<a - <b) >< ksub >a >b))
   (mcof p k) (map (\ m (mco m >< >(mkey m)))
                   (filter (\ m (k = <(mkey m))) p))
   (mlift c k) (map (\ m (mco m >< (k >< mkey m))) c)
   (me1s p) (? (0 = tally p) ()
               (: e <(mkey <p)
                  (e >< me1s (filter (\ m !(e = <(mkey m))) >p))))
   (mdiv f d) (? (0 = tally f) ()
                 (: t `((link (cnorm (mco <f * cinv (mco <d)))
                              (ksub (mkey <f) (mkey <d))))
                    (padd t (mdiv (psub f (pmul t d)) d))))
   (mprem f g) (? (0 = tally f) ()
                  (pdeg f < pdeg g) f
                  (: lg (mlift (mcof g (pdeg g)) 0)
                     lf (mlift (mcof f (pdeg f)) (pdeg f - pdeg g))
                     (mprem (psub (pmul lg f) (pmul lf g)) g)))
   (mcont p) (: es (me1s p)
                (go r acc) (? (0 = tally r) acc
                              (go >r (mgcd acc (mcof p <r))))
                (go >es (mcof p <es)))
   (mpp p) (mdiv p (mlift (mcont p) 0))
   (mgcd a b) (: x (load a) y (load b)
                 (go f g) (: r (mprem f g)
                             (? (0 = tally r) g
                                (0 = pdeg r) (pconst (tally (mkey <g)) 1)
                                (go g (mpp r))))
                 (? (0 = tally x) (pmonic y)
                    (0 = tally y) (pmonic x)
                    (1 = tally (mkey <x)) (pgcd x y)
                    (: c (mgcd (mcont x) (mcont y))
                       (pmonic (pmul (mlift c 0) (go (mpp x) (mpp y))))))))

; --- rational roots: factorization's first rung ------------------------------
; the rational root theorem over the squarefree split: pint clears the
; denominators (roots are scale-blind), candidates are +-n/d for n a divisor
; of the constant and d of the lead, peval tests each EXACTLY (powers by
; application, the sum cnormed), a hit divides out through pquot and the
; part is squarefree so every hit is simple. proots answers the rational
; roots WITH multiplicity (psff carries it), ascending; pfac the factor
; list -- a leading constant when the lead is not 1, then monic factors,
; each (x - r) once per multiplicity, rootless remainders kept whole --
; whose pmul-fold is p EXACTLY. a rootless remainder of degree 2 or 3 is
; thereby CERTIFIED irreducible over Q (a factor would be linear); degree
; 4 up it just has no rational roots, honestly.
(: (pden c) (coin? c ? <>(load c) 1)
   (pint f) (: cs (filter (\ m (coin? (mco m))) f)
               (? (0 = tally cs) f
                  (pint (pmul `((pden (mco <cs) >< '(0))) f))))
   (divs n) (: m ((n < 0) ? (0 - n) n)
               (go i acc) (? ((i * i) > m) acc
                             (0 = (% m i))
                             (go (i + 1) (i >< (cnorm (q m i) >< acc)))
                             (go (i + 1) acc))
               (go 1 ()))
   (flat ls) (? (0 = tally ls) () (<ls + flat >ls))
   (peval f r) (? (0 = tally f) 0
                  (cnorm ((mco <f * ((<(mkey <f)) r)) + peval >f r)))
   (plast f) (? (0 = tally >f) <f (plast >f))
   (plin r) ((r = 0) ? `((1 >< '(1)))
             `((1 >< '(1)) (cnorm (0 - r) >< '(0))))
   (rroots f) (: g (pint f)
                 (? ((0 = tally g) || (0 = pdeg g)) ()
                    (0 < <(mkey (plast g)))
                    (0 >< rroots (pquot g `((1 >< '(1)))))
                    (: cds (: ns (divs (mco (plast g)))
                              ds (divs (mco <g))
                              rs (flat (map (\ nn (map (\ dd (cnorm (q nn dd))) ds)) ns))
                              (rs + map (\ r (cnorm (0 - r))) rs))
                       (go cs h acc)
                       (? (0 = tally cs) acc
                          ((0 < pdeg h) && (0 = peval h <cs))
                          (go >cs (pquot h (plin <cs)) (<cs >< acc))
                          (go >cs h acc))
                       (sort (go cds g ())))))
   (proots p) (: fs (psff (load p))
                 (go i r) (? (0 = tally r) ()
                             (flat (map (\ x (i (link x) ())) (rroots <r))
                              + go (i + 1) >r))
                 (sort (go 1 fs))))

; --- zassenhaus: factorization whole -----------------------------------------
; the summit of the ladder: a rootless remainder of degree 4 up factors into
; irreducibles the classical way. the monic REDUCTION does the heavy
; simplifying -- F(y) = L^(n-1) f(y/L) is monic over Z, its factors map back
; by y -> Lx and pmonic -- so every stage below is monic: pick an odd prime
; p keeping F squarefree mod p (monic F makes every p coprime to the lead),
; BERLEKAMP factors F mod p (dense ascending integer vectors mod m -- the
; fv kit; x^(ip) rows, the nullspace of Q^T - I by row reduction, r = the
; factor count, gcd splitting with v - s), linear HENSEL lifts the factors
; pairwise up a tree to p^K > 2B (B a generous mignotte bound; bignums make
; overshoot free), and RECOMBINATION multiplies subsets mod p^K, centers
; symmetrically, and tests true division over Z -- a hit divides out and the
; search restarts; what remains is irreducible. pfac (rehomed here) drives:
; constant, rational-root linears, and now every remainder split whole.
(: (zmod m x) (: r (% x m) ((r < 0) ? (r + m) r))
   (zcen m x) (: r (zmod m x) ((m < (r + r)) ? (r - m) r))
   (vtrim v) (? (0 = tally v) ()
                (: r (vtrim >v)
                   (? ((0 = tally r) && (0 = <v)) () (<v >< r))))
   (vpadn n v) (v + (((n - tally v) (link 0)) ()))
   (fvshift k v) (k (link 0) v)
   (fvadd m a b) (vtrim (: (go a b) (? (0 = tally a) b
                                       (0 = tally b) a
                                       (zmod m (<a + <b) >< go >a >b))
                           (go a b)))
   (fvneg m a) (map (\ x (zmod m (0 - x))) a)
   (fvsub m a b) (fvadd m a (fvneg m b))
   (fvscale m c v) (vtrim (map (\ x (zmod m (c * x))) v))
   (fvmul m a b) (? (0 = tally a) ()
                    (fvadd m (fvscale m <a b) (fvshift 1 (fvmul m >a b))))
   (fvprod m l) (? (1 = tally l) <l (fvmul m <l (fvprod m >l)))
   (fvlead v) (? (0 = tally >v) <v (fvlead >v))
   (fvdeg v) (tally v - 1)
   (finv m c) (zmod m ((m - 2) (zmod m c)))
   (fvdm m a b) (: (go u qq)
                   (? ((0 = tally u) || (fvdeg u < fvdeg b)) `(qq u)
                      (: c (zmod m (fvlead u * finv m (fvlead b)))
                         k (fvdeg u - fvdeg b)
                         (go (fvsub m u (fvscale m c (fvshift k b)))
                             (fvadd m qq (fvshift k `(c))))))
                   (go a ()))
   (fvquo m a b) <(fvdm m a b)
   (fvrem m a b) <>(fvdm m a b)
   (fvgcd m a b) (? (0 = tally b) (fvscale m (finv m (fvlead a)) a)
                    (fvgcd m b (fvrem m a b)))
   (fvegcd m a b) (? (0 = tally b)
                     (: c (finv m (fvlead a)) `((fvscale m c a) `(c) ()))
                     (: dm (fvdm m a b)
                        e3 (fvegcd m b <>dm)
                        `(<e3 <>(>e3)
                          (fvsub m <>e3 (fvmul m <dm <>(>e3))))))
   (fder m v) (vtrim (: (go l i) (? (0 = tally l) ()
                                    (zmod m (<l * i) >< go >l (i + 1)))
                        (go >v 1)))
   (ivadd a b) (? (0 = tally a) b (0 = tally b) a
                  ((<a + <b) >< ivadd >a >b))
   (ivneg a) (map (\ x (0 - x)) a)
   (ivsub a b) (vtrim (ivadd a (ivneg b)))
   (ivscale c v) (map (\ x (c * x)) v)
   (ivmul a b) (? (0 = tally a) ()
                  (ivadd (ivscale <a b) (fvshift 1 (ivmul >a b))))
   (rng a b) (? (a > b) () (a >< rng (a + 1) b))
   (lat l i) (? (i = 0) <l (lat >l (i - 1)))
   (tovec f) (: n (pdeg f)
                (co i) (: ms (filter (\ m2 (i = <(mkey m2))) f)
                          ((0 = tally ms) ? 0 (mco <ms)))
                (map co (rng 0 n)))
   (topoly v) (: (go l i acc)
                 (? (0 = tally l) acc
                    (go >l (i + 1) ((0 = <l) ? acc ((<l >< `(i)) >< acc))))
                 (go v 0 ()))
   (mflip rs) (? (0 = tally <rs) ()
                 (map (\ r <r) rs >< mflip (map (\ r >r) rs)))
   (msubI m rs) (: (dec r i) (: (go l k)
                                (? (0 = tally l) ()
                                   (link ((k = i) ? (zmod m (<l - 1)) <l)
                                         (go >l (k + 1))))
                                (go r 0))
                   (go2 l i) (? (0 = tally l) ()
                                (dec <l i >< go2 >l (i + 1)))
                   (go2 rs 0))
   (rref m n rows) (: (rowsc c r) (map (\ x (zmod m (c * x))) r)
                      (rowsub a b) (: (go a b) (? (0 = tally a) ()
                                                  (zmod m (<a - <b) >< go >a >b))
                                      (go a b))
                      (go col rs done pivs)
                      (? (col = n) `(done pivs)
                         (: hits (filter (\ r !(0 = lat r col)) rs)
                            (? (0 = tally hits) (go (col + 1) rs done pivs)
                               (: pv <hits
                                  rest (: (d1 l) (? (0 = tally l) ()
                                                    ((<l = pv) ? >l (<l >< d1 >l)))
                                          (d1 rs))
                                  pvn (rowsc (finv m (lat pv col)) pv)
                                  (relim r) (: c (lat r col)
                                               ((0 = c) ? r (rowsub r (rowsc c pvn))))
                                  (go (col + 1) (map relim rest)
                                      (map relim done + `(pvn))
                                      (pivs + `(col)))))))
                      (go 0 rows () ()))
   (nullsp m n R pivs) (: (pfind pv rr i j)
                          (? (0 = tally pv) 0
                             ((<pv = i) ? (zmod m (0 - lat <rr j))
                                          (pfind >pv >rr i j)))
                          (mk j) (map (\ i (? (i = j) 1 (pfind pivs R i j)))
                                      (rng 0 (n - 1)))
                          (map mk (filter (\ j !(member? j pivs)) (rng 0 (n - 1)))))
   (bsplit p r vs fs)
   (? (tally fs = r) fs
      (0 = tally vs) fs
      (: (go s fs2)
         (? (tally fs2 = r) fs2
            (s = p) fs2
            (go (s + 1)
                (flat (map (\ u (? (fvdeg u < 2) `(u)
                                   (: g (fvgcd p u (fvsub p <vs (vtrim `((zmod p s)))))
                                      (? ((0 = fvdeg g) || (fvdeg g = fvdeg u)) `(u)
                                         `(g (fvquo p u g))))))
                          fs2))))
         (bsplit p r >vs (go 0 fs))))
   (bk p f) (: n (fvdeg f)
               (pw e) (? (e = 0) '(1)
                         (0 = (% e 2)) (: h (pw (cnorm (q e 2)))
                                          (fvrem p (fvmul p h h) f))
                         (fvrem p (fvmul p '(0 1) (pw (e - 1))) f))
               xp (pw p)
               (rows i r acc) (? (i = n) acc
                                 (rows (i + 1) (fvrem p (fvmul p r xp) f)
                                       (acc + `((vpadn n r)))))
               rp (rref p n (msubI p (mflip (rows 0 '(1) ()))))
               vs (nullsp p n <rp <>rp)
               (? (1 = tally vs) `(f)
                  (bsplit p (tally vs) (map vtrim vs) `(f))))
   (ltake n l) (? (n = 0) () (<l >< ltake (n - 1) >l))
   (ldrop n l) (? (n = 0) l (ldrop (n - 1) >l))
   (hl2 P p f g h) (: gp (vtrim (map (\ c (zmod p c)) g))
                      hp (vtrim (map (\ c (zmod p c)) h))
                      e3 (fvegcd p gp hp)
                      s <>e3
                      t <>(>e3)
                      (go pk gg hh)
                      (? (P <= pk) `(gg hh)
                         (: ep (vtrim (map (\ c (zmod p (cnorm (q c pk))))
                                           (ivsub f (ivmul gg hh))))
                            dm (fvdm p (fvmul p t ep) gp)
                            w (fvadd p (fvmul p s ep) (fvmul p hp <dm))
                            (go (pk * p)
                                (ivadd gg (ivscale pk <>dm))
                                (ivadd hh (ivscale pk w)))))
                      (go p (vtrim g) (vtrim h)))
   (hliftn P p f gs)
   (? (1 = tally gs) `((vtrim (map (\ c (zmod P c)) f)))
      (: t (tally gs)
         hn (cnorm (q (t - (% t 2)) 2))
         ga (ltake hn gs)
         gb (ldrop hn gs)
         gh (hl2 P p f (fvprod p ga) (fvprod p gb))
         (hliftn P p <gh ga + hliftn P p <>gh gb)))
   (combs s l) (? (s = 0) `(())
                  (0 = tally l) ()
                  (map (link <l) (combs (s - 1) >l) + combs s >l))
   (drop1 x l) (? (0 = tally l) () ((<l = x) ? >l (<l >< drop1 x >l)))
   (lminus l xs) (? (0 = tally xs) l (lminus (drop1 <xs l) >xs))
   (zrec s P F ls acc)
   (? ((s + s) > tally ls) ((0 = pdeg F) ? acc (F >< acc))
      (: (try cs) (? (0 = tally cs) ()
                     (: cand (topoly (map (\ c (zcen P c)) (fvprod P <cs)))
                        ((0 = tally (pmod F cand)) ? `(cand <cs) (try >cs))))
         h (try (combs s ls))
         (? (0 = tally h) (zrec (s + 1) P F ls acc)
            (zrec 1 P (pquot F <h) (lminus ls <>h) (<h >< acc)))))
   primes '(3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59)
   (pickp F ps) (? (0 = tally ps) ()
                   (: pp <ps
                      fp (vtrim (map (\ c (zmod pp c)) (tovec F)))
                      fd (fder pp fp)
                      (? (&& !(0 = tally fd) (fvdeg fp = pdeg F)
                             (0 = fvdeg (fvgcd pp fp fd)))
                         pp
                         (pickp F >ps))))
   (habs v) (: (go l m2) (? (0 = tally l) m2
                            (: a ((<l < 0) ? (0 - <l) <l)
                               (go >l ((m2 < a) ? a m2))))
               (go v 0))
   (zfac F) (: n (pdeg F)
               p (pickp F primes)
               (? !p `(F)
                  (: bfs (bk p (vtrim (map (\ c (zmod p c)) (tovec F))))
                     (? (1 = tally bfs) `(F)
                        (: fv (tovec F)
                           B ((n + 1) * (n 2 * habs fv))
                           P (: (go pk) (? ((B + B) < pk) pk (go (pk * p)))
                                (go p))
                           (zrec 1 P F (hliftn P p fv bfs) ()))))))
   (pdeep f) (? (pdeg f < 4) `(f)
                (: g (pint f)
                   L (mco <g)
                   n (pdeg g)
                   (map (\ G (pmonic (msub2 G L)))
                        (zfac (msub1 g L n)))))
   (msub1 g L n) (map (\ m2 (: e <(mkey m2)
                               (link ((e = n) ? 1 (mco m2 * ((n - 1 - e) L)))
                                     (mkey m2))))
                      g)
   (msub2 G L) (map (\ m2 (cnorm (mco m2 * ((<(mkey m2)) L)) >< mkey m2)) G)
   (ufac x) (: c (mco <x)
               (dstrip g rs) (? (0 = tally rs) g
                                (dstrip (pquot g (plin <rs)) >rs))
               (go i r) (? (0 = tally r) ()
                           (: rs (rroots <r)
                              rem (dstrip <r rs)
                              one (map plin rs
                                   + ((0 = pdeg rem) ? () (pdeep rem)))
                              (flat (map (\ z (i (link z) ())) one)
                               + go (i + 1) >r)))
               ((c = 1) ? (go 1 (psff x))
                  (pconst 1 c >< go 1 (psff x)))))

; --- bivariate factorization: monic in x, hensel in y ------------------------
; the first multivariate rung: f(x y) MONIC IN X (so no y-content exists and
; yun's split rides mgcd/mdiv verbatim -- msff). zfac2 mirrors zassenhaus
; with the prime replaced by the point: SHIFT y -> y+a so the good point sits
; at the origin (then mod y^K is monomial FILTERING, evaluation the e_y = 0
; slice, and the lift arithmetic is padd/pmul whole), factor f(x 0) with the
; univariate pipeline, hensel-lift pairwise in powers of y (bezout once over
; Q[x] via pegcd; the same u/w identity as the integer lift), recombine by
; subset products truncated at K = deg_y + 1 -- a TRUE factor equals its own
; truncation, so no centering: candidates divide (m2dm, exact division by a
; monic-in-x divisor) or die. factors shift back y -> y-a. pfac (rehomed
; once more) now DISPATCHES on arity: univariate whole, bivariate monic-in-x
; whole, anything else honestly kept.
(: (mdegy f) (: (go l m2) (? (0 = tally l) m2
                             (: e <>(mkey <l)
                                (go >l ((m2 < e) ? e m2))))
               (go f 0))
   (ytrunc K f) (filter (\ m2 (<>(mkey m2) < K)) f)
   (a2to1 f) (map (\ m2 (mco m2 >< `(<(mkey m2)))) f)
   (a1to2 k g) (map (\ m2 (mco m2 >< `(<(mkey m2) k))) g)
   (xco2 f k) (map (\ m2 (mco m2 >< `(<(mkey m2))))
                   (filter (\ m2 (k = <>(mkey m2))) f))
   (yevat f aa) (? (0 = tally f) ()
                   (: c (cnorm (mco <f * ((<>(mkey <f)) aa)))
                      r (yevat >f aa)
                      ((c = 0) ? r (padd `((c >< `(<(mkey <f)))) r))))
   (a1y2 g) (map (\ m2 (mco m2 >< `(0 <(mkey m2)))) g)
   (a2y1 f) (map (\ m2 (mco m2 >< `(<>(mkey m2)))) f)
   (lpow k L) (k (pmul L) `((1 >< '(0))))
   (ysub f aa) ((aa = 0) ? f
                (: yp `((1 >< '(0 1)) (aa >< '(0 0)))
                   one `((1 >< '(0 0)))
                   (go l) (? (0 = tally l) ()
                             (padd (pmul `((mco <l >< `(<(mkey <l) 0)))
                                         ((<>(mkey <l)) (pmul yp) one))
                                   (go >l)))
                   (go f)))
   (m2dm f d) (: (go u qq)
                 (? (0 = tally u) `(qq ())
                    (pdeg u < pdeg d) `(qq u)
                    (: t2 (mlift (mcof u (pdeg u)) (pdeg u - pdeg d))
                       (go (psub u (pmul t2 d)) (padd t2 qq))))
                 (go f ()))
   (pegcd a b) (? (0 = tally b)
                  (: c (cinv (mco <a)) `((pmonic a) `((c >< '(0))) ()))
                  (: qq (pquot a b)
                     e3 (pegcd b (pmod a b))
                     `(<e3 <>(>e3) (psub <>e3 (pmul qq <>(>e3))))))
   (msff f) (: (go w y) (? (0 = pdeg w) ()
                           (: z (psub y (pd w 0))
                              a2 (mgcd w z)
                              (a2 >< go (mdiv w a2) (mdiv z a2))))
               (? ((0 = tally f) || (0 = pdeg f)) ()
                  (: g (mgcd f (pd f 0))
                     (go (mdiv f g) (mdiv (pd f 0) g)))))
   (hl22 K g G H) (: e3 (pegcd G H)
                     s <>e3
                     t <>(>e3)
                     (go k gg hh)
                     (? (K <= k) `(gg hh)
                        (: e0 (xco2 (psub g (pmul gg hh)) k)
                           te (pmul t e0)
                           u (pmod te G)
                           w (padd (pmul s e0) (pmul H (pquot te G)))
                           (go (k + 1)
                               (padd gg (a1to2 k u))
                               (padd hh (a1to2 k w)))))
                     (go 1 (a1to2 0 G) (a1to2 0 H)))
   (hliftn2 K g us) (? (1 = tally us) `((ytrunc K g))
                       (: t2 (tally us)
                          hn (cnorm (q (t2 - (% t2 2)) 2))
                          ua (ltake hn us)
                          ub (ldrop hn us)
                          (prodl l) (? (1 = tally l) <l (pmul <l (prodl >l)))
                          gh (hl22 K g (prodl ua) (prodl ub))
                          (hliftn2 K <gh ua + hliftn2 K <>gh ub)))
   (zrec2 s K g ls acc)
   (? ((s + s) > tally ls) ((0 = pdeg g) ? acc (g >< acc))
      (: (prod2 l) (? (1 = tally l) <l (ytrunc K (pmul <l (prod2 >l))))
         (try cs) (? (0 = tally cs) ()
                     (: cand (ytrunc K (prod2 <cs))
                        dm (m2dm g cand)
                        ((0 = tally <>dm) ? `(cand <cs <dm) (try >cs))))
         h (try (combs s ls))
         (? (0 = tally h) (zrec2 (s + 1) K g ls acc)
            (zrec2 1 K <>(>h) (lminus ls <>h) (<h >< acc)))))
   (zfac2 f) (? (0 = mdegy f)
                (map (a1to2 0) (pfac (a2to1 f)))
                (: (pick as) (? (0 = tally as) ()
                                (: f0 (yevat f <as)
                                   (? (0 = pdeg (pgcd f0 (pd f0 0))) `(<as)
                                      (pick >as))))
                   aw (pick '(0 1 -1 2 -2 3 -3 4 -4 5 -5 6 -6))
                   (? (0 = tally aw) `(f)
                      (: a <aw
                         g (ysub f a)
                         us (pfac (a2to1 (ytrunc 1 g)))
                         (? (1 = tally us) `(f)
                            (: K (mdegy f + 1)
                               fs (zrec2 1 K g (hliftn2 K g us) ())
                               (map (\ z (ysub z (0 - a))) fs)))))))
   (zfac2g f) (: L (mcof f (pdeg f))
                 (? (L = `((1 >< '(0)))) (zfac2 f)
                    (: n (pdeg f)
                       (bsub es) (? (0 = tally es) ()
                                    (padd (mlift (? (<es = n) `((1 >< '(0)))
                                                    (pmul (mcof f <es)
                                                          (lpow ((n - 1) - <es) L)))
                                                 <es)
                                          (bsub >es)))
                       (bunsub G) (: (go es) (? (0 = tally es) ()
                                                (padd (mlift (pmul (mcof G <es)
                                                                   (lpow <es L))
                                                             <es)
                                                      (go >es)))
                                     (go (me1s G)))
                       (map (\ G (: gb (bunsub G)
                                    (pmonic (mdiv gb (mlift (mcont gb) 0)))))
                            (zfac2 (bsub (me1s f)))))))
   (lcs fs) (? (0 = tally fs) 1 (cnorm (mco <(<fs) * lcs >fs)))
   (bfac2 x) (: cy (mcont x)
                pp (mdiv x (mlift cy 0))
                yfs (map a1y2 (filter (\ g !(0 = pdeg g)) (pfac cy)))
                (go i r) (? (0 = tally r) ()
                            (: fs ((0 = pdeg <r) ? () (zfac2g <r))
                               (flat (map (\ z (i (link z) ())) fs)
                                + go (i + 1) >r)))
                all (yfs + go 1 (msff pp))
                cst (cnorm (mco <x * cinv (lcs all)))
                ((cst = 1) ? all (pconst 2 cst >< all)))
; --- n variables: kronecker packs onto the bivariate engine ------------------
; the last rung. vpack folds the LAST variable into the one before it --
; the substitution y -> Y^D, z -> Y with D past the last variable's degree,
; a ring homomorphism whose exponent map e*D + e' is INJECTIVE below D and
; ORDER-PRESERVING on lex keys, so the packed poly is canonical as it
; stands. n variables pack down to two, the bivariate engine factors the
; image whole (recursively through pfac), and subsets of image factors
; recombine: a candidate is the UNPACKED subset product, guarded by the
; pack ROUND-TRIP (a true factor's product never carries past D, and the
; guard turns carry accidents into clean misses), then certified by mdm --
; multivariate lex division with remainder, safe on any divisor. hits
; divide out and the search restarts; the residual is a factor too. one
; leading constant squares the product, as everywhere. this trades eez's
; staged lifting for degree growth in the image -- exact and complete
; either way, and the honest choice at this scale.
   (mrg D k) (? (0 = tally >(>k)) `(((<k * D) + <>k))
                (<k >< mrg D >k))
   (vpack x D) (map (\ m2 (mco m2 >< mrg D (mkey m2))) x)
   (vunpk g D) (map (\ m2 (: (go l) (? (0 = tally >l)
                                       `((cnorm (q (<l - (% <l D)) D)) (% <l D))
                                       (<l >< go >l))
                             (mco m2 >< go (mkey m2))))
                    g)
   (lastk k) (? (0 = tally >k) <k (lastk >k))
   (mdeglast x) (: (go l m2) (? (0 = tally l) m2
                                (: e (lastk (mkey <l))
                                   (go >l ((m2 < e) ? e m2))))
                   (go x 0))
   (kz1? x) ((1 = tally x) && (0 = net (mkey <x)))
   (kge a b) (? (0 = tally a) 1 ((<a < <b) ? () (kge >a >b)))
   (mdm f d) (: (go u qq rr)
                (? (0 = tally u) `(qq rr)
                   (kge (mkey <u) (mkey <d))
                   (: t2 `((link (cnorm (mco <u * cinv (mco <d)))
                                 (ksub (mkey <u) (mkey <d))))
                      (go (psub u (pmul t2 d)) (padd t2 qq) rr))
                   (go >u qq (padd `((mco <u >< mkey <u)) rr)))
                (go f () ()))
   (mprod l) (? (1 = tally l) <l (pmul <l (mprod >l)))
   (nrec s D x ls acc)
   (? ((s + s) > tally ls) ((kz1? x) ? acc (pmonic x >< acc))
      (: (try cs) (? (0 = tally cs) ()
                     (: pp (mprod <cs)
                        cand (vunpk pp D)
                        (? !(vpack cand D = pp) (try >cs)
                           (: dm (mdm x cand)
                              ((0 = tally <>dm) ? `(cand <cs <dm)
                                 (try >cs))))))
         h (try (combs s ls))
         (? (0 = tally h) (nrec (s + 1) D x ls acc)
            (nrec 1 D <>(>h) (lminus ls <>h) (<h >< acc)))))
   (mfacn x) (: D (mdeglast x + 1)
                fs (pfac (vpack x D))
                out (nrec 1 D x (filter (\ g !(kz1? g)) fs) ())
                cst (cnorm (mco <x * cinv (lcs out)))
                ((cst = 1) ? out
                   (pconst (tally (mkey <x)) cst >< out)))
   (pfac p2) (: x (load p2)
                (? (0 = tally x) ()
                   (1 = tally (mkey <x))
                   ((0 = pdeg x) ? `(x) (ufac x))
                   !(2 = tally (mkey <x))
                   ((kz1? x) ? `(x) (mfacn x))
                   ((0 = pdeg x) && (0 = mdegy x)) `(x)
                   (0 = pdeg x) (map a1y2 (pfac (a2y1 x)))
                   (bfac2 x))))

; --- the poly coin: polynomials first-class under the generic ops ----------
; strike a CANONICAL monomial list (the engine's own output) and the surface
; arrives whole: + - * run padd/psub/pmul through the die (slots 1/5/2), =
; compares canonical payloads, and net-mode 1 (slot 6) nets a poly by its
; MONOMIAL COUNT -- so ! reads "is the zero polynomial" even when the
; coefficients sum red (the raw-list layer needs (0 = tally p) for that). the
; ZERO polynomial is the coin over () -- never bare (), which rides + and * as
; the do-nothing unit and would dodge annihilation. a method owns its domain:
; a star operand lifts to the other side's arity; against the zero poly the
; arity is unknowable, so + and - answer () there, but * needs no lift at all
; (c times the zero poly IS the zero poly); anything else answers (). slot 7
; declares polys STARS, so a numeral powers a poly through its own * -- ipow's
; squaring over coins, seeded at 1, lands on the zero-mul face. pd/pdet work
; the raw layer: (load p) to reach it, (poly ..) to come back.
; a RED star is a COEFFICIENT, not nothing: polyish? tests the zero point by
; identity and value ((), the charm 0), never by net -- !(-1) is true, and the
; old !v gate read -1 as the zero poly, so (+ p -1) dropped its constant.
(: polyd #(0 'poly 6 1 7 1)
   (polyish? v) (? (id? v ()) 1 (0 = v) 1 (two? v) (two? <v) 0)
   (arity v) (tally (mkey <v))
   (pok? v w) (? (polyish? v) 1 (star? v) (polyish? w ? (tally w) ()) ())
   (plift v w) (polyish? v ? v (pconst (arity w) v))
   (pmeth f) (\ a b (: x (load a) y (load b)
                       ((pok? x y && pok? y x) ?
                        (coin polyd (f (plift x y) (plift y x)))
                        ())))
   (zface v w) (star? v ? (polyish? w ? (0 = tally w) 0) 0)
   mulm (pmeth pmul)
   _ (pin polyd 1 (pmeth padd))
   _ (pin polyd 2 (\ a b (: x (load a) y (load b)
                            (? (zface x y) (coin polyd ())
                               (zface y x) (coin polyd ())
                               (mulm a b)))))
   _ (pin polyd 5 (pmeth psub))
   (poly p) (polyish? p ? (coin polyd p) ()))

; --- the eval bridge: a poly compiles to a live closure ---------------------
; pemit renders a poly (raw or coin) as core source over the given variable
; symbols, one per key position. the emission is folded DYADIC + * ** only --
; never (+ a b c), which is application, and never (e x) numeral power, which
; COMPOSES a non-numeric operand -- a power rides ('** s e): ** is the bound
; base-first power, exact through every DIE_STAR operand (integers, bignums,
; gems, q coins). pfn wraps the emission in \ and hands it to ev: a real
; curried closure, glaze-eligible; args arrive in syms order.
(: (mulfold l) (1 = tally l ? <l `('* <l (mulfold >l)))
   (mfacts key syms) (? (0 = tally key) ()
                        (0 = <key) (mfacts >key >syms)
                        (1 = <key) (<syms >< mfacts >key >syms)
                        (`('** <syms <key) >< mfacts >key >syms))
   (mterm m syms) (: fs (mfacts (mkey m) syms) c (mco m)
                     (? (0 = tally fs) c
                        (c = 1) (mulfold fs)
                        (mulfold (c >< fs))))
   (psum pp syms) (? (0 = tally pp) 0
                     (1 = tally pp) (mterm <pp syms)
                     `('+ (mterm <pp syms) (psum >pp syms)))
   (pemit p syms) (psum (load p) syms)
   (snocl l x) (0 = tally l ? `(x) (<l >< snocl >l x))
   (pfn p syms) (ev ('\ >< snocl syms (pemit p syms))))

; --- the matcher: rules over terms, riding the kanren unifier ---------------
; a TERM is core source -- a star, a symbol, or a folded dyadic form ('+ a b),
; exactly what pemit renders and ev compiles -- so the rewrite layer and the
; eval bridge share one term language. a PATTERN is a term with kanren logic
; vars at the open positions, and `unify` (the compiler-grade core; the
; relational rewrite layer it was kept public for) does the matching: one var
; shared across two positions is a NONLINEAR pattern -- (- a a) matches only
; when both sides agree, structurally, coins by value -- at no extra cost.
; `rule` compiles the readable (vars pat tpl) triple: each named symbol in vs
; becomes ONE var shared across both halves; `grule` adds a GUARD -- a closure
; over the bound values, vs order -- that must answer green for the rewrite to
; fire (how (d a v) -> 0 says "only when a is free of v"). `match` answers the
; binding, or () on a miss -- a var-less hit is the EMPTY binding, whose truth
; is blue, so test a miss against (), never with ! alone. `sew` fills a
; template under a binding. `rewrite` drives a rule list bottom-up to a
; fixpoint (rw1 tries the rules once, at the root, guards included);
; termination is the rule set's burden. a named symbol is a (name . mint)
; chain -- the SAME shape as a var -- so mark and rewrite treat noms as
; atoms, never walking into them.
(: sew (from 'kanren 'subst)
   mnil (from 'kanren 'empty_dict)
   (match p x) (: r (unify mnil p x) (ufail? r ? () r))
   (vlook v ps) (? (0 = tally ps) ()
                   (<(<ps) = v) >(<ps)
                   (vlook v >ps))
   (mark ps t) (? (nom? t) (: w (vlook t ps) (!w ? t w))
                  (two? t) (mark ps <t >< mark ps >t)
                  t)
   (grule vs pat tpl g) (: ps (map (\ s (s >< var s)) vs)
                           `((mark ps pat) (mark ps tpl) g (map (\ pr >pr) ps)))
   (rule vs pat tpl) (grule vs pat tpl 0)
   (gpass? rl r) (: g (nth rl 2)
                    (g = 0 ? 1 (ap g (map (sew r) (nth rl 3)))))
   (rw1 rs x) (? (0 = tally rs) x
                 (: rl <rs r (unify mnil <rl x)
                    (? (ufail? r) (rw1 >rs x)
                       (gpass? rl r) (sew r <>rl)
                       (rw1 >rs x))))
   (rewrite rs x) (: y (? (nom? x) x (two? x) (map (rewrite rs) x) x)
                     z (rw1 rs y)
                     (z = y ? y (rewrite rs z))))

; --- laws + fold: the shipped simplifier -------------------------------------
; `laws` is the core identity table -- units, annihilators, inverses, the
; doubling collect, the log/exp cancellations, and the pythagorean identity --
; every rule shrinking or size-keeping-once, so the fixpoint lands. the
; CONDITIONAL laws (a/a, 0/a, cancellation, sqrt(a^2) -> a) ride guards over
; the two certifiers, each carrying an ASSUMPTIONS CONTEXT c -- a list of
; fact terms like ((pos x) (nz n)) -- as their first argument: nzc certifies
; "nonzero" (a value-nonzero ground star by the square-sat test -- $(t*t) > 0
; forces t /= 0, sound for red stars and blind only toward blue, the safe
; direction -- pi/e, an exp form, a power or root or product of nonzeros, or
; a DECLARED nz/pos/neg fact); posc certifies "positive" (positive ground,
; pi/e, exp, roots/products/sums of positives, even powers of nonzeros, or a
; declared pos). nz? is the empty-context face. `glaws` builds the guarded
; rules over a context; laws = ulaws + (glaws ()), so bare simp certifies
; only the MANIFEST -- x/x stays -- while the c-family carries declared
; facts through every driver: (simpc c t) -- assume is its given name --
; (expandc c t), (derivc c t v); the plain simp/expand/deriv are their
; empty-context faces: (assume '((nz x)) '(/ x x)) is 1. `xlaws`
; adds the GROWING rules on top: distribution and the log/exp splits. simp
; never expands; `expand` does (each pass pushes a * inside a +, or a log/exp
; through its operand, so that fixpoint lands too -- collected on the way).
; fold1 computes a GROUND node: (op a b) for op in + - * ** / with both
; operands stars (the open star: integers, gems, q coins, poly coins alike)
; folds to its value, cnormed so an integer-valued coin lands the integer.
; a power folds by APPLICATION -- (b a) is a**b -- on a whole exponent.
; Q IS THE FOLD CODOMAIN: a ground quotient or negative power folds EXACTLY
; through the q coin ((/ 1 2) and (** 2 -1) both land (q 1 2), (/ 6 3) demotes
; to 2) -- never to a gem. a zero divisor/base (nz? again) or a slot-less die
; keeps its term. `simpw` drives a rule table + fold bottom-up to a
; fixpoint, the same shape as rewrite (noms are atoms, never walked into);
; simp is its laws face, expand its xlaws face -- through collect (below).
(: (nzc c t) (? (nom? t) (? ((t = 'pi) || (t = 'e)) 1
                             (member? `('nz t) c) 1
                             (member? `('pos t) c) 1
                             (member? `('neg t) c) 1
                             ())
                 (star? t) !(0 = sat (t * t))
                 (two? t) (? (<t = 'exp) 1
                             (<t = '**) (nzc c <>t)
                             (<t = 'sqrt) (nzc c <>t)
                             (<t = '*) (nzc c <>t && nzc c <>(>t))
                             ())
                 ())
   (nz? t) (nzc () t)
   (posc c t) (? (nom? t) (? ((t = 'pi) || (t = 'e)) 1
                             (member? `('pos t) c) 1
                             ())
                 (star? t) (0 < t)
                 (two? t) (? (<t = 'exp) 1
                             (<t = 'sqrt) (posc c <>t)
                             (<t = '**)
                             (? (posc c <>t) 1
                                (&& (whole? <>(>t)) (0 = (% <>(>t) 2))
                                    (nzc c <>t)) 1
                                ())
                             (<t = '*) (posc c <>t && posc c <>(>t))
                             (<t = '+) (posc c <>t && posc c <>(>t))
                             ())
                 ())
   (glaws c) `((grule '(a) '(/ a a) 1 (nzc c))
               (grule '(a) '(/ 0 a) 0 (nzc c))
               (grule '(a b) '(/ (* a b) b) 'a (\ a b (nzc c b)))
               (grule '(a b) '(/ (* b a) b) 'a (\ a b (nzc c b)))
               (grule '(a) '(sqrt (** a 2)) 'a (posc c)))
   ulaws `((rule '(a) '(+ a 0) 'a)
          (rule '(a) '(+ 0 a) 'a)
          (rule '(a) '(* a 1) 'a)
          (rule '(a) '(* 1 a) 'a)
          (rule '(a) '(* a 0) 0)
          (rule '(a) '(* 0 a) 0)
          (rule '(a) '(- a 0) 'a)
          (rule '(a) '(- a a) 0)
          (rule '(a) '(/ a 1) 'a)
          (rule '(a) '(** a 1) 'a)
          (rule '(a) '(** a 0) 1)
          (rule '(a) '(** 1 a) 1)
          (rule '(a) '(- 0 (- 0 a)) 'a)
          (rule '(a) '(+ a a) '(* 2 a))
          (rule '(a) '(log (exp a)) 'a)
          (rule '(a) '(exp (log a)) 'a)
          (rule () '(exp 0) 1)
          (rule () '(log 1) 0)
          (rule () '(log e) 1)
          (rule () '(exp 1) 'e)
          (rule '(a) '(** e a) '(exp a))
          (rule () '(sin 0) 0)
          (rule () '(cos 0) 1)
          (rule () '(tan 0) 0)
          (rule () '(atan 0) 0)
          (rule () '(asin 0) 0)
          (rule () '(acos 1) 0)
          (rule () '(sin pi) 0)
          (rule () '(cos pi) -1)
          (rule () '(tan pi) 0)
          (rule () '(sin (* 2 pi)) 0)
          (rule () '(cos (* 2 pi)) 1)
          (rule () `('sin `('* (q 1 2) 'pi)) 1)
          (rule () `('cos `('* (q 1 2) 'pi)) 0)
          (rule () `('sin `('* (q 1 4) 'pi)) `('* (q 1 2) '(sqrt 2)))
          (rule () `('cos `('* (q 1 4) 'pi)) `('* (q 1 2) '(sqrt 2)))
          (rule () `('sin `('* (q 1 6) 'pi)) (q 1 2))
          (rule () `('cos `('* (q 1 6) 'pi)) `('* (q 1 2) '(sqrt 3)))
          (rule () `('sin `('* (q 1 3) 'pi)) `('* (q 1 2) '(sqrt 3)))
          (rule () `('cos `('* (q 1 3) 'pi)) (q 1 2))
          (rule () '(asin 1) `('* (q 1 2) 'pi))
          (rule () '(acos 0) `('* (q 1 2) 'pi))
          (rule () '(acos -1) 'pi)
          (rule () '(atan 1) `('* (q 1 4) 'pi))
          (rule () '(sqrt 0) 0)
          (rule () '(sqrt 1) 1)
          (rule '(a) '(** (sqrt a) 2) 'a)
          (rule '(a) '(sin (- 0 a)) '(- 0 (sin a)))
          (rule '(a) '(cos (- 0 a)) '(cos a))
          (rule '(a) '(tan (- 0 a)) '(- 0 (tan a)))
          (rule '(a) '(atan (- 0 a)) '(- 0 (atan a)))
          (rule '(a) '(asin (- 0 a)) '(- 0 (asin a)))
          (rule '(a) '(/ (sin a) (cos a)) '(tan a))
          (rule '(a) '(tan (atan a)) 'a)
          (rule '(a) '(sin (asin a)) 'a)
          (rule '(a) '(cos (acos a)) 'a)
          (rule '(a) '(+ (* (sin a) (sin a)) (* (cos a) (cos a))) 1)
          (rule '(a) '(+ (** (sin a) 2) (** (cos a) 2)) 1)
          (rule '(a) '(+ (** (cos a) 2) (** (sin a) 2)) 1))
   laws (ulaws + glaws ())
   grows `((rule '(a b c) '(* a (+ b c)) '(+ (* a b) (* a c)))
                (rule '(a b c) '(* (+ a b) c) '(+ (* a c) (* b c)))
                (rule '(a b c) '(* a (- b c)) '(- (* a b) (* a c)))
                (rule '(a b c) '(* (- a b) c) '(- (* a c) (* b c)))
                (rule '(a b) '(log (* a b)) '(+ (log a) (log b)))
                (rule '(a b) '(log (/ a b)) '(- (log a) (log b)))
                (rule '(a b) '(log (** a b)) '(* b (log a)))
                (rule '(a b) '(exp (+ a b)) '(* (exp a) (exp b)))
                (rule '(a b) '(exp (- a b)) '(/ (exp a) (exp b)))
                (rule '(a b) '(sin (+ a b))
                      '(+ (* (sin a) (cos b)) (* (cos a) (sin b))))
                (rule '(a b) '(sin (- a b))
                      '(- (* (sin a) (cos b)) (* (cos a) (sin b))))
                (rule '(a b) '(cos (+ a b))
                      '(- (* (cos a) (cos b)) (* (sin a) (sin b))))
                (rule '(a b) '(cos (- a b))
                      '(+ (* (cos a) (cos b)) (* (sin a) (sin b)))))
   (xlawsc c) ((ulaws + glaws c) + grows)
   xlaws (xlawsc ())
   (fold1 t) (? (nom? t) t
                !(two? t) t
                !(3 = tally t) t
                !(member? <t '(+ - * ** /)) t
                (: a <>t b <>(>t)
                   (? !(star? a && star? b) t
                      (<t = '**)
                      (? !(whole? b) t
                         (: v (? (0 <= b) (b a)
                                 !(nz? a) ()
                                 (whole? a) (q 1 ((0 - b) a))
                                 (b a))
                            ((star? v) ? (cnorm v) t)))
                      (<t = '/)
                      (? !(nz? b) t
                         (: v (? (whole? a && whole? b) (q a b) (a / b))
                            ((star? v) ? (cnorm v) t)))
                      (cnorm (? (<t = '+) (a + b) (<t = '-) (a - b) (a * b))))))
   (simpw rs t) (: y (? (nom? t) t (two? t) (map (simpw rs) t) t)
                   z (fold1 (rw1 rs y))
                   (z = y ? y (simpw rs z)))
   (simp t) (simpw laws t)
   (simpc c t) (simpw (ulaws + glaws c) t)
   assume simpc)

; --- collect: the commutative collect -----------------------------------------
; + and * are commutative and associative, and - is + wearing a sign, so
; collect canonicalizes each +/-/* spine, bottom-up: flatten (sflat carries
; the sign through every -, so subtraction IS a signed addend), split every
; element into (c u) -- ground coefficient and core -- sort, merge equal
; cores summing coefficients exactly, and refold: green terms first
; (constant last), then each red term SUBTRACTED in order, so the sign
; returns to the surface as - ; a lone ground red stays the signed star, a
; pure negation stays (- 0 u). a product also POWER-COLLECTS: repeated
; factors merge into (** u n), exponents whole and >= 1 only (x * x**-1
; never folds -- x might be 0), summed through the same pair merge. the
; orders are structural and deterministic: a product sorts bases noms-first
; (variables precede function forms); a sum sorts by DEGREE descending (vks
; counts a core's factors, powers weighted) then the sorted factor-string,
; so the binomial expand lands x^2 + 2xy + y^2 in that order, and (x+y)(x-y)
; cancels to x^2 - y^2. expand runs it; simp stays order-preserving.
(: (spine o t) (? (nom? t) `(t)
                  (&& (two? t) (3 = tally t) (o = <t))
                  (spine o <>t + spine o <>(>t))
                  `(t))
   (tkey t) ((nom? t) ? ("0" + show t) ("1" + show t))
   (tins lt x ys) (? (0 = tally ys) `(x)
                     (lt x <ys) (x >< ys)
                     (<ys >< tins lt x >ys))
   (tsort lt xs) (? (0 = tally xs) () (tins lt <xs (tsort lt >xs)))
   (csplit t) (? (nom? t) `(1 t)
                 (star? t) `(t 1)
                 (&& (two? t) (3 = tally t) ('* = <t) (star? <>t)) `(<>t <>(>t))
                 `(1 t))
   (sone s t) (: p (csplit t) `((cnorm (s * <p)) <>p))
   (sflat s t) (? (nom? t) `((sone s t))
                  (&& (two? t) (3 = tally t) ('+ = <t))
                  (sflat s <>t + sflat s <>(>t))
                  (&& (two? t) (3 = tally t) ('- = <t))
                  (sflat s <>t + sflat (0 - s) <>(>t))
                  `((sone s t)))
   (cmerge ps) (? (0 = tally ps) ()
                  (0 = tally >ps) ps
                  (: p <ps r <(>ps)
                     ((<>p = <>r) ? (cmerge (`((cnorm (<p + <r)) <>p) >< >(>ps)))
                                    (p >< cmerge >ps))))
   (cone p) (: c <p u <>p
               (? (u = 1) c (c = 1) u `('* c u)))
   (refold o xs) ((1 = tally xs) ? <xs `(o <xs (refold o >xs)))
   (vks t) (? (nom? t) `((show t))
              (&& (two? t) (3 = tally t) ('* = <t))
              (vks <>t + vks <>(>t))
              (&& (two? t) (3 = tally t) ('** = <t)
                  (whole? <>(>t)) (0 < <>(>t)))
              (<>(>t) (+ (vks <>t)) ())
              `((show t)))
   (tcat l) (? (0 = tally l) "" (<l + tcat >l))
   (vkey t) (tcat (tsort (\ a b (a < b)) (vks t)))
   (slt p r) (: da (tally (vks <>p)) db (tally (vks <>r))
                (? (db < da) 1 (da < db) 0 (vkey <>p < vkey <>r)))
   (tsum t) (: ps (cmerge (tsort slt (sflat 1 t)))
               ks (filter (\ p !(<p = 0)) ps)
               gs (filter (\ p !(<p < 0)) ks)
               ns (filter (\ p (<p < 0)) ks)
               (clast l) (filter (\ p !(<>p = 1)) l + filter (\ p (<>p = 1)) l)
               xs (map cone (clast gs))
               ys (map cone (map (\ p `((cnorm (0 - <p)) <>p)) (clast ns)))
               (dsub a l) ((0 = tally l) ? a (dsub `('- a <l) >l))
               (? (0 = tally ks) 0
                  (0 = tally ns) (refold '+ xs)
                  ((0 = tally xs) &&
                   (0 = tally (filter (\ p !(<>p = 1)) ns))) <(<ns)
                  (dsub ((0 = tally xs) ? 0 (refold '+ xs)) ys)))
   (tmul xs) (? (0 = tally xs) 1 (cnorm (<xs * tmul >xs)))
   (esplit f) (? (nom? f) `(1 f)
                 (&& (two? f) (3 = tally f) ('** = <f)
                     (whole? <>(>f)) (0 < <>(>f)))
                 `(<>(>f) <>f)
                 `(1 f))
   (eone p) (: n <p u <>p ((n = 1) ? u `('** u n)))
   (unneg f) (? (nom? f) `(1 f)
                (&& (two? f) (3 = tally f) ('- = <f) (0 = <>f)) `(-1 <>(>f))
                `(1 f))
   (tprod t) (: ps (map unneg (spine '* t))
                gs (map (\ p <>p) ps)
                c (cnorm (tmul (map (\ p <p) ps) * tmul (filter star? gs)))
                us (map eone (cmerge (tsort (\ p r (tkey <>p < tkey <>r))
                                            (map esplit
                                                 (filter (\ f !(star? f)) gs)))))
                (femit k) ((k = 1) ? (refold '* us) (refold '* (k >< us)))
                (? (0 = tally us) c
                   (c = 0) 0
                   (c < 0) `('- 0 (femit (cnorm (0 - c))))
                   (femit c)))
   (collect t) (? (nom? t) t
                  !(two? t) t
                  (: u (map collect t)
                     (? !(3 = tally u) u
                        ('+ = <u) (tsum u)
                        ('- = <u) (tsum u)
                        ('* = <u) (tprod u)
                        u)))
   (expandc c t) (simpc c (collect (simpw (xlawsc c) t)))
   (expand t) (expandc () t))

; --- symbolic d: the derivative as a rule set --------------------------------
; the term head `d` -- (d t v) -- reduces by dlaws: sum/product/quotient rules
; are plain patterns, (d a a) -> 1 rides the nonlinear pattern, sin/cos and
; exp/log carry the chain rule, the power rule guards on a CONSTANT exponent
; (free? n v -- a symbolic exponent needs the log bridge and stays), and the
; base case is the GUARDED rule: (d a v) -> 0 exactly when a is free? of v
; -- the guard the matcher grew for it. a head with no
; rule keeps its (d ..) term: a derivative rune cannot take stays symbolic,
; the honest answer. deriv drives dlaws to a fixpoint and simps the result,
; so d(x*x)/dx lands (* 2 x), not (+ (* 1 x) (* x 1)).
(: (free? t v) (? (t = v) 0
                  (nom? t) 1
                  (two? t) ((free? <t v && free? >t v) ? 1 0)
                  1)
   dlaws `((grule '(a v) '(d a v) 0 free?)
           (rule '(a) '(d a a) 1)
           (rule '(a b v) '(d (+ a b) v) '(+ (d a v) (d b v)))
           (rule '(a b v) '(d (- a b) v) '(- (d a v) (d b v)))
           (rule '(a b v) '(d (* a b) v) '(+ (* (d a v) b) (* a (d b v))))
           (rule '(a b v) '(d (/ a b) v) '(/ (- (* (d a v) b) (* a (d b v))) (* b b)))
           (grule '(a n v) '(d (** a n) v) '(* n (* (** a (- n 1)) (d a v)))
                  (\ a n v (free? n v)))
           (rule '(a v) '(d (sin a) v) '(* (cos a) (d a v)))
           (rule '(a v) '(d (cos a) v) '(- 0 (* (sin a) (d a v))))
           (rule '(a v) '(d (tan a) v) '(/ (d a v) (** (cos a) 2)))
           (rule '(a v) '(d (atan a) v) '(/ (d a v) (+ 1 (** a 2))))
           (rule '(a v) '(d (sqrt a) v) '(/ (d a v) (* 2 (sqrt a))))
           (rule '(a v) '(d (asin a) v) '(/ (d a v) (sqrt (- 1 (** a 2)))))
           (rule '(a v) '(d (acos a) v) '(- 0 (/ (d a v) (sqrt (- 1 (** a 2))))))
           (rule '(a v) '(d (exp a) v) '(* (exp a) (d a v)))
           (rule '(a v) '(d (log a) v) '(/ (d a v) a)))
   (derivc c t v) (simpc c (collect (simpc c (rewrite dlaws `('d (simpc c t) v)))))
   (deriv t v) (derivc () t v))

; --- unfix: terms back to the infix surface ----------------------------------
; the inverse of opfix, as a FORM: unfix renders a core term as low-paren
; infix source -- a value, not text; show prints it, ev runs it, and
; (opfix (unfix t)) = t is the round-trip law the gate asserts over a corpus.
; the grammar, probed: bands are C-like (* / above + -, ** above both),
; arithmetic folds LEFT within its band, ** folds right, application binds
; tightest. the wrap rule follows the HAND: the child on the folding side wraps
; only strictly below its parent's band (the fold already nests that way), the
; other child wraps at the band and below. so (- (- 10 4) 3) renders 10 - 4 - 3
; while (- 10 (- 4 3)) renders 10 - (4 - 3). an application splices bare unless
; its head is an operator, and application arguments wrap whenever compound.
; `dyadics` is mopped post-egg, so ugrip carries the bands AND hands rune
; renders (grip . hand, 1 = left); an op off the table stays prefix, which reads
; back unchanged.
(: ugrip '((+ 1 1) (- 1 1) (* 2 1) (/ 2 1) (** 3 0))
   (glook o gs) (? (0 = tally gs) ()
                   (<(<gs) = o) <>(<gs)
                   (glook o >gs))
   (hlook o gs) (? (0 = tally gs) 0
                   (<(<gs) = o) <>>(<gs)
                   (hlook o >gs))
   (ophead? x) (nom? <x ? (glook <x ugrip ? 1 0) 0)
   (infix3? t) (3 = tally t ? (ophead? t) 0)
   (uxw x) (? (nom? x) x (two? x) (unfix x) x)
   (uxs x g) (? (nom? x) `(x)
                !(two? x) `(x)
                (infix3? x) (glook <x ugrip < g ? `((unfix x)) (unfix x))
                (ophead? x) `((unfix x))
                (unfix x))
   (unfix t) (? (nom? t) t
                !(two? t) t
                (infix3? t) (: g (glook <t ugrip) h (hlook <t ugrip)
                               (uxs <>t (h ? g (g + 1)) + `(<t)
                                + uxs <>(>t) (h ? (g + 1) g)))
                (uxw <t >< map uxw >t)))

; --- the field Q[x]/(m) ----------------------------------------------------
(: (vmax a b) (a < b ? b a)
   (vpad v n) (? (n = 0) ()
                 (0 = tally v) (q 0 1 >< vpad () (n - 1))
                 (<v >< vpad >v (n - 1)))
   (vzip a b) (0 = tally a ? () ((<a + <b) >< vzip >a >b))
   (vadd a b) (: n (vmax (tally a) (tally b)) (vzip (vpad a n) (vpad b n)))
   (vscale c v) (map (* c) v)
   (vshift v n) (n = 0 ? v (q 0 1 >< vshift v (n - 1)))
   (vmul a b) (0 = tally a ? () (vadd (vscale <a b) (vshift (vmul >a b) 1)))
   (vlast v) (1 = tally v ? <v (vlast >v))
   (vinit v) (1 = tally v ? () (<v >< vinit >v))
   (vrem xd v) (tally v < tally xd + 1 ? (vpad v (tally xd))
                  (vrem xd (vadd (vinit v)
                                 (vshift (vscale (vlast v) xd) (tally v - (tally xd + 1))))))
   (kmul xd a b) (vrem xd (vmul a b))
   (knum d c) (vpad `((q c 1)) d))
