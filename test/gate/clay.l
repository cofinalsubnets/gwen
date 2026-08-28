; test/clay.l -- G1, clay's faithfulness gate: (cparse (clay-show c)) == c, over
; every C file in test/cc/. run inside a woken mooncc image, which is where the
; front end lives; clay itself arrives by (use 'clay), a module, so none of its
; helper names touch the compiler's book.
;
; the comparison is STRUCTURAL, on the reparsed AST, and never a string compare of
; the C text -- twice now the printer has been the thing standing
; in front of the bug. so this proves the SHOWER and the PARSER agree about what a
; tree means; it deliberately proves nothing about the text resembling its original.
;
; ⚠ THE PARTITION IS THE POINT. cparse cannot keep a typedef, a struct definition or
; a _Static_assert -- all four land as the empty marker (tdef), which carries nothing
; and which clay-show REFUSES rather than papering over with a `;`. those files are
; counted as INEXPRESSIBLE and named, never skipped in silence. that count is the
; live measure of the gap between "moon can parse this C" and "clay can say it back",
; and the way to make it fall is to teach parse.l to FILL the marker -- (tdef name ty)
; is already in clay's grammar waiting for it.
;
; the gate FAILS on a round-trip difference or a shower crash. it does not fail on an
; inexpressible file, and it does not fail on a file the parser itself refuses (a few
; in test/cc/ are there to BE refused).
(:
   _ (use "crew/moon/clay.l")
   _ (use 'moon)                       ; the front end (clex/cpp/cparse-t) is module 'moon
   _ (use 'kore)                       ; uread below is the u-floor's
   cshow (from 'clay 'clay-show)
   cok? (from 'clay 'clay-ok?)
   nl (string 10)
   (el l i) (? (i < 1) <l (el >l (i - 1)))
   (say1 s) (puts (s + nl))
   (dotc? s) (: n (tally s) (&& (2 < n) (&& ((s (n - 2)) = 46) ((s (n - 1)) = 99))))

   ; the front end, without cc-parse's udie -- a refusal has to come back as a value
   ; here, since a third of the point is counting the refusals.
   (front src)
    (: r (uread src)
       (? (! (two? r)) ()
          (: lx (clex ((deftext (archdefs 'amd64)) + <>r))
             (? (! (okv? lx)) ()
                (: pp (cpp <>lx (\ n s (incload () () (cdir src) n s)) src)
                   (? (! (okv? pp)) ()
                      (: ps (cparse-t <>pp 'amd64)
                         (? (okv? ps) [1 (caup ps)] ()))))))))
   ; the same, off TEXT rather than a path: the reparse leg
   (front2 txt)
    (: lx (clex txt)
       (? (! (okv? lx)) ()
          (: pp (cpp <>lx (\ n s ()) "<clay>")
             (? (! (okv? pp)) ()
                (: ps (cparse-t <>pp 'amd64)
                   (? (okv? ps) [1 (caup ps)] ()))))))

   files (filter dotc? (readdir "test/cc"))

   ; one file -> 'ok | 'diff | 'inexpr | 'noparse | 'reparse
   (one f)
    (: p (front ("test/cc/" + f))
       (? (! (two? p)) 'noparse
          (: a <>p
             v (cok? a)
             (? (! (id? v 1)) 'badshape
                (: t (cshow a)
                   (? (gripe? t) 'inexpr
                      (: b (front2 t)
                         (? (! (two? b)) 'reparse
                            (<>b = a) 'ok
                            'diff))))))))

   ; the tally, and the NAMES of everything that is not plain ok -- a count alone
   ; would let a regression hide inside a category.
   (go l ok df ix np rp bs bad)
    (? (! (two? l)) [ok df ix np rp bs (rev bad)]
       (: f <l v (one f)
          (? (id? v 'ok) (go >l (ok + 1) df ix np rp bs bad)
             (id? v 'diff) (go >l ok (df + 1) ix np rp bs (link ['diff f] bad))
             (id? v 'inexpr) (go >l ok df (ix + 1) np rp bs (link ['inexpr f] bad))
             (id? v 'noparse) (go >l ok df ix (np + 1) rp bs bad)
             (id? v 'reparse) (go >l ok df ix np (rp + 1) bs (link ['reparse f] bad))
             (go >l ok df ix np rp (bs + 1) (link ['badshape f] bad)))))
   r (go files 0 0 0 0 0 0 ())
   ok (el r 0) df (el r 1) ix (el r 2) np (el r 3) rp (el r 4) bs (el r 5)
   bad (el r 6)

   _ (each bad (\ b (say1 ("clay-g1: " + (string <b) + " " + (el b 1)))))
   _ (say1 ("clay-g1: " + (show ok) + " round-trip, " + (show ix) + " inexpressible, "
            + (show np) + " unparsed, " + (show (tally files)) + " files"))
   ; ⚠ A VACUOUS PASS IS A FAIL. this file lived under test/ for one commit's worth
   ; of minutes, where the corpus glob swept it into a run with no compiler in scope:
   ; every file "unparsed", nothing checked, and it printed PASS. so the floor is
   ; explicit -- most of test/cc/ must actually round-trip, or the front end is not
   ; there and the gate is measuring nothing.
   floor 50
   _ (? (ok < floor)
        (: _ (say1 ("clay-g1: FAIL -- only " + (show ok) + " files round-tripped, under the "
                    + (show floor) + " floor: the front end is not in scope"))
           (quit 1)) 0)
   ; the failures: a tree that came back DIFFERENT, C the parser could not read back,
   ; or a tree clay-ok? rejected. an inexpressible file is a measured gap, not a red.
   (? (! (df + rp + bs)) (: _ (say1 "clay-g1: pass") (quit 0))
      (: _ (say1 ("clay-g1: FAIL -- " + (show df) + " diff, " + (show rp)
                  + " unreadable, " + (show bs) + " malformed")) (quit 1))))
