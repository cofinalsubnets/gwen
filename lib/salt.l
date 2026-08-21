; lib/salt.l -- SALT, what you add to taste: the config door every crew app can
; share. (salt 'libra) answers a tablet of that app's settings, read from two
; files, the second overlaying the first:
;
;   ~/.love/etc/<app>.l    your taste, wherever the binary happens to live
;   ./.<app>.l             this tree's own, and it travels in git
;
; the second wins, and the search is the cwd only, with no walk upward. absent
; files are the normal case: no config at all is the same tablet as two empty ones.
;
; a config is data, never code -- the file is read with `sound` and no part of it
; is evaluated, so nothing in it needs quoting: write `(deprecated foo bar)`.
; a form that is not a setting is skipped in silence and the rest still lands.
;
; a setting is one form, `(key operand ..)`: the head names it, the tail is its
; value. so (singleton 1) is a switch, (deprecated a b c) a roster, and a repeated
; key replaces rather than appends.
(:
   ; --- reading one file ------------------------------------------------------
   ; overlay path's settings onto the tablet c, mutated and answered back. the open
   ; is the presence test. `sound` answers three ways -- the datum consed onto the
   ; rest, () at a clean end, `torn` on a dropped paren -- so ask two?, never spin.
   (salt-file path c)
    (: q (open path "r")
       (? q (: t (slurp q)
               (go x) (: r (sound x)
                          (? (two? r)
                             (: d (cap r)
                                _ (? (&& (two? d) (name? <d)) (pin c <d >d) 0)
                                (go (cup r)))
                             0))                               ; a clean end, or torn: keep what landed
               _ (go t)
               _ (close q)
               c)
          c))

   ; --- the door --------------------------------------------------------------
   ; HOME, not the seat: /usr/bin/love seats at /usr and nobody's settings live in
   ; /usr/etc. reading it wrong looks like no config at all, which is legal here.
   (salt-user app) (: h (getenv "HOME")
                      (? (string? h) (h + "/.love/etc/" + (string app) + ".l") ()))
   (salt-here app) ("." + (string app) + ".l")

   (salt app) (: c (tablet 0)
                 u (salt-user app)
                 _ (? (string? u) (salt-file u c) 0)
                 _ (salt-file (salt-here app) c)
                 c)

   ; --- reading a setting -----------------------------------------------------
   ; the whole tail, or (); and the first operand, or a default. an absent key and
   ; one written with no operand are both (), so a switch is `(singleton 1)`.
   (salt-all c k)   (peep c k ())
   (salt-one c k d) (: v (peep c k ()) (? (two? v) <v d))

   ; is nm listed under k? a roster entry is a bare name or `(name "hint")`
   (salt-has? c k nm)
    (: (go l) (? (! (two? l)) ()
                 (: e <l (? ((? (two? e) <e e) = nm) `(1 e) (go >l))))
       (go (salt-all c k))))
