; lib/salt.l -- SALT, what you add to taste: the config door every crew app can
; share. (salt 'libra) answers a tablet of that app's settings, read from two
; files, the second overlaying the first:
;
;   ~/.love/etc/<app>.l    your taste, wherever the binary happens to live
;   ./.<app>.l             this tree's own, and it travels in git
;
; the SECOND wins, which is the direction the module walk already runs (cwd's
; lib/ before the seat's): a project speaks last about itself. the search is the
; CWD only, no walk upward -- cook's (discover _) sets that precedent, and a
; config that changes meaning with your shell's pwd is worse than one you have to
; name. absent files are the NORMAL case: no config at all is the same tablet as
; two empty ones.
;
; ⚠ HOME, not the seat. the seat of /usr/bin/love is /usr, and nobody's settings
; live in /usr/etc -- so this is the one place a love program reads the
; environment to find a file. (the module walk still reads none.)
;
; ⚠ A CONFIG IS DATA, NEVER CODE. the file is read with `sound`, the datum
; reader, and no part of it is ever evaluated -- so a dotfile cannot run anything,
; and nothing in it needs quoting, because nothing in it is evaluated. write
; `(deprecated foo bar)`, not `(deprecated 'foo 'bar)`.
;
; ⚠ PRESENCE IS THE OPEN (crew/vi/hue.l's law, which this generalizes): a config
; file with nothing in it means NO OVERRIDES, which is a perfectly good thing for
; a config file to mean -- so the question is whether it OPENED, never whether it
; had bytes. `(! (fsize p))` would call an empty config a missing one.
;
; ⚠ A FORM THAT IS NOT A SETTING IS SIMPLY NOT A SETTING. a bad line is skipped
; in silence and the rest of the file still lands: a typo in a dotfile must not
; take the tool down with it, and half a config is better than none.
;
; a setting is one form, `(key operand ..)`: the head names it, the tail is its
; value. so (singleton 1) is a switch, (deprecated a b c) a roster, and a repeated
; key REPLACES rather than appends -- one line, one answer.
(:
   ; --- reading one file ------------------------------------------------------
   ; overlay path's settings onto the tablet c (mutated, and answered back). the
   ; open IS the presence test -- one syscall, no gap between asking and reading.
   ; ⚠ `sound` answers THREE ways and all three must be caught: the datum CONSED
   ; ONTO THE REST, () at a clean end, or `torn` on an unfinished shape. ask
   ; `two?` for the datum -- a config file with a dropped paren must stop with
   ; what landed, never spin.
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
   (salt-user app) (: h (getenv "HOME")
                      (? (string? h) (h + "/.love/etc/" + (string app) + ".l") ()))
   (salt-here app) ("." + (string app) + ".l")

   (salt app) (: c (tablet 0)
                 u (salt-user app)
                 _ (? (string? u) (salt-file u c) 0)
                 _ (salt-file (salt-here app) c)
                 c)

   ; --- reading a setting -----------------------------------------------------
   ; the whole tail, or (); and the first operand, or a default. ⚠ salt-all
   ; cannot tell an ABSENT key from one written with no operand -- both are () --
   ; so a switch is `(singleton 1)`, never a bare `(singleton)`.
   (salt-all c k)   (peep c k ())
   (salt-one c k d) (: v (peep c k ()) (? (two? v) <v d))

   ; is nm listed under k? the roster lane: an entry is either the bare name or
   ; `(name "hint")`, so this reads the head of a pair and the name of a bare one.
   (salt-has? c k nm)
    (: (go l) (? (! (two? l)) ()
                 (: e <l (? ((? (two? e) <e e) = nm) `(1 e) (go >l))))
       (go (salt-all c k))))
