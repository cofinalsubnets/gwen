; --- test/host/salt.l: salt, the config door (lib/salt.l) ------------------
; the files are the interesting part, so this writes real ones under out/ and
; reads them back. `salt-file` is the testable unit -- it takes the path, so a
; test never has to fake HOME or depend on whose machine it runs on. run
; standalone from the repo root:
;   cat test/00-init.l test/host/salt.l | out/host/love

(use 'salt)

(: st-dir "out/host/.salt"
   _ (mkdir "out" 493) _ (mkdir "out/host" 493) _ (mkdir st-dir 493)
   (st-put nm text) (: p (st-dir + "/" + nm)
                       q (open p "w")
                       _ (say q text)
                       _ (close q)
                       p)
   st-gone (st-dir + "/no-such-file.l"))

; --- one file ---------------------------------------------------------------
; a setting is `(key operand ..)`: the head names it, the tail is the value.
(: st-a (st-put "a.l" "(singleton 1)\n(deprecated old-thing (worse \"say better\"))\n"))
(assert
 (: c (salt-file st-a (tablet 0))
    (&& (1 = (salt-one c 'singleton 0))
        (2 = (tally (salt-all c 'deprecated)))
        ('old-thing = <(salt-all c 'deprecated)))))

; --- a config is data, never code -----------------------------------------
; the file is read with `sound`, the datum reader, and nothing in it is ever
; evaluated -- so a dotfile cannot run anything. if this line were evaled the
; test would exit here instead of asserting; it lands as a plain list.
(: st-q (st-put "quit.l" "(danger (quit 1))\n"))
(assert
 (: c (salt-file st-q (tablet 0))
    v (salt-one c 'danger ())
    (&& (two? v) ('quit = <v))))

; --- presence is the open -------------------------------------------------
; an absent file and an empty one both mean "no overrides", and neither may
; disturb what is already in the tablet. (an empty config file is a perfectly
; good thing for a config file to be -- the question is whether it opened.)
(: st-e (st-put "empty.l" ""))
(assert
 (: c (salt-file st-e (salt-file st-a (tablet 0))) (1 = (salt-one c 'singleton 0)))
 (: c (salt-file st-gone (salt-file st-a (tablet 0))) (1 = (salt-one c 'singleton 0)))
 !(salt-all (salt-file st-e (tablet 0)) 'singleton)
 !(salt-all (salt-file st-gone (tablet 0)) 'singleton))

; --- the overlay ------------------------------------------------------------
; later wins, key by key: the file read second speaks last, and a key it does
; not mention is left exactly as the first file set it. (that is the whole
; point of the order -- ~/.love/etc/libra.l for your taste, ./.libra.l over it
; for the tree's own.)
(: st-b (st-put "b.l" "(singleton 0)\n"))
(assert
 (: c (salt-file st-b (salt-file st-a (tablet 0)))
    (&& (0 = (salt-one c 'singleton 9))                  ; replaced
        (2 = (tally (salt-all c 'deprecated)))))         ; untouched
 (: c (salt-file st-a (salt-file st-b (tablet 0)))
    (1 = (salt-one c 'singleton 9))))                    ; ...and the other way round

; --- a form that is not a setting is simply not a setting ------------------
; a typo must not take the tool down, and half a config is better than none:
; the bad forms are skipped in silence and everything around them still lands.
(: st-j (st-put "junk.l" "loose-symbol\n42\n\"a string\"\n(singleton 1)\n((f x) 2)\n(deprecated a)\n"))
(assert
 (: c (salt-file st-j (tablet 0))
    (&& (1 = (salt-one c 'singleton 0))
        (1 = (tally (salt-all c 'deprecated))))))

; ...and a dropped paren ends the read rather than spinning on it: `sound`
; answers `torn` on an unfinished shape, which is a third thing to catch --
; not a datum and not the clean end.
(: st-t (st-put "trunc.l" "(singleton 1)\n(deprecated foo\n"))
(assert
 (: c (salt-file st-t (tablet 0))
    (&& (1 = (salt-one c 'singleton 0)) !(salt-all c 'deprecated))))

; --- reading a setting ------------------------------------------------------
; salt-all cannot tell an absent key from one written with no operand -- both
; are () -- so a switch is `(singleton 1)`, never a bare `(singleton)`.
(: st-n (st-put "none.l" "(singleton)\n"))
(assert
 (: c (salt-file st-n (tablet 0))
    (&& !(salt-all c 'singleton) ('dflt = (salt-one c 'singleton 'dflt))))
 (: c (salt-file st-a (tablet 0))
    (&& ('dflt = (salt-one c 'never-written 'dflt))
        !(salt-all c 'never-written))))

; the roster lane: an entry is either a bare name or `(name "hint"), and the
; answer is the entry wrapped -- presence out of band, since a bare name nets
; whatever its spelling nets and could not say "found" on its own.
(assert
 (: c (salt-file st-a (tablet 0))
    (&& (two? (salt-has? c 'deprecated 'old-thing))
        ('old-thing = <>(salt-has? c 'deprecated 'old-thing))
        (two? (salt-has? c 'deprecated 'worse))
        ('worse = <<>(salt-has? c 'deprecated 'worse))   ; the (name "hint") pair
        !(salt-has? c 'deprecated 'never-listed)
        !(salt-has? c 'no-such-key 'old-thing))))

; --- the door ---------------------------------------------------------------
; (salt 'app) is salt-file twice over the two well-known paths. the app name
; spells both, so nothing here hardcodes a tool.
(assert
 (tablet? (salt 'salt-test-no-such-app))                 ; neither file exists: an empty tablet
 !(salt-all (salt 'salt-test-no-such-app) 'singleton))

(test_fin ())
(puts "salt: ok")
(putc 10)
