; --- test/host/loader.l: use on a path, the loader (love/prel.l) ---
; `use` answers a module two ways and only two: a NAME it already holds -- baked,
; catted, or loaded earlier -- and a PATH the caller writes out. it invents no
; paths of its own, so a name it lacks is a miss even when a file of that name is
; sitting in the cwd. a path load is one fresh layer, leave registers it under the
; file's basename, and the prior use splices it below the head.
; paths resolve against the cwd, so run from the repo root:
;   cat test/00-init.l test/host/loader.l | out/host/love
; the baked-table lane (the frontend's ai_libs) is every frontend's boot; this script
; smokes the path lane and the registry behavior around it. fixtures are written here
; and removed at the foot -- zz- names, so a crashed run's leftovers are recognizable.

(: _ (mkdir "lib" 493) _ (mkdir "lib/zz-loadprobe" 493)
   _ (: p (open "lib/zz-loadprobe.l" "w")
        _ (say p "(: (zz-probe-hi x) (+ x 1) zz-probe-pin 41)")
        (close p))
   _ (: p (open "lib/zz-loadprobe/sub.l" "w")
        _ (say p "(: (zz-probe-sub x) (* x 10))")
        (close p)))

; the NAME lane touches no filesystem. lib/zz-loadprobe.l is right there and the
; symbol still misses: the loader has no path to invent, so the splice raises by
; name and nothing registers.
(: a (trap (\ _ (use 'zz-loadprobe)) () (\ a b 'caught))
 (assert (id? a 'caught) (nil? (from 'zz-loadprobe))))

; the path lane: one layer named for the basename, registered, spliced -- bare names
; resolve in forms compiled after the use (the capture law: a use must precede its readers)
(use "lib/zz-loadprobe.l")
(assert (3 = zz-probe-hi 2)
        (41 = (from 'zz-loadprobe 'zz-probe-pin))
        (lit? (from 'zz-loadprobe)))

; the basename is the whole name: a nested path registers under `sub`, and the
; directory it sat in is not part of it
(use "lib/zz-loadprobe/sub.l")
(assert (70 = zz-probe-sub 7)
        (lit? (from 'sub))
        (nil? (from 'zz-loadprobe/sub)))

; a re-use hits the registry, never the file again -- by name or by the path that
; named it: rewrite the file, use both ways, the module keeps its first reading
(: p (open "lib/zz-loadprobe.l" "w") _ (say p "(: zz-probe-pin 999)") (close p))
(use 'zz-loadprobe)
(use "lib/zz-loadprobe.l")
(assert (41 = (from 'zz-loadprobe 'zz-probe-pin)))

; a self-registering file -- its text is a (module 'nm ..) wrap (cook.l's shape) --
; keeps its registration: the loader's empty wrapper layer must not pin over the
; module the file made, and the name it registers need not be the basename. where
; the two differ the wrapper was never written to, so the basename does not register
; at all -- an empty tablet standing under it would answer a later use with nothing.
(: p (open "lib/zz-pathprobe.l" "w")
   _ (say p "(module 'zz-pathprobe (: zz-path-pin 78))") (close p))
(use "lib/zz-pathprobe.l")
(assert (78 = (from 'zz-pathprobe 'zz-path-pin)))
(: p (open "lib/zz-elsewhere.l" "w")
   _ (say p "(module 'zz-othername (: zz-other-pin 79))") (close p))
(use "lib/zz-elsewhere.l")
(assert (79 = (from 'zz-othername 'zz-other-pin))
        (nil? (from 'zz-elsewhere)))

; a path that opens nothing is a miss, like any other
(: a (trap (\ _ (use "lib/zz-absent.l")) () (\ a b 'caught))
 (assert (id? a 'caught)))

(: _ (unlink "lib/zz-loadprobe.l") _ (unlink "lib/zz-loadprobe/sub.l")
   _ (unlink "lib/zz-pathprobe.l") _ (unlink "lib/zz-elsewhere.l")
   _ (rmdir "lib/zz-loadprobe") 0)

; the seat rungs are retired (the modules arc, rung 3) and so is the cwd walk:
; the loader reads no directory of its own, so a fixture beside the binary is as
; invisible as one in lib/. this binary is out/host/love, so its old rungs were
; out/lib/ and out/lib/love/ -- fixture there, and the use must miss.
(: _ (mkdir "out/lib/love" 493)
   _ (: p (open "out/lib/zz-seatprobe.l" "w")
        _ (say p "(: zz-seat-pin 51)")
        (close p))
   _ (: p (open "out/lib/love/zz-fhsprobe.l" "w")
        _ (say p "(: zz-fhs-pin 52)")
        (close p)))
(: a (trap (\ _ (use 'zz-seatprobe)) () (\ a b 'caught))
   b (trap (\ _ (use 'zz-fhsprobe)) () (\ a b 'caught))
 (assert (id? a 'caught) (id? b 'caught)
         (nil? (from 'zz-seatprobe)) (nil? (from 'zz-fhsprobe))))
(: _ (unlink "out/lib/zz-seatprobe.l") _ (unlink "out/lib/love/zz-fhsprobe.l")
   _ (rmdir "out/lib/love") 0)
(test_fin ())
(puts "loader: ok")
(putc 10)
