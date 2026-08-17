; --- test/host/loader.l: use-on-miss, the loader (love/prel.l) ---
; `use` loads what it doesn't have: lib/<name>.l on a miss -- one fresh layer,
; leave registers, the prior use splices below the head -- and a SLASHED name
; ('x/y, one symbol) is an INCLUDE into the current head layer, no registration.
; the lib/ lane resolves against the cwd, so run from the repo root:
;   cat test/00-init.l test/host/loader.l | out/host/love
; the baked-table lane (the frontend's ai_libs) is every frontend's boot; this script smokes
; the filesystem lane and the registry behavior around it. fixtures are written
; here and removed at the foot -- zz- names, so a crashed run's leftovers are
; recognizable.

(: _ (mkdir "lib" 493) _ (mkdir "lib/zz-loadprobe" 493)
   _ (: p (open "lib/zz-loadprobe.l" "w")
        _ (say p "(: (zz-probe-hi x) (+ x 1) zz-probe-pin 41)")
        (close p))
   _ (: p (open "lib/zz-loadprobe/sub.l" "w")
        _ (say p "(: (zz-probe-sub x) (* x 10))")
        (close p)))

; the load: one layer, registered, spliced -- bare names resolve in forms
; compiled AFTER the use (the capture law: a use must precede its readers)
(use 'zz-loadprobe)
(assert (3 = zz-probe-hi 2)
        (41 = (from 'zz-loadprobe 'zz-probe-pin))
        (lit? (from 'zz-loadprobe)))

; the include: into the head, and NOTHING registers under the slashed name
(use 'zz-loadprobe/sub)
(assert (70 = zz-probe-sub 7)
        (nil? (from 'zz-loadprobe/sub)))

; a re-use hits the REGISTRY, never the file again: rewrite the file, use again,
; the module keeps its first reading
(: p (open "lib/zz-loadprobe.l" "w") _ (say p "(: zz-probe-pin 999)") (close p))
(use 'zz-loadprobe)
(assert (41 = (from 'zz-loadprobe 'zz-probe-pin)))

; a SELF-REGISTERING file -- its text is a (module 'nm ..) wrap (cook.l's shape) --
; keeps ITS registration on both lanes: the loader's empty wrapper layer must not
; pin over the module the file made. symbol lane, then the explicit-path lane.
(: p (open "lib/zz-selfprobe.l" "w")
   _ (say p "(module 'zz-selfprobe (: zz-self-pin 77))") (close p))
(use 'zz-selfprobe)
(assert (77 = (from 'zz-selfprobe 'zz-self-pin)))
(: p (open "lib/zz-pathprobe.l" "w")
   _ (say p "(module 'zz-pathprobe (: zz-path-pin 78))") (close p))
(use "lib/zz-pathprobe.l")
(assert (78 = (from 'zz-pathprobe 'zz-path-pin)))

(: _ (unlink "lib/zz-loadprobe.l") _ (unlink "lib/zz-loadprobe/sub.l")
   _ (unlink "lib/zz-selfprobe.l") _ (unlink "lib/zz-pathprobe.l")
   _ (rmdir "lib/zz-loadprobe") 0)

; the SEAT rungs are RETIRED (the modules arc, rung 3): the loader no longer
; walks <seat>/../lib/ or its love/ subfolder -- an artifact's modules are baked
; or image-carried. this binary is out/host/love, so its old rungs were out/lib/
; and out/lib/love/ -- fixture there, cwd-lib absent, and the walk must MISS:
; the use scares no-module and nothing registers.
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
