; per-TU: the FINAL forms line (the python oracle's food) then val.l's rows
(: _ (use 'moon)
   is9 ["out/host/" "./" "src/" "out/lib/" "crew/moon/lib/math/" "crew/moon/include/"]
   src "@FILE@"
   r (cc-parse src {'is is9 'ds '("ai_tco=1" "AiHaveVersionH") 'tgt 'amd64 'os 'linux 'vb () 'dia 'moon})
   _ (? !(okv? r) (puts ("!! parsefail " + src + "\n"))
        (@ r [_ ast stag sigs weaks sfns svars sects alias aligns ainl noinl consts rstrs]
           (: ob (cgen-obj ast stag sigs weaks [sfns svars sects alias aligns ainl noinl consts rstrs ()] 'amd64)
              (? !(two? ob) (puts ("!! genfail " + src + "\n"))
                 (: _ (puts ("== " + src + "\n"))
                    _ (putx <>ob)
                    _ (putc 10)
                    _ (: (go l) (? !(two? l) 0 (: _ (puts (show <l)) _ (putc 10) (go >l)))
                         (go (vgauge src <>ob)))
                    0)))))
   0)
