; test/gate/objcopy.l -- the fixture images for test/gate/objcopy.sh, ours end to end:
; one object off obj.l, linked three ways by ldbare32, at addresses picked to reach the
; format rules a real board's image happens to miss.
;   cross  a lane crossing a 64K line, so a data record has to STOP there
;   mb     a lane crossing 1 MB, where the base record changes age -- and an entry
;          under 1 MB, where the start record does too
;   gap    two `at` runs with a hole between them (zeros in the flat image) and a
;          `copy` run, whose load address is not its run address
; the odd 12-byte lane ahead of the code is the whole trick: it leaves everything
; behind it at 12 mod 16, so records land across the lines instead of on them.
(: (mkfix d)
   (: (pat n k) (map (\ i ((i + k) % 256)) (jot n))
      obj (string (objsecs32 'thumb2
             `(`(".text" 1 6 4 (`(`('label '_start)) + (map (\ _ `('nop)) (jot 24))))
               `(".rodata" 1 2 4 `(`('raw (pat 12 1))))
               `(".data" 1 3 4 `(`('raw (pat 40 7)))))
             '("_start") () ()))
      src `(`("fix.o" obj))
      (lay1 nm runs) (: _ (ld-write ((d + "/fix-") + nm + ".elf")
                                    (ldbare32 'thumb2 runs src ()))
                        0)
      _ (lay1 "cross" `(`('at 0x0800FFF0 'rodata 'text) `('copy 0x20000000 'data)))
      _ (lay1 "mb"    `(`('at 0x000FFFF0 'rodata 'text) `('copy 0x20000000 'data)))
      _ (lay1 "gap"   `(`('at 0x08000000 'rodata) `('at 0x08001000 'text)
                        `('copy 0x20000000 'data)))
      0))
