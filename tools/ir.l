#!/usr/bin/env love
; tools/ir.l -- print the machine-form IR `mooncc -fir=` wrote into an object or a binary.
;
;   love tools/ir.l FILE               every function the record carries, one form a line
;   love tools/ir.l FILE NAME          just that one
;   love tools/ir.l FILE -l            the names and form counts, nothing else
;
; ⚠ ANY FILE, ANY TARGET, ON ANY MACHINE: a record is text and an ELF is a table, so an
; aarch64 object read on x86-64 prints aarch64's forms. Nothing here disassembles, so nothing
; here needs a per-arch mnemonic table or an objdump built with the right target list -- which
; is the argument for the record existing at all (doc/moon.md, -fir).
;
; ⚠ it reads what the compiler INTENDED at the end of gen; holo assembles those forms, so a
; cross-check against the emitted bytes is a real differential and has already earned its
; keep once (test/gate/splice.l's tail-jump law, doc/moon-regalloc.md).
(: _ (use 'irec)
   (die m) (: _ (say err ("ir: " + m + "\n")) _ (flush err) (quit 2))
   as (cup argv)                                  ; ⚠ argv is a BINDING, not a nullary door: (argv ()) is
                                                    ;   an index into it. the head is this script's own path.
   path (? (two? as) <as (die "usage: ir.l FILE [NAME | -l]"))
   what (? (two? as && two? >as) <>as ())
   rows (irec-read path)
   _ (? (two? rows) 0
        (die (path + ": no ai_lvm_ir here -- was it built with mooncc -fir=?")))
   (pr l) (? (two? l) (: _ (say out ("   " + show <l + "\n")) (pr >l)) 0)
   (one r) (: _ (say out (string <r + "  (" + show (tally >r) + " forms)\n")) (pr >r))
   (all l) (? (two? l) (: _ (one <l) (all >l)) 0)
   (names l) (? (two? l) (: _ (say out ("  " + string <(<l) + "  " + show (tally >(<l)) + "\n"))
                            (names >l)) 0)
   _ (? (nil? what) (: _ (say out (path + ": " + show (tally rows) + " functions\n")) (all rows))
        (what = "-l") (: _ (say out (path + ": " + show (tally rows) + " functions\n")) (names rows))
        (: fs (irec-of rows (intern what))
           (? (two? fs) (one (intern what >< fs))
              (die (what + " is not in " + path + "'s record")))))
   _ (flush out) (quit 0))
