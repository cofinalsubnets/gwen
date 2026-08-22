; bench/ccrypto.l -- the cipher workloads ccbench.sh times, one per row.
;
; the compiler shootout's `test` row runs the corpus, which is love code: it reads
; each compiler's work on love.c's VM, and it reads it AVERAGED. these two rows read
; one C function each, and they were added because the average was hiding a 25x --
; on the same source, mooncc's chacha20 is ~30 MB/s where clang's is ~740, while its
; poly1305 sits within 2x. the difference between the two is the whole finding:
;
;   chacha    a 16-word state ARRAY, indexed in the inner loop -- the shape with no
;             register residency in mooncc, so every touch is a fresh load+store.
;   poly1305  five limbs as scalar LOCALS, the shape mooncc already homes.
;
; so the pair is a lever gauge, not a cipher benchmark: chacha wide and poly narrow
; says the gap is array slots. they close together, or the diagnosis was wrong.
; (love.c's own z-tray lane is the same shape and ~5x -- the reason to care.)
;
; ⚠ these run the NIFS (host/tls.c), never crew/tls's love ciphers -- this table
; times C compilers. crew/tls/bench.l is where the two languages meet.
; ⚠ no `use` and no crew file: the binary under test lives in out/bench/cc, so the
; module walk would miss lib/ entirely. everything here is built from the core.
;
; ccbench.sh cats this file and then one driver call; the reps are fixed so all
; three compilers do identical work, and each row prints its sentinel so a lane
; that silently answered nothing cannot be timed as if it were fast.
(:
   ccbytes 65536                             ; 64 KiB per call, 1024 chacha blocks
   msg (string (cask ccbytes))               ; content is irrelevant: both ciphers
   non (string (cask 12))                    ; are constant-time by construction

   ; a key that is not all zeros -- poly1305 with r = 0 would still cost the same
   ; (that IS the invariant), but a degenerate input reads like an accident.
   cckey (: b (cask 32)
            (go i) (? (32 <= i) (string b) (: _ (pin b i (& (i * 7) 255)) (go (i + 1))))
            (go 0))

   ; ⚠ (f) at zero operands is f itself, unrun. the unit is the operand, and it is ().
   (rep k f) (? (k < 1) 0 (: _ (f ()) (rep (k - 1) f)))

   (cc-run _) (: _a (rep 2000 (\ _ (chacha20 cckey non 1 msg)))
                 (puts "ccrypto chacha: ok\n"))
   (po-run _) (: _a (rep 20000 (\ _ (poly1305 cckey msg)))
                 (puts "ccrypto poly1305: ok\n")))
