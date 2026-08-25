; bench/cnifs.l -- the heavy host nifs ccbench.sh times, one row each.
;
; ccrypto.l's pair is a LEVER: two ciphers chosen so the difference between them names
; the defect (array slots). These three are the opposite question -- real work the tree
; actually does, on the path a user waits on -- and they are here because the corpus row
; averages a compiler over all of src/love.c and the ciphers are 120 lines apiece.
;
;   inflate   src/inflate.c: a bit reader, a table lookup per symbol, a byte copy. The
;             branchiest of the three, and the one `love source` spends its time in.
;   crc32     src/hash.c: eight INDEPENDENT table lookups per eight bytes and nothing
;             else. No branches in the loop at all, so it reads pure load/xor throughput.
;   sha256    src/hash.c: a 64-word message schedule ARRAY plus eight scalar working
;             variables -- ccrypto's two shapes in ONE function, which is why it is not
;             a lever and why it sits between them.
;
; What that spread buys: a compiler that is behind on all three is behind, and one that
; is behind on inflate and level on crc32 is losing to branches, not to loads. On the
; day they were added it read mooncc 1.85x gcc on inflate and 1.5x on crc32.
;
; ⚠ these run the NIFS, never lib/gz.l's love twins -- this table times C compilers.
; ⚠ no `use` and no lib file: the binary under test lives in out/bench/cc, so the
; module walk would miss lib/ entirely. everything here is built from the core, and the
; inflate row's stream is laid beside it by ccgen.l through the host love.
(:
   ; ⚠ VARIED BYTES, not a zeroed cask like ccrypto's: crc32 indexes eight tables with
   ; the data, so an all-NUL message would touch one entry of each and read a cache the
   ; real one does not get. Built small and doubled -- a love loop over the whole megabyte
   ; would cost more than the row it feeds, and inside the timed window.
   nifseed (: b (cask 4096)
              (go i) (? (4096 <= i) (string b) (: _ (pin b i (& (i * 7) 255)) (go (i + 1))))
              (go 0))
   (nifdbl s k) (? (k < 1) s (nifdbl (s + s) (k - 1)))
   nifmsg (nifdbl nifseed 8)                 ; 4 KiB -> 1 MiB

   ; ⚠ (f) at zero operands is f itself, unrun. the unit is the operand, and it is ().
   (rep k f) (? (k < 1) 0 (: _ (f ()) (rep (k - 1) f)))

   ; ⚠ the reps are fixed so all three compilers do identical work, and chosen so the
   ; FASTEST lane still spends ~300 ms of real work -- doc/misc/moon-gauge.md puts the floor on
   ; a ccbench ratio at 4%, and a row that finishes in 50 ms cannot say anything at all.
   (crc-run _) (: _a (rep 1000 (\ _ (crc32 nifmsg)))
                  (puts "cnifs crc32: ok\n"))
   (sha-run _) (: _a (rep 100 (\ _ (sha256 nifmsg)))
                  (puts "cnifs sha256: ok\n"))
   ; the stream and its inflated size come from ccbench.sh, which laid them with ccgen.l
   (inf-run p n k)
    (: q (open p "r")
       _ (? q 0 (: _ (say err "cnifs: no stream\n") (quit 1)))
       d ((slurp q) + "")
       _ (close q)
       _a (rep k (\ _ (inflate d n)))
       (puts "cnifs inflate: ok\n")))
