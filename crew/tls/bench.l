; crew/tls/bench.l -- the two ciphers, love against their C twins.
;
; crew/tls/chacha.l and crew/tls/poly1305.l are the readable statement of each
; cipher; src/tls.c holds a C twin of each behind a nif, and test/host/tlsc.l
; proves the two answer the same bytes. this asks the other question: what does
; the readable one cost?
;
; the answer is the binary's, not the language's. love ships mooncc-built, and
; mooncc is weakest at exactly the shape both ciphers lean on -- an indexed loop
; over an array. run this again under the clang binary the static lane leaves in
; out/host-musl/ and the C chacha moves ~25x, the love chacha ~2x, while a scalar
; VM loop moves the catalogued 1.3x. a ratio off one binary is a ratio about that
; binary, and the chacha ratio is the one that moves most.
;
; not a gate: it prints numbers, it asserts nothing. run it by hand --
;   out/host/love crew/tls/bench.l
(use "lib/tls.l")

(:
   size 65536                                      ; 64 KiB, 1024 chacha blocks
   key (bytes 32 (\ i i))
   non (bytes 12 (\ i (? (i = 7) 74 0)))
   msg (bytes size (\ i (& ((i * 7) + 11) 255)))

   ; fire f `n` times and answer the nanoseconds. (f) at zero operands is f
   ; itself, unrun -- the unit is the operand, and it is () rather than 0.
   (rep n f) (? (n < 1) 0 (: _ (f ()) (rep (n - 1) f)))
   (ns n f) (: t0 (nclock 0) _ (rep n f) (nclock t0))

   ; MB/s, and a ratio to one decimal, both in charm arithmetic (no floats: the
   ; products stay far under max-charm and integer division is exact enough for
   ; a number this coarse).
   (mbs n t) ((size * n * 10000) // t)             ; tenths of a MB/s
   (dec1 n) ((show (n // 10)) + "." + (show (n % 10)))
   (padl s w) (? ((tally s) < w) (padl (" " + s) w) s)

   (row nom t n) (puts ((padl nom 16) + (padl (dec1 (mbs n t)) 9) + " MB/s"
                        + (padl (show ((t // n) // 1000)) 9) + " us/call\n"))

   ; the four runs. the rep counts differ by three orders of magnitude because the
   ; costs do; each is picked so its lane runs a few hundred ms.
   lc (ns 8 (\ _ (cc-xor key non 1 msg)))
   cc (ns 200 (\ _ (chacha20 key non 1 msg)))
   lp (ns 40 (\ _ (po-mac key msg)))
   cp (ns 2000 (\ _ (poly1305 key msg)))

   _0 (puts ("crew/tls bench -- " + (show size) + " bytes per call\n\n"))
   _1 (row "chacha20 love" lc 8)
   _2 (row "chacha20 C" cc 200)
   _3 (row "poly1305 love" lp 40)
   _4 (row "poly1305 C" cp 2000)
   _5 (puts ("\n  chacha20 C is " + (dec1 ((lc * 10 * 200) // (cc * 8))) + "x the love lane"
             + "\n  poly1305 C is " + (dec1 ((lp * 10 * 2000) // (cp * 40))) + "x the love lane\n"))
   0)
