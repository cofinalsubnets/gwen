; doc/misc/proto/lzobench.l -- one lane of the LZO-versus-gzip weigh-in, run from make's own
; love so the codec and the corpus are the tree's.
;
;   love -l doc/misc/proto/lzo.l -l doc/misc/proto/zstd.l doc/misc/proto/lzobench.l LANE IN OUT [REPS]
;
; the lanes are lzoc / lzod (doc/misc/proto/lzo.l), zsc / zsd (doc/misc/proto/zstd.l) and
; gzc / gzd (lib/gz.l), and each prints
;
;   LANE bytes-in bytes-out ms-per-rep
;
; ⚠ REPS is over the CODEC ONLY -- the read, the write and the boot sit outside the
; clock. one rep of the 9 MB corpus already dwarfs both, but a small input would not.
;
; what it said, on the seed's own source tar (love-0.1+g8942135a, 9735680 bytes), best
; of three, x86-64:
;
;                       bytes     of in     compress    decompress
;   love lzo1x-1      4264331    43.80%      1980 ms       1618 ms
;   love deflate      2960320    30.41%     23547 ms*      7339 ms**
;   love zstd         4368080    44.87%      6080 ms       7327 ms
;   love zstd, on a frame `zstd -19` wrote:
;                     2283124    23.45%          --       4679 ms
;   C    lzo1x-1      4432169    45.53%      19.1 ms       16.2 ms
;   C    gzip -1      3361368    34.53%        79 ms         30 ms
;   C    gzip -9      2849795    29.27%       661 ms         26 ms
;   C    zstd -1      3203240    32.90%        15 ms         10 ms
;   C    zstd -19     2283124    23.45%      3712 ms         12 ms
;
; ⚠ THE DEFLATE ROW MOVED, and it is the reason none of the others matter much. it
; read 3515716 / 36.11% / 25364 ms / 8823 ms while lib/gz.l wrote one fixed-Huffman
; block. giving it a code of the block's own took it to 2984599 for no measurable pack
; time and a FASTER unpack, there being fewer bits to walk; refusing three-byte matches
; past 4096 took it to 2960320. that is 3.9% behind `gzip -9` where it was 23.4%
; behind, and what is left of the RATIO is the greedy parse -- no lookahead, no lazy
; match.
;
; ⚠ * AND THE PACK TIME MOVED AGAIN, AFTER THE TABLE WAS TAKEN. gz-find has a second
; presentation now -- one holo IR program installed through lib/forge.l, an ablation
; having said the coder was 92% match search -- and it is 2.7x for the same bytes out.
; The starred cell is left at what it read because the rest of that row was measured
; with it; the new number is 8619 ms, and it is on a LATER tarball (9784832 -> 2976421,
; both lanes on that same input, twin 23221 and native 8619). So the pack is 13x off
; `gzip -9` where it was 35x, and LZO's lead on the pack is 4.4x where it was 11.9x --
; which narrows what this file concluded without changing it.
;
; ⚠ ** AND THE UNPACK MOVED FURTHER, AND OFF THIS FILE'S TERMS. host/inflate.c is a C
; twin of gz-puff, so the `gzd` lane no longer times love at all where the artifact
; carries the nif -- it times C, 20 ms under gcc and 37 under mooncc against gz-puff's
; 6294 and 7377 on the later tarball. That is FASTER THAN `gzip -d`, which is the first
; row in this table where love's stack beats the C program it is measured against, and
; it is why the deflate row is now the only interesting one: the pack is 13x off and the
; unpack is ahead. ⚠ every other DECOMPRESS number here is still love, so the column no
; longer compares like with like -- read the row, not the column.
;
; LZO still buys 4.4x on the pack and 4.5x on the unpack, and now costs 1304011 bytes
; -- the blob is 44.0% bigger with it. the LZO coder here lands 3.8% UNDER liblzo2's
; own, its scan differing slightly in where it skips.
;
; ⚠ THE ZSTD ROWS ARE TWO DIFFERENT CLAIMS. the DECODER is complete and reads whatever
; the reference writes, at every level -- so 23.45% is a real number for a blob some
; other zstd packed. the CODER is ours, and 44.87% is what love can pack for itself.
;
; ⚠ AND THE LITERALS ARE NOT WHERE THAT GAP LIVES. Huffman-coding them wins every one
; of the corpus's 149 blocks and still buys only 2.9%, because this coder's literals
; are 5.9% of what it emits (571954 bytes raw, 443859 coded) against 3923318 bytes of
; SEQUENCES. the match finder takes every four-byte match it trips over, and a
; four-byte match a megabyte back spends about twenty bits naming the distance to save
; thirty-two -- so most of the stream is matches that barely pay. refusing the ones
; that do not (a length floor that rises with the offset) measured 3773921, or 13.6%,
; against Huffman's 2.9%.
;
; ⚠ AND THE TARBALL IS NOT ONLY A BLOB. crew/build.mk hands the same bytes to the
; download and to mksrc, which is what makes a seed rebuilt elsewhere byte-identical
; rather than merely equivalent. A blob in any format but gzip breaks that identity,
; and changing both hands a stranger an archive their gunzip cannot open.
(use 'gz)

(: (bench-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) [1 (s + "")]) ()))
   (bench-blurt p s) (: q (open p "w") (? q (: _ (say q s) _ (close q) 1) ()))
   (bench-die m) (: _ (say err ("lzobench: " + m + "\n")) (quit 1))

   ; the codec, k times over, answering the last result. the loop is the measured thing,
   ; so nothing else may live inside it.
   (bench-run f s k) (: r (f s) (? (k <= 1) r (bench-run f s (k - 1))))

   (bench-lane v)
    (? (v = "lzoc") (\ s (lzo-compress s))
       (v = "lzod") (\ s (: r (lzo-decompress s 0) (? r (<(>r)) (bench-die "torn lzo"))))
       (v = "gzc")  (\ s (gz-deflate s))
       (v = "gzd")  (\ s (: r (gz-inflate s 0) (? r (<(>r)) (bench-die "torn deflate"))))
       (v = "zsc")  (\ s (zs-compress s))
       (v = "zsd")  (\ s (: r (zs-decompress s) (? r (<(>r)) (bench-die "torn zstd"))))
       (bench-die ("no lane " + v)))

   (bench v src dst k)
    (: f (bench-lane v)
       g (bench-slurp src)
       _ (? (two? g) 0 (bench-die ("cannot read " + src)))
       s <>g
       t0 (clock 0)
       res (bench-run f s k)
       ms ((clock 0) - t0)
       _ (? (bench-blurt dst res) 0 (bench-die ("cannot write " + dst)))
       (say out (v + "\t" + (show (tally s)) + "\t" + (show (tally res)) + "\t"
                 + (show (ms / k)) + "\n")))

   (bench-main av)
    (: n (tally av)
       (? (4 <= n) (bench (<av) (<(>av)) (<(>(>av)))
                          (? (5 <= n) (<(sound (<(>(>(>av)))))) 1))
          (bench-die "usage: lzobench LANE IN OUT [REPS]")))

   (bench-main >argv))
