; lib/gz.l -- gzip in love, both ways: the RFC 1952 container, the RFC 1951 DEFLATE
; stream inside it, and the crc32 underneath. `(use 'gz)`, then:
;
;   (gz-crc32 s)           -> the IEEE crc32 of a string, a charm
;   (gz-deflate s)         -> raw DEFLATE bytes
;   (gz-inflate s n)       -> (1 bytes) | ()    n a size hint, 0 when unknown
;   (gz-puff s n)          -> the same, in love, and the oracle the C twin answers to
;   (gz-zip s nom stamp)   -> a .gz member; nom "" for no name, stamp 0 for no time
;   (gz-unzip s)           -> (1 bytes) | ()
;
; ⚠ presence rides the (1 _) wrapper on every DECODE, and it has to: the empty string
; is a legitimate member and a torn one answers nothing too, so a bare value could
; never say which happened. test with two?, take with <(>r).
;
; the CODER is greedy LZ77 in blocks of sixteen thousand symbols, and each block is
; costed three ways and written the cheapest: stored, the fixed Huffman code, or a
; code built from the block's own frequencies. on the source tarball the artifact
; carries that is 9735680 -> 2960320, against `gzip -9`'s 2849795 and `gzip -1`'s
; 3361368. what it does NOT do is look ahead: the parse is greedy, one match taken
; where it is found, and that 3.9% is nearly all of what the lookahead would buy.
; the DECODER is complete for the same three, because the far side is not ours to
; choose: we must read whatever `gzip -9` wrote.
;
; ⚠ THE MATCH FINDER HAS A SECOND PRESENTATION -- gz-find's holo IR, installed through
; lib/forge.l on an input big enough to pay for loading it, and answering byte for byte
; what the twin answers or handing the call back. It is 2.7x on the whole coder, and it
; is here rather than in C for the reason the comment above it gives: `(s i)` is a VM
; apply and the compare wants an instruction. test/gate/gzfind.l is the differential.
;
; ⚠ AND SO DOES INFLATE, IN C. `gz-puff` below is the whole of RFC 1951 and the oracle;
; `gz-inflate` is a door that reaches for host/inflate.c's nif and falls back to it. That
; one is 205x, not 2.7x, and the difference is the ALGORITHM: the C indexes a table where
; puff walks a canonical code one bit at a time, which is the right trade at C's per-op
; cost and the wrong one at love's -- building the same table in love costs 44 ms on this
; tarball, more than the whole C decode. Which is also why it is not a holo kernel: the
; small kernel is the bit walk, and the bit walk is not what wins. test/host/gzc.l is the
; differential, and it fuzzes, because a corrupt block is where two decoders part.
;
; ⚠ THE THREE CONTAINERS ARE NOT INTERCHANGEABLE, and each is here for a reason a
; substitute could not serve: a `cask` is the mutable byte array (pin writes through),
; a `tray`'s pin answers a COPY so it holds only tables that never change, and a
; `tablet` is the mutable map the bit cursor and the Huffman decode tables ride. Reading
; a tray with peep is the fast path; reading a tablet spends a hash.
;
; ⚠ `+` IS DYADIC: every sum here is infix. A prefix (+ a b c) is APPLICATION, which
; church-exponentiates and answers a bignum the size of the sky (crew/tls/bytes.l pays
; the same warning).
(:
   ; --- crc32 (IEEE 802.3: reflected, polynomial 0xedb88320) -----------------
   ; built once at load into a tray, read with peep. the final xor is the same
   ; 0xffffffff the register starts at.
   (gz-crcrow n)
    (: (go k c) (? (8 <= k) c (go (k + 1) (? (c & 1) ((c >> 1) ^ 3988292384) (c >> 1))))
       (go 0 n))
   gz-crctab (star-tray '(256) (map gz-crcrow (jot 256)))
   (gz-crcwalk s)
    (: n (tally s)
       (go i c) (? (n <= i) (c ^ 4294967295)
                   (go (i + 1) ((peep gz-crctab ((c ^ (s i)) & 255) 0) ^ (c >> 8))))
       (go 0 4294967295))
   ; the door, gz-inflate's exactly: host/hash.c where the artifact carries it, the walk
   ; above where it does not -- love0, today. ⚠ THE FALLBACK IS NOT WHY THE WALK IS HERE:
   ; it is the statement of the polynomial and the oracle test/host/gzc.l holds the C to,
   ; and it would earn its place in a tree where every build carried every nif.
   ; ⚠ `charm?`, not `?`: the crc of "" is 0, a true answer a truth test calls absent.
   (gz-crc32 s)
    (? (lit? crc32) (: r (crc32 s) (? (charm? r) r (gz-crcwalk s))) (gz-crcwalk s))

   ; --- the bit sink ---------------------------------------------------------
   ; DEFLATE packs bits into bytes LSB-first, but a Huffman CODE is stored
   ; MSB-first, so a code is reversed on the way in and its extra bits are not.
   ; the cursor is a tablet: 0 the accumulator, 1 how many bits it holds.
   (gz-put j t v k)
    (: (go a n) (? (n < 8) (: _ (pin t 0 a) _ (pin t 1 n) 0)
                    (: _ (say j (string (a & 255))) (go (a >> 8) (n - 8))))
       (go ((peep t 0 0) | ((v & ((1 << k) - 1)) << (peep t 1 0))) ((peep t 1 0) + k)))
   (gz-align j t)                          ; flush the tail byte, zero-padded
    (? (peep t 1 0)
       (: _ (say j (string ((peep t 0 0) & 255))) _ (pin t 0 0) _ (pin t 1 0) 0)
       0)
   (gz-rev v k) (: (go i a w) (? (k <= i) a (go (i + 1) ((a << 1) | (w & 1)) (w >> 1))) (go 0 0 v))

   ; --- the length and distance ladders (RFC 1951 3.2.5) ---------------------
   gz-lbase (star-tray '(29) '(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59
                               67 83 99 115 131 163 195 227 258))
   gz-lext  (star-tray '(29) '(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
   gz-dbase (star-tray '(30) '(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
                               1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))
   gz-dext  (star-tray '(30) '(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))
   ; the ladders are short and walked from the top, so the first base at or below
   ; the value IS its code -- no search structure earns its keep at 30 rows.
   (gz-lcode v) (: (go i) (? (i < 1) 0 ((peep gz-lbase i 0) <= v) i (go (i - 1))) (go 28))
   (gz-dcode v) (: (go i) (? (i < 1) 0 ((peep gz-dbase i 0) <= v) i (go (i - 1))) (go 29))

   ; --- 32-bit cells in a cask ----------------------------------------------
   ; the match finder's two arrays hold POSITIONS, which outgrow a byte and outgrow
   ; 16 bits on any file over 64k. a cask is the only mutable array here, so a cell
   ; is four of its bytes.
   (gz-c32 c i) (: o (i * 4)
                   ((peep c o 0) + ((peep c (o + 1) 0) << 8)
                    + ((peep c (o + 2) 0) << 16) + ((peep c (o + 3) 0) << 24)))
   (gz-c32! c i v) (: o (i * 4)
                      _ (pin c o (v & 255))          _ (pin c (o + 1) ((v >> 8) & 255))
                      _ (pin c (o + 2) ((v >> 16) & 255)) _ (pin c (o + 3) ((v >> 24) & 255)) 0)

   ; --- a Huffman code, built from frequencies -------------------------------
   ; sort the used symbols by count, then merge the two lightest live nodes until one
   ; is left; a leaf's DEPTH is its code length. the two candidates come off two
   ; queues -- the sorted leaves, and the internal nodes the merge itself makes, which
   ; come out ascending for free -- so nothing is searched and nothing is re-sorted.
   ; a symbol rides its own count as one number, count:sym, so `sort` needs no
   ; comparator and the tie goes to the lower symbol.
   (gz-hkeys f i a) (? (i < 0) a
                       (: c (gz-c32 f i) (gz-hkeys f (i - 1) (? c (((c << 9) + i) >< a) a))))
   (gz-hcopy f nsym) (: c (cask (nsym * 4))
                        _ (: (go i) (? (nsym <= i) 0
                                       (: _ (gz-c32! c i (gz-c32 f i)) (go (i + 1))))
                             (go 0))
                        c)
   (gz-hdep pa i d) (: p (gz-c32 pa i) (? p (gz-hdep pa (p - 1) (d + 1)) d))
   ; ⚠ halving the counts and rebuilding is how a code is walked back under the depth
   ; the format allows -- crude, and it always terminates: all-ones is a balanced tree,
   ; nine deep over the literal alphabet and five over the code-length one.
   (gz-hhalve f nsym i) (? (nsym <= i) 0
                           (: c (gz-c32 f i)
                              _ (? c (gz-c32! f i (: h (c >> 1) (? h h 1))) 0)
                              (gz-hhalve f nsym (i + 1))))
   (gz-hpass f nsym lim pass)               ; -> (lengths deepest)
    (: ks (sort (gz-hkeys f (nsym - 1) ()))
       nl (tally ks)
       w (cask (nl * 8))   sy (cask (nl * 8))   pa (cask (nl * 8))
       _ (: (go i l) (? (nl <= i) 0
                        (: v <l
                           _ (gz-c32! w i (v >> 9))   _ (gz-c32! sy i (v & 511))
                           (go (i + 1) >l)))
            (go 0 ks))
       (pick li ii top) (? (nl <= li) 0     ; 1 takes the leaf queue, 0 the internal one
                           (top <= ii) 1
                           ((gz-c32 w li) <= (gz-c32 w ii)) 1
                           0)
       _ (: (go top li ii)
             (? ((nl + nl - 1) <= top) 0
                (: p (pick li ii top)
                   a (? p li ii)    li1 (? p (li + 1) li)    ii1 (? p ii (ii + 1))
                   q (pick li1 ii1 top)
                   b (? q li1 ii1)  li2 (? q (li1 + 1) li1)  ii2 (? q ii1 (ii1 + 1))
                   _ (gz-c32! w top ((gz-c32 w a) + (gz-c32 w b)))
                   _ (gz-c32! pa a (top + 1))   _ (gz-c32! pa b (top + 1))
                   (go (top + 1) li2 ii2)))
            (go nl 0 nl))
       lens (tablet 0)
       mx (: (go i m) (? (nl <= i) m
                         (: d (gz-hdep pa i 0)
                            _ (pin lens (gz-c32 sy i) d)
                            (go (i + 1) (? (m < d) d m))))
             (go 0 0))
       (? ((lim < mx) & (pass < 32)) (: _ (gz-hhalve f nsym 0) (gz-hpass f nsym lim (pass + 1)))
          `(lens mx)))
   ; ⚠ A CODE NEEDS TWO SYMBOLS even where the block used one or none: a lone symbol
   ; would want a zero-bit code, and the format cannot spell one. the two cheapest are
   ; conjured at count one, which is the shape zlib writes too.
   (gz-hlens f nsym lim)
    (: c (gz-hcopy f nsym)                  ; halving is destructive; the caller's
       u (: (go i k) (? (nsym <= i) k       ; counts are still owed to the cost sums
                        (go (i + 1) (? (gz-c32 c i) (k + 1) k)))
            (go 0 0))
       _ (? (2 <= u) 0
            (: _ (? (gz-c32 c 0) 0 (gz-c32! c 0 1))
               _ (? (gz-c32 c 1) 0 (gz-c32! c 1 1)) 0))
       (gz-hpass c nsym lim 0))
   ; canonical: shortest codes first, ties by symbol. the code goes in REVERSED, since
   ; a Huffman code is stored MSB-first and gz-put spends bits the other way.
   (gz-hcodes lens nsym mx)
    (: cnt (tablet 0)
       _ (: (go i) (? (nsym <= i) 0
                      (: l (peep lens i 0)
                         _ (? l (pin cnt l ((peep cnt l 0) + 1)) 0)
                         (go (i + 1))))
            (go 0))
       nxt (tablet 0)
       _ (: (go l c) (? (mx < l) 0
                        (: _ (pin nxt l c) (go (l + 1) ((c + (peep cnt l 0)) << 1))))
            (go 1 0))
       code (tablet 0)
       _ (: (go i) (? (nsym <= i) 0
                      (: l (peep lens i 0)
                         _ (? l (: c (peep nxt l 0)
                                   _ (pin code i (gz-rev c l))   _ (pin nxt l (c + 1)) 0)
                              0)
                         (go (i + 1))))
            (go 0))
       code)

   ; the fixed code (RFC 1951 3.2.6) is canonical over four runs of length, so it is
   ; those lengths handed to the same builder -- and the DECODER reads its tables off
   ; the same two rows.
   gz-fixlenl (star-tray '(288) (map (\ i (? (i < 144) 8 (i < 256) 9 (i < 280) 7 8))
                                     (jot 288)))
   gz-fixlend (star-tray '(30) (map (\ i 5) (jot 30)))
   gz-fixcodl (gz-hcodes gz-fixlenl 288 9)
   gz-fixcodd (gz-hcodes gz-fixlend 30 5)

   ; --- the match finder -----------------------------------------------------
   ; deflate's window is 32k and its match 258 bytes, both the format's numbers.
   ; `head` maps a 3-byte hash to the most recent position + 1 (0 being "none", so
   ; the empty cell needs no sentinel of its own); `prev` chains older positions,
   ; indexed by position & the window mask, which is why a chain walk must stop at
   ; a distance past the window -- past it the cell belongs to a newer position.
   gz-wsize 32768   gz-wmask 32767   gz-hmask 32767
   gz-maxmatch 258  gz-minmatch 3
   gz-chain 32      ; how far down a chain to look before taking what we have
   (gz-hash s i) (((((s i) << 10) ^ ((s (i + 1)) << 5)) ^ (s (i + 2))) & gz-hmask)
   (gz-mlen s n i k lim)                   ; the run shared from i and from k
    (: (go l) (? (lim <= l) l
                 (n <= (i + l)) l
                 ((s (i + l)) = (s (k + l))) (go (l + 1))
                 l)
       (go 0))
   (gz-find s n i head prev)                ; -> (len dist), len 0 for no match
    (: lim (? ((n - i) < gz-maxmatch) (n - i) gz-maxmatch)
       (go k d bl bd)
        (? (! k) `(bl bd)
           (! d) `(bl bd)
           (: p (k - 1) dist (i - p)
              (? (gz-wsize < dist) `(bl bd)
                 (: l (gz-mlen s n i p lim)
                    nx (gz-c32 prev (p & gz-wmask))
                    (? (bl < l) (? (lim <= l) `(l dist) (go nx (d - 1) l dist))
                                (go nx (d - 1) bl bd))))))
       (go (gz-c32 head (gz-hash s i)) gz-chain 0 0))
   ; --- the same finder, in machine code -------------------------------------
   ; ⚠ THE TWIN ABOVE IS THE DEFINITION. this is one holo IR program that says the same
   ; thing to a cpu, installed through lib/forge.l's `nif` seam; every guard in it jumps
   ; to `deopt`, which enters the twin with the arguments untouched. so a shape the IR
   ; does not say is not a bug -- it is the twin's turn.
   ;
   ; ⚠ AND THE REASON IS NOT "IT IS HOT", which lib/forge.l's own header says is not a
   ; reason. it is VOCABULARY: `(s i)` is a VM apply, and no compiler improvement hands
   ; love code an instruction the VM has no name for. the ablation, on core/love.c
   ; (461101 bytes), sweeping gz-chain to price the walk:
   ;
   ;                            twin    native
   ;   whole gz-deflate         1110       413 ms      2.7x, byte-identical output
   ;     the tokenizer          1034       335         so the finder went 750 -> 50
   ;     the Huffman half         88        88         sort, merge, bit sink, all of it
   ;   chain 1 (tokenizer)       463        --         so steps 2..32 are 571 of the 1034
   ;
   ; and on the source tarball the artifact carries, 9784832 bytes: 23221 -> 8619 ms, the
   ; same 2.7x, for the same 2976421 bytes out. `gzip -9` writes 2866530 in 661 ms, so the
   ; gap to C goes from 35x to 13x and what is left is no longer the search.
   ; ⚠ THE OTHER AXIS forge names -- a constant known only at RUN TIME -- this does NOT
   ; have: gz-chain and the two masks are baked as immediates because they are already
   ; constants, and a C nif could bake them too. What a C nif could not do is be written
   ; in the seed's own source, which is the whole reason to reach for holo and not for C.
   ;
   ; regs (after the callee-saved file): r11=ai (the bytes of s, at i) r13=lim r14=bl
   ; r12=bd r3=k r10=d r9=l r8=pbp, r0/r7 scratch; [sp+0]=i+1 [sp+8]=prev's bytes.
   ; ⚠ the guards are wider than the twin's domain because the twin READS THROUGH LOVE:
   ; `(s i)` bounds-checks and a cask index is checked, so a torn `n` or a hand-built
   ; window array is merely a wrong answer there and is a wild read here. so the kernel
   ; proves what it is about to trust -- n within the string, the two casks window-sized,
   ; i+3 within n (the hash reads three bytes) -- and hands back everything else.
   ; ⚠ ld4 reads a cask cell in one load, which is the little-endian layout gz-c32
   ; writes; every holo backend is little-endian. ⚠ AND IT SIGN-EXTENDS, where gz-c32's
   ; four shifted bytes do not -- so `zx4` follows every one of them. Without it a cell
   ; with the top bit set reads as a negative k, which this walks off as "no chain" and
   ; the twin walks into a wild position: two different answers from one cask.
   (gz-find-ir _)
    (: guard (from 'forge 'forge-guard)     fixg (from 'forge 'forge-fixg)
       room  (from 'forge 'forge-room)      save (from 'forge 'forge-save)
       restore (from 'forge 'forge-restore) epi  (from 'forge 'forge-epi-ptr)
       strk (apof "ab")   caskk (apof (cask 1))   chaink (apof '(1 2))
       ; the extra two slots ride above the callee-saved file, so they come off first
       unwind ('(label deoptp) >< ('(pop r0) >< '(pop r0) >< restore + '((jmp deopt))))
       (guard 0 strk + guard 3 caskk + guard 4 caskk + fixg 1 + fixg 2
       + [ ; both windows must be the format's size: the kernel indexes them by a mask
           '(ld r0 r1 24) '(ld r0 r0 8) '(ld r0 r0 8)
           ['li 'r8 (4 * gz-wsize)] '(cmp r0 r8) '(br lt deopt)
           '(ld r0 r1 32) '(ld r0 r0 8) '(ld r0 r0 8)
           ['li 'r8 (4 * gz-wsize)] '(cmp r0 r8) '(br lt deopt)
           '(ld r0 r1 8) '(sar r0 1)                          ; n
           '(ld r7 r1 0) '(ld r7 r7 8) '(cmp r0 r7) '(br gt deopt)   ; n <= tally s
           '(li r8 2147483647) '(cmp r0 r8) '(br gt deopt)    ; and inside a 4-byte cell
           '(ld r7 r1 16) '(sar r7 1) '(cmp r7 0) '(br lt deopt)     ; 0 <= i
           '(mov r8 r7) '(add r8 r8 3) '(cmp r8 r0) '(br gt deopt)]  ; i + 3 <= n
       + room 96                                              ; two chain cells and slack
       + save
       + [ '(ld r11 r1 0) '(lea r11 r11 16)
           '(ld r7 r1 16) '(sar r7 1) '(add r11 r11 r7)       ; ai = s bytes + i
           '(ld r0 r1 8) '(sar r0 1) '(sub r0 r0 r7)          ; n - i
           ['li 'r8 gz-maxmatch] '(cmp r0 r8) '(br le lok) '(mov r0 r8)
           '(label lok) '(mov r13 r0)                         ; lim
           '(ld r0 r1 32) '(ld r0 r0 8) '(lea r0 r0 16) '(push r0)   ; prev's bytes
           '(mov r0 r7) '(add r0 r0 1) '(push r0)             ; i + 1
           '(ld1 r0 r11 0) '(zx1 r0) '(shl r0 10)             ; the three-byte hash
           '(ld1 r7 r11 1) '(zx1 r7) '(shl r7 5) '(xor r0 r0 r7)
           '(ld1 r7 r11 2) '(zx1 r7) '(xor r0 r0 r7)
           ['li 'r7 gz-hmask] '(and r0 r0 r7)
           '(ld r7 r1 24) '(ld r7 r7 8) '(lea r7 r7 16)
           '(leax r7 r7 r0 4 0) '(ld4 r3 r7 0) '(zx4 r3)      ; k = head[h]
           ['li 'r10 gz-chain] '(li r14 0) '(li r12 0)        ; d, bl, bd
           ; the chain walk. `k <= 0` is the twin's (! k) exactly, and it is also what
           ; keeps p non-negative, so every byte the compare below reads is inside s.
           '(label cl)
           '(cmp r3 1) '(br lt cend)
           '(cmp r10 0) '(br le cend)
           '(ld r0 sp 0) '(sub r0 r0 r3)                      ; dist = (i + 1) - k
           ['li 'r7 gz-wsize] '(cmp r0 r7) '(br gt cend)
           ; a cask holding a position AHEAD of i is not this window's -- the twin reads
           ; it through love's own bounds check and this cannot, so it goes to the twin
           '(cmp r0 1) '(br lt deoptp)
           '(mov r8 r11) '(sub r8 r8 r0)                      ; the other cursor
           '(li r9 0)
           '(label ml) '(cmp r9 r13) '(br ge mle)
           '(ldxb r0 r11 r9 1 0) '(ldxb r7 r8 r9 1 0)
           '(cmp r0 r7) '(br ne mle)
           '(add r9 r9 1) '(jmp ml)
           '(label mle)                                       ; l
           '(mov r0 r3) '(sub r0 r0 1) ['li 'r7 gz-wmask] '(and r0 r0 r7)
           '(ld r7 sp 8) '(leax r7 r7 r0 4 0) '(ld4 r0 r7 0) '(zx4 r0)   ; the next link
           '(cmp r14 r9) '(br ge cnext)
           '(mov r14 r9) '(mov r12 r11) '(sub r12 r12 r8)     ; a longer run: keep it
           '(cmp r9 r13) '(br ge cend)                        ; and lim is all there is
           '(label cnext)
           '(mov r3 r0) '(sub r10 r10 1) '(jmp cl)
           '(label cend) '(pop r0) '(pop r0)
           ; the answer is (bl bd), two chain cells; the zero point comes off the
           ; extras slot, being the one address no book name hands out
           '(mov r0 r2) '(add r2 r2 48) '(mov r7 r0) '(add r7 r7 24)
           ['li 'r8 chaink] '(st r0 0 r8) '(st r7 0 r8)
           '(mov r9 r14) '(shl r9 1) '(or r9 r9 1) '(st r0 8 r9)
           '(st r0 16 r7)
           '(mov r9 r12) '(shl r9 1) '(or r9 r9 1) '(st r7 8 r9)
           '(ld r9 r5 32) '(st r7 16 r9)]
       + restore + epi + unwind))

   ; the finder in force, resolved ONCE per deflate and cached for the process.
   ; ⚠ LAZY, and gated on size, because `use 'forge` costs 24 ms of parse and eval and a
   ; tar member is usually a few hundred bytes -- below the floor the twin is simply the
   ; better program. ⚠ and TRAPPED: a tree without lib/forge.l, a build with no glaze
   ; module, a cpu with no holo backend all answer the twin, which is the whole design.
   gz-floor 65536
   gz-forged (tablet 0)
   (gz-finder n)
    (? (n < gz-floor) gz-find
       (peep gz-forged 0 ()) (peep gz-forged 0 ())
       (: v (trap (\ _ (: _ (use 'forge)
                          putx (from 'forge 'forge-putx)
                          fit  (from 'forge 'forge-fit)
                          of   (from 'forge 'forge-of)
                          _ (? (lit? putx) (: _ (putx 'gz-find 5 gz-find-ir gz-find ())
                                                _ (fit 0) 0)
                               0)
                          w (? (lit? of) (of 'gz-find) gz-find)
                          (? (lit? w) w gz-find)))
                  () (\ a b gz-find))
          _ (pin gz-forged 0 v) v))

   (gz-ins s n i head prev)                 ; index position i, if 3 bytes remain
    (? ((i + 2) < n)
       (: h (gz-hash s i)
          _ (gz-c32! prev (i & gz-wmask) (gz-c32 head h))
          _ (gz-c32! head h (i + 1)) 0)
       0)
   (gz-fill s n head prev i l)              ; index every position a match covered
    (: (go k) (? (l <= k) 0 (: _ (gz-ins s n (i + k) head prev) (go (k + 1)))) (go 0))

   ; --- the token buffer -----------------------------------------------------
   ; a block is TOKENIZED first and coded second, because a code of the block's own
   ; cannot be built until the block's frequencies are known. a literal is its own
   ; byte; a match packs the two codes and the two extra values that spell it, so the
   ; ladders are walked here and not again at write time.
   gz-tokmax 16384                          ; zlib's block, measured in symbols
   ; a three-byte match spends a length code and a distance code -- thirteen bits and
   ; up, against the fifteen or so three literals cost -- so past some reach it is a
   ; LOSS, and a greedy parse takes it anyway and blocks the longer match one byte on.
   ; refusing them past 4096 pays everywhere measured: the source tarball 2984599 ->
   ; 2960320, this tree's .l files 32094 -> 31862, out/host/love 1225045 -> 1216277.
   ; ⚠ AND REFUSING THEM OUTRIGHT DOES NOT. text likes it (the tarball goes on to
   ; 2949504) and binaries do not -- out/host/love swells to 1275300, 4.6% worse.
   gz-toofar 4096
   ; ⚠ `fn` IS THE FINDER, handed down rather than read: gz-find above or the machine
   ; code that says the same thing, and which one is settled once per deflate.
   (gz-tok fn s n head prev tok fl fd i k xb)  ; -> (i tokens extra-bits)
    (? (n <= i) `(i k xb)
       (gz-tokmax <= k) `(i k xb)
       (: m (? ((i + gz-minmatch) <= n) (fn s n i head prev) '(0 0))
          l (? (<m = 3) (? (gz-toofar < <>m) 0 <m) <m)
          (? (l < gz-minmatch)
             (: c (s i)
                _ (gz-c32! tok k c)
                _ (gz-c32! fl c ((gz-c32 fl c) + 1))
                _ (gz-ins s n i head prev)
                (gz-tok fn s n head prev tok fl fd (i + 1) (k + 1) xb))
             (: d (<(>m))
                lc (gz-lcode l)   dc (gz-dcode d)
                le (peep gz-lext lc 0)   de (peep gz-dext dc 0)
                _ (gz-c32! tok k (256 + (lc << 23) + (dc << 18)
                                  + ((l - (peep gz-lbase lc 0)) << 13)
                                  + (d - (peep gz-dbase dc 0))))
                _ (gz-c32! fl (257 + lc) ((gz-c32 fl (257 + lc)) + 1))
                _ (gz-c32! fd dc ((gz-c32 fd dc) + 1))
                _ (gz-fill s n head prev i l)
                (gz-tok fn s n head prev tok fl fd (i + l) (k + 1) (xb + le + de))))))

   ; --- the block's own code, written out as code LENGTHS ---------------------
   ; the lengths go through a THIRD Huffman code, run-length coded first: 16 repeats
   ; the previous length three to six times, 17 and 18 are runs of zero, three to ten
   ; and eleven to a hundred and thirty-eight. a value is always spelled once before
   ; 16 may repeat it.
   ; ⚠ THE ORDER IS THE FORMAT'S, not ours: the code-length code declares its own
   ; lengths in this permutation so the rarely-used ones sit at the tail and can be
   ; trimmed off. both sides read the same row.
   gz-clord (star-tray '(19) '(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))
   (gz-clx sy) (? (sy = 16) 2 (sy = 17) 3 (sy = 18) 7 0)
   (gz-trim lens k lo) (? (k <= lo) lo (peep lens (k - 1) 0) k (gz-trim lens (k - 1) lo))
   (gz-run cl tot rl)                       ; -> how many entries rl holds
    (: (span v k) (? (tot <= k) k ((peep cl k 0) = v) (span v (k + 1)) k)
       (put j sy ex) (: _ (gz-c32! rl j ((sy << 7) + ex)) (j + 1))
       (zeros i r j) (? (r < 3) (? r (zeros (i + 1) (r - 1) (put j 0 0)) `(i j))
                          (r < 11) (zeros (i + r) 0 (put j 17 (r - 3)))
                          (: t (? (138 < r) 138 r) (zeros (i + t) (r - t) (put j 18 (t - 11)))))
       (reps i r v j) (? (r < 3) (? r (reps (i + 1) (r - 1) v (put j v 0)) `(i j))
                          (: t (? (6 < r) 6 r) (reps (i + t) (r - t) v (put j 16 (t - 3)))))
       (go i j) (? (tot <= i) j
                   (: v (peep cl i 0)
                      r ((span v i) - i)
                      p (? v (reps (i + 1) (r - 1) v (put j v 0)) (zeros i r j))
                      (go <p <>p)))
       (go 0 0))

   ; --- what a block would cost, in BITS -------------------------------------
   (gz-cost f lens nsym i a)
    (? (nsym <= i) a
       (gz-cost f lens nsym (i + 1) (a + ((gz-c32 f i) * (peep lens i 0)))))
   (gz-ccost rl lenc nr i a)
    (? (nr <= i) a
       (: sy ((gz-c32 rl i) >> 7)
          (gz-ccost rl lenc nr (i + 1) (a + (peep lenc sy 0) + (gz-clx sy)))))

   (gz-wtoks j t tok k codl lenl codd lend i)
    (? (k <= i) (gz-put j t (peep codl 256 0) (peep lenl 256 0))   ; 256 ends the block
       (: v (gz-c32 tok i)
          (? (v < 256)
             (: _ (gz-put j t (peep codl v 0) (peep lenl v 0))
                (gz-wtoks j t tok k codl lenl codd lend (i + 1)))
             (: u (v - 256)   lc (u >> 23)   dc ((u >> 18) & 31)
                _ (gz-put j t (peep codl (257 + lc) 0) (peep lenl (257 + lc) 0))
                _ (: e (peep gz-lext lc 0) (? e (gz-put j t ((u >> 13) & 31) e) 0))
                _ (gz-put j t (peep codd dc 0) (peep lend dc 0))
                _ (: e (peep gz-dext dc 0) (? e (gz-put j t (u & 8191) e) 0))
                (gz-wtoks j t tok k codl lenl codd lend (i + 1))))))
   (gz-wstored j t s i0 i1 last)
    (: len (i1 - i0)
       _ (gz-put j t (? last 1 0) 1)   _ (gz-put j t 0 2)
       _ (gz-align j t)                     ; a stored block starts at a byte
       _ (gz-put j t (len & 255) 8)          _ (gz-put j t ((len >> 8) & 255) 8)
       _ (gz-put j t ((len ^ 65535) & 255) 8)
       _ (gz-put j t (((len ^ 65535) >> 8) & 255) 8)
       (: (go k) (? (len <= k) 0 (: _ (gz-put j t (s (i0 + k)) 8) (go (k + 1)))) (go 0)))

   ; three ways to spell one block, and the cheapest in bits wins. the tree the third
   ; must declare ahead of itself is the only reason the other two still win anything:
   ; a block of noise pays it for nothing, and a short one cannot amortize it.
   (gz-flush s j t tok k fl fd i0 i1 last xb)
    (: _ (gz-c32! fl 256 ((gz-c32 fl 256) + 1))          ; end-of-block, always sent
       rl (gz-hlens fl 286 15)   lenl <rl   mxl <>rl
       rd (gz-hlens fd 30 15)    lend <rd   mxd <>rd
       hlit (gz-trim lenl 286 257)   hdist (gz-trim lend 30 1)
       cl (tablet 0)                        ; the two length runs, laid end to end
       _ (: (go i) (? (hlit <= i) 0 (: _ (pin cl i (peep lenl i 0)) (go (i + 1)))) (go 0))
       _ (: (go i) (? (hdist <= i) 0
                      (: _ (pin cl (hlit + i) (peep lend i 0)) (go (i + 1))))
            (go 0))
       rlb (cask (320 * 4))
       nr (gz-run cl (hlit + hdist) rlb)
       fc (cask (19 * 4))
       _ (: (go i) (? (nr <= i) 0
                      (: sy ((gz-c32 rlb i) >> 7)
                         _ (gz-c32! fc sy ((gz-c32 fc sy) + 1))
                         (go (i + 1))))
            (go 0))
       rc (gz-hlens fc 19 7)   lenc <rc   mxc <>rc
       hclen (: (go i) (? (i <= 4) 4 (peep lenc (peep gz-clord (i - 1) 0) 0) i (go (i - 1)))
                (go 19))
       dyn (17 + (hclen * 3) + (gz-ccost rlb lenc nr 0 0)
            + (gz-cost fl lenl 286 0 0) + (gz-cost fd lend 30 0 0) + xb)
       fix (3 + (gz-cost fl gz-fixlenl 286 0 0) + (gz-cost fd gz-fixlend 30 0 0) + xb)
       ; LEN is sixteen bits, so a span past 65535 has no stored spelling at all.
       raw (? ((i1 - i0) < 65536) (42 + ((i1 - i0) * 8)) (dyn + fix))
       (? (raw < (? (dyn <= fix) dyn fix)) (gz-wstored j t s i0 i1 last)
          (fix < dyn)
           (: _ (gz-put j t (? last 1 0) 1)   _ (gz-put j t 1 2)
              (gz-wtoks j t tok k gz-fixcodl gz-fixlenl gz-fixcodd gz-fixlend 0))
          (: codl (gz-hcodes lenl 286 mxl)   codd (gz-hcodes lend 30 mxd)
             codc (gz-hcodes lenc 19 mxc)
             _ (gz-put j t (? last 1 0) 1)   _ (gz-put j t 2 2)
             _ (gz-put j t (hlit - 257) 5)   _ (gz-put j t (hdist - 1) 5)
             _ (gz-put j t (hclen - 4) 4)
             _ (: (go i) (? (hclen <= i) 0
                            (: _ (gz-put j t (peep lenc (peep gz-clord i 0) 0) 3)
                               (go (i + 1))))
                  (go 0))
             _ (: (go i) (? (nr <= i) 0
                            (: v (gz-c32 rlb i)   sy (v >> 7)
                               _ (gz-put j t (peep codc sy 0) (peep lenc sy 0))
                               _ (: b (gz-clx sy) (? b (gz-put j t (v & 127) b) 0))
                               (go (i + 1))))
                  (go 0))
             (gz-wtoks j t tok k codl lenl codd lend 0))))

   ; the window and the chains outlive the block: a match may reach back into any
   ; block already written, which is what makes many blocks cost only their trees.
   (gz-blocks fn s n j t head prev i)
    (: tok (cask (gz-tokmax * 4))
       fl (cask (286 * 4))   fd (cask (30 * 4))
       r (gz-tok fn s n head prev tok fl fd i 0 0)
       i2 <r   k <>r   xb <>>r
       _ (gz-flush s j t tok k fl fd i i2 (? (n <= i2) 1 0) xb)
       (? (n <= i2) 0 (gz-blocks fn s n j t head prev i2)))

   (gz-deflate s)
    (: n (tally s)
       j (jug 0)   t (tablet 0)
       head (cask (32768 * 4))   prev (cask (32768 * 4))
       _ (gz-blocks (gz-finder n) s n j t head prev 0)
       _ (gz-align j t)
       ((slurp j) + ""))

   ; --- the growing byte buffer ---------------------------------------------
   ; inflate writes bytes AND reads them back (a match copies out of what it has
   ; already produced), so its sink cannot be a jug -- a jug pours out, it does not
   ; index. a cask that doubles, then one exact copy at the end.
   (gz-buf n) (: b (tablet 0) _ (pin b 0 (cask (? (n < 64) 64 n))) _ (pin b 1 0) b)
   (gz-room b k)
    (: c (peep b 0 0) cap (tally c) len (peep b 1 0)
       (? ((len + k) <= cap) 0
          (: big (: (go m) (? ((len + k) <= m) m (go (m * 2))) (go cap))
             c2 (cask big)
             _ (? len (pour c2 0 (string c) 0 len) 0)
             _ (pin b 0 c2) 0)))
   ; ⚠ THE ROOM CHECK IS INLINE, and that is not tidiness: `gz-room` reads the cask
   ; and its length again for a test the pushing path has already made, and this
   ; runs once per OUTPUT BYTE -- nine and a half million of them for the source
   ; the artifact carries.
   (gz-push b v)
    (: c (peep b 0 0) len (peep b 1 0)
       (? (len < (tally c)) (: _ (pin c len v) _ (pin b 1 (len + 1)) 0)
          (: _ (gz-room b 1) c2 (peep b 0 0)
             _ (pin c2 len v) _ (pin b 1 (len + 1)) 0)))
   (gz-take b)
    (: c (peep b 0 0) len (peep b 1 0)
       (? len (: c2 (cask len) _ (pour c2 0 (string c) 0 len) (string c2)) ""))
   ; a match copies BYTE BY BYTE on purpose: deflate lets a run overlap its own
   ; source (dist 1, len 100 is a hundred of one byte), so a bulk move would be wrong.
   ; ⚠ 1 for the clean end, never 0 -- the caller asks `(! r)`, and 0 is nothing.
   ; ⚠ the ROOM IS TAKEN ONCE, before the loop, so the cask cannot be replaced
   ; under it and the walk holds `c` and the write cursor in hand -- two tablet
   ; reads for the whole match instead of nine per byte. the copy stays byte by
   ; byte: deflate lets a run overlap its own source and a bulk move would be wrong.
   (gz-copy b dist len)
    (: _ (gz-room b len)
       c (peep b 0 0)
       (go k p) (? (len <= k) (: _ (pin b 1 p) 1)
                   (p < dist) (: _ (pin b 1 p) ())         ; a reach before the start
                   (: _ (pin c p (peep c (p - dist) 0)) (go (k + 1) (p + 1))))
       (go 0 (peep b 1 0)))

   ; --- the bit source -------------------------------------------------------
   ; the cursor is a tablet again: 0 the bit position, 1 the torn flag. reading past
   ; the end sets torn and answers 0 rather than scaring, so a torn stream unwinds
   ; through the ordinary return path and the caller reads presence at the end.
   (gz-bit s n t)
    (: p (peep t 0 0)
       (? (n <= (p >> 3)) (: _ (pin t 1 1) 0)
          (: _ (pin t 0 (p + 1)) (((s (p >> 3)) >> (p & 7)) & 1))))
   ; ⚠ THE POSITION RIDES A LOCAL, read once and written back once. `gz-bit` pays a
   ; peep and a pin PER BIT because the cursor is a tablet, and a deflate stream is
   ; twenty-seven million bits -- so the cursor is taken in hand for the length of a
   ; run and put back at the end. the semantics are gz-bit's exactly, torn flag and
   ; all: past the end sets torn, answers a 0 bit, and does NOT advance.
   (gz-bits s n t k)
    (: (go i a p)
        (? (k <= i) (: _ (pin t 0 p) a)
           (: over (n <= (p >> 3))
              _ (? over (pin t 1 1) 0)
              (go (i + 1) (? over a (a | ((((s (p >> 3)) >> (p & 7)) & 1) << i)))
                  (? over p (p + 1)))))
       (go 0 0 (peep t 0 0)))

   ; --- canonical Huffman, built from code lengths --------------------------
   ; the shape is puff's: a count of codes per length and the symbols in canonical
   ; order. decoding walks one bit at a time -- no lookup table, which is slower per
   ; symbol and is the whole of the algorithm rather than an index over it.
   (gz-huff lens nsym)
    (: cnt (tablet 0)
       _ (: (go i) (? (nsym <= i) 0
                       (: l (peep lens i 0) _ (pin cnt l ((peep cnt l 0) + 1)) (go (i + 1))))
            (go 0))
       _ (pin cnt 0 0)                       ; a zero length is "no code", not a code
       ofs (tablet 0)
       _ (: (go l a) (? (15 < l) 0 (: _ (pin ofs l a) (go (l + 1) (a + (peep cnt l 0)))))
            (go 1 0))
       sym (tablet 0)
       _ (: (go i) (? (nsym <= i) 0
                       (: l (peep lens i 0)
                          _ (? l (: o (peep ofs l 0) _ (pin sym o i) _ (pin ofs l (o + 1)) 0) 0)
                          (go (i + 1))))
            (go 0))
       `(cnt sym))
   ; ..and the decode walks up to fifteen bits for ONE symbol, which is where the
   ; per-bit cursor cost lands hardest -- so it takes the position in hand too, and
   ; writes it back on every way out, the failing one included.
   (gz-dec s n t h)                          ; -> (1 symbol) | ()
    (: cnt <h sym (<(>h))
       (go l code first index p)
        (? (15 < l) (: _ (pin t 0 p) ())
           (: over (n <= (p >> 3))
              _ (? over (pin t 1 1) 0)
              c (? over code (code | (((s (p >> 3)) >> (p & 7)) & 1)))
              p2 (? over p (p + 1))
              k (peep cnt l 0)
              (? ((c - k) < first) (: _ (pin t 0 p2) `(1 (peep sym (index + (c - first)) 0)))
                 (go (l + 1) (c << 1) ((first + k) << 1) (index + k) p2))))
       (go 1 0 0 0 (peep t 0 0)))

   gz-fixlithuff (gz-huff gz-fixlenl 288)
   gz-fixdisthuff (gz-huff gz-fixlend 30)

   ; --- the three block kinds ------------------------------------------------
   (gz-stored s n t b)
    (: p (// ((peep t 0 0) + 7) 8)            ; a stored block starts at a byte
       (? (n < (p + 4)) ()
          (: len ((s p) + ((s (p + 1)) << 8))
             nl ((s (p + 2)) + ((s (p + 3)) << 8))
             (? ((((len ^ 65535) & 65535) = nl) & 1)
                (? (n < (p + 4 + len)) ()
                   (: (go k) (? (len <= k) 1 (: _ (gz-push b (s (p + 4 + k))) (go (k + 1))))
                      _ (go 0)
                      _ (pin t 0 ((p + 4 + len) * 8))
                      1))
                ()))))

   (gz-codes s n t b lit dst)                ; the symbol loop, fixed or dynamic
    (: (go u)
        (: d (gz-dec s n t lit)
           (? (! d) ()
              (peep t 1 0) ()                                  ; ran off the end
              (: sy (<(>d))
                 (? (sy < 256) (: _ (gz-push b sy) (go ()))
                    (sy = 256) 1
                    (285 < sy) ()
                    (: lc (sy - 257)
                       l ((peep gz-lbase lc 0) + (gz-bits s n t (peep gz-lext lc 0)))
                       dd (gz-dec s n t dst)
                       (? (! dd) ()
                          (: dc (<(>dd))
                             (? (29 < dc) ()
                                (: dist ((peep gz-dbase dc 0) + (gz-bits s n t (peep gz-dext dc 0)))
                                   r (gz-copy b dist l)
                                   (? (! r) () (go ())))))))))))
       (go ()))

   ; the dynamic block's own code, read as code LENGTHS through a third Huffman code
   ; -- the one place the format describes itself. 16/17/18 are runs, and 16 repeats
   ; the previous length, so the walk carries it.
   (gz-dynamic s n t b)
    (: hlit ((gz-bits s n t 5) + 257)
       hdist ((gz-bits s n t 5) + 1)
       hclen ((gz-bits s n t 4) + 4)
       cl (tablet 0)
       _ (: (go i) (? (hclen <= i) 0
                      (: _ (pin cl (peep gz-clord i 0) (gz-bits s n t 3)) (go (i + 1))))
            (go 0))
       ch (gz-huff cl 19)
       tot (hlit + hdist)
       lens (tablet 0)
       (rep i r v) (: (go k) (? (r <= k) 0 (: _ (pin lens (i + k) v) (go (k + 1)))) (go 0))
       (rd i prev)
        (? (tot <= i) 1
           (peep t 1 0) ()
           (: d (gz-dec s n t ch)
              (? (! d) ()
                 (: sy (<(>d))
                    (? (sy < 16) (: _ (pin lens i sy) (rd (i + 1) sy))
                       (sy = 16) (: r (3 + (gz-bits s n t 2)) _ (rep i r prev) (rd (i + r) prev))
                       (sy = 17) (: r (3 + (gz-bits s n t 3)) _ (rep i r 0) (rd (i + r) 0))
                       (sy = 18) (: r (11 + (gz-bits s n t 7)) _ (rep i r 0) (rd (i + r) 0))
                       ())))))
       ok (rd 0 0)
       (? (! ok) ()
          ; the distance lengths sit AFTER the literal ones in the same run, and
          ; gz-huff counts from 0, so they are carried down into a table of their own.
          (: dl (tablet 0)
             _ (: (go k) (? (hdist <= k) 0
                            (: _ (pin dl k (peep lens (hlit + k) 0)) (go (k + 1))))
                  (go 0))
             lit (gz-huff lens hlit)
             dst (gz-huff dl hdist)
             (gz-codes s n t b lit dst))))

   (gz-block s n t b)                         ; -> (1 last?) | ()
    (: fin (gz-bit s n t)
       typ (gz-bits s n t 2)
       r (? (typ = 0) (gz-stored s n t b)
            (typ = 1) (gz-codes s n t b gz-fixlithuff gz-fixdisthuff)
            (typ = 2) (gz-dynamic s n t b)
            ())
       (? r `(1 fin) ()))

   (gz-puff s hint)
    (: n (tally s)
       t (tablet 0)   b (gz-buf hint)
       (go u) (: r (gz-block s n t b)
                 (? (! r) ()
                    (peep t 1 0) ()
                    (<(>r)) `(1 (gz-take b))
                    (go ())))
       (? n (go ()) ()))
   ; the door. host/inflate.c where the artifact carries it, gz-puff where it does not
   ; and wherever it declines -- and a TORN STREAM DECLINES, so a torn one is read twice
   ; and answers () both times. ⚠ `string?`, never `?`: a string of NULs sums to zero and
   ; would read as absent.
   (gz-inflate s hint)
    (? (lit? inflate) (: r (inflate s hint) (? (string? r) `(1 r) (gz-puff s hint)))
       (gz-puff s hint))

   ; --- the container (RFC 1952) --------------------------------------------
   (gz-le32s v)
    (: b (cask 4)
       _ (pin b 0 (v & 255))          _ (pin b 1 ((v >> 8) & 255))
       _ (pin b 2 ((v >> 16) & 255))  _ (pin b 3 ((v >> 24) & 255))
       (string b))
   (gz-zip s nom stamp)
    (: txt (s + "")
       named (? (tally nom) 1 0)
       h (cask 10)
       _ (pin h 0 31) _ (pin h 1 139) _ (pin h 2 8) _ (pin h 3 (? named 8 0))
       _ (pin h 4 (stamp & 255))        _ (pin h 5 ((stamp >> 8) & 255))
       _ (pin h 6 ((stamp >> 16) & 255)) _ (pin h 7 ((stamp >> 24) & 255))
       _ (pin h 8 0) _ (pin h 9 3)               ; XFL none, OS 3 = unix
       ((string h) + (? named (nom + (string 0)) "") + (gz-deflate txt)
        + (gz-le32s (gz-crc32 txt)) + (gz-le32s ((tally txt) & 4294967295))))

   ; the header's optional fields are four flags, and every one of them must be
   ; STEPPED OVER even though we write none of them -- the far side is gzip, and
   ; `gzip -N` writes a name. FTEXT (bit 0) carries no bytes.
   (gz-skip s n i)                            ; step a NUL-ended field
    (? (n <= i) () ((s i) = 0) (i + 1) (gz-skip s n (i + 1)))
   (gz-unzip s)
    (: n (tally s)
       (? (n < 18) ()
          (! (((s 0) = 31) & ((s 1) = 139) & ((s 2) = 8)))  ()
          (: flg (s 3)
             i0 10
             i1 (? (flg & 4) (: x ((s i0) + ((s (i0 + 1)) << 8)) (i0 + 2 + x)) i0)   ; FEXTRA
             i2 (? (flg & 8) (gz-skip s n i1) i1)                                  ; FNAME
             i3 (? (! i2) () (flg & 16) (gz-skip s n i2) i2)                        ; FCOMMENT
             i4 (? (! i3) () (flg & 2) (i3 + 2) i3)                                   ; FHCRC
             (? (! i4) ()
                (n < (i4 + 8)) ()
                (: isize ((s (n - 4)) + ((s (n - 3)) << 8)
                          + ((s (n - 2)) << 16) + ((s (n - 1)) << 24))
                   crc ((s (n - 8)) + ((s (n - 7)) << 8)
                        + ((s (n - 6)) << 16) + ((s (n - 5)) << 24))
                   ; ⚠ `pour`'s last argument is a COUNT, not an end offset, and the nif
                   ; clamps it to the shorter of the two arrays -- so an end offset can
                   ; answer right off the destination's size alone, silently, and move
                   ; the whole tail to do it. spell the count.
                   body (: c (cask (n - i4 - 8)) _ (pour c 0 s i4 (n - i4 - 8)) (string c))
                   r (gz-inflate body isize)
                   (? (! r) ()
                      (: out (<(>r))
                         ; the trailer is the whole point of the container: a stream
                         ; that inflated cleanly and hashes wrong is a CORRUPT one,
                         ; and saying so is the only thing the crc is for.
                         (? (! ((gz-crc32 out) = crc)) ()
                            (! ((tally out) = (isize & 4294967295))) ()
                            `(1 out))))))))))
