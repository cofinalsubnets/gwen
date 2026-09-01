; bench/ccdead.l -- how much of each lane's libc the binary can never reach.
;
;   love bench/ccdead.l            (after ./ccbench.sh, or `make ccbench`)
;   love bench/ccdead.l ELF ..     (⚠ no own-object split, so every text symbol
;                                        reads as libc -- see the caveat below)
;
; ccsize.sh answers how many libc bytes a lane ships. This answers how many it can call,
; and the gap is what granularity buys: a libc that arrives as one object arrives whole,
; where one compiled ~a function (musl) or ~an area (crew/moon/lib/nolibc/, pulled member
; by need) sheds what the link never asks for.
;
; Method: walk call/jmp/lea targets out of `objdump -d`, seeded from _start and from every
; function address found in the data sections -- vtables and jump tables are reached no
; other way. Anything unvisited is unreachable. nm/objdump/objcopy do the decoding: holo
; ENCODES the ISAs, it does not read them back.
;
; The error is one-directional -- an indirect call the scan misses marks a live function
; dead, never the reverse -- so the reachable column is a LOWER bound and the dead column
; an UPPER one. Read the musl lane as the control: its ~6.5% is mostly REAL dead code (crt,
; link-time alternates, a public entry riding along with the object-mate that is used), so
; it is the floor a libc built for static linking still cannot get under.
;
; ⚠ this reports the LIBC column only, but love's OWN C is worth asking about too, and the
; natives are the control that makes the answer readable: mooncc leaves 6.4% of its own
; text unreachable against gcc's 2.6% and clang's 1.7%. The excess is not this scan missing
; a dispatch table -- all three lanes run the same tables -- it is that MOONCC INLINES
; WITHOUT DROPPING THE BODY: its `static ai_inline` copy_data is spliced into every call
; site and the out-of-line copy still ships, referenced by nothing.

(: root (cwd 0)
   wd (root + "/out/bench/cc")

   ; --- shelling out, captured ------------------------------------------------
   (shout argv)
    (: pp (pipe 0) r <pp w >pp
       kid (spawnmap argv [(1 >< w) (2 >< ())] () 0 0)
       _ (close w)
       (? (charm? kid && kid < 0) (: _ (close r) "")
          (: q (fdopen r) s (slurp q) _ (close q) _ (wait kid) ("" + s))))

   ; --- little string kit -----------------------------------------------------
   (lines s)
    (: n (tally s)
       (go i a acc) (? (n <= i) (rev (? (i <= a) acc ((snip s a i) >< acc)))
                       ((s i) = 10) (go (i + 1) (i + 1) ((snip s a i) >< acc))
                       (go (i + 1) a acc))
       (go 0 0 ()))
   (blank? c) ((c = 32) || (c = 9) || (c = 13))
   (fields s)
    (: n (tally s)
       (go i a acc) (? (n <= i) (rev (? (i <= a) acc ((snip s a i) >< acc)))
                       (blank? (s i)) (go (i + 1) (i + 1) (? (i <= a) acc ((snip s a i) >< acc)))
                       (go (i + 1) a acc))
       (go 0 0 ()))
   (at? s sub i)                                   ; does sub sit at offset i of s
    (: m (tally sub) ((i + m <= tally s) && ((snip s i (i + m)) = sub)))
   (hex s)                                         ; hex text -> charm, no 0x
    (: n (tally s)
       (go i v) (? (n <= i) v
                   (: c (s i)
                      d (? (47 < c && c < 58) (c - 48) (96 < c && c < 103) (c - 87)
                           (64 < c && c < 71) (c - 55) -1)
                      (? (d < 0) v (go (i + 1) (v * 16 + d)))))
       (go 0 0))

   ; --- the compiler-mangled suffixes, stripped (twice, as gcc nests them) -----
   ; .isra.0 .part.1 .constprop.0 .cold .lto_priv.42 .localalias .llvm.1234
   canon-tags '("isra" "part" "constprop" "cold" "lto_priv" "localalias" "llvm")
   (tail-ok? s i)                                  ; only . _ and digits to the end
    (: n (tally s)
       (go k) (? (n <= k) 1
                 (: c (s k) (? ((c = 46) || (c = 95) || (47 < c && c < 58)) (go (k + 1)) 0)))
       (go i))
   (canon1 s)
    (: n (tally s)
       (try i) (? (n <= i) s
                  ((s i) = 46)
                   (: hit (filter (\ t (at? s t (i + 1) && tail-ok? s (i + 1 + tally t))) canon-tags)
                      (? (two? hit) (snip s 0 i) (try (i + 1))))
                  (try (i + 1)))
       (try 0))
   (canon s) (canon1 (canon1 s))

   ; --- nm: the defined text symbols ------------------------------------------
   (tsyms-of txt)                                  ; -> ((addr . name) ..), nm order
    (: rows (map fields (lines txt))
       ok (filter (\ p (: f p (3 = tally f) && ((f 1) = "T" || (f 1) = "t"))) rows)
       (map (\ f ((hex <f) >< (canon (f 2)))) ok))

   ; sizes by ADDRESS GAP -- mooncc's ELF carries no st_size, so the next symbol
   ; is what bounds this one; the last symbol in the run measures 0.
   (sizes rows)
    (: t (tablet 0)
       (go rs) (? !(two? rs) t
                  (: r <rs nm >r
                     nx (? (two? >rs) (<(<>rs)) <r)
                     _ (pin t nm ((peep t nm 0) + (nx - <r)))
                     (go >rs)))
       (go rows))
   (byaddr rows)
    (: t (tablet 0) _ (each rows (\ r (pin t <r >r))) t)

   ; --- objdump: the call graph ------------------------------------------------
   ; a header line is `0000000000401000 <name>:`; every other line with a tab is an
   ; instruction, and each <target> or <target+0x..> on it is an edge.
   (edges-of txt)
    (: e (tablet 0)
       (tgts ln acc i)                             ; the <..> spellings on one line
        (: n (tally ln)
           (? (n <= i) (rev acc)
              ((ln i) = 60)
               (: (fin j) (? (n <= j) (rev acc)
                             ((ln j) = 62) (: nm (snip ln (i + 1) j)
                                              p (idx-plus nm)
                                              (tgts ln ((? (p < 0) nm (snip nm 0 p)) >< acc) (j + 1)))
                             (fin (j + 1)))
                  (fin (i + 1)))
              (tgts ln acc (i + 1))))
       (idx-plus nm) (: n (tally nm)
                        (go k) (? (n <= k) -1 ((nm k) = 43) k (go (k + 1)))
                        (go 0))
       (tab? ln) (: n (tally ln)
                    (go k) (? (n <= k) 0 ((ln k) = 9) 1 (go (k + 1)))
                    (go 0))
       (hdr ln)                                    ; -> name | ()
        (: n (tally ln)
           (go k) (? (n <= k) ()
                     ((ln k) = 60)
                      (? ((ln (n - 1)) = 58 && (ln (n - 2)) = 62) (snip ln (k + 1) (n - 2)) ())
                     (go (k + 1)))
           (? (2 < n) (go 0) ()))
       (go ls cur) (? !(two? ls) e
                      (: ln <ls h (hdr ln)
                         (? (string? h) (: _ (? (peep e h ()) 0 (pin e h (tablet 0))) (go >ls h))
                            (string? cur && tab? ln)
                             (: s (peep e cur (tablet 0))
                                _ (each (tgts ln () 0) (\ t (pin s t 1)))
                                (go >ls cur))
                            (go >ls cur))))
       (go (lines txt) ()))

   ; --- a function pointer sitting in data is a root: nothing calls it by name --
   data-secs '(".data" ".rodata" ".data.rel.ro" ".init_array")
   (le64 s i)                                      ; 8 bytes little-endian
    (: (go k v) (? (8 <= k) v (go (k + 1) (v + ((s (i + k)) << (8 * k)))))
       (go 0 0))
   (data-roots elf ba acc)
    (: (scan raw i n acc)
        (? (n <= i) acc
           ; the top two bytes gate the read: a text address never reaches 2^48,
           ; so this skips almost every offset without building a number.
           (((raw (i + 7)) = 0) && ((raw (i + 6)) = 0))
            (: nm (peep ba (le64 raw i) ())
               (scan raw (i + 1) n (? (string? nm) (nm >< acc) acc)))
           (scan raw (i + 1) n acc))
       (go secs acc)
        (? !(two? secs) acc
           (: raw (shout ["objcopy" "-O" "binary" "--only-section" <secs elf "/dev/stdout"])
              n (tally raw)
              (go >secs (? (n < 8) acc (scan raw 0 (n - 7) acc)))))
       (go data-secs acc))

   (reach elf ba)
    (: e (edges-of (shout ["objdump" "-d" elf]))
       seed (filter (\ n (peep e n ())) '("_start" "__ai_start" "main"))
       roots (data-roots elf ba seed)
       seen (tablet 0)
       (go q) (? !(two? q) seen
                 (: n <q
                    (? (peep seen n ()) (go >q)
                       (: _ (pin seen n 1)
                          kids (: s (peep e n ()) (? s (keys s) ()))
                          (go (kids + >q))))))
       (go roots))

   ; --- the lane's own love C, by ITS OWN objects, never by what the other lacks -
   (objsyms objs)
    (: t (tablet 0)
       _ (? (two? objs)
            (each (tsyms-of (shout (["nm" "--defined-only"] + objs))) (\ r (pin t >r 1)))
            0)
       t)
   (isdir? p) (: st (stat p) (two? st && ((& 61440 (st 2)) = 16384)))
   (base-of p) (: n (tally p)                      ; the lane name a bare ELF argument wears
                  (go i) (? (n <= i) p ((p (n - 1 - i)) = 47) (snip p (n - i) n) (go (i + 1)))
                  (go 0))
   (ls-suffix d sfx)
    (: n (tally sfx)
       (filter (\ f (: m (tally f) (n <= m && (snip f (m - n) m) = sfx))) (sort (readdir d))))
   (ownof lane)
    (: d (? (lane = "mooncc") (wd + "/mooncc") (wd + "/o-love-" + lane))
       (? !(isdir? d) ()
          (: os (? (lane = "mooncc")
                   (: skip '("love.o" "nolibc.o" "sys.o")
                      keep (filter (\ f (!(member? f skip) && !(at? f "m_" 0)))
                                   (ls-suffix d ".o"))
                      ((map (\ f (d + "/" + f)) keep) + [(d + "/love.o") (d + "/m_am.o")]))
                   (: hd (d + "/host")
                      ((list (d + "/love.o") (d + "/am.o"))
                       + (? (isdir? hd) (map (\ f (hd + "/" + f)) (ls-suffix hd ".o")) ()))))
             live (filter (\ p (two? (stat p))) os)
             (? (two? live) (objsyms live) ()))))

   ; --- the table ---------------------------------------------------------------
   (pad s n) (: m (tally s) (? (n <= m) s (pad (s + " ") n)))
   (lpad s n) (: m (tally s) (? (n <= m) s (lpad (" " + s) n)))
   ; one decimal of a percent, integer-only: `/` would answer a gem and print its whole tail
   (pct a b) (? b (: q ((1000 * a) // b) (show (q // 10) + "." + show (q % 10) + "%")) "0.0%")

   (row lane elf own)
    (: rows (tsyms-of (shout ["nm" "-n" "--defined-only" elf]))
       sz (sizes rows)
       live (reach elf (byaddr rows))
       libc (filter (\ n (! (own && peep own n ()))) (keys sz))
       dead (filter (\ n (! (peep live n ()))) libc)
       tot (foldl (\ a n (a + peep sz n 0)) 0 libc)
       dz (foldl (\ a n (a + peep sz n 0)) 0 dead)
       _ (say out ((pad lane 12) + " " + (lpad (show (tally libc)) 6)
                   + " " + (lpad (show tot) 10)
                   + " " + (lpad (show ((tally libc) - (tally dead))) 6)
                   + " " + (lpad (show (tot - dz)) 10)
                   + " " + (lpad (show (tally dead)) 6)
                   + " " + (lpad (show dz) 10)
                   + "  " + (lpad (pct dz tot) 5) + "\n"))
       _ (? (tally ("" + getenv "CCDEAD_V"))
            (each (take 20 (sortby (\ a b ((peep sz b 0) < (peep sz a 0))) dead))
                  (\ n (say out ("      " + (pad n 28) + " " + (lpad (show (peep sz n 0)) 7) + "\n"))))
            0)
       0)

   (: av >argv
      lanes (? (two? av)
               (map (\ e [(base-of e) e ()]) av)
               (: known '("mooncc" "gcc-musl" "clang-musl" "gcc" "clang")
                  (filter (\ l (two? (stat <>l)))
                          (map (\ l [l (wd + "/love-" + l) (ownof l)]) known))))
      _ (? (two? lanes) 0
           (: _ (say err ("ccdead: no lane binaries under " + wd + " -- run ./ccbench.sh first\n"))
              (quit 1)))
      _ (say out "libc .text, shipped against reachable  (musl is the control: its dead set is the floor)\n")
      _ (say out ((pad "lane" 12) + " " + (lpad "syms" 6) + " " + (lpad "shipped" 10)
                  + " " + (lpad "live" 6) + " " + (lpad "bytes" 10)
                  + " " + (lpad "dead" 6) + " " + (lpad "bytes" 10) + "\n"))
      _ (each lanes (\ l (row <l <>l <>>l)))
      0))
