; tools/fatpack.l -- lay the fat container: one file, every text
; (doc/misc/plan/seed-universal.md rung U1).
;
;   love tools/fatpack.l OUT ARCH FILE [ARCH FILE ..]
;
; ARCH is uname -m's spelling (x86_64, aarch64, ..). the members land at
; page-aligned block offsets behind a one-block #!/bin/sh prefix that picks
; the machine's member, lays it under ~/.love/fat ONCE (content-named: the
; sha of the members rides the cache name, so a new fat lands a new cache),
; and execs it. deterministic: same members in, same bytes out -- the id is
; the members' own sha and nothing here reads a clock.
;
; ⚠ the container never runs native; the EXTRACTION does. the distributed
; bytes stay immutable -- a bake belongs to the extraction (rung U1.2).

(: (fp-die m) (: _ (say err ("fatpack: " + m + "\n")) (quit 1))
   (fp-read p) (: q (open p "r") (? q (: s (slurp q) _ (close q) `(1 (s + ""))) ()))
   (fp-write p s) (: q (open p "w") _ (? q 0 (fp-die ("cannot write " + p))) _ (say q s) (close q))
   bs 4096
   (fp-blocks n) ((n + bs - 1) // bs)
   (fp-pad n) ("\x00" * ((fp-blocks n) * bs - n))
   ; the members' sha, 16 hex -- the cache name's content half
   (fp-id16 h) (: (go i a) (? (i < 16) (go (i + 1) (a + (string (peep h i 0)))) a) (go 0 ""))
   ; (arch size) -> one case arm; b/n in blocks, s the exact byte count
   (fp-arm arch b n s)
    (arch + ") b=" + show b + " n=" + show n + " s=" + show s + ";;\n")
   (fp-prefix id arms)
    ("#!/bin/sh\n"
     + "# love -- one file, every text (doc/misc/plan/seed-universal.md U1)\n"
     + "m=$(uname -m)\ncase \"$m\" in\n" + arms
     + "*) echo \"love: no text aboard for $m\" >&2; exit 2;;\nesac\n"
     + "d=\"${HOME:-/tmp}/.love/fat\"\n"
     + "mkdir -p \"$d\" 2>/dev/null || d=\"${TMPDIR:-/tmp}\"\n"
     + "c=\"$d/love-" + id + "-$m\"\n"
     + "if [ ! -x \"$c\" ]; then\n"
     + "  dd if=\"$0\" of=\"$c.$$\" bs=" + show bs + " skip=$b count=$n 2>/dev/null || exit 3\n"
     + "  head -c \"$s\" \"$c.$$\" > \"$c.h.$$\" && chmod +x \"$c.h.$$\" && mv \"$c.h.$$\" \"$c\"\n"
     + "  rm -f \"$c.$$\"\nfi\n"
     + "exec \"$c\" \"$@\"\n")
   ; args -> ((arch . bytes) ..), in argv order (the order is the make rule's, fixed)
   (fp-members l)
    (? (atom? l) ()
       (atom? >l) (fp-die "ARCH with no FILE")
       (: g (fp-read <>l)
          _ (? (two? g) 0 (fp-die ("cannot read " + <>l)))
          ((<l >< <>g) >< fp-members >>l)))
   (fp-main av)
    (: _ (? (3 <= (tally av)) 0 (fp-die "usage: fatpack OUT ARCH FILE [ARCH FILE ..]"))
       out <av
       ms (fp-members >av)
       id (fp-id16 (sha256 (foldl (\ a m (a + >m)) "" ms)))
       ; lay the arms and the body: member k starts at block 1 + blocks-before-it
       arms (: (go l b) (? (atom? l) ""
                 (: n (tally >(<l))
                    ((fp-arm <(<l) b (fp-blocks n) n) + (go >l (b + (fp-blocks n))))))
               (go ms 1))
       pre (fp-prefix id arms)
       _ (? ((tally pre) <= bs) 0 (fp-die "prefix outgrew its block"))
       body (foldl (\ a m (a + >m + (fp-pad (tally >m)))) (pre + (fp-pad (tally pre))) ms)
       _ (fp-write out body)
       (say out ("FAT\t" + out + " (" + (show (tally ms)) + " texts, "
                 + (show (tally body)) + " bytes, id " + id + ")\n")))
   (fp-main >argv))
