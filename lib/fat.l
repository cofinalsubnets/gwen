; lib/fat.l -- FAT32, read and write
;
; a DEVICE is a chain (rd wr nsec): (rd lba n) -> a string of n*512 bytes | (),
; (wr lba s) -> the sectors written | () (s whole sectors), nsec the total.
; anything that answers those is a disk -- the kernel's virtio nifs
; ((fat-disk ()) builds that dev), a cask in memory (the host laws), a file.
;
; the surface (an fs is what fat-mount answers; paths are "/"-split,
; case-insensitive, the FAT way):
;   (fat-mkfs dev)         -> 1 | ()        format the whole device FAT32
;   (fat-mount dev)        -> fs | ()
;   (fat-ls fs path)       -> (1 names) | ()
;   (fat-stat fs path)     -> (1 size attr) | ()
;   (fat-read fs path)     -> (1 bytes) | ()
;   (fat-write fs path s)  -> 1 | ()        create or replace, parent must exist
;   (fat-mkdir fs path)    -> 1 | ()
;   (fat-rm fs path)       -> 1 | ()        a file, or an empty dir
;
; ⚠ presence rides the (1 _) wrapper throughout (the tree's law): an empty
; file, an empty dir and a 0-size entry are all real answers that net nothing.
; ⚠ long names are read AND written (UCS-2's low byte; a char past 255 reads
; as ?): a name that fits 8.3 is written short with the lowercase basis flags,
; one that does not gets LFN entries over a ~N short alias, mtools-compatible.
; ⚠ FAT32 only, by the format's own law: under 65525 clusters the type flips
; to FAT16 by DEFINITION, so fat-mkfs refuses a device too small (~33 MB) --
; absence, not divergence.

(:
   ; --- little-endian scalars: strings index charms, casks pin them ---
   (u16 s i) (s i + (s (i + 1) << 8))
   (u32 s i) (u16 s i + (u16 s (i + 2) << 16))
   (w16 b i v) (pin b i (v & 255), pin b (i + 1) (v >> 8 & 255), ())
   (w32 b i v) (w16 b i v, w16 b (i + 2) (v >> 16))
   (bcopy b i s) (: n (tally s) (n ? (pour b i s 0 n), ()))      ; pour refuses an empty
   (bfill b i n v) (n ? (pin b i v, bfill b (i + 1) (n - 1) v))
   (zeros n) (string (cask n))

   ; --- the fs tablet, read by slot: 0 rd, 1 wr, 2 spc, 3 rsvd, 4 nfats, 5 fatsz,
   ;     6 rootcl, 7 data0, 8 nclus, 9 fat-cache lba, 10 fat-cache string, 11 free hint ---
   (rd1 f lba) (f 0 lba 1)
   (rdn f lba n) (f 0 lba n)
   (wr1 f lba s) (f 1 lba s)

   ; --- the FAT itself, one cached sector over it ---
   (fat-sec f cl) (f 3 + (cl * 4 // 512))
   (fat@ f cl)                                        ; the entry, 28 bits
    (: lba (fat-sec f cl)
       s (id? lba (f 9) ? (f 10)
          (: t (rd1 f lba) (pin f 9 lba, pin f 10 t, t)))
       (u32 s (cl * 4 % 512) & 0x0FFFFFFF))
   (fat! f cl v)                                      ; write, both copies, top nibble kept
    (: lba (fat-sec f cl)
       s (id? lba (f 9) ? (f 10) (rd1 f lba))
       b (cask 512) _ (bcopy b 0 s)
       off (cl * 4 % 512)
       _ (w32 b off ((u32 s off & 0xF0000000) + (v & 0x0FFFFFFF)))
       t (string b)
       (wr1 f lba t,
        wr1 f (lba + f 5) t,                          ; the mirror
        pin f 9 lba, pin f 10 t, ()))
   (fat-end? v) (0x0FFFFFF8 <= v)
   ; a file's clusters first to last; freeing walks the same spine
   (chain-of f cl) (cl < 2 || fat-end? cl ? ()
                    (cl >< chain-of f (fat@ f cl)))
   (chain-zap f cl) (cl < 2 || fat-end? cl ? ()
                     (: nx (fat@ f cl) (fat! f cl 0, chain-zap f nx)))
   ; one free cluster, marked end -- () when the disk is full
   (fat-take f)
    (: n (f 8)
       (go cl left) (left ?
                     (: c (n + 1 < cl ? 3 cl)
                      (fat@ f c ? (go (c + 1) (left - 1))
                       (fat! f c 0x0FFFFFFF, pin f 11 (c + 1), c))))
       (go (f 11) n))
   ; k fresh clusters CHAINED -- () (and nothing kept) when the disk fills mid-take
   (chain-take f k) (k ?
                     (: c (fat-take f)
                      (? !(charm? c) ()
                         (k = 1) [c]
                         (: r (chain-take f (k - 1))
                          (two? r ? (fat! f c <r, c >< r)
                           (fat! f c 0, ()))))))

   ; --- clusters <-> bytes ---
   (clu-lba f c) (f 7 + (f 2 * (c - 2)))
   (clu-rd f c) (rdn f (clu-lba f c) (f 2))
   (clu-wr f c s) (wr1 f (clu-lba f c) s)
   (clu-bytes f) (f 2 * 512)
   ; a whole chain's bytes, and bytes laid over a chain (the tail zero-padded)
   (chain-rd f cl) (: (go c acc) (two? c ? (go >c (acc + clu-rd f <c)) acc)
                      (go (chain-of f cl) ""))
   (chain-wr f cls s)
    (: cb (clu-bytes f)
       n (tally s)
       (go c off) (atom? c ? 1
                   (: hi (off + cb < n ? (off + cb) n)
                      b (cask cb)
                    (bcopy b 0 (snip s off hi),
                     clu-wr f <c (string b),
                     go >c (off + cb))))
       (go cls 0))

   ; --- names: FAT is case-blind; 8.3 when it fits, LFN over an alias when not ---
   (up1 c) (97 <= c <= 122 ? (c - 32) c)
   (dn1 c) (65 <= c <= 90 ? (c + 32) c)
   (name=? a b) (: n (tally a)
                   (n = tally b &&
                    ((: (go i) (i < n ? (up1 (a i) = up1 (b i) ? (go (i + 1)) 0) 1)) 0)))
   ; split at the LAST dot: (base . ext), ext "" when dotless
   (splitext s) (: n (tally s)
                   (go i d) (? (i < n) (go (i + 1) (s i = 46 ? i d))
                               (d < 0) (s >< "")
                               (snip s 0 d >< snip s (d + 1) n))
                   (go 0 -1))
   (alnum? c) (48 <= c <= 57 || 65 <= c <= 90 || 97 <= c <= 122)
   (plain? c) (alnum? c || c = 95 || c = 45 || c = 126)
   (allc? s p) ((: (go i) (i < tally s ? (p (s i) ? (go (i + 1)) 0) 1)) 0)
   (lower? s) (allc? s (\ c !(65 <= c <= 90)))
   (upper? s) (allc? s (\ c !(97 <= c <= 122)))
   (upcopy b i s) (: (go j) (j < tally s ? (pin b (i + j) (up1 (s j)), go (j + 1)))
                     (go 0))
   ; the 11 raw name bytes + the basis flags | () when the name needs LFN
   (fit83 s)
    (splitext s @ (b >< e)
     (? (tally b && tally b <= 8 && tally e <= 3 &&
         allc? b plain? && allc? e plain? &&
         (lower? b || upper? b) &&
         (lower? e || upper? e))
      (: r (cask 11)
       (bfill r 0 11 32,
        upcopy r 0 b,
        upcopy r 8 e,
        [(string r) ((? (lower? b) 8 0) + (? (lower? e) 16 0))]))))
   (cksum n11) ((: (go i s) (i >= 11 ? s (go (i + 1) (255 & (n11 i + ((s & 1) << 7) + (s >> 1)))))) 0 0)
   ; the ~N alias: up to 6 plain upcased chars + "~" + N, unique among taken raw11s
   (alias gs s)
    (: base ((: (go i acc) (i = tally s || 6 <= tally acc ? acc
                            (go (i + 1) (alnum? (s i) ? (acc + string [(up1 (s i))]) acc))))
             0 "")
       base (tally base ? base "F")
       (go n) (99 < n ? ()
               (: r (cask 11)
                  _ (bfill r 0 11 32, bcopy r 0 (base + "~" + show n))
                  t (string r)
                (any (g \ t = peep g 4 ()) gs ? (go (n + 1)) t)))
       (go 1))

   ; --- directory entries. a GROUP is one slot's story -- its LFN run + short
   ;     entry, raw -- as a tablet: 0 name (() for the label and the dots: out of
   ;     lookups, but their raw survives a rewrite), 1 attr, 2 cluster, 3 size,
   ;     4 raw11, 5 raw ---
   (group nm at cl sz r11 raw) {0 nm 1 at 2 cl 3 sz 4 r11 5 raw}
   lfn-offs '(1 3 5 7 9 14 16 18 20 22 24 28 30)      ; where the 13 UCS-2 chars sit
   (lfn-part s o)                                     ; one LFN entry's text
    (: (ch i) (: v (u16 s (o + i)) (!v || v = 0xFFFF ? () (v < 256 ? v 63)))
       (go l) (atom? l ? "" (: c (ch <l) (charm? c ? (string [c] + go >l) "")))
     (go lfn-offs))
   (short-name s o flags)                             ; 8.3 back to text, basis flags honored
    (: (part i n lc) ((: (go j acc) (? (j < n)
                          (: c (s $ o + i + j)
                             (? (c = 32) (go (j + 1) acc)
                                (go (j + 1) (acc + string [(? lc (dn1 c) c)]))))
                          acc))
                      0 "")
       b (part 0 8 (flags & 8)) e (part 8 3 (flags & 16))
       (tally e ? (b + "." + e) b))
   ; raw dir bytes -> groups. an LFN run rides `start` until its short entry
   ; claims it; a freed slot or the end orphans (drops) a half-run, honestly.
   (dir-scan s)
    (: n (tally s)
       (go o lfn start acc)
        (? (n <= o) (rev acc)
           (: c0 (s o)
              (? !c0 (rev acc)                     ; 0x00: end of directory
                 (= c0 0xE5) (go (o + 32) "" () acc)
                 (: at (s (o + 11))
                    (? ((at & 15) = 15)
                       (go (o + 32) ((lfn-part s o) + lfn) (charm? start ? start o) acc)
                       (: cl ((u16 s (o + 26)) + ((u16 s (o + 20)) << 16))
                          label ((at & 8) = 8)
                          nm (tally lfn ? lfn (short-name s o (s (o + 12))))
                          dot (name=? nm "." || name=? nm "..")
                          raw (snip s (charm? start && !label ? start o) (o + 32))
                          g (group (label || dot ? () nm) at cl (u32 s (o + 28))
                                   (snip s o (o + 11)) raw)
                          (go (o + 32) "" () (g >< acc))))))))
       (go 0 "" () ()))
   (dir-bytes gs) (: (go m acc) (two? m ? (go >m (acc + (peep <m 5 ()))) acc) (go gs ""))
   (dir-find (g >< s) nm)
    (: n (peep g 0 ()) (string? n && name=? n nm ? g (dir-find s nm)))
   (dir-drop gs nm) (filter (\ g (: n (peep g 0 ()) !(string? n && name=? n nm)))
                            gs)
   (dir-names gs) (: (go m acc) (? (two? m)
                                   (go >m (: n (peep <m 0 ()) (string? n ? (n >< acc) acc)))
                                   (rev acc))
                     (go gs ()))

   ; --- a directory as a whole: its groups, and writing them back (growing the
   ;     chain when they outgrow it; a shrunk dir keeps its clusters -- the pad
   ;     zeros read as the end marker, and simple beats clever) ---
   (dir-load f cl) (dir-scan (chain-rd f cl))
   (dir-save f cl gs)
    (: s (dir-bytes gs)
       cb (clu-bytes f)
       need ((tally s + 32 + cb - 1) // cb)     ; +32: the end marker must fit
       cls (chain-of f cl)
       cls (tally cls >= need ? cls
            (: extra (chain-take f (need - tally cls))
             (two? extra ? (fat! f (last cls) <extra, cls + extra) cls)))
       (tally cls >= need ?                     ; short of that the disk is full: refuse
        (chain-wr f cls (s + zeros (cb * tally cls - tally s)),
         1)))

   ; --- paths: "/a/b/c" -> components; the walk answers the parent's cluster,
   ;     the leaf's name, and the leaf's group when present: [dircl nm g|()] --
   ;     () when a mid component is absent or no dir. "" and "/" answer the
   ;     root itself: [rootcl "" ()] ---
   (comps p) (: n (tally p)
                (go i w acc) (? (n <= i) (rev (tally w ? (w >< acc) acc))
                                (: c (p i)
                                   (? (c = 47) (go (i + 1) "" (tally w ? (w >< acc) acc))
                                      (go (i + 1) (w + string [c]) acc))))
                (go 0 "" ()))
   (fat-walk f path)
    (: (go cl cs) (: nm <cs g (dir-find (dir-load f cl) nm)
                   (? (atom? >cs) [cl nm g]
                    (tablet? g && (16 & peep g 1 ()) ? (go (peep g 2 ()) >cs))))
       cs (comps path)
       (two? cs ? (go (f 6) cs) [(f 6) "" ()]))

   ; --- timestamps: (clock 0) ms -> the DOS pair (date . time) ---
   (dos-now u)
    (: ms (clock 0) days (ms // 86400000) secs (ms // 1000 % 86400)
       z (days + 719468) era (z // 146097) doe (z % 146097)
       yoe ((doe - (doe // 1460 + doe // 146096 - doe // 36524)) // 365)
       y (era * 400 + yoe)
       doy (doe - (365 * yoe + (yoe // 4 - yoe // 100)))
       mp ((5 * doy + 2) // 153)
       d (doy - ((153 * mp + 2) // 5) + 1)
       m (mp < 10 ? (mp + 3) (mp - 9))
       y (m <= 2 ? (y + 1) y)
       (((y - 1980) << 9) + (m << 5) + d ><
        ((secs // 3600) << 11) + ((secs // 60 % 60) << 5) + (secs % 60 // 2)))

   ; --- building one entry group for a name ---
   (mk-short n11 flags at cl sz)
    (: b (cask 32) _ (bcopy b 0 n11, pin b 11 at, pin b 12 flags)
       dt (dos-now ()) dat <dt tim >dt
       _ (w16 b 14 tim, w16 b 16 dat, w16 b 18 dat)
       _ (w16 b 20 (cl >> 16), w16 b 22 tim, w16 b 24 dat)
       _ (w16 b 26 (cl & 0xFFFF), w32 b 28 sz)
       (string b))
   (mk-lfn nm ck)                                     ; the run, highest seq first
    (: k ((tally nm + 12) // 13)
       (chr i) (? (i < tally nm) (nm i) (i = tally nm) 0 0xFFFF)
       (one seq first)
        (: b (cask 32) o ((seq - 1) * 13)
           _ (pin b 0 (seq + (first ? 64 0)), pin b 11 15, pin b 13 ck)
           (stamp j offs) (two? offs ? (w16 b <offs (chr (o + j)), stamp (j + 1) >offs))
           _ (stamp 0 lfn-offs)
           (string b))
       (go seq first acc) (seq ? (go (seq - 1) 0 (acc + one seq first)) acc)
       (go k 1 ""))
   (mk-group gs nm at cl sz)                          ; -> a group | () (no alias left)
    (fit83 nm @ [n11 fl] (group nm at cl sz n11 (mk-short n11 fl at cl sz))
     (: n11 (alias gs nm)
      (string? n11 ?
       (: sh (mk-short n11 0 at cl sz)
        (group nm at cl sz n11 (mk-lfn nm (cksum n11) + sh))))))

   ; --- mkfs: reserved 32, two FATs, one-sector clusters ---
   (fat-mkfs dev)
    (: wr <>dev nsec <>>dev
       fatsz ((: (go fz) (: cl (nsec - (32 + 2 * fz))
                            need (((cl + 2) * 4 + 511) // 512)
                            (need <= fz ? fz (go need))))
              1)
       nclus (nsec - (32 + 2 * fatsz))
       (? (nclus < 65525) ()                          ; smaller would BE fat16: refuse
          (: b (cask 512)
             _ (pin b 0 0xEB, pin b 1 0x58, pin b 2 0x90, bcopy b 3 "love fat")
             _ (w16 b 11 512, pin b 13 1, w16 b 14 32, pin b 16 2)
             _ (pin b 21 0xF8, w16 b 24 32, w16 b 26 8)
             _ (w32 b 32 nsec, w32 b 36 fatsz, w32 b 44 2)
             _ (w16 b 48 1, w16 b 50 6, pin b 64 0x80, pin b 66 0x29)
             _ (w32 b 67 (clock 0 & 0xFFFFFFFF))
             _ (bcopy b 71 "LOVE       ", bcopy b 82 "FAT32   ", w16 b 510 0xAA55)
             bpb (string b)
             i (cask 512)
             _ (w32 i 0 0x41615252, w32 i 484 0x61417272,
                w32 i 488 (nclus - 1), w32 i 492 3, w32 i 508 0xAA550000)
             inf (string i)
             _ (wr 0 bpb, wr 1 inf, wr 6 bpb, wr 7 inf)
             ; zero both FATs + the root cluster, 64 sectors a stroke
             z64 (zeros 32768)
             (go lba left) (left ?
                            (: k (left < 64 ? left 64)
                             (wr lba (k = 64 ? z64 (snip z64 0 (k * 512))),
                              go (lba + k) (left - k))))
             _ (go 32 (2 * fatsz + 1))                ; the root cluster rides the tail
             ; FAT[0] media, FAT[1] end, FAT[2] the root dir's end
             fb (cask 512)
             _ (w32 fb 0 0x0FFFFFF8, w32 fb 4 0x0FFFFFFF, w32 fb 8 0x0FFFFFFF)
             ft (string fb)
             (wr 32 ft, wr (32 + fatsz) ft, 1))))

   ; --- mount: the BPB read back, refused unless it is the FAT32 this writes ---
   (fat-mount dev)
    (: rd <dev s (rd 0 1)
       (? !(string? s) ()
          (u16 s 510 != 0xAA55 || u16 s 11 != 512 || u16 s 22 || !(u32 s 36)) ()
          (: spc (s 13) rsvd (u16 s 14) nfats (s 16) fatsz (u32 s 36)
             nsec (u32 s 32) data0 (rsvd + nfats * fatsz)
             {0 rd 1 <>dev 2 spc 3 rsvd 4 nfats 5 fatsz 6 (u32 s 44)
              7 data0 8 ((nsec - data0) // spc) 9 () 10 () 11 3})))

   ; --- the verbs ---
   (fat-ls f path)
    (fat-walk f path @ [dcl nm g]
     (: cl (tally nm ? (tablet? g && (peep g 1 () & 16) ? (peep g 2 ())) dcl)
      (charm? cl ? [1 (dir-names (dir-load f cl))])))
   (fat-stat f path)
    (fat-walk f path @ [_ _ g]
     (tablet? g ? [1 (peep g 3 ()) (peep g 1 ())]))
   (fat-read f path)
    (fat-walk f path @ [_ _ g]
     (? (!(tablet? g) || (peep g 1 () & 16)) ()
        (: sz (peep g 3 ())
         (sz ? [1 (snip (chain-rd f (peep g 2 ())) 0 sz)] [1 ""]))))
   ; ⚠ the ORDER is the safety: new chain laid, entry swapped in, and only then
   ; the old chain freed -- a refusal mid-way leaves the old file whole.
   (fat-write f path s)
    (fat-walk f path @ [dcl nm old]
     (? (!(tally nm) || (tablet? old && (peep old 1 () & 24))) ()
        (: cb (clu-bytes f)
           need ((tally s + cb - 1) // cb)
           cls (chain-take f need)
           (? (need && atom? cls) ()                  ; the disk is full; nothing changed
              (: _ (need ? (chain-wr f cls s))
                 gs (dir-drop (dir-load f dcl) nm)
                 ng (mk-group gs nm 32 (need ? <cls 0) (tally s))
                 ok (tablet? ng ? (dir-save f dcl (gs + [ng])))
                 (? ok (tablet? old ? (chain-zap f (peep old 2 ())), 1)
                  (need ? (chain-zap f <cls),
                   ())))))))
   (fat-mkdir f path)
    (fat-walk f path @ [dcl nm g]
     (? (!(tally nm) || tablet? g) ()                 ; no name, or it exists already
        (: cls (chain-take f 1)
           (? (atom? cls) ()
              (: c <cls cb (clu-bytes f)
                 b (cask cb)
                 _ (bcopy b 0 (mk-short ".          " 0 16 c 0),
                    bcopy b 32 (mk-short "..         " 0 16 (dcl = f 6 ? 0 dcl) 0),
                    clu-wr f c (string b))
                 gs (dir-load f dcl)
                 ng (mk-group gs nm 16 c 0)
                 ok (tablet? ng ? (dir-save f dcl (gs + [ng])))
                 (? ok 1 (chain-zap f c, ())))))))
   (fat-rm f path)
    (fat-walk f path @ [dcl nm g]
     (? !(tablet? g) ()
        (: at (peep g 1 ()) cl (peep g 2 ())
           (? ((at & 16) && two? (dir-names (dir-load f cl))) ()   ; a dir must be empty
              (1 < cl ? (chain-zap f cl),
               (dir-save f dcl (dir-drop (dir-load f dcl) nm) ? 1))))))

   ; --- the kernel's own disk as a dev, when this seat has one ---
   (fat-disk u) (? (member? 'disk (names ()))
                   (: n (ev 'disk ())
                      (n ? [(ev 'disk-read) (ev 'disk-write) n]))
                   ()))
