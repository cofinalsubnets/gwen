; lib/cpio.l -- cpio in love: the SVR4 "newc" archive, read and written. `(use 'cpio)`:
;
;   (cpio-pack entries)      -> the archive bytes
;   (cpio-unpack s)          -> (1 entries) | ()
;   (cpio-scatter root es u) -> (1 count) | ()   write entries back out under root
;
; an entry is lib/tar.l's entry -- the same tablet, the same keys ('nom 'kind 'body 'to
; 'mode 'mtime 'uid 'gid) -- because the walk that fills them (`tar-gather`) and the
; leveller that makes them reproducible (`tar-level`) are about a file and not about a
; wire. this file is a second wire for them, which is the whole reason it is short.
; presence rides the (1 _) wrapper on the read: an archive of no entries is legitimate
; and nets nothing, so the value alone cannot say torn from empty.
;
; the format, in one breath: a 110-byte all-ASCII header per member -- "070701" then
; thirteen eight-digit hex fields -- then the name (its NUL counted in namesize), then
; the body, each padded up to a 4-byte boundary measured from the start of the archive.
; a member named "trailer!!!" ends it, and the whole is padded to 512 with NULs.
;
; the mode field is the whole st_mode, type bits and all -- 0100000 for a file, 0040000
; for a directory, 0120000 for a symlink -- where tar keeps the kind in a separate
; typeflag byte. a newc header carrying permission bits alone extracts as a file of
; mode 0 and no kind, which the kernel's own initramfs reader takes silently. a
; symlink's target is the body, counted in filesize and not NUL-terminated.
;
; hard links are not encoded. newc says them with a shared ino and nlink > 1, the body
; hung on the last member; we write a fresh ino and nlink 1 for every entry, so a tree
; of hard links comes back as copies. a half-done job here would be worse than none: a
; reader that believes nlink waits for a body that never comes.
;
; cpio rides module 'cpio and reads tar's names, the entry shape being tar's on purpose
; so one walk feeds both wires. a consumer says (use 'cpio).
(module 'cpio
(use 'tar)

(:
   cpio-mag "070701"
   cpio-trail "TRAILER!!!"

   ; --- the field pokes ------------------------------------------------------
   ; eight uppercase hex digits, zero-padded. every number in the header is this.
   (cpio-hex! b o v)
    (: (go i x) (? (i < 0) 0
                   (: d (x & 15)
                      _ (pin b (o + i) (d < 10 ? (48 + d) (55 + d)))
                      (go (i - 1) (x >> 4))))
       (go 7 v))
   (cpio-hex s o)                           ; eight hex digits -> the number
    (: (go i acc) (? (8 <= i) acc
                     (: c (s (o + i))
                        d (? (48 <= c <= 57)   (c - 48)
                             (65 <= c <= 70)   (c - 65 + 10)
                             (97 <= c <= 102)  (c - 97 + 10)
                             ())
                        ; the refusal is asked by identity: `nil?` is a truth test, and
                        ; the digit 0 answers it as readily as a miss does
                        (? (id? d ()) () (go (i + 1) (acc << 4 | d)))))
       (go 0 0))
   (cpio-pad n) ((4 - n % 4) % 4)           ; the NULs after a name or a body
   (cpio-nuls k) (: (go i acc) (? (k <= i) acc (go (i + 1) (acc + string 0))) (go 0 ""))

   ; --- the kind, which lives in the mode here --------------------------------
   (cpio-kmode k) (? (id? k 'dir) 16384 (id? k 'link) 40960 32768)
   (cpio-kind m) (: t (m & 61440)
                    (? (t = 16384) 'dir (t = 40960) 'link 'file))

   ; --- writing ---------------------------------------------------------------
   (cpio-body e)                            ; what the member carries: a link says its target
    (: k (peep e 'kind 'file)
       (? (id? k 'link) (peep e 'to "") (id? k 'dir) "" (peep e 'body "")))
   (cpio-head ino nom mode mtime uid gid nb)
    (: nz (#nom + 1)
       h (cask 110)
       (txt! o s) (: n #s
                     (go i) (? (n <= i) 0 (: _ (pin h (o + i) (s i)) (go (i + 1))))
                     (go 0))
       _ (txt! 0 cpio-mag)
       _ (cpio-hex! h 6 ino)   _ (cpio-hex! h 14 mode)
       _ (cpio-hex! h 22 uid)  _ (cpio-hex! h 30 gid)
       _ (cpio-hex! h 38 1)                  ; nlink -- see the header note
       _ (cpio-hex! h 46 mtime) _ (cpio-hex! h 54 nb)
       _ (cpio-hex! h 62 0)  _ (cpio-hex! h 70 0)     ; dev major/minor
       _ (cpio-hex! h 78 0)  _ (cpio-hex! h 86 0)     ; rdev major/minor
       _ (cpio-hex! h 94 nz) _ (cpio-hex! h 102 0)    ; namesize, and newc's dead check
       (string h))
   (cpio-pack es)
    (: o (jug 0)
       (one i e)
        (: nom (peep e 'nom "")
           b (cpio-body e)
           nb #b
           mode (cpio-kmode (peep e 'kind 'file)
                 | peep e 'mode (id? (peep e 'kind 'file) 'dir ? 493 420))
           _ (say o (cpio-head i nom mode (peep e 'mtime 0)
                               (peep e 'uid 0) (peep e 'gid 0) nb))
           _ (say o (nom + string 0))
           _ (say o (cpio-nuls (cpio-pad (110 + #nom + 1))))
           _ (say o b)
           _ (say o (cpio-nuls (cpio-pad nb)))
           (i + 1))
       _ (foldl one 1 es)
       ; the trailer is a member like any other, and its own name is the end mark
       _ (say o (cpio-head 0 cpio-trail 0 0 0 0 0))
       _ (say o (cpio-trail + string 0))
       _ (say o (cpio-nuls (cpio-pad (110 + #cpio-trail + 1))))
       s (slurp o)
       ; the archive ends on a 512 boundary. the kernel does not care and GNU cpio does
       ; not either, but `cmp` against a GNU archive does, and that is the oracle
       (s + cpio-nuls ((512 - #s % 512) % 512)))

   ; --- reading ---------------------------------------------------------------
   (cpio-read1 s n o)                       ; -> (1 entry next) | 'end | ()
    (? (n < o + 110) ()
       !(snip s o (o + 6) = cpio-mag) ()
       (: mode (cpio-hex s (o + 14))
          mtime (cpio-hex s (o + 46))
          nb (cpio-hex s (o + 54))
          uid (cpio-hex s (o + 22)) gid (cpio-hex s (o + 30))
          nz (cpio-hex s (o + 94))
          (? (id? mode () || id? nb () || id? nz ()) ()
             (n < o + 110 + nz) ()
             (: nom (snip s (o + 110) (o + 110 + nz - 1))
                b0 (o + 110 + nz + cpio-pad (110 + nz))
                (? (nom = cpio-trail) 'end
                   (n < b0 + nb) ()
                   (: kind (cpio-kind mode)
                      body (snip s b0 (b0 + nb))
                      e (tar-entry nom kind (id? kind 'link ? "" body))
                      _ (pin e 'mode (mode & 4095))
                      _ (pin e 'mtime mtime)
                      _ (pin e 'uid uid) _ (pin e 'gid gid)
                      _ (id? kind 'link && pin e 'to body)
                      [1 e (b0 + nb + cpio-pad nb)]))))))
   (cpio-unpack s)
    (: n #s
       (go o acc) (: r (cpio-read1 s n o)
                     (? (id? r 'end) [1 (rev acc)]
                        !r ()
                        (go <>>r (<>r . acc))))
       (go 0 ()))

   ; --- writing the tree back out ---------------------------------------------
   ; the leading directories are always made. GNU wants -d before it will and refuses
   ; the member otherwise; refusing to write a file we can write is not a service, so
   ; -d is a flag we accept and were already honouring.
   (cpio-mkp p)                             ; every parent of p, in order
    (: n #p
       (go i) (? (n <= i) 0
                 (: _ (p i = 47 && 0 < i && mkdir (snip p 0 i) 493)
                    (go (i + 1))))
       (go 0))
   (cpio-there? p) (two? (stat p))
   (cpio-newer? p ms)                       ; is what is already there at least as new
    (: st (stat p) (two? st ? !(<>st // 1000 < ms) 0))
   (cpio-scatter root es u)                 ; -> (1 count) | ()
    (: (go xs k)
        (? !(two? xs) [1 k]
           (: e <xs
              nom (tar-strip (peep e 'nom ""))
              p (#nom ? (#root ? (root + "/" + nom) nom) root)
              kind (peep e 'kind 'file)
              _ (cpio-mkp p)
              skip (!u && !(id? kind 'dir) && cpio-newer? p (peep e 'mtime 0))
              ok (? skip 1
                    (id? kind 'dir) (: _ (mkdir p (peep e 'mode 493)) (cpio-there? p))
                    (id? kind 'link) (: _ (cpio-there? p && unlink p)
                                         _ (symlink (peep e 'to "") p)
                                         (string? (readlink p)))
                    (: q (open p "w")
                       _ (q && (: _ (say q (peep e 'body "")) (close q)))
                       _ (cpio-there? p && chmod p (peep e 'mode 420))
                       (cpio-there? p)))
              (ok && go >xs (skip ? k (k + 1)))))
       (go es 0)))

) ; module 'cpio
