; lib/tar.l -- tar in love: the ustar archive, read and written. `(use 'tar)`, then:
;
;   (tar-pack entries)     -> the archive bytes
;   (tar-unpack s)         -> (1 entries) | ()
;   (tar-gather root path) -> (1 entries) | ()    walk the filesystem into entries
;   (tar-scatter root es)  -> (1 count) | ()      write entries back out under root
;
; an entry is a tablet, because the header has fourteen fields and a positional list
; of them would be unreadable at every call site. the keys, and the only ones that
; matter: 'nom the path as stored (relative, no leading /), 'kind one of 'file 'dir
; 'link, 'body the bytes (a file), 'to the target (a link), 'mode the permission bits,
; 'mtime, 'uid, 'gid, 'uname, 'gname. (tar-entry nom kind body) fills the rest with the
; ordinary defaults, which is what nearly every caller wants.
;
; presence rides the (1 _) wrapper on both reads, and it must: an empty archive is a
; legitimate archive and answers no entries, which nets nothing -- indistinguishable
; from a torn one by the value alone. test with two?.
;
; the format, in one breath: a 512-byte header per member, then the body padded up to
; the next 512, then two zero blocks to end. Every numeric field is octal in ASCII with
; a NUL or a space where the digits stop, which is why they are written by hand here
; rather than by any number printer -- the widths are the format's, not ours.
;
; the checksum is computed over the header with its own field read as eight spaces.
; That is not a convention we could choose differently: it is how the field can live
; inside the block it sums. It is written back as six octal digits, a NUL, then a
; space -- an order two of the three historical tars disagree about, and the one every
; reader accepts.
;
; ustar splits a long name across `prefix` (155) and `name` (100), joined with a "/"
; that is not stored. a path that fits neither -- no "/" leaving a <=100 tail -- is
; refused rather than truncated: a silently shortened path is a wrong archive that
; looks right until something extracts it.
; the wrap: tar rides module 'tar. a consumer says (use 'tar); riding a cat -- the
; dist artifact's, where this file is evaluated into the image -- the wrap registers
; the same module, so the compile is paid once at bake time and (use 'tar) is a splice
; afterwards rather than a reload. unwrapped it leaked its names into the cat's layer
; and `use` found no module, so every caller recompiled this file from lib/.
(module 'tar
(:
   tar-blk 512

   ; --- the field pokes ------------------------------------------------------
   ; octal, right-aligned in n-1 columns, zero-padded, then a NUL. this is the
   ; `%0*lo` of every tar ever written, spelled out because the widths are the wire.
   (tar-oct! b o n v)
    (: (go i x) (i >= 0 ? (pin b (o + i) (48 + (x & 7)), go (i - 1) (x >> 3)))
     (pin b (o + n - 1) 0, go (n - 2) v))
   (tar-txt! b o n s)                       ; a text field, NUL-filled, never overrun
    (: k #s (go i) (n > i ? (pin b (o + i) (? (i < k) (s i) 0), go (i + 1))) (go 0))
   (tar-sum b)                              ; the header checksum, own field as spaces
    (: (go i a) (tar-blk <= i ? a (go (i + 1) (a + (148 <= i < 156 ? 32 (peep b i 0))))) (go 0 0))

   ; --- the field peeks ------------------------------------------------------
   (tar-oct s o n)                          ; octal digits until a non-digit
    (: (go i a) (n <= i ? a (s (o + i) @ c (48 <= c < 56 ? (go (i + 1) (a * 8 + c - 48)) a))) (go 0 0))
   (tar-txt s o n)                          ; a NUL-ended text field
    (: (go i) (? (n <= i) i ((s (o + i)) = 0) i (go (i + 1))) k (go 0)
     (!k ? "" (cask k @ c (pour c 0 s o k, string c))))

   ; --- entries --------------------------------------------------------------
   (tar-entry nom kind body)
    {'nom nom 'kind kind 'body (body + "") 'to "" 'mode (kind @ 'dir 493 420)
     'mtime 0 'uid 0 'gid 0 'uname "root" 'gname "root"}
   ; peep's own default is the presence test here -- it answers d only when the key is
   ; absent, so an entry whose 'body is legitimately "" keeps its "". not `lit?`,
   ; which asks whether a name is bound and reads it.
   tar-get peep

   ; --- the name split -------------------------------------------------------
   ; ustar joins prefix and name with a "/" it does not store, so the split must fall
   ; on a separator. we take the longest prefix under 156 that leaves a name under 101
   ; -- longest, because it is the tail that is tight.
   (tar-split m)                          ; -> (prefix name) | ()
    (: n #m
     (n < 101 ? ["" m]
      (: (go i best) (n <= i ? best (go (i + 1) (m i = 47 && i < 156 && n - i - 1 < 101 ? i best)))
         k (go 0 -1)
       (k >= 0 ? (: p (cask k)
                    q (pour p 0 m 0 k, cask (n - k - 1))
        (pour q 0 m (k + 1) (n - k - 1),
         [(string p) (string q)]))))))

   ; src/hash.c's streaming state: 105 bytes, opaque, allocated here because the
   ; nifs do not allocate -- they fold into a cask the caller owns.
   tar-shast 105

   ; --- writing --------------------------------------------------------------
   (tar-kindc k) (k @ 'dir 53 'link 50 48)   ; '5' '2' '0'
   (tar-head e)                             ; -> the 512-byte header | ()
    (: n (tar-get e 'nom "")
       kind (tar-get e 'kind 'file)
       n2 (kind @- n 'dir (n || "/"))
       sp (tar-split n2)
       (? !sp ()
          (: b (cask tar-blk)
             body (tar-get e 'body "")
           (tar-txt! b 0 100 (<(>sp)),                    ; name
            tar-oct! b 100 8 (tar-get e 'mode 420),
            tar-oct! b 108 8 (tar-get e 'uid 0),
            tar-oct! b 116 8 (tar-get e 'gid 0),
             ; the size comes off 'size when the entry carries one -- a thin entry
             ; (tar-thin?) knows its length without holding its bytes. no 'size and
             ; it is the body's tally, which is every fat entry, unchanged.
            tar-oct! b 124 12 (kind @- 0 'file (tar-get e 'size #body)),
            tar-oct! b 136 12 (tar-get e 'mtime 0),
            pin b 156 (tar-kindc kind),
            tar-txt! b 157 100 (tar-get e 'to ""),
            tar-txt! b 257 6 "ustar",                     ; magic, NUL-ended
            pins b 263 48 264 48,                ; version "00", not NUL-ended
            tar-txt! b 265 32 (tar-get e 'uname "root"),
            tar-txt! b 297 32 (tar-get e 'gname "root"),
            tar-oct! b 329 8 0, tar-oct! b 337 8 0,    ; devmajor/minor
            tar-txt! b 345 155 <sp,                     ; prefix
             ; the checksum last -- it sums the block it sits in, so every other
             ; field must already be written. six digits, NUL, space.
            tar-oct! b 148 7 (tar-sum b),
            pin b 154 32,
            string b))))
   ; --- the archive as a stream ----------------------------------------------
   ; tar-pack's answer exists to be looked at once and dropped, and for a source tree
   ; that is megabytes held to be hashed. tar-hash is the same archive fed through a
   ; resumable sha-256 (src/hash.c) a bufferful at a time: the header, then the file's
   ; bytes read straight off 'path, then the pad -- so the peak is one cask and not one
   ; archive. It answers what (sha256 (tar-pack es)) answers, and test/gate/targz.sh
   ; holds the two together over a tree with the awkward shapes in it.
   ; it reads what the header says. a file that moved between the walk's stat and
   ; this read would put a body of one length under a header of another, so a short
   ; read is a torn archive and answers () -- the caller cuts the fat way, which reads
   ; each file once and cannot disagree with itself.
   ; `snip` reads the cask, and it must: (snip (string buf) 0 k) copies the whole
   ; buffer to take the corner a read filled -- 64K for a 12-byte file, and over a
   ; source tree more copying than the archive it is hashing.
   (tar-feed h buf e)                       ; feed one file's body + pad; 1 | ()
    (: n (tar-get e 'size 0)
       q (? (0 < n) (open (tar-get e 'path "") "r") 0)
       (? (0 = n) 1
          (! q) ()
          (: (go left) (? (0 = left) 1
                          (: k (swig q buf)
                             (? (0 >= k) ()
                                (: k2 (? (left < k) left k)
                                   _ (sha256-feed h (snip buf 0 k2))
                                   (go (left - k2))))))
             ok (go n)
             _ (close q)
             (ok && (: r (n % tar-blk) (r && sha256-feed h * string * cask $ tar-blk - r, 1))))))
   (tar-hash es)                            ; -> the 64-char digest | ()
    (: h (cask tar-shast)
       _ (sha256-init h)
       buf (cask 65536)
       (go xs) (? (atom? xs) 1
                  (: e <xs
                     hd (tar-head e)
                     (? !hd ()
                        (: _ (sha256-feed h hd)
                           ok (? (id? (tar-get e 'kind 'file) 'file) (tar-feed h buf e) 1)
                           (ok && go >xs)))))
       (go es ? (sha256-feed h (string (cask (tar-blk * 2))),
                 sha256-done h)))

   (tar-pad s)                              ; up to the next 512
    (: n #s r (n % tar-blk)
       (? r (: c (cask (tar-blk - r)) (s + string c)) s))
   (tar-pack es)
    (: j (jug 0)
       (go xs) (? (! (two? xs)) 1
                  (: e <xs
                     h (tar-head e)
                     (? !h ()
                        (: _ (say j h)
                           _ (? (id? (tar-get e 'kind 'file) 'file)
                                (: body (tar-get e 'body "")
                                   (? #body (say j (tar-pad body)) 0))
                                0)
                           (go >xs)))))
       ok (go es)
       (? (! ok) ()
          ; two zero blocks end an archive -- one is not enough, and readers that
          ; stop at the first will still read this, so it costs nothing to be right.
          (: _ (say j (string (cask (tar-blk * 2))))
             ((slurp j) + ""))))

   ; --- reading --------------------------------------------------------------
   (tar-zero? s o)                          ; is the block at o all zeros?
    (: (go i) (? (tar-blk <= i) 1 ((s (o + i)) = 0) (go (i + 1)) 0) (go 0))
   (tar-read1 s n o)                        ; -> (1 entry next) | () | 'end
    (? (tar-zero? s o) 'end
       (: b (cask tar-blk) _ (pour b 0 s o tar-blk)
          got (tar-sum b)
          want (tar-oct s (o + 148) 8)
          (? (! (got = want)) ()
             (: name (tar-txt s o 100)
                pre (tar-txt s (o + 345) 155)
                nom (? (tally pre) (pre + "/" + name) name)
                tc (s (o + 156))
                kind (? (tc = 53) 'dir (member? tc '(50 49)) 'link 'file)   ; '5' '2'/'1'
                sz (kind == 'file && tar-oct s (o + 124) 12)
                ; a member whose body runs past the end of the bytes we were handed
                ; is a truncated archive, and the size field is where that shows.
                (? (n < (o + tar-blk + sz)) ()
                   (: e {
                       'nom nom 'kind kind
                      'mode (tar-oct s (o + 100) 8)
                      'uid (tar-oct s (o + 108) 8)
                      'gid (tar-oct s (o + 116) 8)
                      'mtime (tar-oct s (o + 136) 12)
                      'to (tar-txt s (o + 157) 100)
                      'uname (tar-txt s (o + 265) 32)
                      'gname (tar-txt s (o + 297) 32)
                      'body (!sz ? "" (cask sz @ c (pour c 0 s (o + tar-blk) sz, string c)))}
                      blocks (sz && tar-blk * ((sz + tar-blk - 1) // tar-blk))
                      [1 e (o + tar-blk + blocks)]))))))
   ; an archive ends at its zero block, never at the end of the bytes. Running
   ; out of input mid-walk is torn, not a clean finish -- a truncated download that
   ; answered the members it happened to contain would extract half a tree in
   ; silence, which is worse than extracting none. (Trailing padding past the zero
   ; block is fine: the walk has already stopped by then.)
   (tar-unpack s)
    (:- (go 0 ())
     n #s
     (go o acc) (n - o >= tar-blk ?
                 (: r (tar-read1 s n o)
                  (? (r == 'end) [1 (rev acc)]
                      r (go <>>r (<(>r) . acc))))))

   ; --- the filesystem, in and out -------------------------------------------
   ; a walk answers paths relative to root and stores them that way: an archive
   ; carrying "/home/gwen/..." is one that unpacks over somebody's system. tar's own
   ; "removing leading /" warning exists because the naive spelling does that.
   (tar-slurp p) (: q (open p "r") (? q (: s (slurp q) (close q, [1 (s + "")]))))
   (tar-blurt p s) (: q (open p "w") (? q (say q s, close q, 1)))
   (tar-join a b) (? #a (a + "/" + b) b)
   ; stat answers (size mtime-ms mode mtime-ns), and its mtime is milliseconds where
   ; tar's field is seconds. it follows a symlink -- there is no lstat here -- so the
   ; link test is readlink, which answers the target for one and nothing for anything
   ; else. Without that test a tree of symlinks archives as copies of its own files.
   (tar-mode st) (4095 & <>>st)
   (tar-dir? st) (61440 & <>>st = 16384)
   (tar-secs st) (<>st // 1000)
   ; the pruned walk: `keep` is asked with each child's rel nom before the walk
   ; descends or slurps, so what the caller will not carry is never read -- a
   ; build tree's out/ is gigabytes the archive was never going to hold.
   (tar-gather? keep root rel) (tar-walk? keep root rel 1)
   ; the thin walk: the same walk that reads no file. each file entry carries its
   ; 'size (off stat) and its 'path (where the bytes are) instead of its body, which
   ; is what lets a caller hash or write the archive as a stream -- the fat walk holds
   ; every byte of the tree at once, and a tree is megabytes that exist to be read
   ; through once. 'path is this machine's and rides no header; tar-head reads
   ; neither key but 'size, so a thin entry heads exactly as its fat twin does.
   (tar-thin? keep root rel) (tar-walk? keep root rel 0)
   (tar-walk? keep root rel fat)            ; -> (1 entries) | ()
    (: full (? #rel (root + "/" + rel) root)
       lnk (readlink full)
       st (stat full)
       (? (string? lnk)
          (: e (tar-entry rel 'link "")
             _ (pin e 'to lnk) _ (pin e 'mode 511)
             _ (? (two? st) (pin e 'mtime (tar-secs st)) 0)
             ; the inner ` is load-bearing: `(1 (e)) would apply e to nothing, and
             ; (f) == f, so the caller would take a bare entry for a list of them.
             [1 [e]])
          (! (two? st)) ()
          (tar-dir? st)
          (: e (tar-entry (? (tally rel) rel ".") 'dir "")
             _ (pin e 'mode (tar-mode st)) _ (pin e 'mtime (tar-secs st))
             ; sorted, so an archive of the same tree is the same bytes -- readdir's
             ; order is the filesystem's and varies between two boxes holding one tree.
             kids (sort (readdir full))
             (go ks acc) (atom? ks ? [1 (rev acc)]
                          (: k2 (tar-join rel <ks)
                           (!(keep k2) ? (go >ks acc)
                            (: r (tar-walk? keep root k2 fat)
                             (r && go >ks (foldl (flip cons) acc <>r))))))
             r (go kids ())
             (r ? [1 (e . <>r)]))
          !fat
          (: e (tar-entry rel 'file "")
             _ (pins e 'mode (tar-mode st) 'mtime (tar-secs st)
                       'size <st           'path full)
             [1 [e]])
          (: b (tar-slurp full)
             (b ?
                (: e (tar-entry rel 'file (<(>b)))
                   _ (pin e 'mode (tar-mode st)) _ (pin e 'mtime (tar-secs st))
                   ; the inner ` is load-bearing: `(1 (e)) would apply e to nothing, and
             ; (f) == f, so the caller would take a bare entry for a list of them.
             [1 [e]])))))
   (tar-gather root rel) (tar-gather? (\ _ 1) root rel)
   ; `mkdir` is dyadic (path, mode). At one operand it hands back a closure and makes
   ; no directory -- (f x) curries, it does not default -- and the closure is truthy, so
   ; the miss is silent at every call site that tests it.
   ; and neither primitive answers its own success: mkdir and symlink both return the
   ; zero point when they worked. So each write is checked by looking -- stat for a dir
   ; or a file, readlink for a link. The presence law from the other side: the value
   ; cannot say, so ask out of band.
   (tar-there? p) (two? (stat p))
   (tar-strip nom)                          ; the "./" GNU tar stores on a `-C dir .`
    (#nom > 1 && nom 0 = 46 && nom 1 = 47 ? (snip nom 2 #nom) nom)
   ; a release tarball must not carry the clock or the packer's uid. Two cuts of
   ; one tree would hash differently, and "this is that release" stops being a thing
   ; anyone can check. mtime, uid and gid drift with who ran the pack and when -- and
   ; so does the mode, for the same reason: `mkdir` and `open` wear the packer's
   ; umask, so one tree cut under 022 and under 077 disagrees in every directory.
   ; Only the x bit of it is content (a binary that unpacks unrunnable is a broken
   ; artifact), so that is the half kept and the rest is spelled canonically.
   (tar-canon e) (: k (tar-get e 'kind 'file)
                    (? (k == 'link) 511
                       (k == 'dir)  493
                       (tar-get e 'mode 420 & 73 > 0) 493
                       420))
   (tar-level es stamp)
    (map (e \ pins (foldl (t \ k \ pin t k (e k)) {} (keys e))
              'mtime stamp 'uid 0 'gid 0
              'uname "root" 'gname "root" 'mode (tar-canon e))
         es)
   (tar-scatter root es)                    ; -> (1 count) | ()
    (:- (go es 0) (go xs k)
     (atom? xs ? [1 k]
      (: e <xs
         nom (tar-strip (tar-get e 'nom ""))
         p (? (tally nom) (? (tally root) (root + "/" + nom) nom) root)
         kind (tar-get e 'kind 'file)
         ; the mode is content, and it must be laid back explicitly: `open`
         ; and `mkdir` both create at the umask's pleasure, so a file written
         ; here comes out 0644 whatever the header said, an extracted binary is
         ; not executable, and under 077 every directory lands 0700. The archive
         ; carried the mode and we dropped it -- which `diff -r` cannot see,
         ; because it compares bytes and not modes. chmod is the only door: it
         ; is the one call the umask does not touch.
         ok (? (kind == 'dir)  (mkdir p (tar-get e 'mode 493),
                                tar-there? p && chmod p (tar-get e 'mode 493),
                                tar-there? p)
               (kind == 'link) (symlink (tar-get e 'to "") p,
                                string? (readlink p))
               (tar-blurt p (tar-get e 'body ""),
                tar-there? p && chmod p (tar-get e 'mode 420),
                tar-there? p))
       (ok && go >xs (k + 1))))))) ; module 'tar
