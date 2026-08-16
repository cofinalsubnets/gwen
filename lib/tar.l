; lib/tar.l -- tar in love: the ustar archive, read and written. `(use 'tar)`, then:
;
;   (tar-pack entries)     -> the archive bytes
;   (tar-unpack s)         -> (1 entries) | ()
;   (tar-gather root path) -> (1 entries) | ()    walk the filesystem into entries
;   (tar-scatter root es)  -> (1 count) | ()      write entries back out under root
;
; an ENTRY is a tablet, because the header has fourteen fields and a positional list
; of them would be unreadable at every call site. the keys, and the only ones that
; matter: 'nom the path AS STORED (relative, no leading /), 'kind one of 'file 'dir
; 'link, 'body the bytes (a file), 'to the target (a link), 'mode the permission bits,
; 'mtime, 'uid, 'gid, 'uname, 'gname. (tar-entry nom kind body) fills the rest with the
; ordinary defaults, which is what nearly every caller wants.
;
; ⚠ presence rides the (1 _) wrapper on both READS, and it must: an empty archive is a
; legitimate archive and answers no entries, which nets nothing -- indistinguishable
; from a torn one by the value alone. test with two?.
;
; the format, in one breath: a 512-byte header per member, then the body padded up to
; the next 512, then two zero blocks to end. Every numeric field is OCTAL IN ASCII with
; a NUL or a space where the digits stop, which is why they are written by hand here
; rather than by any number printer -- the widths are the format's, not ours.
;
; ⚠ THE CHECKSUM IS COMPUTED OVER THE HEADER WITH ITS OWN FIELD READ AS EIGHT SPACES.
; That is not a convention we could choose differently: it is how the field can live
; inside the block it sums. It is written back as six octal digits, a NUL, then a
; space -- an order two of the three historical tars disagree about, and the one every
; reader accepts.
;
; ⚠ USTAR SPLITS A LONG NAME across `prefix` (155) and `name` (100), joined with a "/"
; that is NOT stored. A path that fits neither -- no "/" leaving a <=100 tail -- is
; refused rather than truncated: a silently shortened path is a wrong archive that
; looks right until something extracts it.
(:
   tar-blk 512

   ; --- the field pokes ------------------------------------------------------
   ; octal, right-aligned in n-1 columns, zero-padded, then a NUL. this is the
   ; `%0*lo` of every tar ever written, spelled out because the widths are the wire.
   (tar-oct! b o n v)
    (: (go i x) (? (i < 0) 0
                   (: _ (pin b (o + i) (48 + (x & 7))) (go (i - 1) (x >> 3))))
       _ (pin b (o + n - 1) 0)
       (go (n - 2) v))
   (tar-txt! b o n s)                       ; a text field, NUL-filled, never overrun
    (: k (tally s)
       (go i) (? (n <= i) 0
                 (: _ (pin b (o + i) (? (i < k) (s i) 0)) (go (i + 1))))
       (go 0))
   (tar-sum b)                              ; the header checksum, own field as spaces
    (: (go i a) (? (tar-blk <= i) a
                   (go (i + 1) (a + (? ((148 <= i) && (i < 156)) 32 (peep b i 0)))))
       (go 0 0))

   ; --- the field peeks ------------------------------------------------------
   (tar-oct s o n)                          ; octal digits until a non-digit
    (: (go i a) (? (n <= i) a
                   (: c (s (o + i))
                      (? ((48 <= c) && (c < 56)) (go (i + 1) ((a * 8) + (c - 48))) a)))
       (go 0 0))
   (tar-txt s o n)                          ; a NUL-ended text field
    (: (go i) (? (n <= i) i ((s (o + i)) = 0) i (go (i + 1)))
       k (go 0)
       ; ⚠ `pour`'s last argument is a COUNT, not an end offset, and the nif clamps it to
       ; the shorter of the two arrays -- so an end offset can answer right off the
       ; destination's size alone, silently, and move the whole tail to do it.
       (? k (: c (cask k) _ (pour c 0 s o k) (string c)) ""))

   ; --- entries --------------------------------------------------------------
   (tar-entry nom kind body)
    (: e (tablet 0)
       _ (pin e 'nom nom) _ (pin e 'kind kind) _ (pin e 'body (body + ""))
       _ (pin e 'to "")
       _ (pin e 'mode (? (id? kind 'dir) 493 420))     ; 0755 / 0644
       _ (pin e 'mtime 0) _ (pin e 'uid 0) _ (pin e 'gid 0)
       _ (pin e 'uname "root") _ (pin e 'gname "root")
       e)
   ; peep's own default IS the presence test here -- it answers d only when the key is
   ; ABSENT, so an entry whose 'body is legitimately "" keeps its "". ⚠ not `lit?`,
   ; which asks whether a NAME is bound and reads it.
   (tar-get e k d) (peep e k d)

   ; --- the name split -------------------------------------------------------
   ; ustar joins prefix and name with a "/" it does not store, so the split must fall
   ; ON a separator. we take the LONGEST prefix under 156 that leaves a name under 101
   ; -- longest, because it is the tail that is tight.
   (tar-split nom)                          ; -> (prefix name) | ()
    (: n (tally nom)
       (? (n < 101) `("" nom)
          (: (go i best) (? (n <= i) best
                            (: c (nom i)
                               ok (((c = 47) && (i < 156)) && ((n - i - 1) < 101))
                               (go (i + 1) (? ok i best))))
             k (go 0 -1)
             (? (k < 0) ()
                (: p (cask k) _ (pour p 0 nom 0 k)
                   q (cask (n - k - 1)) _ (pour q 0 nom (k + 1) (n - k - 1))
                   `((string p) (string q)))))))

   ; --- writing --------------------------------------------------------------
   (tar-kindc k) (? (id? k 'dir) 53 (id? k 'link) 50 48)   ; '5' '2' '0'
   (tar-head e)                             ; -> the 512-byte header | ()
    (: nom (tar-get e 'nom "")
       kind (tar-get e 'kind 'file)
       nom2 (? (id? kind 'dir) (? ((tally nom) && ((nom ((tally nom) - 1)) = 47)) nom (nom + "/")) nom)
       sp (tar-split nom2)
       (? (! sp) ()
          (: b (cask tar-blk)
             body (tar-get e 'body "")
             _ (tar-txt! b 0 100 (<(>sp)))                    ; name
             _ (tar-oct! b 100 8 (tar-get e 'mode 420))
             _ (tar-oct! b 108 8 (tar-get e 'uid 0))
             _ (tar-oct! b 116 8 (tar-get e 'gid 0))
             _ (tar-oct! b 124 12 (? (id? kind 'file) (tally body) 0))
             _ (tar-oct! b 136 12 (tar-get e 'mtime 0))
             _ (pin b 156 (tar-kindc kind))
             _ (tar-txt! b 157 100 (tar-get e 'to ""))
             _ (tar-txt! b 257 6 "ustar")                     ; magic, NUL-ended
             _ (pin b 263 48) _ (pin b 264 48)                ; version "00", NOT NUL-ended
             _ (tar-txt! b 265 32 (tar-get e 'uname "root"))
             _ (tar-txt! b 297 32 (tar-get e 'gname "root"))
             _ (tar-oct! b 329 8 0) _ (tar-oct! b 337 8 0)    ; devmajor/minor
             _ (tar-txt! b 345 155 <sp)                     ; prefix
             ; the checksum LAST -- it sums the block it sits in, so every other
             ; field must already be written. six digits, NUL, space.
             _ (tar-oct! b 148 7 (tar-sum b))
             _ (pin b 154 32)
             (string b))))
   (tar-pad s)                              ; up to the next 512
    (: n (tally s) r (% n tar-blk)
       (? r (: c (cask (tar-blk - r)) (s + (string c))) s))
   (tar-pack es)
    (: j (jug 0)
       (go xs) (? (! (two? xs)) 1
                  (: e <xs
                     h (tar-head e)
                     (? (! h) ()
                        (: _ (say j h)
                           _ (? (id? (tar-get e 'kind 'file) 'file)
                                (: body (tar-get e 'body "")
                                   (? (tally body) (say j (tar-pad body)) 0))
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
                sz (? (id? kind 'file) (tar-oct s (o + 124) 12) 0)
                ; a member whose body runs past the end of the bytes we were handed
                ; is a TRUNCATED archive, and the size field is where that shows.
                (? (n < (o + tar-blk + sz)) ()
                   (: e (tablet 0)
                      _ (pin e 'nom nom) _ (pin e 'kind kind)
                      _ (pin e 'mode (tar-oct s (o + 100) 8))
                      _ (pin e 'uid (tar-oct s (o + 108) 8))
                      _ (pin e 'gid (tar-oct s (o + 116) 8))
                      _ (pin e 'mtime (tar-oct s (o + 136) 12))
                      _ (pin e 'to (tar-txt s (o + 157) 100))
                      _ (pin e 'uname (tar-txt s (o + 265) 32))
                      _ (pin e 'gname (tar-txt s (o + 297) 32))
                      _ (pin e 'body (? sz (: c (cask sz)
                                              _ (pour c 0 s (o + tar-blk) sz)
                                              (string c))
                                          ""))
                      blocks (? sz (* tar-blk (// (sz + tar-blk - 1) tar-blk)) 0)
                      `(1 e (o + tar-blk + blocks))))))))
   ; ⚠ AN ARCHIVE ENDS AT ITS ZERO BLOCK, never at the end of the bytes. Running
   ; out of input mid-walk is TORN, not a clean finish -- a truncated download that
   ; answered the members it happened to contain would extract half a tree in
   ; silence, which is worse than extracting none. (Trailing padding past the zero
   ; block is fine: the walk has already stopped by then.)
   (tar-unpack s)
    (: n (tally s)
       (go o acc) (? ((n - o) < tar-blk) ()
                     (: r (tar-read1 s n o)
                        (? (id? r 'end) `(1 (rev acc))
                           (! r) ()
                           (go <>>r (<(>r) >< acc)))))
       (go 0 ()))

   ; --- the filesystem, in and out -------------------------------------------
   ; ⚠ a walk answers PATHS RELATIVE TO root and stores them that way: an archive
   ; carrying "/home/gwen/..." is one that unpacks over somebody's system. tar's own
   ; "removing leading /" warning exists because the naive spelling does that.
   (tar-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) `(1 (s + ""))) ()))
   (tar-blurt p s) (: q (open p "w") (? q (: _ (say q s) _ (close q) 1) ()))
   (tar-join a b) (? (tally a) (a + "/" + b) b)
   ; stat answers (size mtime-ms mode mtime-ns), and its mtime is MILLISECONDS where
   ; tar's field is seconds. ⚠ it FOLLOWS a symlink -- there is no lstat here -- so the
   ; link test is readlink, which answers the target for one and nothing for anything
   ; else. Without that test a tree of symlinks archives as copies of its own files.
   (tar-mode st) (& 4095 <>>st)
   (tar-dir? st) ((& 61440 <>>st) = 16384)
   (tar-secs st) (// <>st 1000)
   (tar-gather root rel)                    ; -> (1 entries) | ()
    (: full (? (tally rel) (root + "/" + rel) root)
       lnk (readlink full)
       st (stat full)
       (? (string? lnk)
          (: e (tar-entry rel 'link "")
             _ (pin e 'to lnk) _ (pin e 'mode 511)
             _ (? (two? st) (pin e 'mtime (tar-secs st)) 0)
             ; ⚠ the inner ` is load-bearing: `(1 (e)) would APPLY e to nothing, and
             ; (f) == f, so the caller would take a bare entry for a list of them.
             `(1 `(e)))
          (! (two? st)) ()
          (tar-dir? st)
          (: e (tar-entry (? (tally rel) rel ".") 'dir "")
             _ (pin e 'mode (tar-mode st)) _ (pin e 'mtime (tar-secs st))
             ; sorted, so an archive of the same tree is the same bytes -- readdir's
             ; order is the filesystem's and varies between two boxes holding one tree.
             kids (sort (readdir full))
             (go ks acc) (? (! (two? ks)) `(1 (rev acc))
                            (: r (tar-gather root (tar-join rel <ks))
                               (? (! r) ()
                                  (go >ks (foldl (\ a x (x >< a)) acc (<(>r)))))))
             r (go kids ())
             (? (! r) () `(1 (e >< (<(>r))))))
          (: b (tar-slurp full)
             (? (! b) ()
                (: e (tar-entry rel 'file (<(>b)))
                   _ (pin e 'mode (tar-mode st)) _ (pin e 'mtime (tar-secs st))
                   ; ⚠ the inner ` is load-bearing: `(1 (e)) would APPLY e to nothing, and
             ; (f) == f, so the caller would take a bare entry for a list of them.
             `(1 `(e)))))))
   ; ⚠ `mkdir` IS DYADIC (path, mode). At one operand it hands back a CLOSURE and makes
   ; no directory -- (f x) curries, it does not default -- and the closure is truthy, so
   ; the miss is silent at every call site that tests it.
   ; ⚠ AND NEITHER PRIMITIVE ANSWERS ITS OWN SUCCESS: mkdir and symlink both return the
   ; zero point when they worked. So each write is CHECKED by looking -- stat for a dir
   ; or a file, readlink for a link. The presence law from the other side: the value
   ; cannot say, so ask out of band.
   (tar-there? p) (? (two? (stat p)) 1 0)
   (tar-strip nom)                          ; the "./" GNU tar stores on a `-C dir .`
    (? (((tally nom) > 1) && (((nom 0) = 46) && ((nom 1) = 47))) (snip nom 2 (tally nom)) nom)
   ; ⚠ A RELEASE TARBALL MUST NOT CARRY THE CLOCK OR THE PACKER'S UID. Two cuts of
   ; one tree would hash differently, and "this is that release" stops being a thing
   ; anyone can check. mtime, uid and gid are the three fields that drift with who
   ; ran the pack and when; MODE is deliberately left alone, because the executable
   ; bit is content (a binary that unpacks unrunnable is a broken artifact).
   (tar-level es stamp)
    (map (\ e (: t (tablet 0)
                 _ (map (\ k (pin t k (peep e k ()))) (keys e))
                 _ (pin t 'mtime stamp) _ (pin t 'uid 0) _ (pin t 'gid 0)
                 _ (pin t 'uname "root") _ (pin t 'gname "root")
                 t))
         es)
   (tar-scatter root es)                    ; -> (1 count) | ()
    (: (go xs k) (? (! (two? xs)) `(1 k)
                    (: e <xs
                       nom (tar-strip (tar-get e 'nom ""))
                       p (? (tally nom) (? (tally root) (root + "/" + nom) nom) root)
                       kind (tar-get e 'kind 'file)
                       ; ⚠ THE MODE IS CONTENT, and it must be laid back EXPLICITLY: `open`
                       ; creates at the umask's pleasure, so a file written here comes out
                       ; 0644 whatever the header said, and an extracted BINARY is not
                       ; executable. The archive carried 0755 and we dropped it -- which
                       ; `diff -r` cannot see, because it compares bytes and not modes.
                       ok (? (id? kind 'dir)  (: _ (mkdir p (tar-get e 'mode 493)) (tar-there? p))
                             (id? kind 'link) (: _ (symlink (tar-get e 'to "") p)
                                                  (? (string? (readlink p)) 1 0))
                             (: _ (tar-blurt p (tar-get e 'body ""))
                                _ (? (tar-there? p) (chmod p (tar-get e 'mode 420)) 0)
                                (tar-there? p)))
                       (? (! ok) () (go >xs (k + 1)))))
       (go es 0)))