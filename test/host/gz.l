; test/host/gz.l -- the laws of lib/gz.l and lib/tar.l, the .tar.gz written and read
; in love. What is proved here is internal: crc32 against its published vector, both
; coders against each other over the shapes that break coders, and the ustar header
; field by field. The half that needs the outside world -- that GNU tar and GNU gzip
; agree with us, in both directions, over a real tree with a symlink and an odd mode --
; is test/gate/targz.sh, because it needs those two programs and this file needs
; nothing.
;
; the round-trip shapes are chosen, not arbitrary. empty and one byte are where a
; block coder's edges live; a long run is the overlapping copy (dist 1) that a bulk
; move would get wrong; incompressible data is the lane where matches never fire; and
; text is the only one that looks like the happy path.
(use 'gz)
(use 'tar)

(: (gz-mk n f) (: b (cask n) (go i) (? (n <= i) (string b) (: _ (pin b i (f i)) (go (i + 1)))) (go 0))
   gz-empty ""
   gz-one   "x"
   gz-run   (gz-mk 300 (\ i 65))                          ; one byte 300 times
   gz-text  "the quick brown fox jumps over the lazy dog. the quick brown fox jumps over the lazy dog."
   gz-noise (gz-mk 2000 (\ i (& ((i * 2654435761) >> 7) 255)))   ; no runs, no matches
   gz-shapes [gz-empty gz-one gz-run gz-text gz-noise]
   ; a round trip through the raw stream, and again through the container
   (gz-rt s) (: r (gz-inflate (gz-deflate s) 0) (? (! r) 0 (? ((<(>r)) = s) 1 0)))
   (gz-crt s) (: r (gz-unzip (gz-zip s "" 0)) (? (! r) 0 (? ((<(>r)) = s) 1 0)))
   (gz-all f xs) (? (! (two? xs)) 1 (? (f <xs) (gz-all f >xs) 0)))

; --- crc32 --- the check value every crc32 implementation publishes is the one for
; "123456789", and it is the whole point of naming a polynomial: two crc32s that
; disagree here are different functions wearing one name.
(assert
 (3421780262 = (gz-crc32 "123456789"))          ; 0xcbf43926
 (0 = (gz-crc32 ""))                            ; the empty message hashes to the seed
 !((gz-crc32 "a") = (gz-crc32 "b"))
 ((gz-crc32 "abc") = (gz-crc32 ("ab" + "c"))))  ; it reads bytes, not strings

; --- deflate and inflate --- ours against ours, over the five shapes.
(assert
 (gz-all gz-rt gz-shapes)
 (gz-all gz-crt gz-shapes)
 ; a run of one byte is the overlapping copy: the match source is still being
 ; written as it is read, so a decoder that moved the block at once would answer
 ; the first bytes repeated rather than the run.
 (: r (gz-inflate (gz-deflate gz-run) 0) (300 = (tally (<(>r)))))
 ; the container's trailer is the only thing that can catch a stream that inflates
 ; and is still wrong, so a corrupted body must be refused, not returned.
 (: g (gz-zip gz-text "" 0)
    n (tally g)
    bad (gz-mk n (\ i (? (i = 20) (^ (g i) 255) (g i))))
    (! (gz-unzip bad)))
 (! (gz-unzip "notagzipatall"))                 ; a wrong magic is absence, not a scare
 (! (gz-unzip ""))
 ; and compression is real rather than nominal -- a coder that stored everything
 ; would pass every law above this one.
 ((tally (gz-deflate gz-run)) < 20)
 ((tally (gz-deflate gz-text)) < (tally gz-text)))

; --- ustar --- the header is a wire, so the laws are its offsets. a field written
; where the next one starts is the classic tar bug and it survives a round trip
; through our own reader, which is why these are checked against the offsets and
; not against what we read back.
(: tz-e (tar-entry "dir/file.txt" 'file "hello")
   _ (pin tz-e 'mode 420) _ (pin tz-e 'mtime 1234)
   tz-h (tar-head tz-e))
(assert
 (512 = (tally tz-h))
 (100 = (tz-h 0))                               ; name starts at offset 0: 'd'
 ((tz-h 156) = 48)                              ; typeflag '0', a regular file
 ("ustar" = (tar-txt tz-h 257 6))               ; the magic, at 257
 ((tz-h 263) = 48) ((tz-h 264) = 48)            ; version "00", not NUL-ended
 (420 = (tar-oct tz-h 100 8))                   ; mode
 (5 = (tar-oct tz-h 124 12))                    ; size
 (1234 = (tar-oct tz-h 136 12))                 ; mtime
 ; a path of 12 characters is not split. The prefix exists for names past 100,
 ; and reaching for it earlier would produce an archive every tar can still read
 ; but none writes -- so the empty prefix here is the law, not an omission.
 ("" = (tar-txt tz-h 345 155))
 ("dir/file.txt" = (tar-txt tz-h 0 100))
 ; the checksum covers the block with its own field read as spaces, which is the
 ; only way a field can live inside what it sums.
 ((tar-sum tz-h) = (tar-oct tz-h 148 8)))

; --- pack and unpack --- a whole archive, ours through ours.
(: tz-es [(tar-entry "a.txt" 'file "alpha\n")
          (tar-entry "sub" 'dir "")
          (tar-entry "sub/b.txt" 'file "beta beta\n")]
   tz-a (tar-pack tz-es)
   tz-r (tar-unpack tz-a)
   tz-back (? (! tz-r) () (<(>tz-r))))
(assert
 (0 = (% (tally tz-a) 512))                     ; an archive is whole blocks, always
 ((tally tz-a) = 3584)                          ; 3 headers + 2 bodies + 2 zero blocks
 (two? tz-r)
 (3 = (tally tz-back))
 ("a.txt" = (peep <tz-back 'nom ()))
 (id? 'file (peep <tz-back 'kind ()))
 ("alpha\n" = (peep <tz-back 'body ()))
 (id? 'dir (peep <(>tz-back) 'kind ()))
 ("sub/b.txt" = (peep <(>(>tz-back)) 'nom ()))
 ; order is the archive's. a reader that answered entries reversed would satisfy
 ; every "is it in there" law and still extract a tree whose directories arrive
 ; after the files that need them.
 ("beta beta\n" = (peep <(>(>tz-back)) 'body ()))
 ; a truncated archive is absence, not half an answer
 (! (tar-unpack (snip tz-a 0 700)))
 (! (tar-unpack "rubbish")))

; --- the name split --- ustar's 100-byte name with a 155-byte prefix, joined by a
; "/" that is not stored. The refusal at the end is the point: a path that fits
; neither field is refused rather than truncated, because a silently shortened path
; is a wrong archive that looks right until something extracts it.
(: tz-long (: (go i a) (? (12 <= i) a (go (i + 1) (a + "0123456789/"))) (go 0 ""))
   tz-deep (tz-long + "leaf.txt"))
(assert
 (: sp (tar-split "short.txt") (&& ("" = <sp) ("short.txt" = <(>sp))))
 (: sp (tar-split tz-deep) (&& (two? sp) ((tally <(>sp)) < 101)))
 ; 200 characters with no "/" at all: no split can exist, and none is invented
 (! (tar-split (gz-mk 200 (\ i 97))))
 (! (tar-head (: e (tar-entry (gz-mk 200 (\ i 97)) 'file "") e))))

; --- the two together --- a .tar.gz built and taken apart entirely in love, which
; is the whole errand: `make dist` shells out to neither tar nor gzip.
(: tgz-a (tar-pack [(tar-entry "x/one.txt" 'file "first\n")
                    (tar-entry "x/two.txt" 'file (gz-mk 5000 (\ i (+ 97 (% i 26)))))])
   tgz-z (gz-zip tgz-a "archive.tar" 0)
   tgz-u (gz-unzip tgz-z)
   tgz-back (? (! tgz-u) () (tar-unpack (<(>tgz-u)))))
(assert
 ((tally tgz-z) < (tally tgz-a))                ; it actually compressed
 (two? tgz-u)
 (two? tgz-back)
 (2 = (tally (<(>tgz-back))))
 ("first\n" = (peep <(<(>tgz-back)) 'body ()))
 (5000 = (tally (peep <(>(<(>tgz-back))) 'body ()))))

(puts "gz: ok") (putc 10)
